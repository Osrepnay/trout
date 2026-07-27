module Trout.Search
  ( SearchEnv,
    newEnv,
    refreshEnv,
    clearEnv,
    getNodecount,
    pvWalk,
    staticExchEval,
    seeOfCapture,
    bestMove,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join, when)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (maximumBy, traverse_)
import Data.Functor ((<&>))
import Data.Int (Int16)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Data.Ord (comparing)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.STRef.Strict (modifySTRef')
import Data.Vector.Primitive ((!))
import Data.Vector.Primitive.Mutable (STVector)
import Data.Vector.Primitive.Mutable qualified as MV
import Debug.Trace
import Trout.Bitboard (Bitboard, clearBit, countTrailingZeros, (.&.), (.|.))
import Trout.Game
  ( Game (..),
    allDisquiets,
    allMoves,
    inCheck,
    isDrawn,
    makeMove,
  )
import Trout.Game.Board
  ( Board (..),
    addPiece,
    getPiece,
    occupancy,
    pieceBitboard,
    removePiece,
  )
import Trout.Game.Move (Move (..), SpecialMove (..))
import Trout.Game.MoveGen (SpecialMove (Promotion), kingTable, knightTable, pawnCaptureTable)
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Piece (Color (..), Piece (..), PieceType (..), other)
import Trout.Search.Eval (eval, materialScore)
import Trout.Search.Node (NodeResult (..), mkNodeResult, nodeUsable)
import Trout.Search.TranspositionTable (STTranspositionTable, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth, pawnWorth, pieceWorth, scoreIsMate, winWorth)

type KillerMap = Map Int16 [Move]

maxKillers :: Int
maxKillers = 2

addKiller :: Int16 -> Move -> KillerMap -> KillerMap
addKiller halfmove move =
  M.alter
    ( \maybeKillers ->
        let killerList = join (maybeToList maybeKillers)
         in if move `elem` killerList
              then Just (move : removeSingle move killerList)
              else Just $ move : trimEnd killerList
    )
    halfmove
  where
    trimEnd xs
      | length xs == maxKillers = init xs
      | otherwise = xs

type HistoryTable s = STVector s Int

maxHistory :: Int
maxHistory = 10000

historyIdx :: Color -> Move -> Int
historyIdx color move =
  fromEnum color * 6 * 64
    + fromEnum (movePiece move) * 64
    + moveTo move

addHistory :: HistoryTable s -> Int -> Int -> ST s ()
addHistory history bonus key =
  MV.modify
    history
    (\curr -> curr + bonus - abs bonus * curr `quot` maxHistory)
    key

getHistory :: HistoryTable s -> Int -> ST s Int
getHistory = MV.read

decayHistory :: HistoryTable s -> ST s ()
decayHistory history =
  traverse_
    (MV.modify history (\h -> h * 1 `quot` 5))
    [0 .. MV.length history - 1]

-- anything that needs to be carried up through search tree
data SearchEnv s = SearchEnv
  { sEnvTT :: !(STTranspositionTable s),
    sEnvKillers :: !(STRef s KillerMap),
    sEnvHistory :: !(HistoryTable s),
    sEnvNodecount :: !(STRef s Int)
  }

incNodecount :: ReaderT (SearchEnv s) (ST s) ()
incNodecount = do
  ref <- sEnvNodecount <$> ask
  lift $ modifySTRef' ref (+ 1)

resetNodecount :: ReaderT (SearchEnv s) (ST s) ()
resetNodecount = do
  ref <- sEnvNodecount <$> ask
  lift $ writeSTRef ref 0

getNodecount :: ReaderT (SearchEnv s) (ST s) Int
getNodecount = ask >>= (lift . readSTRef) . sEnvNodecount

newEnv :: Int -> ST s (SearchEnv s)
newEnv n = do
  tt <- TT.new n
  killers <- newSTRef M.empty
  history <- MV.replicate (2 * 6 * 64) 0
  nodes <- newSTRef 0
  pure (SearchEnv tt killers history nodes)

refreshEnv :: ReaderT (SearchEnv s) (ST s) ()
refreshEnv = do
  (SearchEnv {sEnvKillers = killers, sEnvHistory = history}) <- ask
  -- lift $ writeSTRef killers M.empty
  lift $ decayHistory history
  resetNodecount

clearEnv :: SearchEnv s -> ST s ()
clearEnv (SearchEnv tt killers history nodes) = do
  TT.clear tt
  writeSTRef killers M.empty
  MV.set history 0
  writeSTRef nodes 0

-- (attempt to) find the pv (the tt might have been overwritten)
pvWalk :: Game -> ReaderT (SearchEnv s) (ST s) [Move]
pvWalk game = go game Nothing
  where
    go _ (Just 0) = pure []
    go g maybeDepth
      | not (isDrawn g) = do
          (SearchEnv {sEnvTT = tt}) <- ask
          maybeEntry <- lift (TT.lookup (gameBoard g) tt)
          case maybeEntry of
            Just (TTEntry {entryMove = move, entryDepth = depth}) ->
              if maybe True (depth ==) maybeDepth && move /= NullMove
                then case makeMove g move of
                  Just movedG -> (move :) <$> go movedG (Just (depth - 1))
                  Nothing -> pure [] -- should be rare, this means full tt collision
                else pure []
            Nothing -> pure []
      | otherwise = pure []

-- no check detection, just sends it
staticExchEval :: Board -> Int -> PieceType -> Int
staticExchEval !board !sq = go (boardTurn board) occ
  where
    occ = occupancy (boardPieces board)
    pawnAtt c = pawnCaptureTable c ! sq
    knightAtt = knightTable ! sq
    kingAtt = kingTable ! sq
    pieces = boardPieces board

    go :: Color -> Bitboard -> PieceType -> Int
    go color block victim = case allChecked of
      Just (attSq, attPiece) ->
        let worthCaptured = pieceWorth victim
            newBlock = clearBit block attSq
         in max (worthCaptured - go opp newBlock attPiece) 0
      Nothing -> 0
      where
        opp = other color
        diagAtt = bishopMovesMagic block sq
        orthoAtt = rookMovesMagic block sq
        mkCheck p bb =
          -- also & block to make sure it hasn't been cleared already (pieces isn't being updated)
          let masked = bb .&. pieceBitboard (Piece color p) pieces .&. block
           in if masked /= 0
                then Just (countTrailingZeros masked, p)
                else Nothing
        allChecked =
          mkCheck Pawn (pawnAtt opp)
            <|> mkCheck Knight knightAtt
            <|> mkCheck Bishop diagAtt
            <|> mkCheck Rook orthoAtt
            <|> mkCheck Queen (diagAtt .|. orthoAtt)
            <|> mkCheck King kingAtt

-- static exchange eval
-- returns Nothing if it's not a capture
seeOfCapture :: Board -> Move -> Maybe Int
-- en passant doesn't capture on the square
-- this is a little fragile because staticExchEval doesn't recognize en passant
-- but because this is always called first and en passant can't happen after a capture
-- it should be technically safe
seeOfCapture !board (Move Pawn (EnPassant target) from to) =
  Just $
    max (pawnWorth - staticExchEval newBoard to (pieceType pieceAttacker)) 0
  where
    pieces = boardPieces board
    pieceAttacker = fromJust (getPiece from pieces)
    newPieces =
      removePiece target $
        addPiece pieceAttacker to $
          removePiece from pieces
    newBoard = board {boardPieces = newPieces, boardTurn = other (boardTurn board)}
seeOfCapture !board move =
  getPiece (moveTo move) (boardPieces board)
    <&> \captured ->
      let pieceAttacker = case moveSpecial move of
            Promotion p -> Piece (boardTurn board) p
            _ -> fromJust (getPiece (moveFrom move) pieces)
          newPieces = addPiece pieceAttacker (moveTo move) (removePiece (moveFrom move) pieces)
          newBoard = board {boardPieces = newPieces, boardTurn = other (boardTurn board)}
          worthCaptured = pieceWorth (pieceType captured)
          promoBonus = case moveSpecial move of
            Promotion p -> pieceWorth p - pieceWorth Pawn
            _ -> 0
       in max (promoBonus + worthCaptured - staticExchEval newBoard (moveTo move) (pieceType pieceAttacker)) 0
  where
    pieces = boardPieces board

-- also considers non-capture promotions
seeOfUnquiet :: Board -> Move -> Maybe Int
seeOfUnquiet board move =
  case moveSpecial move of
    Promotion p ->
      Just $
        fromMaybe
          (pieceWorth p - pieceWorth Pawn)
          (seeOfCapture board move)
    _ ->
      if isMoveQuiet board move
        then Nothing
        else seeOfCapture board move

removeSingle :: (Eq a) => a -> [a] -> [a]
removeSingle _ [] = []
removeSingle r (x : xs)
  | r == x = xs
  | otherwise = x : removeSingle r xs

-- selection for move ordering
singleSelect :: (Ord a) => [(a, Move)] -> ((a, Move), [(a, Move)])
singleSelect moves = (best, removeSingle best moves)
  where
    best = maximumBy (comparing fst) moves
{-# SPECIALIZE singleSelect :: [(MoveScore, Move)] -> ((MoveScore, Move), [(MoveScore, Move)]) #-}

quieSearch :: Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
quieSearch !alpha !beta !game = do
  incNodecount

  -- stand-pat from null-move observation (eval immediately = not moving)
  let staticEval = eval board
  (SearchEnv {sEnvTT = tt}) <- ask
  -- let seeReq = max 0 (alpha - staticEval - 200)
  if staticEval >= beta
    then pure staticEval
    else
      go
        staticEval
        (((\m -> (scoreMove m, m)) <$> allDisquiets board))
  where
    board = gameBoard game

    -- seeOfUnquiet should always be Just for a move
    -- generated by allDisquiets
    scoreMove m = fromJust $ seeOfUnquiet board m

    go :: Int -> [(Int, Move)] -> ReaderT (SearchEnv s) (ST s) Int
    go bestScore [] = pure bestScore
    go bestScore moves = case makeMove game move of
      Just movedGame -> do
        let trueAlpha = max alpha bestScore
        score <- negate <$> quieSearch (-beta) (-trueAlpha) movedGame
        if score >= beta
          then pure score
          else go (max score bestScore) movesRest
      Nothing -> go bestScore movesRest
      where
        ((_, move), movesRest) = singleSelect moves

bestMove :: Int16 -> Game -> ReaderT (SearchEnv s) (ST s) (Int, Move)
bestMove depth game = do
  (SearchEnv {sEnvTT = tt}) <- ask
  -- guess <- maybe 0 (nodeResScore . entryScore) <$> lift (TT.lookup (gameBoard game) tt)
  score <-
    search
      SearchState
        { sStateDepth = depth,
          sStatePly = 0,
          sStateAlpha = minBound `quot` 2,
          sStateBeta = maxBound `quot` 2,
          sStatePV = True,
          sStateGame = game
        }
  maybeEntry <- lift (TT.lookup (gameBoard game) tt)
  case maybeEntry of
    Just (TTEntry {entryMove = move}) -> pure (score, move)
    Nothing -> error "no entry"

-- distinct from searchenv, what??
-- everything that doesn't need to be persistent between siblings
-- could be arguments, but there are a lot
data SearchState = SearchState
  { sStateDepth :: !Int16,
    sStatePly :: !Int16,
    sStateAlpha :: !Int,
    sStateBeta :: !Int,
    sStatePV :: !Bool,
    sStateGame :: !Game
  }
  deriving (Eq, Show)

aspirate :: Int16 -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
aspirate depth !initialGuess !game = go 25 25
  where
    go :: Int -> Int -> ReaderT (SearchEnv s) (ST s) Int
    go lowerMargin upperMargin = do
      result <-
        search
          ( SearchState
              { sStateDepth = depth,
                sStatePly = 0,
                sStateAlpha = lower,
                sStateBeta = upper,
                sStatePV = True,
                sStateGame = game
              }
          )
      if result <= lower
        then go (lowerMargin * 4) upperMargin
        else
          if result >= upper
            then go lowerMargin (upperMargin * 4)
            else pure result
      where
        lower = initialGuess - lowerMargin
        upper = initialGuess + upperMargin

-- least to most:
-- TDOO fillout

newtype MoveScore = MoveScore {unMoveScore :: Int} deriving (Eq, Show, Ord)

-- temp - until we can fill out movescore, this is dump for unclassified
badScore :: MoveScore
badScore = MoveScore minBound

ttScore :: MoveScore
ttScore = MoveScore maxBound

mkSEEScore :: Int -> MoveScore
mkSEEScore seeVal = MoveScore $ seeVal + maxHistory + winWorth

isMoveQuiet :: Board -> Move -> Bool
isMoveQuiet board move = case moveSpecial move of
  Promotion _ -> False
  EnPassant _ -> False
  Normal -> isNothing (getPiece (moveTo move) (boardPieces board))
  _ -> True -- pawn double move, castling are both quiet

scoreMoves :: Board -> [Move] -> ReaderT (SearchEnv s) (ST s) [(MoveScore, Move)]
scoreMoves board moves = do
  SearchEnv {sEnvTT = tt} <- ask
  ttMaybeMove <- lift $ fmap entryMove <$> TT.lookup board tt
  flip traverse moves $ \m ->
    fmap ((,m) . fromMaybe badScore) $ runMaybeT $ do
      let isQuiet = isMoveQuiet board m
      let tryTT = ttMaybeMove >>= (\ttm -> if ttm == m then Just ttScore else Nothing)
      let tryHist =
            if isQuiet
              then do
                SearchEnv {sEnvHistory = history} <- ask
                lift $
                  fmap (Just . MoveScore) $
                    getHistory history (historyIdx (boardTurn board) m)
              else pure Nothing
      let trySEE = mkSEEScore <$> seeOfUnquiet board m
      hoistMaybe tryTT <|> MaybeT tryHist <|> hoistMaybe trySEE

search :: SearchState -> ReaderT (SearchEnv s) (ST s) Int
search
  SearchState
    { sStateDepth = !depth,
      sStatePly = !ply,
      sStateAlpha = !alpha,
      sStateBeta = !beta,
      sStatePV = !isPV,
      sStateGame = !game
    }
    | isDrawn game && ply /= 0 = pure drawWorth
    | depth <= 0 = quieSearch alpha beta game
    | otherwise = do
        incNodecount

        SearchEnv {sEnvTT = tt} <- ask

        scoredMoves <- scoreMoves board (allMoves board)
        (score, move) <- go 0 scoredMoves [] Nothing
        let newEntry = TTEntry (mkNodeResult alpha beta score) move (gameHalfmove game) depth
        -- make sure to save bestmove if root
        if ply == 0
          then lift $ TT.basicInsert board newEntry tt
          else lift $ TT.insert board newEntry tt
        pure score
    where
      board = gameBoard game
      pieces = boardPieces board

      currentlyChecked = inCheck (boardTurn board) (boardPieces board)
      staticEval = eval board

      -- move loop
      -- bestScore for fail-soft
      go ::
        Int -> -- move index
        [(MoveScore, Move)] -> -- remaining moves
        [Move] -> -- quiets that didn't fail high
        Maybe (Int, Move) -> -- current best score/move pair
        ReaderT (SearchEnv s) (ST s) (Int, Move)
      go _ [] _ best = case best of
        Nothing ->
          if currentlyChecked
            -- penalize longer checkmates
            then pure (lossWorth + fromIntegral (gameHalfmove game), NullMove)
            else pure (drawWorth, NullMove)
        Just bestRes -> pure bestRes
      go nth moves failedQuiets best = case makeMove game move of
        Nothing -> go nth movesRest failedQuiets best
        Just moveMade -> do
          -- the search can still be a null-window if a/b started that way
          let searchHelper d addNullWindow =
                negate
                  <$> search
                    ( SearchState
                        { sStateDepth = d,
                          sStatePly = ply + 1,
                          sStateAlpha = -newBeta,
                          sStateBeta = -trueAlpha,
                          sStatePV = newIsPV,
                          sStateGame = moveMade
                        }
                    )
                where
                  newIsPV = isPV && not addNullWindow
                  newBeta =
                    if addNullWindow
                      then trueAlpha + 1
                      else beta
          score <-
            negate
              <$> search
                ( SearchState
                    { sStateDepth = depth - 1,
                      sStatePly = ply + 1,
                      sStateAlpha = -beta,
                      sStateBeta = -trueAlpha,
                      sStatePV = isPV,
                      sStateGame = moveMade
                    }
                )

          if score >= beta
            then do
              when isQuiet $ do
                -- update history
                SearchEnv {sEnvHistory = history} <- ask
                let bonus = fromIntegral $ depth * depth
                let mkKey = historyIdx (boardTurn board)
                lift $ addHistory history bonus (mkKey move)
                lift $ traverse_ (addHistory history (-bonus) . mkKey) failedQuiets
              pure (beta, move)
            else
              let newBest =
                    maybe
                      (score, move)
                      (\b -> if score > fst b then (score, move) else b)
                      best
               in go (nth + 1) movesRest newFailedQuiets (Just newBest)
        where
          -- max of alpha and best;
          -- what alpha would be in a fail-hard search
          trueAlpha = maybe alpha (max alpha . fst) best
          ((moveScore, move), movesRest) = singleSelect moves
          isQuiet = isMoveQuiet board move
          -- we won't ever want to not append to this when the move is quiet
          -- because if it does fail high, we never call go again
          newFailedQuiets
            | isQuiet = move : failedQuiets
            | otherwise = failedQuiets

{-
searchPVS :: SearchState -> Game -> ReaderT (SearchEnv s) (ST s) Int
searchPVS
  sState@( SearchState
             { sStateStartingDepth = !startingDepth,
               sStateDepth = 0,
               sStateAlpha = !alpha,
               sStateBeta = !beta
             }
           )
  !game
    | isDrawn game && startingDepth /= 0 = pure 0
    | inCheck (boardTurn board) (boardPieces board) = searchPVS (sState {sStateDepth = 1}) game
    | otherwise = do
        -- don't incNodecount because quiescence does that on the same game, so it would be double-counting
        (SearchEnv {sEnvTT = tt}) <- ask
        score <- quieSearch alpha beta game
        lift $
          TT.insert
            (gameBoard game)
            (TTEntry (mkNodeResult alpha beta score) NullMove (gameHalfmove game) 0)
            tt
        pure score
    where
      board = gameBoard game
searchPVS
  sState@( SearchState
             { sStateStartingDepth = !startingDepth,
               sStateDepth = !depth,
               sStateAlpha = !alpha,
               sStateBeta = !beta,
               sStatePV = !isPV
             }
           )
  !game
    | depth < 0 = searchPVS (sState {sStateDepth = 0}) game
    | isDrawn game && startingDepth /= depth = pure 0
    | otherwise = do
        incNodecount

        (SearchEnv {sEnvTT = tt}) <- ask
        maybeTTEntry <- lift (TT.lookup board tt)
        prunes <-
          runMaybeT $
            hoistMaybe (checkTTCut maybeTTEntry)
              <|> hoistMaybe checkFutility
              <|> MaybeT checkRazor
              <|> MaybeT checkNullMove
        case prunes of
          Just score -> pure score
          Nothing -> do
            scoredMoves <- scoreMovesOld game (allMoves board)
            (bResult, bMove) <- go 0 scoredMoves [] Nothing
            let newEntry = TTEntry bResult bMove (gameHalfmove game) depth
            -- make sure to save bestmove if root
            if depth == startingDepth
              then lift $ TT.basicInsert board newEntry tt
              else lift $ TT.insert board newEntry tt
            pure (nodeResScore bResult)
    where
      board = gameBoard game
      pieces = boardPieces board

      checkNullMove
        | not isPV && materialScore game >= 1 && depth > 1 = case makeMove game NullMove of
            Just nullGame -> do
              nullScore <-
                negate
                  <$> searchPVS
                    ( sState
                        { sStateDepth = depth * 2 `quot` 3 - 2,
                          sStateAlpha = -beta,
                          sStateBeta = -beta + 1,
                          sStatePV = False
                        }
                    )
                    nullGame
              if nullScore >= beta
                then pure (Just nullScore)
                else pure Nothing
            Nothing -> pure Nothing
        | otherwise = pure Nothing

      currentlyChecked = inCheck (boardTurn board) (boardPieces board)

      staticEval = eval board

      -- unfortunately this breaks on games with length over 100k! oh no!
      scoreIsWinning score = abs (abs score - winWorth) < 100000

      checkFutility
        | not isPV
            -- futility if beta is already checkmate is nonsensical
            && not (scoreIsWinning beta)
            && staticEval >= beta + fromIntegral depth * 150
            && not currentlyChecked =
            Just staticEval
        | otherwise = Nothing

      checkRazor
        | not isPV
            && not (scoreIsWinning alpha)
            && staticEval + fromIntegral depth * fromIntegral depth * 110 <= alpha = do
            quieScore <- quieSearch alpha beta game
            if quieScore <= alpha
              then pure (Just quieScore)
              else pure Nothing
        | otherwise = pure Nothing

      checkTTCut maybeEntry =
        maybeEntry
          >>= \( TTEntry
                   { entryScore = res,
                     entryMove = move,
                     entryHalfmove = halfmove,
                     entryDepth = d
                   }
                 ) ->
              if d >= depth
                && nodeUsable alpha beta res
                -- prevents stalling in endgame by making sure halfmove penalty gets applied
                && not (scoreIsWinning (nodeResScore res) && halfmove /= gameHalfmove game)
                -- sanity check in case of full hash collision
                && maybe False ((== movePiece move) . pieceType) (getPiece (moveFrom move) pieces)
                then Just (nodeResScore res)
                else Nothing

      go :: Int -> [(Int, Move)] -> [Move] -> Maybe (Int, Move) -> ReaderT (SearchEnv s) (ST s) (NodeResult, Move)
      -- no valid moves (stalemate, checkmate checks)
      -- bestScore is nothing if all moves are illegal
      go _ [] _ Nothing
        | currentlyChecked = pure (NodeResult (lossWorth + fromIntegral (gameHalfmove game)) AllNode, NullMove)
        | otherwise = pure (mkNodeResult alpha beta drawWorth, NullMove)
      -- bestScore tracks the best score among moves, but separate from real alpha
      -- this way we keep track of realer score and not alpha cutoff (fail-soft)
      go _ [] _ (Just (bestScore, bMove)) = pure (mkNodeResult alpha beta bestScore, bMove)
      go nth moves quiets best = case makeMove game move of
        Nothing -> go nth movesRest quiets best
        Just moveMade -> do
          let trueAlpha = maybe alpha (max alpha . fst) best
          let search d isNullWindow =
                negate
                  <$> searchPVS
                    ( sState
                        { sStateDepth = d,
                          sStateAlpha = a,
                          sStateBeta = b,
                          sStatePV = pv
                        }
                    )
                    moveMade
                where
                  pv = isPV && not isNullWindow
                  (a, b) =
                    if isNullWindow
                      then (-trueAlpha - 1, -trueAlpha)
                      else (-beta, -trueAlpha)
          nodeScore <-
            if nth == 0
              -- principal variation
              then search (depth - 1) False
              else do
                let isLMR = nth > 2 && depth >= 2
                let reducedDepth =
                      if isLMR
                        then
                          depth
                            - 1
                            - ceiling
                              ( log (fromIntegral (depth + 1) :: Double)
                                  * log (fromIntegral nth)
                                  / 2.5
                              )
                        else depth - 1
                let didReduce = reducedDepth /= depth - 1
                score <- search reducedDepth True
                -- we blew the null window!
                if score >= (trueAlpha + 1)
                  then
                    if isPV
                      then search (depth - 1) False
                      -- don't research with full window if non-pv branch, some older relative will research anyways
                      -- (if not pv, this means we are on a null window so -trueAlpha - 1 == -beta)
                      else
                        if didReduce
                          then search (depth - 1) True
                          else pure score
                  else pure score
          if nodeScore >= beta
            then do
              unless isCapture $ do
                (SearchEnv {sEnvKillers = killers, sEnvHistory = history}) <- ask
                lift $ modifySTRef' killers (addKiller (gameHalfmove game) move)
                let bonus = fromIntegral depth * fromIntegral depth
                lift $
                  addHistory
                    (historyIdx (boardTurn board) move)
                    bonus
                    history
                -- penalize quiets that didn't fail high
                lift $ traverse_ (\q -> addHistory (historyIdx (boardTurn board) q) (-bonus) history) quiets
              pure (NodeResult nodeScore CutNode, move)
            else
              let newQuiets = if isCapture then quiets else move : quiets
               in go (nth + 1) movesRest newQuiets $
                    case best of
                      Just (bScore, _) ->
                        if bScore < nodeScore
                          then Just (nodeScore, move)
                          else best
                      Nothing -> Just (nodeScore, move)
        where
          isCapture = isJust (getPiece (moveTo move) (boardPieces board))
          ((_, move), movesRest) = singleSelect moves
-}
