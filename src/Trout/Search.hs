module Trout.Search
  ( SearchEnv,
    OutOfTime,
    newEnv,
    refreshEnv,
    clearEnv,
    staticExchEval,
    seeOfCapture,
    bestMove,
    EngineMessage (..),
    TimeLimit (..),
    iterativeDeepening,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO)
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Bifunctor (first)
import Data.Foldable (maximumBy, traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.List ((!?))
import Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe)
import Data.Ord (comparing)
import Data.Vector.Primitive ((!))
import Data.Vector.Primitive.Mutable (IOVector)
import Data.Vector.Primitive.Mutable qualified as MV
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
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
import Trout.Game.MoveGen (kingTable, knightTable, pawnCaptureTable)
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Piece (Color (..), Piece (..), PieceType (..), other)
import Trout.Search.Eval (eval, materialScore)
import Trout.Search.Node (NodeResult (..), NodeType (..), mkNodeResult, nodeUsable)
import Trout.Search.TranspositionTable (IOTranspositionTable, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth, pawnWorth, pieceWorth, scoreIsLosing, scoreIsMate, winWorth)

maxPly :: Int16
maxPly = 128

type HistoryTable = IOVector Int

maxHistory :: Int
maxHistory = 10000

historyIdx :: Color -> Move -> Int
historyIdx color move =
  fromEnum color * 6 * 64
    + fromEnum (movePiece move) * 64
    + moveTo move

addHistory :: HistoryTable -> Int -> Int -> IO ()
addHistory history bonus key =
  MV.modify
    history
    (\curr -> curr + bonus - abs bonus * curr `quot` maxHistory)
    key

getHistory :: HistoryTable -> Int -> IO Int
getHistory = MV.read

decayHistory :: HistoryTable -> IO ()
decayHistory history =
  traverse_
    (MV.modify history (\h -> h * 1 `quot` 5))
    [0 .. MV.length history - 1]

-- anything that needs to be carried up through search tree
data SearchEnv = SearchEnv
  { sEnvTT :: !IOTranspositionTable,
    sEnvHistory :: !HistoryTable,
    sEnvNodecount :: !(IORef Int),
    sEnvStartTime :: !(IORef Word64), -- nanoseconds
    sEnvTimeAllotted :: !(IORef Word64)
  }

incNodecount :: ReaderT SearchEnv IO ()
incNodecount = do
  ref <- sEnvNodecount <$> ask
  lift $ modifyIORef' ref (+ 1)

resetNodecount :: ReaderT SearchEnv IO ()
resetNodecount = do
  ref <- sEnvNodecount <$> ask
  lift $ writeIORef ref 0

getNodecount :: ReaderT SearchEnv IO Int
getNodecount = ask >>= (lift . readIORef) . sEnvNodecount

data OutOfTime = OutOfTime deriving (Eq, Show)

instance Exception OutOfTime

-- every x nodes, check the time remaining
timeCheckInterval :: Int
timeCheckInterval = 1000

checkTimeLeft :: ReaderT SearchEnv IO ()
checkTimeLeft = do
  SearchEnv
    { sEnvStartTime = startRef,
      sEnvTimeAllotted = allottedRef
    } <-
    ask
  nodecount <- getNodecount
  timeout <-
    if nodecount `rem` timeCheckInterval == 0
      then lift $ do
        start <- readIORef startRef
        allotted <- readIORef allottedRef
        current <- getMonotonicTimeNSec
        pure (current - start >= allotted)
      else pure False
  when timeout (lift (throwIO OutOfTime))

setStartTime :: ReaderT SearchEnv IO ()
setStartTime = do
  ref <- sEnvStartTime <$> ask
  time <- lift getMonotonicTimeNSec
  lift $ writeIORef ref time

getStartTime :: ReaderT SearchEnv IO Word64
getStartTime = ask >>= lift . readIORef . sEnvStartTime

newEnv :: Int -> IO SearchEnv
newEnv n = do
  tt <- TT.new n
  history <- MV.replicate (2 * 6 * 64) 0
  nodes <- newIORef 0
  startRef <- newIORef 0
  allottedRef <- newIORef maxBound
  pure (SearchEnv tt history nodes startRef allottedRef)

refreshEnv :: ReaderT SearchEnv IO ()
refreshEnv = do
  SearchEnv {sEnvHistory = history} <- ask
  lift $ decayHistory history
  resetNodecount

clearEnv :: SearchEnv -> IO ()
clearEnv (SearchEnv tt history nodes startRef allottedRef) = do
  TT.clear tt
  MV.set history 0
  writeIORef nodes 0
  writeIORef startRef 0
  writeIORef allottedRef 0

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
  Just $ pawnWorth - staticExchEval newBoard to (pieceType pieceAttacker)
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
       in promoBonus + worthCaptured - staticExchEval newBoard (moveTo move) (pieceType pieceAttacker)
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

quieSearch :: Int -> Int -> Game -> ReaderT SearchEnv IO Int
quieSearch !alpha !beta !game = do
  incNodecount

  -- stand-pat from null-move observation (eval immediately = not moving)
  let staticEval = eval board
  (SearchEnv {sEnvTT = tt}) <- ask
  maybeRes <- lift $ fmap entryScore <$> TT.lookup board tt
  case maybeRes >>= \s -> if nodeUsable alpha beta s then Just s else Nothing of
    Just s -> pure (nodeResScore s)
    Nothing ->
      if staticEval >= beta
        then pure staticEval
        else
          go
            staticEval
            ( filter
                ((>= 0) . fst)
                ((\m -> (scoreMove m, m)) <$> allDisquiets board)
            )
  where
    board = gameBoard game

    -- seeOfUnquiet should always be Just for a move
    -- generated by allDisquiets
    scoreMove m = fromJust $ seeOfUnquiet board m

    go :: Int -> [(Int, Move)] -> ReaderT SearchEnv IO Int
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

-- distinct from searchenv, what??
-- everything that doesn't need to be persistent between siblings
-- could be arguments, but there are a lot
data SearchState = SearchState
  { sStateDepth :: !Int16,
    sStatePly :: !Int16,
    sStateAlpha :: !Int,
    sStateBeta :: !Int,
    sStatePV :: !Bool,
    sStateEvalHist :: ![Maybe Int],
    sStateGame :: !Game
  }
  deriving (Eq, Show)

-- order:
-- tt move
-- positive/neutral captures
-- history
-- negative captures

newtype MoveScore = MoveScore {unMoveScore :: Int} deriving (Eq, Show, Ord)

ttScore :: MoveScore
ttScore = MoveScore maxBound

mkSEEScore :: Int -> MoveScore
mkSEEScore seeVal
  | seeVal >= 0 = MoveScore $ seeVal + maxHistory + 1
  | otherwise = MoveScore $ seeVal - maxHistory - 1

isMoveQuiet :: Board -> Move -> Bool
isMoveQuiet board move = case moveSpecial move of
  Promotion _ -> False
  EnPassant _ -> False
  Normal -> isNothing (getPiece (moveTo move) (boardPieces board))
  _ -> True -- pawn double move, castling are both quiet

scoreMoves :: Board -> [Move] -> ReaderT SearchEnv IO [(MoveScore, Move)]
scoreMoves board moves = do
  SearchEnv {sEnvTT = tt} <- ask
  ttMaybeMove <- lift $ fmap entryMove <$> TT.lookup board tt
  flip traverse moves $ \m ->
    fmap ((,m) . fromJust) $ runMaybeT $ do
      let isQuiet = isMoveQuiet board m
      let tryTT = ttMaybeMove >>= \ttm -> if ttm == m then Just ttScore else Nothing
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

search :: SearchState -> ReaderT SearchEnv IO (Int, [Move])
search
  SearchState
    { sStateDepth = !depth,
      sStatePly = !ply,
      sStateAlpha = !alpha,
      sStateBeta = !beta,
      sStatePV = !isPV,
      sStateEvalHist = !evalHist,
      sStateGame = !game
    }
    | isDrawn game && ply /= 0 = pure (drawWorth, [])
    | depth <= 0 || ply >= maxPly = do
        -- don't incNodecount, quiescence does it for the same node
        (SearchEnv {sEnvTT = tt}) <- ask
        score <- quieSearch alpha beta game
        let newEntry = TTEntry (mkNodeResult alpha beta score) NullMove (gameHalfmove game) 0
        lift $ TT.insert (gameBoard game) newEntry tt
        pure (score, [])
    | otherwise = do
        checkTimeLeft
        incNodecount

        SearchEnv {sEnvTT = tt} <- ask
        prunes <-
          runMaybeT $
            pruneTT
              <|> hoistMaybe pruneRFP
              <|> MaybeT pruneRazor
              <|> MaybeT pruneNMP
        case prunes of
          Just pruneScore -> pure (pruneScore, [])
          Nothing -> do
            scoredMoves <- scoreMoves board (allMoves board)
            (score, pvLine) <- go 0 scoredMoves [] Nothing
            let move = fromMaybe NullMove (listToMaybe pvLine)
            let nodeResult = mkNodeResult alpha beta score
            let newEntry = TTEntry nodeResult move (gameHalfmove game) depth
            lift $ TT.insert board newEntry tt
            if nodeResType nodeResult == ExactNode
              then pure (score, pvLine)
              else pure (score, [])
    where
      board = gameBoard game
      pieces = boardPieces board

      currentlyChecked = inCheck (boardTurn board) (boardPieces board)
      staticEval = eval board
      checkedEval = if currentlyChecked then Nothing else Just staticEval
      improving = case join (evalHist !? 1 <|> evalHist !? 3) of
        Nothing -> False
        Just oldEval -> staticEval > oldEval

      pruneTT :: MaybeT (ReaderT SearchEnv IO) Int
      pruneTT = do
        SearchEnv {sEnvTT = tt} <- lift ask
        TTEntry
          { entryScore = res,
            entryMove = move,
            entryDepth = d
          } <-
          MaybeT $ lift $ TT.lookup board tt
        MaybeT $
          pure $
            if not isPV
              && d >= depth
              && nodeUsable alpha beta res
              -- sanity check in case of full hash collision
              && maybe False ((== movePiece move) . pieceType) (getPiece (moveFrom move) pieces)
              then Just (nodeResScore res)
              else Nothing

      pruneRFP :: Maybe Int
      pruneRFP
        | not isPV
            && depth <= 6
            && staticEval >= beta + rfpMargin
            && not currentlyChecked =
            Just staticEval
        | otherwise = Nothing
        where
          rfpMargin =
            if improving
              then fromIntegral depth * 80
              else fromIntegral depth * 110

      pruneRazor :: ReaderT SearchEnv IO (Maybe Int)
      pruneRazor
        | not isPV
            && not (scoreIsMate alpha)
            && staticEval + razorMargin <= alpha = do
            quieScore <- quieSearch alpha beta game
            if quieScore <= alpha
              then pure (Just quieScore)
              else pure Nothing
        | otherwise = pure Nothing
        where
          razorMargin = fromIntegral depth * fromIntegral depth * 110

      pruneNMP :: ReaderT SearchEnv IO (Maybe Int)
      pruneNMP
        | not isPV
            && materialScore game >= 1
            && staticEval >= beta =
            case makeMove game NullMove of
              Just nullGame -> do
                (nullScore, _) <-
                  first negate
                    <$> search
                      SearchState
                        { sStateDepth = depth - reduction,
                          sStatePly = ply + 1,
                          sStateAlpha = -beta,
                          sStateBeta = -(beta - 1),
                          sStatePV = False,
                          sStateEvalHist = checkedEval : evalHist,
                          sStateGame = nullGame
                        }
                if nullScore >= beta
                  then
                    if scoreIsMate nullScore
                      then pure (Just beta)
                      else pure (Just nullScore)
                  else pure Nothing
              Nothing -> pure Nothing
        | otherwise = pure Nothing
        where
          reduction = 3 + depth `quot` 3

      lmpLimit =
        if improving
          then 3 + 2 * fromIntegral depth * fromIntegral depth
          else 3 + fromIntegral depth * fromIntegral depth

      -- all the conditions except for isQuiet
      doFutility =
        not isPV
          && depth <= 7
          && staticEval + 400 + fromIntegral depth * 100 <= alpha
          && not currentlyChecked

      -- move loop
      -- bestScore for fail-soft
      go ::
        Int -> -- move index
        [(MoveScore, Move)] -> -- remaining moves
        [Move] -> -- quiets that didn't fail high
        Maybe (Int, [Move]) -> -- current best score/move pair
        ReaderT SearchEnv IO (Int, [Move])
      go _ [] _ best = case best of
        Nothing ->
          if currentlyChecked
            -- penalize longer checkmates
            then pure (lossWorth + fromIntegral (gameHalfmove game), [])
            else pure (drawWorth, [])
        Just bestRes -> pure bestRes
      go nth moves failedQuiets best
        -- late move pruning
        | not isPV
            && isQuiet
            && hasUsableMove
            && nth > lmpLimit =
            go nth [] failedQuiets best
        | isQuiet
            && hasUsableMove
            && doFutility =
            go (nth + 1) movesRest newFailedQuiets best
        | otherwise = case makeMove game move of
            Nothing -> go nth movesRest failedQuiets best
            Just moveMade -> do
              -- the search can still be a null-window if a/b started that way
              let searchHelper d addNullWindow =
                    first negate
                      <$> search
                        SearchState
                          { sStateDepth = d,
                            sStatePly = ply + 1,
                            sStateAlpha = -newBeta,
                            sStateBeta = -trueAlpha,
                            sStatePV = newIsPV,
                            sStateEvalHist = checkedEval : evalHist,
                            sStateGame = moveMade
                          }
                    where
                      newIsPV = isPV && not addNullWindow
                      newBeta =
                        if addNullWindow
                          then trueAlpha + 1
                          else beta
              let lmrReduction =
                    if depth < 3 || nth < 3
                      then 0
                      else
                        ceiling
                          ((log (fromIntegral (depth + 1)) * log (fromIntegral nth) / 2.5) :: Double)
              -- if both lmrReduction > 0 and isPV are true, will re-search twice
              -- if lmrReduction > 0 but !isPV, will re-search w/ basic search
              -- (which is the same as nwScore, addNullWindow has no effect if !isPV)
              -- if there is no reduction but isPV,
              -- will search nwScore and fallback to basic search
              let reducedNWScore =
                    if nth > 0 && lmrReduction > 0
                      then
                        searchHelper (depth - 1 - lmrReduction) True <&> \s ->
                          if fst s > trueAlpha
                            then Nothing
                            else Just s
                      else pure Nothing
              let nwScore =
                    if nth > 0 && isPV
                      then
                        searchHelper (depth - 1) True <&> \s ->
                          if fst s > trueAlpha
                            then Nothing
                            else Just s
                      else pure Nothing
              (score, pvLine) <-
                runMaybeT (MaybeT reducedNWScore <|> MaybeT nwScore)
                  >>= maybe (searchHelper (depth - 1) False) pure

              if score >= beta
                then do
                  when isQuiet $ do
                    -- update history
                    SearchEnv {sEnvHistory = history} <- ask
                    let bonus = fromIntegral $ depth * depth
                    let mkKey = historyIdx (boardTurn board)
                    lift $ addHistory history bonus (mkKey move)
                    lift $ traverse_ (addHistory history (-bonus) . mkKey) failedQuiets
                  pure (score, move : pvLine)
                else
                  let newBest =
                        maybe
                          (score, move : pvLine)
                          (\b -> if score > fst b then (score, move : pvLine) else b)
                          best
                   in go (nth + 1) movesRest newFailedQuiets (Just newBest)
        where
          -- max of alpha and best;
          -- what alpha would be in a fail-hard search
          trueAlpha = maybe alpha (max alpha . fst) best
          ((_moveScore, move), movesRest) = singleSelect moves
          isQuiet = isMoveQuiet board move
          -- we won't ever want to not append to this when the move is quiet
          -- because if it does fail high, we never call go again
          newFailedQuiets
            | isQuiet = move : failedQuiets
            | otherwise = failedQuiets
          -- has a legal move that doesn't just go to checkmate
          hasUsableMove = maybe False (not . scoreIsLosing . fst) best

aspirate :: Int16 -> Int -> Game -> ReaderT SearchEnv IO (Int, [Move])
aspirate depth !initialGuess !game =
  go initialGuess initialGuess initialMargin initialMargin
  where
    initialMargin
      | depth <= 0 = abs initialGuess + winWorth + 1
      | otherwise = max 5 (300 `quot` fromIntegral depth)
    go :: Int -> Int -> Int -> Int -> ReaderT SearchEnv IO (Int, [Move])
    go lowerBound upperBound lowerMargin upperMargin = do
      (result, pvLine) <-
        search
          ( SearchState
              { sStateDepth = depth,
                sStatePly = 0,
                sStateAlpha = alpha,
                sStateBeta = beta,
                sStatePV = True,
                sStateEvalHist = [],
                sStateGame = game
              }
          )
      if result <= alpha
        then go result upperBound (lowerMargin * 2) upperMargin
        else
          if result >= beta
            then go lowerBound result lowerMargin (upperMargin * 2)
            else pure (result, pvLine)
      where
        alpha = lowerBound - lowerMargin
        beta = upperBound + upperMargin

bestMove :: Int16 -> Game -> ReaderT SearchEnv IO (Int, [Move])
bestMove depth game = do
  SearchEnv {sEnvTT = tt} <- ask
  guess <- maybe 0 (nodeResScore . entryScore) <$> lift (TT.lookup (gameBoard game) tt)
  (score, pvLine) <- aspirate depth guess game
  lift $ insertAll tt score 1 depth game pvLine
  pure (score, pvLine)
  where
    insertAll :: IOTranspositionTable -> Int -> Int -> Int16 -> Game -> [Move] -> IO ()
    insertAll _ _ _ _ _ [] = pure ()
    insertAll tt score mult d g (m : moves) = do
      let entry =
            TTEntry
              { entryScore = NodeResult (mult * score) ExactNode,
                entryMove = m,
                entryHalfmove = gameHalfmove g,
                entryDepth = d
              }
      TT.basicInsert (gameBoard g) entry tt
      insertAll tt score (-mult) (d - 1) (fromJust (makeMove g m)) moves

-- will there ever be more entries?
data EngineMessage
  = MsgInfo
      Int16 -- depth
      Int -- evaluation
      Word64 -- total time (ns)
      Int -- nodes searched
      Int -- nps
      [Move] -- pv

-- exact means search for exactly that time, no more no less (roughly)
data TimeLimit = NoTimeLimit | TimeLimit Word64 | ExactTimeLimit Word64
  deriving (Eq, Show)

iterativeDeepening :: TimeLimit -> Int16 -> Game -> (EngineMessage -> IO ()) -> ReaderT SearchEnv IO (Int, [Move])
iterativeDeepening timeLimit maxDepth game messageCb = do
  setStartTime
  startTimeNs <- getStartTime
  SearchEnv {sEnvTimeAllotted = allottedVar} <- ask
  lift $ writeIORef allottedVar maxTimeNs
  go startTimeNs 1
  where
    (maxTimeNs, doSoftBound) = case timeLimit of
      NoTimeLimit -> (maxBound, False)
      TimeLimit t -> (t, True)
      ExactTimeLimit t -> (t, False)
    go startTimeNs d = do
      (score, pv) <- bestMove d game
      timeElapsedNs <- lift $ subtract startTimeNs <$> getMonotonicTimeNSec
      nodecount <- getNodecount
      let nps = fromIntegral $ (1_000_000_000 * fromIntegral nodecount) `quot` timeElapsedNs
      lift $ messageCb (MsgInfo d score timeElapsedNs nodecount nps pv)

      let elapsedFrac :: Double = fromIntegral timeElapsedNs / fromIntegral maxTimeNs
      -- soft bound
      if d >= maxDepth || elapsedFrac > 0.5 && doSoftBound
        then pure (score, pv)
        else go startTimeNs (d + 1)
