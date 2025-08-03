module Trout.Search
  ( SearchEnv,
    newEnv,
    clearEnv,
    pvWalk,
    staticExchEval,
    seeOfCapture,
    bestMove,
    searchPVS,
    eval,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join, unless)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (find, maximumBy, traverse_)
import Data.Functor ((<&>))
import Data.Int (Int16)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust, maybeToList)
import Data.Ord (comparing)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Primitive ((!))
import Data.Vector.Primitive.Mutable (STVector)
import Data.Vector.Primitive.Mutable qualified as MV
import Trout.Bitboard (Bitboard, clearBit, countTrailingZeros, popCount, (!<<.), (!>>.), (.&.), (.|.))
import Trout.Game
  ( Game (..),
    allDisquiets,
    allMoves,
    inCheck,
    isDrawn,
    makeMove,
    mobility,
  )
import Trout.Game.Board
  ( Board (..),
    Pieces,
    addPiece,
    colorOccupancy,
    getPiece,
    occupancy,
    pieceBitboard,
    pieceTypeBitboard,
    removePiece,
  )
import Trout.Game.Move (Move (..), SpecialMove (EnPassant))
import Trout.Game.MoveGen (SpecialMove (Promotion), kingTable, knightTable, pawnCaptureTable)
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign, other)
import Trout.Search.Node (NodeResult (..), NodeType (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable (STTranspositionTable, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth, pawnWorth, pieceWorth, winWorth)

maxKillers :: Int
maxKillers = 3

type KillerMap = Map Int16 [Move]

maxHistory :: Int
maxHistory = 1000

type HistoryTable s = STVector s Int

data SearchEnv s = SearchEnv
  { searchEnvTT :: !(STTranspositionTable s),
    searchEnvKillers :: !(STRef s KillerMap),
    searchEnvHistory :: !(HistoryTable s)
  }

newEnv :: Int -> ST s (SearchEnv s)
newEnv n = do
  tt <- TT.new n
  killers <- newSTRef M.empty
  history <- MV.replicate (2 * 6 * 64) 0
  pure (SearchEnv tt killers history)

clearEnv :: SearchEnv s -> ST s ()
clearEnv (SearchEnv tt killers history) = do
  TT.clear tt
  writeSTRef killers M.empty
  MV.set history 0

-- (attempt to) finid the pv (the tt might have been overwritten)
pvWalk :: Game -> ReaderT (SearchEnv s) (ST s) [Move]
pvWalk game = go game Nothing
  where
    go _ (Just 0) = pure []
    go g maybeDepth = do
      (SearchEnv {searchEnvTT = tt}) <- ask
      maybeEntry <- lift (TT.lookup (gameBoard g) tt)
      case maybeEntry of
        Just (TTEntry {entryMove = move, entryDepth = depth}) ->
          if maybe True (depth ==) maybeDepth && move /= NullMove
            then case makeMove g move of
              Just movedG -> (move :) <$> go movedG (Just (depth - 1))
              Nothing -> pure [] -- should be rare, this means full tt collision
            else pure []
        Nothing -> pure []

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

-- 24: all pieces, 0: none
-- pawns don't count, bishops and knights count 1, rooks 2, queens 4
-- taken from pesto/ethereal/fruit
totalMaterialScore :: Game -> Int
totalMaterialScore game =
  popCount (pieceTypeBitboard Knight pieces .|. pieceTypeBitboard Bishop pieces)
    + 2 * popCount (pieceTypeBitboard Rook pieces)
    + 4 * popCount (pieceTypeBitboard Queen pieces)
  where
    board = gameBoard game
    pieces = boardPieces board

-- for current side
materialScore :: Game -> Int
materialScore game =
  popCount (getBB Knight .|. getBB Bishop)
    + 2 * popCount (getBB Rook)
    + 4 * popCount (getBB Queen)
  where
    getBB p = pieceBitboard (Piece turn p) pieces
    board = gameBoard game
    pieces = boardPieces board
    turn = boardTurn board

virtMobile :: Color -> Pieces -> Int
virtMobile color pieces = popCount movez
  where
    block = colorOccupancy color pieces
    king = pieceBitboard (Piece color King) pieces
    kingSq = countTrailingZeros king
    movez = bishopMovesMagic block kingSq .|. rookMovesMagic block kingSq

blockedPawns :: Color -> Pieces -> Int
blockedPawns color pieces = popCount (pieceBitboard (Piece color Pawn) pieces `shiftOp` 8 .&. occ)
  where
    occ = occupancy pieces
    shiftOp = case color of
      White -> (!<<.)
      Black -> (!>>.)

eval :: Game -> Int
eval game = colorSign (boardTurn board) * (pstEvalValue + mobilityValue + scaledKingSafety + blockedPawnsValue)
  where
    board = gameBoard game
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    mgPhase = totalMaterialScore game
    egPhase = 24 - mgPhase
    pst bb p = pstEval bb p mgPhase egPhase
    pstEvalValue =
      pst (getBB White Pawn) Pawn 0
        - pst (getBB Black Pawn) Pawn 56
        + pst (getBB White Knight) Knight 0
        - pst (getBB Black Knight) Knight 56
        + pst (getBB White Bishop) Bishop 0
        - pst (getBB Black Bishop) Bishop 56
        + pst (getBB White Rook) Rook 0
        - pst (getBB Black Rook) Rook 56
        + pst (getBB White Queen) Queen 0
        - pst (getBB Black Queen) Queen 56
        + pst (getBB White King) King 0
        - pst (getBB Black King) King 56

    mobilityPieces = [Knight, Bishop, Rook]
    mobilityValue =
      sum
        [ colorSign c * mobility board (Piece c p)
        | c <- [White, Black],
          p <- mobilityPieces
        ]

    kingSafety = virtMobile Black pieces - virtMobile White pieces
    scaledKingSafety = kingSafety * mgPhase `quot` 24 * 3

    blockedPawnsValue = 5 * (blockedPawns Black pieces - blockedPawns White pieces)

-- selection for move ordering
singleSelect :: [(Int, Move)] -> ((Int, Move), [(Int, Move)])
singleSelect moves = (best, filter (/= best) moves)
  where
    best = maximumBy (comparing fst) moves

quieSearch :: Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
quieSearch !alpha !beta !game
  -- stand-pat from null-move observation (eval immediately = not moving)
  | staticEval >= beta = pure staticEval
  | otherwise =
      go
        staticEval
        -- seeOfCapture should never be maybe because it's captures only
        (filter ((>= 0) . fst) ((\m -> (scoreMove m, m)) <$> allDisquiets board))
  where
    staticEval = eval game
    board = gameBoard game

    scoreMove m = case seeOfCapture board m of
      Just s -> s
      Nothing -> case moveSpecial m of
        -- non-capture promotions
        -- TODO maybe throw SEE on here too?
        (Promotion p) -> pieceWorth p - pieceWorth Pawn
        -- should be impossible
        _ -> lossWorth

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

cleanKillers :: Int16 -> KillerMap -> KillerMap
cleanKillers currHalfmove killerMap = case M.lookupMin killerMap of
  Just (minHalfmove, _) ->
    if minHalfmove < currHalfmove
      then cleanKillers currHalfmove (M.delete minHalfmove killerMap)
      else killerMap
  Nothing -> killerMap

bestMove :: Int16 -> Game -> ReaderT (SearchEnv s) (ST s) (Int, Move)
bestMove depth game = do
  (SearchEnv {searchEnvTT = tt, searchEnvKillers = killersRef}) <- ask
  lift $ modifySTRef killersRef (cleanKillers (gameHalfmove game))
  guess <- maybe 0 (nodeResScore . entryScore) <$> lift (TT.lookup (gameBoard game) tt)
  score <- aspirate depth guess game
  maybeEntry <- lift (TT.lookup (gameBoard game) tt)
  case maybeEntry of
    Just (TTEntry {entryMove = move}) ->
      pure (score, move)
    Nothing -> error "no entry"

aspirate :: Int16 -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
aspirate depth !initialGuess !game = go 25 25
  where
    go :: Int -> Int -> ReaderT (SearchEnv s) (ST s) Int
    go lowerMargin upperMargin = do
      result <- searchPVS depth depth lower upper True game
      if result <= lower
        then go (lowerMargin * 4) upperMargin
        else
          if result >= upper
            then go lowerMargin (upperMargin * 4)
            else pure result
      where
        lower = initialGuess - lowerMargin
        upper = initialGuess + upperMargin

-- may or may not actually add the killer, only attempts to add based on replacement strategy
addKiller :: Int16 -> Move -> KillerMap -> KillerMap
addKiller halfmove move =
  M.alter
    ( \maybeKillers ->
        let killerList = join (maybeToList maybeKillers)
         in if move `elem` killerList
              then Just killerList
              else Just $ move : trimEnd killerList
    )
    halfmove
  where
    trimEnd xs
      | length xs == maxKillers = init xs
      | otherwise = xs

historyIdx :: Color -> Move -> Int
historyIdx color move =
  fromEnum color * 6 * 64
    + fromEnum (movePiece move) * 64
    + fromEnum (moveTo move)

addHistory :: Int -> Int -> HistoryTable s -> ST s ()
addHistory key bonus history =
  MV.modify
    history
    -- curr + bonus * abs (signum bonus - curr `quot` maxHistory)
    (\curr -> curr + signum bonus * abs (bonus - abs bonus * curr `quot` maxHistory))
    key

nullReduction :: Int16
nullReduction = 3

mkNodeResult :: Int -> Int -> Int -> NodeResult
mkNodeResult alpha beta score
  | score <= alpha = NodeResult score AllNode
  | score >= beta = NodeResult score CutNode
  | otherwise = NodeResult score ExactNode

searchPVS :: Int16 -> Int16 -> Int -> Int -> Bool -> Game -> ReaderT (SearchEnv s) (ST s) Int
searchPVS startingDepth 0 !alpha !beta !isPV !game
  | isDrawn game && startingDepth /= 0 = pure 0
  | inCheck (boardTurn board) (boardPieces board) = searchPVS startingDepth 1 alpha beta isPV game
  | otherwise = do
      (SearchEnv {searchEnvTT = tt}) <- ask
      score <- quieSearch alpha beta game
      lift $
        TT.insert
          (gameBoard game)
          (TTEntry (mkNodeResult alpha beta score) NullMove (gameHalfmove game) 0)
          tt
      pure score
  where
    board = gameBoard game
searchPVS startingDepth depth !alpha !beta !isPV !game
  | depth < 0 = searchPVS startingDepth 0 alpha beta isPV game
  | isDrawn game && startingDepth /= depth = pure 0
  | otherwise = do
      (SearchEnv {searchEnvTT = tt, searchEnvKillers = killers, searchEnvHistory = history}) <- ask
      maybeTTEntry <- lift (TT.lookup board tt)
      let maybeTTMove = entryMove <$> maybeTTEntry
      case checkTTCut maybeTTEntry of
        Just score -> pure score
        Nothing -> do
          case checkFutility of
            Just res -> pure res
            Nothing -> do
              -- null move pruning
              nullMoveResult <- checkNullMove
              case nullMoveResult of
                Just res -> pure res
                Nothing -> do
                  killerMap <- lift (readSTRef killers)
                  let killerMoves = join $ maybeToList (M.lookup (gameHalfmove game) killerMap)
                  let staticScores =
                        maybeToList ((maxHistory + 100000,) <$> maybeTTMove)
                          ++ ((maxHistory + 1,) <$> killerMoves)
                  scoredMoves <- lift $ moveOrderer staticScores history gameMoves
                  (bResult, bMove) <- go 0 scoredMoves [] Nothing
                  let newEntry = TTEntry bResult bMove (gameHalfmove game) depth
                  -- make sure to save bestmove if root
                  if depth == startingDepth
                    then lift $ TT.basicInsert board newEntry tt
                    else lift $ TT.insert board newEntry tt
                  pure (nodeResScore bResult)
  where
    board = gameBoard game

    gameMoves = allMoves board

    checkNullMove = case makeMove game NullMove of
      Just nullGame -> do
        if not isPV && materialScore game >= 1
          then do
            nullScore <- negate <$> searchPVS startingDepth (depth - nullReduction) (-beta) (-beta + 1) False nullGame
            if nullScore >= beta
              then pure (Just nullScore)
              else pure Nothing
          else pure Nothing
      Nothing -> pure Nothing

    currentlyChecked = inCheck (boardTurn board) (boardPieces board)

    staticEval = eval game

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

    checkTTCut maybeEntry =
      maybeEntry
        >>= \( TTEntry
                 { entryScore = (NodeResult {nodeResType = t, nodeResScore = s}),
                   entryMove = move,
                   entryHalfmove = halfmove,
                   entryDepth = d
                 }
               ) ->
            if move `elem` gameMoves
              && d >= depth
              && (t == ExactNode || t == AllNode && s <= alpha || t == CutNode && s >= beta)
              -- prevents stalling in endgame by making sure halfmove penalty gets applied
              && not (scoreIsWinning s && halfmove /= gameHalfmove game)
              then Just s
              else Nothing

    -- ordering: tt, neutral and positive captures, quiets (killer then history), negative captures
    moveOrderer :: [(Int, Move)] -> HistoryTable s -> [Move] -> ST s [(Int, Move)]
    moveOrderer _ _ [] = pure []
    moveOrderer targetMoves history (move : moves) = case targetMatch of
      Just match -> (match :) <$> moveOrderer (filter ((/= move) . snd) targetMoves) history moves
      Nothing -> do
        boring <- boringScore
        ((boring, move) :) <$> moveOrderer targetMoves history moves
      where
        targetMatch = find ((== move) . snd) targetMoves
        -- put neutral captures ahead of noncaptures and killers (1)
        seeScore = maybe 0 (+ 2) (seeOfCapture board move)
        boringScore =
          if seeScore == 0
            then MV.read history (historyIdx (boardTurn board) move)
            else pure (seeScore + signum seeScore * maxHistory)

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
        nodeScore <-
          if nth == 0
            then negate <$> searchPVS startingDepth (depth - 1) (-beta) (-trueAlpha) isPV moveMade
            else do
              let isLMR = nth > 2 && depth >= 2
              let newDepth =
                    if isLMR
                      then
                        if moveScore > maxHistory
                          then depth - 2
                          else depth - 3
                      else depth - 1
              score <- negate <$> searchPVS startingDepth newDepth (-trueAlpha - 1) (-trueAlpha) False moveMade
              if score >= (trueAlpha + 1)
                then
                  if isPV
                    then negate <$> searchPVS startingDepth (depth - 1) (-beta) (-trueAlpha) True moveMade
                    -- don't research with full window if non-pv branch, some older relative will research anyways
                    -- (if not pv, this means we are on a null window so -trueAlpha - 1 == -beta)
                    else
                      if isLMR
                        then negate <$> searchPVS startingDepth (depth - 1) (-trueAlpha - 1) (-trueAlpha) False moveMade
                        else pure score
                else pure score
        if nodeScore >= beta
          then do
            unless isCapture $ do
              (SearchEnv {searchEnvKillers = killers, searchEnvHistory = history}) <- ask
              lift $ modifySTRef killers (addKiller (gameHalfmove game) move)
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
        ((moveScore, move), movesRest) = singleSelect moves
