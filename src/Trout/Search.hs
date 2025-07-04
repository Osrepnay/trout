module Trout.Search
  ( SearchEnv,
    newEnv,
    clearEnv,
    pvWalk,
    bestMove,
    searchPVS,
    eval,
  )
where

import Control.Monad (join, when)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (find, maximumBy)
import Data.Int (Int16)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isNothing, maybeToList)
import Data.Ord (comparing)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Primitive.Mutable (STVector)
import Data.Vector.Primitive.Mutable qualified as MV
import Trout.Bitboard (popCount, (.|.))
import Trout.Game
  ( Game (..),
    allCaptures,
    allMoves,
    inCheck,
    isDrawn,
    makeMove,
    squareAttackers,
  )
import Trout.Game.Board (Board (..), addPiece, getPiece, pieceBitboard, pieceTypeBitboard, removePiece)
import Trout.Game.Move (Move (..))
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search.Node (NodeResult (..), NodeType (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable (STTranspositionTable, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth, pieceWorth)

maxKillers :: Int
maxKillers = 3

type KillerMap = Map Int16 [Move]

maxHistory :: Int
maxHistory = 1000000000

data SearchEnv s = SearchEnv
  { searchEnvTT :: !(STTranspositionTable s),
    searchEnvKillers :: !(STRef s KillerMap),
    searchEnvHistory :: !(STVector s Int)
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
staticExchEval :: Board -> Int -> Int
staticExchEval board sq = case getPiece sq pieces of
  Just pieceVictim -> case attackers of
    (attackerSq : _) ->
      let pieceAttacker = fromJust (getPiece attackerSq pieces)
          newPieces = addPiece pieceAttacker sq (removePiece attackerSq pieces)
          newBoard = board {boardPieces = newPieces}
          worthCaptured = pieceWorth (pieceType pieceVictim)
       in max (worthCaptured - staticExchEval newBoard sq) 0
    [] -> 0
  Nothing -> 0
  where
    pieces = boardPieces board
    attackers = squareAttackers (boardTurn board) pieces sq

-- static exchange eval
seeOfCapture :: Board -> Move -> Int
seeOfCapture board move = case getPiece (moveTo move) (boardPieces board) of
  Just captured ->
    let pieceAttacker = fromJust (getPiece (moveFrom move) pieces)
        newPieces = addPiece pieceAttacker (moveTo move) (removePiece (moveFrom move) pieces)
        newBoard = board {boardPieces = newPieces}
        worthCaptured = pieceWorth (pieceType captured)
     in max (worthCaptured - staticExchEval newBoard (moveTo move)) 0
  Nothing -> 0
  where
    pieces = boardPieces board

-- 24: all pieces, 0: none
-- pawns don't count, bishops and knights count 1, rooks 2, queens 4
-- taken from pesto/ethereal/fruit
gamePhase :: Game -> Int
gamePhase game =
  popCount (pieceTypeBitboard Knight pieces .|. pieceTypeBitboard Bishop pieces)
    + 2 * popCount (pieceTypeBitboard Rook pieces)
    + 4 * popCount (pieceTypeBitboard Queen pieces)
  where
    board = gameBoard game
    pieces = boardPieces board

eval :: Game -> Int
eval game = colorSign (boardTurn board) * pstEvalValue
  where
    board = gameBoard game
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    mgPhase = gamePhase game
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

-- selection for move ordering
singleSelect :: [(Int, Move)] -> (Move, [(Int, Move)])
singleSelect moves = (snd best, filter (/= best) moves)
  where
    best = maximumBy (comparing fst) moves

quieSearch :: Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
quieSearch !alpha !beta !game
  -- stand-pat from null-move observation (eval immediately = not moving)
  | staticEval >= beta = pure staticEval
  | otherwise = go staticEval (filter ((>= 0) . fst) ((\m -> (seeOfCapture board m, m)) <$> allCaptures board))
  where
    staticEval = eval game
    board = gameBoard game
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
        (move, movesRest) = singleSelect moves

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
      pure (score * colorSign (boardTurn (gameBoard game)), move)
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

nullReduction :: Int16
nullReduction = 3

mkNodeResult :: Int -> Int -> Int -> NodeResult
mkNodeResult alpha beta score
  | score <= alpha = NodeResult score AllNode
  | score >= beta = NodeResult score CutNode
  | otherwise = NodeResult score ExactNode

searchPVS :: Int16 -> Int16 -> Int -> Int -> Bool -> Game -> ReaderT (SearchEnv s) (ST s) Int
searchPVS startingDepth 0 !alpha !beta _ !game
  | isDrawn game && startingDepth /= 0 = pure 0
  | otherwise = do
      (SearchEnv {searchEnvTT = tt}) <- ask
      score <- quieSearch alpha beta game
      lift $
        TT.insert
          (gameBoard game)
          (TTEntry (mkNodeResult alpha beta score) NullMove (gameHalfmove game) 0)
          tt
      pure score
searchPVS startingDepth depth !alpha !beta !isPV !game
  | depth < 0 = searchPVS startingDepth 0 alpha beta isPV game
  | isDrawn game && startingDepth /= depth = pure 0
  | otherwise = do
      -- null move pruning
      nullMoveResult <- checkNullMove
      case nullMoveResult of
        Just res -> pure res
        Nothing -> do
          (SearchEnv {searchEnvTT = tt, searchEnvKillers = killers, searchEnvHistory = history}) <- ask
          maybeTTEntry <- lift (TT.lookup board tt)
          let maybeTTMove = entryMove <$> maybeTTEntry
          case checkTTCut maybeTTEntry of
            Just score -> pure score
            Nothing -> do
              killerM <- lift (readSTRef killers)
              let killerMoves = join $ maybeToList $ M.lookup (gameHalfmove game) killerM
              let gameMoves = allMoves board
              let staticScores =
                    maybeToList ((maxHistory + 100000,) <$> maybeTTMove)
                      ++ ((maxHistory + 1,) <$> killerMoves)
              scoredMoves <- lift $ moveOrderer staticScores history gameMoves
              (bResult, bMove) <- go True Nothing scoredMoves
              lift $ TT.insert board (TTEntry bResult bMove (gameHalfmove game) depth) tt
              pure (nodeResScore bResult)
  where
    board = gameBoard game

    checkNullMove = case makeMove game NullMove of
      Just nullGame -> do
        if gamePhase game >= 1 && not isPV
          then do
            nullScore <- negate <$> searchPVS startingDepth (depth - nullReduction) (-beta) (-beta + 1) isPV nullGame
            if nullScore >= beta
              then pure (Just nullScore)
              else pure Nothing
          else pure Nothing
      Nothing -> pure Nothing

    checkTTCut maybeEntry =
      maybeEntry
        >>= \entry ->
          let nodeScore = entryScore entry
              t = nodeResType nodeScore
              s = nodeResScore nodeScore
              d = entryDepth entry
           in if d >= depth && (t == ExactNode || t == AllNode && s <= alpha || t == CutNode && s >= beta)
                then Just (nodeResScore nodeScore)
                else Nothing

    moveOrderer :: [(Int, Move)] -> STVector s Int -> [Move] -> ST s [(Int, Move)]
    moveOrderer _ _ [] = pure []
    moveOrderer targetMoves history (move : moves) = case targetMatch of
      Just match -> (match :) <$> moveOrderer (filter ((/= move) . snd) targetMoves) history moves
      Nothing -> do
        boring <- boringScore
        ((boring, move) :) <$> moveOrderer targetMoves history moves
      where
        targetMatch = find ((== move) . snd) targetMoves
        seeScore = seeOfCapture board move
        boringScore =
          if seeScore == 0
            then MV.read history (historyIdx move)
            else pure (seeScore + signum seeScore * maxHistory)

    historyIdx move = fromEnum (boardTurn board) * 6 * 64 + fromEnum (movePiece move) * 64 + fromEnum (moveTo move)

    go :: Bool -> Maybe (Int, Move) -> [(Int, Move)] -> ReaderT (SearchEnv s) (ST s) (NodeResult, Move)
    -- no valid moves (stalemate, checkmate checks)
    -- bestScore is nothing if all moves are illegal
    go _ Nothing []
      | inCheck (boardTurn board) (boardPieces board) = pure (NodeResult lossWorth AllNode, NullMove)
      | otherwise = pure (mkNodeResult alpha beta drawWorth, NullMove)
    -- bestScore tracks the best score among moves, but separate from real alpha
    -- this way we keep track of realer score and not alpha cutoff (fail-soft)
    go _ (Just (bestScore, bMove)) [] = pure (mkNodeResult alpha beta bestScore, bMove)
    go leftmost best moves = case makeMove game move of
      Nothing -> go True best movesRest
      Just moveMade -> do
        let trueAlpha = maybe alpha (max alpha . fst) best
        nodeScore <-
          if leftmost
            then negate <$> searchPVS startingDepth (depth - 1) (-beta) (-trueAlpha) isPV moveMade
            else do
              score <- negate <$> searchPVS startingDepth (depth - 1) (-trueAlpha - 1) (-trueAlpha) False moveMade
              -- don't research if non-pv branch, some older relative will research anyways
              if score >= (trueAlpha + 1) && isPV
                then negate <$> searchPVS startingDepth (depth - 1) (-beta) (-trueAlpha) True moveMade
                else pure score
        if nodeScore >= beta
          then do
            -- noncapture
            when (isNothing (getPiece (moveTo move) (boardPieces board))) $ do
              (SearchEnv {searchEnvKillers = killers, searchEnvHistory = history}) <- ask
              lift $
                modifySTRef killers (addKiller (gameHalfmove game) move)
              lift $ MV.modify history (min maxHistory . (+ fromIntegral (depth * depth))) (historyIdx move)
            pure (NodeResult nodeScore CutNode, move)
          else flip (go False) movesRest $
            case best of
              Just (bScore, _) ->
                if bScore < nodeScore
                  then Just (nodeScore, move)
                  else best
              Nothing -> Just (nodeScore, move)
      where
        (move, movesRest) = singleSelect moves
