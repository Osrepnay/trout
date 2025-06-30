module Trout.Search
  ( SearchEnv,
    newEnv,
    clearEnv,
    pvWalk,
    bestMove,
    searchNega,
    eval,
  )
where

import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Trout.Bitboard (popCount)
import Trout.Game
  ( Board (..),
    Game (..),
    addPiece,
    allCaptures,
    allMoves,
    getPiece,
    inCheck,
    isDrawn,
    makeMove,
    pieceBitboard,
    pieceTypeBitboard,
    removePiece,
    squareAttackers,
  )
import Trout.Game.Move (Move (..), nullMove)
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search.Node (NodeResult (..), NodeType (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable (STTranspositionTable, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth, pieceWorth)

newtype SearchEnv s = SearchEnv
  { searchStateTT :: STTranspositionTable s
  }

newEnv :: Int -> ST s (SearchEnv s)
newEnv n = SearchEnv <$> TT.new n

clearEnv :: SearchEnv s -> ST s ()
clearEnv (SearchEnv tt) = TT.clear tt

-- (attempt to) finid the pv (the tt might have been overwritten)
pvWalk :: Game -> ReaderT (SearchEnv s) (ST s) [Move]
pvWalk game = go game Nothing
  where
    go _ (Just 0) = pure []
    go g maybeDepth = do
      (SearchEnv {searchStateTT = tt}) <- ask
      maybeEntry <- lift (TT.lookup (gameBoard g) tt)
      case maybeEntry of
        Just (TTEntry {entryMove = move, entryDepth = depth}) ->
          if maybe True (depth ==) maybeDepth && move /= nullMove
            then case makeMove g move of
              Just movedG -> (move :) <$> go movedG (Just (fromMaybe depth maybeDepth - 1))
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

eval :: Game -> Int
eval game = colorSign (boardTurn board) * pstEvalValue
  where
    board = gameBoard game
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    -- calculate game phase
    -- pawns don't count, bishops and rooks count 1, rooks 2, queens 4
    -- taken from pesto/ethereal/fruit
    mgPhase =
      popCount (pieceTypeBitboard Knight pieces)
        + popCount (pieceTypeBitboard Bishop pieces)
        + 2 * popCount (pieceTypeBitboard Rook pieces)
        + 4 * popCount (pieceTypeBitboard Queen pieces)
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
singleSelect moves =
  fst $
    foldl'
      ( \((moveBest, ms), scoreBest) (score, m) ->
          if score > scoreBest
            then ((m, ms), score)
            else ((moveBest, (score, m) : ms), scoreBest)
      )
      ((nullMove, []), minBound)
      moves

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

bestMove :: Int -> Game -> ReaderT (SearchEnv s) (ST s) (Int, Move)
bestMove depth game = do
  (SearchEnv {searchStateTT = tt}) <- ask
  -- guess <- maybe 0 (nodeResScore . entryNode) <$> lift (TT.lookup (gameBoard game) tt)
  -- _ <- aspirate depth guess game
  _ <- searchNega depth (minBound + 1) maxBound game
  maybeEntry <- lift (TT.lookup (gameBoard game) tt)
  case maybeEntry of
    Just (TTEntry {entryNode = node, entryMove = move}) ->
      pure (nodeResScore node * colorSign (boardTurn (gameBoard game)), move)
    Nothing -> error "no entry"

_mtdf :: Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
_mtdf depth !initialGuess !game = go (minBound + 1) maxBound initialGuess
  where
    go :: Int -> Int -> Int -> ReaderT (SearchEnv s) (ST s) Int
    go lower upper guess
      | lower < upper = do
          let beta = if lower == guess then guess + 1 else guess
          newGuess <- searchNega depth (beta - 1) beta game
          if newGuess < beta
            then go lower newGuess newGuess
            else go newGuess upper newGuess
      | otherwise = pure guess

_aspirate :: Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
_aspirate depth !initialGuess !game = go 50 50
  where
    go :: Int -> Int -> ReaderT (SearchEnv s) (ST s) Int
    go lowerMargin upperMargin = do
      result <- searchNega depth lower upper game
      if result <= lower
        then go (lowerMargin * 2) upperMargin
        else
          if result >= upper
            then go lowerMargin (upperMargin * 2)
            else pure result
      where
        lower = initialGuess - lowerMargin
        upper = initialGuess + upperMargin

searchNega :: Int -> Int -> Int -> Game -> ReaderT (SearchEnv s) (ST s) Int
searchNega 0 !alpha !beta !game
  | isDrawn game = pure 0
  | otherwise = do
      (SearchEnv {searchStateTT = tt}) <- ask
      score <- quieSearch alpha beta game
      let nodeType
            | score >= beta = CutNode
            | score <= alpha = AllNode
            | otherwise = ExactNode
      let node = NodeResult score nodeType
      lift $ TT.insert (gameBoard game) (TTEntry node nullMove 0) tt
      pure score
searchNega depth !alpha !beta !game
  | isDrawn game = pure 0
  | otherwise = do
      (SearchEnv {searchStateTT = tt}) <- ask
      let gameMoves = allMoves board
      maybeEntry <- lift (TT.lookup board tt)
      let scoredMoves = case maybeEntry of
            Nothing -> (0,) <$> gameMoves
            Just (TTEntry {entryMove}) ->
              if entryMove /= nullMove
                then
                  (100000, entryMove) : ((\m -> (seeOfCapture board m, m)) <$> filter (/= entryMove) gameMoves)
                else (0,) <$> gameMoves
      (bResult, bMove) <- go Nothing scoredMoves
      lift (TT.insert board (TTEntry bResult bMove depth) tt)
      pure (nodeResScore bResult)
  where
    board = gameBoard game
    go :: Maybe (Int, Move) -> [(Int, Move)] -> ReaderT (SearchEnv s) (ST s) (NodeResult, Move)
    -- no valid moves (stalemate, checkmate checks)
    -- bestScore is nothing if all moves are illegal
    go Nothing []
      | inCheck (boardTurn board) (boardPieces board) = pure (NodeResult lossWorth ExactNode, nullMove)
      | otherwise = pure (NodeResult drawWorth ExactNode, nullMove)
    -- bestScore tracks the best score among moves, but separate from real alpha
    -- this way we keep track of realer score and not alpha cutoff (fail-soft)
    -- <----------|---------------|--------->
    --  AllNode alpha ExactNode beta CutNode
    -- bestScore can be anywhere on the number line left of beta
    -- ExactNode is inclusive on alpha, because bestScore is exact even if it touches alpha
    -- TODO make sure that fail-soft behavior doesn't fiddle with bestScore too weirdly
    go (Just (bestScore, bMove)) []
      -- normally alpha is set if bestScore > alpha, but technically the score is exact
      -- TODO recheck thsi behavior
      | bestScore >= alpha = pure (NodeResult bestScore ExactNode, bMove)
      | otherwise = pure (NodeResult bestScore AllNode, bMove)
    go best moves = case makeMove game move of
      Nothing -> go best movesRest
      Just moveMade -> do
        otherScore <-
          searchNega
            (depth - 1)
            (-beta)
            (-maybe alpha (max alpha . fst) best)
            moveMade
        let nodeScore = -otherScore
        -- fail-high, move is too good - parent node shouldn't play this move
        if nodeScore >= beta
          then pure (NodeResult nodeScore CutNode, move)
          else flip go movesRest $
            case best of
              Just (bScore, _) ->
                if bScore < nodeScore
                  then Just (nodeScore, move)
                  else best
              Nothing -> Just (nodeScore, move)
      where
        (move, movesRest) = singleSelect moves
