module Trout.Search
  ( bestMove,
    searchNega,
    eval,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Trout.Bitboard (popCount)
import Trout.Game
  ( Board (..),
    Game (..),
    allMoves,
    inCheck,
    isDrawn,
    makeMove,
    pieceBitboard,
    pieceTypeBitboard,
  )
import Trout.Game.Move (Move (..), nullMove)
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search.Node (NodeResult (..), NodeType (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable (TTEntry (..), TTType)
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth)

-- TODO this kinda sucks
-- get rid of nullMove weirdness too
-- fails horribly when there are no moves

bestMove :: (Monad m, TTType m) => Int -> Game -> m (Int, Move)
bestMove 0 game@(Game {gameBoard = board})
  | isDrawn game = pure (0, head (allMoves board))
  | otherwise =
      pure
        ( colorSign (boardTurn board) * eval game,
          head (allMoves board)
        )
bestMove depth game
  | isDrawn game = pure (0, head (allMoves board))
  | otherwise =
      first
        -- if player is black, flip because negamax is relative
        (* colorSign (boardTurn board))
        <$> go (alpha, nullMove) (allMoves board)
  where
    board = gameBoard game
    alpha = minBound + 1 -- so negate works!!!!!!!
    beta = maxBound
    go best []
      | snd best == nullMove =
          pure $
            if inCheck (boardTurn board) (boardPieces board)
              then (lossWorth, nullMove)
              else (drawWorth, nullMove)
      | otherwise = pure best
    go best@(bestScore, _) (move : moves) = case makeMove game move of
      Nothing -> go best moves
      Just moveMade -> do
        otherScore <- searchNega (depth - 1) (-beta) (-max alpha bestScore) moveMade
        let nodeScore = -otherScore
        if nodeScore > bestScore
          then go (nodeScore, move) moves
          else go best moves

searchNega :: (Monad m, TTType m) => Int -> Int -> Int -> Game -> m Int
searchNega 0 !_ !_ !game
  | isDrawn game = pure 0
  | otherwise = TT.tttypeInsert (gameBoard game) (TTEntry result nullMove 0) $> nodeResScore result
  where
    result = NodeResult (eval game) ExactNode
searchNega depth !alpha !beta !game
  | isDrawn game = pure 0
  | otherwise = do
      let gameMoves = allMoves board
      maybeEntry <- TT.tttypeLookup board
      case maybeEntry of
        Nothing -> do
          (bResult, bMove) <- go Nothing ((0,) <$> gameMoves)
          TT.tttypeInsert board (TTEntry bResult bMove depth)
          pure (nodeResScore bResult)
        Just (TTEntry {entryMove}) ->
          let scoredMoves =
                if entryMove /= nullMove
                  then
                    (1, entryMove) : ((0,) <$> filter (/= entryMove) gameMoves)
                  else (0,) <$> gameMoves
           in do
                (bResult, bMove) <- go Nothing scoredMoves
                TT.tttypeInsert board (TTEntry bResult bMove depth)
                pure (nodeResScore bResult)
  where
    board = gameBoard game
    go :: (Monad m, TTType m) => Maybe (Int, Move) -> [(Int, Move)] -> m (NodeResult, Move)
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
        -- filter out move with best score and get filtered list in one pass
        (movesRest, _, move) =
          foldl'
            ( \(ms, sb, mb) (s, m) ->
                if s > sb
                  then (ms, s, m)
                  else ((s, m) : ms, sb, mb)
            )
            ([], minBound, nullMove)
            moves

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
