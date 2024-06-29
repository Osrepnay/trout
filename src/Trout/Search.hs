module Trout.Search
  ( bestMove,
    searchNega,
    eval,
  )
where

import Control.Monad.Trans.State.Strict (State)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor (($>))
import Data.Tuple (swap)
import Trout.Bitboard (popCount)
import Trout.Game
  ( Game,
    allMoves,
    gamePieces,
    gameTurn,
    inCheck,
    makeMove,
    pieceBitboard,
    pieceTypeBitboard,
  )
import Trout.Game.Move (Move (..), nullMove)
import Trout.Piece (Color (..), Piece (..), PieceType (..))
import Trout.Search.Node (NodeResult (..), NodeType (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable (HashMapTT, TTEntry (..))
import Trout.Search.TranspositionTable qualified as TT
import Trout.Search.Worthiness (drawWorth, lossWorth)

-- TODO this kinda sucks
-- get rid of nullMove weirdness too
-- fails horribly when there are no moves
bestMove :: Int -> Game -> State HashMapTT (Int, Move)
bestMove 0 game =
  pure
    ( case gameTurn game of
        White -> eval game
        Black -> -eval game,
      head (allMoves game)
    )
bestMove depth game =
  first
    -- if player is black, flip because negamax is relative
    ( case gameTurn game of
        White -> id
        Black -> negate
    )
    <$> go (alpha, nullMove) (allMoves game)
  where
    alpha = minBound + 1 -- so negate works!!!!!!!
    beta = maxBound
    go best [] = pure best
    go best@(bestScore, _) (move : moves) = case makeMove game move of
      Nothing -> go best moves
      Just moveMade -> do
        otherScore <- searchNega (depth - 1) (-beta) (-max alpha bestScore) moveMade
        let nodeScore = -otherScore
        if nodeScore > bestScore
          then go (nodeScore, move) moves
          else go best moves

searchNega :: Int -> Int -> Int -> Game -> State HashMapTT Int
searchNega 0 !_ !_ !game = TT.insert game (TTEntry result nullMove 0) $> nodeResScore result
  where
    result = NodeResult (eval game) ExactNode
searchNega depth !alpha !beta !game = do
  let gameMoves = allMoves game
  maybeEntry <- TT.get game
  let scoredMoves = case maybeEntry of
        Nothing -> (0,) <$> gameMoves
        Just (TTEntry {entryMove}) ->
          -- make sure move has a chance of being in the movelist
          -- intended for speed, it's correct either way
          if entryMove /= nullMove
            then
              (1, entryMove)
                : ((0,) <$> filter (/= entryMove) gameMoves)
            else (0,) <$> gameMoves
  (bResult, bMove) <- go Nothing scoredMoves
  TT.insert game (TTEntry bResult bMove depth)
  pure (nodeResScore bResult)
  where
    go :: Maybe (Int, Move) -> [(Int, Move)] -> State HashMapTT (NodeResult, Move)
    -- no valid moves
    -- bestScore is nothing if all moves are illegal
    go Nothing []
      | inCheck (gameTurn game) (gamePieces game) = pure (NodeResult lossWorth ExactNode, nullMove)
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
eval game = pstEvalValue
  where
    (playerBB, oppBB) =
      ( ($ gamePieces game) . pieceBitboard . Piece White,
        ($ gamePieces game) . pieceBitboard . Piece Black
      )
        & case gameTurn game of
          White -> id
          Black -> swap
    -- calculate game phase
    -- pawns don't count, bishops and rooks count 1, rooks 2, queens 4
    -- taken from pesto/ethereal/fruit
    mgPhase =
      popCount (pieceTypeBitboard Knight (gamePieces game))
        + popCount (pieceTypeBitboard Bishop (gamePieces game))
        + 2 * popCount (pieceTypeBitboard Rook (gamePieces game))
        + 4 * popCount (pieceTypeBitboard Queen (gamePieces game))
    egPhase = 24 - mgPhase
    (aMask, nMask) = case gameTurn game of
      White -> (0, 56)
      Black -> (56, 0)
    pst bb p = pstEval bb p mgPhase egPhase
    pstEvalValue =
      pst (playerBB Pawn) Pawn aMask
        - pst (oppBB Pawn) Pawn nMask
        + pst (playerBB Knight) Knight aMask
        - pst (oppBB Knight) Knight nMask
        + pst (playerBB Bishop) Bishop aMask
        - pst (oppBB Bishop) Bishop nMask
        + pst (playerBB Rook) Rook aMask
        - pst (oppBB Rook) Rook nMask
        + pst (playerBB Queen) Queen aMask
        - pst (oppBB Queen) Queen nMask
        + pst (playerBB King) King aMask
        - pst (oppBB King) King nMask
