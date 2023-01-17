module Trout.Search (SearchState (..), bestMove, searchMini, searchMaxi) where

import Control.Monad.Trans.State.Strict
import Data.Function
import Data.Foldable
import Data.Maybe
import Trout.Bitboard
import Trout.Game
import Trout.Game.Move
import Trout.PieceInfo
import Trout.Search.Worthiness

-- TODO transposition tables, pv, etc.
data SearchState = SearchState deriving (Eq, Show)

-- simple best move finder
-- ok for now, gonna havve to replace to add statefulness between iterative deepening calls
bestMove :: Color -> Int -> Game -> (Int, Move)
bestMove White depth game = go (beta, Move Pawn Normal 0 0) (allMoves game)
  where
    alpha = maxBound
    beta = minBound
    go best [] = best
    go best (m : ms) =
        case moved of
            Just g -> go (maximumBy (compare `on` fst) [best, (score g, m)]) ms
            Nothing -> go best ms
      where
        score g = evalState
            (searchMini (depth - 1) alpha (fst best) g)
            SearchState
        moved = makeMove game m
bestMove Black depth game = go (alpha, Move Pawn Normal 0 0) (allMoves game)
  where
    alpha = maxBound
    beta = minBound
    go best [] = best
    go best (m : ms) =
        case moved of
            Just g -> go (minimumBy (compare `on` fst) [best, (score g, m)]) ms
            Nothing -> go best ms
      where
        score g = evalState
            (searchMaxi (depth - 1) (fst best) beta g)
            SearchState
        moved = makeMove game m

searchMini :: Int -> Int -> Int -> Game -> State SearchState Int
searchMini depth alpha beta game
    | depth <= 0 = pure (eval game)
    | null moved = pure
        $ if inCheck game
        then whiteWonWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = newAlpha alpha moved
  where
    newAlpha a [] = pure a
    newAlpha a (g : gs) = do
        score <- searchMaxi (depth - 1) a beta g
        if score <= beta
        then pure beta
        else newAlpha (min a score) gs
    moved = mapMaybe (makeMove game) (allMoves game)

searchMaxi :: Int -> Int -> Int -> Game -> State SearchState Int
searchMaxi depth alpha beta game
    | depth <= 0 = pure (eval game)
    | null moved = pure
        $ if inCheck game
        then blackWonWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = newBeta beta moved
  where
    newBeta b [] = pure b
    newBeta b (g : gs) = do
        score <- searchMini (depth - 1) alpha b g
        if score >= alpha
        then pure alpha
        else newBeta (max b score) gs
    moved = mapMaybe (makeMove game) (allMoves game)

eval :: Game -> Int
eval game = pawnWorth * pawnDiff
    + knightWorth * knightDiff
    + bishopWorth * bishopDiff
    + rookWorth * rookDiff
    + queenWorth * queenDiff
  where
    whitePieces = sidePieces (gameWhite game)
    blackPieces = sidePieces (gameBlack game)
    pawnDiff = popCount (pawns whitePieces) - popCount (pawns blackPieces)
    knightDiff = popCount (knights whitePieces) - popCount (knights blackPieces)
    bishopDiff = popCount (bishops whitePieces) - popCount (bishops blackPieces)
    rookDiff = popCount (rooks whitePieces) - popCount (rooks blackPieces)
    queenDiff = popCount (queens whitePieces) - popCount (queens blackPieces)
