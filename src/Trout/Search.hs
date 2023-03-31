module Trout.Search (SearchState (..), bestMove, eval, searchMini, searchMaxi) where

import Control.Monad.Trans.State.Strict (State, evalState)
import Data.Function                    (on)
import Data.Maybe                       (mapMaybe)
import Lens.Micro                       ((^.))
import Trout.Bitboard                   (popCount)
import Trout.Game
    ( Game (..)
    , allMoves
    , bishops
    , gamePieces
    , inCheck
    , knights
    , makeMove
    , pawns
    , queens
    , rooks
    , sideBlack
    , sideWhite
    )
import Trout.Game.Move                  (Move (..), SpecialMove (..))
import Trout.Piece                      (Color (..), Piece (..))
import Trout.PieceSquareTables          (pstEvalBitboard)
import Trout.Search.Worthiness
    ( bishopWorth
    , blackWonWorth
    , drawWorth
    , knightWorth
    , pawnWorth
    , queenWorth
    , rookWorth
    , whiteWonWorth
    )

-- TODO transposition tables, pv, etc.
data SearchState = SearchState deriving (Eq, Show)

-- simple best move finder
-- ok for now, gonna havve to replace to add statefulness between iterative deepening calls
bestMove :: Int -> Game -> (Int, Move)
bestMove 0 game = (eval game, head (allMoves game))
bestMove depth game@(Game _ _ _ _ White) =
    go (alpha, Move Pawn Normal 0 0) (allMoves game)
  where
    alpha = minBound
    beta = maxBound
    maxByPreferFst cmp a b = case a `cmp` b of
        LT -> b
        EQ -> a
        GT -> a
    go best [] = best
    go best (m : ms) = case makeMove game m of
        Just g  -> go (maxByPreferFst (compare `on` fst) best (score g, m)) ms
        Nothing -> go best ms
      where
        score g = evalState
            (searchMini (depth - 1) (fst best) beta g)
            SearchState
bestMove depth game@(Game _ _ _ _ Black) =
    go (beta, Move Pawn Normal 0 0) (allMoves game)
  where
    alpha = minBound
    beta = maxBound
    minByPreferFst cmp a b = case a `cmp` b of
        LT -> a
        EQ -> a
        GT -> b
    go best [] = best
    go best (m : ms) = case makeMove game m of
        Just g  -> go (minByPreferFst (compare `on` fst) best (score g, m)) ms
        Nothing -> go best ms
      where
        score g = evalState
            (searchMaxi (depth - 1) alpha (fst best) g)
            SearchState

searchMini :: Int -> Int -> Int -> Game -> State SearchState Int
searchMini depth alpha beta game
    | depth <= 0 = pure (eval game)
    | null moved = pure
        $ if inCheck game
        then whiteWonWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = newBeta beta moved
  where
    newBeta b [] = pure b
    newBeta b (g : gs) = do
        score <- searchMaxi (depth - 1) alpha b g
        if score < alpha
        then pure alpha
        else newBeta (min b score) gs
    moved = mapMaybe (makeMove game) (allMoves game)

searchMaxi :: Int -> Int -> Int -> Game -> State SearchState Int
searchMaxi depth alpha beta game
    | depth <= 0 = pure (eval game)
    | null moved = pure
        $ if inCheck game
        then blackWonWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = newAlpha alpha moved
  where
    newAlpha a [] = pure a
    newAlpha a (g : gs) = do
        score <- searchMini (depth - 1) a beta g
        if score > beta
        then pure beta
        else newAlpha (max a score) gs
    moved = mapMaybe (makeMove game) (allMoves game)

eval :: Game -> Int
eval game = pawnWorth * pawnDiff
    + knightWorth * knightDiff
    + bishopWorth * bishopDiff
    + rookWorth * rookDiff
    + queenWorth * queenDiff
    + pstEvalValue
  where
    whitePieces = game ^. gamePieces . sideWhite
    blackPieces = game ^. gamePieces . sideBlack
    whitePawns = popCount (whitePieces ^. pawns)
    blackPawns = popCount (blackPieces ^. pawns)
    pawnDiff = whitePawns - blackPawns
    whiteKnights = popCount (whitePieces ^. knights)
    blackKnights = popCount (blackPieces ^. knights)
    knightDiff = whiteKnights - blackKnights
    whiteBishops = popCount (whitePieces ^. bishops)
    blackBishops = popCount (blackPieces ^. bishops)
    bishopDiff = whiteBishops - blackBishops
    whiteRooks = popCount (whitePieces ^. rooks)
    blackRooks = popCount (blackPieces ^. rooks)
    rookDiff = whiteRooks - blackRooks
    whiteQueens = popCount (whitePieces ^. queens)
    blackQueens = popCount (blackPieces ^. queens)
    queenDiff = whiteQueens - popCount blackQueens
    pstEval = pstEvalBitboard
        $ pawnWorth * (whitePawns + blackPawns)
        + knightWorth * (whiteKnights + blackKnights)
        + bishopWorth * (whiteBishops + blackBishops)
        + rookWorth * (whiteRooks + blackRooks)
        + queenWorth * (whiteQueens + blackKnights)
    pstEvalValue = pstEval Pawn (whitePieces ^. pawns)
        + pstEval Knight (whitePieces ^. knights)
        + pstEval Bishop (whitePieces ^. bishops)
        + pstEval Rook (whitePieces ^. rooks)
        + pstEval Queen (whitePieces ^. queens)
        - pstEval Pawn (blackPieces ^. pawns)
        - pstEval Knight (blackPieces ^. knights)
        - pstEval Bishop (blackPieces ^. bishops)
        - pstEval Rook (blackPieces ^. rooks)
        - pstEval Queen (blackPieces ^. queens)
