module Trout.Search
    ( SearchState (..)
    , bestMove
    , searchNega
    , eval
    ) where

import           Control.Monad.Trans.State.Strict
    ( State
    , evalState
    , get
    , modify
    )
import           Data.Bifunctor                   (first)
import           Data.Function                    (on)
import           Data.Hashable                    (hash)
import           Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict               as IM
import           Data.Maybe                       (mapMaybe)
import           Lens.Micro                       ((^.))
import           Trout.Bitboard                   (popCount)
import           Trout.Game
    ( Game (..)
    , Pieces (..)
    , allMoves
    , gamePlaying
    , gameTurn
    , gameWaiting
    , inCheck
    , makeMove
    )
import           Trout.Game.Move                  (Move (..), SpecialMove (..))
import           Trout.Piece                      (Color (..), Piece (..))
import           Trout.PieceSquareTables          (pstEvalBitboard)
import           Trout.Search.Worthiness
    ( bishopWorth
    , drawWorth
    , knightWorth
    , lossWorth
    , pawnWorth
    , queenWorth
    , rookWorth
    )

data TTEntry = TTEntry
    { entryEval  :: Int
    , entryDepth :: Int
    } deriving (Eq, Show)

data SearchState = SearchState
    { ssTranspositions :: IntMap TTEntry
    } deriving (Eq, Show)

-- simple best move finder
-- ok for now, gonna havve to replace to add statefulness between iterative deepening calls
bestMove :: Int -> Game -> (Int, Move)
bestMove 0 game =
    ( eval game
        * case game ^. gameTurn of
            White -> 1
            Black -> -1
    , head (allMoves game)
    )
bestMove depth game = first
    -- if player is black, flip because negamax is relative
    (* case game ^. gameTurn of
        White -> 1
        Black -> -1)
    (evalState
        (go (alpha, Move Pawn Normal 0 0) (allMoves game))
        (SearchState IM.empty))
  where
    alpha = minBound + 1 -- so negate works!!!!!!!
    beta = maxBound
    maxByPreferFst cmp a b = case a `cmp` b of
        LT -> b
        EQ -> a
        GT -> a
    go best [] = pure best
    go best (m : ms) = case makeMove game m of
        Just g  -> do
            score <- negate <$> searchNega (depth - 1) (-beta) (-fst best) g
            go (maxByPreferFst (compare `on` fst) best (score, m)) ms
        Nothing -> go best ms

searchNega :: Int -> Int -> Int -> Game -> State SearchState Int
searchNega depth alpha beta game
    | depth <= 0 = pure (eval game)
    | null moved = pure
        $ if inCheck game
        then lossWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = newAlpha alpha moved
  where
    newAlpha a [] = pure a
    newAlpha a (g : gs) = do
        let gHash = hash g
        maybeEntry <- IM.lookup gHash . ssTranspositions <$> get
        let ttScore = maybeEntry
                >>= \(TTEntry s d) ->
                    if d < depth
                    then Nothing
                    else pure s
        score <- case ttScore of
            Just score -> pure score
            Nothing -> do
                ret <- negate <$> searchNega (depth - 1) (-beta) (-a) g
                modify $ \(SearchState ntt) ->
                    SearchState (IM.insert gHash (TTEntry ret depth) ntt)
                pure ret
        if score >= beta
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
    (Pieces pp pn pb pr pq _) = game ^. gamePlaying
    (Pieces wp wn wb wr wq _) = game ^. gameWaiting
    pPawns = popCount pp
    wPawns = popCount wp
    pawnDiff = pPawns - wPawns
    pKnights = popCount pn
    wKnights = popCount wn
    knightDiff = pKnights - wKnights
    pBishops = popCount pb
    wBishops = popCount wb
    bishopDiff = pBishops - wBishops
    pRooks = popCount pr
    wRooks = popCount wr
    rookDiff = pRooks - wRooks
    pQueens = popCount pq
    wQueens = popCount wq
    queenDiff = pQueens - popCount wQueens
    pstEval = pstEvalBitboard
        $ pawnWorth * (pPawns + wPawns)
        + knightWorth * (pKnights + wKnights)
        + bishopWorth * (pBishops + wBishops)
        + rookWorth * (pRooks + wRooks)
        + queenWorth * (pQueens + wQueens)
    pstEvalValue = pstEval Pawn pp
        + pstEval Knight pn
        + pstEval Bishop pb
        + pstEval Rook pr
        + pstEval Queen pq
        - pstEval Pawn wp
        - pstEval Knight wn
        - pstEval Bishop wb
        - pstEval Rook wr
        - pstEval Queen wq
