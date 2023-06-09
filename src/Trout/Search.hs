module Trout.Search
    ( SearchState (..)
    , bestMove
    , searchNega
    , eval
    ) where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT (..), get)
import           Data.Bifunctor                   (first)
import           Data.Function                    (on)
import           Data.Hashable                    (hash)
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Vector.Mutable              (IOVector)
import qualified Data.Vector.Mutable              as MV
import           Lens.Micro                       ((^.))
import           Trout.Bitboard                   (popCount)
import           Trout.Game
    ( Game (..)
    , HGame
    , Pieces (..)
    , allMoves
    , gamePlaying
    , gameTurn
    , gameWaiting
    , hgGame
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
    { entryHash  :: Int -- real hash, not moduloed
    , entryEval  :: Int
    , entryDepth :: Int
    } deriving (Eq, Show)

data SearchState = SearchState
    { ssTranspositions :: IOVector (Maybe TTEntry)
    }

hashToIdx :: Int -> Int -> Int
hashToIdx h tableLen = fromIntegral
    $ (fromIntegral h :: Word)
    `rem` (fromIntegral tableLen :: Word)

-- simple best move finder
-- ReaderT works because of IO, for now
bestMove :: Int -> HGame -> StateT SearchState IO (Int, Move)
bestMove 0 game = pure
    ( eval (game ^. hgGame)
        * case game ^. hgGame . gameTurn of
            White -> 1
            Black -> -1
    , head (allMoves (game ^. hgGame))
    )
bestMove depth game = first
    -- if player is black, flip because negamax is relative
    (* case game ^. hgGame . gameTurn of
        White -> 1
        Black -> -1)
    <$> go (alpha, Move Pawn Normal 0 0) (allMoves (game ^. hgGame))
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

searchNega :: Int -> Int -> Int -> HGame -> StateT SearchState IO Int
searchNega depth alpha beta game
    | depth <= 0 = pure (eval (game ^. hgGame))
    | null moved = pure
        $ if inCheck (game ^. hgGame)
        then lossWorth -- checkmate
        else drawWorth -- stalemate
    | otherwise = do
        state <- get
        sorted <- liftIO (ioSortBy (gameCmp state) moved)
        newAlpha alpha sorted
  where
    newAlpha a [] = pure a
    newAlpha a (g : gs) = do
        -- check transpositions
        table <- ssTranspositions <$> get
        let gHash = hash g
        let gIdx = hashToIdx gHash (MV.length table)
        maybeEntry <- liftIO (MV.read table gIdx)
        let ttScore = maybeEntry
                >>= \(TTEntry h s d) ->
                    if d >= depth && h == gHash
                    then pure s
                    else Nothing
        score <- case ttScore of
            -- return already if entry exists
            Just score -> pure score
            Nothing -> do
                ret <- negate <$> searchNega (depth - 1) (-beta) (-a) g
                _ <- liftIO
                    $ MV.write table gIdx (Just (TTEntry gHash ret depth))
                pure ret
        if score >= beta
        then pure beta
        else newAlpha (max a score) gs
    -- games from all possible moves from current position
    moved = mapMaybe (makeMove game) (allMoves (game ^. hgGame))

-- iolic insertion sort
ioSortBy :: (a -> a -> IO Ordering) -> [a] -> IO [a]
ioSortBy cmp = go []
  where
    go acc (x : xs) = insert x acc >>= \nacc -> go nacc xs
    go acc []       = pure acc
    insert a (b : bs) = cmp a b
        >>= \c -> case c of
            LT -> pure (a : b : bs)
            EQ -> pure (a : b : bs)
            GT -> (b :) <$> insert a bs
    insert a [] = pure [a]

-- currently only checks transposition table
-- supposed to be "movecmp" but transposition table doesn't store previous move
gameCmp :: SearchState -> HGame -> HGame -> IO Ordering
gameCmp (SearchState table) hgameA hgameB = do
    let aHash = hash hgameA
    let aIdx = hashToIdx aHash (MV.length table)
    maybeAEntry <- liftIO (MV.read table aIdx)
    let checkedAEntry = maybeAEntry
            >>= \(TTEntry h s _) ->
                if h == aHash
                then Just s
                else Nothing
    let bHash = hash hgameB
    let bIdx = hashToIdx bHash (MV.length table)
    maybeBEntry <- liftIO (MV.read table bIdx)
    let checkedBEntry = maybeBEntry
            >>= \(TTEntry h s _) ->
                if h == bHash
                then Just s
                else Nothing
    -- assume 0 score if no entry
    let aEntry = fromMaybe 0 checkedAEntry
    let bEntry = fromMaybe 0 checkedBEntry
    -- intentionally flipped to get greatest -> least ordering
    pure (compare bEntry aEntry)

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
