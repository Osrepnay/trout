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
import           Data.Maybe                       (mapMaybe)
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
import           Trout.Game.Move                  (Move (..), nullMove)
import           Trout.Piece                      (Color (..))
import           Trout.Search.PieceSquareTables
    ( bishopEPST
    , bishopMPST
    , kingEPST
    , kingMPST
    , knightEPST
    , knightMPST
    , pawnEPST
    , pawnMPST
    , pstEval
    , queenEPST
    , queenMPST
    , rookEPST
    , rookMPST
    )
import           Trout.Search.Worthiness          (drawWorth, lossWorth)

data TTEntry = TTEntry
    { entryHash  :: Int -- real hash, not moduloed
    , entryEval  :: Int
    , entryDepth :: Int
    , entryMove  :: Move
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
    <$> go (alpha, nullMove) (allMoves (game ^. hgGame))
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
        (SearchState table) <- get
        -- TODO abstract this
        let gHash = hash game
        let gIdx = hashToIdx gHash (MV.length table)
        maybeEntry <- liftIO (MV.read table gIdx)
        let ttMove = maybeEntry
                >>= \(TTEntry h _ _ m) ->
                    if h == gHash
                    then pure m
                    else Nothing
        -- if there is a tt move, make it first
        let movedWithTT = case ttMove of
                Just m  -> case makeMove game m of
                    Just g  -> (m, g) : filter ((/= m) . fst) moved
                    Nothing -> moved
                Nothing -> moved
        (nAlpha, nMove) <- newAlpha (alpha, nullMove) movedWithTT
        _ <- liftIO
            $ MV.write table gIdx
            $ Just (TTEntry gHash nAlpha depth nMove)
        pure nAlpha
  where
    -- main search body
    newAlpha best [] = pure best
    newAlpha (a, bm) ((m, g) : mgs) = do
        -- check transpositions
        table <- ssTranspositions <$> get
        let gHash = hash g
        let gIdx = hashToIdx gHash (MV.length table)
        maybeEntry <- liftIO (MV.read table gIdx)
        let ttScore = maybeEntry
                >>= \(TTEntry h s d _) ->
                    if d >= depth && h == gHash
                    then pure s
                    else Nothing
        score <- case ttScore of
            -- skip search if entry exists
            Just score -> pure score
            Nothing    -> negate <$> searchNega (depth - 1) (-beta) (-a) g
        if score >= beta
        then pure (beta, m) -- fail high
        else newAlpha (if a < score then (score, m) else (a, bm)) mgs
    -- games from all possible moves from current position w/ their respective
    -- moves
    moved = mapMaybe
        (\m -> (m, ) <$> makeMove game m)
        (allMoves (game ^. hgGame))

eval :: Game -> Int
eval game = pstEvalValue
  where
    -- piece counting
    -- TODO merge with psqtables
    (Pieces pp pn pb pr pq pk) = game ^. gamePlaying
    (Pieces wp wn wb wr wq wk) = game ^. gameWaiting
    pKnights = popCount pn
    wKnights = popCount wn
    pBishops = popCount pb
    wBishops = popCount wb
    pRooks = popCount pr
    wRooks = popCount wr
    pQueens = popCount pq
    wQueens = popCount wq

    -- psqtables part
    phase = pKnights + wKnights + pBishops + wBishops
        + 2 * (pRooks + wRooks)
        + 4 * (pQueens + wQueens)
    -- blend is 0-1; 0=all endgame, 1=all middlegame
    -- 24 is starting position phase
    blend = fromIntegral phase / 24

    color = game ^. gameTurn
    pstEvalValue = pstEval pp pawnMPST pawnEPST blend color
        - pstEval wp pawnMPST pawnEPST blend color
        + pstEval pn knightMPST knightEPST blend color
        - pstEval wn knightMPST knightEPST blend color
        + pstEval pb bishopMPST bishopEPST blend color
        - pstEval wb bishopMPST bishopEPST blend color
        + pstEval pr rookMPST rookEPST blend color
        - pstEval wr rookMPST rookEPST blend color
        + pstEval pq queenMPST queenEPST blend color
        - pstEval wq queenMPST queenEPST blend color
        + pstEval pk kingMPST kingEPST blend color
        - pstEval wk kingMPST kingEPST blend color
