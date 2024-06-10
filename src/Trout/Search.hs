module Trout.Search
  ( SearchState (..),
    bestMove,
    searchNega,
    eval,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), get)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.))
import Trout.Bitboard (popCount)
import Trout.Game
  ( HGame,
    HasGame (..),
    active,
    allMoves,
    bishops,
    gamePieces,
    gameTurn,
    hgGame,
    inCheck,
    inactive,
    kings,
    knights,
    makeMove,
    pawns,
    queens,
    rooks,
  )
import Trout.Game.Move (Move (..), nullMove)
import Trout.Piece (Color (..), Piece (..))
import Trout.Search.PieceSquareTables (pstEval)
import Trout.Search.TranspositionTable
  ( NodeType (..),
    TTEntry (..),
    TranspositionTable,
    insertTT,
    readTT,
  )
import Trout.Search.Worthiness (drawWorth, lossWorth)

data SearchState = SearchState
  { ssTranspositions :: TranspositionTable
  }

-- simple best move finder
-- ReaderT works because of IO, for now
bestMove :: Int -> HGame -> StateT SearchState IO (Int, Move)
bestMove 0 game =
  pure
    ( eval game
        * case game ^. hgGame . gameTurn of
          White -> 1
          Black -> -1,
      head (allMoves game)
    )
bestMove depth game =
  first
    -- if player is black, flip because negamax is relative
    ( *
        case game ^. hgGame . gameTurn of
          White -> 1
          Black -> -1
    )
    <$> go (alpha, nullMove) (allMoves game)
  where
    alpha = minBound + 1 -- so negate works!!!!!!!
    beta = maxBound
    maxByPreferFst cmp a b = case a `cmp` b of
      LT -> b
      EQ -> a
      GT -> a
    go best [] = pure best
    go best (m : ms) = case makeMove game m of
      Just g -> do
        score <- negate <$> searchNega (depth - 1) (-beta) (-fst best) g
        go (maxByPreferFst (compare `on` fst) best (score, m)) ms
      Nothing -> go best ms

searchNega :: Int -> Int -> Int -> HGame -> StateT SearchState IO Int
searchNega depth alpha beta game
  | depth <= 0 = pure (eval game)
  | null moved =
      pure $
        if inCheck (game ^. hgGame)
          then lossWorth -- checkmate
          else drawWorth -- stalemate
  | otherwise = do
      (SearchState table) <- get
      ttMove <- liftIO (fmap entryMove <$> readTT game table)
      -- if there is a tt move, make it first
      let movedWithTT = case ttMove of
            Just m -> case makeMove game m of
              Just g -> (m, g) : filter ((/= m) . fst) moved
              Nothing -> moved
            Nothing -> moved
      (nAlpha, nMove) <- newAlpha (alpha, nullMove) movedWithTT
      let nodeType
            | nAlpha == beta = CutNode -- failed high
            | nAlpha == alpha = AllNode -- failed low
            | otherwise = ExactNode
      _ <-
        liftIO $
          insertTT game (TTEntry nAlpha nodeType depth nMove) table
      pure nAlpha
  where
    -- main search body
    newAlpha best [] = pure best
    newAlpha (a, bm) ((m, g) : mgs) = do
      -- check transpositions
      table <- ssTranspositions <$> get
      maybeEntry <- liftIO (readTT game table)
      let ttScore =
            maybeEntry
              >>= \(TTEntry s t d _) ->
                if d >= depth
                  -- failed low, i.e. we can't trust `s`
                  -- because the "real" eval could be much lower
                  -- so only accept if `s` is already lower than bound
                  && (t /= AllNode || s <= a)
                  -- failed high, "real" eval could be much higher
                  && (t /= CutNode || s >= beta)
                  then pure s
                  else Nothing
      score <- case ttScore of
        -- skip search if entry exists
        Just score -> pure score
        Nothing -> negate <$> searchNega (depth - 1) (-beta) (-a) g
      if score >= beta
        then pure (beta, m) -- fail high
        else newAlpha (if a < score then (score, m) else (a, bm)) mgs
    -- games from all possible moves from current position w/ their respective
    -- moves
    moved =
      mapMaybe
        (\m -> (m,) <$> makeMove game m)
        (allMoves game)

eval :: (HasGame a) => a -> Int
eval game = pstEvalValue
  where
    pieces = game ^. gamePieces
    actives = pieces ^. active
    inactives = pieces ^. inactive

    -- calculate game phase
    -- pawns don't count, bishops and rooks count 1, rooks 2, queens 4
    -- taken from pesto/ethereal/fruit
    mgPhase =
      popCount (pieces ^. knights)
        + popCount (pieces ^. bishops)
        + 2 * popCount (pieces ^. rooks)
        + 4 * popCount (pieces ^. queens)
    egPhase = 24 - mgPhase
    (aMask, nMask) = case game ^. gameTurn of
      White -> (0, 56)
      Black -> (56, 0)
    pst bb p = pstEval bb p mgPhase egPhase
    pstEvalValue =
      pst (actives ^. pawns) Pawn aMask
        - pst (inactives ^. pawns) Pawn nMask
        + pst (actives ^. knights) Knight aMask
        - pst (inactives ^. knights) Knight nMask
        + pst (actives ^. bishops) Bishop aMask
        - pst (inactives ^. bishops) Bishop nMask
        + pst (actives ^. rooks) Rook aMask
        - pst (inactives ^. rooks) Rook nMask
        + pst (actives ^. queens) Queen aMask
        - pst (inactives ^. queens) Queen nMask
        + pst (actives ^. kings) King aMask
        - pst (inactives ^. kings) King nMask
