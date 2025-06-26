module Trout.Search.TranspositionTable
  ( TTEntry (..),
    TranspositionTable,
    STTranspositionTable,
    new,
    clear,
    lookup,
    insert,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Hashable (hash)
import Data.Vector (Vector)
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable qualified as MV
import Trout.Game (Board)
import Trout.Game.Move (Move)
import Trout.Search.Node (NodeResult (..))
import Prelude hiding (lookup)

-- correct move for depth only guaranteed on exact nodes
-- TODO consider moving entryMove into NodeResult
data TTEntry = TTEntry
  { entryNode :: !NodeResult,
    entryMove :: !Move,
    entryDepth :: !Int
  }
  deriving (Eq, Show)

type TranspositionTable = Vector (Maybe (Int, TTEntry))

type STTranspositionTable s = STVector s (Maybe (Int, TTEntry))

new :: Int -> ST s (STTranspositionTable s)
new n = MV.replicate n Nothing

clear :: STTranspositionTable s -> ST s ()
clear tt = MV.set tt Nothing

toKey :: Int -> Int -> Int
toKey unkeyed len = fromIntegral ((fromIntegral unkeyed :: Word) `rem` fromIntegral len)

basicInsert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
basicInsert board entry vec =
  MV.write
    vec
    (toKey (hash board) (MV.length vec))
    (Just (hash board, entry))

lookup :: Board -> STTranspositionTable s -> ST s (Maybe TTEntry)
lookup board vec =
  checkEntry
    <$> MV.read
      vec
      (toKey (hash board) (MV.length vec))
  where
    checkEntry Nothing = Nothing
    checkEntry (Just (cHash, cEntry))
      | cHash == hash board = Just cEntry
      | otherwise = Nothing

insert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
insert board entry vec = do
  existing <- lookup board vec
  case existing of
    Just oldEntry ->
      when (entryDepth oldEntry <= entryDepth entry) $
        basicInsert board entry vec
    Nothing -> basicInsert board entry vec
