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

maxChain :: Int
maxChain = 1

type TranspositionTable = Vector [(Int, TTEntry)]

type STTranspositionTable s = STVector s [(Int, TTEntry)]

new :: Int -> ST s (STTranspositionTable s)
new n = MV.replicate n []

clear :: STTranspositionTable s -> ST s ()
clear tt = MV.set tt []

toKey :: Int -> Int -> Int
toKey unkeyed len = fromIntegral ((fromIntegral unkeyed :: Word) `rem` fromIntegral len)

basicInsert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
basicInsert board entry vec =
  MV.modify
    vec
    ( \chain ->
        let filteredChain = filter ((/= hash board) . fst) chain
         in (hash board, entry)
              : if length filteredChain < maxChain
                then filteredChain
                else init filteredChain
    )
    (toKey (hash board) (MV.length vec))

lookup :: Board -> STTranspositionTable s -> ST s (Maybe TTEntry)
lookup board vec =
  findRealEntry
    <$> MV.read
      vec
      (toKey (hash board) (MV.length vec))
  where
    findRealEntry [] = Nothing
    findRealEntry ((cHash, cEntry) : chain)
      | cHash == hash board = Just cEntry
      | otherwise = findRealEntry chain

insert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
insert board entry vec = do
  existing <- lookup board vec
  case existing of
    Just oldEntry ->
      when (entryDepth oldEntry <= entryDepth entry) $
        basicInsert board entry vec
    Nothing -> basicInsert board entry vec
