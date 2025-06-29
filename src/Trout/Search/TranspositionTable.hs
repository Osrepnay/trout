module Trout.Search.TranspositionTable
  ( TTEntry (..),
    TranspositionTable,
    STTranspositionTable,
    new,
    clear,
    lookup,
    insert,
    sizeOfEntry,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bifunctor (first)
import Data.Hashable (hash)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (STVector)
import Data.Vector.Storable.Mutable qualified as MSV
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
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

instance Storable TTEntry where
  sizeOf :: TTEntry -> Int
  sizeOf _ = sizeOf (undefined :: NodeResult) + sizeOf (undefined :: Move) + sizeOf (undefined :: Int)
  alignment :: TTEntry -> Int
  alignment _ = sizeOf (undefined :: Int)
  peek :: Ptr TTEntry -> IO TTEntry
  peek ptr = do
    node <- peek (castPtr ptr)
    move <- peek (ptr `plusPtr` sizeOf node)
    depth <- peek (ptr `plusPtr` (sizeOf node + sizeOf move))
    pure (TTEntry node move depth)
  poke :: Ptr TTEntry -> TTEntry -> IO ()
  poke ptr (TTEntry node move depth) = do
    poke (castPtr ptr) node
    poke (ptr `plusPtr` sizeOf node) move
    poke (ptr `plusPtr` (sizeOf node + sizeOf move)) depth

instance Storable (Maybe (Int, TTEntry)) where
  sizeOf :: Maybe (Int, TTEntry) -> Int
  sizeOf _ = 2 * sizeOf (undefined :: Int) + sizeOf (undefined :: TTEntry)
  alignment :: Maybe (Int, TTEntry) -> Int
  alignment _ = sizeOf (undefined :: Int)
  peek :: Ptr (Maybe (Int, TTEntry)) -> IO (Maybe (Int, TTEntry))
  peek ptr = do
    let intPtr = castPtr ptr :: Ptr Int
    (maybeMarker :: Int) <- peek intPtr
    if maybeMarker == 0
      then pure Nothing
      else do
        trueHash <- peekElemOff intPtr 1
        entry <- peek (ptr `plusPtr` (2 * sizeOf (undefined :: Int)))
        pure (Just (trueHash, entry))
  poke :: Ptr (Maybe (Int, TTEntry)) -> Maybe (Int, TTEntry) -> IO ()
  poke ptr Nothing = poke (castPtr ptr) (0 :: Int)
  poke ptr (Just (trueHash, entry)) = do
    let intPtr = castPtr ptr :: Ptr Int
    poke intPtr 1
    pokeElemOff intPtr 1 trueHash
    pokeByteOff ptr (2 * sizeOf (undefined :: Int)) entry

sizeOfEntry :: Int
sizeOfEntry = sizeOf (undefined :: Maybe (Int, TTEntry))

type TranspositionTable = Vector (Maybe (Int, TTEntry))

type STTranspositionTable s = STVector s (Maybe (Int, TTEntry))

new :: Int -> ST s (STTranspositionTable s)
new n = MSV.replicate n Nothing

clear :: STTranspositionTable s -> ST s ()
clear tt = MSV.set tt Nothing

toKey :: Int -> Int -> Int
toKey unkeyed len = fromIntegral ((fromIntegral unkeyed :: Word) `rem` fromIntegral len)

basicInsert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
basicInsert board entry vec =
  MSV.write
    vec
    (toKey (hash board) (MSV.length vec))
    (Just (hash board, entry))

lookup :: Board -> STTranspositionTable s -> ST s (Maybe TTEntry)
lookup board vec =
  checkEntry
    <$> MSV.read
      vec
      (toKey (hash board) (MSV.length vec))
  where
    checkEntry Nothing = Nothing
    checkEntry (Just (cHash, cEntry))
      | cHash == hash board = Just cEntry
      | otherwise = Nothing

-- also returns if wrong hash
slotLookup :: Board -> STTranspositionTable s -> ST s (Maybe (Bool, TTEntry))
slotLookup board vec = fmap (first (== hash board)) <$> MSV.read vec (toKey (hash board) (MSV.length vec))

insert :: Board -> TTEntry -> STTranspositionTable s -> ST s ()
insert board entry vec = do
  existing <- slotLookup board vec
  case existing of
    Just (sameBoard, oldEntry) ->
      when (not sameBoard || sameBoard && entryDepth oldEntry <= entryDepth entry) $
        basicInsert board entry vec
    Nothing -> basicInsert board entry vec
