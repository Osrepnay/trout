module Trout.Search.TranspositionTable
  ( TTEntry (..),
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
import Data.Bits ((.&.))
import Data.Hashable (hash)
import Data.Int (Int16)
import Data.Vector.Storable.Mutable (STVector)
import Data.Vector.Storable.Mutable qualified as MSV
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Trout.Game.Board (Board)
import Trout.Game.Move (Move (NullMove))
import Trout.Search.Node (NodeResult (NodeResult), NodeType (..))
import Prelude hiding (lookup)

-- correct move for depth only guaranteed on exact nodes
-- TODO consider moving entryMove into NodeResult
data TTEntry = TTEntry
  { entryScore :: !NodeResult,
    entryMove :: !Move,
    entryHalfmove :: !Int16,
    entryDepth :: !Int16
  }
  deriving (Eq, Show)

instance Storable TTEntry where
  sizeOf :: TTEntry -> Int
  sizeOf _ =
    sizeOf (undefined :: NodeResult)
      + sizeOf (undefined :: Move)
      + padding
      + sizeOf (undefined :: Int16)
      + sizeOf (undefined :: Int16)
    where
      padding = 2 - sizeOf (undefined :: Move) .&. 1
  alignment :: TTEntry -> Int
  alignment _ =
    max
      (alignment (undefined :: NodeResult))
      (max (alignment (undefined :: Move)) (alignment (undefined :: Int16)))
  peek :: Ptr TTEntry -> IO TTEntry
  peek ptr = do
    nodeResult <- peek (castPtr ptr)
    let nodedPtr = ptr `plusPtr` sizeOf nodeResult
    move <- peek nodedPtr
    let paddedPtr = nodedPtr `plusPtr` (sizeOf move + 2 - sizeOf move .&. 1)
    halfmove <- peekElemOff paddedPtr 0
    depth <- peekElemOff paddedPtr 1
    pure (TTEntry nodeResult move halfmove depth)
  poke :: Ptr TTEntry -> TTEntry -> IO ()
  poke ptr (TTEntry nodeResult move halfmove depth) = do
    poke (castPtr ptr) nodeResult
    let nodedPtr = ptr `plusPtr` sizeOf nodeResult
    poke nodedPtr move
    let paddedPtr = nodedPtr `plusPtr` (sizeOf move + 2 - sizeOf move .&. 1)
    pokeElemOff paddedPtr 0 halfmove
    pokeElemOff paddedPtr 1 depth

instance Storable (Maybe (Int, TTEntry)) where
  sizeOf :: Maybe (Int, TTEntry) -> Int
  sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: TTEntry)
  alignment :: Maybe (Int, TTEntry) -> Int
  alignment _ = max (alignment (undefined :: Int)) (alignment (undefined :: TTEntry))
  peek :: Ptr (Maybe (Int, TTEntry)) -> IO (Maybe (Int, TTEntry))
  peek ptr = do
    let intPtr = castPtr ptr :: Ptr Int
    trueHash <- peekElemOff intPtr 0
    entry <- peek (ptr `plusPtr` sizeOf (undefined :: Int))
    if trueHash == 0 && entry == TTEntry (NodeResult 0 ExactNode) NullMove 0 0
      then pure Nothing
      else pure (Just (trueHash, entry))
  poke :: Ptr (Maybe (Int, TTEntry)) -> Maybe (Int, TTEntry) -> IO ()
  poke ptr Nothing = do
    poke (castPtr ptr) (0 :: Int)
    pokeByteOff ptr (sizeOf (undefined :: Int)) (TTEntry (NodeResult 0 ExactNode) NullMove 0 0)
  poke ptr (Just (trueHash, entry)) = do
    poke (castPtr ptr) trueHash
    pokeByteOff ptr (sizeOf (trueHash :: Int)) entry

sizeOfEntry :: Int
sizeOfEntry = sizeOf (undefined :: Maybe (Int, TTEntry))

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
      when
        ( not sameBoard
            && ( entryDepth entry >= entryDepth oldEntry
                   || (entryHalfmove oldEntry + entryDepth oldEntry + 3 < entryHalfmove entry + entryDepth entry)
               )
            || sameBoard && entryDepth oldEntry <= entryDepth entry
        )
        $ basicInsert board entry vec
    Nothing -> basicInsert board entry vec
