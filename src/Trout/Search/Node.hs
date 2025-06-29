{-# LANGUAGE MagicHash #-}

module Trout.Search.Node (NodeType (..), NodeResult (..)) where

import Foreign (Ptr, Storable (..), castPtr)
import GHC.Base (Int (I#), dataToTag#, tagToEnum#)
import Data.Bits ((.&.), complement)

data NodeType
  = AllNode -- failed low, move is "too bad"
  | ExactNode -- not fail high or fail low
  | CutNode -- failed high/beta cutoff, move is "too good"
  deriving (Eq, Show)

instance Enum NodeType where
  toEnum (I# i) = tagToEnum# i
  fromEnum x = I# (dataToTag# x)

data NodeResult = NodeResult
  { nodeResScore :: !Int,
    nodeResType :: !NodeType
  }
  deriving (Eq, Show)

-- integrated bounds and values
toIbv :: NodeResult -> Int
toIbv (NodeResult score nodeType) =
  4 * score + fromEnum nodeType - 1

fromIbv :: Int -> NodeResult
fromIbv ibv = NodeResult rawScore (toEnum (diff + 1))
  where
    rawScore = ((ibv + 1) .&. complement 3) `quot` 4
    diff = ibv - rawScore

instance Storable NodeResult where
  sizeOf :: NodeResult -> Int
  sizeOf _ = sizeOf (undefined :: Int)
  alignment :: NodeResult -> Int
  alignment _ = alignment (undefined :: Int)
  peek :: Ptr NodeResult -> IO NodeResult
  peek ptr = do
    score <- peek (castPtr ptr)
    pure $ fromIbv score
  poke :: Ptr NodeResult -> NodeResult -> IO ()
  poke ptr res = do
    poke (castPtr ptr) (toIbv res)
