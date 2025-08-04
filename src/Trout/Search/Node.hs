module Trout.Search.Node (NodeType (..), NodeResult (..), matchesBounds) where

import Data.Bits (complement, (.&.))
import Foreign (Ptr, Storable (..), castPtr)

data NodeType
  = AllNode -- failed low, move is "too bad"
  | ExactNode -- not fail high or fail low
  | CutNode -- failed high/beta cutoff, move is "too good"
  deriving (Enum, Eq, Show)

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
fromIbv ibv = NodeResult (rounded `quot` 4) (toEnum (diff + 1))
  where
    rounded = (ibv + 1) .&. complement 3
    diff = ibv - rounded

matchesBounds :: Int -> Int -> NodeResult -> Bool
matchesBounds alpha beta (NodeResult s t) =
  t == ExactNode
    || t == AllNode && s <= alpha
    || t == CutNode && s >= beta

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
