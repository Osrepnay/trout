module Trout.Search.Node (NodeType (..), NodeResult (..), ibv) where

import Foreign (Ptr, Storable (..), castPtr, plusPtr)

data NodeType
  = ExactNode -- not fail high or fail low
  | CutNode -- failed high/beta cutoff, move is "too good"
  | AllNode -- failed low, move is "too bad"
  deriving (Eq, Show)

data NodeResult = NodeResult
  { nodeResScore :: !Int,
    nodeResType :: !NodeType
  }
  deriving (Eq, Show)

-- integrated bounds and values
ibv :: NodeResult -> Int
ibv (NodeResult score nodeType) =
  4 * score + case nodeType of
    ExactNode -> 0
    CutNode -> 1
    AllNode -> -1

-- TODO ibv?
instance Storable NodeResult where
  sizeOf :: NodeResult -> Int
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment :: NodeResult -> Int
  alignment _ = alignment (undefined :: Int)
  peek :: Ptr NodeResult -> IO NodeResult
  peek ptr = do
    score <- peek (castPtr ptr)
    nodeTypeInt <- peek ((ptr `plusPtr` sizeOf score) :: Ptr Int)
    let nodeType = case nodeTypeInt of
          0 -> ExactNode
          1 -> CutNode
          2 -> AllNode
          _ -> error "unknown node"
    pure $ NodeResult score nodeType
  poke :: Ptr NodeResult -> NodeResult -> IO ()
  poke ptr (NodeResult score nodeType) = do
    poke (castPtr ptr) score
    poke
      (ptr `plusPtr` sizeOf score)
      ( case nodeType of
          ExactNode -> 0 :: Int
          CutNode -> 1
          AllNode -> 2
      )
