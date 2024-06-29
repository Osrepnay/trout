module Trout.Search.Node (NodeType (..), NodeResult (..), ibv) where

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
