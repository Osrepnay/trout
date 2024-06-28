module Trout.Search.Node (NodeType (..), NodeResult (..)) where

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
