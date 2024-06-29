module Trout.Search.TranspositionTable (TTEntry (..), HashMapTT, new, clear, insert, get) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as S
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Trout.Game (Game)
import Trout.Game.Move (Move)
import Trout.Search.Node (NodeResult (..), NodeType (ExactNode))

-- correct move for depth only guaranteed on exact nodes
-- TODO consider moving entryMove into NodeResult
data TTEntry = TTEntry
  { entryNode :: !NodeResult,
    entryMove :: !Move,
    entryDepth :: !Int
  }
  deriving (Eq, Show)

-- TODO limit size

type HashMapTT = HashMap Game TTEntry

new :: HashMapTT
new = M.empty

clear :: State HashMapTT ()
clear = S.put new

-- replacement strategy: depth-preferred, exact preferred, lazy otherwise and won't replace
insert :: Game -> TTEntry -> State HashMapTT ()
insert game entry = S.modify $
  \m -> case M.lookup game m of
    Just oldEntry ->
      if entryDepth oldEntry < entryDepth entry
        || entryDepth oldEntry == entryDepth entry
          && nodeResType (entryNode entry) == ExactNode
          && nodeResType (entryNode oldEntry) /= ExactNode
        then M.insert game entry m
        else m
    Nothing -> M.insert game entry m

get :: Game -> State HashMapTT (Maybe TTEntry)
get game = M.lookup game <$> S.get
