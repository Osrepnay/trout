module Trout.Search.TranspositionTable (TTEntry, HashMapTT, new, clear, insert, get) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as S
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Trout.Game (Game)
import Trout.Search.Node (NodeResult)

data TTEntry = TTEntry
  { entryNode :: !NodeResult,
    entryDepth :: !Int
  }
  deriving (Eq, Show)

-- TODO limit size

type HashMapTT = HashMap Game TTEntry

new :: HashMapTT
new = M.empty

clear :: State HashMapTT ()
clear = S.put new

insert :: Game -> TTEntry -> State HashMapTT ()
insert game entry = S.modify (M.insert game entry)

get :: Game -> State HashMapTT (Maybe TTEntry)
get game = M.lookup game <$> S.get
