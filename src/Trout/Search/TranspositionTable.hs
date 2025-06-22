module Trout.Search.TranspositionTable (TTEntry (..), SizedHashMap (..), SizedHashMapTT, sizedHMEmpty, TTType (..)) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as S
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
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

class TTType m where
  tttypeInit :: Int -> m ()
  tttypeInsert :: Game -> TTEntry -> m ()
  tttypeLookup :: Game -> m (Maybe TTEntry)

-- if queue is nonempty, output side should
data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

pop :: Queue a -> (a, Queue a)
pop (Queue [] []) = error "empty queue"
pop (Queue ins (o : outs)) = (o, Queue ins outs)
pop (Queue ins []) = pop (Queue [] (reverse ins))

push :: a -> Queue a -> Queue a
push x (Queue ins outs) = Queue (x : ins) outs

data SizedHashMap k v = SizedHashMap
  { sizedHMQueue :: Queue k,
    sizedHMMap :: HashMap k v,
    sizedHMSize :: Int,
    sizedHMMaxSize :: Int
  }

type SizedHashMapTT = SizedHashMap Game TTEntry

sizedHMEmpty :: Int -> SizedHashMap k v
sizedHMEmpty = SizedHashMap emptyQueue HM.empty 0

sizedHMInsert :: (Hashable k) => k -> v -> SizedHashMap k v -> SizedHashMap k v
sizedHMInsert k v (SizedHashMap q m size maxSize)
  | size /= maxSize = SizedHashMap (push k q) (HM.insert k v m) (size + 1) maxSize
  | otherwise =
      let (toDrop, newQ) = pop q
       in SizedHashMap (push k newQ) (HM.insert k v (HM.delete toDrop m)) size maxSize

instance TTType (State SizedHashMapTT) where
  tttypeInit = S.put . sizedHMEmpty

  -- replacement strategy: depth-preferred, exact preferred, lazy otherwise and won't replace
  tttypeInsert game entry = S.modify $
    \m -> case HM.lookup game (sizedHMMap m) of
      Just oldEntry ->
        if entryDepth oldEntry < entryDepth entry
          || entryDepth oldEntry == entryDepth entry
            && nodeResType (entryNode entry) == ExactNode
            && nodeResType (entryNode oldEntry) /= ExactNode
          then sizedHMInsert game entry m
          else m
      Nothing -> sizedHMInsert game entry m
  tttypeLookup game = HM.lookup game . sizedHMMap <$> S.get
