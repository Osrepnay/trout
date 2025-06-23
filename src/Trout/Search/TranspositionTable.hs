module Trout.Search.TranspositionTable (TTEntry (..), SizedHashMap (..), SizedHashMapTT, sizedHMEmpty, TTType (..)) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as S
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable, hash)
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

class TTType m where
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

data SizedHashMap v = SizedHashMap
  { sizedHMQueue :: Queue Int,
    sizedHMMap :: HashMap Int v,
    sizedHMSize :: Int,
    sizedHMMaxSize :: Int
  }

type SizedHashMapTT = SizedHashMap TTEntry

sizedHMEmpty :: Int -> SizedHashMap v
sizedHMEmpty = SizedHashMap emptyQueue HM.empty 0

sizedHMInsert :: (Hashable k) => k -> v -> SizedHashMap v -> SizedHashMap v
sizedHMInsert k v (SizedHashMap q m size maxSize)
  | size /= maxSize = SizedHashMap (push kHash q) (HM.insert kHash v m) (size + 1) maxSize
  | otherwise =
      let (toDrop, newQ) = pop q
       in SizedHashMap (push kHash newQ) (HM.insert kHash v (HM.delete (hash toDrop) m)) size maxSize
  where
    kHash = hash k

instance TTType (State SizedHashMapTT) where
  -- replacement strategy: depth-preferred, exact preferred, lazy otherwise and won't replace
  tttypeInsert game entry = S.modify $
    \m -> case HM.lookup (hash game) (sizedHMMap m) of
      Just oldEntry ->
        if entryDepth oldEntry < entryDepth entry
          || entryDepth oldEntry == entryDepth entry
            && nodeResType (entryNode entry) == ExactNode
            && nodeResType (entryNode oldEntry) /= ExactNode
          then sizedHMInsert game entry m
          else m
      Nothing -> sizedHMInsert game entry m
  tttypeLookup game = HM.lookup (hash game) . sizedHMMap <$> S.get
