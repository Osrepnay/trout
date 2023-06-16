module Trout.Search.TranspositionTable
    ( NodeType (..)
    , TTEntry (..)
    , TTKey
    , TranspositionTable
    , newTT
    , clearTT
    , insertTT
    , readTT
    ) where

import           Data.Hashable       (hash)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import           Trout.Game          (HGame)
import           Trout.Game.Move     (Move)

data NodeType
    = ExactNode -- not fail high or fail low
    | CutNode -- failed high/beta cutoff, move is "too good"
    | AllNode -- failed low, move is "too bad"
    deriving (Eq, Show)

data TTEntry = TTEntry
    { entryEval  :: !Int
    , entryType  :: !NodeType
    , entryDepth :: !Int
    , entryMove  :: !Move
    } deriving (Eq, Show)

type TTKey = HGame
type TranspositionTable = IOVector (Maybe (Int, TTEntry))

-- create a new empty transposition table of the given size
newTT :: Int -> IO TranspositionTable
newTT size = do
    table <- MV.new size
    MV.set table Nothing
    pure table

-- clear transposition table
clearTT :: TranspositionTable -> IO ()
clearTT table = MV.set table Nothing

hashToIdx :: Int -> Int -> Int
hashToIdx h tableLen = fromIntegral
    $ (fromIntegral h :: Word)
    `rem` (fromIntegral tableLen :: Word)
{-# INLINE hashToIdx #-}

insertTT :: TTKey -> TTEntry -> TranspositionTable -> IO ()
insertTT hgame entry table = do
    let hgHash = hash hgame
    let hgIdx = hashToIdx hgHash (MV.length table)
    MV.write table hgIdx (Just (hgHash, entry))
{-# INLINE insertTT #-}

readTT :: TTKey -> TranspositionTable -> IO (Maybe TTEntry)
readTT hgame table = do
    let hgHash = hash hgame
    let hgIdx = hashToIdx hgHash (MV.length table)
    maybeRes <- MV.read table hgIdx
    pure
        $ maybeRes
        >>= \(fullHash, entry) ->
            if fullHash == hgHash
            then Just entry
            else Nothing
{-# INLINE readTT #-}
