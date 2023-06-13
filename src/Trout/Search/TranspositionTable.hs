module Trout.Search.TranspositionTable
    ( TTEntry (..)
    , TTKey
    , TranspositionTable
    , insertTT
    , readTT
    ) where

import           Data.Hashable       (hash)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import           Trout.Game          (HGame)
import           Trout.Game.Move     (Move)

data TTEntry = TTEntry
    { entryEval  :: !Int
    , entryDepth :: !Int
    , entryMove  :: !Move
    } deriving (Eq, Show)

type TTKey = HGame
type TranspositionTable = IOVector (Maybe (Int, TTEntry))

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
