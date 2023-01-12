module Trout.Game.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    ) where

import           Data.Foldable
import           Data.Vector                        (Vector, (!), (//))
import qualified Data.Vector                        as V
import           Data.Word
import           Trout.Bitboard
import           Trout.Game.MoveGen.Sliding.Classic
import           Trout.Game.MoveGen.Sliding.Magics

bishopMasks :: Vector Bitboard
bishopMasks = (complement (rank1 .|. rank8 .|. fileA .|. fileH) .&.)
    . foldl' (.|.) 0
    . (<$> bishopRays)
    . flip (!)
    <$> V.fromList [0..63]

rookMasks :: Vector Bitboard
rookMasks = foldl' (.|.) 0
    . zipWith (.&.)
        (complement <$>
            [ rank8
            , fileH
            , rank1
            , fileA
            ])
    . (<$> rookRays)
    . flip (!)
    <$> V.fromList [0..63]

-- maps an index to a mask
-- index has to be less than or equal to the number of set bits in the mask
mapToMask :: Bitboard -> Int -> Bitboard
mapToMask 0 _ = 0
mapToMask mask idx = ((fromIntegral idx .&. 1) !<<. maskLowest)
    .|. mapToMask (clearBit mask maskLowest) (idx !>>. 1)
  where maskLowest = countTrailingZeros mask

-- maps all possible blockers to the mask
allMapped :: Word64 -> Int -> Bitboard -> Vector Bitboard
allMapped magic bits mask = V.replicate (bit bits) 0
    // ((\b -> (genKey b magic bits, b)) . mapToMask mask <$> [0..bit bits])

bishopMagicTable :: Vector (Vector Bitboard)
bishopMagicTable = V.zipWith (<$>)
    (flip bishopMovesClassic <$> V.fromList [0..63])
    (V.zipWith3 allMapped bishopMagics bishopBits bishopMasks)

rookMagicTable :: Vector (Vector Bitboard)
rookMagicTable = V.zipWith (<$>)
    (flip rookMovesClassic <$> V.fromList [0..63])
    (V.zipWith3 allMapped rookMagics rookBits rookMasks)

bishopMovesMagic :: Bitboard -> Int -> Bitboard
bishopMovesMagic block sq = bishopMagicTable
    ! sq
    ! genKey masked (bishopMagics ! sq) (bishopBits ! sq)
  where masked = block .&. (bishopMasks ! sq)

rookMovesMagic :: Bitboard -> Int -> Bitboard
rookMovesMagic block sq = rookMagicTable
    ! sq
    ! genKey masked (rookMagics ! sq) (rookBits ! sq)
  where masked = block .&. (rookMasks ! sq)
