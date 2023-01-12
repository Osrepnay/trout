module Trout.Game.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    ) where

import           Data.Foldable
import qualified Data.Vector                        as V
import           Data.Vector.Primitive              (Vector, (!), (//))
import qualified Data.Vector.Primitive              as PV
import           Data.Word
import           Trout.Bitboard
import           Trout.Game.MoveGen.Sliding.Classic
import           Trout.Game.MoveGen.Sliding.Magics

bishopMasks :: Vector Bitboard
bishopMasks = PV.generate
    64
    ((complement (rank1 .|. rank8 .|. fileA .|. fileH) .&.)
        . foldl' (.|.) 0
        . (<$> bishopRays)
        . flip (!))

rookMasks :: Vector Bitboard
rookMasks = PV.generate
    64
    (foldl' (.|.) 0
        . zipWith (.&.)
            (complement <$>
                [ rank8
                , fileH
                , rank1
                , fileA
                ])
        . (<$> rookRays)
        . flip (!))

-- maps an index to a mask
-- index has to be less than or equal to the number of set bits in the mask
mapToMask :: Bitboard -> Int -> Bitboard
mapToMask 0 _ = 0
mapToMask mask idx = ((fromIntegral idx .&. 1) !<<. maskLowest)
    .|. mapToMask (clearBit mask maskLowest) (idx !>>. 1)
  where maskLowest = countTrailingZeros mask

-- maps all possible blockers to the mask
allMapped :: Word64 -> Int -> Bitboard -> Vector Bitboard
allMapped magic bits mask = PV.replicate (bit bits) 0
    // ((\b -> (genKey b magic bits, b)) . mapToMask mask <$> [0..bit bits])

bishopMagicTable :: V.Vector (Vector Bitboard)
bishopMagicTable = V.generate
    64
    $ \sq -> flip bishopMovesClassic sq
        `PV.map` allMapped
            (bishopMagics ! sq)
            (bishopBits ! sq)
            (bishopMasks ! sq)

rookMagicTable :: V.Vector (Vector Bitboard)
rookMagicTable = V.generate
    64
    $ \sq -> flip rookMovesClassic sq
        `PV.map` allMapped
            (rookMagics ! sq)
            (rookBits ! sq)
            (rookMasks ! sq)

bishopMovesMagic :: Bitboard -> Int -> Bitboard
bishopMovesMagic block sq = bishopMagicTable
    V.! sq
    ! genKey masked (bishopMagics ! sq) (bishopBits ! sq)
  where masked = block .&. (bishopMasks ! sq)

rookMovesMagic :: Bitboard -> Int -> Bitboard
rookMovesMagic block sq = rookMagicTable
    V.! sq
    ! genKey masked (rookMagics ! sq) (rookBits ! sq)
  where masked = block .&. (rookMasks ! sq)
