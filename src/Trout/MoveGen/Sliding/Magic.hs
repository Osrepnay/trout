module Trout.MoveGen.Sliding.Magic
    {-( bishopMovesMagic
    , rookMovesMagic
    )-} where

import           Data.Foldable
import           Data.IntMap.Strict            (IntMap)
import qualified Data.IntMap.Strict            as I
import           Data.Vector                   (Vector, (!))
import qualified Data.Vector                   as V
import           Data.Word
import           Trout.Bitboard
import           Trout.MoveGen.Sliding.Classic
import           Trout.MoveGen.Sliding.Magics

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
allMapped :: Word64 -> Int -> Bitboard -> IntMap Bitboard
allMapped magic bits mask = I.fromList $
    (\b -> (genKey b magic bits, b)) . mapToMask mask <$> [0..bit bits]

bishopMagicTable :: Vector (IntMap Bitboard)
bishopMagicTable = V.zipWith (<$>)
    (bishopMovesClassic <$> V.fromList [0..63])
    (V.zipWith3 allMapped bishopMagics bishopBits bishopMasks)

rookMagicTable :: Vector (IntMap Bitboard)
rookMagicTable = V.zipWith (<$>)
    (rookMovesClassic <$> V.fromList [0..63])
    (V.zipWith3 allMapped rookMagics rookBits rookMasks)

bishopMovesMagic :: Int -> Bitboard -> Bitboard
bishopMovesMagic sq block = bishopMagicTable
    ! sq
    I.! genKey masked (bishopMagics ! sq) (bishopBits ! sq)
  where masked = block .&. (bishopMasks ! sq)

rookMovesMagic :: Int -> Bitboard -> Bitboard
rookMovesMagic sq block = rookMagicTable
    ! sq
    I.! genKey masked (rookMagics ! sq) (rookBits ! sq)
  where masked = block .&. (rookMasks ! sq)
