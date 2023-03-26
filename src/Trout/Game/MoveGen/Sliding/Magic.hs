module Trout.Game.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    , bishopMagicTable, rookMagicTable
    ) where

import           Data.Foldable                      (foldl')
import           Data.Functor                       ((<&>))
import           Data.Vector.Primitive              (Vector, (!), (//))
import qualified Data.Vector.Primitive              as V
import           Data.Word                          (Word64)
import           Trout.Bitboard
    ( Bitboard
    , bit
    , clearBit
    , complement
    , countTrailingZeros
    , fileA
    , fileH
    , rank1
    , rank8
    , (!<<.)
    , (!>>.)
    , (.&.)
    , (.|.)
    )
import           Trout.Game.MoveGen.Sliding.Classic
    ( bishopMovesClassic
    , bishopRays
    , rookMovesClassic
    , rookRays
    )
import           Trout.Game.MoveGen.Sliding.Magics
    ( bishopBits
    , bishopMagics
    , genKey
    , rookBits
    , rookMagics
    )

bishopMasks :: Vector Bitboard
bishopMasks = V.generate
    64
    ((complement (rank1 .|. rank8 .|. fileA .|. fileH) .&.)
        . foldl' (.|.) 0
        . (<$> bishopRays)
        . flip (!))

rookMasks :: Vector Bitboard
rookMasks = V.generate
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
allMapped :: Word64 -> Int -> Int -> Bitboard -> Vector Bitboard
allMapped magic bits len mask = V.replicate len 0
    // ((\b -> (genKey b magic bits, b)) . mapToMask mask <$> [0..bit bits])

bishopMagicTable :: Vector Bitboard
bishopMagicTable = V.concat
    $ [0..63]
    <&> \sq -> flip bishopMovesClassic sq
        `V.map` allMapped
            (bishopMagics ! sq)
            (bishopBits ! sq)
            1024
            (bishopMasks ! sq)

rookMagicTable :: Vector Bitboard
rookMagicTable = V.concat
    $ [0..63]
    <&> \sq -> flip rookMovesClassic sq
        `V.map` allMapped
            (rookMagics ! sq)
            (rookBits ! sq)
            4096
            (rookMasks ! sq)

bishopMovesMagic :: Bitboard -> Int -> Bitboard
bishopMovesMagic block sq = bishopMagicTable
    ! (sq * 1024 + genKey masked (bishopMagics ! sq) (bishopBits ! sq))
  where masked = block .&. (bishopMasks ! sq)
{-# INLINE bishopMovesMagic #-}

rookMovesMagic :: Bitboard -> Int -> Bitboard
rookMovesMagic block sq = rookMagicTable
    ! (sq * 4096 + genKey masked (rookMagics ! sq) (rookBits ! sq))
  where masked = block .&. (rookMasks ! sq)
{-# INLINE rookMovesMagic #-}
