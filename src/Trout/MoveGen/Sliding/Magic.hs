module Trout.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    ) where

import           Data.Foldable
import           Data.Vector                   (Vector, (!))
import qualified Data.Vector                   as V
import           Trout.Bitboard
import           Trout.MoveGen.Sliding.Classic
import           Trout.MoveGen.Sliding.Magics

bishopMasks :: Vector Bitboard
bishopMasks = (.&.complement 0x7E7E7E7E7E7E00) . foldl' (.|.) 0
    <$> sequence bishopRays

rookMasks :: Vector Bitboard
rookMasks = foldl' (.|.) 0
    . zipWith (\r e -> r .&. complement e)
        [ 0xFF00000000000000
        , 0x8080808080808080
        , 0x00000000000000FF
        , 0x0101010101010101
        ]
    <$> sequence rookRays

mapToMask :: Bitboard -> Int -> Bitboard
mapToMask 0 _ = 0
mapToMask mask idx = mapToMask
    (mask .&. fromIntegral (complement (idx .&. 1)) !<<. countLeadingZeros mask)
    (idx !>>. 1)

-- maps all possible blockers to the mask
magicSquare :: Int -> Bitboard -> Vector Bitboard
magicSquare bits mask = mapToMask mask <$> V.fromList [0..bit bits]

bishopMagicTable :: Vector (Vector Bitboard)
bishopMagicTable =
    fmap (uncurry bishopMovesClassic)
    . V.zip (V.fromList [0..63])
        <$> (magicSquare <$> bishopBits <*> bishopMasks)

rookMagicTable :: Vector (Vector Bitboard)
rookMagicTable =
    fmap (uncurry rookMovesClassic)
    . V.zip (V.fromList [0..63])
        <$> (magicSquare <$> rookBits <*> rookMasks)

bishopMovesMagic :: Int -> Bitboard -> Bitboard
bishopMovesMagic sq block = bishopMagicTable
    ! sq
    ! fromIntegral ((masked * (bishopMagics ! sq)) !>>. (64 - (bishopBits ! sq)))
  where masked = block .&. (bishopMasks ! sq)

rookMovesMagic :: Int -> Bitboard -> Bitboard
rookMovesMagic sq block = rookMagicTable
    ! sq
    ! fromIntegral ((masked * (rookMagics ! sq)) !>>. (64 - (rookBits ! sq)))
  where masked = block .&. (rookMasks ! sq)
