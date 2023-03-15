module Trout.Bitboard
    ( Bitboard
    , fromSqs
    , toSqs
    , xyToSq
    , inBoard
    , blocked, unblocked
    , showBitboard
    , rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8
    , fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH
    , module Data.Bits
#if !MIN_VERSION_base(4, 17, 0)
    , (!>>.), (!<<.)
#endif
    ) where

import Data.Bits
import Data.Foldable (foldl')
import Data.Word     (Word64)

type Bitboard = Word64

fromSqs :: [Int] -> Bitboard
fromSqs = foldl' setBit 0
{-# INLINE fromSqs #-}

toSqs :: Bitboard -> [Int]
toSqs 0 = []
toSqs b = trailing : toSqs (clearBit b trailing)
  where trailing = countTrailingZeros b
{-# INLINE toSqs #-}

xyToSq :: Int -> Int -> Int
xyToSq x y = y * 8 + x
{-# INLINE xyToSq #-}

inBoard :: Int -> Bool
inBoard sq = 0 <= sq && sq < 64
{-# INLINE inBoard #-}

blocked :: Bitboard -> Int -> Bool
blocked block sq = inBoard sq && testBit block sq
{-# INLINE blocked #-}

unblocked :: Bitboard -> Int -> Bool
unblocked block sq = inBoard sq && not (testBit block sq)
{-# INLINE unblocked #-}

#if !MIN_VERSION_base(4, 17, 0)
-- same code as in new base
(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR
infixl 8 !>>.
{-# INLINE (!>>.) #-}

(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL
infixl 8 !<<.
{-# INLINE (!<<.) #-}
#endif

showBitboard :: Bitboard -> String
showBitboard bb = init $ concat
    [
        [
            if testBit bb (r * 8 + c)
            then '#'
            else '*'
        | c <- [0..7]
        ] ++ "\n"
    | r <- [7, 6..0]
    ]

rank1 :: Bitboard
rank2 :: Bitboard
rank3 :: Bitboard
rank4 :: Bitboard
rank5 :: Bitboard
rank6 :: Bitboard
rank7 :: Bitboard
rank8 :: Bitboard

rank1 = 0x00000000000000FF
rank2 = 0x000000000000FF00
rank3 = 0x0000000000FF0000
rank4 = 0x00000000FF000000
rank5 = 0x000000FF00000000
rank6 = 0x0000FF0000000000
rank7 = 0x00FF000000000000
rank8 = 0xFF00000000000000

fileA :: Bitboard
fileB :: Bitboard
fileC :: Bitboard
fileD :: Bitboard
fileE :: Bitboard
fileF :: Bitboard
fileG :: Bitboard
fileH :: Bitboard

fileA = 0x0101010101010101
fileB = 0x0202020202020202
fileC = 0x0404040404040404
fileD = 0x0808080808080808
fileE = 0x1010101010101010
fileF = 0x2020202020202020
fileG = 0x4040404040404040
fileH = 0x8080808080808080
