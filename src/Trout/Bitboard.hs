module Trout.Bitboard
    ( Bitboard
#if !MIN_VERSION_base(4, 17, 0)
    , (!>>.), (!<<.)
#endif
    ) where

import Data.Bits
import Data.Word

type Bitboard = Word64

#if !MIN_VERSION_base(4, 17, 0)
-- same code as in new base
(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR
infixl 8 !>>.

(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL
infixl 8 !<<.
#endif

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
