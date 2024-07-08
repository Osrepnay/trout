{-# LANGUAGE PatternSynonyms #-}

module Trout.Piece
  ( Color (White, Black),
    PieceType (Pawn, Knight, Bishop, Rook, Queen, King),
    Piece (..),
    other,
    colorSign,
  )
where

import Data.Bits (xor)

newtype Color = Color Int deriving (Eq, Show)

-- unsafe, should be fast though
instance Enum Color where
  toEnum = Color
  fromEnum (Color i) = i

pattern White, Black :: Color
pattern White = Color 0
pattern Black = Color 1

{-# COMPLETE White, Black #-}

other :: Color -> Color
other = toEnum . xor 1 . fromEnum

colorSign :: Color -> Int
colorSign = negate . subtract 1 . (* 2) . fromEnum

newtype PieceType = PieceType Int deriving (Eq, Show)

pattern Pawn, Knight, Bishop, Rook, Queen, King :: PieceType
pattern Pawn = PieceType 0
pattern Knight = PieceType 1
pattern Bishop = PieceType 2
pattern Rook = PieceType 3
pattern Queen = PieceType 4
pattern King = PieceType 5

{-# COMPLETE Pawn, Knight, Bishop, Rook, Queen, King #-}

instance Enum PieceType where
  toEnum = PieceType
  fromEnum (PieceType p) = p

-- TODO replace Color -> PieceType instances with Piece
data Piece = Piece
  { pieceColor :: !Color,
    pieceType :: !PieceType
  }
  deriving (Eq, Show)
