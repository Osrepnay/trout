module Trout.Piece
  ( Color (..),
    Piece (..),
    PieceType (..),
    other,
  )
where

data Color = White | Black
  deriving (Eq, Show)

other :: Color -> Color
other White = Black
other Black = White

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Show)

-- TODO replace Color -> PieceType instances with Piece
data Piece = Piece
  { pieceColor :: !Color,
    pieceType :: !PieceType
  }
  deriving (Eq, Show)
