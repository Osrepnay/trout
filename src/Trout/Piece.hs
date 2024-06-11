module Trout.Piece
  ( Color (..),
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
