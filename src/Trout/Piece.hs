module Trout.Piece
  ( Color (..),
    Piece (..),
    other,
  )
where

data Color = White | Black
  deriving (Eq, Show)

other :: Color -> Color
other White = Black
other Black = White

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Show)
