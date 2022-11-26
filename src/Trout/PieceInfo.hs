module Trout.PieceInfo
    ( Color (..)
    , Piece (..)
    ) where

data Color = White | Black
    deriving (Eq, Show)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Show)
