module Trout.PieceInfo
    ( Color (..)
    , Piece
    , pawn, knight, bishop, rook, queen, king
    ) where

data Color = White | Black
type Piece = Int
pawn   :: Int
knight :: Int
bishop :: Int
rook   :: Int
queen  :: Int
king   :: Int
pawn   = 0
knight = 1
bishop = 2
rook   = 3
queen  = 4
king   = 5
