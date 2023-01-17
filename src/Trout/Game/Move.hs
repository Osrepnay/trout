module Trout.Game.Move
    ( SpecialMove(..)
    , Move(..)
    ) where

import Trout.Piece

-- moves that dont fit normal piece things
data SpecialMove
    = Normal
    | PawnDouble -- double moe forware
    | CastleKing
    | CastleQueen
    | EnPassant Int -- en passant pawn squaree
    | Promotion Piece -- promote piece
    deriving (Eq, Show)

data Move = Move
    { movePiece   :: Piece
    , moveSpecial :: SpecialMove
    , moveFrom    :: Int
    , moveTo      :: Int
    } deriving (Eq, Show)
