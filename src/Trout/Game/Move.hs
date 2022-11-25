module Trout.Game.Move
    ( SpecialMove(..)
    , Move(..)
    ) where

import Trout.PieceInfo

-- moves that dont fit normal piece things
data SpecialMove
    = Normal
    | PawnDouble -- double moe forware
    | Castle Bool -- kingside?
    | EnPassant Int -- en passant pawn squaree
    | Promotion Int -- promote piece
    deriving (Eq, Show)

data Move = Move
    { movePiece   :: Piece
    , moveSpecial :: SpecialMove
    , moveFrom    :: Int
    , moveTo      :: Int
    } deriving (Eq, Show)
