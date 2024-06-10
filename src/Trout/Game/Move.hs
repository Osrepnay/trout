module Trout.Game.Move
  ( SpecialMove (..),
    Move (..),
    uciShowMove,
    nullMove,
  )
where

import Data.Char (chr, ord)
import Trout.Piece (Piece (..))

-- moves that dont fit normal piece things
data SpecialMove
  = Normal
  | PawnDouble -- double moe forware
  | CastleKing
  | CastleQueen
  | EnPassant Int -- en passant pawn squaree
  | Promotion Piece -- promote piece
  deriving (Eq, Show)

-- TODO consider moveIsCapture :: Bool
data Move = Move
  { movePiece :: !Piece,
    moveSpecial :: !SpecialMove,
    moveFrom :: !Int,
    moveTo :: !Int
  }
  deriving (Eq, Show)

nullMove :: Move
nullMove = Move Pawn Normal 0 0

-- show uci format of move
uciShowMove :: Move -> String
uciShowMove (Move _ special from to) =
  uciShowSquare from
    ++ uciShowSquare to
    ++ case special of
      Promotion pp -> case pp of
        Pawn -> "p"
        Knight -> "n"
        Bishop -> "b"
        Rook -> "r"
        Queen -> "q"
        King -> "k"
      _ -> ""
  where
    uciShowSquare sq =
      [ chr (sq `rem` 8 + ord 'a'),
        chr (sq `quot` 8 + ord '1')
      ]
