module Trout.Game.Move
  ( SpecialMove (..),
    Move (..),
    uciShowMove,
    nullMove,
  )
where

import Data.Char (chr, ord)
import Foreign (Ptr, Storable (..), castPtr)
import Trout.Piece (PieceType (..))

-- moves that dont fit normal piece things
data SpecialMove
  = Normal
  | PawnDouble -- double moe forware
  | CastleKing
  | CastleQueen
  | EnPassant Int -- en passant pawn squaree
  | Promotion PieceType -- promote piece
  deriving (Eq, Show)

-- TODO consider moveIsCapture :: Bool
data Move = Move
  { movePiece :: !PieceType,
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

instance Storable Move where
  sizeOf :: Move -> Int
  sizeOf _ = 5 * sizeOf (undefined :: Int)
  alignment :: Move -> Int
  alignment _ = alignment (undefined :: Int)
  peek :: Ptr Move -> IO Move
  peek ptr = do
    let casted = castPtr ptr :: Ptr Int
    pieceInt <- peek casted
    let piece = toEnum pieceInt
    specialTypeInt <- peekElemOff casted 1
    special <- case specialTypeInt of
      0 -> pure Normal
      1 -> pure PawnDouble
      2 -> pure CastleKing
      3 -> pure CastleQueen
      4 -> EnPassant <$> peekElemOff casted 2
      5 -> Promotion . toEnum <$> peekElemOff casted 2
      _ -> error "unknown special move type"
    from <- peekElemOff casted 3
    to <- peekElemOff casted 4
    pure (Move piece special from to)
  poke :: Ptr Move -> Move -> IO ()
  poke ptr (Move piece special from to) = do
    let casted = castPtr ptr :: Ptr Int
    poke casted (fromEnum piece)
    case special of
      Normal -> pokeElemOff casted 1 0
      PawnDouble -> pokeElemOff casted 1 1
      CastleKing -> pokeElemOff casted 1 2
      CastleQueen -> pokeElemOff casted 1 3
      EnPassant enp -> do
        pokeElemOff casted 1 4
        pokeElemOff casted 2 enp
      Promotion p -> do
        pokeElemOff casted 1 5
        pokeElemOff casted 2 (fromEnum p)
    pokeElemOff casted 3 from
    pokeElemOff casted 4 to
