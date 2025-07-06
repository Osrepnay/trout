{-# LANGUAGE PatternSynonyms #-}

module Trout.Game.Move
  ( SpecialMove (..),
    Move (.., NullMove),
    uciShowMove,
  )
where

import Data.Char (chr, ord)
import Data.Int (Int8)
import Foreign (Ptr, Storable (..), castPtr)
import Trout.Piece (PieceType (..))

-- moves that dont fit normal piece things
data SpecialMove
  = Normal
  | PawnDouble -- double moe forware
  | CastleKing
  | CastleQueen
  | EnPassant Int -- en passant pawn scolumn
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

pattern NullMove :: Move
pattern NullMove = Move Pawn Normal 0 0

-- show uci format of move
uciShowMove :: Move -> String
uciShowMove NullMove = "0000"
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
  -- 5 int8s, 40 bits total
  sizeOf _ = 5 * sizeOf (undefined :: Int8)
  alignment :: Move -> Int
  alignment _ = alignment (undefined :: Int8)
  peek :: Ptr Move -> IO Move
  peek ptr = do
    let casted = castPtr ptr :: Ptr Int8
    pieceInt <- fromIntegral <$> peek casted
    let piece = toEnum pieceInt
    specialTypeInt <- peekElemOff casted 1
    special <- case specialTypeInt of
      0 -> pure Normal
      1 -> pure PawnDouble
      2 -> pure CastleKing
      3 -> pure CastleQueen
      4 -> EnPassant . fromIntegral <$> peekElemOff casted 2
      5 -> Promotion . toEnum . fromIntegral <$> peekElemOff casted 2
      _ -> error "unknown special move type"
    from <- fromIntegral <$> peekElemOff casted 3
    to <- fromIntegral <$> peekElemOff casted 4
    pure (Move piece special from to)
  poke :: Ptr Move -> Move -> IO ()
  poke ptr (Move piece special from to) = do
    let casted = castPtr ptr :: Ptr Int8
    poke casted (fromIntegral (fromEnum piece))
    case special of
      Normal -> pokeElemOff casted 1 0
      PawnDouble -> pokeElemOff casted 1 1
      CastleKing -> pokeElemOff casted 1 2
      CastleQueen -> pokeElemOff casted 1 3
      EnPassant enp -> do
        pokeElemOff casted 1 4
        pokeElemOff casted 2 (fromIntegral enp)
      Promotion p -> do
        pokeElemOff casted 1 5
        pokeElemOff casted 2 (fromIntegral (fromEnum p))
    pokeElemOff casted 3 (fromIntegral from)
    pokeElemOff casted 4 (fromIntegral to)
