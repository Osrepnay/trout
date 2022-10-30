module Trout.MoveGen
    ( pawnMoves
    , knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingMoves
    , SpecialMove(..)
    , Move(..)
    ) where

import Data.Maybe
import Trout.Bitboard
import Trout.MoveGen.Sliding.Magic
import Trout.PieceInfo

-- consider moving non sliding movegen to a table
-- cache can be weird though? idk

-- moves that dont fit normal piece things
data SpecialMove
    = Normal
    | Castle Bool -- kingside?
    | EnPassant Int -- en passant pawn squaree
    | Promotion Int -- promote piece
    | PawnDouble -- double moe forware
    deriving (Eq, Show)
-- from, to
data Move = Move SpecialMove Int Int deriving (Eq, Show)

pawnMoves :: Maybe Int -> Int -> Color -> Bitboard -> [Move]
pawnMoves enPSq sq White block =
    [Move p sq (sq + 8) | frontOpen,    p <- promotes] ++
    [Move p sq (sq + 7) | captureLeft,  p <- promotes] ++
    [Move p sq (sq + 9) | captureRight, p <- promotes] ++
    [Move PawnDouble sq (sq + 16) | doubleFrontOpen] ++ -- can't promote
    [Move (EnPassant en) sq (en + 8) | en <- enPassant]
  where
    enPassant = filter
        ((rank5 .&. bit sq /= 0 &&) . (== 1) . abs . (sq -))
        (maybeToList enPSq)
    promotes = Normal : [Promotion p | rank8 .&. bit sq /= 0, p <- [1..4]]
    frontOpen = unblocked (sq + 8)
    doubleFrontOpen = frontOpen && (rank2 .&. bit sq /= 0) && unblocked (sq + 16)
    captureLeft  = blocked (sq + 7) && fileA .&. bit sq == 0
    captureRight = blocked (sq + 9) && fileH .&. bit sq == 0
    unblocked s = inBoard s && not (testBit block s)
    blocked s = inBoard s && testBit block s
pawnMoves enPSq sq Black block =
    [Move p sq (sq - 8) | frontOpen,    p <- promotes] ++
    [Move p sq (sq - 9) | captureLeft,  p <- promotes] ++
    [Move p sq (sq - 7) | captureRight, p <- promotes] ++
    [Move PawnDouble sq (sq - 16) | doubleFrontOpen] ++
    [Move (EnPassant en) sq (en - 8) | en <- enPassant]
  where
    enPassant = filter
        ((rank4 .&. bit sq /= 0 &&) . (== 1) . abs . (sq -))
        (maybeToList enPSq)
    promotes = Normal : [Promotion p | rank1 .&. bit sq /= 0, p <- [1..4]]
    frontOpen = unblocked (sq - 8)
    doubleFrontOpen = frontOpen && (rank7 .&. bit sq /= 0) && unblocked (sq - 16)
    captureLeft  = blocked (sq - 9) && fileA .&. bit sq == 0
    captureRight = blocked (sq - 7) && fileH .&. bit sq == 0
    unblocked s = inBoard s && not (testBit block s)
    blocked s = inBoard s && testBit block s

knightMoves :: Int -> Bitboard -> [Move]
knightMoves sq _ = Move Normal sq
    <$> filter inBoard ((sq +) <$> possDiffs)
  where
    possDiffs = filter inBoard 
        (mconcat [[a * 8 + b, b * 8 + a] | a <- [-2, 2], b <- [-1, 1]])

bishopMoves :: Int -> Bitboard -> [Move]
bishopMoves sq block = Move Normal sq
    <$> toSqs (bishopMovesMagic sq block)

rookMoves :: Int -> Bitboard -> [Move]
rookMoves sq block = Move Normal sq
    <$> toSqs (rookMovesMagic sq block)

queenMoves :: Int -> Bitboard -> [Move]
queenMoves sq block = Move Normal sq
    <$> toSqs (bishopMovesMagic sq block .|. rookMovesMagic sq block)

kingMoves :: Bool -> Bool -> Int -> Bitboard -> [Move]
kingMoves kAllowed qAllowed sq block =
    [Move (Castle True) sq (sq + 2) | castleK] ++
    [Move (Castle False) sq (sq - 2) | castleQ] ++
    (Move Normal sq <$> filter inBoard ((sq +) <$> possDiffs))
  where
    possDiffs = [xyToSq x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
    castleK = kAllowed && unblocked (sq + 1) && unblocked (sq + 2)
    castleQ = qAllowed && unblocked (sq - 1) && unblocked (sq - 2) && unblocked (sq - 3)
    unblocked s = inBoard s && not (testBit block s)
