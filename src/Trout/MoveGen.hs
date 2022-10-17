module Trout.MoveGen
    ( pawnMoves
    , knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingMoves
    ) where

import Data.Maybe
import Trout.Bitboard
import Trout.MoveGen.Sliding.Magic
import Trout.PieceInfo

-- consider moving non sliding movegen to a table
-- cache can be weird though? idk
data SpecialMove
    = Normal
    | Castle Bool -- kingside?
    | EnPassant Int -- en passant pawn squaree
    | Promotion Int -- promote piece
-- from, to
data Move = Move SpecialMove Int Int

-- TODO incomplete
pawnMoves :: Maybe Int -> Int -> Color -> Bitboard -> [Move]
pawnMoves enPSq sq White block =
    [Move p sq (sq + 8) | frontOpen,    p <- promotes] ++
    [Move p sq (sq + 7) | captureLeft,  p <- promotes] ++
    [Move p sq (sq + 9) | captureRight, p <- promotes] ++
    [Move Normal sq (sq + 16) | doubleFrontOpen] ++ -- can't promote
    [Move (EnPassant en) sq en | en <- enPassant]
  where
    enPassant = filter
        (\e -> abs (e - sq) == 1 && (rank2 .&. bit sq /= 0))
        (maybeToList enPSq)
    promotes = if rank8 .&. bit sq /= 0
        then Normal : (Promotion <$> [1..4])
        else [Normal]
    frontOpen = unblocked (sq + 8)
    doubleFrontOpen = frontOpen && (rank2 .&. bit sq /= 0) && unblocked (sq + 16)
    captureLeft  = not $ unblocked (sq + 7)
    captureRight = not $ unblocked (sq + 9)
    unblocked s = inBoard s && not (testBit block s)
pawnMoves enPSq sq Black block =
    [Move p sq (sq - 8) | frontOpen,    p <- promotes] ++
    [Move p sq (sq - 9) | captureLeft,  p <- promotes] ++
    [Move p sq (sq - 7) | captureRight, p <- promotes] ++
    [Move Normal sq (sq - 16) | doubleFrontOpen] ++
    [Move (EnPassant en) sq en | en <- enPassant]
  where
    enPassant = filter
        (\e -> abs (e - sq) == 1 && (rank2 .&. bit sq /= 0))
        (maybeToList enPSq)
    promotes = if rank1 .&. bit sq /= 0
        then Normal : (EnPassant <$> [1..4])
        else [Normal]
    frontOpen = unblocked (sq - 8)
    doubleFrontOpen = frontOpen && (rank7 .&. bit sq /= 0) && unblocked (sq - 16)
    captureLeft  = not $ unblocked (sq - 9)
    captureRight = not $ unblocked (sq - 7)
    unblocked s = inBoard s && not (testBit block s)

knightMoves :: Int -> Bitboard -> [Move]
knightMoves sq _ = Move Normal sq
    <$> filter inBoard ((sq +) <$> possDiffs)
  where
    possDiffs = [-17, -15, -10, -6, 6, 10, 15, 17]

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
kingMoves castleK castleQ sq _ =
    [Move (Castle True) sq (sq + 2) | castleK] ++
    [Move (Castle False) sq (sq - 2) | castleQ] ++
    (Move Normal sq
        <$> filter inBoard ((sq +) <$> possDiffs))
  where
    possDiffs = [xyToSq x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
