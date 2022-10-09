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
-- from, to, captures
data Move = Move Int Int Bool SpecialMove

-- TODO incomplete
pawnMoves :: Maybe Int -> Int -> Color -> Bitboard -> [Move]
pawnMoves enPSq sq White block =
    [Move sq (sq + 8) False p | frontOpen,    p <- promotes] ++
    [Move sq (sq + 7) True  p | captureLeft,  p <- promotes] ++
    [Move sq (sq + 9) True  p | captureRight, p <- promotes] ++
    [Move sq (sq + 16) False Normal | doubleFrontOpen] ++ -- can't promote
    [Move sq en True (EnPassant en) | en <- enPassant]
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
    [Move sq (sq - 8) False p | frontOpen,    p <- promotes] ++
    [Move sq (sq - 9) False p | captureLeft,  p <- promotes] ++
    [Move sq (sq - 7) True  p | captureRight, p <- promotes] ++
    [Move sq (sq - 16) False Normal | doubleFrontOpen] ++
    [Move sq en True (EnPassant en) | en <- enPassant]
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
knightMoves sq block = (\m -> Move sq m (testBit block m) Normal)
    <$> filter inBoard ((sq +) <$> possDiffs)
  where
    possDiffs = [-17, -15, -10, -6, 6, 10, 15, 17]

bishopMoves :: Int -> Bitboard -> [Move]
bishopMoves sq block = (\m -> Move sq m (testBit block m) Normal)
    <$> toSqs (bishopMovesMagic sq block)

rookMoves :: Int -> Bitboard -> [Move]
rookMoves sq block = (\m -> Move sq m (testBit block m) Normal)
    <$> toSqs (rookMovesMagic sq block)

queenMoves :: Int -> Bitboard -> [Move]
queenMoves sq block = (\m -> Move sq m (testBit block m) Normal)
    <$> toSqs (bishopMovesMagic sq block .|. rookMovesMagic sq block)

kingMoves :: Bool -> Bool -> Int -> Bitboard -> [Move]
kingMoves castleK castleQ sq block =
    [Move sq (sq + 2) False (Castle True ) | castleK] ++
    [Move sq (sq - 2) False (Castle False) | castleQ] ++
    ((\m -> Move sq m (testBit block m) Normal)
        <$> filter inBoard ((sq +) <$> possDiffs))
  where
    possDiffs = [xyToSq x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
