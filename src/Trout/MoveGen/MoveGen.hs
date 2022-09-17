module Trout.MoveGen.MoveGen
    ( pawnMoves
    , knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingMoves
    ) where

import Data.Bits
import Trout.Bitboard
import Trout.MoveGen.SlidingMoveGen
import Trout.PieceInfo

-- consider moving non sliding movegen to a table
-- cache can be weird though? idk
--
-- piece, from, to, captures
data Move = Move Int Int Int Bool

-- TODO incomplete
pawnMoves :: Bitboard -> Int -> Color -> Bitboard -> [Move]
pawnMoves otherPawns sq White block = 
     [Move 0 sq (sq + 8) False | frontOpen] ++
    [Move 0 sq (sq + 16) False | doubleFrontOpen] ++
    [Move 0 sq (sq + 7) True | captureLeft] ++
    [Move 0 sq (sq + 9) True | captureRight]
  where
    frontOpen       = unblocked (sq + 8)
    doubleFrontOpen = frontOpen && unblocked (sq + 16)
    captureLeft  = not $ unblocked (sq + 7)
    captureRight = not $ unblocked (sq + 9)
    -- also does checking to make sure square is in board
    unblocked s = inBoard s && not (testBit block s)

knightMoves :: Int -> Bitboard -> [Move]
knightMoves sq block = (\m -> Move 1 sq m (testBit block m))
    <$> filter inBoard ((sq +) <$> possDiffs)
  where
    possDiffs = [-17, -15, -10, -6, 6, 10, 15, 17]

bishopMoves :: Int -> Bitboard -> [Move]
bishopMoves sq block = (\m -> Move 2 sq m (testBit block m)) <$> toSqs (bishopMovesMagic sq block)

rookMoves :: Int -> Bitboard -> [Move]
rookMoves sq block = (\m -> Move 3 sq m (testBit block m)) <$> toSqs (rookMovesMagic sq block)

queenMoves :: Int -> Bitboard -> [Move]
queenMoves sq block = (\m -> Move 4 sq m (testBit block m))
    <$> toSqs (bishopMovesMagic sq block .|. rookMovesMagic sq block)

-- TODO incomplete
kingMoves :: Bool -> Bitboard -> Int -> Bitboard -> [Move]
kingMoves _castle _rooks sq block = (\m -> Move 5 sq m (testBit block m))
    <$> filter inBoard ((sq +) <$> possDiffs)
  where
    possDiffs = [xyToSq x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
