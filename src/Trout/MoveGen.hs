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
    | PawnDouble -- double moe forware
    | Castle Bool -- kingside?
    | EnPassant Int -- en passant pawn squaree
    | Promotion Int -- promote piece
    deriving (Eq, Show)

-- piece, special move type, from, to
data Move = Move
    { movePiece :: Piece
    , moveSpecial :: SpecialMove
    , moveFrom :: Int
    , moveTo :: Int
    } deriving (Eq, Show)

pawnMoves :: Maybe Int -> Color -> Bitboard -> Int -> [Move]
pawnMoves enPSq White block sq =
    [Move pawn p sq (sq + 8) | frontOpen,    p <- promotes] ++
    [Move pawn p sq (sq + 7) | captureLeft,  p <- promotes] ++
    [Move pawn p sq (sq + 9) | captureRight, p <- promotes] ++
    [Move pawn PawnDouble sq (sq + 16) | doubleFrontOpen] ++ -- can't promote
    [Move pawn (EnPassant en) sq (en + 8) | en <- enPassant]
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
pawnMoves enPSq Black block sq =
    [Move pawn p sq (sq - 8) | frontOpen,    p <- promotes] ++
    [Move pawn p sq (sq - 9) | captureLeft,  p <- promotes] ++
    [Move pawn p sq (sq - 7) | captureRight, p <- promotes] ++
    [Move pawn PawnDouble sq (sq - 16) | doubleFrontOpen] ++
    [Move pawn (EnPassant en) sq (en - 8) | en <- enPassant]
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

knightMoves :: Bitboard -> Int -> [Move]
knightMoves _ sq = Move knight Normal sq <$> newSqs
  where
    sqx = sq `rem` 8
    sqy = sq `quot` 8
    ds =
        [ (-2, -1), (-2, 1)
        , (-1, -2), (-1, 2)
        , ( 1, -2), ( 1, 2)
        , ( 2, -1), ( 2, 1)
        ]
    newSqs =
        [ nsq
        | (dx, dy) <- ds
        , let nsqx = sqx + dx
        , 0 <= nsqx, nsqx < 8
        , let nsqy = sqy + dy
        , 0 <= nsqy, nsqy < 8
        , let nsq = xyToSq nsqx nsqy
        , inBoard nsq
        ]

bishopMoves :: Bitboard -> Int -> [Move]
bishopMoves block sq = Move bishop Normal sq
    <$> toSqs (bishopMovesMagic sq block)

rookMoves :: Bitboard -> Int -> [Move]
rookMoves block sq = Move rook Normal sq
    <$> toSqs (rookMovesMagic sq block)

queenMoves :: Bitboard -> Int -> [Move]
queenMoves block sq = Move queen Normal sq
    <$> toSqs (bishopMovesMagic sq block .|. rookMovesMagic sq block)

kingMoves :: Bool -> Bool -> Bitboard -> Int -> [Move]
kingMoves kAllowed qAllowed block sq =
    [Move king (Castle True) sq (sq + 2) | castleK] ++
    [Move king (Castle False) sq (sq - 2) | castleQ] ++
    (Move king Normal sq <$> filter inBoard ((sq +) <$> possDiffs))
  where
    possDiffs = [xyToSq x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
    castleK = kAllowed && unblocked (sq + 1) && unblocked (sq + 2)
    castleQ = qAllowed && unblocked (sq - 1) && unblocked (sq - 2) && unblocked (sq - 3)
    unblocked s = inBoard s && not (testBit block s)
