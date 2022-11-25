module Trout.Game.MoveGen
    ( pawnWhiteAttackTable, pawnBlackAttackTable, pawnMoves
    , knightTable, knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingTable, kingMoves
    , SpecialMove(..)
    , Move(..)
    ) where

import           Data.Maybe
import           Data.Vector                      (Vector, (!))
import qualified Data.Vector                      as V
import           Trout.Bitboard
import           Trout.Game.Move
import           Trout.Game.MoveGen.Sliding.Magic
import           Trout.PieceInfo

tableGen :: [(Int, Int)] -> Vector Bitboard
tableGen ds = V.fromList
    [ fromSqs
        [ xyToSq nSqX nSqY
        | (dx, dy) <- ds
        , let nSqX = sqX + dx
        , 0 <= nSqX, nSqX < 8
        , let nSqY = sqY + dy
        , 0 <= nSqY, nSqY < 8
        ]
    | sq <- [0..63]
    , let sqX = sq `rem` 8
    , let sqY = sq `quot` 8
    ]

-- only used for check detection rn
-- might use in actual pawnmoves, need perft speed test first
pawnWhiteAttackTable :: Vector Bitboard
pawnWhiteAttackTable = tableGen [(-1, 1), (1, 1)]

pawnBlackAttackTable :: Vector Bitboard
pawnBlackAttackTable = tableGen [(-1, -1), (1, -1)]

pawnMoves :: Maybe Int -> Color -> Bitboard -> Bitboard -> Int -> [Move]
pawnMoves enPSq White block myBlock sq = concat
    [ [Move pawn p sq (sq + 8) | frontOpen,    p <- promotes]
    , [Move pawn p sq (sq + 7) | captureLeft,  p <- promotes]
    , [Move pawn p sq (sq + 9) | captureRight, p <- promotes]
    , [Move pawn PawnDouble sq (sq + 16) | doubleFrontOpen] -- can't promote
    , [Move pawn (EnPassant en) sq (en + 8) | en <- enPassant]
    ]
  where
    enPassant = filter
        ((rank5 .&. bit sq /= 0 &&) . (== 1) . abs . (sq -))
        (maybeToList enPSq)
    promotes = Normal : [Promotion p | rank8 .&. bit sq /= 0, p <- [1..4]]
    frontOpen = unblocked block (sq + 8)
    doubleFrontOpen = frontOpen && (rank2 .&. bit sq /= 0) && unblocked block (sq + 16)
    captureLeft  = blocked (block .&. complement myBlock) (sq + 7) &&
        fileA .&. bit sq == 0
    captureRight = blocked (block .&. complement myBlock) (sq + 9) &&
        fileH .&. bit sq == 0
pawnMoves enPSq Black block myBlock sq = concat
    [ [Move pawn p sq (sq - 8) | frontOpen,    p <- promotes]
    , [Move pawn p sq (sq - 9) | captureLeft,  p <- promotes]
    , [Move pawn p sq (sq - 7) | captureRight, p <- promotes]
    , [Move pawn PawnDouble sq (sq - 16) | doubleFrontOpen]
    , [Move pawn (EnPassant en) sq (en - 8) | en <- enPassant]
    ]
  where
    enPassant = filter
        ((rank4 .&. bit sq /= 0 &&) . (== 1) . abs . (sq -))
        (maybeToList enPSq)
    promotes = Normal : [Promotion p | rank1 .&. bit sq /= 0, p <- [1..4]]
    frontOpen = unblocked block (sq - 8)
    doubleFrontOpen = frontOpen && (rank7 .&. bit sq /= 0) && unblocked block (sq - 16)
    captureLeft  = blocked (block .&. complement myBlock) (sq - 9) &&
        fileA .&. bit sq == 0
    captureRight = blocked (block .&. complement myBlock) (sq - 7) &&
        fileH .&. bit sq == 0

knightTable :: Vector Bitboard
knightTable = tableGen
    [ (-2, -1), (-2, 1)
    , (-1, -2), (-1, 2)
    , ( 1, -2), ( 1, 2)
    , ( 2, -1), ( 2, 1)
    ]

knightMoves :: Bitboard -> Bitboard -> Int -> [Move]
knightMoves _ myBlock sq = Move knight Normal sq
    <$> toSqs (knightTable ! sq .&. complement myBlock)

bishopMoves :: Bitboard -> Bitboard -> Int -> [Move]
bishopMoves block myBlock sq = Move bishop Normal sq
    <$> toSqs (bishopMovesMagic block sq .&. complement myBlock)

rookMoves :: Bitboard -> Bitboard -> Int -> [Move]
rookMoves block myBlock sq = Move rook Normal sq
    <$> toSqs (rookMovesMagic block sq .&. complement myBlock)

queenMoves :: Bitboard -> Bitboard -> Int -> [Move]
queenMoves block myBlock sq = Move queen Normal sq
    <$> toSqs ((bishopMovesMagic block sq .|. rookMovesMagic block sq) .&. complement myBlock)

kingTable :: Vector Bitboard
kingTable = tableGen
    [ (-1, -1), (-1, 0), (-1, 1)
    , ( 0, -1),          ( 0, 1)
    , ( 1, -1), ( 1, 0), ( 1, 1)
    ]

kingMoves :: Bool -> Bool -> Bitboard -> Bitboard -> Int -> [Move]
kingMoves kAllowed qAllowed block myBlock sq = concat
    [ [Move king (Castle True) sq (sq + 2) | castleK]
    , [Move king (Castle False) sq (sq - 2) | castleQ]
    , Move king Normal sq <$> toSqs (kingTable ! sq .&. complement myBlock)
    ]
  where
    castleK = kAllowed && unblocked block (sq + 1) && unblocked block (sq + 2)
    castleQ = qAllowed &&
        unblocked block (sq - 1) &&
        unblocked block (sq - 2) &&
        unblocked block (sq - 3)
