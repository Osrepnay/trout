module Trout.Game.MoveGen
    ( pawnWhiteAttackTable, pawnBlackAttackTable, pawnsMoves
    , knightTable, knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingTable, kingMoves
    , SpecialMove (..)
    , Move (..)
    ) where

import           Data.Maybe                       (maybeToList)
import           Data.Vector.Primitive            (Vector, (!))
import qualified Data.Vector.Primitive            as V
import           Trout.Bitboard
    ( Bitboard
    , complement
    , fileA
    , fileH
    , fromSqs
    , rank1
    , rank3
    , rank4
    , rank5
    , rank6
    , rank8
    , setBit
    , toSqs
    , unblocked
    , xyToSq
    , (!<<.)
    , (!>>.)
    , (.&.)
    , (.|.)
    )
import           Trout.Game.Move                  (Move (..), SpecialMove (..))
import           Trout.Game.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    )
import           Trout.Piece                      (Color (..), Piece (..))

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
-- dont use for now it's slowwer
pawnWhiteAttackTable :: Vector Bitboard
pawnWhiteAttackTable = tableGen [(-1, 1), (1, 1)]

pawnBlackAttackTable :: Vector Bitboard
pawnBlackAttackTable = tableGen [(-1, -1), (1, -1)]

promos :: [Piece]
promos = [Knight, Bishop, Rook, Queen]

-- batched!
pawnsMoves :: Maybe Int -> Color -> Bitboard -> Bitboard -> Bitboard -> [Move]
pawnsMoves enPSq White block myBlock pawnBB = concat
    [ enPassant
    , [Move Pawn PawnDouble (to - 16) to | to <- toSqs doubles]
    , [Move Pawn (Promotion p) (to - 8)  to | to <- toSqs normalP, p <- promos]
    , [Move Pawn (Promotion p) (to - 7)  to | to <- toSqs leftP, p <- promos]
    , [Move Pawn (Promotion p) (to - 9)  to | to <- toSqs rightP, p <- promos]
    , [Move Pawn Normal (to - 8) to | to <- toSqs normalN]
    , [Move Pawn Normal (to - 7) to | to <- toSqs leftN]
    , [Move Pawn Normal (to - 9) to | to <- toSqs rightN]
    ]
  where
    normals = pawnBB !<<. 8 .&. complement block
    doubles = (normals .&. rank3) !<<. 8 .&. complement block
    leftCaptures = (pawnBB .&. complement fileA)
        !<<. 7
        .&. block
        .&. complement myBlock
    rightCaptures = (pawnBB .&. complement fileH)
        !<<. 9
        .&. block
        .&. complement myBlock
    normalN = normals .&. complement rank8
    leftN = leftCaptures .&. complement rank8
    rightN = rightCaptures .&. complement rank8
    normalP = normals .&. rank8
    leftP = leftCaptures .&. rank8
    rightP = rightCaptures .&. rank8
    enPassant = maybeToList enPSq
        >>= \enP ->
            let mask = rank5 .&. 0 `setBit` (enP - 1) `setBit` (enP + 1)
            in (\sq -> Move Pawn (EnPassant enP) sq (enP + 8))
                <$> toSqs (pawnBB .&. mask)
pawnsMoves enPSq Black block myBlock pawnBB = concat
    [ enPassant
    , [Move Pawn PawnDouble (to + 16) to | to <- toSqs doubles]
    , [Move Pawn (Promotion p) (to + 8)  to | to <- toSqs normalP, p <- promos]
    , [Move Pawn (Promotion p) (to + 9)  to | to <- toSqs leftP, p <- promos]
    , [Move Pawn (Promotion p) (to + 7)  to | to <- toSqs rightP, p <- promos]
    , [Move Pawn Normal (to + 8) to | to <- toSqs normalN]
    , [Move Pawn Normal (to + 9) to | to <- toSqs leftN]
    , [Move Pawn Normal (to + 7) to | to <- toSqs rightN]
    ]
  where
    normals = pawnBB !>>. 8 .&. complement block
    doubles = (normals .&. rank6) !>>. 8 .&. complement block
    leftCaptures = (pawnBB .&. complement fileA)
        !>>. 9
        .&. block
        .&. complement myBlock
    rightCaptures = (pawnBB .&. complement fileH)
        !>>. 7
        .&. block
        .&. complement myBlock
    normalN = normals .&. complement rank1
    leftN = leftCaptures .&. complement rank1
    rightN = rightCaptures .&. complement rank1
    normalP = normals .&. rank1
    leftP = leftCaptures .&. rank1
    rightP = rightCaptures .&. rank1
    enPassant = maybeToList enPSq
        >>= \enP ->
            let mask = rank4 .&. 0 `setBit` (enP - 1) `setBit` (enP + 1)
            in (\sq -> Move Pawn (EnPassant enP) sq (enP - 8))
                <$> toSqs (pawnBB .&. mask)

knightTable :: Vector Bitboard
knightTable = tableGen
    [ (-2, -1), (-2, 1)
    , (-1, -2), (-1, 2)
    , ( 1, -2), ( 1, 2)
    , ( 2, -1), ( 2, 1)
    ]

knightMoves :: Bitboard -> Bitboard -> Int -> [Move]
knightMoves _ myBlock sq = Move Knight Normal sq
    <$> toSqs (knightTable ! sq .&. complement myBlock)

bishopMoves :: Bitboard -> Bitboard -> Int -> [Move]
bishopMoves block myBlock sq = Move Bishop Normal sq
    <$> toSqs (bishopMovesMagic block sq .&. complement myBlock)

rookMoves :: Bitboard -> Bitboard -> Int -> [Move]
rookMoves block myBlock sq = Move Rook Normal sq
    <$> toSqs (rookMovesMagic block sq .&. complement myBlock)

queenMoves :: Bitboard -> Bitboard -> Int -> [Move]
queenMoves block myBlock sq = Move Queen Normal sq
    <$> toSqs
        ((bishopMovesMagic block sq .|. rookMovesMagic block sq)
            .&. complement myBlock)

kingTable :: Vector Bitboard
kingTable = tableGen
    [ (-1, -1), (-1, 0), (-1, 1)
    , ( 0, -1),          ( 0, 1)
    , ( 1, -1), ( 1, 0), ( 1, 1)
    ]

kingMoves :: Bool -> Bool -> Bitboard -> Bitboard -> Int -> [Move]
kingMoves kAllowed qAllowed block myBlock sq = concat
    [ [Move King CastleKing sq (sq + 2) | castleK]
    , [Move King CastleQueen sq (sq - 2) | castleQ]
    , Move King Normal sq <$> toSqs (kingTable ! sq .&. complement myBlock)
    ]
  where
    castleK = kAllowed && unblocked block (sq + 1) && unblocked block (sq + 2)
    castleQ = qAllowed
        && unblocked block (sq - 1)
        && unblocked block (sq - 2)
        && unblocked block (sq - 3)
