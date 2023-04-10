module Trout.Game.MoveGen
    ( pawnsMoves
    , knightTable, knightMoves
    , bishopMoves
    , rookMoves
    , queenMoves
    , kingTable, kingMoves
    , SpecialMove (..)
    , Move (..)
    , DList, concatDL, mapOnes
    ) where

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
    , xyToSq
    , (!<<.)
    , (!>>.)
    , (.&.)
    , (.|.), clearBit, countTrailingZeros
    )
import           Trout.Game.Move                  (Move (..), SpecialMove (..))
import           Trout.Game.MoveGen.Sliding.Magic
    ( bishopMovesMagic
    , rookMovesMagic
    )
import           Trout.Piece                      (Color (..), Piece (..))
import Data.Functor ((<&>))

type DList a = [a] -> [a]

concatDL :: [DList a] -> DList a
concatDL = foldr (.) id

-- difference list version of toSqs
-- has mapping function because can't map on dlist without converting to list first
mapOnes :: (Int -> a) -> Bitboard -> DList a
mapOnes f = go
  where
    go 0 !acc = acc
    go bb !acc = go (bb `clearBit` trailing) (f trailing : acc)
      where trailing = countTrailingZeros bb

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

promos :: [Piece]
promos = [Knight, Bishop, Rook, Queen]

-- batched!
pawnsMoves
    :: Maybe Int
    -> Color
    -> Bitboard
    -> Bitboard
    -> Bitboard
    -> DList Move
pawnsMoves enPSq White block myBlock pawnBB = concatDL
    [ enPassant
    , mapOnes (\to -> Move Pawn PawnDouble (to - 16) to) doubles
    , concatDL
        $ promos
        <&> \p -> concatDL
            [ mapOnes (\to -> Move Pawn (Promotion p) (to - 8) to) normalP
            , mapOnes (\to -> Move Pawn (Promotion p) (to - 7) to) leftP
            , mapOnes (\to -> Move Pawn (Promotion p) (to - 9) to) rightP
            ]
    , mapOnes (\to -> Move Pawn Normal (to - 8) to) normalN
    , mapOnes (\to -> Move Pawn Normal (to - 7) to) leftN
    , mapOnes (\to -> Move Pawn Normal (to - 9) to) rightN
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
    enPassant = case enPSq of
        Just enP -> mapOnes (\sq -> Move Pawn (EnPassant enP) sq (enP + 8))
            (pawnBB
                .&. rank5
                .&. 0 `setBit` (enP - 1) `setBit` (enP + 1))
        Nothing -> id
pawnsMoves enPSq Black block myBlock pawnBB = concatDL
    [ enPassant
    , mapOnes (\to -> Move Pawn PawnDouble (to + 16) to) doubles
    , concatDL
        $ promos
        <&> \p -> concatDL
            [ mapOnes (\to -> Move Pawn (Promotion p) (to + 8) to) normalP
            , mapOnes (\to -> Move Pawn (Promotion p) (to + 9) to) leftP
            , mapOnes (\to -> Move Pawn (Promotion p) (to + 7) to) rightP
            ]
    , mapOnes (\to -> Move Pawn Normal (to + 8) to) normalN
    , mapOnes (\to -> Move Pawn Normal (to + 9) to) leftN
    , mapOnes (\to -> Move Pawn Normal (to + 7) to) rightN
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
    enPassant = case enPSq of
        Just enP -> mapOnes (\sq -> Move Pawn (EnPassant enP) sq (enP - 8))
            (pawnBB
                .&. rank4
                .&. 0 `setBit` (enP - 1) `setBit` (enP + 1))
        Nothing -> id
{-# INLINE pawnsMoves #-}

knightTable :: Vector Bitboard
knightTable = tableGen
    [ (-2, -1), (-2, 1)
    , (-1, -2), (-1, 2)
    , ( 1, -2), ( 1, 2)
    , ( 2, -1), ( 2, 1)
    ]

knightMoves :: Bitboard -> Bitboard -> Int -> DList Move
knightMoves _ myBlock sq = mapOnes
    (Move Knight Normal sq)
    (knightTable ! sq .&. complement myBlock)
{-# INLINE knightMoves #-}

bishopMoves :: Bitboard -> Bitboard -> Int -> DList Move
bishopMoves block myBlock sq = mapOnes
    (Move Bishop Normal sq)
    (bishopMovesMagic block sq .&. complement myBlock)
{-# INLINE bishopMoves #-}

rookMoves :: Bitboard -> Bitboard -> Int -> DList Move
rookMoves block myBlock sq = mapOnes
    (Move Rook Normal sq)
    (rookMovesMagic block sq .&. complement myBlock)
{-# INLINE rookMoves #-}

queenMoves :: Bitboard -> Bitboard -> Int -> DList Move
queenMoves block myBlock sq = mapOnes
    (Move Queen Normal sq)
    ((bishopMovesMagic block sq .|. rookMovesMagic block sq)
        .&. complement myBlock)
{-# INLINE queenMoves #-}

kingTable :: Vector Bitboard
kingTable = tableGen
    [ (-1, -1), (-1, 0), (-1, 1)
    , ( 0, -1),          ( 0, 1)
    , ( 1, -1), ( 1, 0), ( 1, 1)
    ]

kingMoves :: Bool -> Bool -> Bitboard -> Bitboard -> Int -> DList Move
kingMoves kAllowed qAllowed block myBlock sq = concatDL
    [ ([Move King CastleKing sq (sq + 2) | castleK] ++)
    , ([Move King CastleQueen sq (sq - 2) | castleQ] ++)
    , mapOnes (Move King Normal sq) (kingTable ! sq .&. complement myBlock)
    ]
  where
    castleK = kAllowed && block .&. (3 !<<. (sq + 1)) == 0
    castleQ = qAllowed && block .&. (7 !<<. (sq - 3)) == 0
{-# INLINE kingMoves #-}
