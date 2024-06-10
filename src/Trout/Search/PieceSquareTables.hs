module Trout.Search.PieceSquareTables
    ( pawnMPST, pawnEPST
    , knightMPST, knightEPST
    , bishopMPST, bishopEPST
    , rookMPST, rookEPST
    , queenMPST, queenEPST
    , kingMPST, kingEPST
    , pstEval
    ) where

import           Data.Vector.Primitive   (Vector, unsafeIndex)
import qualified Data.Vector.Primitive   as V
import           Trout.Bitboard          (Bitboard, foldSqs, (.^.))
import           Trout.Piece             (Piece (..))
import           Trout.Search.Worthiness
    ( bishopWorth
    , knightWorth
    , pawnWorth
    , queenWorth
    , rookWorth
    )

pawnMPST :: Vector Int
pawnMPST = V.fromList $ fmap (+ pawnWorth) $ concat $ reverse
    [ [   0,   0,   0,   0,   0,   0,   0,   0 ]
    , [  16,  17,  18,  20,  18,  16,  12,  11 ]
    , [   8,  10,  13,  18,  18,  16,  13,  10 ]
    , [   5,   5,   5,   8,   8,   7,   5,   5 ]
    , [   4,   3,   4,   5,   4,   3,   2,   3 ]
    , [   3,   2,   3,   1,   2,   1,   3,   3 ]
    , [   3,   3,   2,   0,   0,   3,   3,   3 ]
    , [   0,   0,   0,   0,   0,   0,   0,   0 ]
    ]

knightMPST :: Vector Int
knightMPST = V.fromList $ fmap (+ knightWorth) $ concat $ reverse
    [ [  -4,   0,   4,   2,   8,   3,   5,  -4 ]
    , [   2,   2,  16,  10,  15,  21,  14,  13 ]
    , [   3,  11,  11,  18,  21,  24,  19,  14 ]
    , [   4,   6,   9,  14,   9,  15,   7,  11 ]
    , [   1,   2,   6,   5,   8,   6,   6,   5 ]
    , [  -3,   0,   4,   3,   4,   4,   3,  -3 ]
    , [  -6,  -4,  -2,   2,   1,  -1,  -5,  -1 ]
    , [ -11,   1,  -6,  -3,  -3,  -2,   0, -11 ]
    ]

bishopMPST :: Vector Int
bishopMPST = V.fromList $ fmap (+ bishopWorth) $ concat $ reverse
    [ [   4,   3,   3,   3,   3,   3,   6,   7 ]
    , [   3,   6,   6,   6,   9,  15,  12,  12 ]
    , [   8,  10,   9,  13,  15,  17,  21,  14 ]
    , [   7,   4,   9,  10,   9,  12,   5,  10 ]
    , [   2,   6,   6,   8,   9,   6,   6,   4 ]
    , [   6,   6,   6,   8,   8,   6,   7,   6 ]
    , [   5,   2,   8,   3,   3,   5,   3,   7 ]
    , [   1,   8,   2,  -1,  -1,   1,   2,  -2 ]
    ]

rookMPST :: Vector Int
rookMPST = V.fromList $ fmap (+ rookWorth) $ concat $ reverse
    [ [   8,   6,   8,   8,   9,   6,   7,  11 ]
    , [   7,   9,  10,  10,  12,  12,  11,  10 ]
    , [   5,   6,   7,   9,  12,  12,  10,  10 ]
    , [   4,   5,   6,   7,   8,   9,   8,  10 ]
    , [   5,   3,   4,   5,   6,   6,   6,   7 ]
    , [   2,   4,   4,   3,   4,   5,   9,   7 ]
    , [  -1,   2,   3,   3,   3,   3,   3,   0 ]
    , [   3,   5,   6,   7,   7,   5,   5,   2 ]
    ]

queenMPST :: Vector Int
queenMPST = V.fromList $ fmap (+ queenWorth) $ concat $ reverse
    [ [   9,   9,  11,  11,  12,  15,  16,  21 ]
    , [   4,   6,   9,  12,  15,  25,  24,  27 ]
    , [   4,   9,  12,  16,  19,  24,  26,  20 ]
    , [   5,   4,   8,  11,  11,  13,  10,  13 ]
    , [   3,   5,   5,   6,   7,   7,  10,   8 ]
    , [   5,   5,   5,   4,   4,   5,   6,   9 ]
    , [   2,   2,   4,   5,   4,   4,   1,   7 ]
    , [   6,  -1,  -1,   2,   0,  -2,  -6,   0 ]
    ]

kingMPST :: Vector Int
kingMPST = V.fromList $ concat $ reverse
    [ [   3,   7,   6,   5,   5,   4,   5,   1 ]
    , [   2,   5,   6,   6,   4,   4,   4,   2 ]
    , [   2,   4,   4,   4,   3,   4,   3,   2 ]
    , [   1,   2,   3,   3,   2,   2,   2,   1 ]
    , [  -1,   1,   1,   1,   1,   1,   0,  -1 ]
    , [  -1,   0,   0,   0,   1,   0,   1,   0 ]
    , [   2,   1,   0,  -1,  -1,   1,   2,   4 ]
    , [   3,   7,   7,  -5,   1,  -3,   6,   5 ]
    ]

pawnEPST :: Vector Int
pawnEPST = V.fromList $ fmap (+ pawnWorth) $ concat $ reverse
    [ [   0,   0,   0,   0,   0,   0,   0,   0 ]
    , [  50,  45,  44,  38,  35,  36,  31,  34 ]
    , [  16,  18,  17,  14,  16,  15,  18,  12 ]
    , [   5,   4,   3,   2,   2,   4,   4,   5 ]
    , [   2,   2,   1,   1,   1,   1,   2,   2 ]
    , [   1,   1,   1,   0,   1,   1,   1,   1 ]
    , [   1,   0,   0,   0,   0,   1,   1,   1 ]
    , [   0,   0,   0,   0,   0,   0,   0,   0 ]
    ]

knightEPST :: Vector Int
knightEPST = V.fromList $ fmap (+ knightWorth) $ concat $ reverse
    [ [  -4,   3,   9,   5,   9,   5,   9,  -5 ]
    , [  -1,   2,   6,   8,  10,   7,   5,   3 ]
    , [   2,   5,   7,   7,   9,  12,   6,   4 ]
    , [   2,   1,   4,   3,   2,   5,   2,   3 ]
    , [   0,   2,   2,   1,   2,   2,   2,   1 ]
    , [   0,   0,   0,   1,   1,   0,   1,   0 ]
    , [  -1,  -1,   0,   0,   0,   0,  -1,   0 ]
    , [  -8,   0,  -3,  -1,  -1,  -1,   0,  -4 ]
    ]

bishopEPST :: Vector Int
bishopEPST = V.fromList $ fmap (+ bishopWorth) $ concat $ reverse
    [ [   4,   7,   9,   8,   8,   6,   9,   4 ]
    , [   2,   6,   6,   8,   7,   6,   7,   3 ]
    , [   4,   7,   7,   7,   8,   9,   8,   3 ]
    , [   4,   1,   7,   5,   4,   6,   1,   4 ]
    , [   1,   4,   1,   4,   4,   1,   3,   1 ]
    , [   2,   1,   2,   1,   1,   2,   1,   2 ]
    , [   1,   1,   1,   1,   0,   1,   1,   1 ]
    , [   0,   1,   0,   0,   0,   0,  -1,   0 ]
    ]

rookEPST :: Vector Int
rookEPST = V.fromList $ fmap (+ rookWorth) $ concat $ reverse
    [ [  11,  11,  12,  11,  11,  10,  12,  12 ]
    , [   8,  10,  11,  12,  12,  13,  13,  11 ]
    , [   5,   7,   8,   9,   9,   9,   9,   6 ]
    , [   4,   4,   5,   5,   5,   5,   5,   4 ]
    , [   3,   3,   3,   4,   3,   3,   4,   3 ]
    , [   1,   2,   2,   2,   2,   2,   3,   2 ]
    , [   0,   1,   1,   2,   1,   1,   1,   1 ]
    , [   0,   1,   2,   2,   2,   1,   2,   0 ]
    ]

queenEPST :: Vector Int
queenEPST = V.fromList $ fmap (+ queenWorth) $ concat $ reverse
    [ [   4,   7,   8,  10,  10,  12,  11,  10 ]
    , [   2,   3,   7,  10,  12,  11,   8,   9 ]
    , [   2,   5,   6,   7,  10,  13,   9,   6 ]
    , [   2,   2,   5,   4,   5,   6,   4,   3 ]
    , [   1,   2,   2,   2,   3,   3,   3,   2 ]
    , [   1,   1,   2,   1,   1,   1,   2,   2 ]
    , [   1,   1,   1,   1,   1,   1,   0,   1 ]
    , [   2,   0,   0,   0,   0,   0,  -2,  -1 ]
    ]

kingEPST :: Vector Int
kingEPST = V.fromList $ concat $ reverse
    [ [   4,  33,  31,  29,  21,  21,  22,   0 ]
    , [   9,  39,  41,  38,  30,  32,  29,  12 ]
    , [  13,  37,  38,  40,  36,  33,  24,  14 ]
    , [   8,  23,  31,  38,  34,  27,  20,   7 ]
    , [  -1,  10,  18,  22,  20,  16,   8,   3 ]
    , [  -3,   1,   5,   4,   5,   4,   3,   1 ]
    , [   1,   1,   1,   1,   1,   2,   3,   3 ]
    , [   1,   3,   2,  -2,   0,  -1,   1,   2 ]
    ]

pstEval :: Bitboard -> Piece -> Int -> Int -> Int -> Int
pstEval bb piece !mgPhase !egPhase !mask = foldSqs
    (\score sqRaw ->
        let sq = sqRaw .^. mask
            m = mg `unsafeIndex` sq
            e = eg `unsafeIndex` sq
        in score + (m * mgPhase + e * egPhase) `quot` 24)
    0
    bb
  where
    (mg, eg) = case piece of
        Pawn   -> (pawnMPST, pawnEPST)
        Knight -> (knightMPST, knightEPST)
        Bishop -> (bishopMPST, bishopEPST)
        Rook   -> (rookMPST, rookEPST)
        Queen  -> (queenMPST, queenEPST)
        King   -> (kingMPST, kingMPST)
{-# INLINE pstEval #-}
