module Trout.PieceSquareTables (egMaterial, mgMaterial, pstEvalBitboard) where

import           Data.Foldable           (foldl')
import           Data.Vector             (Vector, (!))
import qualified Data.Vector             as V
import           Trout.Bitboard          (Bitboard, toSqs)
import           Trout.Piece             (Piece (..))
import           Trout.Search.Worthiness
    ( bishopWorth
    , knightWorth
    , pawnWorth
    , queenWorth
    , rookWorth
    )

pawnMPST :: Vector Int
pawnMPST = V.fromList
    [   0,   0,   0,   0,   0,   0,   0,   0
    ,   5,   5,   6,   0,   0,   6,   5,   5
    ,   4,   4,   3,   0,   0,   3,   4,   4
    ,  -3,  -5,  -3,  10,  10,  -8,  -6,  -3
    ,   3,  -3,   8,  17,  17,   6,  -4,  -2
    ,   3,   4,   2,   8,   8,  -3,   5,   3
    ,   6,   7,   2, -10, -10,  10,  -2,   5
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

knightMPST :: Vector Int
knightMPST = V.fromList
    [  -7,  -7,  -7,  -7,  -7,  -7,  -7,  -7
    ,  -7,  -2,   6,   0,   0,   7,  -2,  -7
    ,  -7,  -3,  -3,  -3,  -3,  -3,  -3,  -7
    ,  -7,   3,   0,   8,   8,   0,   3,  -7
    ,  -7,   0,   0,   2,   2,   0,   0,  -7
    ,  -7,   5,  10,   3,   3,  10,   5,  -7
    ,  -7,   1,   0,   6,   6,   0,   1,  -7
    ,  -7,  -7,  -7,  -7,  -7,  -7,  -7,  -7
    ]

bishopMPST :: Vector Int
bishopMPST = V.fromList
    [   3,  -4,  -3,   0,  -1,  -3,  -4,   3 -- rook :D
    ,  -2,   0,   1,   0,   0,   1,   0,  -2
    ,   2,  -3,   2,  -2,  -2,   2,  -3,   2
    ,   4,   9,   0,  -2,  -2,   0,  10,   4
    ,   5,   4,  10,   1,   1,  15,   4,   6
    ,   0,   1,   3,   6,   6,   3,   1,   0
    ,   1,   7,   2,   4,   4,   2,   8,   1
    ,  -5,  -5,  -2,  -5,  -5,  -2,  -5,  -5
    ]

rookMPST :: Vector Int
rookMPST = V.fromList
    [   5,   5,   5,   5,   5,   5,   5,   5
    ,   3,   3,   3,   3,   3,   3,   3,   3
    ,   0,   0,   0,   0,   0,   0,   0,   0 -- lazy
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,  -1,  -1,  -1,  -1,  -1,   0,  -2,  -2
    ,   0,  -1,   0,   1,   1,   0,  -2,  -1
    ,   1,   1,   1,   2,   2,   1,   1,  -5
    ,   0,   3,   4,  10,  10,   8,   3,   0
    ]

queenMPST :: Vector Int
queenMPST = V.fromList
    [   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   2,   0,   0,   0,   1,   2,   1
    ,   0,   0,   0,   0,   0,   0,   0,   0 -- hhhrnrngnn
    ,   0,   0,   0,   1,   1,   0,   0,   1
    ,   1,   0,   0,   2,   2,   0,   1,   0
    ,   0,   1,   0,   3,   3,   1,   0,   0
    ,   0,   0,   1,   3,   3,   0,   0,   0
    ,   1,   0,   0,   3,   3,   1,   1,   1
    ]

kingMPST :: Vector Int
kingMPST = V.fromList
    [ -20, -20, -20, -20, -20, -20, -20, -20
    , -20, -20, -20, -20, -20, -20, -20, -20
    , -20, -20, -20, -20, -20, -20, -20, -20
    , -20, -20, -20, -20, -20, -20, -20, -20
    , -20, -20, -20, -20, -20, -20, -20, -20
    , -10, -10, -10, -10, -10, -10, -10, -10
    ,  -6,  -6,  -6,  -9,  -9,  -6,  -6,  -6
    ,   3,   7,  15,  -3,   0,   5,  20,  10
    ]

pawnEPST :: Vector Int
pawnEPST = V.fromList
    [   0,   0,   0,   0,   0,   0,   0,   0
    ,  33,  33,  30,  30,  30,  30,  33,  33
    ,  28,  28,  25,  25,  25,  25,  28,  28
    ,  18,  18,  15,  15,  15,  15,  18,  18
    ,   3,   3,   0,   0,   0,   0,   3,   3
    ,  -8,  -8,  -5,  -5,  -5,  -5,  -8,  -8
    , -13, -13, -10, -10, -10, -10, -13, -13
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

knightEPST :: Vector Int
knightEPST = V.fromList -- meh
    [   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   3,   3,   0,   0,   0
    ,   0,   0,   3,   5,   5,   3,   0,   0
    ,   0,   0,   3,   5,   5,   3,   0,   0
    ,   0,   0,   0,   3,   3,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

bishopEPST :: Vector Int
bishopEPST = V.fromList
    [   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

rookEPST :: Vector Int
rookEPST = V.fromList
    [   5,   5,   5,   5,   5,   5,   5,   5
    ,   3,   3,   3,   3,   3,   3,   3,   3
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

queenEPST :: Vector Int
queenEPST = V.fromList
    [   5,   5,   5,   5,   5,   5,   5,   5
    ,   3,   3,   3,   3,   3,   3,   3,   3
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ,   0,   0,   0,   0,   0,   0,   0,   0
    ]

kingEPST :: Vector Int
kingEPST = V.fromList
    [   3,   5,   5,   8,   8,   5,   5,   3
    ,   3,   5,   5,   8,   8,   5,   5,   3
    ,   3,   5,   5,   9,   9,   5,   5,   3
    ,   3,   7,   7,  10,  10,   7,   7,   3
    ,   3,   7,   7,  10,  10,   7,   7,   3
    ,   2,   3,   4,   4,   4,   4,   3,   2
    ,  -8,  -8,  -4,  -4,  -4,  -4,  -8,  -8
    , -15, -13, -10,  -8,  -8, -10, -13, -15
    ]

mgMaterial :: Int
mgMaterial = 16 * pawnWorth
    + 4 * knightWorth
    + 4 * bishopWorth
    + 4 * rookWorth
    + 2 * queenWorth
    - 3 * knightWorth

egMaterial :: Int
egMaterial = 8 * pawnWorth
    + 2 * rookWorth
    + 2 * bishopWorth

pstEvalBitboard :: Int -> Piece -> Bitboard -> Int
pstEvalBitboard material piece bb = foldl'
    (\b a -> b + blend (pstM ! a) (pstE ! a))
    0
    (toSqs bb)
  where
    blend m e = round $ mFac * fromIntegral m + (1 - mFac) * fromIntegral e
      where
        mFac :: Double
        mFac = zeroOneClamp
            $ negate
            $ fromIntegral (material - egMaterial)
            / fromIntegral (egMaterial - mgMaterial)
    zeroOneClamp x
        | x < 0     = 0
        | x > 1     = 1
        | otherwise = x
    (pstM, pstE) = case piece of
        Pawn   -> (pawnMPST, pawnEPST)
        Knight -> (knightMPST, knightEPST)
        Bishop -> (bishopMPST, bishopEPST)
        Rook   -> (rookMPST, rookEPST)
        Queen  -> (queenMPST, queenEPST)
        King   -> (kingMPST, kingEPST)
