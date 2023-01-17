module Trout.Game.MoveGen.Sliding.Classic
    ( bishopRays
    , rookRays
    , bishopMovesClassic
    , rookMovesClassic
    ) where

import           Data.Vector.Primitive (Vector, (!))
import qualified Data.Vector.Primitive as V
import           Trout.Bitboard
    ( Bitboard
    , complement
    , countLeadingZeros
    , countTrailingZeros
    , fromSqs
    , xyToSq
    , (.&.)
    , (.|.)
    )

xyInBoard :: (Int, Int) -> Bool
xyInBoard (x, y) = 0 <= x && 0 <= y && x < 8 && y < 8

-- NW, NE, SE, SW
-- lists are probably fine, it's only 4 elements and only used in table init
bishopRays :: [Vector Bitboard]
bishopRays =
    [ genRay (-) (+)
    , genRay (+) (+)
    , genRay (+) (-)
    , genRay (-) (-)
    ]
  where
    genRay fx fy = V.fromList
        [ fromSqs
            [ sq
            | d <- [1..7]
            , let dc = (sx `fx` d, sy `fy` d)
            , xyInBoard dc
            , let sq = uncurry xyToSq dc
            ]
        | sy <- [0..7]
        , sx <- [0..7]
        ]

-- N E S W
rookRays :: [Vector Bitboard]
rookRays =
    [ genRay False (+)
    , genRay True  (+)
    , genRay False (-)
    , genRay True  (-)
    ]
  where
    genRay horz f = V.fromList
        [ fromSqs
            [ sq
            | d <- [1..7]
            , let dc = if horz then (sx `f` d, sy) else (sx, sy `f` d)
            , xyInBoard dc
            , let sq = uncurry xyToSq dc
            ]
        | sy <- [0..7]
        , sx <- [0..7]
        ]

slidingMovesClassic :: [Vector Bitboard] -> Bitboard -> Int -> Bitboard
slidingMovesClassic rayss block sq = movesDir countTrailingZeros (rayss !! 0)
    .|. movesDir countTrailingZeros (rayss !! 1)
    .|. movesDir ((63 -) . countLeadingZeros) (rayss !! 2)
    .|. movesDir ((63 -) . countLeadingZeros) (rayss !! 3)
  where
    movesDir scan rays
        | masked == 0 = rays ! sq
        | otherwise   = (rays ! sq) .&. complement (rays ! scan masked)
      where masked = block .&. (rays ! sq)

bishopMovesClassic :: Bitboard -> Int -> Bitboard
bishopMovesClassic = slidingMovesClassic bishopRays

rookMovesClassic :: Bitboard -> Int -> Bitboard
rookMovesClassic = slidingMovesClassic rookRays
