module Trout.MoveGen.Sliding.Classic
    ( bishopRays
    , rookRays
    , slidingMovesClassic
    ) where

import           Data.Vector    (Vector, (!))
import qualified Data.Vector    as V
import           Trout.Bitboard

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
            , let sq = xyToSq (sx `fx` d) (sy `fy` d)
            , inBoard sq
            ]
        | sx <- [0..8]
        , sy <- [0..8]
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
            , let sq = if horz then xyToSq (sx `f` d) sy else xyToSq sx (sy `f` d)
            , inBoard sq
            ]
        | sx <- [0..8]
        , sy <- [0..8]
        ]

slidingMovesClassic :: [Vector Bitboard] -> Int -> Bitboard -> Bitboard
slidingMovesClassic rayss sq block = movesDir countLeadingZeros (rayss !! 0)
    .|. movesDir countLeadingZeros (rayss !! 1)
    .|. movesDir countTrailingZeros (rayss !! 2)
    .|. movesDir countTrailingZeros (rayss !! 3)
  where
    movesDir scan rays
        | masked == 0 = rays ! sq
        | otherwise   = (rays ! sq) .&. complement (rays ! scan masked)
        where masked = block .&. (rays ! sq)
