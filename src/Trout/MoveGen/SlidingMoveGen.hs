module Trout.MoveGen.SlidingMoveGen
    ( bishopMovesMagic
    , rookMovesMagic
    ) where

import           Data.Foldable
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V
import           Trout.Bitboard
import           Trout.MoveGen.Magics

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

bishopMasks :: Vector Bitboard
bishopMasks = (.&.complement 0x7E7E7E7E7E7E00) . foldl' (.|.) 0
    <$> sequence bishopRays

rookMasks :: Vector Bitboard
rookMasks = foldl' (.|.) 0
    . zipWith (\r e -> r .&. complement e)
        [ 0xFF00000000000000
        , 0x8080808080808080
        , 0x00000000000000FF
        , 0x0101010101010101
        ]
    <$> sequence rookRays

movesClassic :: [Vector Bitboard] -> Int -> Bitboard -> Bitboard
movesClassic rayss sq block = movesDir countLeadingZeros (rayss !! 0)
    .|. movesDir countLeadingZeros (rayss !! 1)
    .|. movesDir countTrailingZeros (rayss !! 2)
    .|. movesDir countTrailingZeros (rayss !! 3)
  where
    movesDir scan rays
        | masked == 0 = rays ! sq
        | otherwise   = (rays ! sq) .&. complement (rays ! scan masked)
        where masked = block .&. (rays ! sq)

mapToMask :: Bitboard -> Int -> Bitboard
mapToMask 0 _ = 0
mapToMask mask idx = mapToMask
    (mask .&. fromIntegral (complement (idx .&. 1)) !<<. countLeadingZeros mask)
    (idx !>>. 1)

-- maps all possible blockers to the mask
magicSquare :: Int -> Bitboard -> Vector Bitboard
magicSquare bits mask = mapToMask mask <$> V.fromList [0..bit bits]

bishopMagicTable :: Vector (Vector Bitboard)
bishopMagicTable = fmap (uncurry $ movesClassic bishopRays) . V.zip (V.fromList [0..63])
    <$> (magicSquare <$> bishopBits <*> bishopMasks)

rookMagicTable :: Vector (Vector Bitboard)
rookMagicTable = fmap (uncurry $ movesClassic rookRays) . V.zip (V.fromList [0..63])
    <$> (magicSquare <$> rookBits <*> rookMasks)

bishopMovesMagic :: Int -> Bitboard -> Bitboard
bishopMovesMagic sq block = (masked * (bishopMagics ! sq)) !>>.
    (64 - (bishopBits ! sq))
  where masked = block .&. (bishopMasks ! sq)

rookMovesMagic :: Int -> Bitboard -> Bitboard
rookMovesMagic sq block = (masked * (rookMagics ! sq)) !>>.
    (64 - (rookBits ! sq))
  where masked = block .&. (rookMasks ! sq)
