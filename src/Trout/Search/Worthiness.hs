module Trout.Search.Worthiness
    ( pawnWorth
    , knightWorth
    , bishopWorth
    , rookWorth
    , queenWorth
    , kingWorth
    , lossWorth
    , drawWorth
    , egMaterial
    , mgMaterial
    ) where

-- based on centipawns; maybe go for 256 instead? more common
pawnWorth   :: Int
knightWorth :: Int
bishopWorth :: Int
rookWorth   :: Int
queenWorth  :: Int
kingWorth   :: Int

pawnWorth   = 100
knightWorth = 300
bishopWorth = 325
rookWorth   = 500
queenWorth  = 900
kingWorth   = 100000000

lossWorth :: Int
lossWorth = -kingWorth

drawWorth :: Int
drawWorth = 0

-- estimate of material remaining in the middlegame (EXTREMELY rough)
mgMaterial :: Int
mgMaterial = 16 * pawnWorth
    + 4 * knightWorth
    + 4 * bishopWorth
    + 4 * rookWorth
    + 2 * queenWorth
    - 3 * knightWorth

-- same for endgame
egMaterial :: Int
egMaterial = 8 * pawnWorth
    + 2 * rookWorth
    + 2 * bishopWorth

