module Trout.Search.Worthiness
    ( pawnWorth
    , knightWorth
    , bishopWorth
    , rookWorth
    , queenWorth
    , kingWorth
    , lossWorth
    , drawWorth
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
