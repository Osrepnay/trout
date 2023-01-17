module Trout.Search.Worthiness
    ( pawnWorth
    , knightWorth
    , bishopWorth
    , rookWorth
    , queenWorth
    , kingWorth
    , whiteWonWorth
    , blackWonWorth
    , drawWorth
    ) where

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

whiteWonWorth :: Int
whiteWonWorth = kingWorth

blackWonWorth :: Int
blackWonWorth = -kingWorth

drawWorth :: Int
drawWorth = 0
