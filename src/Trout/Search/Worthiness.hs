module Trout.Search.Worthiness
  ( pawnWorth,
    knightWorth,
    bishopWorth,
    rookWorth,
    queenWorth,
    kingWorth,
    lossWorth,
    drawWorth,
    pieceWorth,
  )
where

import Trout.Piece (PieceType (..))

-- based on centipawns
pawnWorth :: Int
knightWorth :: Int
bishopWorth :: Int
rookWorth :: Int
queenWorth :: Int
kingWorth :: Int
pawnWorth = 100

knightWorth = 300

bishopWorth = 325

rookWorth = 500

queenWorth = 900

kingWorth = 100000000

pieceWorth :: PieceType -> Int
pieceWorth Pawn = pawnWorth
pieceWorth Knight = knightWorth
pieceWorth Bishop = bishopWorth
pieceWorth Rook = rookWorth
pieceWorth Queen = queenWorth
pieceWorth King = kingWorth

lossWorth :: Int
lossWorth = -kingWorth

drawWorth :: Int
drawWorth = 0
