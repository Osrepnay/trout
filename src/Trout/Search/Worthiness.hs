module Trout.Search.Worthiness
  ( pawnWorth,
    knightWorth,
    bishopWorth,
    rookWorth,
    queenWorth,
    kingWorth,
    lossWorth,
    winWorth,
    drawWorth,
    pieceWorth,
    scoreIsMate,
    scoreIsLosing,
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
-- mind the first and last row being 0
pawnWorth = 100

knightWorth = 435

bishopWorth = 487

rookWorth = 703

queenWorth = 1360

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

winWorth :: Int
winWorth = kingWorth

drawWorth :: Int
drawWorth = 0

-- whether score is winworth/lossworth with allowances for the halfmove added
scoreIsMate :: Int -> Bool
scoreIsMate score = abs (abs score - winWorth) < 100000

scoreIsLosing :: Int -> Bool
scoreIsLosing score = score < -100000
