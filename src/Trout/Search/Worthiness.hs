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
  )
where

import Data.Vector.Primitive qualified as PV
import Trout.Piece (PieceType (..))
import Trout.Search.PieceSquareTables (bishopMPST, knightMPST, pawnMPST, queenMPST, rookMPST)

-- based on centipawns
pawnWorth :: Int
knightWorth :: Int
bishopWorth :: Int
rookWorth :: Int
queenWorth :: Int
kingWorth :: Int
-- mind the first and last row being 0
pawnWorth = PV.sum pawnMPST `quot` 48

knightWorth = PV.sum knightMPST `quot` 64

bishopWorth = PV.sum bishopMPST `quot` 64

rookWorth = PV.sum rookMPST `quot` 64

queenWorth = PV.sum queenMPST `quot` 64

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
