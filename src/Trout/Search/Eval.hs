module Trout.Search.Eval (totalMaterialScore, materialScore, virtMobile, eval) where

import Trout.Bitboard (countTrailingZeros, popCount, (.|.))
import Trout.Game (Game (..), mobility)
import Trout.Game.Board (Board (..), Pieces, colorOccupancy, pieceBitboard, pieceTypeBitboard)
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search.PieceSquareTables (pstEval)

-- 24: all pieces, 0: none
-- pawns don't count, bishops and knights count 1, rooks 2, queens 4
-- taken from pesto/ethereal/fruit
totalMaterialScore :: Board -> Int
totalMaterialScore board =
  popCount (pieceTypeBitboard Knight pieces .|. pieceTypeBitboard Bishop pieces)
    + 2 * popCount (pieceTypeBitboard Rook pieces)
    + 4 * popCount (pieceTypeBitboard Queen pieces)
  where
    pieces = boardPieces board
{-# INLINEABLE totalMaterialScore #-}

-- for current side
materialScore :: Game -> Int
materialScore game =
  popCount (getBB Knight .|. getBB Bishop)
    + 2 * popCount (getBB Rook)
    + 4 * popCount (getBB Queen)
  where
    getBB p = pieceBitboard (Piece turn p) pieces
    board = gameBoard game
    pieces = boardPieces board
    turn = boardTurn board
{-# INLINEABLE materialScore #-}

virtMobile :: Color -> Pieces -> Int
virtMobile color pieces = popCount movez
  where
    block = colorOccupancy color pieces
    king = pieceBitboard (Piece color King) pieces
    kingSq = countTrailingZeros king
    movez = bishopMovesMagic block kingSq .|. rookMovesMagic block kingSq
{-# INLINEABLE virtMobile #-}

eval :: Board -> Int
eval board = colorSign (boardTurn board) * (pstEvalValue + mobilityValue + scaledKingSafety)
  where
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    mgPhase = totalMaterialScore board
    egPhase = 24 - mgPhase
    pst bb p = pstEval bb p mgPhase egPhase
    pstEvalValue =
      pst (getBB White Pawn) Pawn 0
        - pst (getBB Black Pawn) Pawn 56
        + pst (getBB White Knight) Knight 0
        - pst (getBB Black Knight) Knight 56
        + pst (getBB White Bishop) Bishop 0
        - pst (getBB Black Bishop) Bishop 56
        + pst (getBB White Rook) Rook 0
        - pst (getBB Black Rook) Rook 56
        + pst (getBB White Queen) Queen 0
        - pst (getBB Black Queen) Queen 56
        + pst (getBB White King) King 0
        - pst (getBB Black King) King 56

    mobilityValue =
      round $
        sum
          [ (mgMult * fromIntegral mgPhase + egMult * fromIntegral egPhase)
              * fromIntegral (colorSign c)
              * fromIntegral (mobility board (Piece c p))
              / 24
          | c <- [White, Black],
            (p, mgMult :: Double, egMult) <-
              [ (Pawn, 8.833997074443921, 12.556370043532233),
                (Knight, 10.833140455990184, 0.7540671801689474),
                (Bishop, 9.694322211223236, 1.9841962477184265),
                (Rook, 5.8999106161593655, 5.3280937143442015),
                (Queen, 5.366607226301831, 6.444660325419429),
                (King, 4.959262051920608, 6.946238277144365)
              ]
          ]

    safetyMg, safetyEg :: Double
    (safetyMg, safetyEg) = (6.205675047110871, -0.4122395550691301)
    kingSafety = virtMobile Black pieces - virtMobile White pieces
    scaledKingSafety =
      round $
        fromIntegral kingSafety
          * (fromIntegral mgPhase * safetyMg + fromIntegral egPhase * safetyEg)
          / 24
