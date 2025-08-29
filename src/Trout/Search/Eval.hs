module Trout.Search.Eval
  ( totalMaterialScore,
    materialScore,
    virtMobile,
    numPassers,
    eval,
  )
where

import Data.Maybe (fromMaybe)
import Data.Vector.Primitive qualified as PV
import Trout.Bitboard
  ( Bitboard,
    countLeadingZeros,
    countTrailingZeros,
    fileA,
    fileB,
    fileC,
    fileD,
    fileE,
    fileF,
    fileG,
    fileH,
    popCount,
    rank1,
    rank2,
    rank3,
    rank4,
    rank5,
    rank6,
    rank7,
    rank8,
    (.&.),
    (.|.),
  )
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

-- crude and not fully correct but it doesn't really need to be (?)
numPassers :: Color -> Bitboard -> Bitboard -> Int
numPassers color pawns oppPawns =
  sum $
    ( \(col, neighbors) ->
        -- pawn on column exists and
        -- all relevant columns (neighbors and current) are free of opponent pawns
        -- assumes there is only one pawn here
        let pawnSq = sqFinder (pawns .&. col)
            hasPawn = pawnSq >= 0 && pawnSq < 64
            pawnRow = pawnSq `quot` 8
         in if hasPawn && (neighbors .&. activeMasks PV.! pawnRow .&. oppPawns) == 0
              then 1
              else 0
    )
      <$> relevantCols
  where
    mkCols _ [] = error "what?"
    mkCols prev [col] = [(col, col .|. fromMaybe 0 prev)]
    mkCols prev (col : nextCol : cs) =
      (col, col .|. nextCol .|. fromMaybe 0 prev)
        : mkCols (Just col) (nextCol : cs)
    relevantCols = mkCols Nothing [fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH]

    -- masks to only check squares ahead of pawn
    ranks = [rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8]
    genMasks rs =
      PV.fromList $
        snd $
          foldr
            (\r (currMask, ms) -> (currMask .|. r, currMask : ms))
            (0, [])
            rs
    whiteMasks = genMasks ranks
    blackMasks = PV.reverse $ genMasks $ reverse ranks
    activeMasks = case color of
      White -> whiteMasks
      Black -> blackMasks

    sqFinder bb = case color of
      White -> 63 - countLeadingZeros bb
      Black -> countTrailingZeros bb
{-# INLINEABLE numPassers #-}

eval :: Board -> Int
eval board =
  colorSign (boardTurn board)
    * ( pstEvalValue
          + mobilityValue
          + scaledKingSafety
          + scaledPasserDiff
      )
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
              [ (Pawn, 11.205221664378167, 8.757388451627591),
                (Knight, 11.01809959029444, 2.9830169369930584),
                (Bishop, 9.647364620721548, 4.207102199072473),
                (Rook, 5.46628063981836, 7.074045328426514),
                (Queen, 4.69937003753212, 7.451574024272128),
                (King, 8.53008558115183, 6.61159419922287)
              ]
          ]

    safetyMg, safetyEg :: Double
    (safetyMg, safetyEg) = (7.513632511315112, -0.7270409680525919)

    kingSafety = virtMobile Black pieces - virtMobile White pieces
    scaledKingSafety =
      round $
        fromIntegral kingSafety
          * (fromIntegral mgPhase * safetyMg + fromIntegral egPhase * safetyEg)
          / 24

    passerMultMg, passerMultEg :: Double
    (passerMultMg, passerMultEg) = (-0.6854114581591634, 34.681735784817946)
    whitePawns = pieceBitboard (Piece White Pawn) pieces
    blackPawns = pieceBitboard (Piece Black Pawn) pieces
    passerDiff = numPassers White whitePawns blackPawns - numPassers Black blackPawns whitePawns
    scaledPasserDiff =
      round $
        fromIntegral passerDiff
          * (fromIntegral mgPhase * passerMultMg + fromIntegral egPhase * passerMultEg)
          / 24
