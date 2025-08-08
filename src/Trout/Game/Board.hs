module Trout.Game.Board
  ( Pieces (..),
    emptyPieces,
    addPiece,
    removePiece,
    getPiece,
    pieceTypeBitboard,
    pieceBitboard,
    occupancy,
    colorOccupancy,
    piecesBoard,
    Castling (..),
    clearColorRights,
    clearRights,
    canCastle,
    Board (..),
  )
where

import Data.Bits (Bits, clearBit, complement, testBit, (!<<.), (!>>.), (.&.), (.^.), (.|.))
import Data.Hashable (Hashable (..))
import Data.List (intersperse)
import Trout.Bitboard (Bitboard)
import Trout.Piece (Color (..), Piece (..), PieceType (..))

data Pieces = Pieces
  { piecesWhite :: !Bitboard,
    piecesPiece0 :: !Bitboard,
    piecesPiece1 :: !Bitboard,
    piecesPiece2 :: !Bitboard
  }
  deriving (Eq, Show)

emptyPieces :: Pieces
emptyPieces = Pieces 0 0 0 0

addPiece :: Piece -> Int -> Pieces -> Pieces
addPiece (Piece c p) sq (Pieces pw p0 p1 p2) =
  Pieces
    (branchlessSet (fromEnum c .^. 1) pw)
    (branchlessSet (pieceInt .&. 1) p0)
    (branchlessSet (pieceInt !>>. 1 .&. 1) p1)
    (branchlessSet (pieceInt !>>. 2 .&. 1) p2)
  where
    sqBB = 1 !<<. sq
    -- https://graphics.stanford.edu/~seander/bithacks.html#ConditionalSetOrClearBitsWithoutBranching
    branchlessSet clear bb = (fromIntegral (-clear) .^. bb) .&. sqBB .^. bb
    pieceInt = fromEnum p + 1

removePiece :: Int -> Pieces -> Pieces
removePiece sq (Pieces pw p0 p1 p2) = Pieces (pw .&. unSqBB) (p0 .&. unSqBB) (p1 .&. unSqBB) (p2 .&. unSqBB)
  where
    unSqBB = complement (1 !<<. sq)

getPiece :: Int -> Pieces -> Maybe Piece
getPiece sq (Pieces pw p0 p1 p2)
  | testBit (pw .|. p0 .|. p1 .|. p2) sq =
      Just $
        Piece
          (toEnum (fromIntegral (pw !>>. sq .&. 1 .^. 1)))
          ( toEnum
              ( fromIntegral
                  ( p0 !>>. sq .&. 1
                      + p1 !>>. sq .&. 1 * 2
                      + p2 !>>. sq .&. 1 * 4
                  )
                  - 1
              )
          )
  | otherwise = Nothing

pieceTypeBitboard :: PieceType -> Pieces -> Bitboard
pieceTypeBitboard pieceType (Pieces _pw p0 p1 p2) =
  flipIfNotSet (pieceInt .&. 1) p0
    .&. flipIfNotSet (pieceInt !>>. 1 .&. 1) p1
    .&. flipIfNotSet (pieceInt !>>. 2 .&. 1) p2
  where
    flipIfNotSet bit = ((fromIntegral bit - 1) .^.)
    pieceInt = fromEnum pieceType + 1

pieceBitboard :: Piece -> Pieces -> Bitboard
pieceBitboard (Piece c pieceType) pieces =
  pieceTypeBitboard pieceType pieces
    .&. (piecesWhite pieces .^. fromIntegral (negate (fromEnum c)))

occupancy :: Pieces -> Bitboard
occupancy (Pieces _pw p0 p1 p2) = p0 .|. p1 .|. p2

colorOccupancy :: Color -> Pieces -> Bitboard
colorOccupancy White (Pieces pw p0 p1 p2) = pw .&. (p0 .|. p1 .|. p2)
colorOccupancy Black (Pieces pw p0 p1 p2) = complement pw .&. (p0 .|. p1 .|. p2)

piecesBoard :: Pieces -> String
piecesBoard pcs =
  concat
    [ intersperse
        ' '
        [ case getPiece sq pcs of
            Nothing -> '.'
            Just (Piece White Pawn) -> 'P'
            Just (Piece White Knight) -> 'N'
            Just (Piece White Bishop) -> 'B'
            Just (Piece White Rook) -> 'R'
            Just (Piece White Queen) -> 'Q'
            Just (Piece White King) -> 'K'
            Just (Piece Black Pawn) -> 'p'
            Just (Piece Black Knight) -> 'n'
            Just (Piece Black Bishop) -> 'b'
            Just (Piece Black Rook) -> 'r'
            Just (Piece Black Queen) -> 'q'
            Just (Piece Black King) -> 'k'
        | c <- [0 .. 7],
          let sq = r * 8 + c
        ]
        ++ "\n"
    | r <- [7, 6 .. 0]
    ]

newtype Castling = Castling {unCastling :: Int} deriving (Bits, Eq, Show)

clearColorRights :: Color -> Castling -> Castling
clearColorRights White (Castling castle) = Castling (castle .&. 12)
clearColorRights Black (Castling castle) = Castling (castle .&. 3)

clearRights :: Color -> Bool -> Castling -> Castling
clearRights color kingside (Castling castle) =
  Castling $
    castle
      `clearBit` ( case color of
                     White -> if kingside then 1 else 0
                     Black -> if kingside then 3 else 2
                 )

-- TODO check if this is slow
canCastle :: Color -> Bool -> Castling -> Bool
canCastle color kingside castling = clearRights color kingside castling /= castling

data Board = Board
  { boardPieces :: !Pieces,
    boardCastling :: !Castling,
    boardEnPassant :: !(Maybe Int),
    boardTurn :: !Color,
    boardHash :: !Int
  }
  deriving (Eq, Show)

instance Hashable Board where
  hash = boardHash
  hashWithSalt salt board = hash board .^. salt
