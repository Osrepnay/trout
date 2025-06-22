module Trout.Game
  ( Pieces,
    emptyPieces,
    addPiece,
    removePiece,
    getPiece,
    pieceTypeBitboard,
    pieceBitboard,
    occupancy,
    colorOccupancy,
    Castling (..),
    clearColorRights,
    clearRights,
    canCastle,
    Game (..),
    mkGame,
    startingGame,
    allMoves,
    inCheck,
    makeMove,
  )
where

import Data.Bits (Bits)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.Hashable (Hashable (..))
import Data.Vector.Primitive (unsafeIndex)
import Trout.Bitboard
  ( Bitboard,
    clearBit,
    complement,
    countTrailingZeros,
    fileA,
    fileH,
    testBit,
    toSqs,
    (!<<.),
    (!>>.),
    (.&.),
    (.^.),
    (.|.),
  )
import Trout.Game.Move (Move (..), SpecialMove (..))
import Trout.Game.MoveGen
  ( bishopMoves,
    concatDL,
    kingMoves,
    kingTable,
    knightMoves,
    knightTable,
    mapOnes,
    pawnsMoves,
    queenMoves,
    rookMoves,
  )
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Game.Zobrists
  ( castleZobrists,
    enPassantZobrists,
    pieceZobrists,
    playingZobrist,
  )
import Trout.Piece
  ( Color (..),
    Piece (..),
    PieceType (..),
    other,
  )

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

data Game = Game
  { gamePieces :: !Pieces,
    gameCastling :: !Castling,
    gameEnPassant :: !(Maybe Int),
    gameTurn :: !Color,
    gameHash :: !Int
  }
  deriving (Eq, Show)

instance Hashable Game where
  hash = gameHash
  hashWithSalt salt game = hash game .^. salt

mkGame :: Pieces -> Castling -> Maybe Int -> Color -> Game
mkGame pieces castling enPassant turn =
  Game pieces castling enPassant turn $
    (if turn == White then playingZobrist else 0)
      .^. (castleZobrists `unsafeIndex` unCastling castling)
      .^. maybe 0 (\sq -> enPassantZobrists `unsafeIndex` (sq `rem` 8)) enPassant
      .^. foldl'
        (.^.)
        0
        [ hashBitboard (pieceBitboard (Piece color pieceType) pieces) (pieceZobrists color pieceType)
        | color <- [White, Black],
          pieceType <- [Pawn, Knight, Bishop, Rook, Queen, King]
        ]
  where
    hashBitboard bb table = foldl' (\b a -> table `unsafeIndex` a .^. b) 0 (toSqs bb)

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame =
  mkGame
    ( Pieces
        0xFFFF
        0x2CFF00000000FF2C
        0x7600000000000076
        0x9900000000000099
    )
    (Castling 15)
    Nothing
    White

allMoves :: Game -> [Move]
allMoves (Game pieces castling enPassant turn _) =
  pawnsMoves enPassant turn block myBlock (pieceBB Pawn) $
    concatDL
      ( moveSqs knightMoves (pieceBB Knight) $
          moveSqs bishopMoves (pieceBB Bishop) $
            moveSqs rookMoves (pieceBB Rook) $
              moveSqs queenMoves (pieceBB Queen) $
                moveSqs (kingMoves (canCastle turn True castling) (canCastle turn False castling)) (pieceBB King) []
      )
      []
  where
    block = occupancy pieces
    myBlock = colorOccupancy turn pieces
    pieceBB = ($ pieces) . pieceBitboard . Piece turn
    moveSqs mover = mapOnes (mover block myBlock)

inCheck :: Color -> Pieces -> Bool
inCheck color pieces =
  (bishopAttackers /= 0)
    || (rookAttackers /= 0)
    || (queenAttackers /= 0)
    || (knightAttackers /= 0)
    || (pawnAttackers /= 0)
    || (kingAttackers /= 0)
  where
    oppColor = other color
    king = pieceBitboard (Piece color King) pieces
    kingSq = countTrailingZeros king
    getOppPieces = ($ pieces) . pieceBitboard . Piece oppColor

    pawnAttackers =
      getOppPieces Pawn
        .&. case oppColor of
          White ->
            (king .&. complement fileA)
              !>>. 9
              .|. (king .&. complement fileH)
              !>>. 7
          Black ->
            (king .&. complement fileA)
              !<<. 7
              .|. (king .&. complement fileH)
              !<<. 9
    knightAttackers = knightTable `unsafeIndex` kingSq .&. getOppPieces Knight
    blockers = occupancy pieces
    bishopSqs = bishopMovesMagic blockers kingSq
    bishopAttackers = bishopSqs .&. getOppPieces Bishop
    rookSqs = rookMovesMagic blockers kingSq
    rookAttackers = rookSqs .&. getOppPieces Rook
    queenAttackers = (bishopSqs .|. rookSqs) .&. getOppPieces Queen
    kingAttackers = kingTable `unsafeIndex` kingSq .&. getOppPieces King

makeMove :: Game -> Move -> Maybe Game
makeMove (Game pieces castling enPassant turn hashed) (Move pieceType special from to) =
  case special of
    Normal -> checkInCheck $ Game movedPieces movedCastling Nothing opp movedHash
    PawnDouble ->
      checkInCheck $
        Game
          movedPieces
          movedCastling
          (Just to)
          opp
          (movedHash .^. enPassantZobrists `unsafeIndex` (to `rem` 8))
    CastleKing -> do
      toMaybe (inCheck turn pieces)
      toMaybe (inCheck turn movedPieces)
      let oneRight = addPiece (Piece turn King) (from + 1) $ removePiece from pieces
      toMaybe (inCheck turn oneRight)
      let rookMoved = addPiece (Piece turn Rook) (to - 1) $ removePiece (from + 3) movedPieces
      checkInCheck
        ( Game
            rookMoved
            movedCastling
            Nothing
            opp
            ( movedHash
                .^. pieceZobrists turn Rook
                `unsafeIndex` (from + 3)
                .^. pieceZobrists turn Rook
                `unsafeIndex` (to - 1)
            )
        )
    CastleQueen -> do
      toMaybe (inCheck turn pieces)
      toMaybe (inCheck turn movedPieces)
      let oneLeft = addPiece (Piece turn King) (from - 1) $ removePiece from pieces
      toMaybe (inCheck turn oneLeft)
      let rookMoved = addPiece (Piece turn Rook) (to + 1) $ removePiece (from - 4) movedPieces
      checkInCheck
        ( Game
            rookMoved
            movedCastling
            Nothing
            opp
            ( movedHash
                .^. pieceZobrists turn Rook
                `unsafeIndex` (from - 4)
                .^. pieceZobrists turn Rook
                `unsafeIndex` (to + 1)
            )
        )
    EnPassant sq ->
      checkInCheck $
        Game
          (removePiece sq movedPieces)
          movedCastling
          Nothing
          opp
          (movedHash .^. pieceZobrists opp Pawn `unsafeIndex` sq)
    Promotion promote ->
      checkInCheck $
        Game
          (addPiece (Piece turn promote) to $ removePiece from pieces)
          movedCastling
          Nothing
          opp
          ( movedHash
              .^. pieceZobrists turn Pawn
              `unsafeIndex` to
              .^. pieceZobrists turn promote
              `unsafeIndex` to
          )
  where
    checkInCheck game = if inCheck turn (gamePieces game) then Nothing else Just game
    toMaybe = bool (Just ()) Nothing
    movedPieces = addPiece (Piece turn pieceType) to $ removePiece from pieces

    clearCorners White 0 = clearRights White False
    clearCorners White 7 = clearRights White True
    clearCorners Black 56 = clearRights Black False
    clearCorners Black 63 = clearRights Black True
    clearCorners _ _ = id

    movedCastling = clearCorners opp to $
      case pieceType of
        King -> clearColorRights turn castling
        Rook -> clearCorners turn from castling
        _ -> castling

    movedHash =
      hashed
        .^. (pieceZobrists turn pieceType `unsafeIndex` from)
        .^. (pieceZobrists turn pieceType `unsafeIndex` to)
        .^. (castleZobrists `unsafeIndex` unCastling movedCastling)
        .^. (castleZobrists `unsafeIndex` unCastling castling)
        .^. maybe 0 (\(Piece c p) -> pieceZobrists c p `unsafeIndex` to) capturePiece
        .^. maybe 0 (pieceZobrists turn Pawn `unsafeIndex`) enPassant

    capturePiece = getPiece to pieces
    opp = other turn
