module Trout.Game
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
    Game (..),
    hashBoard,
    mkBoard,
    startingGame,
    isDrawn,
    allMoves,
    allCaptures,
    inCheck,
    makeMove,
  )
where

import Data.Bits (Bits)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (..))
import Data.Maybe (isJust)
import Data.Vector.Primitive ((!))
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
  ( bishopCaptures,
    bishopMoves,
    concatDL,
    kingCaptures,
    kingMoves,
    kingTable,
    knightCaptures,
    knightMoves,
    knightTable,
    mapOnes,
    pawnsCaptures,
    pawnsMoves,
    queenCaptures,
    queenMoves,
    rookCaptures,
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
    [ [ case getPiece sq pcs of
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

data Game = Game
  { gameHalfmove :: !Int,
    game50MovePlies :: !Int, -- halfmoves since last capture or pawn move
    gameHistory :: !(HashMap Board Int),
    gameBoard :: !Board
  }
  deriving (Eq, Show)

-- actually hash game instead of using cached value
hashBoard :: Board -> Int
hashBoard (Board pieces castling enPassant turn _) =
  (if turn == White then playingZobrist else 0)
    .^. (castleZobrists ! unCastling castling)
    .^. maybe 0 (\sq -> enPassantZobrists ! (sq `rem` 8)) enPassant
    .^. foldl'
      (.^.)
      0
      [ hashBitboard (pieceBitboard (Piece color pieceType) pieces) (pieceZobrists color pieceType)
      | color <- [White, Black],
        pieceType <- [Pawn, Knight, Bishop, Rook, Queen, King]
      ]
  where
    hashBitboard bb table = foldl' (\b a -> table ! a .^. b) 0 (toSqs bb)

mkBoard :: Pieces -> Castling -> Maybe Int -> Color -> Board
mkBoard pieces castling enPassant turn = Board pieces castling enPassant turn (hashBoard board)
  where
    board = Board pieces castling enPassant turn 0

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame =
  Game
    1
    0
    HM.empty
    ( mkBoard
        ( Pieces
            0xFFFF
            0x2CFF00000000FF2C
            0x7600000000000076
            0x9900000000000099
        )
        (Castling 15)
        Nothing
        White
    )

-- non-stalemate draws
isDrawn :: Game -> Bool
isDrawn (Game {game50MovePlies = plies, gameHistory = history, gameBoard = board}) =
  plies >= 50 || maybe False (>= 1) (HM.lookup board history)

allMoves :: Board -> [Move]
allMoves (Board pieces castling enPassant turn _) =
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

allCaptures :: Board -> [Move]
allCaptures (Board pieces _ enPassant turn _) =
  pawnsCaptures enPassant turn block myBlock (pieceBB Pawn) $
    concatDL
      ( moveSqs knightCaptures (pieceBB Knight) $
          moveSqs bishopCaptures (pieceBB Bishop) $
            moveSqs rookCaptures (pieceBB Rook) $
              moveSqs queenCaptures (pieceBB Queen) $
                moveSqs kingCaptures (pieceBB King) []
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
    knightAttackers = knightTable ! kingSq .&. getOppPieces Knight
    blockers = occupancy pieces
    bishopSqs = bishopMovesMagic blockers kingSq
    bishopAttackers = bishopSqs .&. getOppPieces Bishop
    rookSqs = rookMovesMagic blockers kingSq
    rookAttackers = rookSqs .&. getOppPieces Rook
    queenAttackers = (bishopSqs .|. rookSqs) .&. getOppPieces Queen
    kingAttackers = kingTable ! kingSq .&. getOppPieces King

makeMove :: Game -> Move -> Maybe Game
makeMove
  (Game halfmove fiftyPlies history origBoard@(Board pieces castling enPassant turn hashed))
  (Move pieceType special from to) =
    case special of
      Normal ->
        mkMovedGame
          ( Board
              movedPieces
              movedCastling
              Nothing
              opp
              movedHash
          )
      PawnDouble ->
        mkMovedGame
          ( Board
              movedPieces
              movedCastling
              (Just to)
              opp
              (movedHash .^. enPassantZobrists ! (to `rem` 8))
          )
      CastleKing -> do
        toMaybe (inCheck turn pieces)
        toMaybe (inCheck turn movedPieces)
        let oneRight = addPiece (Piece turn King) (from + 1) $ removePiece from pieces
        toMaybe (inCheck turn oneRight)
        let rookMoved = addPiece (Piece turn Rook) (to - 1) $ removePiece (from + 3) movedPieces
        mkMovedGame
          ( Board
              rookMoved
              movedCastling
              Nothing
              opp
              ( movedHash
                  .^. (pieceZobrists turn Rook ! (from + 3))
                  .^. (pieceZobrists turn Rook ! (to - 1))
              )
          )
      CastleQueen -> do
        toMaybe (inCheck turn pieces)
        toMaybe (inCheck turn movedPieces)
        let oneLeft = addPiece (Piece turn King) (from - 1) $ removePiece from pieces
        toMaybe (inCheck turn oneLeft)
        let rookMoved = addPiece (Piece turn Rook) (to + 1) $ removePiece (from - 4) movedPieces
        mkMovedGame
          ( Board
              rookMoved
              movedCastling
              Nothing
              opp
              ( movedHash
                  .^. (pieceZobrists turn Rook ! (from - 4))
                  .^. (pieceZobrists turn Rook ! (to + 1))
              )
          )
      EnPassant sq ->
        mkMovedGame
          ( Board
              (removePiece sq movedPieces)
              movedCastling
              Nothing
              opp
              (movedHash .^. pieceZobrists opp Pawn ! sq)
          )
      Promotion promote ->
        mkMovedGame
          ( Board
              (addPiece (Piece turn promote) to $ removePiece from pieces)
              movedCastling
              Nothing
              opp
              ( movedHash
                  .^. (pieceZobrists turn Pawn ! to)
                  .^. (pieceZobrists turn promote ! to)
              )
          )
    where
      toMaybe = bool (Just ()) Nothing

      opp = other turn
      capturePiece = getPiece to pieces

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
          .^. (pieceZobrists turn pieceType ! from)
          .^. (pieceZobrists turn pieceType ! to)
          .^. (castleZobrists ! unCastling movedCastling)
          .^. (castleZobrists ! unCastling castling)
          .^. maybe 0 (\(Piece c p) -> pieceZobrists c p ! to) capturePiece
          .^. maybe 0 ((enPassantZobrists !) . (`rem` 8)) enPassant
          .^. playingZobrist

      movedFiftyPlies =
        if isJust capturePiece || pieceType == Pawn
          then 0
          else fiftyPlies + 1

      movedHistory = HM.insertWith (+) origBoard 1 history

      mkMovedGame board =
        if inCheck turn (boardPieces board)
          then Nothing
          else Just $ Game (halfmove + 1) movedFiftyPlies movedHistory board
