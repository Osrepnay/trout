module Trout.Game
  ( Game (..),
    hashBoard,
    mkBoard,
    startingGame,
    isDrawn,
    allMoves,
    allCaptures,
    squareAttackers,
    inCheck,
    makeMove,
  )
where

import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust)
import Data.Vector.Primitive ((!))
import Trout.Bitboard
  ( complement,
    countTrailingZeros,
    fileA,
    fileH,
    toSqs,
    (!<<.),
    (!>>.),
    (.&.),
    (.^.),
    (.|.),
  )
import Trout.Game.Board
  ( Board (..),
    Castling (..),
    Pieces (..),
    addPiece,
    canCastle,
    clearColorRights,
    clearRights,
    colorOccupancy,
    getPiece,
    occupancy,
    pieceBitboard,
    removePiece,
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

-- smallest first, for SEE
squareAttackers :: Color -> Pieces -> Int -> [Int]
squareAttackers color pieces sq =
  toSqs pawnAttackers
    ++ toSqs knightAttackers
    ++ toSqs bishopAttackers
    ++ toSqs rookAttackers
    ++ toSqs queenAttackers
    ++ toSqs kingAttackers
  where
    oppColor = other color
    getOppPieces = ($ pieces) . pieceBitboard . Piece oppColor
    sqSet = 1 !<<. sq

    pawnAttackers =
      getOppPieces Pawn
        .&. case oppColor of
          White ->
            (sqSet .&. complement fileA)
              !>>. 9
              .|. (sqSet .&. complement fileH)
              !>>. 7
          Black ->
            (sqSet .&. complement fileA)
              !<<. 7
              .|. (sqSet .&. complement fileH)
              !<<. 9
    knightAttackers = knightTable ! sq .&. getOppPieces Knight
    blockers = occupancy pieces
    bishopSqs = bishopMovesMagic blockers sq
    bishopAttackers = bishopSqs .&. getOppPieces Bishop
    rookSqs = rookMovesMagic blockers sq
    rookAttackers = rookSqs .&. getOppPieces Rook
    queenAttackers = (bishopSqs .|. rookSqs) .&. getOppPieces Queen
    kingAttackers = kingTable ! sq .&. getOppPieces King

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
  ( Game
      halfmove
      fiftyPlies
      history
      origBoard@(Board {boardPieces = pieces, boardTurn = turn, boardHash = hashed})
    )
  NullMove = do
    if inCheck turn pieces
      then Nothing
      else
        Just $
          Game
            (halfmove + 1)
            (fiftyPlies + 1)
            history
            (origBoard {boardTurn = other turn, boardHash = hashed .^. playingZobrist})
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

      movedHistory =
        -- unreversible
        -- should probably check more rigorously
        if isJust capturePiece || movedCastling /= castling || pieceType == Pawn
          then HM.empty
          else HM.insertWith (+) origBoard 1 history

      mkMovedGame board =
        if inCheck turn (boardPieces board)
          then Nothing
          else Just $ Game (halfmove + 1) movedFiftyPlies movedHistory board
