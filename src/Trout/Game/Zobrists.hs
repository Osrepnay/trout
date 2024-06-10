module Trout.Game.Zobrists
  ( whitePawnZobrists,
    blackPawnZobrists,
    whiteKnightZobrists,
    blackKnightZobrists,
    whiteBishopZobrists,
    blackBishopZobrists,
    whiteRookZobrists,
    blackRookZobrists,
    whiteQueenZobrists,
    blackQueenZobrists,
    whiteKingZobrists,
    blackKingZobrists,
    castleZobrists,
    enPassantZobrists,
    pieceZobrists,
    playingZobrist,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Bits ((!<<.), (!>>.), (.^.))
import Data.Vector.Primitive (Vector, (!))
import Data.Vector.Primitive qualified as V
import Trout.Piece (Color (..), Piece (..))

-- NEED 64 BIT INT!!! (probably) FOR GOOD RESULTS!!!!!

xorshiftState :: State Int Int
xorshiftState = state $
  \s0 ->
    let s1 = s0 .^. (s0 !<<. 13)
        s2 = s1 .^. (s1 !>>. 7)
        s3 = s2 .^. (s2 !<<. 17)
     in (s3, s3)

-- all of the zobrists, cut it up later
-- easier to generate all of them in one go
allOfThem :: Vector Int
allOfThem =
  V.fromList $
    evalState
      (replicateM (12 * 64 + 16 + 1 + 8) xorshiftState)
      69420

whitePawnZobrists :: Vector Int
blackPawnZobrists :: Vector Int
whiteKnightZobrists :: Vector Int
blackKnightZobrists :: Vector Int
whiteBishopZobrists :: Vector Int
blackBishopZobrists :: Vector Int
whiteRookZobrists :: Vector Int
blackRookZobrists :: Vector Int
whiteQueenZobrists :: Vector Int
blackQueenZobrists :: Vector Int
whiteKingZobrists :: Vector Int
blackKingZobrists :: Vector Int
whitePawnZobrists = V.slice (0 * 64) 64 allOfThem

blackPawnZobrists = V.slice (1 * 64) 64 allOfThem

whiteKnightZobrists = V.slice (2 * 64) 64 allOfThem

blackKnightZobrists = V.slice (3 * 64) 64 allOfThem

whiteBishopZobrists = V.slice (4 * 64) 64 allOfThem

blackBishopZobrists = V.slice (5 * 64) 64 allOfThem

whiteRookZobrists = V.slice (6 * 64) 64 allOfThem

blackRookZobrists = V.slice (7 * 64) 64 allOfThem

whiteQueenZobrists = V.slice (8 * 64) 64 allOfThem

blackQueenZobrists = V.slice (9 * 64) 64 allOfThem

whiteKingZobrists = V.slice (10 * 64) 64 allOfThem

blackKingZobrists = V.slice (11 * 64) 64 allOfThem

castleZobrists :: Vector Int
castleZobrists = V.slice (12 * 64) 16 allOfThem

-- is white?
playingZobrist :: Int
playingZobrist = allOfThem ! (12 * 64 + 16)

enPassantZobrists :: Vector Int
enPassantZobrists = V.slice (12 * 64 + 16 + 1) 8 allOfThem

pieceZobrists :: Color -> Piece -> Vector Int
pieceZobrists White Pawn = whitePawnZobrists
pieceZobrists Black Pawn = blackPawnZobrists
pieceZobrists White Knight = whiteKnightZobrists
pieceZobrists Black Knight = blackKnightZobrists
pieceZobrists White Bishop = whiteBishopZobrists
pieceZobrists Black Bishop = blackBishopZobrists
pieceZobrists White Rook = whiteRookZobrists
pieceZobrists Black Rook = blackRookZobrists
pieceZobrists White Queen = whiteQueenZobrists
pieceZobrists Black Queen = blackQueenZobrists
pieceZobrists White King = whiteKingZobrists
pieceZobrists Black King = blackKingZobrists
