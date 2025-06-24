module Trout.Fen.Parse (parseFen, readFen, Fen (..), fenToGame) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit, ord)
import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>), (<&>))
import Data.HashMap.Strict qualified as HM
import Text.Parsec (char, oneOf, parse, spaces)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)
import Trout.Bitboard ((.|.))
import Trout.Game (Castling (..), Game (..), Pieces, addPiece, emptyPieces, mkBoard)
import Trout.Piece (Color (Black, White), Piece (..), PieceType (..))

-- TODO better fen handling; instead of 1 shot use setSq and friends
-- TODO error out properly with parsec if e.g. invalid fen

-- returns white as active side
parsePieces :: Parser Pieces
parsePieces =
  fenRows
    <&> foldl'
      ( \b (sq, piece) -> case piece of
          'P' -> addPiece (Piece White Pawn) sq b
          'N' -> addPiece (Piece White Knight) sq b
          'B' -> addPiece (Piece White Bishop) sq b
          'R' -> addPiece (Piece White Rook) sq b
          'Q' -> addPiece (Piece White Queen) sq b
          'K' -> addPiece (Piece White King) sq b
          'p' -> addPiece (Piece Black Pawn) sq b
          'n' -> addPiece (Piece Black Knight) sq b
          'b' -> addPiece (Piece Black Bishop) sq b
          'r' -> addPiece (Piece Black Rook) sq b
          'q' -> addPiece (Piece Black Queen) sq b
          'k' -> addPiece (Piece Black King) sq b
          _ -> error "invalid piece (impossible?)"
      )
      emptyPieces
  where
    expandRow _ [] = []
    expandRow i (x : xs)
      | isDigit x = expandRow (i + digitToInt x) xs
      | otherwise = (i, x) : expandRow (i + 1) xs
    fenRow y =
      map (first (\x -> x + y * 8))
        . expandRow 0
        <$> many1 (oneOf "12345678pnbrqkPNBRQK")
    fenRows =
      concat
        <$> sequence
          [ fenRow 7 <* char '/',
            fenRow 6 <* char '/',
            fenRow 5 <* char '/',
            fenRow 4 <* char '/',
            fenRow 3 <* char '/',
            fenRow 2 <* char '/',
            fenRow 1 <* char '/',
            fenRow 0
          ]

parseTurn :: Parser Color
parseTurn =
  char 'w' $> White
    <|> char 'b' $> Black

parseCastling :: Parser Castling
parseCastling =
  many1 (oneOf "kqKQ-")
    <&> \c ->
      Castling $
        ( if 'K' `elem` c
            then 2
            else 0
        )
          .|. ( if 'Q' `elem` c
                  then 1
                  else 0
              )
          .|. ( if 'k' `elem` c
                  then 8
                  else 0
              )
          .|. ( if 'q' `elem` c
                  then 4
                  else 0
              )

parseEnPassant :: Parser (Maybe Int)
parseEnPassant =
  char '-' $> Nothing
    <|> (\c r -> Just (ord c - ord 'a' + ord r - ord '1'))
      <$> oneOf "abcdefgh"
      <*> oneOf "12345678"

data Fen = Fen
  { fenPieces :: Pieces,
    fenTurn :: Color,
    fenCastling :: Castling,
    fenEnPassant :: Maybe Int,
    fenHalfmove :: Int,
    fenFullmove :: Int
  }
  deriving (Eq, Show)

parseFen :: Parser Fen
parseFen =
  Fen
    <$> (parsePieces <* spaces)
    <*> (parseTurn <* spaces)
    <*> (parseCastling <* spaces)
    <*> (parseEnPassant <* spaces)
    <*> (read <$> many1 digit <* spaces)
    <*> (read <$> many1 digit <* spaces)

readFen :: String -> Either String Fen
readFen = first show . parse parseFen ""

fenToGame :: Fen -> Game
fenToGame fen =
  Game
    ( fenFullmove fen * 2
        + ( if fenTurn fen == White
              then -1
              else 0
          )
    )
    (fenHalfmove fen)
    HM.empty
    ( mkBoard
        (fenPieces fen)
        (fenCastling fen)
        (fenEnPassant fen)
        turn
    )
  where
    turn = fenTurn fen
