module Trout.Fen.Parse (parseFen, readFen, Fen (..), fenToGame) where

import Control.Applicative    ((<|>))
import Data.Bifunctor         (first)
import Data.Char              (digitToInt, isDigit, ord)
import Data.Functor           (($>), (<&>))
import Text.Parsec            (char, oneOf, parse, spaces)
import Text.Parsec.Char       (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String     (Parser)
import Trout.Bitboard         (Bitboard, fromSqs, (.|.))
import Trout.Game             (Game (Game), Pieces (Pieces), Sides)
import Trout.Piece            (Color (Black, White))

parsePieces :: Parser (Sides Pieces)
parsePieces = fenRows
    <&> \pcs ->
        ( Pieces
            (genBitboard 'P' pcs)
            (genBitboard 'N' pcs)
            (genBitboard 'B' pcs)
            (genBitboard 'R' pcs)
            (genBitboard 'Q' pcs)
            (genBitboard 'K' pcs)
        , Pieces
            (genBitboard 'p' pcs)
            (genBitboard 'n' pcs)
            (genBitboard 'b' pcs)
            (genBitboard 'r' pcs)
            (genBitboard 'q' pcs)
            (genBitboard 'k' pcs)
        )
  where
    expandRow _ [] = []
    expandRow i (x : xs)
        | isDigit x = expandRow (i + digitToInt x) xs
        | otherwise = (i, x) : expandRow (i + 1) xs
    fenRow y = map (first (\x -> x + y * 8))
        . expandRow 0
        <$> many1 (oneOf "12345678pnbrqkPNBRQK")
    fenRows = concat
        <$> sequence
            [ fenRow 7 <* char '/'
            , fenRow 6 <* char '/'
            , fenRow 5 <* char '/'
            , fenRow 4 <* char '/'
            , fenRow 3 <* char '/'
            , fenRow 2 <* char '/'
            , fenRow 1 <* char '/'
            , fenRow 0
            ]
    genBitboard c pcs = fromSqs
        [ sq
        | sq <- [0..63]
        , Just c == lookup sq pcs
        ]

parseTurn :: Parser Color
parseTurn = char 'w' $> White
    <|> char 'b' $> Black

parseCastling :: Parser Bitboard
parseCastling = many1 (oneOf "kqKQ-")
    <&>
        \c ->
            (if 'K' `elem` c
                then 128
                else 0)
            .|. (if 'Q' `elem` c
                then 1
                else 0)
            .|. (if 'k' `elem` c
                then 9223372036854775808
                else 0)
            .|. (if 'q' `elem` c
                then 72057594037927936
                else 0)

parseEnPassant :: Parser (Maybe Int)
parseEnPassant = char '-' $> Nothing
    <|> (\c r -> Just (ord c - ord 'a' + ord r - ord '1'))
    <$> oneOf "abcdefgh"
    <*> oneOf "12345678"

data Fen = Fen
    { fenPieces    :: Sides Pieces
    , fenTurn      :: Color
    , fenCastling  :: Bitboard
    , fenEnPassant :: Maybe Int
    , fenHalfmove  :: Int
    , fenFullmove  :: Int
    } deriving (Eq, Show)

parseFen :: Parser Fen
parseFen = Fen
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
    (case turn of
        White -> Game w b
        Black -> Game b w)
    (fenCastling fen)
    (fenEnPassant fen)
    turn
  where
      (w, b) = fenPieces fen
      turn = fenTurn fen
