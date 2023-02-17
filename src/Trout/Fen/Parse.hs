module Trout.Fen.Parse (parseFen, readFen, Fen (..), fenToGame) where

import Control.Applicative    ((<|>))
import Data.Bifunctor         (first)
import Data.Char              (digitToInt, isDigit, ord)
import Data.Functor           (($>), (<&>))
import Text.Parsec            (char, oneOf, parse, spaces)
import Text.Parsec.Char       (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String     (Parser)
import Trout.Bitboard         (fromSqs)
import Trout.Game
    ( CanCastle (CanCastle)
    , Game (Game)
    , Pieces (Pieces)
    , Sides
    )
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

parseCastling :: Parser (Sides CanCastle)
parseCastling = many1 (oneOf "kqKQ-")
    <&>
        \c ->
            ( CanCastle ('K' `elem` c) ('Q' `elem` c)
            , CanCastle ('k' `elem` c) ('q' `elem` c)
            )

parseEnPassant :: Parser (Maybe Int)
parseEnPassant = char '-' $> Nothing
    <|> (\c r -> Just (ord c - ord 'a' + ord r - ord '1'))
    <$> oneOf "abcdefgh"
    <*> oneOf "12345678"

data Fen = Fen
    { fenPieces    :: Sides Pieces
    , fenTurn      :: Color
    , fenCastling  :: Sides CanCastle
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
fenToGame fen = Game
    (fenPieces fen)
    (fenCastling fen)
    (fenEnPassant fen)
    (fenTurn fen)
