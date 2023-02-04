module Trout.Fen.Parse where

import Control.Applicative    ((<|>))
import Data.Bifunctor         (first)
import Data.Char              (digitToInt, isDigit)
import Data.Functor           (($>), (<&>))
import Text.Parsec            (char, oneOf)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String     (Parser)
import Trout.Bitboard         (fromSqs)
import Trout.Game             (Pieces (Pieces))
import Trout.Piece            (Color (Black, White))

parsePieces :: Parser Pieces
parsePieces = fenRows
    <&> \pcs ->
        Pieces
            (genBitboard 'p' pcs)
            (genBitboard 'n' pcs)
            (genBitboard 'b' pcs)
            (genBitboard 'r' pcs)
            (genBitboard 'q' pcs)
            (genBitboard 'k' pcs)
  where
    expandRow _ [] = []
    expandRow i (x : xs)
        | isDigit x = (i, x) : expandRow (i + digitToInt x) xs
        | otherwise = (i, x) : expandRow i xs
    fenRow :: Int -> Parser [(Int, Char)]
    fenRow y = map (first (\x -> x + y * 8))
        . expandRow 0
        <$> many1 (oneOf "1234567pnbrqk")
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

parseSide :: Parser Color
parseSide = char 'w' $> White
    <|> char 'b' $> Black
