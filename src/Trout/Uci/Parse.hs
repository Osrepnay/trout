module Trout.Uci.Parse
    ( CommPositionInit (..)
    , CommGoArg (..)
    , UciCommand (..)
    , UciMove (..)
    , parseUciCommand
    , readUciLine
    ) where

import Data.Bifunctor     (Bifunctor (first))
import Data.Char          (ord)
import Data.Functor       (($>), (<&>))
import Text.Parsec
    ( alphaNum
    , anyChar
    , digit
    , eof
    , many
    , many1
    , manyTill
    , oneOf
    , optionMaybe
    , parse
    , space
    , spaces
    , string
    , try
    , (<|>)
    )
import Text.Parsec.String (Parser)
import Trout.Piece        (Piece (..))
import Trout.Fen.Parse (parseFen, Fen)

data CommPositionInit
    = PositionStartpos
    | PositionFen Fen
    deriving (Eq, Show)

-- has less info than trout move
data UciMove = UciMove
    { uciMoveFrom    :: Int
    , uciMoveTo      :: Int
    , uciMovePromote :: Maybe Piece
    } deriving (Eq, Show)

data CommGoArg
    = GoSearchMoves [String]
    | GoPonder
    | GoWtime Int
    | GoWinc Int
    | GoBtime Int
    | GoBinc Int
    | GoMovestogo Int
    | GoDepth Int
    | GoNodes Int
    | GoMate Int
    | GoMovetime Int
    | GoInfinite
    deriving (Eq, Show)

data UciCommand
    = CommUci
    | CommDebug Bool
    | CommDont
    | CommIsready
    | CommSetoption String String
    | CommRegister String -- not actual type, but we dont need register
    | CommUcinewgame
    | CommPosition CommPositionInit [UciMove]
    | CommGo [CommGoArg]
    | CommStop
    | CommPonderhit
    | CommQuit
    deriving (Eq, Show)

wordBreak :: Parser ()
wordBreak = (many1 space $> ()) <|> eof

-- simple commands without arguments
parseArgless :: Parser UciCommand
parseArgless = try (string "uci" *> wordBreak) $> CommUci -- TODO when switch to nightly or get newer parsec switch to string'
    <|> try (string "Dont" *> wordBreak) $> CommDont
    <|> try (string "isready" *> wordBreak) $> CommIsready
    <|> try (string "ucinewgame" *> wordBreak) $> CommUcinewgame
    <|> try (string "stop" *> wordBreak) $> CommStop
    <|> try (string "ponderhit" *> wordBreak) $> CommPonderhit
    <|> try (string "quit" *> wordBreak) $> CommQuit

parseDebug :: Parser UciCommand
parseDebug = try (string "debug")
    *> spaces
    *> (string "on" $> True <|> string "off" $> False)
    <&> CommDebug

parseSetoption :: Parser UciCommand
parseSetoption = CommSetoption
    <$> (try (string "setoption")
        *> spaces
        *> string "name"
        *> manyTill anyChar (try (string "value")))
    <*> many anyChar

-- garbage command for normie engines!
parseRegister :: Parser UciCommand
parseRegister = CommRegister <$> (try (string "register") *> many anyChar)

parsePosition :: Parser UciCommand
parsePosition = try (string "position")
    *> spaces
    $> CommPosition
    <*> (string "startpos" $> PositionStartpos
        <|> PositionFen <$> (string "fen" *> spaces *> parseFen))
    <*> (spaces
        *> string "moves"
        *> many parseUciMoves
        <|> pure [])
  where
    makeUciMove fromCol fromRow toCol toRow maybePromote = UciMove
        fromSq
        toSq
        maybePromotePiece
      where
        fromSq = (ord fromRow - ord '1') * 8 + ord fromCol - ord 'a'
        toSq = (ord toRow - ord '1') * 8 + ord toCol - ord 'a'
        maybePromotePiece = maybePromote
            >>= \p ->
                case p of
                    'n' -> Just Knight
                    'b' -> Just Bishop
                    'r' -> Just Rook
                    'q' -> Just Queen
                    _   -> Nothing
    parseUciMove :: Parser UciMove
    parseUciMove = makeUciMove
        <$> oneOf "abcdefgh"
        <*> oneOf "12345678"
        <*> oneOf "abcdefgh"
        <*> oneOf "12345678"
        <*> optionMaybe (oneOf "nbrq")
    parseUciMoves = spaces *> parseUciMove

parseGo :: Parser UciCommand
parseGo = try (string "go") *> many (try (spaces *> parseArg)) <&> CommGo
  where
    parseIntArg :: String -> (Int -> CommGoArg) -> Parser CommGoArg
    parseIntArg n c = try (string n) *> spaces *> (c . read <$> many1 digit)
    parseArg =
        (try (string "searchmoves")
            *> many (try (spaces *> many alphaNum))
            <&> GoSearchMoves)
        <|> try (string "ponder") $> GoPonder
        <|> parseIntArg "wtime" GoWtime
        <|> parseIntArg "winc" GoWinc
        <|> parseIntArg "btime" GoBtime
        <|> parseIntArg "binc" GoBinc
        <|> parseIntArg "movestogo" GoMovestogo
        <|> parseIntArg "depth" GoDepth
        <|> parseIntArg "nodes" GoNodes
        <|> parseIntArg "mate" GoMate
        <|> parseIntArg "movetime" GoMovetime
        <|> try (string "infinite") $> GoInfinite

parseUciCommand :: Parser UciCommand
parseUciCommand = parseArgless
    <|> parseDebug
    <|> parseSetoption
    <|> parseRegister
    <|> parsePosition
    <|> parseGo

-- dont require parsec import
readUciLine :: String -> Either String UciCommand
readUciLine = first show . parse parseUciCommand ""
