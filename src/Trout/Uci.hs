module Trout.Uci
    ( CommPositionInit (..)
    , CommGoArg (..)
    , UciCommand (..)
    , parseUciCommand
    ) where

import Data.Functor       (($>), (<&>))
import Text.Parsec
    ( alphaNum
    , anyChar
    , many
    , manyTill
    , spaces
    , string
    , try
    , (<|>), many1, digit
    )
import Text.Parsec.String (Parser)

data CommPositionInit
    = PositionStartpos
    | PositionFen String
    deriving (Eq, Show)

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
    | CommPosition CommPositionInit [String]
    | CommGo [CommGoArg]
    | CommStop
    | CommPonderhit
    | CommQuit
    deriving (Eq, Show)

-- simple commands without arguments
parseArgless :: Parser UciCommand
parseArgless = try (string "uci") $> CommUci -- TODO when switch to nightly or get newer parsec switch to string'
    <|> try (string "Dont") $> CommDont
    <|> try (string "isready") $> CommIsready
    <|> try (string "ucinewgame") $> CommUcinewgame
    <|> try (string "stop") $> CommStop
    <|> try (string "ponderhit") $> CommPonderhit
    <|> try (string "quit") $> CommQuit

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
    $> CommPosition
    <*> (string "startpos" $> PositionStartpos
        <|> PositionFen <$> (string "fen" *> parseFen))
    <*> (spaces
        *> string "moves"
        *> many parseMove)
  where
    parseFen = manyTill anyChar (try (string "moves"))
    parseMove = spaces *> many alphaNum

parseGo :: Parser UciCommand
parseGo = try (string "go") *> many (spaces *> parseArg) <&> CommGo
  where
    parseIntArg :: String -> (Int -> CommGoArg) -> Parser CommGoArg
    parseIntArg n c = try (string n) *> spaces *> (c . read <$> many1 digit)
    parseArg = (try (string "searchmoves")
            *> many (spaces *> many alphaNum)
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
