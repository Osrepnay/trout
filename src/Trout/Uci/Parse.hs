{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Trout.Uci.Parse
  ( CommPositionInit (..),
    CommGoArg (..),
    UciCommand (..),
    UciMove (..),
    parseUciCommand,
    readUciLine,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (ord)
import Data.Functor (($>), (<&>))
import Text.Parsec
  ( alphaNum,
    anyChar,
    digit,
    eof,
    many,
    many1,
    manyTill,
    oneOf,
    optionMaybe,
    parse,
    skipMany1,
    space,
    spaces,
    string,
    string',
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Trout.Fen.Parse (Fen, parseFen)
import Trout.Piece (PieceType (..))

data CommPositionInit
  = PositionStartpos
  | PositionFen Fen
  deriving (Eq, Show)

-- has less info than trout move
data UciMove = UciMove
  { uciMoveFrom :: Int,
    uciMoveTo :: Int,
    uciMovePromote :: Maybe PieceType
  }
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
  | CommPosition CommPositionInit [UciMove]
  | CommGo [CommGoArg]
  | CommStop
  | CommPonderhit
  | CommQuit
  deriving (Eq, Show)

-- TODO a lot of this is kinda questionably correct, run through it fully sometime

-- make sure there aren't any trailing bits
commBreak :: Parser ()
commBreak = (many1 space $> ()) <|> eof

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- simple commands without arguments
parseArgless :: Parser UciCommand
parseArgless =
  try (string' "uci" *> commBreak) $> CommUci
    <|> try (string' "Dont" *> commBreak) $> CommDont
    <|> try (string' "isready" *> commBreak) $> CommIsready
    <|> try (string' "ucinewgame" *> commBreak) $> CommUcinewgame
    <|> try (string' "stop" *> commBreak) $> CommStop
    <|> try (string' "ponderhit" *> commBreak) $> CommPonderhit
    <|> try (string' "quit" *> commBreak) $> CommQuit

parseDebug :: Parser UciCommand
parseDebug =
  string' "debug"
    *> spaces1
    *> (string' "on" $> True <|> string' "off" $> False)
    <* commBreak
    <&> CommDebug

parseSetoption :: Parser UciCommand
parseSetoption =
  CommSetoption
    <$> ( string' "setoption"
            *> spaces1
            *> string "name"
            *> manyTill anyChar (string' "value")
        )
    <*> many anyChar
    <* commBreak

-- garbage command for normie engines!
parseRegister :: Parser UciCommand
parseRegister = CommRegister <$> (string' "register" *> many anyChar)

parsePosition :: Parser UciCommand
parsePosition =
  string' "position"
    *> spaces1
    $> CommPosition
    <*> ( string' "startpos" $> PositionStartpos
            <|> PositionFen <$> (string' "fen" *> spaces1 *> parseFen)
        )
    <*> ( spaces1
            *> string "moves"
            *> many parseUciMoves
            <|> pure [] -- maybe should be more strict; reject if mangled here
        )
    <* commBreak
  where
    makeUciMove fromCol fromRow toCol toRow maybePromote =
      UciMove
        fromSq
        toSq
        maybePromotePiece
      where
        fromSq = (ord fromRow - ord '1') * 8 + ord fromCol - ord 'a'
        toSq = (ord toRow - ord '1') * 8 + ord toCol - ord 'a'
        maybePromotePiece =
          maybePromote
            >>= \p -> case p of
              'n' -> Just Knight
              'b' -> Just Bishop
              'r' -> Just Rook
              'q' -> Just Queen
              _ -> Nothing
    parseUciMove :: Parser UciMove
    parseUciMove =
      makeUciMove
        <$> oneOf "abcdefgh"
        <*> oneOf "12345678"
        <*> oneOf "abcdefgh"
        <*> oneOf "12345678"
        <*> optionMaybe (oneOf "nbrq")
    parseUciMoves = spaces1 *> parseUciMove

parseGo :: Parser UciCommand
parseGo = string' "go" *> many (spaces1 *> parseArg) <&> CommGo
  where
    parseIntArg :: String -> (Int -> CommGoArg) -> Parser CommGoArg
    parseIntArg n c = string' n *> spaces1 *> (c . read <$> many1 digit)
    parseArg =
      ( string' "searchmoves"
          *> many (try (spaces *> many alphaNum))
          <&> GoSearchMoves
      )
        <|> string' "ponder" $> GoPonder
        <|> parseIntArg "wtime" GoWtime
        <|> parseIntArg "winc" GoWinc
        <|> parseIntArg "btime" GoBtime
        <|> parseIntArg "binc" GoBinc
        <|> parseIntArg "movestogo" GoMovestogo
        <|> parseIntArg "depth" GoDepth
        <|> parseIntArg "nodes" GoNodes
        <|> parseIntArg "mate" GoMate
        <|> parseIntArg "movetime" GoMovetime
        <|> string' "infinite" $> GoInfinite

parseUciCommand :: Parser UciCommand
parseUciCommand =
  parseArgless
    <|> parseDebug
    <|> parseSetoption
    <|> parseRegister
    <|> parsePosition
    <|> parseGo

-- dont require parsec import
readUciLine :: String -> Either String UciCommand
readUciLine = first show . parse parseUciCommand ""
