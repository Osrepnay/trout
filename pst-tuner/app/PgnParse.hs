module PgnParse
  ( Disambiguate (..),
    MoveAlgebraic (..),
    PgnMove (..),
    parsePgns,
    playPgn,
  )
where

import Control.Applicative (many, optional, (<|>))
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, manyTill, noneOf, oneOf, try)
import Text.Megaparsec.Char (char, digitChar, space, space1, string')
import Text.Megaparsec.Char.Lexer (charLiteral, decimal, float, signed)
import Trout.Game (Game (..), allMoves, makeMove, startingGame)
import Trout.Game.Move (Move (..), SpecialMove (..))
import Trout.Piece (Color (..), PieceType (..))
import Trout.Search.Worthiness (scoreIsMate)

type Parser = Parsec Void Text

cleanHeader :: Parser ()
cleanHeader = between (char '[') (char ']') innards $> ()
  where
    stringLiteral = char '"' >> manyTill charLiteral (char '"')
    innards = manyTill anySingle space1 *> stringLiteral

cleanHeaders :: Parser ()
cleanHeaders = many (cleanHeader *> space) $> ()

data Disambiguate = Disambiguate
  { disamRow :: Maybe Int,
    disamCol :: Maybe Int
  }
  deriving (Show)

noDisam :: Disambiguate
noDisam = Disambiguate Nothing Nothing

data MoveAlgebraic
  = KingsideCastle
  | QueensideCastle
  | Standard PieceType Disambiguate Int (Maybe PieceType)
  deriving (Show)

normRow :: Char -> Int
normRow row = ord row - ord '1'

normCol :: Char -> Int
normCol col = ord col - ord 'a'

parseSq :: Parser Int
parseSq = optional (char 'x') *> (handleSq <$> oneOf "abcdefgh") <*> digitChar
  where
    handleSq col row = normRow row * 8 + normCol col

parseDisam :: Parser Disambiguate
parseDisam = makeDisam <$> optional (oneOf "abcdefgh") <*> optional digitChar
  where
    makeDisam col row = Disambiguate (normRow <$> row) (normCol <$> col)

mkPieceType :: Char -> PieceType
mkPieceType c = case c of
  'P' -> Pawn
  'N' -> Knight
  'B' -> Bishop
  'R' -> Rook
  'Q' -> Queen
  'K' -> King
  _ -> error "impossible!"

parsePromo :: Parser PieceType
parsePromo = char '=' *> (mkPieceType <$> oneOf "NRBQ")

parseStandard :: Parser MoveAlgebraic
parseStandard =
  ( try (Standard <$> pieceTypeParser <*> parseDisam <*> parseSq <*> optional parsePromo)
      <|> (Standard <$> pieceTypeParser <*> pure noDisam <*> parseSq <*> optional parsePromo)
  )
    <* many (oneOf "+#")
  where
    pieceTypeParser = maybe Pawn mkPieceType <$> optional (oneOf "NRBQK")

parseCastle :: Parser MoveAlgebraic
parseCastle =
  ( (string' (T.pack "O-O-O") $> QueensideCastle)
      <|> (string' (T.pack "O-O") $> KingsideCastle)
  )
    <* many (oneOf "+#")

parseAlgebraic :: Parser MoveAlgebraic
parseAlgebraic = parseCastle <|> parseStandard

data PgnMove = PgnMove
  { pgnMoveAlg :: MoveAlgebraic,
    pgnMoveScore :: Maybe Int
  }
  deriving (Show)

parseAnnotation :: Parser (Maybe Int)
parseAnnotation = between (char '{') (char '}') inner
  where
    inner =
      string' (T.pack "book") $> Nothing
        <|> Just . round . (* 100)
          <$> signed (pure ()) (float :: Parser Double)
          <* char '/'
          <* (decimal :: Parser Int)
          <* space
          <* many (noneOf "}")

parsePgnMove :: Parser PgnMove
parsePgnMove =
  optional (try ((decimal :: Parser Int) *> char '.' *> space))
    *> (PgnMove <$> parseAlgebraic)
    <*> (space *> parseAnnotation)

parseResult :: Parser (Maybe Color)
parseResult =
  string' (T.pack "1-0") $> Just White
    <|> string' (T.pack "0-1") $> Just Black
    <|> string' (T.pack "1/2-1/2") $> Nothing
    <|> string' (T.pack "*") $> Nothing

parsePgn :: Parser ([PgnMove], Maybe Color)
parsePgn = cleanHeaders *> ((,) <$> many (parsePgnMove <* space) <*> parseResult)

parsePgns :: Parser [([PgnMove], Maybe Color)]
parsePgns = many (parsePgn <* space)

-- will crash at a moment's notice but honestly cba import safe.... etc.
-- its fine for now, if it crashes its not the end of the worldb
-- and could uncover bug in engine or fastchess
playPgn :: ([PgnMove], Maybe Color) -> [(Game, Double)]
playPgn (moves, res) =
  tail $
    reverse $
      snd $
        foldl'
          ( \(lastGame, prevGames) pgnMove ->
              -- pgn doesn't disambiguate for pins...
              let candidates =
                    filter (moveValidator (pgnMoveAlg pgnMove)) $
                      allMoves (gameBoard lastGame)
                  -- remove mate scores
                  realScore =
                    pgnMoveScore pgnMove
                      >>= ( \score ->
                              if scoreIsMate score
                                then Nothing
                                else Just score
                          )
                  !newGame = fromJust $ foldl' (<|>) Nothing (makeMove lastGame <$> candidates)
               in (newGame, maybe prevGames (\_ -> (newGame, gameResDouble) : prevGames) realScore)
          )
          (startingGame, [])
          moves
  where
    disamValidator disam sq =
      maybe True (== sq `quot` 8) (disamRow disam)
        && maybe True (== sq `rem` 8) (disamCol disam)

    promoValidator (Just pgnP) (Promotion moveP) = pgnP == moveP
    promoValidator (Just _) _ = False
    -- not strictly correct but we can never have a counterexample irl
    promoValidator Nothing _ = True

    moveValidator KingsideCastle move = case moveSpecial move of
      CastleKing -> True
      _ -> False
    moveValidator QueensideCastle move = case moveSpecial move of
      CastleQueen -> True
      _ -> False
    moveValidator (Standard pieceType disam to maybePromo) move =
      movePiece move == pieceType
        && moveTo move == to
        && disamValidator disam (moveFrom move)
        && promoValidator maybePromo (moveSpecial move)
    gameResDouble = case res of
      Just White -> 1
      Just Black -> 0
      Nothing -> 0.5
