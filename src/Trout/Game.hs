{-# LANGUAGE TemplateHaskell #-}
module Trout.Game
    ( Pieces (..)
    , pawns, knights, bishops, rooks, queens, kings
    , CanCastle (..)
    , canCastleKing, canCastleQueen
    , Sides
    , sideWhite, sideBlack, sideByColor
    , Game (..)
    , gamePieces, gameCastling, gameEnPassant, gameTurn
    , fromFen
    , gameAsBoard
    , startingGame
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Char                        (isDigit, ord)
import Data.Function                    ((&))
import Data.List.Split                  (splitOn)
import Data.Vector.Primitive            ((!))
import Lens.Micro                       (Lens', (%~), (.~), (?~), (^.))
import Lens.Micro.TH                    (makeLenses)
import Trout.Bitboard
    ( Bitboard
    , bit
    , blocked
    , clearBit
    , complement
    , countTrailingZeros
    , fromSqs
    , setBit
    , toSqs
    , xyToSq
    , zeroBits
    , (.&.)
    , (.|.)
    )
import Trout.Game.MoveGen
    ( Move (..)
    , SpecialMove (..)
    , bishopMoves
    , kingMoves
    , kingTable
    , knightMoves
    , knightTable
    , pawnBlackAttackTable
    , pawnMoves
    , pawnWhiteAttackTable
    , queenMoves
    , rookMoves
    )
import Trout.Game.MoveGen.Sliding.Magic
import Trout.Piece

data Pieces = Pieces
    { _pawns   :: !Bitboard
    , _knights :: !Bitboard
    , _bishops :: !Bitboard
    , _rooks   :: !Bitboard
    , _queens  :: !Bitboard
    , _kings   :: !Bitboard
    } deriving (Eq, Show)
makeLenses ''Pieces

-- ghc gets mad when i use the normal lens typedef
byPiece :: Functor f => Piece -> (Bitboard -> f Bitboard) -> Pieces -> f Pieces
byPiece Pawn   = pawns
byPiece Knight = knights
byPiece Bishop = bishops
byPiece Rook   = rooks
byPiece Queen  = queens
byPiece King   = kings

data CanCastle = CanCastle
    { _canCastleKing  :: !Bool
    , _canCastleQueen :: !Bool
    } deriving (Eq, Show)
makeLenses ''CanCastle

type Sides a = (a, a)

sideWhite :: Functor f => (a -> f a) -> Sides a -> f (Sides a)
sideWhite afb (a, b) = (\a' -> (a', b)) <$> afb a
{-# INLINE sideWhite #-}

sideBlack :: Functor f => (a -> f a) -> Sides a -> f (Sides a)
sideBlack afb (a, b) = (\b' -> (a, b')) <$> afb b
{-# INLINE sideBlack #-}

sideByColor :: Functor f => Color -> (a -> f a) -> Sides a -> f (Sides a)
sideByColor White afb (a, b) = (\a' -> (a', b)) <$> afb a
sideByColor Black afb (a, b) = (\b' -> (a, b')) <$> afb b
{-# INLINE sideByColor #-}

sideByntColor :: Functor f => Color -> (a -> f a) -> Sides a -> f (Sides a)
sideByntColor Black afb (a, b) = (\a' -> (a', b)) <$> afb a
sideByntColor White afb (a, b) = (\b' -> (a, b')) <$> afb b
{-# INLINE sideByntColor #-}

data Game = Game
    { _gamePieces    :: {-# UNPACK #-} !(Sides Pieces)
    , _gameCastling  :: !(Sides CanCastle)
    , _gameEnPassant :: !(Maybe Int)
    , _gameTurn      :: !Color
    } deriving (Eq, Show)
makeLenses ''Game

piecesAll :: Pieces -> Bitboard
piecesAll (Pieces p n b r q k) = p .|. n .|. b .|. r .|. q .|. k

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    ( Pieces
        65280
        66
        36
        129
        8
        16
    , Pieces
        71776119061217280
        4755801206503243776
        2594073385365405696
        9295429630892703744
        576460752303423488
        1152921504606846976
    )
    ( CanCastle True True
    , CanCastle True True
    )
    Nothing
    White

gameAsBoard :: Game -> String
gameAsBoard game = unlines [[posChar x y | x <- [0..7]] | y <- [7, 6..0]]
  where
    posChar x y = fst $ head $ filter (\(_, b) -> blocked b sq)
        [ ('P', whitePieces ^. pawns)
        , ('N', whitePieces ^. knights)
        , ('B', whitePieces ^. bishops)
        , ('R', whitePieces ^. rooks)
        , ('Q', whitePieces ^. queens)
        , ('K', whitePieces ^. kings)
        , ('p', blackPieces ^. pawns)
        , ('n', blackPieces ^. knights)
        , ('b', blackPieces ^. bishops)
        , ('r', blackPieces ^. rooks)
        , ('q', blackPieces ^. queens)
        , ('k', blackPieces ^. kings)
        , ('.', complement zeroBits)
        ]
      where
        whitePieces = game ^. gamePieces . sideWhite
        blackPieces = game ^. gamePieces . sideBlack
        sq = xyToSq x y

fromFen :: String -> Game
fromFen fen = Game
    { _gamePieces =
        ( Pieces
            { _pawns = bbBy (== 'P')
            , _knights = bbBy (== 'N')
            , _bishops = bbBy (== 'B')
            , _rooks = bbBy (== 'R')
            , _queens = bbBy (== 'Q')
            , _kings = bbBy (== 'K')
            }
        , Pieces
            { _pawns = bbBy (== 'p')
            , _knights = bbBy (== 'n')
            , _bishops = bbBy (== 'b')
            , _rooks = bbBy (== 'r')
            , _queens = bbBy (== 'q')
            , _kings = bbBy (== 'k')
            }
        )
    , _gameCastling = (whiteCastles, blackCastles)
    , _gameEnPassant = enPassant
    , _gameTurn = color
    }
  where
    bbBy f = fromSqs $ fst <$> filter (f . snd) flatBoard
    fenSections = words fen
    flatBoard = zip [0..]
        $ concat
        $ reverse
        $ (>>= (\c -> if isDigit c then replicate (read [c]) ' ' else [c]))
        <$> splitOn "/" (head fenSections)
    color = case fenSections !! 1 of
        "w" -> White
        "b" -> Black
        _   -> error "unknown color"
    whiteCastles = CanCastle
        ('K' `elem` fenSections !! 2)
        ('Q' `elem` fenSections !! 2)
    blackCastles = CanCastle
        ('k' `elem` fenSections !! 2)
        ('q' `elem` fenSections !! 2)
    enPassant
        | fenSections !! 3 == "-" = Nothing
        | otherwise               = Just (parseCoord (fenSections !! 3))
    parseCoord coordStr = file + row * 8
      where
        file = ord (head coordStr) - ord 'a'
        row = ord (coordStr !! 1) - ord '0' - 1

flipTurn :: Game -> Game
flipTurn (Game w b enP White) = Game w b enP Black
flipTurn (Game w b enP Black) = Game w b enP White

gameBlockers :: Game -> Bitboard
gameBlockers game =
    piecesAll (game ^. gamePieces . sideByColor (game ^. gameTurn))
    .|. piecesAll (game ^. gamePieces . sideByntColor (game ^. gameTurn))

allMoves :: Game -> [Move]
allMoves game = concat
    [ moveSqs (pawnMoves (game ^. gameEnPassant) (game ^. gameTurn)) pawns
    , moveSqs knightMoves knights
    , moveSqs bishopMoves bishops
    , moveSqs rookMoves rooks
    , moveSqs queenMoves queens
    , moveSqs (kingMoves kingside queenside) kings
    ]
  where
    kingside = game ^. gameCastling . turnSide . canCastleKing
    queenside = game ^. gameCastling . turnSide . canCastleQueen
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover piece = concatMap
        (mover block myBlock)
        (toSqs (game ^. gamePieces . turnSide . piece))
    -- TODO update incrementally
    block = myBlock .|. piecesAll (game ^. gamePieces . oppSide)
    myBlock = piecesAll (game ^. gamePieces . turnSide)

    turnSide :: Lens' (Sides a) a
    oppSide :: Lens' (Sides a) a
    turnSide = sideByColor (game ^. gameTurn)
    oppSide = sideByntColor (game ^. gameTurn)

squareAttacked :: Int -> Game -> Bool
squareAttacked sq game = pawnAttackTable ! sq .&. thisOppPieces ^. pawns /= 0
    || knightTable ! sq .&. thisOppPieces ^. knights /= 0
    || bishoped .&. thisOppPieces ^. bishops /= 0
    || rooked .&. thisOppPieces ^. rooks /= 0
    || bishoped .&. thisOppPieces ^. queens /= 0
    || rooked .&. thisOppPieces ^. queens /= 0
    || kingTable ! sq .&. thisOppPieces ^. kings /= 0
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    block = gameBlockers game
    pawnAttackTable = case game ^. gameTurn of
        White -> pawnWhiteAttackTable
        Black -> pawnBlackAttackTable
    thisOppPieces = game ^. gamePieces . sideByntColor (game ^. gameTurn)

inCheck :: Game -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise    = squareAttacked kingSq game
  where
    kingSq = countTrailingZeros kingMask
    kingMask = game ^. gamePieces . sideByColor (game ^. gameTurn) . kings

makeMove :: Game -> Move -> Maybe Game
makeMove game (Move piece special from to) = do
    let moveAndCaptured = captureAll to (doMove piece from to game)
    afterSpecials <- specials moveAndCaptured
    checkChecked <- nothingIfCheck afterSpecials
    let castleCleared = clearCastles checkChecked
    pure (flipTurn castleCleared)
  where
    -- basic moving
    doMove p fsq tsq = gamePieces
        . turnSide
        . byPiece p
        %~ (`setBit` tsq) . (`clearBit` fsq)
    captureAll sq = gamePieces
        . oppSide
        %~ \(Pieces p n b r q k) -> Pieces
            (p .&. clearMask)
            (n .&. clearMask)
            (b .&. clearMask)
            (r .&. clearMask)
            (q .&. clearMask)
            (k .&. clearMask)
      where
        clearMask = complement (bit sq)
    clearCastles =
        (gameCastling
            . sideWhite
            %~ \(CanCastle k q) -> CanCastle
                (k && from /= 7 && to /= 7)
                (q && from /= 0 && to /= 0))
        . (gameCastling
            . sideBlack
            %~ \(CanCastle k q) -> CanCastle
                (k && from /= 63 && to /= 63)
                (q && from /= 56 && to /= 56))
        . case piece of
            King -> gameCastling . turnSide .~ CanCastle False False
            _    -> id
    nothingIfCheck g = if inCheck g then Nothing else Just g
    specials = case special of
        PawnDouble -> Just . (gameEnPassant ?~ to)
        EnPassant enPSq -> Just
            . clearEnPassant
            . (gamePieces
                . oppSide
                . pawns
                %~ (`clearBit` enPSq))
        Promotion promote -> Just
            . clearEnPassant
            . (gamePieces
                . turnSide
                . byPiece promote
                %~ (`setBit` to))
            . (gamePieces
                . turnSide
                . pawns
                %~ (`clearBit` to))
        CastleKing ->
            fmap (clearEnPassant
                . (gamePieces
                    . turnSide
                    . rooks
                    %~ (`setBit` (kingOrigin + 1))
                    . (`clearBit` (kingOrigin + 3))))
            . throughCheckKing
        CastleQueen ->
            fmap (clearEnPassant
                . (gamePieces
                    . turnSide
                    . rooks
                    %~ (`setBit` (kingOrigin - 1))
                    . (`clearBit` (kingOrigin - 4))))
            . throughCheckQueen
        Normal -> Just . clearEnPassant
    clearEnPassant = gameEnPassant .~ Nothing
    -- assumes king is moved, rook is not
    throughCheckKing g
        | squareAttacked (kingOrigin + 1) kingless = Nothing
        | squareAttacked kingOrigin kingless = Nothing
        | otherwise                 = Just g
      where
        kingless = g & gamePieces . turnSide . kings %~ (`clearBit` 6)
    throughCheckQueen g
        | squareAttacked (kingOrigin - 1) kingless = Nothing
        | squareAttacked kingOrigin kingless = Nothing
        | otherwise = Just g
      where
        kingless = g & gamePieces . turnSide . kings %~ (`clearBit` 2)
    -- for castling things
    kingOrigin = case game ^. gameTurn of
        White -> 4
        Black -> 60

    turnSide :: Lens' (Sides a) a
    oppSide :: Lens' (Sides a) a
    turnSide = sideByColor (game ^. gameTurn)
    oppSide = sideByntColor (game ^. gameTurn)
