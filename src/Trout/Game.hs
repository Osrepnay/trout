{-# LANGUAGE TemplateHaskell #-}
module Trout.Game
    ( Pieces (..)
    , pawns, knights, bishops, rooks, queens, kings
    , CanCastle (..)
    , canCastleKing, canCastleQueen
    , Sides
    , sideWhite, sideBlack, sideByColor
    , Game (..)
    , gamePieces, gamePieces'
    , gameCastling, gameCastling'
    , gameEnPassant, gameTurn
    , gameAsBoard
    , startingGame
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Function                    ((&))
import Data.Vector.Primitive            ((!))
import Lens.Micro                       (Lens', (%~), (.~), (?~), (^.), Lens, (<&>))
import Lens.Micro.TH                    (makeLenses)
import Trout.Bitboard
    ( Bitboard
    , bit
    , blocked
    , clearBit
    , complement
    , countTrailingZeros
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

sideWhite :: Lens' (Sides a) a
sideWhite afb (a, b) = (, b) <$> afb a
{-# INLINE sideWhite #-}

sideBlack :: Lens' (Sides a) a
sideBlack afb (a, b) = (a, ) <$> afb b
{-# INLINE sideBlack #-}

sideByColor :: Lens (Sides a, Color) (Sides a) a a
sideByColor afb ((a, b), White) = (, b) <$> afb a
sideByColor afb ((a, b), Black) = (a, ) <$> afb b
{-# INLINE sideByColor #-}

sideByntColor :: Lens (Sides a, Color) (Sides a) a a
sideByntColor afb ((a, b), Black) = (, b) <$> afb a
sideByntColor afb ((a, b), White) = (a, ) <$> afb b
{-# INLINE sideByntColor #-}

data Game = Game
    { _gamePieces    :: Sides Pieces
    , _gameCastling  :: Sides CanCastle
    , _gameEnPassant :: !(Maybe Int)
    , _gameTurn      :: !Color
    } deriving (Eq, Show)
makeLenses ''Game

gamePieces' :: Lens Game Game (Sides Pieces, Color) (Sides Pieces)
gamePieces' afb game@(Game {_gamePieces = p, _gameTurn = t}) = afb (p, t)
    <&> \b -> game {_gamePieces = b}

gameCastling' :: Lens Game Game (Sides CanCastle, Color) (Sides CanCastle)
gameCastling' afb game@(Game {_gameCastling = c, _gameTurn = t}) = afb (c, t)
    <&> \b -> game {_gameCastling = b}

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

flipTurn :: Game -> Game
flipTurn (Game w b enP White) = Game w b enP Black
flipTurn (Game w b enP Black) = Game w b enP White

gameBlockers :: Game -> Bitboard
gameBlockers game =
    piecesAll (game ^. gamePieces' . sideByColor)
    .|. piecesAll (game ^. gamePieces' . sideByntColor)

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
    kingside = game ^. gameCastling' . sideByColor . canCastleKing
    queenside = game ^. gameCastling' . sideByColor . canCastleQueen
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover piece = concatMap
        (mover block myBlock)
        (toSqs (turnPieces ^. piece))
    -- TODO update incrementally
    block = myBlock .|. piecesAll oppPieces
    myBlock = piecesAll turnPieces
    turnPieces = game ^. gamePieces' . sideByColor
    oppPieces = game ^. gamePieces' . sideByntColor

squareAttacked :: Int -> Game -> Bool
squareAttacked sq game = pawnAttackTable ! sq .&. oppPieces ^. pawns /= 0
    || knightTable ! sq .&. oppPieces ^. knights /= 0
    || bishoped .&. oppPieces ^. bishops /= 0
    || rooked .&. oppPieces ^. rooks /= 0
    || bishoped .&. oppPieces ^. queens /= 0
    || rooked .&. oppPieces ^. queens /= 0
    || kingTable ! sq .&. oppPieces ^. kings /= 0
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    block = gameBlockers game
    pawnAttackTable = case game ^. gameTurn of
        White -> pawnWhiteAttackTable
        Black -> pawnBlackAttackTable
    oppPieces = game ^. gamePieces' . sideByntColor

inCheck :: Game -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise    = squareAttacked kingSq game
  where
    kingSq = countTrailingZeros kingMask
    kingMask = game ^. gamePieces' . sideByColor . kings

makeMove :: Game -> Move -> Maybe Game
makeMove game (Move piece special from to) = do
    let moveAndCaptured = captureAll to (doMove piece from to game)
    afterSpecials <- specials moveAndCaptured
    checkChecked <- nothingIfCheck afterSpecials
    let castleCleared = clearCastles checkChecked
    pure (flipTurn castleCleared)
  where
    -- basic moving
    doMove p fsq tsq = gamePieces'
        . sideByColor
        . byPiece p
        %~ (`setBit` tsq) . (`clearBit` fsq)
    captureAll sq = gamePieces'
        . sideByntColor
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
            King -> gameCastling' . sideByColor .~ CanCastle False False
            _    -> id
    nothingIfCheck g = if inCheck g then Nothing else Just g
    specials = case special of
        PawnDouble -> Just . (gameEnPassant ?~ to)
        EnPassant enPSq -> Just
            . clearEnPassant
            . (gamePieces'
                . sideByntColor
                . pawns
                %~ (`clearBit` enPSq))
        Promotion promote -> Just
            . clearEnPassant
            . (gamePieces'
                . sideByColor
                . byPiece promote
                %~ (`setBit` to))
            . (gamePieces'
                . sideByColor
                . pawns
                %~ (`clearBit` to))
        CastleKing ->
            fmap (clearEnPassant
                . (gamePieces'
                    . sideByColor
                    . rooks
                    %~ (`setBit` (kingOrigin + 1))
                    . (`clearBit` (kingOrigin + 3))))
            . throughCheckKing
        CastleQueen ->
            fmap (clearEnPassant
                . (gamePieces'
                    . sideByColor
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
        kingless = g & gamePieces' . sideByColor . kings %~ (`clearBit` 6)
    throughCheckQueen g
        | squareAttacked (kingOrigin - 1) kingless = Nothing
        | squareAttacked kingOrigin kingless = Nothing
        | otherwise = Just g
      where
        kingless = g & gamePieces' . sideByColor . kings %~ (`clearBit` 2)
    -- for castling things
    kingOrigin = case game ^. gameTurn of
        White -> 4
        Black -> 60
