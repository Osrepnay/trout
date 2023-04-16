{-# LANGUAGE TemplateHaskell #-}
module Trout.Game
    ( Pieces (..)
    , pawns, knights, bishops, rooks, queens, kings, byPiece
    , Sides
    , sideWhite, sideBlack
    , Game (..)
    , gamePlaying, gameWaiting, gameCastling, gameEnPassant, gameTurn
    , gamePieces
    , gameCastling'
    , gameAsBoard
    , startingGame
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Bool                        (bool)
import Data.Foldable                    (foldl')
import Data.Hashable                    (Hashable (..))
import Data.Vector.Primitive            (unsafeIndex)
import Lens.Micro
    ( Lens
    , Lens'
    , (%~)
    , (.~)
    , (<&>)
    , (?~)
    , (^.)
    )
import Lens.Micro.TH                    (makeLenses)
import Trout.Bitboard
    ( Bitboard
    , bit
    , clearBit
    , complement
    , countTrailingZeros
    , fileA
    , fileH
    , setBit
    , testBit
    , toSqs
    , xor
    , xyToSq
    , zeroBits
    , (!<<.)
    , (!>>.)
    , (.&.)
    , (.|.)
    )
import Trout.Game.MoveGen
    ( Move (..)
    , SpecialMove (..)
    , bishopMoves
    , concatDL
    , kingMoves
    , kingTable
    , knightMoves
    , knightTable
    , mapOnes
    , pawnsMoves
    , queenMoves
    , rookMoves
    )
import Trout.Game.MoveGen.Sliding.Magic (bishopMovesMagic, rookMovesMagic)
import Trout.Game.Zobrists
    ( blackBishopZobrists
    , blackKingZobrists
    , blackKnightZobrists
    , blackPawnZobrists
    , blackQueenZobrists
    , blackRookZobrists
    , castleZobrists
    , enPassantZobrists
    , playingZobrist
    , whiteBishopZobrists
    , whiteKingZobrists
    , whiteKnightZobrists
    , whitePawnZobrists
    , whiteQueenZobrists
    , whiteRookZobrists
    )
import Trout.Piece                      (Color (..), Piece (..), other)

data Pieces = Pieces
    { _pawns   :: !Bitboard
    , _knights :: !Bitboard
    , _bishops :: !Bitboard
    , _rooks   :: !Bitboard
    , _queens  :: !Bitboard
    , _kings   :: !Bitboard
    } deriving (Eq, Show)
makeLenses ''Pieces

instance Hashable Game where
    hash :: Game -> Int
    hash (Game play wait castle enP turn) = turnHash
        `xor` castleHash
        `xor` enPassantHash
        `xor` hashBitboard (white ^. pawns) whitePawnZobrists
        `xor` hashBitboard (black ^. pawns) blackPawnZobrists
        `xor` hashBitboard (white ^. knights) whiteKnightZobrists
        `xor` hashBitboard (black ^. knights) blackKnightZobrists
        `xor` hashBitboard (white ^. bishops) whiteBishopZobrists
        `xor` hashBitboard (black ^. bishops) blackBishopZobrists
        `xor` hashBitboard (white ^. rooks) whiteRookZobrists
        `xor` hashBitboard (black ^. rooks) blackRookZobrists
        `xor` hashBitboard (white ^. queens) whiteQueenZobrists
        `xor` hashBitboard (black ^. queens) blackQueenZobrists
        `xor` hashBitboard (white ^. kings) whiteKingZobrists
        `xor` hashBitboard (black ^. kings) blackKingZobrists
      where
        hashBitboard bb table = foldl'
            (\b a -> table `unsafeIndex` a `xor` b)
            0
            (toSqs bb)
        (white, black, turnHash) = case turn of
            White -> (play, wait, playingZobrist)
            Black -> (wait, play, 0)
        -- only first 4 bits of castle should be used
        castleHash = castleZobrists `unsafeIndex` castle
        enPassantHash = case enP of
            Just sq -> enPassantZobrists `unsafeIndex` (sq `rem` 8)
            Nothing -> 0

    hashWithSalt :: Int -> Game -> Int
    hashWithSalt salt game = hash game `xor` salt


-- ghc gets mad when i use the normal lens typedef
byPiece :: Functor f => Piece -> (Bitboard -> f Bitboard) -> Pieces -> f Pieces
byPiece Pawn   = pawns
byPiece Knight = knights
byPiece Bishop = bishops
byPiece Rook   = rooks
byPiece Queen  = queens
byPiece King   = kings
{-# INLINE byPiece #-}

piecesAll :: Pieces -> Bitboard
piecesAll (Pieces p n b r q k) = p .|. n .|. b .|. r .|. q .|. k
{-# INLINE piecesAll #-}

type Sides a = (a, a)

sideWhite :: Lens' (Sides a) a
sideWhite afb (a, b) = (, b) <$> afb a
{-# INLINE sideWhite #-}

sideBlack :: Lens' (Sides a) a
sideBlack afb (a, b) = (a, ) <$> afb b
{-# INLINE sideBlack #-}

data Game = Game
    { _gamePlaying   :: Pieces
    , _gameWaiting   :: Pieces
    , _gameCastling  :: !Int
    , _gameEnPassant :: !(Maybe Int)
    , _gameTurn      :: !Color
    } deriving (Eq, Show)
makeLenses ''Game

gamePieces :: Lens Game Game (Sides Pieces) (Sides Pieces)
gamePieces afb game@(Game p w _ _ t) = case t of
    White -> afb (p, w)
        <&> \(p', w') -> game {_gamePlaying = p', _gameWaiting = w'}
    Black -> afb (w, p)
        <&> \(w', p') -> game {_gamePlaying = w', _gameWaiting = p'}
{-# INLINE gamePieces #-}

gameCastling' :: Lens Game Game (Int, Color) Int
gameCastling' afb game@(Game {_gameCastling = c, _gameTurn = t}) = afb (c, t)
    <&> \b -> game {_gameCastling = b}
{-# INLINE gameCastling' #-}

-- masks home rows by color
maskByColor :: Lens (Int, Color) Int Int Int
maskByColor afb (bb, White) = (.|. (bb .&. complement 3))
    . (.&. 3)
    <$> afb (bb .&. 3)
maskByColor afb (bb, Black) = (.|. (bb .&. complement 12))
    . (.&. 12)
    <$> afb (bb .&. 12)
{-# INLINE maskByColor #-}

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    (Pieces
        65280
        66
        36
        129
        8
        16)
    (Pieces
        71776119061217280
        4755801206503243776
        2594073385365405696
        9295429630892703744
        576460752303423488
        1152921504606846976)
    15
    Nothing
    White

gameAsBoard :: Game -> String
gameAsBoard game = unlines [[posChar x y | x <- [0..7]] | y <- [7, 6..0]]
  where
    posChar x y = fst $ head $ filter (\(_, b) -> b `testBit` sq)
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
flipTurn (Game p w c enP t) = Game w p c enP (other t)
{-# INLINE flipTurn #-}

gameBlockers :: Game -> Bitboard
gameBlockers game =
    piecesAll (game ^. gamePlaying)
    .|. piecesAll (game ^. gameWaiting)
{-# INLINE gameBlockers #-}

allMoves :: Game -> [Move]
allMoves game =
    pawnsMoves
        (game ^. gameEnPassant)
        (game ^. gameTurn)
        block
        myBlock
        p
    $ concatDL
        (moveSqs knightMoves n
            $ moveSqs bishopMoves b
            $ moveSqs rookMoves r
            $ moveSqs queenMoves q
            $ moveSqs (kingMoves kingside queenside) k [])
        []
  where
    kingside = 0 /= 10 .&. thisCastling
    queenside = 0 /= 5 .&. thisCastling
    thisCastling = game ^. gameCastling' . maskByColor
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover = mapOnes (mover block myBlock)
    block = myBlock .|. piecesAll (game ^. gameWaiting)
    myBlock = piecesAll turnPieces
    turnPieces@(Pieces p n b r q k) = game ^. gamePlaying

squareAttacked :: Bitboard -> Int -> Game -> Bool
squareAttacked block sq game = knightTable `unsafeIndex` sq .&. n
    .|. bishoped .&. b
    .|. rooked .&. r
    .|. bishoped .&. q
    .|. rooked .&. q
    .|. kingTable `unsafeIndex` sq .&. k
    /= 0
    || pawnMask `testBit` sq
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    pawnMask = case game ^. gameTurn of
        White -> (p .&. complement fileA) !>>. 9
            .|. (p .&. complement fileH) !>>. 7
        Black -> (p .&. complement fileA) !<<. 7
            .|. (p .&. complement fileH) !<<. 9
    (Pieces p n b r q k) = game ^. gameWaiting

inCheck :: Game -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise    = squareAttacked (gameBlockers game) kingSq game
  where
    kingSq = countTrailingZeros kingMask
    kingMask = game ^. gamePlaying . kings

makeMove :: Game -> Move -> Maybe Game
makeMove game (Move piece special from to) = do
    let moveAndCaptured = captureAll (doMove game)
    afterSpecials <- specials moveAndCaptured
    checkChecked <- nothingIfCheck afterSpecials
    pure (flipTurn (clearCastles checkChecked))
  where
    -- basic moving
    doMove = gamePlaying
        . byPiece piece
        %~ (`setBit` to) . (`clearBit` from)
    captureAll = gameWaiting
        %~ \(Pieces p n b r q k) -> Pieces
            (p .&. clearMask)
            (n .&. clearMask)
            (b .&. clearMask)
            (r .&. clearMask)
            (q .&. clearMask)
            (k .&. clearMask)
      where
        clearMask = complement (bit to)
    clearCastles = gameCastling
        %~ (complement
            (case piece of
                King -> case game ^. gameTurn of
                    White -> 3
                        .|. bool 0 4 (from == 56 || to == 56)
                        .|. bool 0 8 (from == 63 || to == 63)
                    Black -> 12
                        .|. bool 0 1 (from == 0 || to == 0)
                        .|. bool 0 2 (from == 7 || to == 7)
                _ -> bool 0 1 (from == 0 || to == 0)
                    .|. bool 0 2 (from == 7 || to == 7)
                    .|. bool 0 4 (from == 56 || to == 56)
                    .|. bool 0 8 (from == 63 || to == 63)) .&.)
    nothingIfCheck g = if inCheck g then Nothing else Just g
    specials = case special of
        PawnDouble -> Just . (gameEnPassant ?~ to)
        EnPassant enPSq -> Just
            . clearEnPassant
            . (gameWaiting . pawns %~ (`clearBit` enPSq))
        Promotion promote -> Just
            . clearEnPassant
            . (gamePlaying . byPiece promote %~ (`setBit` to))
            . (gamePlaying . pawns %~ (`clearBit` to))
        CastleKing ->
            fmap (clearEnPassant
                . (gamePlaying
                    . rooks
                    %~ (`setBit` (kingOrigin + 1))
                    . (`clearBit` (kingOrigin + 3))))
            . throughCheckKing
        CastleQueen ->
            fmap (clearEnPassant
                . (gamePlaying
                    . rooks
                    %~ (`setBit` (kingOrigin - 1))
                    . (`clearBit` (kingOrigin - 4))))
            . throughCheckQueen
        Normal -> Just . clearEnPassant
    clearEnPassant = gameEnPassant .~ Nothing
    -- assumes king is moved, rook is not
    throughCheckKing g
        | squareAttacked kingless (kingOrigin + 1) g = Nothing
        | squareAttacked kingless kingOrigin g = Nothing
        | otherwise = Just g
      where
        kingless = gameBlockers g `clearBit` 6
    throughCheckQueen g
        | squareAttacked kingless (kingOrigin - 1) g = Nothing
        | squareAttacked kingless kingOrigin g = Nothing
        | otherwise = Just g
      where
        kingless = gameBlockers g `clearBit` 2
    -- for castling things
    kingOrigin = case game ^. gameTurn of
        White -> 4
        Black -> 60
