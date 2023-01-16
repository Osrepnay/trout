module Trout.Game
    ( Pieces (..)
    , CanCastle (..)
    , Side (..)
    , Game (..)
    , gameAsBoard
    , startingGame
    , turnSide, turnOppSide
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Vector.Primitive            ((!))
import Trout.Bitboard
import Trout.Game.Move
import Trout.Game.MoveGen
import Trout.Game.MoveGen.Sliding.Magic
import Trout.PieceInfo

data Pieces = Pieces
    { pawns     :: !Bitboard
    , knights   :: !Bitboard
    , bishops   :: !Bitboard
    , rooks     :: !Bitboard
    , queens    :: !Bitboard
    , kings     :: !Bitboard
    , piecesAll :: !Bitboard
    } deriving (Eq, Show)

modPiece :: Piece -> (Bitboard -> Bitboard) -> Pieces -> Pieces
modPiece p f pieces = modAll $ case p of
    Pawn   -> pieces { pawns   = f (pawns   pieces) }
    Knight -> pieces { knights = f (knights pieces) }
    Bishop -> pieces { bishops = f (bishops pieces) }
    Rook   -> pieces { rooks   = f (rooks   pieces) }
    Queen  -> pieces { queens  = f (queens  pieces) }
    King   -> pieces { kings   = f (kings   pieces) }
  where
    modAll ps = ps { piecesAll = f (piecesAll ps) }
{-# INLINE modPiece #-}

data CanCastle = CanCastle
    { canCastleKing  :: !Bool
    , canCastleQueen :: !Bool
    } deriving (Eq, Show)

data Side = Side
    { sidePieces    :: {-# UNPACK #-} !Pieces
    , sideCanCastle :: !CanCastle
    } deriving (Eq, Show)

modPieces :: (Pieces -> Pieces) -> Side -> Side
modPieces f side = side { sidePieces = f (sidePieces side) }
{-# INLINE modPieces #-}

modCanCastle :: (CanCastle -> CanCastle) -> Side -> Side
modCanCastle f side = side { sideCanCastle = f (sideCanCastle side) }

data Game = Game
    { gameWhite     :: {-# UNPACK #-} !Side
    , gameBlack     :: {-# UNPACK #-} !Side
    , gameEnPassant :: !(Maybe Int)
    , gameTurn      :: !Color
    } deriving (Eq, Show)

gameAsBoard :: Game -> String
gameAsBoard game = unlines [[posChar x y | x <- [0..7]] | y <- [7, 6..0]]
  where
    posChar x y = fst $ head $ filter (\(_, b) -> blocked b sq)
        [ ('P', pawns whitePieces)
        , ('N', knights whitePieces)
        , ('B', bishops whitePieces)
        , ('R', rooks whitePieces)
        , ('Q', queens whitePieces)
        , ('K', kings whitePieces)
        , ('p', pawns blackPieces)
        , ('n', knights blackPieces)
        , ('b', bishops blackPieces)
        , ('r', rooks blackPieces)
        , ('q', queens blackPieces)
        , ('k', kings blackPieces)
        , ('.', complement zeroBits)
        ]
      where
        whitePieces = sidePieces $ gameWhite game
        blackPieces = sidePieces $ gameBlack game
        sq = xyToSq x y

modWhiteSide :: (Side -> Side) -> Game -> Game
modWhiteSide f game = game { gameWhite = f (gameWhite game) }
{-# INLINE modWhiteSide #-}

modBlackSide :: (Side -> Side) -> Game -> Game
modBlackSide f game = game { gameBlack = f (gameBlack game) }
{-# INLINE modBlackSide #-}

modTurnSide :: (Side -> Side) -> Game -> Game
modTurnSide f game = case gameTurn game of
    White -> game { gameWhite = f (gameWhite game) }
    Black -> game { gameBlack = f (gameBlack game) }
{-# INLINE modTurnSide #-}

modOppSide :: (Side -> Side) -> Game -> Game
modOppSide f game = case gameTurn game of
    Black -> game { gameWhite = f (gameWhite game) }
    White -> game { gameBlack = f (gameBlack game) }
{-# INLINE modOppSide #-}

modTurnPieces :: (Pieces -> Pieces) -> Game -> Game
modTurnPieces f = modTurnSide (modPieces f)
{-# INLINE modTurnPieces #-}

modOppPieces :: (Pieces -> Pieces) -> Game -> Game
modOppPieces f = modOppSide (modPieces f)
{-# INLINE modOppPieces #-}

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    (Side
        (Pieces
            65280
            66
            36
            129
            8
            16
            65535)
        (CanCastle True True))
    (Side
        (Pieces
            71776119061217280
            4755801206503243776
            2594073385365405696
            9295429630892703744
            576460752303423488
            1152921504606846976
            18446462598732840960)
        (CanCastle True True))
    Nothing
    White

flipTurn :: Game -> Game
flipTurn (Game w b enP White) = Game w b enP Black
flipTurn (Game w b enP Black) = Game w b enP White

turnSide :: Game -> Side
turnSide (Game w _ _ White) = w
turnSide (Game _ b _ Black) = b

turnOppSide :: Game -> Side
turnOppSide (Game w _ _ Black) = w
turnOppSide (Game _ b _ White) = b

turnPieces :: Game -> Pieces
turnPieces = sidePieces . turnSide

turnOppPieces :: Game -> Pieces
turnOppPieces = sidePieces . turnOppSide

gameBlockers :: Game -> Bitboard
gameBlockers game = piecesAll (turnPieces game)
    .|. piecesAll (turnOppPieces game)

allMoves :: Game -> [Move]
allMoves game = concat
    [ moveSqs (pawnMoves (gameEnPassant game) (gameTurn game)) pawns
    , moveSqs knightMoves knights
    , moveSqs bishopMoves bishops
    , moveSqs rookMoves rooks
    , moveSqs queenMoves queens
    , moveSqs (kingMoves kingside queenside) kings
    ]
  where
    kingside = canCastleKing $ sideCanCastle $ turnSide game
    queenside = canCastleQueen $ sideCanCastle $ turnSide game
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover piece = concatMap
        (mover block myBlock)
        (toSqs (piece (turnPieces game)))
    -- TODO update incrementally
    block = myBlock .|. piecesAll (turnOppPieces game)
    myBlock = piecesAll (turnPieces game)

squareAttacked :: Int -> Game -> Bool
squareAttacked sq game = pawnAttackTable ! sq .&. pawns thisOppPieces /= 0
    || knightTable ! sq .&. knights thisOppPieces /= 0
    || bishoped .&. bishops thisOppPieces /= 0
    || rooked .&. rooks thisOppPieces /= 0
    || bishoped .&. queens thisOppPieces /= 0
    || rooked .&. queens thisOppPieces /= 0
    || kingTable ! sq .&. kings thisOppPieces /= 0
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    block = gameBlockers game
    pawnAttackTable = case gameTurn game of
        White -> pawnWhiteAttackTable
        Black -> pawnBlackAttackTable
    thisOppPieces = turnOppPieces game

-- use || instead of .|. with a /= 0 at the end for short circuit
inCheck :: Game -> Bool
inCheck game
    | kingSq == 64 = True -- king gone!
    | otherwise    = squareAttacked kingSq game
  where
    kingSq = countTrailingZeros kingMask
    kingMask = kings (turnPieces game)

makeMove :: Game -> Move -> Maybe Game
makeMove game (Move piece special from to) = do
    let moveAndCaptured = captureAll to (doMove piece from to game)
    afterSpecials <- specials moveAndCaptured
    checkChecked <- nothingIfCheck afterSpecials
    let castleCleared = clearCastles checkChecked
    pure (flipTurn castleCleared)
  where
    -- basic moving
    doMove p fsq tsq = modTurnPieces
        (modPiece p ((`setBit` tsq) . (`clearBit` fsq)))
    captureAll sq = modOppPieces
        $ \(Pieces p n b r q k a) -> Pieces
            (p .&. clearMask)
            (n .&. clearMask)
            (b .&. clearMask)
            (r .&. clearMask)
            (q .&. clearMask)
            (k .&. clearMask)
            (a .&. clearMask)
      where
        clearMask = complement (bit sq)
    clearCastles g = modWhiteSide
            (modCanCastle
            (\(CanCastle k q) -> CanCastle
                (k && from /= 7 && to /= 7)
                (q && from /= 0 && to /= 0)))
        $ modBlackSide
            (modCanCastle
            (\(CanCastle k q) -> CanCastle
                (k && from /= 63 && to /= 63)
                (q && from /= 56 && to /= 56)))
        $ case piece of
            King -> modTurnSide
                (\s -> s { sideCanCastle = CanCastle False False })
                g
            _ -> g
    nothingIfCheck g = if inCheck g then Nothing else Just g
    specials g = case special of
        PawnDouble -> Just (g { gameEnPassant = Just to })
        EnPassant enPSq -> Just
            $ clearEnPassant
            $ (modOppSide
                $ modPieces
                $ modPiece Pawn (`clearBit` enPSq)) g
        Promotion promote -> Just
            $ clearEnPassant
            $ modTurnPieces
                (modPiece promote (`setBit` to)
                    . modPiece Pawn (`clearBit` to)) g
        CastleKing ->
            ((<$> throughCheckKing g)
                . (clearEnPassant .))
            $ modTurnPieces
            $ modPiece
                Rook
                ((`setBit` (kingOrigin + 1)) . (`clearBit` (kingOrigin + 3)))
        CastleQueen ->
            ((<$> throughCheckQueen g)
                . (clearEnPassant .))
            $ modTurnPieces
            $ modPiece
                Rook
                ((`setBit` (kingOrigin - 1)) . (`clearBit` (kingOrigin - 4)))
        Normal -> Just (clearEnPassant g)
    clearEnPassant g = g { gameEnPassant = Nothing }
    -- assumes king is moved, rook is not
    throughCheckKing g
        | squareAttacked (kingOrigin + 1) kingless = Nothing
        | squareAttacked kingOrigin kingless = Nothing
        | otherwise                 = Just g
      where
        kingless = modTurnSide (modPieces (modPiece King (`clearBit` 6))) g
    throughCheckQueen g
        | squareAttacked (kingOrigin - 1) kingless = Nothing
        | squareAttacked kingOrigin kingless = Nothing
        | otherwise = Just g
      where
        kingless = modTurnSide (modPieces (modPiece King (`clearBit` 2))) g
    -- for castling things
    kingOrigin = case gameTurn game of
        White -> 4
        Black -> 60
