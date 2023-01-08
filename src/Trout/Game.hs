module Trout.Game
    ( Pieces (..)
    , CanCastle (..)
    , Side (..)
    , Game (..)
    , gameAsBoard
    , startingGame
    , turnSide, turnOtherSide
    , allMoves
    , inCheck
    , makeMove
    ) where

import Data.Function
import Data.Vector                      ((!))
import Trout.Bitboard
import Trout.Game.Move
import Trout.Game.MoveGen
import Trout.Game.MoveGen.Sliding.Magic
import Trout.PieceInfo

data Pieces = Pieces
    { pawns     :: Bitboard
    , knights   :: Bitboard
    , bishops   :: Bitboard
    , rooks     :: Bitboard
    , queens    :: Bitboard
    , kings     :: Bitboard
    , piecesAll :: Bitboard
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

data CanCastle = CanCastle
    { canCastleKing  :: Bool
    , canCastleQueen :: Bool
    } deriving (Eq, Show)

data Side = Side
    { sidePieces    :: Pieces
    , sideCanCastle :: CanCastle
    } deriving (Eq, Show)

modPieces :: (Pieces -> Pieces) -> Side -> Side
modPieces f side = side { sidePieces = f (sidePieces side) }

modCanCastle :: (CanCastle -> CanCastle) -> Side -> Side
modCanCastle f side = side { sideCanCastle = f (sideCanCastle side) }

data Game = Game
    { gameWhite     :: Side
    , gameBlack     :: Side
    , gameEnPassant :: Maybe Int
    , gameTurn      :: Color
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

modTurnSide :: (Side -> Side) -> Game -> Game
modTurnSide f game = case gameTurn game of
    White -> game { gameWhite = f (gameWhite game) }
    Black -> game { gameBlack = f (gameBlack game) }

modTurnOtherSide :: (Side -> Side) -> Game -> Game
modTurnOtherSide f game = case gameTurn game of
    Black -> game { gameWhite = f (gameWhite game) }
    White -> game { gameBlack = f (gameBlack game) }

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

turnOtherSide :: Game -> Side
turnOtherSide (Game w _ _ Black) = w
turnOtherSide (Game _ b _ White) = b

turnPieces :: Game -> Pieces
turnPieces = sidePieces . turnSide

turnOtherPieces :: Game -> Pieces
turnOtherPieces = sidePieces . turnOtherSide

gameBlockers :: Game -> Bitboard
gameBlockers game = piecesAll (sidePieces (turnSide game))
    .|. piecesAll (sidePieces (turnOtherSide game))

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
    moveSqs mover piece = concatMap (mover block myBlock) (toSqs (piece (turnPieces game)))
    -- TODO update incrementally
    block = myBlock .|. piecesAll (sidePieces (turnOtherSide game))
    myBlock = piecesAll (sidePieces (turnSide game))

squareAttacked :: Int -> Game -> Bool
squareAttacked sq game = pawnAttackTable ! sq .&. pawns thisOtherPieces /= 0
    || knightTable ! sq .&. knights thisOtherPieces /= 0
    || bishoped .&. bishops thisOtherPieces /= 0
    || rooked .&. rooks thisOtherPieces /= 0
    || bishoped .&. queens thisOtherPieces /= 0
    || rooked .&. queens thisOtherPieces /= 0
  where
    bishoped = bishopMovesMagic block sq
    rooked = rookMovesMagic block sq
    block = gameBlockers game
    pawnAttackTable = case gameTurn game of
        White -> pawnWhiteAttackTable
        Black -> pawnBlackAttackTable
    thisOtherPieces = turnOtherPieces game

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
    doMove p fsq tsq = modTurnSide
        $ modPieces
        $ modPiece p ((`setBit` tsq) . (`clearBit` fsq))
    captureAll sq = modTurnOtherSide
        $ modPieces
        $ modPiece Pawn clearTarget
        . modPiece Knight clearTarget
        . modPiece Bishop clearTarget
        . modPiece Rook clearTarget
        . modPiece Queen clearTarget
        . modPiece King clearTarget
      where
        clearTarget = (`clearBit` sq)
    clearCastles g = case piece of
        Rook -> g
            & if from == 0
            then clearCastleSide False
            else if from == 7
            then clearCastleSide True
            else if from == 56
            then clearCastleSide False
            else if from == 63
            then clearCastleSide True
            else id
        King -> modTurnSide
            (\s -> s { sideCanCastle = CanCastle False False })
            g
        _ -> g
    clearCastleSide isKingside = modTurnSide
        $ modCanCastle
        $ \(CanCastle k q) ->
            if isKingside
            then CanCastle False q
            else CanCastle k False
    nothingIfCheck g = if inCheck g then Nothing else Just g
    specials g = case special of
        PawnDouble -> Just $ g { gameEnPassant = Just to }
        EnPassant enPSq -> Just $ clearEnPassant $ (modTurnOtherSide
            $ modPieces
            $ modPiece Pawn (`clearBit` enPSq)) g
        Promotion promote -> Just $ clearEnPassant $ (modTurnSide
            $ modPieces
            $ modPiece promote (`setBit` to)
            . modPiece Pawn (`clearBit` to)) g
        Castle isKingside ->
            ((<$> castleChecks isKingside g)
                . (clearEnPassant .))
            $ modTurnSide
            $ modPieces
            $ if isKingside
            then
                modPiece
                    Rook
                    ((`setBit` byTurn 5 61) . (`clearBit` byTurn 7 63))
                . \ps -> ps { kings = byTurn 64 4611686018427387904 }
            else
                modPiece
                    Rook
                    ((`setBit` byTurn 3 59) . (`clearBit` byTurn 0 56))
                . \ps -> ps { kings = byTurn 4 288230376151711744}
        Normal -> Just $ clearEnPassant g
    byTurn w b = case gameTurn game of
        White -> w
        Black -> b
    clearEnPassant g = g { gameEnPassant = Nothing }
    -- assumes king is moved, rook is not
    castleChecks True  = castleChecksKing
    castleChecks False = castleChecksQueen
    castleChecksKing g
        | squareAttacked 5 kingless = Nothing
        | otherwise                 = Just g
      where
        kingless = (modTurnSide $ modPieces $ modPiece King (`clearBit` 6)) g
    castleChecksQueen g
        | squareAttacked 3 kingless = Nothing
        | otherwise                 = Just g
      where
        kingless = (modTurnSide $ modPieces $ modPiece King (`clearBit` 2)) g
