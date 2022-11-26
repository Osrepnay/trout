module Trout.Game
    ( Pieces (..)
    , CanCastle (..)
    , CanEnPassant (..)
    , SideInfo (..)
    , Game (..)
    , startingGame
    , turnSide, turnOtherSide
    , allMoves
    , inCheck
    ) where

import           Data.Vector                      ((!))
import           Trout.Bitboard
import           Trout.Game.Move
import           Trout.Game.MoveGen
import           Trout.Game.MoveGen.Sliding.Magic
import           Trout.PieceInfo

data Pieces = Pieces
    { pawns   :: Bitboard
    , knights :: Bitboard
    , bishops :: Bitboard
    , rooks   :: Bitboard
    , queens  :: Bitboard
    , kings   :: Bitboard
    }
data CanCastle = CanCastle
    { canCastleKing  :: Bool
    , canCastleQueen :: Bool
    }
newtype CanEnPassant = CanEnPassant { unEPassant :: Maybe Int }
data SideInfo = SideInfo
    { sidePieces    :: Pieces
    , sideCanCastle :: CanCastle
    }
data Game = Game
    { gameWhite     :: SideInfo
    , gameBlack     :: SideInfo
    , gameEnPassant :: CanEnPassant
    , gameTurn      :: Color
    }

-- https://tearth.dev/bitboard-viewer/
startingGame :: Game
startingGame = Game
    (SideInfo (Pieces
        65280
        66
        36
        129
        8
        16) (CanCastle True True))
    (SideInfo (Pieces
        71776119061217280
        4755801206503243776
        2594073385365405696
        9295429630892703744
        576460752303423488
        1152921504606846976) (CanCastle True True))
    (CanEnPassant Nothing)
    White

flipSide :: Game -> Game
flipSide (Game w b enP White) = Game w b enP Black
flipSide (Game w b enP Black) = Game w b enP White

turnSide :: Game -> SideInfo
turnSide (Game w _ _ White) = w
turnSide (Game _ b _ Black) = b

turnOtherSide :: Game -> SideInfo
turnOtherSide (Game w _ _ Black) = w
turnOtherSide (Game _ b _ White) = b

turnPieces :: Game -> Pieces
turnPieces = sidePieces . turnSide

turnOtherPieces :: Game -> Pieces
turnOtherPieces = sidePieces . turnOtherSide

{- moveGens :: Game -> Bitboard -> Bitboard -> Vector (Int -> [Move])
moveGens game block myBlock = V.fromList
    [ pawnMoves (unCanEnPassant (gameEnPassant game)) (gameTurn game) block myBlock
    , knightMoves block myBlock
    , bishopMoves block myBlock
    , rookMoves block myBlock
    , queenMoves block myBlock
    , kingMoves
        (canCastleKing (sideCanCastle (turnSide game)))
        (canCastleQueen (sideCanCastle (turnSide game)))
        block
        myBlock
    ] -}

-- need... lens....
piecesBlockers :: Pieces -> Bitboard
piecesBlockers (Pieces p n b r q k) = p .|. n .|. b .|. r .|. q .|. k

gameBlockers :: Game -> Bitboard
gameBlockers game = piecesBlockers (sidePieces (turnSide game))
    .|. piecesBlockers (sidePieces (turnOtherSide game))

allMoves :: Game -> [Move]
allMoves game = concat
    [ moveSqs (pawnMoves (unEPassant (gameEnPassant game)) (gameTurn game)) pawns
    , moveSqs knightMoves knights
    , moveSqs bishopMoves bishops
    , moveSqs queenMoves queens
    , moveSqs (kingMoves kingside queenside) kings
    ]
  where
    kingside = canCastleKing $ sideCanCastle $ turnSide game
    queenside = canCastleQueen $ sideCanCastle $ turnSide game
    -- gets and concats the move for a set of squares (for a piece)
    moveSqs mover piece = concatMap (mover block myBlock) (toSqs (piece (turnPieces game)))
    -- TODO update incrementally
    block = myBlock .|. piecesBlockers (sidePieces (turnOtherSide game))
    myBlock = piecesBlockers (sidePieces (turnSide game))

-- use || instead of .|. with a /= 0 at the end for short circuit
inCheck :: Game -> Bool
inCheck game = pawnWhiteAttackTable ! kingSq .&. pawns (sidePieces (turnSide game)) /= 0
    || knightTable ! kingSq .&. knights (sidePieces (turnSide game)) /= 0
    || bishopMovesMagic block kingSq .&. bishops (sidePieces (turnSide game)) /= 0
    || rookMovesMagic block kingSq .&. rooks (sidePieces (turnSide game)) /= 0
    || rookMovesMagic block kingSq .&. rooks (sidePieces (turnSide game)) /= 0
  where
    kingMask = kings (sidePieces (turnSide game))
    kingSq = countTrailingZeros kingMask
    block = gameBlockers game
{-
makeMove :: Move -> Game -> Maybe Game
makeMove move game = case movePiece move of
    Pawn -> 
  where
    -- basic moving n stuff
    doMove game = game {  }
-}
