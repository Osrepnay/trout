module Trout.Game
    ( Pieces(..)
    , CanCastle(..)
    , CanEnPassant(..)
    , SideInfo(..)
    , Game(..)
    , startingGame
    , turnSide, turnOtherSide
    , allMoves
    , inCheck
    ) where

import           Data.Foldable
import           Data.Vector     (Vector, (!))
import qualified Data.Vector     as V
import           Trout.Bitboard
import           Trout.MoveGen
import           Trout.MoveGen.Sliding.Magic
import           Trout.PieceInfo

newtype Pieces = Pieces { unPieces :: Vector Bitboard }
data CanCastle = CanCastle
    { canCastleKing  :: Bool
    , canCastleQueen :: Bool
    }
newtype CanEnPassant = CanEnPassant { unCanEnPassant :: Maybe Int }
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
    (SideInfo (Pieces (V.fromList
        [ 65280
        , 66
        , 36
        , 129
        , 8
        , 16
        ])) (CanCastle True True))
    (SideInfo (Pieces (V.fromList
        [ 71776119061217280
        , 4755801206503243776
        , 2594073385365405696
        , 9295429630892703744
        , 576460752303423488
        , 1152921504606846976
        ])) (CanCastle True True))
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

moveGens :: Game -> Bitboard -> Bitboard -> Vector (Int -> [Move])
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
    ]

piecesBlockers :: Pieces -> Bitboard
piecesBlockers (Pieces ps) = foldl' (.|.) 0 ps

gameBlockers :: Game -> Bitboard
gameBlockers game = piecesBlockers (sidePieces (turnSide game))
    .|. piecesBlockers (sidePieces (turnOtherSide game))

allMoves :: Game -> [Move]
allMoves game = foldl' (++) [] $
    concat . uncurry fmap
    <$> V.zip (moveGens game block myBlock) (toSqs <$> unPieces (sidePieces (turnSide game)))
  where
    -- TODO update incrementally
    block = myBlock .|. piecesBlockers (sidePieces (turnOtherSide game))
    myBlock = piecesBlockers (sidePieces (turnSide game))

-- like moveGens but bitboards instead
checkMasks :: Color -> Bitboard -> Vector (Vector Bitboard)
checkMasks White block = V.fromList
    [ pawnWhiteAttackTable
    , knightTable
    , bishopMovesMagic block <$> V.fromList [0..63]
    , rookMovesMagic block <$> V.fromList [0..63]
    , (\s -> bishopMovesMagic block s .|. rookMovesMagic block s) <$> V.fromList [0..63]
    , kingTable
    ]
checkMasks Black block = V.fromList
    [ pawnBlackAttackTable
    , knightTable
    , bishopMovesMagic block <$> V.fromList [0..63]
    , rookMovesMagic block <$> V.fromList [0..63]
    , (\s -> bishopMovesMagic block s .|. rookMovesMagic block s) <$> V.fromList [0..63]
    , kingTable
    ]

-- TODO make movegen have a bitboard thing so we dont have to loop
-- simplified movegen with only possible king attacks?
inCheck :: Game -> Bool
inCheck game = foldl' (.|.) 0
    (V.zipWith (.&.) checkMasksKing (unPieces (sidePieces (turnOtherSide game))))
    /= 0
  where
    kingMask = unPieces (sidePieces (turnSide game)) ! king
    kingSq = countTrailingZeros kingMask
    checkMasksKing = (! kingSq) <$> checkMasks (gameTurn game) (gameBlockers game)

-- makeMove :: Game -> Move -> Maybe Game
-- makeMove game move = 
