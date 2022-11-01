module Trout.Game
    ( Pieces(..)
    , CanCastle(..)
    , CanEnPassant(..)
    , SideInfo(..)
    , Game(..)
    , startingGame
    , turnSide
    , allMoves
    ) where

import           Data.Foldable
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Trout.Bitboard
import           Trout.MoveGen
import           Trout.PieceInfo

newtype Pieces = Pieces { unPieces :: Vector Bitboard }
data CanCastle = CanCastle
    { canCastleKing :: Bool
    , canCastleQueen :: Bool
    }
newtype CanEnPassant = CanEnPassant { unCanEnPassant :: Maybe Int }
data SideInfo = SideInfo
    { sidePieces :: Pieces
    , sideCanCastle :: CanCastle
    }
data Game = Game
    { gameWhite :: SideInfo
    , gameBlack :: SideInfo
    , gameEnPassant :: CanEnPassant
    , gameTurn :: Color
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

turnSide :: Game -> SideInfo
turnSide (Game w _ _ White) = w
turnSide (Game b _ _ Black) = b

vecMoveGens :: Game -> Bitboard -> Vector (Int -> [Move])
vecMoveGens game block = V.fromList
    [ pawnMoves (unCanEnPassant (gameEnPassant game)) (gameTurn game) block
    , knightMoves block
    , bishopMoves block
    , rookMoves block
    , queenMoves block
    , kingMoves
        (canCastleKing (sideCanCastle (turnSide game)))
        (canCastleQueen (sideCanCastle (turnSide game)))
        block
    ]

allMoves :: Game -> [Move]
allMoves game = foldl' (++) [] $
    concat . uncurry fmap
    <$> V.zip (vecMoveGens game block) (toSqs <$> unPieces (sidePieces (turnSide game)))
  where
    -- TODO update incrementally
    block = foldl' (.|.) 0 (unPieces (sidePieces (gameWhite game)))
        .|. foldl' (.|.) 0 (unPieces (sidePieces (gameBlack game)))
