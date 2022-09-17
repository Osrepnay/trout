module Trout.Game
    (
    ) where

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Trout.Bitboard
import           Trout.PieceInfo

newtype Pieces = PiecesBoards [Bitboard]
data CanCastle = CanCastle Bool Bool -- kingside, queenside
newtype CanEnPassant = CanEnPassant (Maybe Int)
data SideInfo = SideInfo Pieces CanCastle
data Game = GameOn SideInfo SideInfo CanEnPassant | Won Color
