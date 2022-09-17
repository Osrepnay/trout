module Trout.Game
    (
    ) where

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Trout.Bitboard
import           Trout.PieceInfo

newtype PiecesBoards = PiecesBoards [Bitboard]
data Game = GameOn PiecesBoards PiecesBoards | Won Color
