module Trout.Game
    (
    ) where

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Trout.Bitboard
import           Trout.PieceInfo

newtype PiecesBoards = PiecesBoards [Bitboard]
data Game = GameOn PiecesBoards PiecesBoards | Won Color

-- int
-- from piece, from, capture piece, to
newtype Move = Move Int Int (Maybe Int) Int

moves :: PiecesBoards -> PiecesBoards -> [Move]
moves mover block = 