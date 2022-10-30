module Trout.Game
    ( Pieces(..)
    , CanCastle(..)
    , CanEnPassant(..)
    , SideInfo(..)
    , Game(..)
    ) where

import           Trout.Bitboard
import           Trout.PieceInfo

newtype Pieces = Pieces [Bitboard]
data CanCastle = CanCastle Bool Bool -- kingside, queenside
newtype CanEnPassant = CanEnPassant (Maybe Int)
data SideInfo = SideInfo Pieces CanCastle
data Game = GameOn SideInfo SideInfo Color CanEnPassant | Won Color
