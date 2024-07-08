module Trout.Game.Zobrists
  ( castleZobrists,
    enPassantZobrists,
    pieceZobrists,
    playingZobrist,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Bits ((!<<.), (!>>.), (.^.))
import Data.Vector.Primitive (Vector, (!))
import Data.Vector.Primitive qualified as V
import Trout.Piece (Color (..), PieceType (..))

-- NEED 64 BIT INT!!! (probably) FOR GOOD RESULTS!!!!!

xorshiftState :: State Int Int
xorshiftState = state $
  \s0 ->
    let s1 = s0 .^. (s0 !<<. 13)
        s2 = s1 .^. (s1 !>>. 7)
        s3 = s2 .^. (s2 !<<. 17)
     in (s3, s3)

-- all of the zobrists, cut it up later
-- easier to generate all of them in one go
allOfThem :: Vector Int
allOfThem =
  V.fromList $
    evalState
      (replicateM (12 * 64 + 16 + 1 + 8) xorshiftState)
      69420

allPieceZobrists :: Vector Int
allPieceZobrists = V.slice 0 (2 * 6 * 64) allOfThem

castleZobrists :: Vector Int
castleZobrists = V.slice (12 * 64) 16 allOfThem

-- is white?
playingZobrist :: Int
playingZobrist = allOfThem ! (12 * 64 + 16)

enPassantZobrists :: Vector Int
enPassantZobrists = V.slice (12 * 64 + 16 + 1) 8 allOfThem

pieceZobrists :: Color -> PieceType -> Vector Int
pieceZobrists color pieceType = V.slice ((fromEnum color + fromEnum pieceType * 2) * 64) 64 allPieceZobrists
