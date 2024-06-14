module CheckIncZobrist
  ( incZobristSpec,
  )
where

import Data.Hashable (hash)
import Test.Hspec (Spec, context, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInt, forAll, listOf, withMaxSuccess)
import Trout.Game (Game (..), allMoves, makeMove, startingGame)

incZobristSpec :: Spec
incZobristSpec = describe "makeMove" $
  context "for random series of moves" $
    prop "should have the correct hash" $
      withMaxSuccess 10000 $
        forAll (listOf (chooseInt (0, maxBound))) $
          \nums -> doInts nums startingGame
  where
    -- uses ints as moves
    doInts (m : ms) game = case moved of
      Just hg -> doInts ms hg
      Nothing -> doInts [] game
      where
        moved = makeMove game (moves !! (m `rem` length moves))
        moves = allMoves game
    doInts [] game = gameHash game == hash game
