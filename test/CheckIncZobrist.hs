module CheckIncZobrist
  ( incZobristSpec,
  )
where

import Data.Hashable (hash)
import Lens.Micro ((^.))
import Test.Hspec (Spec, context, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInt, forAll, listOf, withMaxSuccess)
import Trout.Game (allMoves, hgGame, hgHash, makeMove, mkHGame, startingGame)

incZobristSpec :: Spec
incZobristSpec = describe "makeMove" $
  context "for random series of moves" $
    prop "should have the correct hash" $
      withMaxSuccess 1000 $
        forAll (listOf (chooseInt (0, maxBound))) $
          \nums -> doInts nums (mkHGame startingGame)
  where
    -- uses ints as moves
    doInts (m : ms) hgame = case moved of
      Just hg -> doInts ms hg
      Nothing -> doInts [] hgame
      where
        moved = makeMove hgame (moves !! (m `rem` length moves))
        moves = allMoves (hgame ^. hgGame)
    doInts [] hgame = hgame ^. hgHash == hash (hgame ^. hgGame)
