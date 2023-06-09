module CheckIncZobrist
    ( incZobristSpec
    ) where
import Test.Hspec (Spec, describe, context)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, listOf, chooseInt)
import Trout.Game (makeMove, hgGame, allMoves, startingGame, mkHGame, hgHash)
import Lens.Micro ((^.))
import Data.Hashable (hash)

incZobristSpec :: Spec
incZobristSpec = describe "makeMove"
    $ context "for random series of moves"
    $ prop "should have the correct hash"
    $ forAll (listOf (chooseInt (0, maxBound)))
    $ \nums -> doInts nums (mkHGame startingGame)
  where
    -- uses ints as moves
    doInts (m : ms) hgame = case moved of
        Just hg -> doInts ms hg
        Nothing -> doInts [] hgame
      where
        moved = makeMove hgame (moves !! (m `rem` length moves))
        moves = allMoves (hgame ^. hgGame)
    doInts [] hgame = hgame ^. hgHash == hash (hgame ^. hgGame)
