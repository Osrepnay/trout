module CountMoves
    ( startMovesSpec
    ) where

import Test.Hspec
import Trout.Game

startMovesSpec :: Spec
startMovesSpec = describe "allMoves" $
    context "at starting position" $
        it "should return the right amount of moves" $
            allMoves startingGame `shouldSatisfy` ((== 8 + 8 + 2 + 2) . length)
