module CountMoves
    ( startMovesSpec
    ) where

import Test.Hspec
import Trout.Game

startMovesSpec :: Spec
startMovesSpec = describe "allMoves" $
    context "at starting position" $ 
        it "should return the right amount of moves" $
            allMoves startingGame `shouldSatisfy` ((== 20) . length)
