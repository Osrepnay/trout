module CountMoves
    ( moveCounterSpec
    ) where

import Data.Maybe
import Test.Hspec
import Trout.Game
import Trout.Game.Move

import Trout.PieceInfo

import Debug.Trace

startMovesSpec :: Spec
startMovesSpec = describe "allMoves" $
    context "at starting position" $
        it "should return the right amount of moves" $
            allMoves startingGame `shouldSatisfy` ((== 8 + 8 + 2 + 2) . length)

perftSpec :: Spec
perftSpec = describe "makeMove" $
    it "should return the right results for perft" $
        perft 7 startingGame `shouldBe` 3195901860
  where
    perft :: Int -> Game -> Int -- shut up ghc
    perft 0 _ = 1
    perft depth game = sum $ perft (depth - 1) <$> mapMaybe (makeMove game) (allMoves game)

moveCounterSpec :: Spec
moveCounterSpec = do
    startMovesSpec
    perftSpec
