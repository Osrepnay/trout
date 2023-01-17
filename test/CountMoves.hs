module CountMoves
    ( moveCounterSpec
    ) where

import Data.Foldable
import Data.List.Split
import Data.Maybe
import Test.Hspec
import Trout.Game

startMovesSpec :: Spec
startMovesSpec = describe "allMoves"
    $ context "at starting position"
    $ it "should return the right amount of moves"
    $ allMoves startingGame `shouldSatisfy` ((== 8 + 8 + 2 + 2) . length)

perftSpec :: Spec
perftSpec = describe "makeMove"
    $ xit "should return the right results for perft"
    $ perft 6 startingGame `shouldBe` 119060324

perft :: Int -> Game -> Int -- shut up ghc
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves game)

readDepthEpd :: String -> IO [(Game, [(Int, Int)])]
readDepthEpd filename = do
    file <- readFile filename
    let fileLines = lines file
    pure (parseLine <$> fileLines)
  where
    -- performance? what's that?
    trimSpaces = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    parseLine line = (game, depths)
      where
        game = fromFen (head parts)
        depths = depth <$> tail parts
        depth ('D' : dc : ' ' : p) = (read [dc], read p)
        depth _                    = error "not a depth"
        parts = trimSpaces <$> splitOn ";" line

epdSpec :: Spec
epdSpec = describe "movegen"
    $ context "for epd positions"
    $ do
        let epdFilename = "test/epd/hartmann.epd"
        posses <- runIO (readDepthEpd epdFilename)
        let withLines = zip [(1 :: Int)..] posses
        traverse_
            (\(l, p) ->
                it
                    ("should return the right results for perft at line "
                        ++ show l)
                    $ uncurry perftDepths p)
            withLines
  where
    perftDepths g ds = flip perft g <$> depths `shouldBe` correct
      where
        (depths, correct) = unzip ds

moveCounterSpec :: Spec
moveCounterSpec = do
    startMovesSpec
    perftSpec
    parallel epdSpec
