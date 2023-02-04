module Main (main) where
import Trout.Game (Game, makeMove, allMoves, startingGame)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    print $ perft 5 startingGame

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves game)

