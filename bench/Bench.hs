import Criterion.Main
import Data.Maybe
import Trout.Game

main :: IO ()
main = defaultMain [bench "perft(5)" $ whnf (perft 5) startingGame]

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves game)
