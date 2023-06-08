import Criterion.Main
import Data.Maybe
import Lens.Micro
import Trout.Game

main :: IO ()
main = defaultMain [bench "perft(5)" $ whnf (perft 5) (mkHGame startingGame)]

perft :: Int -> HGame -> Int
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves (game ^. hgGame))
