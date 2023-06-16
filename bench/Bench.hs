import Control.Monad.Trans.State.Strict
import Criterion.Main
import Data.Maybe
import Lens.Micro
import Trout.Game
import Trout.Search
import Trout.Search.TranspositionTable

main :: IO ()
main = do
    -- probably better way to do this (env), cba read docs
    table <- newTT 1000000
    defaultMain
        [ bench "perft(5)" $ whnf (perft 5) startingHGame
        , bench "bestMove depth 5"
            $ whnfIO (bestMoveWrapper 5 startingHGame table)
        ]

bestMoveWrapper :: Int -> HGame -> TranspositionTable -> IO Int
bestMoveWrapper depth hgame table = do
    res <- evalStateT (bestMove depth hgame) (SearchState table)
    clearTT table
    pure (fst res)

perft :: Int -> HGame -> Int
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves (game ^. hgGame))
