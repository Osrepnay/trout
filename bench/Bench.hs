import Control.Monad.Trans.State.Strict
import Criterion.Main
import Data.Maybe
import Trout.Game
import Trout.Search
import Trout.Search.TranspositionTable

main :: IO ()
main = do
  -- probably better way to do this (env), cba read docs
  table <- newTT 1000000
  defaultMain
    [ bench "perft(5)" $ whnf (perft 5) startingGame,
      bench "bestMove depth 5" $
        whnfIO (bestMoveWrapper 5 startingGame table)
    ]

bestMoveWrapper :: Int -> Game -> TranspositionTable -> IO Int
bestMoveWrapper depth game table = do
  res <- evalStateT (bestMove depth game) (SearchState table)
  clearTT table
  pure (fst res)

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game =
  sum $
    perft (depth - 1)
      <$> mapMaybe (makeMove game) (allMoves game)
