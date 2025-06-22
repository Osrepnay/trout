import Control.Monad.Trans.State.Strict
import Criterion.Main
import Data.Maybe
import Trout.Game
import Trout.Search
import Trout.Search.TranspositionTable (SizedHashMapTT)
import Trout.Search.TranspositionTable qualified as TT

main :: IO ()
main = do
  -- probably better way to do this (env), cba read docs
  let table = TT.sizedHMEmpty 1000000
  defaultMain
    [ bench "perft(5)" $ whnf (perft 5) startingGame,
      bench "bestMove depth 5" $
        whnf (bestMoveWrapper 5 startingGame) table
    ]

bestMoveWrapper :: Int -> Game -> SizedHashMapTT -> Int
bestMoveWrapper depth game = fst . evalState (bestMove depth game)

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game =
  sum $
    perft (depth - 1)
      <$> mapMaybe (makeMove game) (allMoves game)
