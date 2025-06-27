{-# OPTIONS_GHC -Wno-orphans #-}
import Control.DeepSeq (NFData (rnf))
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans.Reader (runReaderT)
import Criterion.Main
import Data.Maybe
import Trout.Game
import Trout.Search

instance NFData (SearchEnv s) where
  rnf !_ = ()

main :: IO ()
main = do
  -- probably better way to do this (env), cba read docs
  defaultMain
    [ bench "perft(5)" $ whnf (perft 5) startingGame,
      bench "bestMove depth 6" $
        perRunEnv
          createVec
          (bestMoveWrapper 6 startingGame)
    ]

createVec :: IO (SearchEnv RealWorld)
createVec = stToIO (newEnv 200000)

bestMoveWrapper :: Int -> Game -> SearchEnv RealWorld -> IO Int
bestMoveWrapper depth game vec = stToIO (fst <$> runReaderT (bestMove depth game) vec)

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game =
  sum $
    perft (depth - 1)
      <$> mapMaybe (makeMove game) (allMoves (gameBoard game))
