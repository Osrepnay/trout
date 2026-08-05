{-# OPTIONS_GHC -Wno-orphans #-}

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Trans.Reader (runReaderT)
import Criterion.Main
import Data.Int (Int16)
import Data.Maybe
import Foreign.Storable (sizeOf)
import Trout.Game
import Trout.Search
import Trout.Search.TranspositionTable (TTEntry)

instance NFData SearchEnv where
  rnf !_ = ()

main :: IO ()
main = do
  -- probably better way to do this (env), cba read docs
  defaultMain
    [ bench "bestMove depth 10" $
        perRunEnv
          createEnv
          (bestMoveWrapper 10 startingGame),
      bench "perft(5)" $ whnf (perft 5) startingGame
    ]

createEnv :: IO SearchEnv
createEnv = newEnv (16000000 `quot` sizeOf (undefined :: TTEntry))

bestMoveWrapper :: Int16 -> Game -> SearchEnv -> IO Int
bestMoveWrapper depth game searchEnv =
  flip runReaderT searchEnv $
    last . fmap fst <$> traverse (`bestMove` game) [1 .. depth]

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game =
  sum $
    perft (depth - 1)
      <$> mapMaybe (makeMove game) (allMoves (gameBoard game))
