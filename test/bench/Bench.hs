import Control.DeepSeq
import Criterion.Main
import Trout.Game
import Trout.MoveGen

instance NFData Move where
  -- too excessive? idk
  rnf (Move p s f t) = p `seq` s `seq` f `seq` t `seq` ()

main :: IO ()
main = defaultMain [bench "starting position move generation" $ nf allMoves startingGame]
