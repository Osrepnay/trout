module Main (main) where

import Data.Text.IO qualified as TIO
import PgnParse (parsePgns, playPgn)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)
import Tuner (calcSigmoidK, newTunables, sgdStep, Tunables (..), steppin)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import qualified Data.Vector.Primitive as PV

main :: IO ()
main = do
  let tunables = newTunables
  filename <- head <$> getArgs
  fileContents <- TIO.readFile filename
  let parsed = parse parsePgns filename fileContents
  case parsed of
    Left err -> putStrLn (errorBundlePretty err)
    Right pgnGames -> do
      let allGames = pgnGames >>= playPgn
      let generator = mkStdGen 69
      let shuffledGames = shuffle' allGames (length allGames) generator
      print $ foldr seq () shuffledGames
      print $ length shuffledGames
      let k = calcSigmoidK tunables shuffledGames
      print k
      putStrLn "k"
      let stepped = steppin tunables shuffledGames k 50000 10
      print stepped
      pure ()
