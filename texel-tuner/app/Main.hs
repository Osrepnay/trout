module Main (main) where

import Data.Text.IO qualified as TIO
import PgnParse (parsePgns, playPgn)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)
import Tuner (newTunables, calcSigmoidShape)

main :: IO ()
main = do
  let tunables = newTunables
  filename <- head <$> getArgs
  fileContents <- TIO.readFile filename
  let parsed = parse parsePgns filename fileContents
  case parsed of
    Left err -> putStrLn (errorBundlePretty err)
    Right pgnGames -> do
      let res = pgnGames >>= playPgn
      print $ foldr seq () res
      print $ length res
      print (calcSigmoidShape tunables res)
      pure ()
