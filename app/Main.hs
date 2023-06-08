module Main (main) where
import Trout.Game (mkHGame, startingGame)
import Trout.Uci  (UciState (UciState), doUci)

main :: IO ()
main = doUci (UciState (mkHGame startingGame) False Nothing Nothing)
