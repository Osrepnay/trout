module Main (main) where
import Trout.Game (startingGame)
import Trout.Uci  (UciState (UciState), doUci)

main :: IO ()
main = doUci (UciState startingGame False Nothing)
