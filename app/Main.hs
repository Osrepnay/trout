module Main (main) where
import Trout.Game (startingHGame)
import Trout.Uci  (UciState (UciState), doUci)

main :: IO ()
main = doUci (UciState startingHGame False Nothing Nothing)
