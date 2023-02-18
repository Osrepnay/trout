module Main (main) where
import Trout.Game (startingGame)
import Trout.Uci (doUci)
import Trout.Uci (UciState(UciState))

main :: IO ()
main = doUci (UciState startingGame False Nothing)
