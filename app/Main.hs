module Main (main) where

import Trout.Uci (doUci, newUciState)

main :: IO ()
main = newUciState >>= doUci
