module Main (main) where

import Trout.Game
import Trout.PieceInfo
import Trout.Search

main :: IO ()
main = do
    print $ bestMove White 6 startingGame
