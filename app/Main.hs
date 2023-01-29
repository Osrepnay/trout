module Main (main) where

import Trout.Game
import Trout.Piece
import Trout.Search

main :: IO ()
main = do
    print $ bestMove White 6 startingGame
