module Main (main) where

import Data.Vector
import Trout.Bitboard
import Trout.MoveGen.Sliding.Classic
import Trout.MoveGen.Sliding.Magic

main :: IO ()
main = do
    putStrLn (showBitboard $ bit 0)
    putStrLn ""
    putStrLn (showBitboard (rookMasks ! 0))
    putStrLn ""
    putStrLn (showBitboard (rookMasks ! 30))
