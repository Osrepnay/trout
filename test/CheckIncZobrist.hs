module CheckIncZobrist
  ( incZobristSpec,
  )
where

import Test.Hspec (Spec, context, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInt, forAll, listOf, resize, withMaxSuccess)
import Trout.Game (Game (..), allMoves, boardHash, hashBoard, makeMove, startingGame)

incZobristSpec :: Spec
incZobristSpec = describe "makeMove" $
  context "for random series of moves" $
    prop "should have the correct hash" $
      withMaxSuccess 10000 $
        -- TODO how many moves is appropriate until its just two kings?
        forAll (resize 500 $ listOf $ chooseInt (0, maxBound)) $
          \nums -> doInts nums startingGame
  where
    -- uses ints as moves
    doInts (m : ms) game =
      boardHash board == hashBoard board
        && case moved of
          Just hg -> doInts ms hg
          Nothing -> doInts ms game
      where
        board = gameBoard game
        moved = makeMove game (moves !! (m `rem` length moves))
        moves = allMoves (gameBoard game)
    doInts [] (Game {gameBoard = board}) = boardHash board == hashBoard board
