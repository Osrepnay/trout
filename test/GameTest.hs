module GameTest
  ( addRemoveGetPieceTest,
    drawTest
  )
where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Test.Hspec
import Trout.Game
import Trout.Game.Move
import Trout.Piece

-- TODO add comprehensive piece testing?
addRemoveGetPieceTest :: Spec
addRemoveGetPieceTest =
  parallel $
    describe "Pieces" $
      it "should add, remove, and get pieces correctly" $
        let startingPieces = boardPieces (gameBoard startingGame)
            addedPieces = addPiece (Piece White King) 30 startingPieces
            gotPiece = getPiece 30 addedPieces
            removedPieces = removePiece 30 addedPieces
         in removedPieces == startingPieces && gotPiece == Just (Piece White King)

drawTest :: Spec
drawTest =
  parallel $
    describe "isDrawn" $ do
      it "should return true for repetition" $
        let moves =
              [ Move Knight Normal 1 18,
                Move Knight Normal 62 45,
                Move Knight Normal 18 1,
                Move Knight Normal 45 62
              ]
            game =
              fromJust $
                foldl'
                  (\maybeGame move -> maybeGame >>= flip makeMove move)
                  (Just startingGame)
                  moves
         in isDrawn game
      it "should return true for 50 move rule" $
       let game = startingGame {game50MovePlies = 50}
        in isDrawn game
