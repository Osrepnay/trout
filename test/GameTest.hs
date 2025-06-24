module GameTest
  ( addRemoveGetPieceTest,
  )
where

import Test.Hspec
import Trout.Game
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
