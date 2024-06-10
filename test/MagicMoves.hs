module MagicMoves
  ( bishopMagicsSpec,
    rookMagicsSpec,
  )
where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Trout.Game.MoveGen.Sliding.Classic
import Trout.Game.MoveGen.Sliding.Magic

-- checks that sliding and magic movegen make the same moves
bishopMagicsSpec :: Spec
bishopMagicsSpec = describe "bishopMovesMagic" $
  prop "should return the same values as bishopMovesClassic" $
    forAll (chooseInt (0, 63)) $
      \sq block ->
        bishopMovesMagic block sq
          `shouldBe` bishopMovesClassic block sq

rookMagicsSpec :: Spec
rookMagicsSpec = describe "rookMovesMagic" $
  prop "should return the same values as rookMovesClassic" $
    forAll (chooseInt (0, 63)) $
      \sq block ->
        rookMovesMagic block sq
          `shouldBe` rookMovesClassic block sq
