module OtherMoves
    ( pawnSpec
    , kingSpec
    ) where

import Data.Foldable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Trout.Bitboard
import Trout.Game.MoveGen
import Trout.PieceInfo

pawnBlockNoMoveSpec :: Spec
pawnBlockNoMoveSpec = context "when pawn is blocked in front" $
    traverse_ (prop "can't go forward" . forAll (chooseInt (8, 55))) -- how merge props?
        [ \sq -> pawnMoves Nothing White (bit (sq + 8)) 0 sq `shouldBe` []
        , \sq -> pawnMoves Nothing Black (bit (sq - 8)) 0 sq `shouldBe` []
        ]

pawnDoubleMoveSpec :: Spec
pawnDoubleMoveSpec = context "when pawn is on starting row" $
    traverse_ (prop "returns a double move forward")
        [ forAll (chooseInt (8, 15)) $
            \sq -> pawnMoves Nothing White 0 0 sq `shouldContain` [Move Pawn PawnDouble sq (sq + 16)]
        , forAll (chooseInt (48, 55)) $
            \sq -> pawnMoves Nothing Black 0 0 sq `shouldContain` [Move Pawn PawnDouble sq (sq - 16)]
        ]

pawnEnPassantSpec :: Spec
pawnEnPassantSpec = context "when pawn can en passant" $
    traverse_ (prop "returns en passant")
        [ forAll (chooseInt (24 + 1, 31)) $
            -- blockers technically wrong? whatever
            -- also only checks one direction...
            \sq -> pawnMoves (Just (sq - 1)) Black (complement zeroBits) 0 sq `shouldContain`
                [Move Pawn (EnPassant (sq - 1)) sq (sq - 8 - 1)]
        , forAll (chooseInt (32, 39 - 1)) $
            \sq -> pawnMoves (Just (sq + 1)) White (complement zeroBits) 0 sq `shouldContain`
                [Move Pawn (EnPassant (sq + 1)) sq (sq + 8 + 1)]
        ]

pawnSpec :: Spec
pawnSpec = describe "pawnMoves" $ do
    pawnBlockNoMoveSpec
    pawnDoubleMoveSpec
    pawnEnPassantSpec

-- no knight quickcheck because its so simple it would almost just be another movegen
-- individual tests work but dont wanna rn

kingCastleSpec :: Spec
kingCastleSpec = context "when castling is available" $ do
    it "returns castling move for kingside" $
        kingMoves True True 0 0 4 `shouldContain` [Move King (Castle True) 4 6]
    it "returns castling move for queenside" $
        kingMoves True True 0 0 60 `shouldContain` [Move King (Castle False) 60 58]
    it "doesn't return castling when blocked" $
        kingMoves True False (bit 6) 0 4 `shouldSatisfy` ((== 5) . length)

kingSpec :: Spec
kingSpec = describe "kingMoves" kingCastleSpec
