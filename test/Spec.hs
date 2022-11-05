import CountMoves
import MagicMoves
import OtherMoves
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "magic moves generation" $ do
        bishopMagicsSpec
        rookMagicsSpec
    describe "pawn moves" $ do
        pawnSpec
        kingSpec
    startMovesSpec
