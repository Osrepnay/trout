import CountMoves
import MagicMoves
import OtherMoves
import Test.Hspec

main :: IO ()
main = hspec $ do
    bishopMagicsSpec
    rookMagicsSpec
    pawnSpec
    kingSpec
    moveCounterSpec
