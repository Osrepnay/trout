import MagicMoves
import Test.Hspec

main :: IO ()
main = hspec $
    describe "magic moves generation" $ do
        bishopMagicsSpec
        rookMagicsSpec
