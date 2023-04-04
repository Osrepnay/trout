import EpdPositions
import MagicMoves
import Test.Hspec

main :: IO ()
main = hspec $ do
    bishopMagicsSpec
    rookMagicsSpec
    epdSpec
