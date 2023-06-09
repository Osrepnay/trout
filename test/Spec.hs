import EpdPositions
import MagicMoves
import Test.Hspec
import CheckIncZobrist

main :: IO ()
main = hspec $ do
    bishopMagicsSpec
    rookMagicsSpec
    incZobristSpec
    epdSpec
