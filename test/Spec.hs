import CheckIncZobrist
import EpdPositions
import MagicMoves
import Test.Hspec

main :: IO ()
main = hspec $ do
  bishopMagicsSpec
  rookMagicsSpec
  incZobristSpec
  epdSpec
