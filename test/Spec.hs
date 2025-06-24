import GameTest
import CheckIncZobrist
import EpdPositions
import MagicMoves
import Test.Hspec

main :: IO ()
main = hspec $ do
  addRemoveGetPieceTest
  drawTest
  bishopMagicsSpec
  rookMagicsSpec
  incZobristSpec
  epdSpec
