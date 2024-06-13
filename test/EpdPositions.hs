module EpdPositions
  ( epdSpec,
  )
where

import Data.Foldable
import Data.List.Split
import Data.Maybe
import Test.Hspec
import Trout.Fen.Parse
import Trout.Game

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game =
  sum $
    perft (depth - 1)
      <$> mapMaybe (makeMove game) (allMoves game)

readDepthEpd :: String -> IO [(Game, [(Int, Int)])]
readDepthEpd filename = do
  file <- readFile filename
  let fileLines = lines file
  pure (parseLine <$> fileLines)
  where
    -- performance? what's that?
    trimSpaces = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    parseLine line = (game, depths)
      where
        (Right game) = fenToGame <$> readFen (head parts)
        depths = depth <$> tail parts
        depth ('D' : dc : ' ' : p) = (read [dc], read p)
        depth _ = error "not a depth"
        parts = trimSpaces <$> splitOn ";" line

epdSpec :: Spec
epdSpec = parallel $
  describe "movegen" $
    context "for epd positions" $
      do
        let epdFilename = "test/epd/hartmann.epd"
        posses <- runIO (readDepthEpd epdFilename)
        let withLines = zip [(1 :: Int) ..] posses
        traverse_
          ( \(l, p) ->
              it
                ( "should return the right results for perft at line "
                    ++ show l
                )
                $ uncurry perftDepths p
          )
          withLines
  where
    perftDepths g ds = flip perft g <$> depths `shouldBe` correct
      where
        (depths, correct) = unzip ds
