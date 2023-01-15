module CountMoves
    ( moveCounterSpec
    ) where

import Data.Char
import Data.Foldable
import Data.Maybe
import Test.Hspec
import Trout.Bitboard
import Trout.Game
import Trout.PieceInfo

startMovesSpec :: Spec
startMovesSpec = describe "allMoves"
    $ context "at starting position"
    $ it "should return the right amount of moves"
    $ allMoves startingGame `shouldSatisfy` ((== 8 + 8 + 2 + 2) . length)

perftSpec :: Spec
perftSpec = describe "makeMove"
    $ xit "should return the right results for perft"
    $ perft 6 startingGame `shouldBe` 119060324

perft :: Int -> Game -> Int -- shut up ghc
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves game)

parseFen :: String -> Game
parseFen fen = Game
    { gameWhite = Side
        { sidePieces = Pieces
            { pawns = bbBy (== 'P')
            , knights = bbBy (== 'N')
            , bishops = bbBy (== 'B')
            , rooks = bbBy (== 'R')
            , queens = bbBy (== 'Q')
            , kings = bbBy (== 'K')
            , piecesAll = bbBy isUpper
            }
        , sideCanCastle = whiteCastles
        }
    , gameBlack = Side
        { sidePieces = Pieces
            { pawns = bbBy (== 'p')
            , knights = bbBy (== 'n')
            , bishops = bbBy (== 'b')
            , rooks = bbBy (== 'r')
            , queens = bbBy (== 'q')
            , kings = bbBy (== 'k')
            , piecesAll = bbBy isLower
            }
        , sideCanCastle = blackCastles
        }
    , gameEnPassant = enPassant
    , gameTurn = color
    }
  where
    bbBy f = fromSqs $ fst <$> filter (f . snd) flatBoard
    fenSections = words fen
    flatBoard = zip [0..]
        $ concat
        $ reverse
        $ (>>= (\c -> if isDigit c then replicate (read [c]) ' ' else [c]))
        <$> splitOn '/' (head fenSections)
    color = case fenSections !! 1 of
        "w" -> White
        "b" -> Black
        _   -> error "unknown color"
    whiteCastles = CanCastle
        ('K' `elem` fenSections !! 2)
        ('Q' `elem` fenSections !! 2)
    blackCastles = CanCastle
        ('k' `elem` fenSections !! 2)
        ('q' `elem` fenSections !! 2)
    enPassant
        | fenSections !! 3 == "-" = Nothing
        | otherwise               = Just (parseCoord (fenSections !! 3))

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
        game = parseFen (head parts)
        depths = depth <$> tail parts
        depth ('D' : dc : ' ' : p) = (read [dc], read p)
        depth _ = error "not a depth"
        parts = trimSpaces <$> splitOn ';' line

parseCoord :: String -> Int
parseCoord coordStr = file + row * 8
  where
    file = ord (head coordStr) - ord 'a'
    row = ord (coordStr !! 1) - ord '0' - 1

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn on splitee = go "" splitee
  where
    go thisSection [] = [reverse thisSection]
    go thisSection (c : s)
        | c == on   = reverse thisSection : go [] s
        | otherwise = go (c : thisSection) s

epdSpec :: Spec
epdSpec = describe "movegen"
    $ context "for epd positions"
    $ do
        let epdFilename = "test/epd/hartmann.epd"
        posses <- runIO (readDepthEpd epdFilename)
        let withLines = zip [(1 :: Int)..] posses
        traverse_
            (\(l, p) ->
                it
                    ("should return the right results for perft at line "
                        ++ show l)
                    $ uncurry perftDepths p)
            withLines
  where
    perftDepths g ds = flip perft g <$> depths `shouldBe` correct
      where
        (depths, correct) = unzip ds

moveCounterSpec :: Spec
moveCounterSpec = do
    startMovesSpec
    perftSpec
    epdSpec
