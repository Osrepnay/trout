module Main (main) where

import Data.Char
import Data.Maybe
import System.Environment
import Trout.Bitboard
import Trout.Game
import Trout.Game.Move
import Trout.PieceInfo

import Debug.Trace

main :: IO ()
main = do
    {- let depth = read (head args)
    let fenGame = parseFen (args !! 1)
    let fenGameWithMoves = mapMaybe
            (\m -> (\g -> (m, g)) <$> makeMove fenGame m)
            (allMoves fenGame)
    mapM_
        (\(m, g) -> putStrLn (showMove m ++ " " ++ show (perft (depth - 1) g)))
        fenGameWithMoves -}
    {- let (Just afterMoves) = foldl
            (\b a -> b >>= flip makeMove a)
            (Just startingGame)
            [ Move Knight Normal (parseCoord "g1") (parseCoord "h3")
            , Move Knight Normal (parseCoord "g8") (parseCoord "h6")
            , Move Pawn Normal (parseCoord "e2") (parseCoord "e3")
            , Move Pawn Normal (parseCoord "b7") (parseCoord "b6")
            , Move Bishop Normal (parseCoord "f1") (parseCoord "a6")
            , Move Bishop Normal (parseCoord "c8") (parseCoord "a6")
            ]
    let withMoves = mapMaybe
            (\m -> (\g -> (m, g)) <$> makeMove afterMoves m)
            (allMoves afterMoves)
    mapM_
        (\(m, g) -> putStrLn (showMove m ++ ": " ++ show (perft 1 g)))
        withMoves -}
    -- print $ perft 5 startingGame
    print $ allMoves startingGame

showMove :: Move -> String
showMove (Move _ special from to) = case special of
    Promotion promote -> case promote of
        Knight -> fromToShowed ++ "N"
        Bishop -> fromToShowed ++ "B"
        Rook -> fromToShowed ++ "R"
        Queen -> fromToShowed ++ "Q"
        _ -> error "can't promote there! stupid!!!!!"
    _ -> fromToShowed
  where
    fromToShowed = unparseCoord from ++ unparseCoord to

perft :: Int -> Game -> Int
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
        _ -> error "unknown color"
    whiteCastles = CanCastle
        ('K' `elem` fenSections !! 2)
        ('Q' `elem` fenSections !! 2)
    blackCastles = CanCastle
        ('k' `elem` fenSections !! 2)
        ('q' `elem` fenSections !! 2)
    enPassant
        | fenSections !! 3 == "-" = Nothing
        | otherwise               = Just (parseCoord (fenSections !! 3))

parseCoord :: String -> Int
parseCoord coordStr = file + row * 8
  where
    file = ord (head coordStr) - ord 'a'
    row = ord (coordStr !! 1) - ord '0' - 1

unparseCoord :: Int -> String
unparseCoord sq = file : show row
  where
    file = chr ((sq `rem` 8) + ord 'a')
    row = sq `quot` 8 + 1

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn on splitee = go "" splitee
  where
    go thisSection [] = [reverse thisSection]
    go thisSection (c : s)
        | c == on   = reverse thisSection : go [] s
        | otherwise = go (c : thisSection) s

