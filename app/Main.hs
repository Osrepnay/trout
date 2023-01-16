module Main (main) where

import Data.Char
import Data.Maybe
import Trout.Bitboard
import Trout.Game
import Trout.Game.Move
import Trout.PieceInfo

wrongGame = Game {gameWhite = Side {sidePieces = Pieces {pawns = 0, knights = 0, bishops = 0, rooks = 0, queens = 0, kings = 562949953421312, piecesAll = 562949953421312}, sideCanCastle = CanCastle {canCastleKing = False, canCastleQueen = False}}, gameBlack = Side {sidePieces = Pieces {pawns = 0, knights = 0, bishops = 0, rooks = 72057594037927936, queens = 0, kings = 1152921504606846976, piecesAll = 1224979098644774912}, sideCanCastle = CanCastle {canCastleKing = False, canCastleQueen = True}}, gameEnPassant = Nothing, gameTurn = White}
main :: IO ()
main = do
    let (Just afterMoves) = foldl
            (\b a -> b >>= flip makeMove a)
            (Just wrongGame)
            [
            ]
    let withMoves = mapMaybe
            (\m -> (\g -> (m, g)) <$> makeMove afterMoves m)
            (allMoves afterMoves)
    mapM_
        (\(m, g) -> putStrLn (showMove m ++ ": " ++ show (perft 1 g)))
        withMoves

showMove :: Move -> String
showMove (Move _ special from to) = case special of
    Promotion promote -> case promote of
        Knight -> fromToShowed ++ "N"
        Bishop -> fromToShowed ++ "B"
        Rook   -> fromToShowed ++ "R"
        Queen  -> fromToShowed ++ "Q"
        _      -> error "can't promote there! stupid!!!!!"
    _ -> fromToShowed
  where
    fromToShowed = unparseCoord from ++ unparseCoord to

perft :: Int -> Game -> Int
perft 0 _ = 1
perft depth game = sum
    $ perft (depth - 1)
    <$> mapMaybe (makeMove game) (allMoves game)

unparseCoord :: Int -> String
unparseCoord sq = file : show row
  where
    file = chr ((sq `rem` 8) + ord 'a')
    row = sq `quot` 8 + 1
