module Trout.Uci (doUci) where

import Control.Concurrent (ThreadId, killThread)
import System.IO          (hPutStrLn, stderr)
import Trout.Game         (Game (..), startingGame, allMoves, makeMove)
import Trout.Uci.Parse
    ( CommPositionInit (..)
    , UciCommand (..)
    , UciMove (..)
    , readUciLine
    )
import Trout.Game.Move (Move(..), SpecialMove (Promotion))
import Data.Maybe (fromMaybe)

data Time = Time
    { timeLeft :: Int
    , timeInc  :: Int
    } deriving (Show)

data UciState = UciState
    { uciGame         :: Game
    , uciIsDebug      :: Bool
    , uciSearchThread :: Maybe ThreadId
    } deriving (Show)

doUci :: UciState -> IO UciState
doUci uciState = do
    line <- getLine
    let command = readUciLine line
    case command of
        Right CommUci -> do
            putStrLn "id name Trout"
            putStrLn "id author Osrepnay"
            putStrLn "uciok"
            doUci uciState
        Right (CommDebug debug) -> doUci (uciState { uciIsDebug = debug })
        Right CommDont -> do
            putStrLn "miss the annual ShredderChess Annual Barbeque"
            doUci uciState
        Right CommIsready -> do
            putStrLn "readyok"
            doUci uciState
        Right (CommSetoption _ _) -> do
            hPutStrLn stderr "option not supported"
            doUci uciState
        Right (CommRegister _) -> doUci uciState
        Right CommUcinewgame -> doUci (uciState { uciGame = startingGame })
        Right (CommPosition posInit moves) -> case posInit of
            PositionStartpos ->
                case playMoves startingGame moves of
                    Left err -> do
                        hPutStrLn stderr err
                        doUci uciState
                    Right game -> do
                        doUci (uciState { uciGame = game })
            PositionFen _    -> doUci uciState -- TODO
        Right (CommGo _) -> doUci uciState -- TODO
        Right CommStop -> case uciSearchThread uciState of
            Just searchId -> do
                killThread searchId
                doUci uciState
            Nothing -> doUci uciState
        Right CommQuit -> pure uciState
        Left err -> do
            hPutStrLn stderr err
            doUci uciState
        _ -> doUci uciState
  where
    playMoves g [] = Right g
    playMoves g ((UciMove from to promote) : ms) = case gMoves of
        (move : _) ->
            case makeMove g move of
                Just ng -> playMoves ng ms
                Nothing -> Left "illegal move"
        [] -> Left "ILLEGAL move"
      where
        moveMatches (Move _ (Promotion p) f t) = Just p == promote
            && f == from
            && t == to
        moveMatches (Move _ _ f t) = f == from && t == to
        gMoves = filter moveMatches (allMoves g)
