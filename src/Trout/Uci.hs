module Trout.Uci (doUci) where

import Control.Concurrent (ThreadId, killThread)
import System.IO          (hPutStrLn, stderr)
import Trout.Game         (Game (..), startingGame)
import Trout.Uci.Parse    (UciCommand (..), readUciLine)

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
        Right (CommPosition _ _) -> doUci uciState -- TODO
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
