module Trout.Uci (doUci, UciState (..)) where

import Control.Concurrent
    ( MVar
    , ThreadId
    , forkFinally
    , forkIO
    , killThread
    , myThreadId
    , newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    , tryTakeMVar
    )
import Data.Foldable      (foldl')
import Data.Maybe         (fromMaybe)
import Lens.Micro         ((&), (.~), (^.))
import System.IO          (hPutStrLn, stderr)
import Trout.Fen.Parse    (fenToGame)
import Trout.Game
    ( Game (..)
    , Sides
    , allMoves
    , gameTurn
    , makeMove
    , sideBlack
    , sideWhite
    , startingGame
    )
import Trout.Game.Move    (Move (..), SpecialMove (Promotion), uciShowMove)
import Trout.Piece        (Color (..))
import Trout.Search       (bestMove)
import Trout.Uci.Parse
    ( CommGoArg (..)
    , CommPositionInit (..)
    , UciCommand (..)
    , UciMove (..)
    , readUciLine
    )

data UciState = UciState
    { uciGame         :: Game
    , uciIsDebug      :: Bool
    , uciSearchThread :: Maybe ThreadId
    } deriving (Show)

data GoSettings = GoSettings
    { goMovetime :: Maybe Int
    , goTimes    :: Sides Int
    , goIncs     :: Sides Int
    , goMaxDepth :: Int
    } deriving (Show)

niceDays :: Int
niceDays = 69 * 24 * 60 * 60 * 1000

defaultSettings :: GoSettings
defaultSettings = GoSettings
    { goMovetime = Nothing
    , goTimes    = (niceDays, niceDays)
    , goIncs     = (0, 0)
    , goMaxDepth = niceDays
    }

launchGo :: MVar ThreadId -> Game -> GoSettings -> IO ()
launchGo goIdVar game (GoSettings movetime times _incs maxDepth) = do
    thisId <- myThreadId
    latestMove <- newEmptyMVar
    goThread <- forkFinally
        (searches 0 latestMove)
        (const (reportAndKill latestMove thisId))
    putMVar goIdVar goThread
    threadDelay (time * 1000)
    killThread goThread
  where
    reportAndKill moveVar thread = do
        moveMaybe <- tryTakeMVar moveVar
        let move = maybe "0000" uciShowMove moveMaybe
        putStrLn ("bestmove " ++ move)
        killThread thread
    searches depth moveVar
        | depth <= maxDepth = do
            let (score, move) = bestMove depth game
            _ <- tryTakeMVar moveVar
            putMVar moveVar move
            putStrLn ("info depth " ++ show depth ++ " score cp " ++ show score)
            searches (depth + 1) moveVar
        | otherwise = pure ()
    time = flip fromMaybe movetime
        $ case game ^. gameTurn of
            White -> times ^. sideWhite `quot` 20
            Black -> times ^. sideBlack `quot` 20

doUci :: UciState -> IO ()
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
        Right (CommPosition posInit moves) ->
            let ng =
                    case posInit of
                        PositionStartpos -> startingGame
                        PositionFen fen  -> fenToGame fen
            in
                case playMoves ng moves of
                    Left err -> do
                        hPutStrLn stderr err
                        doUci uciState
                    Right game -> do
                        doUci (uciState { uciGame = game })
        Right (CommGo args) -> do
            goVar <- newEmptyMVar
            _ <- forkIO
                $ launchGo
                    goVar
                    (uciGame uciState)
                    (foldl' (&) defaultSettings (doGoArg <$> args))
            goId <- takeMVar goVar
            doUci
                (uciState
                    { uciSearchThread = Just goId })
        Right CommStop -> case uciSearchThread uciState of
            Just searchId -> do
                killThread searchId
                doUci uciState
            Nothing -> doUci uciState
        Right CommQuit -> pure ()
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
    doGoArg arg gs@(GoSettings mt ts is depth) =
        case arg of
            GoSearchMoves _ -> gs
            GoPonder        -> gs
            GoWtime t       -> GoSettings mt (ts & sideWhite .~ t) is depth
            GoWinc i        -> GoSettings mt ts (is & sideWhite .~ i) depth
            GoBtime t       -> GoSettings mt (ts & sideBlack .~ t) is depth
            GoBinc i        -> GoSettings mt ts (is & sideBlack .~ i) depth
            GoMovestogo _   -> gs
            GoDepth d       -> GoSettings mt ts is d
            GoNodes _       -> gs
            GoMate _        -> gs
            GoMovetime m    -> GoSettings (Just m) ts is depth
            GoInfinite      -> GoSettings (Just niceDays) ts is depth
