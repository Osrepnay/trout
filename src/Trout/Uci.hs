module Trout.Uci (doUci, UciState (..)) where

import Control.Concurrent
    ( MVar
    , ThreadId
    , forkIO
    , killThread
    , newEmptyMVar
    , putMVar
    , tryTakeMVar
    )
import Control.Exception  (evaluate)
import Data.Foldable      (foldl')
import Data.Maybe         (fromMaybe)
import Lens.Micro         ((&), (.~), (^.))
import System.IO          (hPutStrLn, stderr)
import System.Timeout     (timeout)
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
    { uciGame    :: Game
    , uciIsDebug :: Bool
    , uciSearch  :: Maybe (ThreadId, MVar Move)
    }

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

reportMove :: MVar Move -> IO ()
reportMove moveVar = do
    moveMaybe <- tryTakeMVar moveVar
    let move = maybe "0000" uciShowMove moveMaybe
    putStrLn ("bestmove " ++ move)

launchGo :: MVar Move -> Game -> GoSettings -> IO ()
launchGo moveVar game (GoSettings movetime times _incs maxDepth) = do
    _ <- timeout (time * 1000) (searches 0)
    reportMove moveVar
  where
    searches depth
        | depth <= maxDepth = do
            (score, move) <- evaluate (bestMove depth game)
            _ <- tryTakeMVar moveVar
            putMVar moveVar move
            putStrLn ("info depth " ++ show depth ++ " score cp " ++ show score)
            searches (depth + 1)
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
            thread <- forkIO
                $ launchGo
                    goVar
                    (uciGame uciState)
                    (foldl' (&) defaultSettings (doGoArg <$> args))
            doUci
                (uciState
                    { uciSearch = Just (thread, goVar) })
        Right CommStop -> case uciSearch uciState of
            Just (searchId, moveVar) -> do
                killThread searchId
                reportMove moveVar
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
