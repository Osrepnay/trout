module Trout.Uci (doUci, UciState (..), newUciState) where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    killThread,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    swapMVar,
    tryReadMVar,
    tryTakeMVar,
  )
import Control.Exception (evaluate)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readEither)
import Trout.Fen.Parse (fenToGame)
import Trout.Game
  ( Game (..),
    allMoves,
    gameBoard,
    makeMove,
    startingGame,
  )
import Trout.Game.Board (boardTurn)
import Trout.Game.Move
  ( Move (..),
    SpecialMove (Promotion),
    uciShowMove,
  )
import Trout.Piece (Color (..))
import Trout.Search (SearchEnv, bestMove, clearEnv, getNodecount, newEnv, pvWalk)
import Trout.Search.TranspositionTable (sizeOfEntry)
import Trout.Uci.Parse
  ( CommGoArg (..),
    CommPositionInit (..),
    UciCommand (..),
    UciMove (..),
    readUciLine,
  )

data UciState = UciState
  { uciGame :: Game,
    uciIsDebug :: Bool,
    uciSearch :: Maybe (ThreadId, MVar Move),
    uciSearchEnv :: MVar (SearchEnv RealWorld)
  }

newUciState :: IO UciState
newUciState =
  UciState startingGame False Nothing
    <$> (stToIO (newEnv (16000000 `quot` sizeOfEntry)) >>= newMVar)

modUciStateHash :: Int -> UciState -> IO UciState
modUciStateHash hashMB state = do
  newSearchEnv <- stToIO (newEnv (hashMB * 1000000 `quot` sizeOfEntry))
  var <- newMVar newSearchEnv
  pure $ state {uciSearchEnv = var}

data PlayerTime = PlayerTime
  { playerTime :: Int,
    playerInc :: Int
  }
  deriving (Eq, Show)

data GoSettings = GoSettings
  { goMovetime :: Maybe Int,
    goTimes :: (Int, Int),
    goIncs :: (Int, Int),
    goMaxDepth :: Int16
  }
  deriving (Show)

defaultSettings :: GoSettings
defaultSettings =
  GoSettings
    { goMovetime = Nothing,
      goTimes = (maxBound, maxBound),
      goIncs = (0, 0),
      goMaxDepth = maxBound
    }

reportMove :: MVar Move -> IO ()
reportMove moveVar = do
  moveMaybe <- tryTakeMVar moveVar
  let move = uciShowMove (fromMaybe NullMove moveMaybe)
  putStrLn ("bestmove " ++ move)
  hFlush stdout

launchGo :: MVar Move -> MVar (SearchEnv RealWorld) -> Game -> GoSettings -> IO ()
launchGo moveVar ssVar game (GoSettings movetime times incs maxDepth) = do
  startTime <- getCurrentTime
  _ <- timeout (time * 999) (searches startTime 1)
  reportMove moveVar
  where
    searches startTime depth
      | depth <= maxDepth = do
          stateEnv <- readMVar ssVar
          (score, move) <- stToIO (runReaderT (bestMove depth game) stateEnv)
          _ <- evaluate score
          _ <- tryTakeMVar moveVar
          putMVar moveVar move
          _ <- swapMVar ssVar stateEnv
          pv <- stToIO (runReaderT (pvWalk game) stateEnv)
          let pvMoves = foldr (\a str -> ' ' : (uciShowMove a ++ str)) "" pv
          let pvStr =
                if pvMoves == ""
                  then ""
                  else " pv" ++ pvMoves
          nodes <- stToIO (runReaderT getNodecount stateEnv)
          currTime <- getCurrentTime
          let elapsedSecs = max 0.000000000001 $ nominalDiffTimeToSeconds $ diffUTCTime currTime startTime
          let elapsedMs :: Int = round (elapsedSecs * 1000)
          let nps = round (fromIntegral nodes / elapsedSecs) :: Int
          printf
            "info depth %d score cp %d time %d nodes %d nps %d%s\n"
            depth
            score
            elapsedMs
            nodes
            nps
            pvStr
          hFlush stdout
          searches startTime (depth + 1)
      | otherwise = pure ()
    time = fromMaybe (getter times `quot` 20 + getter incs `quot` 2) movetime
    getter = case boardTurn (gameBoard game) of
      White -> fst
      Black -> snd

doUci :: UciState -> IO ()
doUci uciState = do
  line <- getLine
  let command = readUciLine line
  case command of
    Right CommUci -> do
      putStrLn "id name Trout"
      putStrLn "id author Osrepnay"
      putStrLn $ "option name Hash type spin default 16 min 1 max " ++ show (maxBound :: Int)
      putStrLn "uciok"
      hFlush stdout
      doUci uciState
    Right (CommDebug debug) -> doUci (uciState {uciIsDebug = debug})
    Right CommDont -> do
      putStrLn "miss the annual ShredderChess Annual Barbeque"
      hFlush stdout
      doUci uciState
    Right CommIsready -> do
      -- make sure movegen is working/initialized
      -- throw it here because... it's good enough
      -- could use whole search but there isn't a lot to initialize there relatively speaking
      allMoves (gameBoard startingGame) `seq` pure ()

      putStrLn "readyok"
      hFlush stdout
      doUci uciState
    Right (CommSetoption name value) -> do
      uciState' <- case name of
        "Hash" -> case readEither value of
          Left err -> do
            hPutStrLn stderr err
            pure uciState
          Right hashMB -> modUciStateHash hashMB uciState
        _ -> do
          hPutStrLn stderr $ "option not supported" ++ name
          hFlush stderr
          pure uciState
      doUci uciState'
    Right (CommRegister _) -> doUci uciState
    Right CommUcinewgame -> do
      let envMVar = uciSearchEnv uciState
      maybeEnv <- tryReadMVar envMVar
      case maybeEnv of
        Just env -> stToIO (clearEnv env)
        Nothing -> pure ()
      doUci $
        uciState {uciGame = startingGame}
    Right (CommPosition posInit moves) ->
      let ng = case posInit of
            PositionStartpos -> startingGame
            PositionFen fen -> fenToGame fen
       in case playMoves ng moves of
            Left err -> do
              hPutStrLn stderr err
              hFlush stderr
              doUci uciState
            Right game -> do
              doUci (uciState {uciGame = game})
    Right (CommGo args) -> do
      goVar <- newEmptyMVar
      let ssVar = uciSearchEnv uciState
      thread <-
        forkIO $
          launchGo
            goVar
            ssVar
            (uciGame uciState)
            (foldl' (&) defaultSettings (doGoArg <$> args))
      doUci
        ( uciState
            { uciSearch = Just (thread, goVar),
              uciSearchEnv = ssVar
            }
        )
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
      (move : _) -> case makeMove g move of
        Just ng -> playMoves ng ms
        Nothing -> Left "illegal move"
      [] -> Left "ILLEGAL move"
      where
        moveMatches (Move _ (Promotion p) f t) =
          Just p == promote
            && f == from
            && t == to
        moveMatches (Move _ _ f t) = f == from && t == to
        gMoves = filter moveMatches (allMoves (gameBoard g))
    doGoArg arg gs@(GoSettings mt ts is depth) = case arg of
      GoSearchMoves _ -> gs
      GoPonder -> gs
      GoWtime t -> GoSettings mt (first (const t) ts) is depth
      GoWinc i -> GoSettings mt ts (first (const i) is) depth
      GoBtime t -> GoSettings mt (second (const t) ts) is depth
      GoBinc i -> GoSettings mt ts (second (const i) is) depth
      GoMovestogo _ -> gs
      GoDepth d -> GoSettings mt ts is d
      GoNodes _ -> gs
      GoMate _ -> gs
      GoMovetime m -> GoSettings (Just m) ts is depth
      GoInfinite -> GoSettings (Just maxBound) ts is depth
