module Trout.Uci (doUci, UciState (..), newUciState) where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    killThread,
    newEmptyMVar,
    putMVar,
    swapMVar,
    tryTakeMVar,
  )
import Control.Exception (catch, evaluate)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bifunctor (first, second)
import Data.Function ((&))
import Data.Int (Int16)
import Data.Maybe (fromMaybe, listToMaybe)
import Foreign.Storable (sizeOf)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (hPrintf, printf)
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
import Trout.Search (OutOfTime, SearchEnv, bestMove, clearEnv, getNodecount, newEnv, refreshEnv)
import Trout.Search.TranspositionTable (TTEntry)
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
    uciSearchEnv :: SearchEnv
  }

calcNumEntries :: Int -> Int
calcNumEntries hashMB = hashMB * 1000000 `quot` sizeOf (undefined :: TTEntry)

newUciState :: IO UciState
newUciState = UciState startingGame False Nothing <$> newEnv (calcNumEntries 16)

modUciStateHash :: Int -> UciState -> IO UciState
modUciStateHash hashMB state = do
  newSearchEnv <- newEnv (calcNumEntries hashMB)
  pure $ state {uciSearchEnv = newSearchEnv}

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

launchGo :: MVar Move -> SearchEnv -> Game -> GoSettings -> IO ()
launchGo moveVar stateEnv game (GoSettings movetime times incs maxDepth) =
  flip catch (\(_ :: OutOfTime) -> final) $
    do
      startTime <- getMonotonicTimeNSec
      putMVar moveVar NullMove
      searches startTime 1
  where
    final = do
      reportMove moveVar
      runReaderT refreshEnv stateEnv
    searches startTime depth
      | depth <= maxDepth = do
          thisStartTime <- getMonotonicTimeNSec
          let delta = thisStartTime - startTime
          let timeRemaining = if delta > timeNs then 0 else timeNs - delta
          (score, pvLine) <- runReaderT (bestMove timeRemaining depth game) stateEnv
          let move = fromMaybe NullMove (listToMaybe pvLine)
          _ <- evaluate score
          _ <- swapMVar moveVar move
          let pvMoves = foldr (\a str -> ' ' : (uciShowMove a ++ str)) "" pvLine
          let pvStr =
                if pvMoves == ""
                  then ""
                  else " pv" ++ pvMoves
          nodes <- runReaderT getNodecount stateEnv
          currTime <- getMonotonicTimeNSec
          let elapsedNs = max 1 (currTime - startTime)
          let elapsedMs = max 1 (elapsedNs `quot` 1_000_000)
          let elapsedSecs = max 1 (elapsedMs `quot` 1000)
          let nps = fromIntegral nodes `quot` elapsedSecs
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
    timeNs =
      1_000_000
        * fromIntegral
          (fromMaybe (getter times `quot` 20 + getter incs `quot` 2) movetime - 20)
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
          hPrintf stderr "option not supported: \"%s\"\n" name
          hFlush stderr
          pure uciState
      doUci uciState'
    Right (CommRegister _) -> doUci uciState
    Right CommUcinewgame -> do
      clearEnv (uciSearchEnv uciState)
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
      thread <-
        forkIO $
          launchGo
            goVar
            (uciSearchEnv uciState)
            (uciGame uciState)
            (foldl' (&) defaultSettings (doGoArg <$> args))
      doUci
        uciState
          { uciSearch = Just (thread, goVar)
          }
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
