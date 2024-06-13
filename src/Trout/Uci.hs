module Trout.Uci (doUci, UciState (..)) where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    swapMVar,
    tryTakeMVar,
  )
import Control.Exception (evaluate)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)
import Trout.Fen.Parse (fenToGame)
import Trout.Game
  ( Game (..),
    allMoves,
    gameTurn,
    makeMove,
    startingGame,
  )
import Trout.Game.Move
  ( Move (..),
    SpecialMove (Promotion),
    uciShowMove,
  )
import Trout.Piece (Color (..))
import Trout.Search (SearchState (..), bestMove)
import Trout.Search.TranspositionTable (newTT)
import Trout.Uci.Parse
  ( CommGoArg (..),
    CommPositionInit (..),
    UciCommand (..),
    UciMove (..),
    readUciLine,
  )
import Data.Bifunctor (first, second)
import Data.Function ((&))

data UciState = UciState
  { uciGame :: Game,
    uciIsDebug :: Bool,
    uciSearch :: Maybe (ThreadId, MVar Move),
    uciSearchState :: Maybe (MVar SearchState)
  }

data PlayerTime = PlayerTime
  { playerTime :: Int,
    playerInc :: Int
  }
  deriving (Eq, Show)

data GoSettings = GoSettings
  { goMovetime :: Maybe Int,
    goTimes :: (Int, Int),
    goIncs :: (Int, Int),
    goMaxDepth :: Int
  }
  deriving (Show)

niceDays :: Int
niceDays = 69 * 24 * 60 * 60 * 1000

defaultSettings :: GoSettings
defaultSettings =
  GoSettings
    { goMovetime = Nothing,
      goTimes = (niceDays, niceDays),
      goIncs = (0, 0),
      goMaxDepth = niceDays
    }

reportMove :: MVar Move -> IO ()
reportMove moveVar = do
  moveMaybe <- tryTakeMVar moveVar
  let move = maybe "0000" uciShowMove moveMaybe
  putStrLn ("bestmove " ++ move)
  hFlush stdout

launchGo :: MVar Move -> MVar SearchState -> Game -> GoSettings -> IO ()
launchGo moveVar ssVar game (GoSettings movetime times _incs maxDepth) = do
  _ <- timeout (time * 1000) (searches 0)
  reportMove moveVar
  where
    searches depth
      | depth <= maxDepth = do
          st0 <- readMVar ssVar
          ((score, move), st1) <-
            runStateT
              (bestMove depth game)
              st0
          _ <- evaluate score
          _ <- tryTakeMVar moveVar
          putMVar moveVar move
          _ <- swapMVar ssVar st1
          putStrLn ("info depth " ++ show depth ++ " score cp " ++ show score)
          hFlush stdout
          searches (depth + 1)
      | otherwise = pure ()
    time = flip fromMaybe movetime $
      flip quot 20 $
        case gameTurn game of
          White -> fst times
          Black -> snd times

doUci :: UciState -> IO ()
doUci uciState = do
  line <- getLine
  let command = readUciLine line
  case command of
    Right CommUci -> do
      putStrLn "id name Trout"
      putStrLn "id author Osrepnay"
      putStrLn "uciok"
      hFlush stdout
      doUci uciState
    Right (CommDebug debug) -> doUci (uciState {uciIsDebug = debug})
    Right CommDont -> do
      putStrLn "miss the annual ShredderChess Annual Barbeque"
      hFlush stdout
      doUci uciState
    Right CommIsready -> do
      putStrLn "readyok"
      hFlush stdout
      doUci uciState
    Right (CommSetoption _ _) -> do
      hPutStrLn stderr "option not supported"
      hFlush stderr
      doUci uciState
    Right (CommRegister _) -> doUci uciState
    Right CommUcinewgame ->
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
      ssVar <- case uciSearchState uciState of
        Just x -> pure x
        Nothing -> do
          v <- newEmptyMVar
          tt <- newTT 1000000
          _ <- putMVar v (SearchState tt)
          pure v
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
              uciSearchState = Just ssVar
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
        gMoves = filter moveMatches (allMoves g)
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
      GoInfinite -> GoSettings (Just niceDays) ts is depth
