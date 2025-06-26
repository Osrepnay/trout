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
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)
import Trout.Fen.Parse (fenToGame)
import Trout.Game
  ( Game (..),
    allMoves,
    boardTurn,
    gameBoard,
    makeMove,
    startingGame,
  )
import Trout.Game.Move
  ( Move (..),
    SpecialMove (Promotion),
    uciShowMove,
  )
import Trout.Piece (Color (..))
import Trout.Search (SearchEnv, bestMove, newEnv)
import Trout.Search.TranspositionTable ()
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
    uciSearchEnv :: Maybe (MVar (SearchEnv RealWorld))
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
  let move = maybe "0000" uciShowMove moveMaybe
  putStrLn ("bestmove " ++ move)
  hFlush stdout

launchGo :: MVar Move -> MVar (SearchEnv RealWorld) -> Game -> GoSettings -> IO ()
launchGo moveVar ssVar game (GoSettings movetime times incs maxDepth) = do
  _ <- timeout (time * 1000) (searches 0)
  reportMove moveVar
  where
    searches depth
      | depth <= maxDepth = do
          stateVec <- readMVar ssVar
          (score, move, stateVecPost) <- stToIO $ do
            (score, move) <-
              runReaderT
                (bestMove depth game)
                stateVec
            pure (score, move, stateVec)
          _ <- evaluate score
          _ <- tryTakeMVar moveVar
          putMVar moveVar move
          _ <- swapMVar ssVar stateVecPost
          putStrLn ("info depth " ++ show depth ++ " score cp " ++ show score)
          hFlush stdout
          searches (depth + 1)
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
      -- TODO reset transposition table
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
      ssVar <- case uciSearchEnv uciState of
        Just x -> pure x
        Nothing -> do
          v <- newEmptyMVar
          tt <- stToIO (newEnv 1000000)
          _ <- putMVar v tt
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
              uciSearchEnv = Just ssVar
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
