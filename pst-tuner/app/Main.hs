module Main (main) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text.IO qualified as TIO
import Data.Vector.Primitive qualified as PV
import PgnParse (parsePgns, playPgn)
import System.Environment (getArgs)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Printf (printf)
import Tuner (Tunables (..), calcError, calcSigmoidK, newTunables, tunableKingSafety, tunableMobility, tuneEpoch)

formatTunables :: Tunables -> String
formatTunables tunables =
  "piece-square tables:\n"
    ++ intercalate
      "\n\n"
      ( uncurry formatSet
          <$> zip
            [ "pawnMPST",
              "knightMPST",
              "bishopMPST",
              "rookMPST",
              "queenMPST",
              "kingMPST",
              "pawnEPST",
              "knightEPST",
              "bishopEPST",
              "rookEPST",
              "queenEPST",
              "kingEPST"
            ]
            [0 ..]
      )
    ++ "\n"
    ++ "mobility:\n"
    ++ formatMob
    ++ "\n"
    ++ "king safety:\n"
    ++ show safeties
  where
    formatStr =
      intercalate
        "\n"
        [ "%s :: Vector Int",
          "%s = V.fromList $ concat $ reverse",
          "%s"
        ]
    formatSet name nthBoard = printf formatStr name name formattedBoard
      where
        twoDimBoard :: [[Int]]
        twoDimBoard =
          reverse $
            chunksOf 8 $
              fmap round $
                PV.toList $
                  PV.slice (nthBoard * 64) 64 (unTunables tunables)
        formattedBoard = "  [ " ++ intercalate ",\n    " (show <$> twoDimBoard) ++ "\n  ]"

    mobs = tunableMobility tunables
    formatMobStr =
      intercalate
        ",\n"
        [ "[ (Pawn, %f, %f)",
          "  (Knight, %f, %f)",
          "  (Bishop, %f, %f)",
          "  (Rook, %f, %f)",
          "  (Queen, %f, %f)",
          "  (King, %f, %f)"
        ]
        ++ "\n]"
    formatMob =
      printf
        formatMobStr
        (mobs PV.! 0)
        (mobs PV.! 1)
        (mobs PV.! 2)
        (mobs PV.! 3)
        (mobs PV.! 4)
        (mobs PV.! 5)
        (mobs PV.! 6)
        (mobs PV.! 7)
        (mobs PV.! 8)
        (mobs PV.! 9)
        (mobs PV.! 10)
        (mobs PV.! 11)
    safeties = tunableKingSafety tunables

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        (x : _) -> x
        [] -> error "no file provided as argument"
  fileContents <- TIO.readFile filename
  let startingTunables = newTunables
  let parsed = parse parsePgns filename fileContents
  case parsed of
    Left err -> putStrLn (errorBundlePretty err)
    Right pgnGames -> do
      let allGames = pgnGames >>= playPgn
      putStrLn $ "number of positions: " ++ show (length allGames)
      let k = calcSigmoidK startingTunables allGames
      putStrLn $ "k: " ++ show k
      let keepTuning prevErr currTunables = do
            putStrLn $ "previous error: " ++ show prevErr
            putStrLn $ "previous tunables: " ++ show currTunables
            gen <- newStdGen
            let shuffledGames = shuffle' allGames (length allGames) gen
            let steppedTunables = tuneEpoch currTunables shuffledGames k 500
            let newErr = calcError steppedTunables shuffledGames k
            if newErr > prevErr
              then pure currTunables
              else keepTuning newErr steppedTunables
      finalTunables <- keepTuning (fromIntegral (maxBound :: Int)) startingTunables
      putStrLn $ formatTunables finalTunables
