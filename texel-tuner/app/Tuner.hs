module Tuner (newTunables, tunedEval, tracingQuie, calcSigmoidShape) where

import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Bifunctor (first)
import Data.Foldable (maximumBy)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Ord (comparing)
import Data.Vector.Primitive qualified as PV
import Debug.Trace
import Trout.Bitboard (Bitboard, foldSqs, (.^.))
import Trout.Game (Game (..), allDisquiets, makeMove, mobility)
import Trout.Game.Board (Board (..), pieceBitboard)
import Trout.Game.Move (Move (..), SpecialMove (..))
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search (seeOfCapture)
import Trout.Search.Eval (totalMaterialScore, virtMobile)
import Trout.Search.Worthiness
  ( bishopWorth,
    knightWorth,
    lossWorth,
    pawnWorth,
    pieceWorth,
    queenWorth,
    rookWorth,
    winWorth,
  )

pawnMPST :: PV.Vector Int
pawnMPST =
  PV.fromList $
    fmap (+ pawnWorth) $
      concat $
        reverse
          [ [0, 0, 0, 0, 0, 0, 0, 0],
            [16, 17, 18, 20, 18, 16, 12, 11],
            [8, 10, 13, 18, 18, 16, 13, 10],
            [5, 5, 5, 8, 8, 7, 5, 5],
            [4, 3, 4, 5, 4, 3, 2, 3],
            [3, 2, 3, 1, 2, 1, 3, 3],
            [3, 3, 2, 0, 0, 3, 3, 3],
            [0, 0, 0, 0, 0, 0, 0, 0]
          ]

knightMPST :: PV.Vector Int
knightMPST =
  PV.fromList $
    fmap (+ knightWorth) $
      concat $
        reverse
          [ [-4, 0, 4, 2, 8, 3, 5, -4],
            [2, 2, 16, 10, 15, 21, 14, 13],
            [3, 11, 11, 18, 21, 24, 19, 14],
            [4, 6, 9, 14, 9, 15, 7, 11],
            [1, 2, 6, 5, 8, 6, 6, 5],
            [-3, 0, 4, 3, 4, 4, 3, -3],
            [-6, -4, -2, 2, 1, -1, -5, -1],
            [-11, 1, -6, -3, -3, -2, 0, -11]
          ]

bishopMPST :: PV.Vector Int
bishopMPST =
  PV.fromList $
    fmap (+ bishopWorth) $
      concat $
        reverse
          [ [4, 3, 3, 3, 3, 3, 6, 7],
            [3, 6, 6, 6, 9, 15, 12, 12],
            [8, 10, 9, 13, 15, 17, 21, 14],
            [7, 4, 9, 10, 9, 12, 5, 10],
            [2, 6, 6, 8, 9, 6, 6, 4],
            [6, 6, 6, 8, 8, 6, 7, 6],
            [5, 2, 8, 3, 3, 5, 3, 7],
            [1, 8, 2, -1, -1, 1, 2, -2]
          ]

rookMPST :: PV.Vector Int
rookMPST =
  PV.fromList $
    fmap (+ rookWorth) $
      concat $
        reverse
          [ [8, 6, 8, 8, 9, 6, 7, 11],
            [7, 9, 10, 10, 12, 12, 11, 10],
            [5, 6, 7, 9, 12, 12, 10, 10],
            [4, 5, 6, 7, 8, 9, 8, 10],
            [5, 3, 4, 5, 6, 6, 6, 7],
            [2, 4, 4, 3, 4, 5, 9, 7],
            [-1, 2, 3, 3, 3, 3, 3, 0],
            [3, 5, 6, 7, 7, 5, 5, 2]
          ]

queenMPST :: PV.Vector Int
queenMPST =
  PV.fromList $
    fmap (+ queenWorth) $
      concat $
        reverse
          [ [9, 9, 11, 11, 12, 15, 16, 21],
            [4, 6, 9, 12, 15, 25, 24, 27],
            [4, 9, 12, 16, 19, 24, 26, 20],
            [5, 4, 8, 11, 11, 13, 10, 13],
            [3, 5, 5, 6, 7, 7, 10, 8],
            [5, 5, 5, 4, 4, 5, 6, 9],
            [2, 2, 4, 5, 4, 4, 1, 7],
            [6, -1, -1, 2, 0, -2, -6, 0]
          ]

kingMPST :: PV.Vector Int
kingMPST =
  PV.fromList $
    concat $
      reverse
        [ [3, 7, 6, 5, 5, 4, 5, 1],
          [2, 5, 6, 6, 4, 4, 4, 2],
          [2, 4, 4, 4, 3, 4, 3, 2],
          [1, 2, 3, 3, 2, 2, 2, 1],
          [-1, 1, 1, 1, 1, 1, 0, -1],
          [-1, 0, 0, 0, 1, 0, 1, 0],
          [2, 1, 0, -1, -1, 1, 2, 4],
          [3, 7, 7, -5, 1, -3, 6, 5]
        ]

pawnEPST :: PV.Vector Int
pawnEPST =
  PV.fromList $
    fmap (+ pawnWorth) $
      concat $
        reverse
          [ [0, 0, 0, 0, 0, 0, 0, 0],
            [50, 45, 44, 38, 35, 36, 31, 34],
            [16, 18, 17, 14, 16, 15, 18, 12],
            [5, 4, 3, 2, 2, 4, 4, 5],
            [2, 2, 1, 1, 1, 1, 2, 2],
            [1, 1, 1, 0, 1, 1, 1, 1],
            [1, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 0, 0]
          ]

knightEPST :: PV.Vector Int
knightEPST =
  PV.fromList $
    fmap (+ knightWorth) $
      concat $
        reverse
          [ [-4, 3, 9, 5, 9, 5, 9, -5],
            [-1, 2, 6, 8, 10, 7, 5, 3],
            [2, 5, 7, 7, 9, 12, 6, 4],
            [2, 1, 4, 3, 2, 5, 2, 3],
            [0, 2, 2, 1, 2, 2, 2, 1],
            [0, 0, 0, 1, 1, 0, 1, 0],
            [-1, -1, 0, 0, 0, 0, -1, 0],
            [-8, 0, -3, -1, -1, -1, 0, -4]
          ]

bishopEPST :: PV.Vector Int
bishopEPST =
  PV.fromList $
    fmap (+ bishopWorth) $
      concat $
        reverse
          [ [4, 7, 9, 8, 8, 6, 9, 4],
            [2, 6, 6, 8, 7, 6, 7, 3],
            [4, 7, 7, 7, 8, 9, 8, 3],
            [4, 1, 7, 5, 4, 6, 1, 4],
            [1, 4, 1, 4, 4, 1, 3, 1],
            [2, 1, 2, 1, 1, 2, 1, 2],
            [1, 1, 1, 1, 0, 1, 1, 1],
            [0, 1, 0, 0, 0, 0, -1, 0]
          ]

rookEPST :: PV.Vector Int
rookEPST =
  PV.fromList $
    fmap (+ rookWorth) $
      concat $
        reverse
          [ [11, 11, 12, 11, 11, 10, 12, 12],
            [8, 10, 11, 12, 12, 13, 13, 11],
            [5, 7, 8, 9, 9, 9, 9, 6],
            [4, 4, 5, 5, 5, 5, 5, 4],
            [3, 3, 3, 4, 3, 3, 4, 3],
            [1, 2, 2, 2, 2, 2, 3, 2],
            [0, 1, 1, 2, 1, 1, 1, 1],
            [0, 1, 2, 2, 2, 1, 2, 0]
          ]

queenEPST :: PV.Vector Int
queenEPST =
  PV.fromList $
    fmap (+ queenWorth) $
      concat $
        reverse
          [ [4, 7, 8, 10, 10, 12, 11, 10],
            [2, 3, 7, 10, 12, 11, 8, 9],
            [2, 5, 6, 7, 10, 13, 9, 6],
            [2, 2, 5, 4, 5, 6, 4, 3],
            [1, 2, 2, 2, 3, 3, 3, 2],
            [1, 1, 2, 1, 1, 1, 2, 2],
            [1, 1, 1, 1, 1, 1, 0, 1],
            [2, 0, 0, 0, 0, 0, -2, -1]
          ]

kingEPST :: PV.Vector Int
kingEPST =
  PV.fromList $
    concat $
      reverse
        [ [4, 33, 31, 29, 21, 21, 22, 0],
          [9, 39, 41, 38, 30, 32, 29, 12],
          [13, 37, 38, 40, 36, 33, 24, 14],
          [8, 23, 31, 38, 34, 27, 20, 7],
          [-1, 10, 18, 22, 20, 16, 8, 3],
          [-3, 1, 5, 4, 5, 4, 3, 1],
          [1, 1, 1, 1, 1, 2, 3, 3],
          [1, 3, 2, -2, 0, -1, 1, 2]
        ]

mpstsBase :: PV.Vector Int
mpstsBase = PV.concat [pawnMPST, knightMPST, bishopMPST, rookMPST, queenMPST, kingMPST]

epstsBase :: PV.Vector Int
epstsBase = PV.concat [pawnEPST, knightEPST, bishopEPST, rookEPST, queenEPST, kingEPST]

data Tunables = Tunables
  { tunableMPST :: PV.Vector Int,
    tunableEPST :: PV.Vector Int
  }

newTunables :: Tunables
newTunables = Tunables mpstsBase epstsBase

pstEval :: Tunables -> Bitboard -> PieceType -> Int -> Int -> Int -> Int
pstEval (Tunables {tunableMPST = mpsts, tunableEPST = epsts}) bb piece !mgPhase !egPhase !mask =
  foldSqs
    ( \score sqRaw ->
        let sq = sqRaw .^. mask
            m = mpsts PV.! (pieceOffset + sq)
            e = epsts PV.! (pieceOffset + sq)
         in score + ((m * mgPhase + e * egPhase) `quot` 24)
    )
    0
    bb
  where
    pieceOffset = fromEnum piece * 64
{-# INLINE pstEval #-}

tunedEval :: Tunables -> Game -> Int
tunedEval !tunables !game = colorSign (boardTurn board) * (pstEvalValue + mobilityValue + scaledKingSafety)
  where
    board = gameBoard game
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    mgPhase = totalMaterialScore game
    egPhase = 24 - mgPhase

    pst c p = pstEval tunables (getBB c p) p mgPhase egPhase $ case c of
      White -> 0
      Black -> 56
    pstEvalValue =
      pst White Pawn
        - pst Black Pawn
        + pst White Knight
        - pst Black Knight
        + pst White Bishop
        - pst Black Bishop
        + pst White Rook
        - pst Black Rook
        + pst White Queen
        - pst Black Queen
        + pst White King
        - pst Black King

    mobilityValue =
      sum
        [ (mgMult * mgPhase + egMult * egPhase)
            * colorSign c
            * mobility board (Piece c p)
            `quot` 24
        | c <- [White, Black],
          (p, mgMult, egMult) <-
            [ (Pawn, 3, 7),
              (Knight, 8, 6),
              (Bishop, 6, 4),
              (Rook, 4, 6),
              (Queen, 5, 4),
              (King, 2, 6)
            ]
        ]

    kingSafety = virtMobile Black pieces - virtMobile White pieces
    scaledKingSafety = kingSafety * mgPhase `quot` 24 * 3

removeSingle :: (Eq a) => a -> [a] -> [a]
removeSingle _ [] = []
removeSingle r (x : xs)
  | r == x = xs
  | otherwise = x : removeSingle r xs

singleSelect :: [(Int, Move)] -> ((Int, Move), [(Int, Move)])
singleSelect moves = (best, removeSingle best moves)
  where
    best = maximumBy (comparing fst) moves

tracingQuie :: Tunables -> Int -> Int -> Game -> Identity (Int, Game)
tracingQuie !tunables !alpha !beta !game = do
  -- stand-pat from null-move observation (tunedEval immediately = not moving)
  let staticEval = tunedEval tunables game
  let seeReq = max 0 (alpha - staticEval - 2 * pieceWorth Pawn)
  if staticEval >= beta
    then pure (staticEval, game)
    else
      go
        staticEval
        game
        (filter ((>= seeReq) . fst) ((\m -> (scoreMove m, m)) <$> allDisquiets board))
  where
    board = gameBoard game

    scoreMove m = case seeOfCapture board m of
      Just s -> s
      Nothing -> case moveSpecial m of
        -- non-capture promotions
        -- TODO maybe throw SEE on here too?
        (Promotion p) -> pieceWorth p - pieceWorth Pawn
        -- should be impossible
        _ -> lossWorth

    go bestScore bestGame [] = pure (bestScore, bestGame)
    go bestScore bestGame moves = case makeMove game move of
      Just movedGame -> do
        let trueAlpha = max alpha bestScore
        (score, endGame) <- first negate <$> tracingQuie tunables (-beta) (-trueAlpha) movedGame
        if score >= beta
          then pure (score, endGame)
          else
            if score > bestScore
              then go score endGame movesRest
              else go bestScore bestGame movesRest
      Nothing -> go bestScore bestGame movesRest
      where
        ((_, move), movesRest) = singleSelect moves

quieWrapper :: Tunables -> Game -> Int
quieWrapper tunables game = runIdentity $ do
  (res, _) <- tracingQuie tunables lossWorth winWorth game
  pure (res * colorSign (boardTurn (gameBoard game)))

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp 1 ** (-x))

calcSigmoidShape :: Tunables -> [(Game, Double)] -> Double
calcSigmoidShape tunables games = go 0.00665
  where
    numGames = length games
    -- mean squared error
    calcError fac =
      let errParts =
            ( \(g, res) ->
                let rawScore = quieWrapper tunables g
                 in (sigmoid (fromIntegral rawScore * fac) - res) ** 2
            )
              <$> games
          errSum = sum $ withStrategy (parListChunk 10000 rseq) errParts
       in (errSum / fromIntegral numGames)

    go fac =
      traceShow fac $
        let nudgeLeft = calcError (fac - 0.00005)
            nudgeRight = calcError (fac + 0.00005)
         in if nudgeLeft < nudgeRight
              then go (fac - 0.00005)
              else go (fac + 0.00005)
