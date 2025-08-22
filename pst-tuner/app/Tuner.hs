module Tuner
  ( Tunables (..),
    newTunables,
    tunableMobility,
    tunableKingSafety,
    tunedEval,
    tracingQuie,
    calcSigmoidK,
    calcError,
    sgdBatch,
    tuneEpoch,
  )
where

import Control.Parallel.Strategies (parListChunk, rseq, withStrategy, rdeepseq, evalList)
import Data.Bifunctor (first)
import Data.Foldable (foldl', maximumBy)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Vector.Primitive qualified as PV
import Trout.Bitboard (Bitboard, foldSqs, (.^.))
import Trout.Game (Game (..), allDisquiets, makeMove, mobility)
import Trout.Game.Board (Board (..), getPiece, pieceBitboard)
import Trout.Game.Move (Move (..), SpecialMove (..))
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search (seeOfCapture)
import Trout.Search.Eval (totalMaterialScore, virtMobile)
import Trout.Search.PieceSquareTables
  ( bishopEPST,
    bishopMPST,
    kingEPST,
    kingMPST,
    knightEPST,
    knightMPST,
    pawnEPST,
    pawnMPST,
    queenEPST,
    queenMPST,
    rookEPST,
    rookMPST,
  )
import Trout.Search.Worthiness
  ( lossWorth,
    pieceWorth,
    winWorth,
  )

mpstsBase :: PV.Vector Double
mpstsBase = PV.map fromIntegral $ PV.concat [pawnMPST, knightMPST, bishopMPST, rookMPST, queenMPST, kingMPST]

epstsBase :: PV.Vector Double
epstsBase = PV.map fromIntegral $ PV.concat [pawnEPST, knightEPST, bishopEPST, rookEPST, queenEPST, kingEPST]

-- TODO make this an actual type at some point
-- weirder to do derivatives but it is what it is
newtype Tunables = Tunables
  { unTunables :: PV.Vector Double
  }
  deriving (Show)

newTunables :: Tunables
newTunables =
  Tunables
    ( PV.concat
        [ mpstsBase,
          epstsBase,
          PV.fromList [8.9304334836208, 12.842035165579508, 9.27158925373256, 0.42504263816624105, 8.817018669569705, 1.7618265292984037, 5.430428346121639, 3.5073542178510158, 5.553963075362616, 4.995892091104656, 3.3835406224833062, 6.03041405592747],
          PV.fromList [5.748215440955903, -0.46339401922742257]
        ]
    )

tunableMPST :: Tunables -> PV.Vector Double
tunableMPST (Tunables vec) = PV.slice 0 (PV.length mpstsBase) vec

tunableEPST :: Tunables -> PV.Vector Double
tunableEPST (Tunables vec) = PV.slice (PV.length mpstsBase) (PV.length epstsBase) vec

tunableMobility :: Tunables -> PV.Vector Double
tunableMobility (Tunables vec) = PV.slice (2 * 6 * 64) 12 vec

tunableKingSafety :: Tunables -> (Double, Double)
tunableKingSafety (Tunables vec) = (ks PV.! 0, ks PV.! 1)
  where
    ks = PV.slice (2 * 6 * 64 + 12) 2 vec

pstEval :: Tunables -> Bitboard -> PieceType -> Int -> Int -> Int -> Double
pstEval tunables bb piece !mgPhase !egPhase !mask =
  foldSqs
    ( \score sqRaw ->
        let sq = sqRaw .^. mask
            m = mpsts PV.! (pieceOffset + sq)
            e = epsts PV.! (pieceOffset + sq)
         in score + ((m * fromIntegral mgPhase + e * fromIntegral egPhase) / 24)
    )
    0
    bb
  where
    pieceOffset = fromEnum piece * 64
    mpsts = tunableMPST tunables
    epsts = tunableEPST tunables
{-# INLINE pstEval #-}

tunedEval :: Tunables -> Board -> Double
tunedEval !tunables !board =
  fromIntegral (colorSign (boardTurn board))
    * (pstEvalValue + mobilityValue + scaledKingSafety)
  where
    pieces = boardPieces board
    getBB color = ($ pieces) . pieceBitboard . Piece color
    mgPhase = totalMaterialScore board
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

    mobs = tunableMobility tunables
    mobilityValue =
      sum
        [ (mgMult * fromIntegral mgPhase + egMult * fromIntegral egPhase)
            * fromIntegral (colorSign c)
            * fromIntegral (mobility board (Piece c p))
            / 24
        | c <- [White, Black],
          (p, mgMult, egMult) <-
            [ (Pawn, mobs PV.! 0, mobs PV.! 1),
              (Knight, mobs PV.! 2, mobs PV.! 3),
              (Bishop, mobs PV.! 4, mobs PV.! 5),
              (Rook, mobs PV.! 6, mobs PV.! 7),
              (Queen, mobs PV.! 8, mobs PV.! 9),
              (King, mobs PV.! 10, mobs PV.! 11)
            ]
        ]

    (safetyMg, safetyEg) = tunableKingSafety tunables
    kingSafety = virtMobile Black pieces - virtMobile White pieces
    scaledKingSafety =
      fromIntegral kingSafety
        * (fromIntegral mgPhase * safetyMg + fromIntegral egPhase * safetyEg)
        / 24

removeSingle :: (Eq a) => a -> [a] -> [a]
removeSingle _ [] = []
removeSingle r (x : xs)
  | r == x = xs
  | otherwise = x : removeSingle r xs

singleSelect :: [(Int, Move)] -> ((Int, Move), [(Int, Move)])
singleSelect moves = (best, removeSingle best moves)
  where
    best = maximumBy (comparing fst) moves

-- it's in readert st monad in real quie
-- and it was easier to jut wrap it in identity monad to keep the syntax
tracingQuie :: Tunables -> Double -> Double -> Game -> (Double, Game)
tracingQuie !tunables !alpha !beta !game = runIdentity $ do
  -- stand-pat from null-move observation (tunedEval immediately = not moving)
  let staticEval = tunedEval tunables board
  let seeReq = max 0 (alpha - staticEval - 2 * fromIntegral (pieceWorth Pawn))
  if staticEval >= beta
    then pure (staticEval, game)
    else
      go
        staticEval
        game
        (filter ((>= seeReq) . fromIntegral . fst) ((\m -> (scoreMove m, m)) <$> allDisquiets board))
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
        let (score, endGame) = first negate $ tracingQuie tunables (-beta) (-trueAlpha) movedGame
        if score >= beta
          then pure (score, endGame)
          else
            if score > bestScore
              then go score endGame movesRest
              else go bestScore bestGame movesRest
      Nothing -> go bestScore bestGame movesRest
      where
        ((_, move), movesRest) = singleSelect moves

quieWrapper :: Tunables -> Game -> (Double, Game)
quieWrapper tunables game =
  first
    (* fromIntegral (colorSign (boardTurn (gameBoard game))))
    (tracingQuie tunables (fromIntegral (lossWorth :: Int)) (fromIntegral (winWorth :: Int)) game)

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp 1 ** (-x))

-- there's the sigmoid * (1 - sigmoid) nonsense but
-- i cba derivate that
sigmoidDerivative :: Double -> Double
sigmoidDerivative x = ex / (1 + ex) ** 2
  where
    ex = exp 1 ** (-x)

-- mean squared error
calcError :: Tunables -> [(Game, Double)] -> Double -> Double
calcError tunables games fac =
  let errParts =
        ( \(g, res) ->
            let (rawScore, _) = quieWrapper tunables g
             in (sigmoid (rawScore * fac) - res) ** 2
        )
          <$> games
      errSum = sum $ withStrategy (parListChunk 1024 rseq) errParts
   in (errSum / fromIntegral (length games))

calcSigmoidK :: Tunables -> [(Game, Double)] -> Double
calcSigmoidK tunables games
  | rootError < nudgeRight = go (-kStep) initialK rootError
  | otherwise = go kStep (initialK + kStep) nudgeRight
  where
    kStep = 0.0001
    -- from previous runs
    -- cache here to save time
    -- initialK = 0.0058
    initialK = 0.0058
    rootError = calcError tunables games initialK
    nudgeRight = calcError tunables games (initialK + kStep)

    go delta k lastErr
      | newErr > lastErr = k
      | otherwise = go delta (k + delta) newErr
      where
        newErr = calcError tunables games (k + delta)

batchSize :: Int
batchSize = 16384

sgdBatch :: Tunables -> [(Game, Double)] -> Double -> Double -> Tunables
sgdBatch tunables games k step =
  Tunables
    ( PV.zipWith
        (\x d -> x - d / fromIntegral batchSize * step)
        (unTunables tunables)
        derivativesSum
    )
  where
    batch = take batchSize games
    calcAlterations (game, res) = alterations
      where
        (endpointEval, quieEndpoint) = quieWrapper tunables game
        mgPhaseFrac = fromIntegral (totalMaterialScore (gameBoard game)) / 24
        egPhaseFrac = 1 - mgPhaseFrac
        commonD = 2 * (sigmoid (k * endpointEval) - res) * k * sigmoidDerivative (k * endpointEval)
        board = gameBoard quieEndpoint
        pieces = boardPieces board
        sqAlterations rawSq =
          maybeToList (getPiece rawSq pieces)
            >>= \(Piece c p) ->
              let (sq, existMult) = case c of
                    White -> (rawSq, 1)
                    Black -> (rawSq .^. 56, -1)
                  mgIdx = sq + fromEnum p * 64
                  egIdx = mgIdx + 64 * 6
               in [ (mgIdx, commonD * mgPhaseFrac * existMult),
                    (egIdx, commonD * egPhaseFrac * existMult)
                  ]
        mobilityAlterations =
          concat
            [ [ (mobMgIdx, commonD * mgPhaseFrac * mobMult),
                (mobEgIdx, commonD * egPhaseFrac * mobMult)
              ]
            | c <- [White, Black],
              p <- enumFromTo Pawn King,
              let piece = Piece c p
                  mobMgIdx = 2 * 6 * 64 + fromEnum p * 2
                  mobEgIdx = mobMgIdx + 1
                  mobMult = fromIntegral $ colorSign c * mobility board piece
            ]
        safetyMult = fromIntegral $ virtMobile Black pieces - virtMobile White pieces
        safetyMgIdx = 2 * 6 * 64 + 12
        safetyEgIdx = safetyMgIdx + 1
        kingSafetyAlterations =
          [ (safetyMgIdx, commonD * mgPhaseFrac * safetyMult),
            (safetyEgIdx, commonD * egPhaseFrac * safetyMult)
          ]
        alterations = ([0 .. 64] >>= sqAlterations) ++ mobilityAlterations ++ kingSafetyAlterations

    -- TODO parallelize
    derivativesSum =
      foldl'
        (PV.accum (+))
        (PV.replicate (PV.length (unTunables tunables)) 0)
        (withStrategy (parListChunk 1024 (evalList rdeepseq)) (calcAlterations <$> batch))

tuneEpoch :: Tunables -> [(Game, Double)] -> Double -> Double -> Tunables
tuneEpoch startingTunables games k step
  | length games < batchSize = startingTunables
  | otherwise = tuneEpoch fullRetuned (drop batchSize games) k step
  where
    fullRetuned = sgdBatch startingTunables games k step
