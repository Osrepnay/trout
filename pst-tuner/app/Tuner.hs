module Tuner
  ( StructuredTunables (..),
    Tunables,
    structurize,
    flattenTunables,
    newTunables,
    tunableFactors,
    tunedEval,
    calcSigmoidK,
    calcError,
    sgdBatch,
    tuneEpoch,
    calculateWorthiness,
  )
where

import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Bits (popCount, (!>>.))
import Data.Functor ((<&>))
import Data.List (foldl1', scanl')
import Data.Vector.Primitive qualified as PV
import Numeric.LinearAlgebra (Matrix, matrix, scale, sumElements, toLists)
import Trout.Bitboard ((.^.))
import Trout.Game (Game (..), mobility)
import Trout.Game.Board (Board (..), getPiece, pieceBitboard)
import Trout.Piece (Color (..), Piece (..), PieceType (..), colorSign)
import Trout.Search.Eval
  ( bishopPairEg,
    bishopPairMg,
    mobilityMults,
    passerRows,
    passersEg,
    passersMg,
    safetyMultEg,
    safetyMultMg,
    tempoEg,
    tempoMg,
    totalMaterialScore,
    virtMobile,
  )
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

mpstsBase :: PV.Vector Double
mpstsBase = PV.map fromIntegral $ PV.concat [pawnMPST, knightMPST, bishopMPST, rookMPST, queenMPST, kingMPST]

epstsBase :: PV.Vector Double
epstsBase = PV.map fromIntegral $ PV.concat [pawnEPST, knightEPST, bishopEPST, rookEPST, queenEPST, kingEPST]

data StructuredTunables = StructuredTunables
  { sTunableMPST :: PV.Vector Double,
    sTunableEPST :: PV.Vector Double,
    sTunableMobility :: PV.Vector Double,
    sTunableKingSafety :: (Double, Double),
    sTunablePassers :: PV.Vector Double,
    sTunableBishopPair :: (Double, Double),
    sTunableTempo :: (Double, Double)
  }
  deriving (Eq, Show)

-- flat version of StructuredTunables
type Tunables = Matrix Double

flattenTunables :: StructuredTunables -> Tunables
flattenTunables
  StructuredTunables
    { sTunableMPST = mpst,
      sTunableEPST = epst,
      sTunableMobility = mob,
      sTunableKingSafety = kingSafety,
      sTunablePassers = passers,
      sTunableBishopPair = bishopPair,
      sTunableTempo = tempo
    } =
    matrix 1 $
      PV.toList $
        PV.concat
          [ mpst,
            epst,
            mob,
            tupToVec kingSafety,
            passers,
            tupToVec bishopPair,
            tupToVec tempo
          ]
    where
      tupToVec (a, b) = PV.fromList [a, b]

-- TODO is this optimized well?
structurize :: Tunables -> StructuredTunables
structurize mat = case segments of
  [mpsts, epsts, mob, kingSafetyVec, passers, bishopPairVec, tempoVec] ->
    StructuredTunables
      { sTunableMPST = mpsts,
        sTunableEPST = epsts,
        sTunableMobility = mob,
        sTunableKingSafety = (kingSafetyVec PV.! 0, kingSafetyVec PV.! 1),
        sTunablePassers = passers,
        sTunableBishopPair = (bishopPairVec PV.! 0, bishopPairVec PV.! 1),
        sTunableTempo = (tempoVec PV.! 0, tempoVec PV.! 1)
      }
  _ -> error "wrong number of segments"
  where
    -- TODO hack
    vec = PV.fromList $ concat $ toLists mat
    segmentLengths =
      [ PV.length mpstsBase,
        PV.length epstsBase,
        PV.length mobilityMults,
        2, -- safety
        PV.length passersMg * 2,
        2, -- bishop pair
        2 -- tempo
      ]
    -- accumulates the lengths to find indices
    indices = init (scanl' (+) 0 segmentLengths)
    segments = zipWith (\i l -> PV.slice i l vec) indices segmentLengths

newTunables :: Tunables
newTunables =
  flattenTunables $
    StructuredTunables
      (PV.map (* 1) mpstsBase)
      (PV.map (* 1) epstsBase)
      (PV.map fromIntegral mobilityMults)
      (fromIntegral safetyMultMg, fromIntegral safetyMultEg)
      (PV.map fromIntegral (passersMg PV.++ passersEg))
      (fromIntegral bishopPairMg, fromIntegral bishopPairEg)
      (fromIntegral tempoMg, fromIntegral tempoEg)

-- calculate the factors for each tunable entry
-- basically, how much influence it has on this position
-- sum (tunableFactors .* tunables) = absolute position eval
tunableFactors :: Game -> Tunables
tunableFactors game =
  flattenTunables $
    StructuredTunables
      { sTunableMPST = mpstFactors,
        sTunableEPST = epstFactors,
        sTunableMobility = mobilityFactors,
        sTunableKingSafety = kingSafetyFactors,
        sTunablePassers = passerFactors,
        sTunableBishopPair = bishopPairFactors,
        sTunableTempo = tempoBonusFactors
      }
  where
    board = gameBoard game
    pieces = boardPieces board
    mgPhase = fromIntegral (totalMaterialScore board)
    egPhase = 24 - mgPhase

    boardFactors =
      PV.fromList $
        [ fromIntegral $ fromEnum whiteExist - fromEnum blackExist
        | p <- [Pawn .. King],
          sq <- [0 .. 63],
          let whiteSq = sq,
          let blackSq = sq .^. 56,
          let whiteExist = getPiece whiteSq pieces == Just (Piece White p),
          let blackExist = getPiece blackSq pieces == Just (Piece Black p)
        ]
    mpstFactors = PV.map ((/ 24) . (* mgPhase)) boardFactors
    epstFactors = PV.map ((/ 24) . (* egPhase)) boardFactors

    mobilityFactors =
      PV.fromList $
        concat $
          [ [mobCount * mgPhase / 24, mobCount * egPhase / 24]
          | p <- [Pawn .. King],
            let mkMob c = mobility board (Piece c p),
            let mobCount = fromIntegral (mkMob White - mkMob Black)
          ]

    kingSafety = fromIntegral $ virtMobile Black pieces - virtMobile White pieces
    kingSafetyFactors = (kingSafety * mgPhase / 24, kingSafety * egPhase / 24)

    whitePawns = pieceBitboard (Piece White Pawn) pieces
    blackPawns = pieceBitboard (Piece Black Pawn) pieces
    mkPasserVec rs = PV.accum (+) (PV.replicate 8 0) ((,1) <$> rs)
    passerBaseVec =
      PV.zipWith
        (-)
        (mkPasserVec (passerRows White whitePawns blackPawns))
        (mkPasserVec (passerRows Black blackPawns whitePawns))
    passerFactors =
      PV.map (/ 24) $
        PV.map (* mgPhase) passerBaseVec
          PV.++ PV.map (* egPhase) passerBaseVec

    hasPair c = popCount (pieceBitboard (Piece c Bishop) pieces) !>>. 1
    bishopPairDiff = fromIntegral $ hasPair White - hasPair Black
    bishopPairFactors = (bishopPairDiff * mgPhase / 24, bishopPairDiff * egPhase / 24)

    tempoBonus = fromIntegral $ colorSign (boardTurn board)
    tempoBonusFactors = (tempoBonus * mgPhase / 24, tempoBonus * egPhase / 24)

tunedEval :: Tunables -> Tunables -> Double
tunedEval !factors !tunables = sumElements (factors * tunables) / 10

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

-- there's the sigmoid * (1 - sigmoid) nonsense but
-- i cba derivate that
sigmoidDerivative :: Double -> Double
sigmoidDerivative x = ex / (1 + ex) ** 2
  where
    ex = exp (-x)

-- mean squared error
calcError :: Tunables -> [(Tunables, Double)] -> Double -> Double
calcError tunables games fac =
  let errParts =
        ( \(facs, res) ->
            let rawScore = tunedEval facs tunables
             in (sigmoid (rawScore * fac) - res) ** 2
        )
          <$> games
      errSum = sum $ withStrategy (parListChunk 1024 rseq) errParts
   in (errSum / fromIntegral (length games))

-- iteratively estimate k term for sigmoid
calcSigmoidK :: Tunables -> [(Tunables, Double)] -> Double
calcSigmoidK tunables games
  | rootError < nudgeRight = go (-kStep) initialK rootError
  | otherwise = go kStep (initialK + kStep) nudgeRight
  where
    kStep = 0.0001
    -- from previous runs
    -- cache here to save time
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

sgdBatch :: Tunables -> [(Tunables, Double)] -> Double -> Double -> Tunables
sgdBatch tunables games k step = tunables - (scale (step / fromIntegral batchSize) derivativesSum)
  where
    batch = take batchSize games
    calcAlterations (factors, res) = alterations
      where
        evalScore = tunedEval factors tunables
        commonD = (1 / 10) * 2 * (sigmoid (k * evalScore) - res) * k * sigmoidDerivative (k * evalScore)
        alterations = scale commonD factors

    derivativesSum =
      foldl1'
        (+)
        (withStrategy (parListChunk 1024 rseq) (calcAlterations <$> batch))

tuneEpoch :: Tunables -> [(Tunables, Double)] -> Double -> Double -> Tunables
tuneEpoch startingTunables games k step
  | length games < batchSize = startingTunables
  | otherwise = tuneEpoch fullRetuned (drop batchSize games) k step
  where
    fullRetuned = sgdBatch startingTunables games k step

-- should be better than straight average b/c of e.g.
-- terrible squares that are very rare
-- unnormalized!!
calculateWorthiness :: [Game] -> Tunables -> PV.Vector Double
calculateWorthiness games tunables = PV.fromList worths
  where
    piecesList = boardPieces . gameBoard <$> games
    changes =
      piecesList >>= \pieces ->
        [0 .. 63] >>= \sq -> case getPiece sq pieces of
          Nothing -> []
          Just (Piece c p) ->
            let mask = if c == White then 0 else 56
                newSq = sq .^. mask
                idx = fromEnum p * 64 + newSq
             in [(idx, 1)]
    weights = PV.accum (+) (PV.replicate (6 * 64) 0) changes

    mpst = sTunableMPST (structurize tunables)
    weightedMPST = PV.zipWith (*) mpst weights

    worths =
      [Pawn .. King] <&> \p ->
        let slicer = PV.slice (fromEnum p * 64) 64
            totalWeight = PV.sum (slicer weights)
         in (/ 10) $ PV.sum $ PV.map (/ totalWeight) (slicer weightedMPST)
