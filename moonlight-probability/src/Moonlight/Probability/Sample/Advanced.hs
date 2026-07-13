{-# LANGUAGE BangPatterns #-}

module Moonlight.Probability.Sample.Advanced
  ( CounterGen,
    counterGenFromSeed,
    deriveCounterGen,
    splitCounterGen,
    word64At,
    doubleAt,
    CounterSample,
    runCounterSample,
    evalCounterSample,
    nextWord64,
    nextDouble,
    nextOpenDouble,
    nextGaussian,
    sampleGumbel,
    sampleConcrete,
    sampleConcreteFromLogits,
    sampleCategoricalGumbelMax,
    sampleCategoricalGumbelMaxFromLogits,
    sampleGumbelTopK,
    sampleGumbelTopKFromLogits,
    Euclidean (..),
    DiagMetric,
    diagMetric,
    identityMetric,
    NUTSConfig (..),
    defaultNUTSConfig,
    NUTSResult (..),
    essStep,
    findReasonableStepSize,
    nutsStep,
    SobolTable,
    sobolTable64,
    sobolPoint,
    owenScrambledSobolPoint,
    rqmcAverage,
    rqmcReplicateMeans
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    xor,
    rotateL,
    shiftL,
    shiftR,
    testBit,
    setBit
  )
import Data.Kind (Constraint, Type)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Word (Word64)
import Moonlight.Probability.Sample.Input
  ( isFinite,
    isFinitePositive,
    negativeInfinity,
  )
import Prelude
import qualified Moonlight.Probability.Sample.Input as SampleInput

type CounterGen :: Type
data CounterGen = CounterGen
  { cgKey0 :: !Word64,
    cgKey1 :: !Word64,
    cgBlock :: !Word64,
    cgHasBufferedWord :: !Bool,
    cgBufferedWord :: !Word64,
    cgHasBufferedGaussian :: !Bool,
    cgBufferedGaussian :: !Double
  }

type CounterSample :: Type -> Type
newtype CounterSample a = CounterSample
  { unCounterSample :: CounterGen -> (a, CounterGen)
  }

type Euclidean :: Type -> Constraint
class Euclidean v where
  emap :: (Double -> Double) -> v -> v
  ezipWith :: (Double -> Double -> Double) -> v -> v -> v
  efoldl' :: (a -> Double -> a) -> a -> v -> a
  efoldZipWith' :: (a -> Double -> Double -> a) -> a -> v -> v -> a
  etraverse :: Applicative f => (Double -> f Double) -> v -> f v

instance Euclidean [Double] where
  emap = map
  ezipWith = zipExactWith
  efoldl' = foldl'
  efoldZipWith' = foldZipExactWith'
  etraverse = traverse

instance Functor CounterSample where
  fmap transform (CounterSample sampleValue) =
    CounterSample $ \generator ->
      case sampleValue generator of
        (value, nextGenerator) -> (transform value, nextGenerator)

instance Applicative CounterSample where
  pure value = CounterSample (\generator -> (value, generator))
  CounterSample sampleTransform <*> CounterSample sampleValue =
    CounterSample $ \generator ->
      case sampleTransform generator of
        (transform, generatorAfterTransform) ->
          case sampleValue generatorAfterTransform of
            (value, generatorAfterValue) -> (transform value, generatorAfterValue)

instance Monad CounterSample where
  CounterSample sampleValue >>= continue =
    CounterSample $ \generator ->
      case sampleValue generator of
        (value, nextGenerator) ->
          case continue value of
            CounterSample nextSample -> nextSample nextGenerator

type DiagMetric :: Type -> Type
data DiagMetric v = DiagMetric !v !v

type NUTSConfig :: Type -> Type
data NUTSConfig v = NUTSConfig
  { nutsStepSize :: !Double,
    nutsMaxDepth :: !Int,
    nutsMaxDeltaEnergy :: !Double,
    nutsMetric :: !(DiagMetric v)
  }

type NUTSResult :: Type -> Type
data NUTSResult v = NUTSResult
  { nutsPosition :: !v,
    nutsLogDensity :: !Double,
    nutsGradient :: !v,
    nutsAcceptanceRate :: !Double,
    nutsTreeDepthUsed :: !Int,
    nutsLeapfrogSteps :: !Int
  }

type SobolTable :: Type
data SobolTable = SobolTable ![[Word64]]

type SobolPolynomial :: Type
data SobolPolynomial = SobolPolynomial
  !Int
  !Word64
  ![Word64]

type TargetState :: Type -> Type
data TargetState v = TargetState !v !Double !v

type Tree :: Type -> Type
data Tree v = Tree
  { treeQMinus :: !v,
    treePMinus :: !v,
    treeGradMinus :: !v,
    treeQPlus :: !v,
    treePPlus :: !v,
    treeGradPlus :: !v,
    treeProposal :: !(TargetState v),
    treeValidCount :: !Int,
    treeContinue :: !Bool,
    treeAlphaSum :: !Double,
    treeAlphaCount :: !Int,
    treeLeapfrogCount :: !Int
  }

counterGenFromSeed :: Word64 -> CounterGen
counterGenFromSeed seed =
  let !key0 = splitMix64 seed
      !key1 = splitMix64 (seed + goldenGamma)
   in CounterGen key0 key1 0 False 0 False 0.0
{-# INLINE counterGenFromSeed #-}

deriveCounterGen :: Word64 -> CounterGen -> CounterGen
deriveCounterGen tag generator =
  let !(key0', key1') =
        threefry2x64
          (tag, tag `xor` 0xD1B54A32D192ED03)
          (cgKey0 generator, cgKey1 generator)
   in CounterGen key0' key1' 0 False 0 False 0.0
{-# INLINE deriveCounterGen #-}

splitCounterGen :: CounterGen -> (CounterGen, CounterGen)
splitCounterGen generator =
  (deriveCounterGen 0x243F6A8885A308D3 generator, deriveCounterGen 0x13198A2E03707344 generator)
{-# INLINE splitCounterGen #-}

runCounterSample :: CounterSample a -> CounterGen -> (a, CounterGen)
runCounterSample = unCounterSample
{-# INLINE runCounterSample #-}

evalCounterSample :: Word64 -> CounterSample a -> a
evalCounterSample seed sampleValue =
  fst (runCounterSample sampleValue (counterGenFromSeed seed))
{-# INLINE evalCounterSample #-}

word64At :: CounterGen -> Word64 -> Word64
word64At generator wordIndex =
  let !blockIndex = shiftR wordIndex 1
      !lane = wordIndex .&. 1
      !(word0, word1) =
        threefry2x64
          (blockIndex, 0)
          (cgKey0 generator, cgKey1 generator)
   in if lane == 0 then word0 else word1
{-# INLINE word64At #-}

doubleAt :: CounterGen -> Word64 -> Double
doubleAt generator wordIndex = wordToUnit53 (word64At generator wordIndex)
{-# INLINE doubleAt #-}

nextWord64 :: CounterSample Word64
nextWord64 = CounterSample nextWord64FromGen
{-# INLINE nextWord64 #-}

nextDouble :: CounterSample Double
nextDouble = wordSample wordToUnit53
{-# INLINE nextDouble #-}

nextOpenDouble :: CounterSample Double
nextOpenDouble = wordSample wordToOpenUnit53
{-# INLINE nextOpenDouble #-}

wordSample :: (Word64 -> a) -> CounterSample a
wordSample transform =
  CounterSample $ \generator -> case nextWord64FromGen generator of (word, nextGenerator) -> (transform word, nextGenerator)
{-# INLINE wordSample #-}

nextGaussian :: CounterSample Double
nextGaussian = CounterSample go
  where
    go !generator
      | cgHasBufferedGaussian generator =
          ( cgBufferedGaussian generator,
            generator
              { cgHasBufferedGaussian = False,
                cgBufferedGaussian = 0.0
              }
          )
      | otherwise =
          case nextWord64FromGen generator of
            (word0, generator1) ->
              case nextWord64FromGen generator1 of
                (word1, generator2) ->
                  let !u = twiceOpenUnit word0 - 1.0
                      !v = twiceOpenUnit word1 - 1.0
                      !s = u * u + v * v
                   in if s <= 0.0 || s >= 1.0
                        then go generator2
                        else
                          let !multiplier = sqrt ((-2.0 * log s) / s)
                              !z0 = u * multiplier
                              !z1 = v * multiplier
                              !nextGenerator =
                                generator2
                                  { cgHasBufferedGaussian = True,
                                    cgBufferedGaussian = z1
                                  }
                           in (z0, nextGenerator)
{-# INLINE nextGaussian #-}

sampleGumbel :: CounterSample Double
sampleGumbel = do
  u <- nextOpenDouble
  pure $! negate (log (negate (log u)))
{-# INLINE sampleGumbel #-}

sampleConcrete :: Double -> [Double] -> CounterSample [Double]
sampleConcrete temperature weights =
  sampleConcreteFromLogits temperature (nonNegativeWeightsToLogits "sampleConcrete" weights)
{-# INLINE sampleConcrete #-}

sampleConcreteFromLogits :: Double -> [Double] -> CounterSample [Double]
sampleConcreteFromLogits !temperature logits =
  let !validLogits =
        SampleInput.expectSampleInput $
          SampleInput.validatePositiveTemperature "sampleConcrete" temperature
            *> SampleInput.validatePossibleLogits "sampleConcrete" logits
   in do
      perturbed <- traverse perturb validLogits
      pure $! softmaxStable (map (/ temperature) perturbed)
  where
    perturb logit
      | logit == negativeInfinity = pure negativeInfinity
      | otherwise = do
          g <- sampleGumbel
          pure $! logit + g
{-# INLINE sampleConcreteFromLogits #-}

sampleCategoricalGumbelMax :: [Double] -> CounterSample Int
sampleCategoricalGumbelMax weights =
  sampleCategoricalGumbelMaxFromLogits (nonNegativeWeightsToLogits "sampleCategoricalGumbelMax" weights)
{-# INLINE sampleCategoricalGumbelMax #-}

sampleCategoricalGumbelMaxFromLogits :: [Double] -> CounterSample Int
sampleCategoricalGumbelMaxFromLogits logits =
  let !validLogits =
        SampleInput.expectSampleInput $
          SampleInput.validatePossibleLogits "sampleCategoricalGumbelMax" logits
   in do
      scores <- traverse perturbIndexed (zip [0 ..] validLogits)
      pure $! fst (maximumBy (comparing snd) scores)
  where
    perturbIndexed :: (Int, Double) -> CounterSample (Int, Double)
    perturbIndexed (index, logit)
      | logit == negativeInfinity = pure (index, negativeInfinity)
      | otherwise = do
          g <- sampleGumbel
          pure $! (index, logit + g)
{-# INLINE sampleCategoricalGumbelMaxFromLogits #-}

sampleGumbelTopK :: Int -> [Double] -> CounterSample [Int]
sampleGumbelTopK count weights =
  sampleGumbelTopKFromLogits count (nonNegativeWeightsToLogits "sampleGumbelTopK" weights)
{-# INLINE sampleGumbelTopK #-}

sampleGumbelTopKFromLogits :: Int -> [Double] -> CounterSample [Int]
sampleGumbelTopKFromLogits !count logits
  | count == 0 =
      pure []
  | otherwise =
      let !validLogits =
            SampleInput.expectSampleInput $
              SampleInput.validateTopKLogits "sampleGumbelTopK" count logits
       in do
          scores <- traverse perturbIndexed (zip [0 ..] validLogits)
          let !sorted =
                sortBy
                  (flip (comparing snd))
                  (filter ((/= negativeInfinity) . snd) scores)
          pure $! map fst (take count sorted)
  where
    perturbIndexed :: (Int, Double) -> CounterSample (Int, Double)
    perturbIndexed (index, logit)
      | logit == negativeInfinity = pure (index, negativeInfinity)
      | otherwise = do
          g <- sampleGumbel
          pure $! (index, logit + g)
{-# INLINE sampleGumbelTopKFromLogits #-}

diagMetric :: Euclidean v => v -> DiagMetric v
diagMetric massDiag =
  validateMassDiag massDiag `seq`
    DiagMetric massDiag (emap recip massDiag)
{-# INLINE diagMetric #-}

identityMetric :: Euclidean v => v -> DiagMetric v
identityMetric prototype =
  let !ones = emap (const 1.0) prototype
   in DiagMetric ones ones
{-# INLINE identityMetric #-}

defaultNUTSConfig :: DiagMetric v -> NUTSConfig v
defaultNUTSConfig metric =
  NUTSConfig
    { nutsStepSize = 0.25,
      nutsMaxDepth = 10,
      nutsMaxDeltaEnergy = 1000.0,
      nutsMetric = metric
    }
{-# INLINE defaultNUTSConfig #-}

essStep :: Euclidean v => CounterSample v -> (v -> Double) -> v -> CounterSample v
essStep samplePrior logLikelihood current =
  let !logCurrent = logLikelihood current
   in if not (isFinite logCurrent)
        then error "essStep: current log likelihood must be finite"
        else do
          priorDirection <- samplePrior
          theta0 <- fmap (\u -> (2.0 * pi * u) - pi) nextOpenDouble
          threshold <- fmap (\u -> logCurrent + log u) nextOpenDouble
          let !thetaMin0 = theta0 - (2.0 * pi)
              !thetaMax0 = theta0
          shrinkBracket threshold priorDirection theta0 thetaMin0 thetaMax0
  where
    propose !theta !direction =
      eadd
        (escale (cos theta) current)
        (escale (sin theta) direction)

    shrinkBracket !threshold !direction !theta !thetaMin !thetaMax =
      let !proposal = propose theta direction
          !logProposal = logLikelihood proposal
       in if isFinite logProposal && logProposal > threshold
            then pure proposal
            else
              let !(thetaMin', thetaMax') =
                    if theta < 0.0
                      then (theta, thetaMax)
                      else (thetaMin, theta)
               in if thetaMax' <= thetaMin'
                    then pure current
                    else do
                      theta' <- fmap (\u -> thetaMin' + u * (thetaMax' - thetaMin')) nextOpenDouble
                      shrinkBracket threshold direction theta' thetaMin' thetaMax'
{-# INLINE essStep #-}

findReasonableStepSize ::
  Euclidean v =>
  DiagMetric v ->
  (v -> (Double, v)) ->
  v ->
  CounterSample Double
findReasonableStepSize metric target current = do
  let !state0 = evaluateTargetState "findReasonableStepSize" target current
      !(TargetState _ logp0 grad0) = state0
      !logThreshold = log 0.5
  momentum0 <- sampleMomentum metric
  pure $! refine logp0 grad0 momentum0 1.0 logThreshold
  where
    refine !logp0 !grad0 !momentum0 !epsilon !logThreshold =
      let !(_, momentum1, logp1, _) =
            leapfrog metric target epsilon current momentum0 grad0
          !joint0 = logp0 - kineticEnergy metric momentum0
          !joint1 = logp1 - kineticEnergy metric momentum1
          !logAccept = joint1 - joint0
          !direction =
            if isFinite logAccept && logAccept > logThreshold
              then (1 :: Int)
              else (-1 :: Int)
          !epsilon' =
            if direction > 0
              then epsilon * 2.0
              else epsilon * 0.5
          !continueScaling =
            if direction > 0
              then isFinite logAccept && logAccept > logThreshold && epsilon' < 1.0e6
              else (not (isFinite logAccept) || logAccept < logThreshold) && epsilon' > 1.0e-6
       in if continueScaling
            then refine logp0 grad0 momentum0 epsilon' logThreshold
            else epsilon
{-# INLINE findReasonableStepSize #-}

nutsStep ::
  Euclidean v =>
  NUTSConfig v ->
  (v -> (Double, v)) ->
  v ->
  CounterSample (NUTSResult v)
nutsStep config target current = do
  validateNUTSConfig config `seq` pure ()
  let !state0 = evaluateTargetState "nutsStep" target current
      !(TargetState _ logp0 grad0) = state0
  momentum0 <- sampleMomentum metricValue
  sliceU <- nextOpenDouble
  let !joint0 = logp0 - kineticEnergy metricValue momentum0
      !logSlice = joint0 + log sliceU
      !initialTree =
        Tree
          { treeQMinus = current,
            treePMinus = momentum0,
            treeGradMinus = grad0,
            treeQPlus = current,
            treePPlus = momentum0,
            treeGradPlus = grad0,
            treeProposal = state0,
            treeValidCount = 1,
            treeContinue = True,
            treeAlphaSum = 0.0,
            treeAlphaCount = 0,
            treeLeapfrogCount = 0
          }
  growTree joint0 logSlice 0 initialTree
  where
    metricValue = nutsMetric config

    growTree !joint0 !logSlice !depth !tree
      | depth >= nutsMaxDepth config || not (treeContinue tree) =
          pure (finalize depth tree)
      | otherwise = do
          direction <- sampleDirection
          subtree <-
            if direction < 0
              then
                buildTree
                  config
                  target
                  logSlice
                  joint0
                  direction
                  depth
                  (treeQMinus tree)
                  (treePMinus tree)
                  (treeGradMinus tree)
              else
                buildTree
                  config
                  target
                  logSlice
                  joint0
                  direction
                  depth
                  (treeQPlus tree)
                  (treePPlus tree)
                  (treeGradPlus tree)

          proposal' <-
            chooseProposal
              (treeProposal tree)
              (treeValidCount tree)
              (treeProposal subtree)
              (treeValidCount subtree)

          let !qMinus' =
                if direction < 0
                  then treeQMinus subtree
                  else treeQMinus tree
              !pMinus' =
                if direction < 0
                  then treePMinus subtree
                  else treePMinus tree
              !gradMinus' =
                if direction < 0
                  then treeGradMinus subtree
                  else treeGradMinus tree
              !qPlus' =
                if direction < 0
                  then treeQPlus tree
                  else treeQPlus subtree
              !pPlus' =
                if direction < 0
                  then treePPlus tree
                  else treePPlus subtree
              !gradPlus' =
                if direction < 0
                  then treeGradPlus tree
                  else treeGradPlus subtree
              !valid' = treeValidCount tree + treeValidCount subtree
              !continue' =
                treeContinue tree
                  && treeContinue subtree
                  && not (isUTurn metricValue qMinus' qPlus' pMinus' pPlus')
              !alphaSum' = treeAlphaSum tree + treeAlphaSum subtree
              !alphaCount' = treeAlphaCount tree + treeAlphaCount subtree
              !leapfrogCount' = treeLeapfrogCount tree + treeLeapfrogCount subtree
              !tree' =
                Tree
                  { treeQMinus = qMinus',
                    treePMinus = pMinus',
                    treeGradMinus = gradMinus',
                    treeQPlus = qPlus',
                    treePPlus = pPlus',
                    treeGradPlus = gradPlus',
                    treeProposal = proposal',
                    treeValidCount = valid',
                    treeContinue = continue',
                    treeAlphaSum = alphaSum',
                    treeAlphaCount = alphaCount',
                    treeLeapfrogCount = leapfrogCount'
                  }
          growTree joint0 logSlice (depth + 1) tree'

    finalize :: Int -> Tree vertex -> NUTSResult vertex
    finalize !depth !tree =
      let TargetState finalPosition finalLogDensity finalGradient = treeProposal tree
          !acceptanceRate =
            if treeAlphaCount tree <= 0
              then 0.0
              else treeAlphaSum tree / fromIntegral (treeAlphaCount tree)
       in NUTSResult
            { nutsPosition = finalPosition,
              nutsLogDensity = finalLogDensity,
              nutsGradient = finalGradient,
              nutsAcceptanceRate = acceptanceRate,
              nutsTreeDepthUsed = depth,
              nutsLeapfrogSteps = treeLeapfrogCount tree
            }
{-# INLINE nutsStep #-}

sobolTable64 :: SobolTable
sobolTable64 =
  SobolTable
    (dimension1Directions : map sobolDirectionWordsFromPolynomial sobolPolynomials64)
{-# NOINLINE sobolTable64 #-}

sobolPoint :: SobolTable -> Word64 -> [Double]
sobolPoint (SobolTable directionTable) pointIndex =
  map (wordToUnit53 . sobolCoordinateWord pointIndex) directionTable
{-# INLINE sobolPoint #-}

owenScrambledSobolPoint :: CounterGen -> SobolTable -> Word64 -> [Double]
owenScrambledSobolPoint generator (SobolTable directionTable) pointIndex =
  let !key0 = cgKey0 generator
      !key1 = cgKey1 generator
      scrambleOne !dimensionIndex !directions =
        wordToUnit53
          (owenScrambleWord key0 key1 dimensionIndex (sobolCoordinateWord pointIndex directions))
   in zipWith scrambleOne [0 ..] directionTable
{-# INLINE owenScrambledSobolPoint #-}

rqmcAverage :: SobolTable -> Int -> CounterGen -> ([Double] -> Double) -> Double
rqmcAverage table count generator integrand
  | count <= 0 =
      error "rqmcAverage: count must be > 0"
  | otherwise =
      let go !index !acc
            | index > count =
                acc / fromIntegral count
            | otherwise =
                let !point = owenScrambledSobolPoint generator table (fromIntegral index)
                    !value = integrand point
                 in go (index + 1) (acc + value)
       in go 1 0.0
{-# INLINE rqmcAverage #-}

rqmcReplicateMeans :: Int -> SobolTable -> Int -> CounterGen -> ([Double] -> Double) -> [Double]
rqmcReplicateMeans replicateCount table count generator integrand
  | replicateCount <= 0 = []
  | otherwise =
      map
        (\replicateIndex ->
           rqmcAverage
             table
             count
             (deriveCounterGen (fromIntegral replicateIndex) generator)
             integrand)
        [0 .. replicateCount - 1]
{-# INLINE rqmcReplicateMeans #-}

nextWord64FromGen :: CounterGen -> (Word64, CounterGen)
nextWord64FromGen generator
  | cgHasBufferedWord generator =
      ( cgBufferedWord generator,
        generator
          { cgHasBufferedWord = False,
            cgBufferedWord = 0
          }
      )
  | otherwise =
      let !(word0, word1) =
            threefry2x64
              (cgBlock generator, 0)
              (cgKey0 generator, cgKey1 generator)
          !nextGenerator =
            generator
              { cgBlock = cgBlock generator + 1,
                cgHasBufferedWord = True,
                cgBufferedWord = word1
              }
       in (word0, nextGenerator)
{-# INLINE nextWord64FromGen #-}

threefry2x64 :: (Word64, Word64) -> (Word64, Word64) -> (Word64, Word64)
threefry2x64 (!counter0, !counter1) (!key0, !key1) =
  let !parity = threefryParity `xor` key0 `xor` key1
      !x0Start = counter0 + key0
      !x1Start = counter1 + key1

      !(x0a, x1a) = mix4 16 42 12 31 x0Start x1Start
      !x0b = x0a + key1
      !x1b = x1a + parity + 1

      !(x0c, x1c) = mix4 16 32 24 21 x0b x1b
      !x0d = x0c + parity
      !x1d = x1c + key0 + 2

      !(x0e, x1e) = mix4 16 42 12 31 x0d x1d
      !x0f = x0e + key0
      !x1f = x1e + key1 + 3

      !(x0g, x1g) = mix4 16 32 24 21 x0f x1f
      !x0h = x0g + key1
      !x1h = x1g + parity + 4

      !(x0i, x1i) = mix4 16 42 12 31 x0h x1h
      !x0j = x0i + parity
      !x1j = x1i + key0 + 5
   in (x0j, x1j)
{-# INLINE threefry2x64 #-}

mix4 :: Int -> Int -> Int -> Int -> Word64 -> Word64 -> (Word64, Word64)
mix4 !r0 !r1 !r2 !r3 !x0 !x1 =
  let !(y0, y1) = mixRound r0 x0 x1
      !(z0, z1) = mixRound r1 y0 y1
      !(u0, u1) = mixRound r2 z0 z1
   in mixRound r3 u0 u1
{-# INLINE mix4 #-}

mixRound :: Int -> Word64 -> Word64 -> (Word64, Word64)
mixRound !rotation !x0 !x1 =
  let !y0 = x0 + x1
      !y1 = rotateL x1 rotation `xor` y0
   in (y0, y1)
{-# INLINE mixRound #-}

sampleDirection :: CounterSample Int
sampleDirection = do
  bit <- nextWord64
  pure $! if testBit bit 0 then 1 else (-1)
{-# INLINE sampleDirection #-}

sampleMomentum :: Euclidean v => DiagMetric v -> CounterSample v
sampleMomentum (DiagMetric massDiag _) = do
  standard <- etraverse (const nextGaussian) massDiag
  pure $! ezipWith (\mass z -> sqrt mass * z) massDiag standard
{-# INLINE sampleMomentum #-}

evaluateTargetState ::
  Euclidean v =>
  String ->
  (v -> (Double, v)) ->
  v ->
  TargetState v
evaluateTargetState label target position =
  let !(logDensityValue, gradientValue) = target position
   in if not (isFinite logDensityValue) || not (allFinite gradientValue)
        then error (label ++ ": target must return finite log density and gradient")
        else TargetState position logDensityValue gradientValue
{-# INLINE evaluateTargetState #-}

leapfrog ::
  Euclidean v =>
  DiagMetric v ->
  (v -> (Double, v)) ->
  Double ->
  v ->
  v ->
  v ->
  (v, v, Double, v)
leapfrog (DiagMetric _ invMassDiag) target !epsilon !position !momentum !gradient0 =
  let !momentumHalf = eadd momentum (escale (0.5 * epsilon) gradient0)
      !velocity = ezipWith (*) invMassDiag momentumHalf
      !position' = eadd position (escale epsilon velocity)
      !(logDensity', gradient') = target position'
      !momentum' = eadd momentumHalf (escale (0.5 * epsilon) gradient')
   in (position', momentum', logDensity', gradient')
{-# INLINE leapfrog #-}

kineticEnergy :: Euclidean v => DiagMetric v -> v -> Double
kineticEnergy (DiagMetric _ invMassDiag) momentum =
  0.5
    * efoldZipWith'
      (\acc invMass component -> acc + invMass * component * component)
      0.0
      invMassDiag
      momentum
{-# INLINE kineticEnergy #-}

isUTurn :: Euclidean v => DiagMetric v -> v -> v -> v -> v -> Bool
isUTurn (DiagMetric _ invMassDiag) qMinus qPlus pMinus pPlus =
  let !delta = esub qPlus qMinus
      !velocityMinus = ezipWith (*) invMassDiag pMinus
      !velocityPlus = ezipWith (*) invMassDiag pPlus
   in edot delta velocityMinus < 0.0 || edot delta velocityPlus < 0.0
{-# INLINE isUTurn #-}

buildTree ::
  Euclidean v =>
  NUTSConfig v ->
  (v -> (Double, v)) ->
  Double ->
  Double ->
  Int ->
  Int ->
  v ->
  v ->
  v ->
  CounterSample (Tree v)
buildTree config target !logSlice !joint0 !direction !depth !position !momentum !gradient0
  | depth <= 0 = do
      let !epsilon = fromIntegral direction * nutsStepSize config
          !(position1, momentum1, logp1, gradient1) =
            leapfrog (nutsMetric config) target epsilon position momentum gradient0
          !joint1 = logp1 - kineticEnergy (nutsMetric config) momentum1
          !stateValid =
            isFinite logp1
              && isFinite joint1
              && allFinite gradient1
              && allFinite momentum1
          !isValid = stateValid && logSlice <= joint1
          !continueHere =
            stateValid && logSlice < joint1 + nutsMaxDeltaEnergy config
          !acceptance =
            if not stateValid
              then 0.0
              else
                if joint1 >= joint0
                  then 1.0
                  else exp (joint1 - joint0)
          !fallbackLogDensity = fst (target position)
          !proposal =
            if stateValid
              then TargetState position1 logp1 gradient1
              else TargetState position fallbackLogDensity gradient0
      pure
        Tree
          { treeQMinus = position1,
            treePMinus = momentum1,
            treeGradMinus = gradient1,
            treeQPlus = position1,
            treePPlus = momentum1,
            treeGradPlus = gradient1,
            treeProposal = proposal,
            treeValidCount = if isValid then 1 else 0,
            treeContinue = continueHere,
            treeAlphaSum = acceptance,
            treeAlphaCount = 1,
            treeLeapfrogCount = 1
          }
  | otherwise = do
      leftTree <-
        buildTree
          config
          target
          logSlice
          joint0
          direction
          (depth - 1)
          position
          momentum
          gradient0

      if not (treeContinue leftTree)
        then pure leftTree
        else do
          rightTree <-
            if direction < 0
              then
                buildTree
                  config
                  target
                  logSlice
                  joint0
                  direction
                  (depth - 1)
                  (treeQMinus leftTree)
                  (treePMinus leftTree)
                  (treeGradMinus leftTree)
              else
                buildTree
                  config
                  target
                  logSlice
                  joint0
                  direction
                  (depth - 1)
                  (treeQPlus leftTree)
                  (treePPlus leftTree)
                  (treeGradPlus leftTree)

          proposal' <-
            chooseProposal
              (treeProposal leftTree)
              (treeValidCount leftTree)
              (treeProposal rightTree)
              (treeValidCount rightTree)

          let !qMinus' =
                if direction < 0
                  then treeQMinus rightTree
                  else treeQMinus leftTree
              !pMinus' =
                if direction < 0
                  then treePMinus rightTree
                  else treePMinus leftTree
              !gradMinus' =
                if direction < 0
                  then treeGradMinus rightTree
                  else treeGradMinus leftTree
              !qPlus' =
                if direction < 0
                  then treeQPlus leftTree
                  else treeQPlus rightTree
              !pPlus' =
                if direction < 0
                  then treePPlus leftTree
                  else treePPlus rightTree
              !gradPlus' =
                if direction < 0
                  then treeGradPlus leftTree
                  else treeGradPlus rightTree
              !valid' = treeValidCount leftTree + treeValidCount rightTree
              !continue' =
                treeContinue leftTree
                  && treeContinue rightTree
                  && not (isUTurn (nutsMetric config) qMinus' qPlus' pMinus' pPlus')
              !alphaSum' = treeAlphaSum leftTree + treeAlphaSum rightTree
              !alphaCount' = treeAlphaCount leftTree + treeAlphaCount rightTree
              !leapfrogCount' = treeLeapfrogCount leftTree + treeLeapfrogCount rightTree
          pure
            Tree
              { treeQMinus = qMinus',
                treePMinus = pMinus',
                treeGradMinus = gradMinus',
                treeQPlus = qPlus',
                treePPlus = pPlus',
                treeGradPlus = gradPlus',
                treeProposal = proposal',
                treeValidCount = valid',
                treeContinue = continue',
                treeAlphaSum = alphaSum',
                treeAlphaCount = alphaCount',
                treeLeapfrogCount = leapfrogCount'
              }
{-# INLINE buildTree #-}

chooseProposal ::
  TargetState v ->
  Int ->
  TargetState v ->
  Int ->
  CounterSample (TargetState v)
chooseProposal leftProposal !leftCount rightProposal !rightCount
  | rightCount <= 0 = pure leftProposal
  | leftCount <= 0 = pure rightProposal
  | otherwise = do
      u <- nextOpenDouble
      let !threshold = fromIntegral rightCount / fromIntegral (leftCount + rightCount)
      pure $! if u < threshold then rightProposal else leftProposal
{-# INLINE chooseProposal #-}

softmaxStable :: [Double] -> [Double]
softmaxStable =
  SampleInput.expectSampleInput . SampleInput.softmaxStable "softmaxStable"
{-# INLINE softmaxStable #-}

nonNegativeWeightsToLogits :: String -> [Double] -> [Double]
nonNegativeWeightsToLogits label =
  SampleInput.expectSampleInput . SampleInput.weightsToLogits label
{-# INLINE nonNegativeWeightsToLogits #-}

validateMassDiag :: Euclidean v => v -> ()
validateMassDiag massDiag =
  efoldl'
    (\() mass ->
       if not (isFinitePositive mass)
         then error "diagMetric: all diagonal mass entries must be finite and > 0"
         else ())
    ()
    massDiag
{-# INLINE validateMassDiag #-}

validateNUTSConfig :: NUTSConfig v -> ()
validateNUTSConfig config
  | not (isFinitePositive (nutsStepSize config)) =
      error "nutsStep: step size must be finite and > 0"
  | nutsMaxDepth config <= 0 =
      error "nutsStep: max depth must be > 0"
  | not (isFinitePositive (nutsMaxDeltaEnergy config)) =
      error "nutsStep: max delta energy must be finite and > 0"
  | otherwise =
      ()
{-# INLINE validateNUTSConfig #-}

allFinite :: Euclidean v => v -> Bool
allFinite =
  efoldl' (\ok x -> ok && isFinite x) True
{-# INLINE allFinite #-}

eadd :: Euclidean v => v -> v -> v
eadd = ezipWith (+)
{-# INLINE eadd #-}

esub :: Euclidean v => v -> v -> v
esub = ezipWith (-)
{-# INLINE esub #-}

escale :: Euclidean v => Double -> v -> v
escale !scaleValue = emap (scaleValue *)
{-# INLINE escale #-}

edot :: Euclidean v => v -> v -> Double
edot =
  efoldZipWith' (\acc x y -> acc + x * y) 0.0
{-# INLINE edot #-}

zipExactWith :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
zipExactWith transform = go
  where
    go [] [] = []
    go (left : lefts) (right : rights) =
      let !value = transform left right
       in value : go lefts rights
    go _ _ =
      error "Euclidean[List]: shape mismatch"
{-# INLINE zipExactWith #-}

foldZipExactWith' :: (a -> Double -> Double -> a) -> a -> [Double] -> [Double] -> a
foldZipExactWith' step = go
  where
    go !acc [] [] = acc
    go !acc (left : lefts) (right : rights) =
      go (step acc left right) lefts rights
    go _ _ _ =
      error "Euclidean[List]: shape mismatch"
{-# INLINE foldZipExactWith' #-}

sobolCoordinateWord :: Word64 -> [Word64] -> Word64
sobolCoordinateWord pointIndex directions =
  let !gray = pointIndex `xor` shiftR pointIndex 1
   in go gray directions 0
  where
    go :: Word64 -> [Word64] -> Word64 -> Word64
    go !bits remainingDirections !acc =
      case remainingDirections of
        [] -> acc
        direction : moreDirections ->
          let !acc' =
                if bits .&. 1 == 1
                  then acc `xor` direction
                  else acc
           in go (shiftR bits 1) moreDirections acc'
{-# INLINE sobolCoordinateWord #-}

owenScrambleWord :: Word64 -> Word64 -> Int -> Word64 -> Word64
owenScrambleWord !key0 !key1 !dimensionIndex !rawWord =
  go 63 0 0
  where
    go !bitIndex !prefix !outWord
      | bitIndex < 0 = outWord
      | otherwise =
          let !rawBit = if testBit rawWord bitIndex then 1 else 0 :: Word64
              !depth = fromIntegral (63 - bitIndex) :: Word64
              !counter0 = prefix
              !counter1 =
                (fromIntegral dimensionIndex `shiftL` 32) .|. depth
              !(randomWord, _) = threefry2x64 (counter0, counter1) (key0, key1)
              !scrambleBit = randomWord .&. 1
              !outBit = rawBit `xor` scrambleBit
              !prefix' = (prefix `shiftL` 1) .|. outBit
              !outWord' =
                if outBit == 1
                  then setBit outWord bitIndex
                  else outWord
           in go (bitIndex - 1) prefix' outWord'
{-# INLINE owenScrambleWord #-}

sobolDirectionWordsFromPolynomial :: SobolPolynomial -> [Word64]
sobolDirectionWordsFromPolynomial (SobolPolynomial degree a initialMs)
  | degree <= 0 =
      dimension1Directions
  | otherwise =
      let !initialDirections =
            [m `shiftL` (64 - index) | (index, m) <- zip [1 ..] initialMs]
       in initialDirections ++ generate initialDirections (degree + 1)
  where
    generate directions index
      | index > 64 = []
      | otherwise =
          let !baseIndex = index - degree - 1
              !base = directions !! baseIndex
              !shifted = base `xor` shiftR base degree
              !direction =
                foldl'
                  (\acc k ->
                     if testBit a (degree - 1 - k)
                       then acc `xor` (directions !! (index - k - 1))
                       else acc)
                  shifted
                  [1 .. degree - 1]
           in direction : generate (directions ++ [direction]) (index + 1)
{-# INLINEABLE sobolDirectionWordsFromPolynomial #-}

dimension1Directions :: [Word64]
dimension1Directions =
  map
    (\index -> 1 `shiftL` (64 - index))
    [1 .. 64]
{-# NOINLINE dimension1Directions #-}

sobolPolynomials64 :: [SobolPolynomial]
sobolPolynomials64 =
  [ SobolPolynomial 1 0 [1],
    SobolPolynomial 2 1 [1, 3],
    SobolPolynomial 3 1 [1, 3, 1],
    SobolPolynomial 3 2 [1, 1, 1],
    SobolPolynomial 4 1 [1, 1, 3, 3],
    SobolPolynomial 4 4 [1, 3, 5, 13],
    SobolPolynomial 5 2 [1, 1, 5, 5, 17],
    SobolPolynomial 5 4 [1, 1, 5, 5, 5],
    SobolPolynomial 5 7 [1, 1, 7, 11, 19],
    SobolPolynomial 5 11 [1, 1, 5, 1, 1],
    SobolPolynomial 5 13 [1, 1, 1, 3, 11],
    SobolPolynomial 5 14 [1, 3, 5, 5, 31],
    SobolPolynomial 6 1 [1, 3, 3, 9, 7, 49],
    SobolPolynomial 6 13 [1, 1, 1, 15, 21, 21],
    SobolPolynomial 6 16 [1, 3, 1, 13, 27, 49],
    SobolPolynomial 6 19 [1, 1, 1, 15, 7, 5],
    SobolPolynomial 6 22 [1, 3, 1, 15, 13, 25],
    SobolPolynomial 6 25 [1, 1, 5, 5, 19, 61],
    SobolPolynomial 7 1 [1, 3, 7, 11, 23, 15, 103],
    SobolPolynomial 7 4 [1, 3, 7, 13, 13, 15, 69],
    SobolPolynomial 7 7 [1, 1, 3, 13, 7, 35, 63],
    SobolPolynomial 7 8 [1, 3, 5, 9, 1, 25, 53],
    SobolPolynomial 7 14 [1, 3, 1, 13, 9, 35, 107],
    SobolPolynomial 7 19 [1, 3, 1, 5, 27, 61, 31],
    SobolPolynomial 7 21 [1, 1, 5, 11, 19, 41, 61],
    SobolPolynomial 7 28 [1, 3, 5, 3, 3, 13, 69],
    SobolPolynomial 7 31 [1, 1, 7, 13, 1, 19, 1],
    SobolPolynomial 7 32 [1, 3, 7, 5, 13, 19, 59],
    SobolPolynomial 7 37 [1, 1, 3, 9, 25, 29, 41],
    SobolPolynomial 7 41 [1, 3, 5, 13, 23, 1, 55],
    SobolPolynomial 7 42 [1, 3, 7, 3, 13, 59, 17],
    SobolPolynomial 7 50 [1, 3, 1, 3, 5, 53, 69],
    SobolPolynomial 7 55 [1, 1, 5, 5, 23, 33, 13],
    SobolPolynomial 7 56 [1, 1, 7, 7, 1, 61, 123],
    SobolPolynomial 7 59 [1, 1, 7, 9, 13, 61, 49],
    SobolPolynomial 7 62 [1, 3, 3, 5, 3, 55, 33],
    SobolPolynomial 8 14 [1, 3, 1, 15, 31, 13, 49, 245],
    SobolPolynomial 8 21 [1, 3, 5, 15, 31, 59, 63, 97],
    SobolPolynomial 8 22 [1, 3, 1, 11, 11, 11, 77, 249],
    SobolPolynomial 8 38 [1, 3, 1, 11, 27, 43, 71, 9],
    SobolPolynomial 8 47 [1, 1, 7, 15, 21, 11, 81, 45],
    SobolPolynomial 8 49 [1, 3, 7, 3, 25, 31, 65, 79],
    SobolPolynomial 8 50 [1, 3, 1, 1, 19, 11, 3, 205],
    SobolPolynomial 8 52 [1, 1, 5, 9, 19, 21, 29, 157],
    SobolPolynomial 8 56 [1, 3, 7, 11, 1, 33, 89, 185],
    SobolPolynomial 8 67 [1, 3, 3, 3, 15, 9, 79, 71],
    SobolPolynomial 8 70 [1, 3, 7, 11, 15, 39, 119, 27],
    SobolPolynomial 8 84 [1, 1, 3, 1, 11, 31, 97, 225],
    SobolPolynomial 8 97 [1, 1, 1, 3, 23, 43, 57, 177],
    SobolPolynomial 8 103 [1, 3, 7, 7, 17, 17, 37, 71],
    SobolPolynomial 8 115 [1, 3, 1, 5, 27, 63, 123, 213],
    SobolPolynomial 8 122 [1, 1, 3, 5, 11, 43, 53, 133],
    SobolPolynomial 9 8 [1, 3, 5, 5, 29, 17, 47, 173, 479],
    SobolPolynomial 9 13 [1, 3, 3, 11, 3, 1, 109, 9, 69],
    SobolPolynomial 9 16 [1, 1, 1, 5, 17, 39, 23, 5, 343],
    SobolPolynomial 9 22 [1, 3, 1, 5, 25, 15, 31, 103, 499],
    SobolPolynomial 9 25 [1, 1, 1, 11, 11, 17, 63, 105, 183],
    SobolPolynomial 9 44 [1, 1, 5, 11, 9, 29, 97, 231, 363],
    SobolPolynomial 9 47 [1, 1, 5, 15, 19, 45, 41, 7, 383],
    SobolPolynomial 9 52 [1, 3, 7, 7, 31, 19, 83, 137, 221],
    SobolPolynomial 9 55 [1, 1, 1, 3, 23, 15, 111, 223, 83],
    SobolPolynomial 9 59 [1, 1, 5, 13, 31, 15, 55, 25, 161],
    SobolPolynomial 9 62 [1, 1, 3, 13, 25, 47, 39, 87, 257]
  ]
{-# NOINLINE sobolPolynomials64 #-}

wordToUnit53 :: Word64 -> Double
wordToUnit53 word =
  fromIntegral (shiftR word 11) * reciprocalTwoTo53
{-# INLINE wordToUnit53 #-}

wordToOpenUnit53 :: Word64 -> Double
wordToOpenUnit53 word =
  (fromIntegral (shiftR word 11) + 0.5) * reciprocalTwoTo53
{-# INLINE wordToOpenUnit53 #-}

twiceOpenUnit :: Word64 -> Double
twiceOpenUnit word =
  (fromIntegral (shiftR word 11) + 0.5) * reciprocalTwoTo52
{-# INLINE twiceOpenUnit #-}

splitMix64 :: Word64 -> Word64
splitMix64 seed0 =
  let !seed1 = (seed0 `xor` shiftR seed0 30) * 0xBF58476D1CE4E5B9
      !seed2 = (seed1 `xor` shiftR seed1 27) * 0x94D049BB133111EB
   in seed2 `xor` shiftR seed2 31
{-# INLINE splitMix64 #-}

threefryParity :: Word64
threefryParity = 0x1BD11BDAA9FC1A22
{-# INLINE threefryParity #-}

goldenGamma :: Word64
goldenGamma = 0x9E3779B97F4A7C15
{-# INLINE goldenGamma #-}

reciprocalTwoTo53 :: Double
reciprocalTwoTo53 = 1.0 / 9007199254740992.0
{-# INLINE reciprocalTwoTo53 #-}

reciprocalTwoTo52 :: Double
reciprocalTwoTo52 = 1.0 / 4503599627370496.0
{-# INLINE reciprocalTwoTo52 #-}
