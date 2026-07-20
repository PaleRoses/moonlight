{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Probability.Sample
  ( -- * Entropy and compatibility surface
    PureGen,
    genFromSeed,
    splitGen,
    splitN,
    SampleT,
    Sample,
    runSampleTWith,
    runSampleWith,
    evalSampleT,
    evalSample,
    liftSampleT,
    nextWord64FromGen,
    nextIntFromGen,
    nextWord64,
    nextInt,
    nextProb,
    nextDouble,
    nextOpenDouble,
    nextGaussian,
    nextExponential,
    nextGumbel,
    sampleGamma,
    sampleBeta,
    sampleDirichlet,
    sampleDirichletVector,
    sampleN,
    sampleContinuous,
    sampleCategorical,

    -- * Static categorical sampling
    AliasTable,
    buildAliasTable,
    buildAliasTableFromList,
    sampleAliasIndex,
    sampleAlias,

    -- * Dynamic categorical sampling
    MutableFenwick,
    buildMutableFenwick,
    buildMutableFenwickFromList,
    fenwickLength,
    fenwickTotal,
    fenwickWeight,
    writeFenwickWeight,
    adjustFenwickWeight,
    selectFenwickIndex,
    sampleFenwickIndex,
    sampleFenwick,

    -- * Simplex / compositional geometry
    closure,
    clr,
    inverseClr,
    ilr,
    inverseIlr,
    perturb,
    aitchisonScale,
    aitchisonInner,
    aitchisonNorm,
    aitchisonDistance,

    -- * Gaussian measures and logistic-normal simplex models
    GaussianMeasure,
    compileGaussianMeasure,
    sampleGaussianMeasure,
    LogisticNormal,
    compileLogisticNormal,
    sampleLogisticNormal,

    -- * Universal one-dimensional samplers
    AdaptiveRejection1D,
    compileAdaptiveRejection,
    sampleAdaptiveRejection,
    RatioOfUniforms1D (..),
    sampleRatioOfUniforms,

    -- * Count kernels
    samplePoisson,
    sampleBinomial,

    -- * Gumbel / Concrete
    sampleGumbelMaxIndex,
    sampleGumbelMaxIndexFromLogWeights,
    sampleGumbelTopKIndicesFromLogWeights,
    sampleConcrete,
    sampleConcreteFromLogWeights,

    -- * Inference kernels
    ellipticalSlice,
    DifferentiableTarget (..),
    NUTSConfig (..),
    NUTSStats (..),
    defaultNUTSConfig,
    findReasonableNUTSStepSize,
    nutsTransition,

    -- * Random-access entropy and randomized QMC
    CounterGen,
    counterGenFromSeed,
    splitCounterGen,
    counterWord64At,
    counterDoubleAt,
    RandomizedSobol1D,
    sampleRandomizedSobol1D,
    sobol1DAt,
    randomizedSobol1DAt,
    qmcContinuous1D,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Bits
  ( Bits (xor),
    bit,
    countLeadingZeros,
    finiteBitSize,
    shiftL,
    shiftR,
    (.&.),
    (.|.),
  )
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List (sortBy, unfoldr)
import Data.Vector (Vector)
import Data.Word (Word64)
import Moonlight.Probability.Core (Prob, probValue)
import Moonlight.Probability.Core.Internal (Prob (..))
import Moonlight.Probability.Distribution (HasContDistr, distributionQuantile)
import Moonlight.Probability.Distribution.Categorical (Categorical, categoricalCollapseAt)
import Moonlight.Probability.Sample.Input
  ( isFinite,
    isNonNegativeFinite,
    isStrictlyPositiveFinite,
    negativeInfinity,
  )
import Prelude
import System.Random.SplitMix (SMGen, mkSMGen, splitSMGen, unseedSMGen)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Moonlight.Probability.Sample.Input as SampleInput
import qualified System.Random.SplitMix as SplitMix

type PureGen :: Type
newtype PureGen = PureGen SMGen

instance Eq PureGen where
  left == right = pureGenState left == pureGenState right

type SampleT :: (Type -> Type) -> Type -> Type
newtype SampleT m a = SampleT
  { unSampleT :: PureGen -> m (a, PureGen)
  }

type Sample :: Type -> Type
type Sample = SampleT Identity

runSampleTWith :: SampleT m a -> PureGen -> m (a, PureGen)
runSampleTWith = unSampleT

runSampleWith :: Sample a -> PureGen -> (a, PureGen)
runSampleWith sampleValue generator = runIdentity (runSampleTWith sampleValue generator)

evalSampleT :: Functor m => Word64 -> SampleT m a -> m a
evalSampleT seed sampleValue = fmap fst (runSampleTWith sampleValue (genFromSeed seed))

evalSample :: Word64 -> Sample a -> a
evalSample seed sampleValue = runIdentity (evalSampleT seed sampleValue)

liftSampleT :: Functor m => m a -> SampleT m a
liftSampleT action = SampleT (\generator -> fmap (\value -> (value, generator)) action)

instance Functor m => Functor (SampleT m) where
  fmap transform sampleValue =
    SampleT
      (\generator ->
         fmap
           (\(value, nextGenerator) -> (transform value, nextGenerator))
           (unSampleT sampleValue generator))

instance Monad m => Applicative (SampleT m) where
  pure value = SampleT (\generator -> pure (value, generator))
  SampleT applySample <*> SampleT valueSample =
    SampleT
      (\generator -> do
         (transform, generatorAfterTransform) <- applySample generator
         (value, generatorAfterValue) <- valueSample generatorAfterTransform
         pure (transform value, generatorAfterValue))

instance Monad m => Monad (SampleT m) where
  SampleT valueSample >>= continue =
    SampleT
      (\generator -> do
         (value, nextGenerator) <- valueSample generator
         unSampleT (continue value) nextGenerator)

genFromSeed :: Word64 -> PureGen
genFromSeed = PureGen . mkSMGen

splitGen :: PureGen -> (PureGen, PureGen)
splitGen (PureGen generator) =
  case splitSMGen generator of (leftGenerator, rightGenerator) -> (PureGen leftGenerator, PureGen rightGenerator)

splitN :: Int -> PureGen -> [PureGen]
splitN count =
  take (max 0 count) . unfoldr (Just . splitGen)

nextWord64FromGen :: PureGen -> (Word64, PureGen)
nextWord64FromGen (PureGen generator) =
  case SplitMix.nextWord64 generator of (value, nextGenerator) -> (value, PureGen nextGenerator)

nextIntFromGen :: PureGen -> (Int, PureGen)
nextIntFromGen (PureGen generator) =
  case SplitMix.nextInt generator of (value, nextGenerator) -> (value, PureGen nextGenerator)

nextWord64 :: Applicative m => SampleT m Word64
nextWord64 = SampleT (pure . nextWord64FromGen)

nextInt :: Applicative m => SampleT m Int
nextInt = SampleT (pure . nextIntFromGen)

nextProb :: Applicative m => SampleT m Prob
nextProb = wordToProb <$> nextWord64

nextDouble :: Applicative m => SampleT m Double
nextDouble = probValue <$> nextProb

nextOpenDouble :: Applicative m => SampleT m Double
nextOpenDouble = wordToOpenUnit <$> nextWord64

nextSignedOpenDouble :: Applicative m => SampleT m Double
nextSignedOpenDouble = (\u -> 2.0 * u - 1.0) <$> nextOpenDouble

sampleN :: Monad m => Int -> SampleT m a -> SampleT m [a]
sampleN count = replicateM (max 0 count)

sampleContinuous :: Applicative m => HasContDistr d => d -> SampleT m Double
sampleContinuous distribution = distributionQuantile distribution <$> nextOpenDouble

sampleCategorical :: Applicative m => Categorical a -> SampleT m a
sampleCategorical categorical = (`categoricalCollapseAt` categorical) <$> nextProb

sampleGamma :: Monad m => Double -> SampleT m Double
sampleGamma alpha = exp <$> sampleLogGamma alpha

sampleBeta :: Monad m => Double -> Double -> SampleT m Double
sampleBeta alpha beta
  | not (isStrictlyPositiveFinite alpha) =
      error "sampleBeta: alpha must be finite and strictly positive"
  | not (isStrictlyPositiveFinite beta) =
      error "sampleBeta: beta must be finite and strictly positive"
  | otherwise = do
      logX <- sampleLogGamma alpha
      logY <- sampleLogGamma beta
      pure (clampOpenUnit (exp (logX - logSumExp2 logX logY)))

sampleDirichlet :: Monad m => [Double] -> SampleT m [Double]
sampleDirichlet = fmap VU.toList . sampleDirichletVector . VU.fromList

sampleDirichletVector :: Monad m => VU.Vector Double -> SampleT m (VU.Vector Double)
sampleDirichletVector alphaValues
  | VU.null alphaValues =
      error "sampleDirichlet: empty concentration vector"
  | VU.any (not . isStrictlyPositiveFinite) alphaValues =
      error "sampleDirichlet: concentration parameters must be finite and strictly positive"
  | otherwise = do
      logGammaSamples <-
        sampleDoubleVectorByIndex
          (VU.length alphaValues)
          (\i -> sampleLogGamma (VU.unsafeIndex alphaValues i))
      pure (normalizeLogWeights logGammaSamples)

type AliasTable :: Type -> Type
data AliasTable a = AliasTable
  { aliasItems :: !(Vector a),
    aliasProbabilities :: !(VU.Vector Double),
    aliasFallbacks :: !(VU.Vector Int)
  }

buildAliasTableFromList :: [(a, Double)] -> Either String (AliasTable a)
buildAliasTableFromList weightedValues =
  buildAliasTable
    (VB.fromList (fmap fst weightedValues))
    (VU.fromList (fmap snd weightedValues))

buildAliasTable :: Vector a -> VU.Vector Double -> Either String (AliasTable a)
buildAliasTable items weights = do
  let !n = VB.length items
  !totalWeight <- validateWeightTable "buildAliasTable" n weights
  let !scaledWeights =
        VU.map
          (\weight -> weight * fromIntegral n / totalWeight)
          weights
      !(smallStack, largeStack) =
        VU.ifoldl'
          (\(!smallAcc, !largeAcc) index scaledWeight ->
             if scaledWeight < 1.0
               then (index : smallAcc, largeAcc)
               else (smallAcc, index : largeAcc))
          ([], [])
          scaledWeights
      (!probabilities, !fallbacks) = runST $ do
        scaledMutable <- VU.thaw scaledWeights
        probabilityMutable <- VUM.replicate n 1.0
        fallbackMutable <- VU.thaw (VU.enumFromN 0 n)
        let finalize [] [] = pure ()
            finalize (index : rest) large = do
              VUM.unsafeWrite probabilityMutable index 1.0
              VUM.unsafeWrite fallbackMutable index index
              finalize rest large
            finalize small (index : rest) = do
              VUM.unsafeWrite probabilityMutable index 1.0
              VUM.unsafeWrite fallbackMutable index index
              finalize small rest
            go [] [] = pure ()
            go small [] = finalize small []
            go [] large = finalize [] large
            go (smallIndex : remainingSmall) (largeIndex : remainingLarge) = do
              smallProbability <- VUM.unsafeRead scaledMutable smallIndex
              largeProbability <- VUM.unsafeRead scaledMutable largeIndex
              VUM.unsafeWrite probabilityMutable smallIndex smallProbability
              VUM.unsafeWrite fallbackMutable smallIndex largeIndex
              let !updatedLargeProbability = largeProbability + smallProbability - 1.0
              VUM.unsafeWrite scaledMutable largeIndex updatedLargeProbability
              if updatedLargeProbability < 1.0
                then go (largeIndex : remainingSmall) remainingLarge
                else go remainingSmall (largeIndex : remainingLarge)
        go smallStack largeStack
        builtProbabilities <- VU.unsafeFreeze probabilityMutable
        builtFallbacks <- VU.unsafeFreeze fallbackMutable
        pure (builtProbabilities, builtFallbacks)
  pure
    ( AliasTable
        { aliasItems = items,
          aliasProbabilities = probabilities,
          aliasFallbacks = fallbacks
        }
    )

sampleAliasIndex :: Monad m => AliasTable a -> SampleT m Int
sampleAliasIndex table = do
  let !n = VB.length (aliasItems table)
  !column <- nextIndexBounded n
  !threshold <- nextDouble
  let !probability = VU.unsafeIndex (aliasProbabilities table) column
  pure
    ( if threshold < probability
        then column
        else VU.unsafeIndex (aliasFallbacks table) column
    )

sampleAlias :: Monad m => AliasTable a -> SampleT m a
sampleAlias table = do
  !index <- sampleAliasIndex table
  pure (VB.unsafeIndex (aliasItems table) index)

type MutableFenwick :: Type -> Type -> Type
data MutableFenwick s a = MutableFenwick
  { mutableFenwickItems :: !(Vector a),
    mutableFenwickWeights :: !(VUM.MVector s Double),
    mutableFenwickTree :: !(VUM.MVector s Double)
  }

buildMutableFenwickFromList ::
  PrimMonad m =>
  [(a, Double)] ->
  m (Either String (MutableFenwick (PrimState m) a))
buildMutableFenwickFromList weightedValues =
  buildMutableFenwick
    (VB.fromList (fmap fst weightedValues))
    (VU.fromList (fmap snd weightedValues))

buildMutableFenwick ::
  PrimMonad m =>
  Vector a ->
  VU.Vector Double ->
  m (Either String (MutableFenwick (PrimState m) a))
buildMutableFenwick items weights =
  case validateWeightTable "buildMutableFenwick" (VB.length items) weights of
    Left err -> pure (Left err)
    Right _totalWeight -> do
      let !n = VB.length items
      weightMutable <- VU.thaw weights
      treeMutable <- VUM.replicate (n + 1) 0.0
      let buildAt !index
            | index >= n = pure ()
            | otherwise = do
                fenwickAccumulate treeMutable (index + 1) (VU.unsafeIndex weights index)
                buildAt (index + 1)
      buildAt 0
      pure
        (Right (MutableFenwick items weightMutable treeMutable))

fenwickLength :: MutableFenwick s a -> Int
fenwickLength = VB.length . mutableFenwickItems

fenwickTotal :: PrimMonad m => MutableFenwick (PrimState m) a -> m Double
fenwickTotal fenwick =
  fenwickPrefixSum (mutableFenwickTree fenwick) (fenwickLength fenwick)

fenwickWeight ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  Int ->
  m (Maybe Double)
fenwickWeight fenwick index
  | index < 0 || index >= fenwickLength fenwick = pure Nothing
  | otherwise = Just <$> VUM.unsafeRead (mutableFenwickWeights fenwick) index

writeFenwickWeight ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  Int ->
  Double ->
  m (Either String ())
writeFenwickWeight fenwick index newWeight
  | index < 0 || index >= fenwickLength fenwick =
      pure (Left "writeFenwickWeight: index out of bounds")
  | not (isNonNegativeFinite newWeight) =
      pure (Left "writeFenwickWeight: weights must be finite and non-negative")
  | otherwise = do
      oldWeight <- VUM.unsafeRead (mutableFenwickWeights fenwick) index
      let !delta = newWeight - oldWeight
      VUM.unsafeWrite (mutableFenwickWeights fenwick) index newWeight
      fenwickAccumulate (mutableFenwickTree fenwick) (index + 1) delta
      pure (Right ())

adjustFenwickWeight ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  Int ->
  Double ->
  m (Either String ())
adjustFenwickWeight fenwick index delta
  | index < 0 || index >= fenwickLength fenwick =
      pure (Left "adjustFenwickWeight: index out of bounds")
  | not (isFinite delta) =
      pure (Left "adjustFenwickWeight: delta must be finite")
  | otherwise = do
      oldWeight <- VUM.unsafeRead (mutableFenwickWeights fenwick) index
      let !newWeight = oldWeight + delta
      if newWeight < 0.0 || isNaN newWeight || isInfinite newWeight
        then pure (Left "adjustFenwickWeight: resulting weight must be finite and non-negative")
        else do
          VUM.unsafeWrite (mutableFenwickWeights fenwick) index newWeight
          fenwickAccumulate (mutableFenwickTree fenwick) (index + 1) delta
          pure (Right ())

selectFenwickIndex ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  Double ->
  m (Maybe Int)
selectFenwickIndex fenwick threshold = do
  !totalWeight <- fenwickTotal fenwick
  if not (totalWeight > 0.0) || not (isFinite totalWeight) || threshold < 0.0 || threshold >= totalWeight
    then pure Nothing
    else do
      let !tree = mutableFenwickTree fenwick
          !n = fenwickLength fenwick
          !startingBit = highestPowerOfTwoLE n
          go !index !bitMask !remaining
            | bitMask == 0 = pure index
            | otherwise =
                let !candidateIndex = index + bitMask
                 in if candidateIndex <= n
                      then do
                        candidateMass <- VUM.unsafeRead tree candidateIndex
                        if candidateMass <= remaining
                          then go candidateIndex (bitMask `shiftR` 1) (remaining - candidateMass)
                          else go index (bitMask `shiftR` 1) remaining
                      else go index (bitMask `shiftR` 1) remaining
      Just <$> go 0 startingBit threshold

sampleFenwickIndex ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  SampleT m (Maybe Int)
sampleFenwickIndex fenwick = do
  !totalWeight <- liftSampleT (fenwickTotal fenwick)
  if not (totalWeight > 0.0) || not (isFinite totalWeight)
    then pure Nothing
    else do
      !u <- nextDouble
      liftSampleT (selectFenwickIndex fenwick (u * totalWeight))

sampleFenwick ::
  PrimMonad m =>
  MutableFenwick (PrimState m) a ->
  SampleT m (Maybe a)
sampleFenwick fenwick = do
  sampledIndex <- sampleFenwickIndex fenwick
  pure
    (fmap (\index -> VB.unsafeIndex (mutableFenwickItems fenwick) index) sampledIndex)

closure :: VU.Vector Double -> Either String (VU.Vector Double)
closure composition = do
  validateComposition "closure" composition
  pure (normalizeLogWeights (VU.map log composition))

clr :: VU.Vector Double -> Either String (VU.Vector Double)
clr composition = do
  validateComposition "clr" composition
  let !logComposition = VU.map log composition
      !meanLog = VU.sum logComposition / fromIntegral (VU.length logComposition)
  pure (VU.map (\logPart -> logPart - meanLog) logComposition)

inverseClr :: VU.Vector Double -> VU.Vector Double
inverseClr = normalizeLogWeights

ilr :: VU.Vector Double -> Either String (VU.Vector Double)
ilr composition = do
  validateComposition "ilr" composition
  let !partCount = VU.length composition
  if partCount <= 1
    then pure VU.empty
    else
      let !dimension = partCount - 1
       in pure
            ( VU.create $ do
                coordinates <- VUM.new dimension
                let go !prefixLog !index
                      | index >= dimension = pure ()
                      | otherwise = do
                          let !k = fromIntegral (index + 1) :: Double
                              !logFront = log (VU.unsafeIndex composition index)
                              !prefixLog' = prefixLog + logFront
                              !logNext = log (VU.unsafeIndex composition (index + 1))
                              !coordinate =
                                sqrt (k / (k + 1.0))
                                  * (prefixLog' / k - logNext)
                          VUM.unsafeWrite coordinates index coordinate
                          go prefixLog' (index + 1)
                go 0.0 0
                pure coordinates
            )

inverseIlr :: VU.Vector Double -> VU.Vector Double
inverseIlr coordinates
  | VU.null coordinates = VU.singleton 1.0
  | otherwise =
      let !dimension = VU.length coordinates
          !scaledCoordinates =
            VU.create $ do
              result <- VUM.new dimension
              let go !index
                    | index >= dimension = pure ()
                    | otherwise = do
                        let !k = fromIntegral (index + 1) :: Double
                            !value =
                              VU.unsafeIndex coordinates index
                                / sqrt (k * (k + 1.0))
                        VUM.unsafeWrite result index value
                        go (index + 1)
              go 0
              pure result
          !suffixSums =
            VU.create $ do
              result <- VUM.new dimension
              let go !acc !index
                    | index < 0 = pure ()
                    | otherwise = do
                        let !acc' = acc + VU.unsafeIndex scaledCoordinates index
                        VUM.unsafeWrite result index acc'
                        go acc' (index - 1)
              go 0.0 (dimension - 1)
              pure result
          !clrCoordinates =
            VU.generate
              (dimension + 1)
              (\component ->
                 if component == 0
                   then VU.unsafeIndex suffixSums 0
                   else
                     if component == dimension
                       then negate (fromIntegral dimension) * VU.unsafeIndex scaledCoordinates (dimension - 1)
                       else
                         VU.unsafeIndex suffixSums component
                           - fromIntegral component * VU.unsafeIndex scaledCoordinates (component - 1))
       in inverseClr clrCoordinates

perturb :: VU.Vector Double -> VU.Vector Double -> Either String (VU.Vector Double)
perturb left right = do
  validateComposition "perturb(left)" left
  validateComposition "perturb(right)" right
  requireSameLength "perturb" left right
  pure
    ( normalizeLogWeights
        (VU.zipWith (\leftPart rightPart -> log leftPart + log rightPart) left right)
    )

aitchisonScale :: Double -> VU.Vector Double -> Either String (VU.Vector Double)
aitchisonScale scalar composition = do
  validateComposition "aitchisonScale" composition
  if not (isFinite scalar)
    then Left "aitchisonScale: scale must be finite"
    else
      pure
        ( normalizeLogWeights
            (VU.map (\part -> scalar * log part) composition)
        )

aitchisonInner :: VU.Vector Double -> VU.Vector Double -> Either String Double
aitchisonInner left right = do
  leftClr <- clr left
  rightClr <- clr right
  requireSameLength "aitchisonInner" leftClr rightClr
  pure (dot leftClr rightClr)

aitchisonNorm :: VU.Vector Double -> Either String Double
aitchisonNorm composition = sqrt <$> aitchisonInner composition composition

aitchisonDistance :: VU.Vector Double -> VU.Vector Double -> Either String Double
aitchisonDistance left right = do
  leftIlr <- ilr left
  rightIlr <- ilr right
  requireSameLength "aitchisonDistance" leftIlr rightIlr
  pure
    ( sqrt
        ( VU.sum
            (VU.zipWith (\a b -> let !d = a - b in d * d) leftIlr rightIlr)
        )
    )

type GaussianMeasure :: Type
data GaussianMeasure = GaussianMeasure
  { gaussianDimension :: !Int,
    gaussianMean :: !(VU.Vector Double),
    gaussianCholesky :: !(VU.Vector Double)
  }

compileGaussianMeasure :: VU.Vector Double -> VU.Vector Double -> Either String GaussianMeasure
compileGaussianMeasure meanVector covarianceMatrix = do
  let !dimension = VU.length meanVector
      !expectedCells = dimension * dimension
  if VU.length covarianceMatrix /= expectedCells
    then
      Left
        ( "compileGaussianMeasure: covariance matrix has "
            ++ show (VU.length covarianceMatrix)
            ++ " cells but expected "
            ++ show expectedCells
        )
    else
      if VU.any (not . isFinite) meanVector
        then Left "compileGaussianMeasure: mean coordinates must be finite"
        else do
          validateSymmetricPositiveSemantics "compileGaussianMeasure" dimension covarianceMatrix
          lowerFactor <- choleskyLower "compileGaussianMeasure" dimension covarianceMatrix
          pure
            ( GaussianMeasure
                { gaussianDimension = dimension,
                  gaussianMean = meanVector,
                  gaussianCholesky = lowerFactor
                }
            )

sampleGaussianMeasure :: Monad m => GaussianMeasure -> SampleT m (VU.Vector Double)
sampleGaussianMeasure measure = do
  let !dimension = gaussianDimension measure
      !meanVector = gaussianMean measure
      !lowerFactor = gaussianCholesky measure
  standardNormals <- sampleDoubleVector dimension nextGaussian
  pure
    ( VU.generate
        dimension
        (\rowIndex ->
           let go !acc !columnIndex
                 | columnIndex > rowIndex = acc
                 | otherwise =
                     let !coefficient =
                           VU.unsafeIndex lowerFactor (matrixIndex dimension rowIndex columnIndex)
                         !standardNormal = VU.unsafeIndex standardNormals columnIndex
                      in go (acc + coefficient * standardNormal) (columnIndex + 1)
            in go (VU.unsafeIndex meanVector rowIndex) 0)
    )

type LogisticNormal :: Type
newtype LogisticNormal = LogisticNormal
  { unLogisticNormal :: GaussianMeasure
  }

compileLogisticNormal :: VU.Vector Double -> VU.Vector Double -> Either String LogisticNormal
compileLogisticNormal meanCoordinates covarianceMatrix =
  LogisticNormal <$> compileGaussianMeasure meanCoordinates covarianceMatrix

sampleLogisticNormal :: Monad m => LogisticNormal -> SampleT m (VU.Vector Double)
sampleLogisticNormal model = inverseIlr <$> sampleGaussianMeasure (unLogisticNormal model)

type AdaptiveRejection1D :: Type
data AdaptiveRejection1D = AdaptiveRejection1D
  { arsSupport :: !(Double, Double),
    arsLogDensity :: !(Double -> Double),
    arsDerivative :: !(Double -> Double),
    arsInitialKnots :: !(VU.Vector Double)
  }

compileAdaptiveRejection ::
  (Double -> Double) ->
  (Double -> Double) ->
  (Double, Double) ->
  VU.Vector Double ->
  Either String AdaptiveRejection1D
compileAdaptiveRejection logDensity derivative support@(supportLeft, supportRight) knots
  | not (supportLeft < supportRight) =
      Left "compileAdaptiveRejection: support must satisfy left < right"
  | VU.length knots < 2 =
      Left "compileAdaptiveRejection: at least two initial knots are required"
  | not (strictlyIncreasing knots) =
      Left "compileAdaptiveRejection: initial knots must be strictly increasing"
  | VU.any (\x -> not (x > supportLeft && x < supportRight)) knots =
      Left "compileAdaptiveRejection: initial knots must lie strictly inside support"
  | VU.any (\x -> let y = logDensity x in isNaN y || isInfinite y) knots =
      Left "compileAdaptiveRejection: log-density must be finite at all initial knots"
  | VU.any (\x -> let y = derivative x in isNaN y || isInfinite y) knots =
      Left "compileAdaptiveRejection: derivative must be finite at all initial knots"
  | isInfinite supportLeft && derivative (VU.unsafeIndex knots 0) <= 0.0 =
      Left "compileAdaptiveRejection: leftmost derivative must be positive for left-infinite support"
  | isInfinite supportRight && derivative (VU.unsafeIndex knots (VU.length knots - 1)) >= 0.0 =
      Left "compileAdaptiveRejection: rightmost derivative must be negative for right-infinite support"
  | not (slopesStrictlyDecreasing derivative knots) =
      Left "compileAdaptiveRejection: derivatives at the initial knots must decrease monotonically"
  | otherwise =
      Right
        ( AdaptiveRejection1D
            { arsSupport = support,
              arsLogDensity = logDensity,
              arsDerivative = derivative,
              arsInitialKnots = knots
            }
        )

sampleAdaptiveRejection :: Monad m => AdaptiveRejection1D -> SampleT m Double
sampleAdaptiveRejection model = go (arsInitialKnots model)
  where
    go !knots =
      case buildAdaptiveHull model knots of
        Left err -> error err
        Right hull -> do
          candidate <- sampleAdaptiveHull hull
          acceptanceUniform <- nextOpenDouble
          let !candidateLogDensity = arsLogDensity model candidate
              !candidateUpperHull = adaptiveUpperHullValue hull candidate
          if log acceptanceUniform <= candidateLogDensity - candidateUpperHull
            then pure candidate
            else go (insertSortedUnique candidate knots)

type RatioOfUniforms1D :: Type
data RatioOfUniforms1D = RatioOfUniforms1D
  { rouLogDensity :: !(Double -> Double),
    rouSupport :: !(Double, Double),
    rouUMax :: !Double,
    rouVMin :: !Double,
    rouVMax :: !Double
  }

sampleRatioOfUniforms :: Monad m => RatioOfUniforms1D -> SampleT m Double
sampleRatioOfUniforms model
  | not (rouUMax model > 0.0) || not (isFinite (rouUMax model)) =
      error "sampleRatioOfUniforms: U bound must be finite and strictly positive"
  | not (rouVMin model < rouVMax model) =
      error "sampleRatioOfUniforms: V bounds must satisfy vMin < vMax"
  | otherwise =
      go
  where
    !(supportLeft, supportRight) = rouSupport model
    !uMax = rouUMax model
    !vMin = rouVMin model
    !vSpan = rouVMax model - rouVMin model

    go = do
      uUnit <- nextOpenDouble
      vUnit <- nextDouble
      let !u = uUnit * uMax
          !v = vMin + vSpan * vUnit
          !x = v / u
      if isNaN x || isInfinite x || x <= supportLeft || x >= supportRight
        then go
        else do
          let !logDensityValue = rouLogDensity model x
          if isNaN logDensityValue || isInfinite logDensityValue
            then go
            else
              if 2.0 * log u <= logDensityValue
                then pure x
                else go

samplePoisson :: Monad m => Double -> SampleT m Int
samplePoisson lambda
  | not (isNonNegativeFinite lambda) =
      error "samplePoisson: rate must be finite and non-negative"
  | lambda == 0.0 = pure 0
  | lambda < 30.0 = smallPoisson lambda
  | otherwise = poissonPTRS lambda

sampleBinomial :: Monad m => Int -> Double -> SampleT m Int
sampleBinomial n p
  | n < 0 =
      error "sampleBinomial: trial count must be non-negative"
  | isNaN p || isInfinite p || p < 0.0 || p > 1.0 =
      error "sampleBinomial: probability must lie in [0, 1]"
  | n == 0 || p == 0.0 = pure 0
  | p == 1.0 = pure n
  | p > 0.5 = do
      failures <- sampleBinomial n (1.0 - p)
      pure (n - failures)
  | fromIntegral n * p < 32.0 = smallBinomialInversion n p
  | otherwise = recursiveBinomialBetaSplit n p

nextGaussian :: Monad m => SampleT m Double
nextGaussian = drawNormal
  where
    drawNormal = do
      u <- nextSignedOpenDouble
      boxWord <- nextWord64
      let !boxIndex = fromIntegral (boxWord .&. 0x7F)
          !ratio = VU.unsafeIndex zigNorRatio boxIndex
      if abs u < ratio
        then pure (u * VU.unsafeIndex zigNorX boxIndex)
        else
          if boxIndex == 0
            then drawNormalTail (u < 0.0)
            else do
              let !x = u * VU.unsafeIndex zigNorX boxIndex
                  !upperX = VU.unsafeIndex zigNorX boxIndex
                  !lowerX = VU.unsafeIndex zigNorX (boxIndex + 1)
                  !f0 = exp (-0.5 * (upperX * upperX - x * x))
                  !f1 = exp (-0.5 * (lowerX * lowerX - x * x))
              v <- nextOpenDouble
              if f1 + v * (f0 - f1) < 1.0
                then pure x
                else drawNormal

nextExponential :: Monad m => SampleT m Double
nextExponential = drawExponential
  where
    drawExponential = do
      u <- nextOpenDouble
      boxWord <- nextWord64
      let !boxIndex = fromIntegral (boxWord .&. 0xFF)
          !ratio = VU.unsafeIndex zigExpRatio boxIndex
      if u < ratio
        then pure (u * VU.unsafeIndex zigExpX boxIndex)
        else
          if boxIndex == 0
            then do
              tailUniform <- nextOpenDouble
              pure (zigExpR - log tailUniform)
            else do
              let !x = u * VU.unsafeIndex zigExpX boxIndex
                  !upperX = VU.unsafeIndex zigExpX boxIndex
                  !lowerX = VU.unsafeIndex zigExpX (boxIndex + 1)
                  !f0 = exp (-(upperX - x))
                  !f1 = exp (-(lowerX - x))
              v <- nextOpenDouble
              if f1 + v * (f0 - f1) < 1.0
                then pure x
                else drawExponential

nextGumbel :: Monad m => SampleT m Double
nextGumbel = do
  u <- nextOpenDouble
  pure (negate (log (negate (log u))))

sampleGumbelMaxIndex :: Monad m => VU.Vector Double -> SampleT m Int
sampleGumbelMaxIndex weights =
  sampleGumbelMaxIndexFromLogWeights (weightsToLogWeights "sampleGumbelMaxIndex" weights)

sampleGumbelMaxIndexFromLogWeights :: Monad m => VU.Vector Double -> SampleT m Int
sampleGumbelMaxIndexFromLogWeights logWeights =
  let !validLogWeights =
        SampleInput.expectSampleInput $
          SampleInput.validatePossibleLogWeights "sampleGumbelMaxIndexFromLogWeights" logWeights
   in do
      perturbed <-
        sampleDoubleVectorByIndex
          (VU.length validLogWeights)
          (\i -> do g <- nextGumbel; pure (VU.unsafeIndex validLogWeights i + g))
      pure (argmaxIndex perturbed)

sampleGumbelTopKIndicesFromLogWeights :: Monad m => Int -> VU.Vector Double -> SampleT m (VU.Vector Int)
sampleGumbelTopKIndicesFromLogWeights k logWeights
  | k == 0 = pure VU.empty
  | otherwise =
      let !validLogWeights =
            SampleInput.expectSampleInput $
              SampleInput.validateTopKLogWeights "sampleGumbelTopKIndicesFromLogWeights" k logWeights
       in do
          perturbedPairs <-
            fmap
              (sortBy (\(_, leftValue) (_, rightValue) -> compare rightValue leftValue))
              (sequence
                 [ do
                     g <- nextGumbel
                     pure (index, VU.unsafeIndex validLogWeights index + g)
                 | index <- [0 .. VU.length validLogWeights - 1],
                   let weight = VU.unsafeIndex validLogWeights index,
                   isFinite weight || (isInfinite weight && weight > 0.0)
                 ])
          pure (VU.fromListN k (fmap fst (take k perturbedPairs)))

sampleConcrete :: Monad m => Double -> VU.Vector Double -> SampleT m (VU.Vector Double)
sampleConcrete temperature weights =
  sampleConcreteFromLogWeights temperature (weightsToLogWeights "sampleConcrete" weights)

sampleConcreteFromLogWeights :: Monad m => Double -> VU.Vector Double -> SampleT m (VU.Vector Double)
sampleConcreteFromLogWeights temperature logWeights =
  let !validLogWeights =
        SampleInput.expectSampleInput $
          SampleInput.validatePositiveTemperature "sampleConcreteFromLogWeights" temperature
            *> SampleInput.validatePossibleLogWeights "sampleConcreteFromLogWeights" logWeights
   in do
      noisyLogits <-
        sampleDoubleVectorByIndex
          (VU.length validLogWeights)
          (\i -> do
             g <- nextGumbel
             pure ((VU.unsafeIndex validLogWeights i + g) / temperature))
      pure (normalizeLogWeights noisyLogits)

ellipticalSlice ::
  Monad m =>
  GaussianMeasure ->
  (VU.Vector Double -> Double) ->
  VU.Vector Double ->
  SampleT m (VU.Vector Double)
ellipticalSlice prior logLikelihood currentState
  | VU.length currentState /= gaussianDimension prior =
      error "ellipticalSlice: state dimension does not match the prior"
  | otherwise = do
      innovation <- sampleGaussianMeasure centeredPrior
      thresholdUniform <- nextOpenDouble
      let !currentLogLikelihood = safeLogDensity (logLikelihood currentState)
          !threshold = currentLogLikelihood + log thresholdUniform
          !centeredState = subtractVectors currentState (gaussianMean prior)
      angle0 <- fmap (* (2.0 * pi)) nextDouble
      let !thetaMin0 = angle0 - 2.0 * pi
          !thetaMax0 = angle0
      go centeredState innovation angle0 thetaMin0 thetaMax0 threshold
  where
    centeredPrior =
      GaussianMeasure
        { gaussianDimension = gaussianDimension prior,
          gaussianMean = VU.replicate (gaussianDimension prior) 0.0,
          gaussianCholesky = gaussianCholesky prior
        }

    go !centeredState !innovation !theta !thetaMin !thetaMax !threshold = do
      let !proposalCentered =
            addVectors
              (scaleVector (cos theta) centeredState)
              (scaleVector (sin theta) innovation)
          !proposal = addVectors proposalCentered (gaussianMean prior)
          !proposalLogLikelihood = safeLogDensity (logLikelihood proposal)
      if proposalLogLikelihood >= threshold
        then pure proposal
        else do
          let (!thetaMin', !thetaMax') =
                if theta < 0.0
                  then (theta, thetaMax)
                  else (thetaMin, theta)
          thetaUnit <- nextDouble
          let !theta' = thetaMin' + thetaUnit * (thetaMax' - thetaMin')
          go centeredState innovation theta' thetaMin' thetaMax' threshold

type DifferentiableTarget :: Type
data DifferentiableTarget = DifferentiableTarget
  { targetLogDensity :: !(VU.Vector Double -> Double),
    targetGradient :: !(VU.Vector Double -> VU.Vector Double)
  }

type NUTSConfig :: Type
data NUTSConfig = NUTSConfig
  { nutsStepSize :: !Double,
    nutsMaxTreeDepth :: !Int,
    nutsDivergenceThreshold :: !Double
  }

type NUTSStats :: Type
data NUTSStats = NUTSStats
  { nutsTreeDepthUsed :: !Int,
    nutsLeapfrogSteps :: !Int,
    nutsMeanAcceptanceProbability :: !Double
  }
  deriving stock (Eq, Show)

defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig =
  NUTSConfig
    { nutsStepSize = 0.25,
      nutsMaxTreeDepth = 10,
      nutsDivergenceThreshold = 1000.0
    }

findReasonableNUTSStepSize ::
  Monad m =>
  DifferentiableTarget ->
  VU.Vector Double ->
  SampleT m Double
findReasonableNUTSStepSize target initialPosition
  | VU.null initialPosition = pure 1.0
  | otherwise = do
      initialMomentum <- sampleStandardNormalVector (VU.length initialPosition)
      let !initialPhase = evaluatePhasePoint target initialPosition initialMomentum
          !joint0 = phaseJointDensity initialPhase
      phase1 <- pure (leapfrog target 1.0 initialPhase)
      let !acceptProb0 = boundedAcceptProbability (phaseJointDensity phase1 - joint0)
          !grow = acceptProb0 > 0.5
      go 1.0 acceptProb0 grow initialPhase joint0 0
  where
    go :: Monad n => Double -> Double -> Bool -> PhasePoint -> Double -> Int -> SampleT n Double
    go !epsilon !acceptProb !grow !initialPhase !joint0 !iterations
      | iterations >= 60 = pure epsilon
      | grow && acceptProb > 0.5 = do
          let !epsilon' = epsilon * 2.0
              !phase' = leapfrog target epsilon' initialPhase
              !acceptProb' = boundedAcceptProbability (phaseJointDensity phase' - joint0)
          go epsilon' acceptProb' grow initialPhase joint0 (iterations + 1)
      | (not grow) && acceptProb < 0.5 = do
          let !epsilon' = epsilon * 0.5
              !phase' = leapfrog target epsilon' initialPhase
              !acceptProb' = boundedAcceptProbability (phaseJointDensity phase' - joint0)
          go epsilon' acceptProb' grow initialPhase joint0 (iterations + 1)
      | otherwise = pure epsilon

nutsTransition ::
  Monad m =>
  DifferentiableTarget ->
  NUTSConfig ->
  VU.Vector Double ->
  SampleT m (VU.Vector Double, NUTSStats)
nutsTransition target config initialPosition
  | VU.null initialPosition =
      pure
        ( initialPosition,
          NUTSStats
            { nutsTreeDepthUsed = 0,
              nutsLeapfrogSteps = 0,
              nutsMeanAcceptanceProbability = 1.0
            }
        )
  | VU.any (not . isFinite) initialPosition =
      error "nutsTransition: initial position must be finite"
  | not (nutsStepSize config > 0.0) || not (isFinite (nutsStepSize config)) =
      error "nutsTransition: step size must be finite and strictly positive"
  | nutsMaxTreeDepth config < 0 =
      error "nutsTransition: max tree depth must be non-negative"
  | otherwise = do
      initialMomentum <- sampleStandardNormalVector (VU.length initialPosition)
      let !initialPhase = evaluatePhasePoint target initialPosition initialMomentum
          !joint0 = phaseJointDensity initialPhase
      thresholdUniform <- nextOpenDouble
      let !logSlice = log thresholdUniform + joint0
      result <-
        nutsGo
          target
          config
          logSlice
          joint0
          0
          initialPhase
          initialPhase
          initialPhase
          1
          True
          0.0
          0
          0
      let !meanAcceptance =
            if resultAlphaCount result == 0
              then 1.0
              else resultAlphaSum result / fromIntegral (resultAlphaCount result)
      pure
        ( phasePosition (resultProposal result),
          NUTSStats
            { nutsTreeDepthUsed = resultDepth result,
              nutsLeapfrogSteps = resultSteps result,
              nutsMeanAcceptanceProbability = meanAcceptance
            }
        )

type CounterGen :: Type
data CounterGen = CounterGen
  { counterSeed :: !Word64
  }

counterGenFromSeed :: Word64 -> CounterGen
counterGenFromSeed seed = CounterGen (mix64 seed)

splitCounterGen :: Word64 -> CounterGen -> CounterGen
splitCounterGen salt generator =
  CounterGen (mix64 (counterSeed generator + mix64 salt + 0x9e3779b97f4a7c15))

counterWord64At :: CounterGen -> Word64 -> Word64
counterWord64At generator counter =
  mix64 (counterSeed generator + counter * 0x9e3779b97f4a7c15)

counterDoubleAt :: CounterGen -> Word64 -> Double
counterDoubleAt generator counter =
  wordToUnit (counterWord64At generator counter)

type RandomizedSobol1D :: Type
newtype RandomizedSobol1D = RandomizedSobol1D
  { sobolDigitalShift :: Word64
  }

sampleRandomizedSobol1D :: Applicative m => SampleT m RandomizedSobol1D
sampleRandomizedSobol1D = RandomizedSobol1D <$> nextWord64

sobol1DAt :: Word64 -> Double
sobol1DAt index =
  wordToUnit (sobolWord64 index)

randomizedSobol1DAt :: RandomizedSobol1D -> Word64 -> Double
randomizedSobol1DAt randomized index =
  wordToOpenUnit (sobolWord64 index `xor` sobolDigitalShift randomized)

qmcContinuous1D :: HasContDistr d => d -> RandomizedSobol1D -> Word64 -> Double
qmcContinuous1D distribution randomized index =
  distributionQuantile distribution (randomizedSobol1DAt randomized index)

type PhasePoint :: Type
data PhasePoint = PhasePoint
  { phasePosition :: !(VU.Vector Double),
    phaseMomentum :: !(VU.Vector Double),
    phaseGradient :: !(VU.Vector Double),
    phaseLogDensityValue :: !Double
  }

type TreeState :: Type
data TreeState = TreeState
  { treeMinus :: !PhasePoint,
    treePlus :: !PhasePoint,
    treeProposal :: !PhasePoint,
    treeWeight :: !Int,
    treeContinue :: !Bool,
    treeAlphaSum :: !Double,
    treeAlphaCount :: !Int,
    treeSteps :: !Int
  }

type NutsLoopState :: Type
data NutsLoopState = NutsLoopState
  { resultDepth :: !Int,
    resultMinus :: !PhasePoint,
    resultPlus :: !PhasePoint,
    resultProposal :: !PhasePoint,
    resultWeight :: !Int,
    resultContinue :: !Bool,
    resultAlphaSum :: !Double,
    resultAlphaCount :: !Int,
    resultSteps :: !Int
  }

evaluatePhasePoint :: DifferentiableTarget -> VU.Vector Double -> VU.Vector Double -> PhasePoint
evaluatePhasePoint target position momentum =
  let !logDensityValue = safeLogDensity (targetLogDensity target position)
      !gradientValue = sanitizeGradient (VU.length position) (targetGradient target position)
   in PhasePoint
        { phasePosition = position,
          phaseMomentum = momentum,
          phaseGradient = gradientValue,
          phaseLogDensityValue = logDensityValue
        }

phaseJointDensity :: PhasePoint -> Double
phaseJointDensity phase =
  phaseLogDensityValue phase - 0.5 * dot (phaseMomentum phase) (phaseMomentum phase)

leapfrog :: DifferentiableTarget -> Double -> PhasePoint -> PhasePoint
leapfrog target stepSize phase =
  let !momentumHalf =
        addVectors
          (phaseMomentum phase)
          (scaleVector (0.5 * stepSize) (phaseGradient phase))
      !position' =
        addVectors (phasePosition phase) (scaleVector stepSize momentumHalf)
      !logDensity' = safeLogDensity (targetLogDensity target position')
      !gradient' = sanitizeGradient (VU.length position') (targetGradient target position')
      !momentum' =
        addVectors momentumHalf (scaleVector (0.5 * stepSize) gradient')
   in PhasePoint
        { phasePosition = position',
          phaseMomentum = momentum',
          phaseGradient = gradient',
          phaseLogDensityValue = logDensity'
        }

nutsGo ::
  Monad m =>
  DifferentiableTarget ->
  NUTSConfig ->
  Double ->
  Double ->
  Int ->
  PhasePoint ->
  PhasePoint ->
  PhasePoint ->
  Int ->
  Bool ->
  Double ->
  Int ->
  Int ->
  SampleT m NutsLoopState
nutsGo target config logSlice joint0 !depth !phaseMinus !phasePlus !proposal !weight !continue !alphaSum !alphaCount !steps
  | not continue || depth >= nutsMaxTreeDepth config =
      pure
        NutsLoopState
          { resultDepth = depth,
            resultMinus = phaseMinus,
            resultPlus = phasePlus,
            resultProposal = proposal,
            resultWeight = weight,
            resultContinue = continue,
            resultAlphaSum = alphaSum,
            resultAlphaCount = alphaCount,
            resultSteps = steps
          }
  | otherwise = do
      directionWord <- nextWord64
      let !direction = if directionWord .&. 1 == 0 then -1 else 1
          !epsilon = nutsStepSize config
          !deltaMax = nutsDivergenceThreshold config
      subtree <-
        if direction < 0
          then buildTree target epsilon deltaMax logSlice joint0 phaseMinus direction depth
          else buildTree target epsilon deltaMax logSlice joint0 phasePlus direction depth
      let !phaseMinus' =
            if direction < 0
              then treeMinus subtree
              else phaseMinus
          !phasePlus' =
            if direction < 0
              then phasePlus
              else treePlus subtree
          !totalWeight = weight + treeWeight subtree
          !continue' =
            continue
              && treeContinue subtree
              && noUTurn phaseMinus' phasePlus'
      proposal' <-
        if treeWeight subtree <= 0
          then pure proposal
          else do
            chooseUniform <- nextDouble
            pure
              ( if chooseUniform * fromIntegral totalWeight < fromIntegral (treeWeight subtree)
                  then treeProposal subtree
                  else proposal
              )
      nutsGo
        target
        config
        logSlice
        joint0
        (depth + 1)
        phaseMinus'
        phasePlus'
        proposal'
        totalWeight
        continue'
        (alphaSum + treeAlphaSum subtree)
        (alphaCount + treeAlphaCount subtree)
        (steps + treeSteps subtree)

buildTree ::
  Monad m =>
  DifferentiableTarget ->
  Double ->
  Double ->
  Double ->
  Double ->
  PhasePoint ->
  Int ->
  Int ->
  SampleT m TreeState
buildTree target epsilon deltaMax logSlice joint0 phase direction depth
  | depth == 0 = do
      let !phase' = leapfrog target (fromIntegral direction * epsilon) phase
          !joint' = phaseJointDensity phase'
          !isValid = logSlice < joint'
          !continue' = logSlice - deltaMax < joint'
          !weight' = if isValid then 1 else 0
          !alpha' = boundedAcceptProbability (joint' - joint0)
      pure
        TreeState
          { treeMinus = phase',
            treePlus = phase',
            treeProposal = phase',
            treeWeight = weight',
            treeContinue = continue',
            treeAlphaSum = alpha',
            treeAlphaCount = 1,
            treeSteps = 1
          }
  | otherwise = do
      leftTree <- buildTree target epsilon deltaMax logSlice joint0 phase direction (depth - 1)
      if not (treeContinue leftTree)
        then pure leftTree
        else do
          rightTree <-
            if direction < 0
              then buildTree target epsilon deltaMax logSlice joint0 (treeMinus leftTree) direction (depth - 1)
              else buildTree target epsilon deltaMax logSlice joint0 (treePlus leftTree) direction (depth - 1)
          proposal' <-
            chooseWeightedProposal
              (treeProposal leftTree, treeWeight leftTree)
              (treeProposal rightTree, treeWeight rightTree)
          let !minus' =
                if direction < 0
                  then treeMinus rightTree
                  else treeMinus leftTree
              !plus' =
                if direction < 0
                  then treePlus leftTree
                  else treePlus rightTree
              !continue' =
                treeContinue leftTree
                  && treeContinue rightTree
                  && noUTurn minus' plus'
          pure
            TreeState
              { treeMinus = minus',
                treePlus = plus',
                treeProposal = proposal',
                treeWeight = treeWeight leftTree + treeWeight rightTree,
                treeContinue = continue',
                treeAlphaSum = treeAlphaSum leftTree + treeAlphaSum rightTree,
                treeAlphaCount = treeAlphaCount leftTree + treeAlphaCount rightTree,
                treeSteps = treeSteps leftTree + treeSteps rightTree
              }

chooseWeightedProposal ::
  Monad m =>
  (PhasePoint, Int) ->
  (PhasePoint, Int) ->
  SampleT m PhasePoint
chooseWeightedProposal (leftProposal, leftWeight) (rightProposal, rightWeight)
  | leftWeight <= 0 && rightWeight <= 0 = pure leftProposal
  | rightWeight <= 0 = pure leftProposal
  | leftWeight <= 0 = pure rightProposal
  | otherwise = do
      u <- nextDouble
      pure
        ( if u * fromIntegral (leftWeight + rightWeight) < fromIntegral rightWeight
            then rightProposal
            else leftProposal
        )

noUTurn :: PhasePoint -> PhasePoint -> Bool
noUTurn phaseMinus phasePlus =
  let !delta = subtractVectors (phasePosition phasePlus) (phasePosition phaseMinus)
   in dot delta (phaseMomentum phaseMinus) >= 0.0
        && dot delta (phaseMomentum phasePlus) >= 0.0

type AdaptiveHull :: Type
data AdaptiveHull = AdaptiveHull
  { hullXs :: !(VU.Vector Double),
    hullHs :: !(VU.Vector Double),
    hullDs :: !(VU.Vector Double),
    hullZs :: !(VU.Vector Double),
    hullScaledMasses :: !(VU.Vector Double),
    hullScaledTotalMass :: !Double
  }

buildAdaptiveHull :: AdaptiveRejection1D -> VU.Vector Double -> Either String AdaptiveHull
buildAdaptiveHull model knots = do
  let !n = VU.length knots
      !supportLeft = fst (arsSupport model)
      !supportRight = snd (arsSupport model)
      !hs = VU.map (arsLogDensity model) knots
      !ds = VU.map (arsDerivative model) knots
  if not (strictlyIncreasing knots)
    then Left "sampleAdaptiveRejection: knot set is not strictly increasing"
    else
      if VU.any isNaN hs || VU.any isInfinite hs
        then Left "sampleAdaptiveRejection: log-density became non-finite on the knot set"
        else
          if VU.any isNaN ds || VU.any isInfinite ds
            then Left "sampleAdaptiveRejection: derivative became non-finite on the knot set"
            else do
              let !zs =
                    VU.generate
                      (n + 1)
                      (\index ->
                         if index == 0
                           then supportLeft
                           else
                             if index == n
                               then supportRight
                               else
                                 let !xLeft = VU.unsafeIndex knots (index - 1)
                                     !xRight = VU.unsafeIndex knots index
                                     !hLeft = VU.unsafeIndex hs (index - 1)
                                     !hRight = VU.unsafeIndex hs index
                                     !dLeft = VU.unsafeIndex ds (index - 1)
                                     !dRight = VU.unsafeIndex ds index
                                     !rawIntersection =
                                       if abs (dLeft - dRight) < 1.0e-15
                                         then 0.5 * (xLeft + xRight)
                                         else
                                           (hRight - hLeft - xRight * dRight + xLeft * dLeft)
                                             / (dLeft - dRight)
                                  in clamp rawIntersection xLeft xRight
                      )
              if not (nonDecreasing zs)
                then Left "sampleAdaptiveRejection: hull intersections lost monotonicity"
                else do
                  let !logMasses =
                        VU.generate
                          n
                          (\index ->
                             let !x = VU.unsafeIndex knots index
                                 !h = VU.unsafeIndex hs index
                                 !d = VU.unsafeIndex ds index
                                 !leftBoundary = VU.unsafeIndex zs index
                                 !rightBoundary = VU.unsafeIndex zs (index + 1)
                              in logIntegralExpAffine (h - d * x) d leftBoundary rightBoundary)
                      !maxLogMass = VU.maximum logMasses
                      !scaledMasses = VU.map (\logMass -> exp (logMass - maxLogMass)) logMasses
                      !scaledTotalMass = VU.sum scaledMasses
                  if not (scaledTotalMass > 0.0) || isNaN scaledTotalMass || isInfinite scaledTotalMass
                    then Left "sampleAdaptiveRejection: upper hull mass is non-finite"
                    else
                      pure
                        ( AdaptiveHull
                            { hullXs = knots,
                              hullHs = hs,
                              hullDs = ds,
                              hullZs = zs,
                              hullScaledMasses = scaledMasses,
                              hullScaledTotalMass = scaledTotalMass
                            }
                        )

adaptiveUpperHullValue :: AdaptiveHull -> Double -> Double
adaptiveUpperHullValue hull x =
  let !segment = findHullSegment (hullZs hull) x
      !knot = VU.unsafeIndex (hullXs hull) segment
      !h = VU.unsafeIndex (hullHs hull) segment
      !d = VU.unsafeIndex (hullDs hull) segment
   in h + d * (x - knot)

sampleAdaptiveHull :: Monad m => AdaptiveHull -> SampleT m Double
sampleAdaptiveHull hull = do
  massUniform <- nextDouble
  let !targetMass = massUniform * hullScaledTotalMass hull
      !segment = findMassSegment (hullScaledMasses hull) targetMass
      !leftBoundary = VU.unsafeIndex (hullZs hull) segment
      !rightBoundary = VU.unsafeIndex (hullZs hull) (segment + 1)
      !slope = VU.unsafeIndex (hullDs hull) segment
  sampleTruncatedExponential slope leftBoundary rightBoundary

smallPoisson :: Monad m => Double -> SampleT m Int
smallPoisson lambda =
  let !limit = exp (negate lambda)
      go !k !productAcc = do
        u <- nextOpenDouble
        let !productAcc' = productAcc * u
        if productAcc' <= limit
          then pure k
          else go (k + 1) productAcc'
   in go 0 1.0

poissonPTRS :: Monad m => Double -> SampleT m Int
poissonPTRS lambda =
  let !sqrtLambda = sqrt lambda
      !b = 0.931 + 2.53 * sqrtLambda
      !a = -0.059 + 0.02483 * b
      !inverseAlpha = 1.1239 + 1.1328 / (b - 3.4)
      !vR = 0.9277 - 3.6224 / (b - 2.0)
      go = do
        u0 <- nextOpenDouble
        v <- nextOpenDouble
        let !u = u0 - 0.5
            !uSym = 0.5 - abs u
        let !candidate = floor (((2.0 * a / uSym) + b) * u + lambda + 0.43)
            !k = fromIntegral candidate :: Double
            !lhs = log (v * inverseAlpha / (a / (uSym * uSym) + b))
            !rhs = negate lambda + k * log lambda - logGammaLanczos (k + 1.0)
        case () of
          _
            | uSym <= 0.0 -> go
            | candidate < 0 -> go
            | uSym >= 0.07 && v <= vR -> pure candidate
            | uSym < 0.013 && v > uSym -> go
            | lhs <= rhs -> pure candidate
            | otherwise -> go
   in go

smallBinomialInversion :: Monad m => Int -> Double -> SampleT m Int
smallBinomialInversion n p =
  let !q = 1.0 - p
      !initialMass = q ** fromIntegral n
      go !k !mass !u
        | u <= mass = pure k
        | k >= n = pure n
        | otherwise =
            let !u' = u - mass
                !k' = k + 1
                !mass' =
                  mass
                    * (fromIntegral (n - k) * p)
                    / (fromIntegral k' * q)
             in go k' mass' u'
   in do
        u <- nextOpenDouble
        go 0 initialMass u

recursiveBinomialBetaSplit :: Monad m => Int -> Double -> SampleT m Int
recursiveBinomialBetaSplit n p
  | n <= 64 = smallBinomialBernoulli n p
  | otherwise =
      let !i = max 1 (min n (floor (fromIntegral (n + 1) * p)))
          !a = fromIntegral i
          !b = fromIntegral (n + 1 - i)
       in do
            splitPoint <- sampleBeta a b
            if splitPoint >= p
              then recursiveBinomialBetaSplit (i - 1) (p / splitPoint)
              else do
                tailSuccesses <- recursiveBinomialBetaSplit (n - i) ((p - splitPoint) / (1.0 - splitPoint))
                pure (i + tailSuccesses)

smallBinomialBernoulli :: Monad m => Int -> Double -> SampleT m Int
smallBinomialBernoulli n p =
  let go !remaining !acc
        | remaining <= 0 = pure acc
        | otherwise = do
            u <- nextDouble
            go (remaining - 1) (if u < p then acc + 1 else acc)
   in go n 0

sampleLogGamma :: Monad m => Double -> SampleT m Double
sampleLogGamma alpha
  | not (isStrictlyPositiveFinite alpha) =
      error ("sampleGamma: invalid shape " ++ show alpha)
  | alpha == 1.0 = log <$> nextExponential
  | alpha < 1.0 = do
      lifted <- sampleLogGamma (alpha + 1.0)
      u <- nextOpenDouble
      pure (lifted + log u / alpha)
  | otherwise = marsagliaTsangLog alpha

marsagliaTsangLog :: Monad m => Double -> SampleT m Double
marsagliaTsangLog alpha =
  let !d = alpha - (1.0 / 3.0)
      !c = 1.0 / sqrt (9.0 * d)
      !logD = log d
      go = do
        x <- nextGaussian
        let !candidateBase = 1.0 + c * x
        if candidateBase <= 0.0
          then go
          else do
            u <- nextOpenDouble
            let !xSquared = x * x
                !xFourth = xSquared * xSquared
                !v = candidateBase * candidateBase * candidateBase
                !logV = 3.0 * log candidateBase
                !squeezeBound = 1.0 - 0.0331 * xFourth
                !acceptanceBound = 0.5 * xSquared + d * (1.0 - v + logV)
            if u < squeezeBound || log u < acceptanceBound
              then pure (logD + logV)
              else go
   in go

drawNormalTail :: Monad m => Bool -> SampleT m Double
drawNormalTail negativeTail =
  let go = do
        u1 <- nextOpenDouble
        u2 <- nextOpenDouble
        let !x = log u1 / zigNorR
            !y = log u2
        if (-2.0 * y) < x * x
          then go
          else
            pure
              ( if negativeTail
                  then x - zigNorR
                  else zigNorR - x
              )
   in go

sampleStandardNormalVector :: Monad m => Int -> SampleT m (VU.Vector Double)
sampleStandardNormalVector count =
  sampleDoubleVector count nextGaussian

sampleDoubleVector :: Monad m => Int -> SampleT m Double -> SampleT m (VU.Vector Double)
sampleDoubleVector count sampleValue =
  VU.fromListN count <$> replicateM count sampleValue

sampleDoubleVectorByIndex ::
  Monad m =>
  Int ->
  (Int -> SampleT m Double) ->
  SampleT m (VU.Vector Double)
sampleDoubleVectorByIndex count sampleValue =
  VU.fromListN count <$> traverse sampleValue [0 .. count - 1]

fenwickAccumulate ::
  PrimMonad m =>
  VUM.MVector (PrimState m) Double ->
  Int ->
  Double ->
  m ()
fenwickAccumulate tree startIndex delta =
  let !limit = VUM.length tree - 1
      go !index
        | index > limit = pure ()
        | otherwise = do
            current <- VUM.unsafeRead tree index
            VUM.unsafeWrite tree index (current + delta)
            go (index + leastSignificantBit index)
   in if delta == 0.0 then pure () else go startIndex

fenwickPrefixSum ::
  PrimMonad m =>
  VUM.MVector (PrimState m) Double ->
  Int ->
  m Double
fenwickPrefixSum tree endIndex =
  let go !acc !index
        | index <= 0 = pure acc
        | otherwise = do
            current <- VUM.unsafeRead tree index
            go (acc + current) (index - leastSignificantBit index)
   in go 0.0 endIndex

validateComposition :: String -> VU.Vector Double -> Either String ()
validateComposition context composition
  | VU.null composition =
      Left (context ++ ": composition must be non-empty")
  | VU.any (not . isStrictlyPositiveFinite) composition =
      Left (context ++ ": composition parts must be finite and strictly positive")
  | otherwise = Right ()

validateWeightTable :: String -> Int -> VU.Vector Double -> Either String Double
validateWeightTable context expectedLength weights
  | expectedLength <= 0 =
      Left (context ++ ": at least one outcome is required")
  | expectedLength /= VU.length weights =
      Left
        ( context
            ++ ": outcome count "
            ++ show expectedLength
            ++ " does not match weight count "
            ++ show (VU.length weights)
        )
  | VU.any (not . isNonNegativeFinite) weights =
      Left (context ++ ": weights must be finite and non-negative")
  | otherwise =
      let !totalWeight = VU.sum weights
       in if totalWeight <= 0.0 || isNaN totalWeight || isInfinite totalWeight
            then Left (context ++ ": total weight must be finite and strictly positive")
            else Right totalWeight

requireSameLength :: String -> VU.Vector Double -> VU.Vector Double -> Either String ()
requireSameLength context left right
  | VU.length left == VU.length right = Right ()
  | otherwise =
      Left
        ( context
            ++ ": dimension mismatch "
            ++ show (VU.length left)
            ++ " /= "
            ++ show (VU.length right)
        )

validateSymmetricPositiveSemantics :: String -> Int -> VU.Vector Double -> Either String ()
validateSymmetricPositiveSemantics context dimension matrixValues =
  let tolerance :: Double -> Double -> Double
      tolerance !a !b =
        1.0e-12 * max 1.0 (max (abs a) (abs b))
      go !row !column
        | row >= dimension = Right ()
        | column >= dimension = go (row + 1) 0
        | otherwise =
            let !leftValue = VU.unsafeIndex matrixValues (matrixIndex dimension row column)
                !rightValue = VU.unsafeIndex matrixValues (matrixIndex dimension column row)
             in if abs (leftValue - rightValue) <= tolerance leftValue rightValue
                  then go row (column + 1)
                  else
                    Left
                      ( context
                          ++ ": covariance matrix is not symmetric at ("
                          ++ show row
                          ++ ", "
                          ++ show column
                          ++ ")"
                      )
   in go 0 0

choleskyLower :: String -> Int -> VU.Vector Double -> Either String (VU.Vector Double)
choleskyLower context dimension matrixValues =
  runST $ do
    lowerFactor <- VUM.replicate (dimension * dimension) 0.0
    let buildRow !row
          | row >= dimension = Right <$> VU.unsafeFreeze lowerFactor
          | otherwise = buildColumn row 0
        buildColumn !row !column
          | column > row = buildRow (row + 1)
          | otherwise = do
              let diagonalOrOffDiagonal = do
                    let accumulate !acc !k
                          | k >= column = pure acc
                          | otherwise = do
                              leftValue <- VUM.unsafeRead lowerFactor (matrixIndex dimension row k)
                              rightValue <- VUM.unsafeRead lowerFactor (matrixIndex dimension column k)
                              accumulate (acc + leftValue * rightValue) (k + 1)
                    partialSum <- accumulate 0.0 0
                    let !matrixValue = VU.unsafeIndex matrixValues (matrixIndex dimension row column)
                        !residual = matrixValue - partialSum
                    if row == column
                      then
                        if residual <= 0.0 || isNaN residual || isInfinite residual
                          then
                            pure
                              ( Left
                                  ( context
                                      ++ ": covariance matrix is not positive definite at row "
                                      ++ show row
                                  )
                              )
                          else do
                            VUM.unsafeWrite lowerFactor (matrixIndex dimension row column) (sqrt residual)
                            buildColumn row (column + 1)
                      else do
                        diagonalValue <- VUM.unsafeRead lowerFactor (matrixIndex dimension column column)
                        let !factorValue = residual / diagonalValue
                        VUM.unsafeWrite lowerFactor (matrixIndex dimension row column) factorValue
                        buildColumn row (column + 1)
              diagonalOrOffDiagonal
    buildRow 0

normalizeLogWeights :: VU.Vector Double -> VU.Vector Double
normalizeLogWeights =
  SampleInput.expectSampleInput . SampleInput.normalizeLogWeights "normalizeLogWeights"

weightsToLogWeights :: String -> VU.Vector Double -> VU.Vector Double
weightsToLogWeights context =
  SampleInput.expectSampleInput . SampleInput.weightsToLogWeights context

dot :: VU.Vector Double -> VU.Vector Double -> Double
dot left right = VU.sum (VU.zipWith (*) left right)

addVectors :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
addVectors left right = VU.zipWith (+) left right

subtractVectors :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
subtractVectors left right = VU.zipWith (-) left right

scaleVector :: Double -> VU.Vector Double -> VU.Vector Double
scaleVector scalar = VU.map (scalar *)

argmaxIndex :: VU.Vector Double -> Int
argmaxIndex values
  | VU.null values = error "argmaxIndex: empty vector"
  | otherwise =
      snd
        ( VU.ifoldl'
            (\(!bestValue, !bestIndex) index value ->
               if value > bestValue
                 then (value, index)
                 else (bestValue, bestIndex))
            (VU.unsafeIndex values 0, 0)
            values
        )

findHullSegment :: VU.Vector Double -> Double -> Int
findHullSegment boundaries x =
  let !lastSegment = VU.length boundaries - 2
      go !index
        | index >= lastSegment = lastSegment
        | x <= VU.unsafeIndex boundaries (index + 1) = index
        | otherwise = go (index + 1)
   in go 0

findMassSegment :: VU.Vector Double -> Double -> Int
findMassSegment masses target =
  let !lastSegment = VU.length masses - 1
      go !index !acc
        | index >= lastSegment = lastSegment
        | acc + VU.unsafeIndex masses index >= target = index
        | otherwise = go (index + 1) (acc + VU.unsafeIndex masses index)
   in go 0 0.0

sampleTruncatedExponential :: Monad m => Double -> Double -> Double -> SampleT m Double
sampleTruncatedExponential slope leftBoundary rightBoundary
  | not (leftBoundary < rightBoundary) =
      error "sampleTruncatedExponential: left boundary must be smaller than right boundary"
  | abs slope < 1.0e-14 = do
      u <- nextOpenDouble
      pure (leftBoundary + u * (rightBoundary - leftBoundary))
  | isInfinite leftBoundary && isInfinite rightBoundary =
      error "sampleTruncatedExponential: both boundaries cannot be infinite"
  | isInfinite leftBoundary =
      if slope <= 0.0
        then error "sampleTruncatedExponential: non-integrable left tail"
        else do
          u <- nextOpenDouble
          pure (rightBoundary + log u / slope)
  | isInfinite rightBoundary =
      if slope >= 0.0
        then error "sampleTruncatedExponential: non-integrable right tail"
        else do
          u <- nextOpenDouble
          pure (leftBoundary + log u / slope)
  | otherwise = do
      u <- nextOpenDouble
      let !delta = slope * (rightBoundary - leftBoundary)
      pure
        ( if delta > 0.0
            then
              rightBoundary
                + log (u + (1.0 - u) * exp (negate delta))
                  / slope
            else
              leftBoundary
                + log (1.0 + u * (exp delta - 1.0))
                  / slope
        )

logIntegralExpAffine :: Double -> Double -> Double -> Double -> Double
logIntegralExpAffine intercept slope leftBoundary rightBoundary
  | not (leftBoundary < rightBoundary) =
      error "logIntegralExpAffine: left boundary must be smaller than right boundary"
  | abs slope < 1.0e-14 =
      intercept + log (rightBoundary - leftBoundary)
  | isInfinite leftBoundary && isInfinite rightBoundary =
      error "logIntegralExpAffine: both boundaries cannot be infinite"
  | isInfinite leftBoundary =
      if slope <= 0.0
        then error "logIntegralExpAffine: non-integrable left tail"
        else intercept + slope * rightBoundary - log slope
  | isInfinite rightBoundary =
      if slope >= 0.0
        then error "logIntegralExpAffine: non-integrable right tail"
        else intercept + slope * leftBoundary - log (negate slope)
  | slope > 0.0 =
      let !delta = slope * (rightBoundary - leftBoundary)
       in if delta > 50.0
            then intercept + slope * rightBoundary - log slope
            else intercept + slope * rightBoundary + log1mExpPositive delta - log slope
  | otherwise =
      let !delta = negate slope * (rightBoundary - leftBoundary)
       in if delta > 50.0
            then intercept + slope * leftBoundary - log (negate slope)
            else intercept + slope * leftBoundary + log1mExpPositive delta - log (negate slope)

log1mExpPositive :: Double -> Double
log1mExpPositive x
  | x <= 0.0 = error "log1mExpPositive: x must be strictly positive"
  | x <= 0.6931471805599453 = log (negate (exp (negate x) - 1.0))
  | otherwise = log (1.0 - exp (negate x))

strictlyIncreasing :: VU.Vector Double -> Bool
strictlyIncreasing values =
  let !n = VU.length values
      go !index
        | index >= n = True
        | VU.unsafeIndex values (index - 1) < VU.unsafeIndex values index = go (index + 1)
        | otherwise = False
   in if n <= 1 then True else go 1

nonDecreasing :: VU.Vector Double -> Bool
nonDecreasing values =
  let !n = VU.length values
      go !index
        | index >= n = True
        | VU.unsafeIndex values (index - 1) <= VU.unsafeIndex values index = go (index + 1)
        | otherwise = False
   in if n <= 1 then True else go 1

slopesStrictlyDecreasing :: (Double -> Double) -> VU.Vector Double -> Bool
slopesStrictlyDecreasing derivative knots =
  let !n = VU.length knots
      go !index !previousSlope
        | index >= n = True
        | otherwise =
            let !slope = derivative (VU.unsafeIndex knots index)
             in previousSlope > slope && go (index + 1) slope
   in if n <= 1
        then True
        else
          let !s0 = derivative (VU.unsafeIndex knots 0)
           in go 1 s0

insertSortedUnique :: Double -> VU.Vector Double -> VU.Vector Double
insertSortedUnique value values =
  let !tolerance = 1.0e-12 * max 1.0 (abs value)
      !n = VU.length values
      go !index !acc
        | index >= n = VU.fromListN (length acc + 1) (reverse (value : acc))
        | otherwise =
            let !current = VU.unsafeIndex values index
             in if abs (current - value) <= tolerance
                  then values
                  else
                    if value < current
                      then
                        VU.fromListN
                          (n + 1)
                          (reverse acc ++ value : drop index (VU.toList values))
                      else go (index + 1) (current : acc)
   in go 0 []

nextIndexBounded :: Monad m => Int -> SampleT m Int
nextIndexBounded upperExclusive
  | upperExclusive <= 0 =
      error "nextIndexBounded: upper bound must be positive"
  | otherwise =
      let !bound = fromIntegral upperExclusive :: Word64
          !threshold = negate bound `mod` bound
          go = do
            word <- nextWord64
            if word < threshold
              then go
              else pure (fromIntegral (word `mod` bound))
       in go

wordToUnit :: Word64 -> Double
wordToUnit word =
  fromIntegral (shiftR word 11) * oneOverTwoToThe53

wordToOpenUnit :: Word64 -> Double
wordToOpenUnit word =
  (fromIntegral (shiftR word 11) + 0.5) * oneOverTwoToThe53

pureGenState :: PureGen -> (Word64, Word64)
pureGenState (PureGen generator) = unseedSMGen generator

matrixIndex :: Int -> Int -> Int -> Int
matrixIndex dimension row column = row * dimension + column

leastSignificantBit :: Int -> Int
leastSignificantBit index = index .&. negate index

highestPowerOfTwoLE :: Int -> Int
highestPowerOfTwoLE value
  | value <= 0 = 0
  | otherwise =
      let !shiftAmount = finiteBitSize value - 1 - countLeadingZeros value
       in bit shiftAmount

clamp :: Double -> Double -> Double -> Double
clamp value lower upper = max lower (min upper value)

clampOpenUnit :: Double -> Double
clampOpenUnit value =
  min (1.0 - 0.5 * oneOverTwoToThe53) (max (0.5 * oneOverTwoToThe53) value)

safeLogDensity :: Double -> Double
safeLogDensity value
  | isNaN value = negativeInfinity
  | otherwise = value

sanitizeGradient :: Int -> VU.Vector Double -> VU.Vector Double
sanitizeGradient expectedLength gradientValue
  | VU.length gradientValue /= expectedLength =
      error "gradient dimension mismatch"
  | VU.any (\x -> isNaN x || isInfinite x) gradientValue =
      VU.replicate expectedLength 0.0
  | otherwise = gradientValue

boundedAcceptProbability :: Double -> Double
boundedAcceptProbability logAcceptance
  | isNaN logAcceptance = 0.0
  | logAcceptance >= 0.0 = 1.0
  | logAcceptance <= -745.0 = 0.0
  | otherwise = exp logAcceptance

logSumExp2 :: Double -> Double -> Double
logSumExp2 left right
  | left >= right = left + log (1.0 + exp (right - left))
  | otherwise = right + log (1.0 + exp (left - right))

wordToProb :: Word64 -> Prob
wordToProb word =
  Prob (fromIntegral (shiftR word 11) / 9007199254740992.0)

oneOverTwoToThe53 :: Double
oneOverTwoToThe53 = 1.1102230246251565e-16

mix64 :: Word64 -> Word64
mix64 z0 =
  let !z1 = (z0 `xor` shiftR z0 30) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xor` shiftR z1 27) * 0x94d049bb133111eb
   in z2 `xor` shiftR z2 31

sobolWord64 :: Word64 -> Word64
sobolWord64 index =
  bitReverseWord64 (index `xor` shiftR index 1)

bitReverseWord64 :: Word64 -> Word64
bitReverseWord64 word0 =
  let !word1 = ((word0 `shiftR` 1) .&. 0x5555555555555555) .|. ((word0 .&. 0x5555555555555555) `shiftL` 1)
      !word2 = ((word1 `shiftR` 2) .&. 0x3333333333333333) .|. ((word1 .&. 0x3333333333333333) `shiftL` 2)
      !word3 = ((word2 `shiftR` 4) .&. 0x0f0f0f0f0f0f0f0f) .|. ((word2 .&. 0x0f0f0f0f0f0f0f0f) `shiftL` 4)
      !word4 = ((word3 `shiftR` 8) .&. 0x00ff00ff00ff00ff) .|. ((word3 .&. 0x00ff00ff00ff00ff) `shiftL` 8)
      !word5 = ((word4 `shiftR` 16) .&. 0x0000ffff0000ffff) .|. ((word4 .&. 0x0000ffff0000ffff) `shiftL` 16)
   in (word5 `shiftR` 32) .|. (word5 `shiftL` 32)

logGammaLanczos :: Double -> Double
logGammaLanczos z
  | z < 0.5 =
      log pi - log (sin (pi * z)) - logGammaLanczos (1.0 - z)
  | otherwise =
      let !z' = z - 1.0
          !coefficients =
            [ 0.99999999999980993,
              676.5203681218851,
              -1259.1392167224028,
              771.32342877765313,
              -176.61502916214059,
              12.507343278686905,
              -0.13857109526572012,
              9.9843695780195716e-6,
              1.5056327351493116e-7
            ]
          accumulate :: Double -> Double -> [Double] -> Double
          accumulate !acc !_ [] = acc
          accumulate !acc !offset (coefficient : rest) =
            accumulate (acc + coefficient / (z' + offset)) (offset + 1.0) rest
          !x =
            case coefficients of
              [] -> error "logGammaLanczos: coefficient table empty"
              firstCoefficient : remainingCoefficients ->
                accumulate 0.0 1.0 remainingCoefficients + firstCoefficient
          !t = z' + 7.5
       in 0.9189385332046727 + (z' + 0.5) * log t - t + log x

zigNorC :: Int
zigNorC = 128

zigNorR :: Double
zigNorR = 3.442619855899

zigNorV :: Double
zigNorV = 9.91256303526217e-3

zigExpC :: Int
zigExpC = 256

zigExpR :: Double
zigExpR = 7.69711747013105

zigExpV :: Double
zigExpV = 3.94965982258156e-3

zigNorX :: VU.Vector Double
zigNorX =
  let !fR = exp (-0.5 * zigNorR * zigNorR)
      !x0 = zigNorV / fR
      build !index !previousX !previousF
        | index >= zigNorC = [0.0]
        | otherwise =
            let !currentX = sqrt (-2.0 * log (zigNorV / previousX + previousF))
                !currentF = exp (-0.5 * currentX * currentX)
             in currentX : build (index + 1) currentX currentF
   in VU.fromListN (zigNorC + 1) (x0 : zigNorR : build 2 zigNorR fR)

zigNorRatio :: VU.Vector Double
zigNorRatio =
  VU.generate
    zigNorC
    (\index -> VU.unsafeIndex zigNorX (index + 1) / VU.unsafeIndex zigNorX index)

zigExpX :: VU.Vector Double
zigExpX =
  let !x0 = zigExpR + 1.0
      build !index !previousX
        | index >= zigExpC = [0.0]
        | otherwise =
            let !currentX = negate (log (zigExpV / previousX + exp (-previousX)))
             in currentX : build (index + 1) currentX
   in VU.fromListN (zigExpC + 1) (x0 : zigExpR : build 2 zigExpR)

zigExpRatio :: VU.Vector Double
zigExpRatio =
  VU.generate
    zigExpC
    (\index -> VU.unsafeIndex zigExpX (index + 1) / VU.unsafeIndex zigExpX index)

{-# NOINLINE zigNorX #-}
{-# NOINLINE zigNorRatio #-}
{-# NOINLINE zigExpX #-}
{-# NOINLINE zigExpRatio #-}
