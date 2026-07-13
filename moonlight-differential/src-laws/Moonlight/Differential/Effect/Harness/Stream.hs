module Moonlight.Differential.Effect.Harness.Stream
  ( integralSamplerAgreesWithGenericFold,
    productIntegralSamplerAgreesWithGenericFold,
    memoTimeIsExtensionallyIdentity,
    locallyFiniteMobiusInvertsClosedIntervals,
    naturalPrefixExecutionAgreesWithDenotation,
    naturalProductPrefixExecutionAgreesWithDenotation,
    naturalProductScansFactorAsNestedScans,
    naturalScalarLinearIncrementalizationBypassesReplay,
    productMobiusCoefficientsFactor,
    productMobiusSupportFactors,
    streamDifferentiateIntegrateInverse,
    streamMobiusInversionLawful,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Order.LocallyFinite
  ( LocallyFiniteOrder (..),
    RootedLocallyFiniteOrder (..),
    integralSamplerGeneric,
    mobius,
    mobiusProduct,
    mobiusSupport,
  )
import Moonlight.Differential.Stream
  ( ScalarLinearMap (..),
    Stream,
    delay,
    differentiate,
    differentiateNaturalPrefix,
    differentiateNaturalProductPrefix,
    differentiateNaturalProductRows,
    incrementalize,
    incrementalizeNaturalPrefix,
    incrementalizeScalarLinearNaturalPrefix,
    integrate,
    integrateNaturalPrefix,
    integrateNaturalProductPrefix,
    integrateNaturalProductRows,
    mapScalarLinearStream,
    mapStream,
    stream,
    streamAt,
    streamNaturalPrefix,
    streamNaturalProductPrefix,
  )
import Numeric.Natural
  ( Natural,
  )
import Test.Tasty.QuickCheck qualified as QC

integralSamplerAgreesWithGenericFold :: [Int] -> Int -> QC.Property
integralSamplerAgreesWithGenericFold samples prefixLimit =
  let sampleFunction =
        streamAt (streamFromSamples samples)
      memoized =
        integralSampler sampleFunction
      generic =
        integralSamplerGeneric sampleFunction
      points =
        fmap fromIntegral [0 .. prefixLimit] :: [Natural]
   in fmap memoized points QC.=== fmap generic points

productIntegralSamplerAgreesWithGenericFold :: [((Natural, Natural), Int)] -> Int -> QC.Property
productIntegralSamplerAgreesWithGenericFold samples sideLength =
  let sampleMap =
        Map.fromList samples
      sampleFunction time =
        Map.findWithDefault 0 time sampleMap
      memoized =
        integralSampler sampleFunction
      generic =
        integralSamplerGeneric sampleFunction
      points =
        productGridPoints sideLength
   in fmap memoized points QC.=== fmap generic points

memoTimeIsExtensionallyIdentity :: [Int] -> [((Natural, Natural), Int)] -> Int -> QC.Property
memoTimeIsExtensionallyIdentity samples productSamples sideLength =
  let sampleFunction =
        streamAt (streamFromSamples samples)
      naturalMemo =
        memoTime sampleFunction
      naturalPoints =
        fmap fromIntegral [0 .. sideLength] :: [Natural]
      productSampleMap =
        Map.fromList productSamples
      productSample time =
        Map.findWithDefault (0 :: Int) time productSampleMap
      productMemo =
        memoTime productSample
      gridPoints =
        productGridPoints sideLength
   in QC.conjoin
        [ QC.counterexample
            "natural memoTime agrees pointwise with its sampler"
            (fmap naturalMemo naturalPoints QC.=== fmap sampleFunction naturalPoints),
          QC.counterexample
            "product memoTime agrees pointwise with its sampler"
            (fmap productMemo gridPoints QC.=== fmap productSample gridPoints)
        ]

productGridPoints :: Int -> [(Natural, Natural)]
productGridPoints sideLength =
  [ (fromIntegral row, fromIntegral column)
  | row <- [0 .. sideLength],
    column <- [0 .. sideLength]
  ]

streamDifferentiateIntegrateInverse :: [Int] -> Int -> QC.Property
streamDifferentiateIntegrateInverse samples prefixLimit =
  let streamValue = streamFromSamples samples
   in QC.conjoin
        [ QC.counterexample
            "integrate . differentiate"
            (streamPrefix prefixLimit (integrate (differentiate streamValue)) QC.=== streamPrefix prefixLimit streamValue),
          QC.counterexample
            "differentiate . integrate"
            (streamPrefix prefixLimit (differentiate (integrate streamValue)) QC.=== streamPrefix prefixLimit streamValue),
          QC.counterexample
            "incrementalize id"
            (streamPrefix prefixLimit (incrementalize id streamValue) QC.=== streamPrefix prefixLimit streamValue),
          QC.counterexample
            "delay starts at zero"
            (streamAt (delay streamValue) 0 QC.=== (0 :: Int)),
          QC.counterexample
            "delay shifts a finite prefix by one step"
            (streamPrefix prefixLimit (delay streamValue) QC.=== delayedPrefixExpected prefixLimit streamValue)
        ]

streamFromSamples :: [Int] -> Stream Natural Int
streamFromSamples samples =
  stream
    ( \time ->
        IntMap.findWithDefault
          0
          (fromIntegral time)
          (IntMap.fromList (zip [0 ..] samples))
    )

streamPrefix :: Int -> Stream Natural Int -> [Int]
streamPrefix prefixLimit streamValue =
  fmap (streamAt streamValue . fromIntegral) [0 .. prefixLimit]

delayedPrefixExpected :: Int -> Stream Natural Int -> [Int]
delayedPrefixExpected prefixLimit streamValue =
  0 : fmap (streamAt streamValue . fromIntegral) [0 .. prefixLimit - 1]

streamMobiusInversionLawful :: [((Natural, Natural), Int)] -> (Natural, Natural) -> QC.Property
streamMobiusInversionLawful samples target =
  let sampleMap =
        Map.fromList samples
      streamValue =
        stream (\time -> Map.findWithDefault 0 time sampleMap)
      prefix =
        streamValuesOver (interval leastTime target)
   in QC.conjoin
        [ QC.counterexample
            "integrate . differentiate over N x N"
            (prefix (integrate (differentiate streamValue)) QC.=== prefix streamValue),
          QC.counterexample
            "differentiate . integrate over N x N"
            (prefix (differentiate (integrate streamValue)) QC.=== prefix streamValue),
          QC.counterexample
            "incrementalize id over N x N"
            (prefix (incrementalize id streamValue) QC.=== prefix streamValue)
        ]

streamValuesOver :: [time] -> Stream time value -> [value]
streamValuesOver times streamValue =
  fmap (streamAt streamValue) times

locallyFiniteMobiusInvertsClosedIntervals :: Int -> Int -> QC.Property
locallyFiniteMobiusInvertsClosedIntervals leftRaw rightRaw =
  let lower =
        fromIntegral (min leftRaw rightRaw) :: Natural
      upper =
        fromIntegral (max leftRaw rightRaw) :: Natural
      coefficientSum =
        sum (fmap (mobius lower) (interval lower upper))
      expected =
        if lower == upper then 1 else 0
   in coefficientSum QC.=== expected

productMobiusCoefficientsFactor :: Int -> Int -> Int -> Int -> QC.Property
productMobiusCoefficientsFactor leftStartRaw rightStartRaw leftWidthRaw rightWidthRaw =
  let lower =
        (fromIntegral (leftStartRaw `mod` 5), fromIntegral (rightStartRaw `mod` 5)) :: (Natural, Natural)
      upper =
        ( fst lower + fromIntegral (leftWidthRaw `mod` 5),
          snd lower + fromIntegral (rightWidthRaw `mod` 5)
        )
   in mobius lower upper QC.=== mobiusProduct lower upper

productMobiusSupportFactors :: Int -> Int -> Int -> Int -> QC.Property
productMobiusSupportFactors leftStartRaw rightStartRaw leftWidthRaw rightWidthRaw =
  let lower =
        (fromIntegral (leftStartRaw `mod` 5), fromIntegral (rightStartRaw `mod` 5)) :: (Natural, Natural)
      upper =
        ( fst lower + fromIntegral (leftWidthRaw `mod` 5),
          snd lower + fromIntegral (rightWidthRaw `mod` 5)
        )
      expected =
        [ ((left, right), leftCoefficient * rightCoefficient)
        | (left, leftCoefficient) <- mobiusSupport (fst lower) (fst upper),
          (right, rightCoefficient) <- mobiusSupport (snd lower) (snd upper),
          leftCoefficient * rightCoefficient /= 0
        ]
   in mobiusSupport lower upper QC.=== expected

naturalPrefixExecutionAgreesWithDenotation :: [Int] -> Int -> QC.Property
naturalPrefixExecutionAgreesWithDenotation samples prefixLength =
  let streamValue =
        streamFromSamples samples
   in QC.conjoin
        [ QC.counterexample
            "finite-prefix differentiate agrees with pointwise differentiate"
            (differentiateNaturalPrefix prefixLength streamValue QC.=== streamNaturalPrefixOf prefixLength (differentiate streamValue)),
          QC.counterexample
            "finite-prefix integrate agrees with pointwise integrate"
            (integrateNaturalPrefix prefixLength streamValue QC.=== streamNaturalPrefixOf prefixLength (integrate streamValue)),
          QC.counterexample
            "finite-prefix incrementalize id agrees with pointwise incrementalize id"
            (incrementalizeNaturalPrefix id prefixLength streamValue QC.=== streamNaturalPrefixOf prefixLength (incrementalize id streamValue)),
          QC.counterexample
            "finite-prefix incrementalize map agrees with pointwise incrementalize map"
            (incrementalizeNaturalPrefix (mapStream (* 2)) prefixLength streamValue QC.=== streamNaturalPrefixOf prefixLength (incrementalize (mapStream (* 2)) streamValue))
        ]

naturalScalarLinearIncrementalizationBypassesReplay :: [Int] -> Integer -> Int -> QC.Property
naturalScalarLinearIncrementalizationBypassesReplay samples coefficient prefixLength =
  let streamValue =
        streamFromSamples samples
      linearMap =
        ScaleByInteger coefficient
      directDelta =
        streamNaturalPrefixOf prefixLength (mapScalarLinearStream linearMap streamValue)
      genericIncremental =
        streamNaturalPrefixOf prefixLength (incrementalize (mapScalarLinearStream linearMap) streamValue)
      finiteFastPath =
        incrementalizeScalarLinearNaturalPrefix linearMap prefixLength streamValue
   in QC.conjoin
        [ QC.counterexample
            "scalar-linear fast path equals direct delta map"
            (finiteFastPath QC.=== directDelta),
          QC.counterexample
            "scalar-linear fast path equals generic incrementalization"
            (finiteFastPath QC.=== genericIncremental)
        ]

naturalProductPrefixExecutionAgreesWithDenotation :: [((Natural, Natural), Int)] -> Int -> QC.Property
naturalProductPrefixExecutionAgreesWithDenotation samples sideLength =
  let sampleMap =
        Map.fromList samples
      streamValue =
        stream (\time -> Map.findWithDefault 0 time sampleMap)
   in QC.conjoin
        [ QC.counterexample
            "finite-product differentiate agrees with pointwise differentiate"
            (differentiateNaturalProductPrefix sideLength streamValue QC.=== streamNaturalProductPrefixOf sideLength (differentiate streamValue)),
          QC.counterexample
            "finite-product integrate agrees with pointwise integrate"
            (integrateNaturalProductPrefix sideLength streamValue QC.=== streamNaturalProductPrefixOf sideLength (integrate streamValue)),
          QC.counterexample
            "finite-product integrate . differentiate agrees with the source section"
            (integrateNaturalProductRows (differentiateNaturalProductPrefix sideLength streamValue) QC.=== streamNaturalProductPrefixOf sideLength streamValue)
        ]

naturalProductScansFactorAsNestedScans :: [[Int]] -> QC.Property
naturalProductScansFactorAsNestedScans rows =
  let boundedRows =
        fmap (take 8) (take 8 rows)
   in QC.conjoin
        [ QC.counterexample
            "differentiate product rows factors as vertical then horizontal differentiation"
            (differentiateNaturalProductRows boundedRows QC.=== nestedDifferentiateProductRows boundedRows),
          QC.counterexample
            "integrate product rows factors as horizontal then vertical integration"
            (integrateNaturalProductRows boundedRows QC.=== nestedIntegrateProductRows boundedRows)
        ]

streamNaturalPrefixOf :: Int -> Stream Natural Int -> [Int]
streamNaturalPrefixOf =
  streamNaturalPrefix

streamNaturalProductPrefixOf :: Int -> Stream (Natural, Natural) Int -> [[Int]]
streamNaturalProductPrefixOf =
  streamNaturalProductPrefix

differentiateNaturalValuesSpec :: [Int] -> [Int]
differentiateNaturalValuesSpec values =
  reverse
    ( snd
        ( Foldable.foldl'
            collect
            (0, [])
            values
        )
    )
  where
    collect :: (Int, [Int]) -> Int -> (Int, [Int])
    collect (previous, differences) current =
      let difference =
            current - previous
       in difference `seq` (current, difference : differences)

integrateNaturalValuesSpec :: [Int] -> [Int]
integrateNaturalValuesSpec values =
  reverse
    ( snd
        ( Foldable.foldl'
            collect
            (0, [])
            values
        )
    )
  where
    collect :: (Int, [Int]) -> Int -> (Int, [Int])
    collect (accumulated, integrals) current =
      let integral =
            accumulated + current
       in integral `seq` (integral, integral : integrals)

nestedDifferentiateProductRows :: [[Int]] -> [[Int]]
nestedDifferentiateProductRows =
  fmap differentiateNaturalValuesSpec . differentiateRowsVerticallySpec

nestedIntegrateProductRows :: [[Int]] -> [[Int]]
nestedIntegrateProductRows =
  integrateRowsVerticallySpec . fmap integrateNaturalValuesSpec

differentiateRowsVerticallySpec :: [[Int]] -> [[Int]]
differentiateRowsVerticallySpec rows =
  reverse (snd (Foldable.foldl' collect ([], []) rows))
  where
    collect (previousRow, differences) currentRow =
      let rowDifference =
            zipWith (-) currentRow (zeroPaddedPrefixSpec (length currentRow) previousRow)
       in (currentRow, rowDifference : differences)

integrateRowsVerticallySpec :: [[Int]] -> [[Int]]
integrateRowsVerticallySpec rows =
  reverse (snd (Foldable.foldl' collect ([], []) rows))
  where
    collect (previousRow, integrals) currentRow =
      let rowIntegral =
            zipWith (+) currentRow (zeroPaddedPrefixSpec (length currentRow) previousRow)
       in (rowIntegral, rowIntegral : integrals)

zeroPaddedPrefixSpec :: Int -> [Int] -> [Int]
zeroPaddedPrefixSpec width values =
  take width (values <> repeat 0)
