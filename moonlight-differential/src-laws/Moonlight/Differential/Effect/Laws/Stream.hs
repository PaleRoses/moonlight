module Moonlight.Differential.Effect.Laws.Stream
  ( lawBundles,
  )
where

import Moonlight.Differential.Effect.Harness.Stream qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "stream"
      [ quickCheckLawDefinition StreamDifferentiateIntegrateInverse propStreamDifferentialIntegralLaws,
        quickCheckLawDefinition StreamMobiusInversionLawful propProductStreamDifferentialIntegralLaws,
        quickCheckLawDefinition LocallyFiniteMobiusInvertsClosedIntervals propMobiusIntervalInversion,
        quickCheckLawDefinition ProductMobiusCoefficientsFactor propMobiusProductFactorization,
        quickCheckLawDefinition ProductMobiusSupportFactors propMobiusProductSupportFactorization,
        quickCheckLawDefinition NaturalPrefixExecutionAgreesWithDenotation propNaturalPrefixExecutionMatchesPointwise,
        quickCheckLawDefinition NaturalScalarLinearIncrementalizationBypassesReplay propNaturalScalarLinearIncrementalization,
        quickCheckLawDefinition NaturalProductPrefixExecutionAgreesWithDenotation propNaturalProductPrefixExecutionMatchesPointwise,
        quickCheckLawDefinition NaturalProductScansFactorAsNestedScans propNaturalProductRowsFactorAsNestedScans,
        quickCheckLawDefinition IntegralSamplerAgreesWithGenericFold propIntegralSamplerMatchesGenericFold,
        quickCheckLawDefinition ProductIntegralSamplerAgreesWithGenericFold propProductIntegralSamplerMatchesGenericFold,
        quickCheckLawDefinition MemoTimeIsExtensionallyIdentity propMemoTimeExtensionallyIdentity
      ]
  ]

propIntegralSamplerMatchesGenericFold :: [Int] -> QC.Property
propIntegralSamplerMatchesGenericFold samples =
  QC.forAll (QC.chooseInt (0, 24)) $ \prefixLimit ->
    Harness.integralSamplerAgreesWithGenericFold samples prefixLimit

propProductIntegralSamplerMatchesGenericFold :: [((Natural, Natural), Int)] -> QC.Property
propProductIntegralSamplerMatchesGenericFold samples =
  QC.forAll (QC.chooseInt (0, 6)) $ \sideLength ->
    Harness.productIntegralSamplerAgreesWithGenericFold samples sideLength

propMemoTimeExtensionallyIdentity :: [Int] -> [((Natural, Natural), Int)] -> QC.Property
propMemoTimeExtensionallyIdentity samples productSamples =
  QC.forAll (QC.chooseInt (0, 6)) $ \sideLength ->
    Harness.memoTimeIsExtensionallyIdentity samples productSamples sideLength

propStreamDifferentialIntegralLaws :: [Int] -> QC.Property
propStreamDifferentialIntegralLaws samples =
  QC.forAll (QC.chooseInt (0, 24)) $ \prefixLimit ->
    Harness.streamDifferentiateIntegrateInverse samples prefixLimit

propProductStreamDifferentialIntegralLaws :: [((Natural, Natural), Int)] -> QC.Property
propProductStreamDifferentialIntegralLaws samples =
  QC.forAll productStreamTargetGen $ \target ->
    Harness.streamMobiusInversionLawful samples target

productStreamTargetGen :: QC.Gen (Natural, Natural)
productStreamTargetGen =
  (,)
    <$> (fromIntegral <$> QC.chooseInt (0, 5))
    <*> (fromIntegral <$> QC.chooseInt (0, 5))

propMobiusIntervalInversion :: QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
propMobiusIntervalInversion (QC.NonNegative leftRaw) (QC.NonNegative rightRaw) =
  Harness.locallyFiniteMobiusInvertsClosedIntervals leftRaw rightRaw

propMobiusProductFactorization :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
propMobiusProductFactorization (QC.NonNegative leftStartRaw) (QC.NonNegative rightStartRaw) (QC.NonNegative leftWidthRaw) (QC.NonNegative rightWidthRaw) =
  Harness.productMobiusCoefficientsFactor leftStartRaw rightStartRaw leftWidthRaw rightWidthRaw

propMobiusProductSupportFactorization :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
propMobiusProductSupportFactorization (QC.NonNegative leftStartRaw) (QC.NonNegative rightStartRaw) (QC.NonNegative leftWidthRaw) (QC.NonNegative rightWidthRaw) =
  Harness.productMobiusSupportFactors leftStartRaw rightStartRaw leftWidthRaw rightWidthRaw

propNaturalPrefixExecutionMatchesPointwise :: [Int] -> QC.Property
propNaturalPrefixExecutionMatchesPointwise samples =
  QC.forAll (QC.chooseInt (0, 24)) $ \prefixLength ->
    Harness.naturalPrefixExecutionAgreesWithDenotation samples prefixLength

propNaturalScalarLinearIncrementalization :: [Int] -> QC.Property
propNaturalScalarLinearIncrementalization samples =
  QC.forAll (QC.chooseInteger (-4, 4)) $ \coefficient ->
    QC.forAll (QC.chooseInt (0, 24)) $ \prefixLength ->
      Harness.naturalScalarLinearIncrementalizationBypassesReplay samples coefficient prefixLength

propNaturalProductPrefixExecutionMatchesPointwise :: [((Natural, Natural), Int)] -> QC.Property
propNaturalProductPrefixExecutionMatchesPointwise samples =
  QC.forAll (QC.chooseInt (0, 6)) $ \sideLength ->
    Harness.naturalProductPrefixExecutionAgreesWithDenotation samples sideLength

propNaturalProductRowsFactorAsNestedScans :: [[Int]] -> QC.Property
propNaturalProductRowsFactorAsNestedScans rows =
  Harness.naturalProductScansFactorAsNestedScans rows
