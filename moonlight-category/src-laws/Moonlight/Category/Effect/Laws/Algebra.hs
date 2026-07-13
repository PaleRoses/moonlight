module Moonlight.Category.Effect.Laws.Algebra
  ( lawBundles,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleOrdinalLower (..),
    SampleOrdinalUpper (..),
  )
import Moonlight.Category.Pure.Poset
  ( OrdinalLower,
    OrdinalUpper,
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)

galoisAdjointProp :: SampleOrdinalLower -> SampleOrdinalUpper -> Bool
galoisAdjointProp (SampleOrdinalLower leftValue) (SampleOrdinalUpper rightValue) =
  Harness.galoisAdjoint @OrdinalLower @OrdinalUpper leftValue rightValue

galoisDeflationProp :: SampleOrdinalUpper -> Bool
galoisDeflationProp (SampleOrdinalUpper rightValue) =
  Harness.galoisDeflation @OrdinalLower @OrdinalUpper rightValue

galoisInflationProp :: SampleOrdinalLower -> Bool
galoisInflationProp (SampleOrdinalLower leftValue) =
  Harness.galoisInflation @OrdinalLower @OrdinalUpper leftValue

galoisRetractionProp :: SampleOrdinalLower -> Bool
galoisRetractionProp (SampleOrdinalLower leftValue) =
  Harness.galoisRetraction @OrdinalLower @OrdinalUpper leftValue

ordinalMonotoneProp :: Bool
ordinalMonotoneProp = Harness.ordinalGaloisMonotone @OrdinalLower @OrdinalUpper

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "galois"
      [ quickCheckLawDefinition GaloisAdjoint galoisAdjointProp,
        quickCheckLawDefinition GaloisDeflation galoisDeflationProp,
        quickCheckLawDefinition GaloisInflation galoisInflationProp,
        quickCheckLawDefinition GaloisRetraction galoisRetractionProp,
        quickCheckLawDefinition OrdinalGaloisMonotone ordinalMonotoneProp
      ]
  ]
