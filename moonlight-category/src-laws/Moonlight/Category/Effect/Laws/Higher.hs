module Moonlight.Category.Effect.Laws.Higher
  ( lawBundles,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleUnitTwoMorphism (..),
  )
import Moonlight.Category.Pure.Unit
  ( UnitCat (..),
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)

higherHorizontalProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherHorizontalProp (SampleUnitTwoMorphism leftValue) (SampleUnitTwoMorphism rightValue) =
  Harness.horizontalBoundary @UnitCat UnitCat leftValue rightValue

higherVerticalProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherVerticalProp (SampleUnitTwoMorphism leftValue) (SampleUnitTwoMorphism rightValue) =
  Harness.verticalBoundary @UnitCat UnitCat leftValue rightValue

higherInterchangeProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherInterchangeProp
  (SampleUnitTwoMorphism upperLeftValue)
  (SampleUnitTwoMorphism upperRightValue)
  (SampleUnitTwoMorphism lowerLeftValue)
  (SampleUnitTwoMorphism lowerRightValue) =
    Harness.interchange @UnitCat UnitCat upperLeftValue upperRightValue lowerLeftValue lowerRightValue

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "higher"
      [ quickCheckLawDefinition HigherHorizontalBoundary higherHorizontalProp,
        quickCheckLawDefinition HigherVerticalBoundary higherVerticalProp,
        quickCheckLawDefinition HigherInterchange higherInterchangeProp
      ]
  ]
