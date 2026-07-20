module Moonlight.Category.Effect.Laws.Category
  ( lawBundles,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleComposableFinTriple (..),
    SampleFinMorphism (..),
  )
import Moonlight.Category.Pure.FinCat (FinCat, sampleFinCat)
import Moonlight.Pale.Test.LawSuite
  ( LawBundle,
    lawBundleQuickCheck,
    quickCheckLawDefinition,
  )

finCategoryLaws :: Harness.CategoryLaws FinCat
finCategoryLaws = Harness.mkCategoryLaws @FinCat sampleFinCat

categoryLeftIdProp :: SampleFinMorphism -> Bool
categoryLeftIdProp (SampleFinMorphism morphism) =
  Harness.categoryLeftIdentity finCategoryLaws morphism

categoryRightIdProp :: SampleFinMorphism -> Bool
categoryRightIdProp (SampleFinMorphism morphism) =
  Harness.categoryRightIdentity finCategoryLaws morphism

categoryAssocProp :: SampleComposableFinTriple -> Bool
categoryAssocProp (SampleComposableFinTriple firstValue secondValue thirdValue) =
  Harness.categoryAssociativity finCategoryLaws firstValue secondValue thirdValue

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "category"
      [ quickCheckLawDefinition CategoryLeftId categoryLeftIdProp,
        quickCheckLawDefinition CategoryRightId categoryRightIdProp,
        quickCheckLawDefinition CategoryAssoc categoryAssocProp
      ]
  ]
