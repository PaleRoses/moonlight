
module Moonlight.Category.Effect.Laws.Limits
  ( lawBundles,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleUnitMorphism (..),
    SampleUnitObject (..),
  )
import Moonlight.Category.Pure.Unit (UnitCat (..), UnitMor (..))
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)

productProj1Prop :: SampleUnitObject -> Bool
productProj1Prop (SampleUnitObject productObject) =
  Harness.productProjection1 @UnitCat UnitCat productObject UnitMor UnitMor

productProj2Prop :: SampleUnitObject -> Bool
productProj2Prop (SampleUnitObject productObject) =
  Harness.productProjection2 @UnitCat UnitCat productObject UnitMor UnitMor

coproductInj1Prop :: SampleUnitObject -> Bool
coproductInj1Prop (SampleUnitObject coproductObject) =
  Harness.coproductInjection1 @UnitCat UnitCat coproductObject UnitMor UnitMor

coproductInj2Prop :: SampleUnitObject -> Bool
coproductInj2Prop (SampleUnitObject coproductObject) =
  Harness.coproductInjection2 @UnitCat UnitCat coproductObject UnitMor UnitMor

pullbackProp :: SampleUnitMorphism -> Bool
pullbackProp (SampleUnitMorphism morphism) =
  Harness.pullbackCommutative @UnitCat UnitCat morphism morphism

pushoutProp :: SampleUnitMorphism -> Bool
pushoutProp (SampleUnitMorphism morphism) =
  Harness.pushoutCommutative @UnitCat UnitCat morphism morphism

equalizerProp :: SampleUnitMorphism -> Bool
equalizerProp (SampleUnitMorphism morphism) =
  Harness.equalizerCommutative @UnitCat UnitCat morphism morphism

coequalizerProp :: SampleUnitMorphism -> Bool
coequalizerProp (SampleUnitMorphism morphism) =
  Harness.coequalizerCommutative @UnitCat UnitCat morphism morphism

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "limits"
      [ quickCheckLawDefinition ProductProj1 productProj1Prop,
        quickCheckLawDefinition ProductProj2 productProj2Prop,
        quickCheckLawDefinition CoproductInj1 coproductInj1Prop,
        quickCheckLawDefinition CoproductInj2 coproductInj2Prop,
        quickCheckLawDefinition PullbackCommutes pullbackProp,
        quickCheckLawDefinition PushoutCommutes pushoutProp,
        quickCheckLawDefinition EqualizerCommutes equalizerProp,
        quickCheckLawDefinition CoequalizerCommutes coequalizerProp
      ]
  ]
