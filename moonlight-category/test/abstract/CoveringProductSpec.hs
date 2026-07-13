{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module CoveringProductSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Category
  ( CoveringFamily (..),
    CoveringProduct,
    Exists (..),
    adjustCoveringProduct,
    indexCoveringProduct,
    replaceCoveringProduct,
    restrictCoveringProduct,
    tabulateCoveringProduct,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

type DemoField :: Type
data DemoField
  = AlphaField
  | BetaField
  | GammaField

type DemoFieldWitness :: DemoField -> Type
data DemoFieldWitness (field :: DemoField) where
  AlphaFieldWitness :: DemoFieldWitness 'AlphaField
  BetaFieldWitness :: DemoFieldWitness 'BetaField
  GammaFieldWitness :: DemoFieldWitness 'GammaField

type DemoSubsetWitness :: DemoField -> Type
data DemoSubsetWitness (field :: DemoField) where
  AlphaSubsetWitness :: DemoSubsetWitness 'AlphaField
  GammaSubsetWitness :: DemoSubsetWitness 'GammaField

type DemoValue :: DemoField -> Type
newtype DemoValue (field :: DemoField) = DemoValue
  { unDemoValue :: String
  }
  deriving stock (Eq, Show)

instance CoveringFamily DemoFieldWitness where
  allMembers =
    [ Exists AlphaFieldWitness,
      Exists BetaFieldWitness,
      Exists GammaFieldWitness
    ]

instance CoveringFamily DemoSubsetWitness where
  allMembers =
    [ Exists AlphaSubsetWitness,
      Exists GammaSubsetWitness
    ]

sameDemoFieldWitness ::
  DemoFieldWitness left ->
  DemoFieldWitness right ->
  Maybe (left :~: right)
sameDemoFieldWitness leftWitness rightWitness =
  case (leftWitness, rightWitness) of
    (AlphaFieldWitness, AlphaFieldWitness) -> Just Refl
    (BetaFieldWitness, BetaFieldWitness) -> Just Refl
    (GammaFieldWitness, GammaFieldWitness) -> Just Refl
    _ -> Nothing

embedDemoSubsetWitness ::
  DemoSubsetWitness field ->
  DemoFieldWitness field
embedDemoSubsetWitness subsetWitness =
  case subsetWitness of
    AlphaSubsetWitness -> AlphaFieldWitness
    GammaSubsetWitness -> GammaFieldWitness

demoProduct :: CoveringProduct DemoFieldWitness DemoValue
demoProduct =
  tabulateCoveringProduct
    ( \witness ->
        case witness of
          AlphaFieldWitness -> DemoValue "alpha"
          BetaFieldWitness -> DemoValue "beta"
          GammaFieldWitness -> DemoValue "gamma"
    )

tests :: TestTree
tests =
  testGroup
    "CoveringProduct"
    [ testCase "restrictCoveringProduct projects a witness-indexed subset" $
        let restrictedProduct =
              restrictCoveringProduct embedDemoSubsetWitness demoProduct
         in do
              unDemoValue (indexCoveringProduct restrictedProduct AlphaSubsetWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct restrictedProduct GammaSubsetWitness) @?= "gamma",
      testCase "adjustCoveringProduct updates exactly the targeted witness" $
        let adjustedProduct =
              adjustCoveringProduct
                sameDemoFieldWitness
                BetaFieldWitness
                (\(DemoValue value) -> DemoValue (value <> "-adjusted"))
                demoProduct
         in do
              unDemoValue (indexCoveringProduct adjustedProduct AlphaFieldWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct adjustedProduct BetaFieldWitness) @?= "beta-adjusted"
              unDemoValue (indexCoveringProduct adjustedProduct GammaFieldWitness) @?= "gamma",
      testCase "replaceCoveringProduct delegates through typed witness equality" $
        let replacedProduct =
              replaceCoveringProduct
                sameDemoFieldWitness
                GammaFieldWitness
                (DemoValue "gamma-replaced")
                demoProduct
         in do
              unDemoValue (indexCoveringProduct replacedProduct AlphaFieldWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct replacedProduct BetaFieldWitness) @?= "beta"
              unDemoValue (indexCoveringProduct replacedProduct GammaFieldWitness) @?= "gamma-replaced"
    ]
