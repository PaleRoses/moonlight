{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.SubsumptionProjectionSpec
  ( spec,
    prop_projectInjectivePreservesMultiplicity,
    prop_projectCollisionMergesMultiplicity,
    prop_projectRelationalBoundaryRejectsImpossibleTarget,
  )
where

import Moonlight.Flow.Carrier.Morphism.Internal.Projection
  ( projectRowDelta,
    projectRowDeltaExact,
  )
import Moonlight.Flow.Carrier.Morphism.SubsumptionProjectionFixtures
  ( CollisionProjectionCase (..),
    ImpossibleBoundaryProjectionCase (..),
    InjectiveProjectionCase (..),
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( projectRelationalBoundary,
  )
import Test.QuickCheck
  ( Property,
    counterexample,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

spec :: TestTree
spec =
  testGroup
    "SubsumptionProjection"
    [ testProperty "projectInjectivePreservesMultiplicity" prop_projectInjectivePreservesMultiplicity,
      testProperty "projectCollisionMergesMultiplicity" prop_projectCollisionMergesMultiplicity,
      testProperty "projectRelationalBoundaryRejectsImpossibleTarget" prop_projectRelationalBoundaryRejectsImpossibleTarget
    ]

prop_projectInjectivePreservesMultiplicity ::
  InjectiveProjectionCase ->
  Property
prop_projectInjectivePreservesMultiplicity testCase =
  counterexample "injective exact projection changed multiplicities" $
    projectRowDeltaExact
      (ipcProjection testCase)
      (ipcRows testCase)
      === Right (ipcExpectedRows testCase)

prop_projectCollisionMergesMultiplicity ::
  CollisionProjectionCase ->
  Property
prop_projectCollisionMergesMultiplicity testCase =
  counterexample "non-injective projection failed to merge multiplicities" $
    projectRowDelta
      (cpcProjection testCase)
      (cpcRows testCase)
      === Right (cpcMergedRows testCase)

prop_projectRelationalBoundaryRejectsImpossibleTarget ::
  ImpossibleBoundaryProjectionCase ->
  Property
prop_projectRelationalBoundaryRejectsImpossibleTarget testCase =
  case projectRelationalBoundary
    (ibpcBoundaryProjection testCase)
    (ibpcBoundary testCase) of
    Left _ ->
      property True
    Right projected ->
      counterexample ("unexpected projected boundary: " <> show projected) False
