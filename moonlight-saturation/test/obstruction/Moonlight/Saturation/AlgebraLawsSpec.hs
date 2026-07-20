module Moonlight.Saturation.AlgebraLawsSpec
  ( algebraLawsTests,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( ObstructionFootprint (..),
    ObstructionInvalidation (..),
  )
import Moonlight.Saturation.Substrate
  ( FactViewGraphChanges (..),
  )
import Test.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    chooseInt,
    forAll,
    listOf,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- The hand-rolled instances these laws pin down are refactor insurance: a
-- future collapse of the obstruction algebra must die here, not in review.
algebraLawsTests :: TestTree
algebraLawsTests =
  testGroup
    "algebra-laws"
    [ boundedSemilatticeLaws "ObstructionFootprint" genObstructionFootprint,
      boundedSemilatticeLaws "ObstructionInvalidation" genObstructionInvalidation,
      boundedSemilatticeLaws "FactViewGraphChanges" genFactViewGraphChanges
    ]

-- Every algebra here is a bounded join-semilattice, so the full law set is
-- commutative idempotent monoid, not merely monoid.
boundedSemilatticeLaws :: (Eq a, Show a, Monoid a) => String -> Gen a -> TestTree
boundedSemilatticeLaws typeName gen =
  testGroup
    typeName
    [ testProperty "semigroup associativity" (associativity gen),
      testProperty "monoid left identity" (leftIdentity gen),
      testProperty "monoid right identity" (rightIdentity gen),
      testProperty "semilattice commutativity" (commutativity gen),
      testProperty "semilattice idempotence" (idempotence gen)
    ]

associativity :: (Eq a, Show a, Semigroup a) => Gen a -> Property
associativity gen =
  forAll ((,,) <$> gen <*> gen <*> gen) $ \(x, y, z) ->
    (x <> y) <> z === x <> (y <> z)

leftIdentity :: (Eq a, Show a, Monoid a) => Gen a -> Property
leftIdentity gen =
  forAll gen $ \x -> mempty <> x === x

rightIdentity :: (Eq a, Show a, Monoid a) => Gen a -> Property
rightIdentity gen =
  forAll gen $ \x -> x <> mempty === x

commutativity :: (Eq a, Show a, Semigroup a) => Gen a -> Property
commutativity gen =
  forAll ((,) <$> gen <*> gen) $ \(x, y) ->
    x <> y === y <> x

idempotence :: (Eq a, Show a, Semigroup a) => Gen a -> Property
idempotence gen =
  forAll gen $ \x -> x <> x === x

genSupportKeys :: Gen IntSet
genSupportKeys =
  IntSet.fromList <$> listOf (chooseInt (0, 63))

genRoots :: Gen (Set Int)
genRoots =
  Set.fromList <$> listOf (chooseInt (0, 15))

genObstructionFootprint :: Gen (ObstructionFootprint Int)
genObstructionFootprint =
  ObstructionFootprint
    <$> genSupportKeys
    <*> genSupportKeys
    <*> genSupportKeys
    <*> genRoots

genObstructionInvalidation :: Gen (ObstructionInvalidation Int)
genObstructionInvalidation =
  ObstructionInvalidation
    <$> genSupportKeys
    <*> genSupportKeys
    <*> genSupportKeys
    <*> genRoots
    <*> arbitrary

genFactViewGraphChanges :: Gen (FactViewGraphChanges Int)
genFactViewGraphChanges =
  FactViewGraphChanges
    <$> arbitrary
    <*> genRoots
