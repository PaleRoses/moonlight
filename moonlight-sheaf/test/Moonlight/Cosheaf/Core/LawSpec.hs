{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Core.LawSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
import Moonlight.Cosheaf.Test.Fixture
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "finite cosheaf laws"
    [ testCase "identity corestriction law passes for every object value" testIdentityLawPassesEverywhere,
      testCase "covariant composition law passes for every composable pair" testCompositionLawPassesEverywhere,
      testCase "compiled corestriction tables agree with the build-time algebra" testCompiledTablesAgree,
      testCase "constructor rejects bad identity with typed mismatch" testBadIdentityMismatch,
      testCase "constructor rejects bad composition with typed mismatch" testBadCompositionMismatch,
      testCase "constructor rejects missing composite with typed obstruction" testMissingComposite,
      testCase "constructor rejects core action failure with typed obstruction" testCoreActionFailure,
      testCase "constructor rejects missing costalk" testMissingCostalk,
      testCase "constructor rejects unknown costalk object" testUnknownCostalkObject,
      testCase "constructor rejects non-normalized value" testNonNormalizedValue,
      testCase "constructor rejects duplicate value" testDuplicateValue,
      testCase "constructor rejects unknown morphism source through site-index failure" testUnknownMorphismSource,
      testCase "constructor rejects unknown morphism target through site-index failure" testUnknownMorphismTarget,
      testCase "constructor rejects outside-target corestriction" testOutsideTargetCorestriction
    ]

testIdentityLawPassesEverywhere :: Assertion
testIdentityLawPassesEverywhere =
  traverse_ assertIdentityAtObject (siteObjects chainSite)
  where
    chainSite =
      ChainSite ChainGoodSite

    assertIdentityAtObject objectValue =
      traverse_
        ( \value ->
            assertEqual
              ("identity law at " <> show objectValue <> " / " <> show value)
              (Right ())
              ( checkCorestrictionIdentityLawWith
                  chainCorestrictValue
                  chainMismatch
                  chainSite
                  objectValue
                  value
              )
        )
        (chainValuesAt objectValue)

testCompositionLawPassesEverywhere :: Assertion
testCompositionLawPassesEverywhere =
  traverse_ assertCompositionAtPair composablePairs
  where
    chainSite =
      ChainSite ChainGoodSite

    indexedMorphisms =
      siteMorphisms chainSite <> fmap (identityAt chainSite) (siteObjects chainSite)

    composablePairs =
      filter
        (\(outerMorphism, innerMorphism) -> cmSource outerMorphism == cmTarget innerMorphism)
        [ (outerMorphism, innerMorphism)
        | outerMorphism <- indexedMorphisms,
          innerMorphism <- indexedMorphisms
        ]

    assertCompositionAtPair (outerMorphism, innerMorphism) =
      traverse_
        ( \value ->
            assertEqual
              ( "composition law for "
                  <> show (cmWitness outerMorphism)
                  <> " after "
                  <> show (cmWitness innerMorphism)
                  <> " / "
                  <> show value
              )
              (Right ())
              ( checkCorestrictionCompositionLawWith
                  chainCorestrictValue
                  chainMismatch
                  chainSite
                  outerMorphism
                  innerMorphism
                  value
              )
        )
        (chainValuesAt (cmSource innerMorphism))

testCompiledTablesAgree :: Assertion
testCompiledTablesAgree = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  traverse_ (assertCompiledCorestriction cosheaf) (finiteCosheafCorestrictions cosheaf)

assertCompiledCorestriction :: FiniteCosheaf ChainSite Int -> CompiledCorestriction ChainObject ChainMorphism -> Assertion
assertCompiledCorestriction cosheaf corestrictionValue = do
  sourceCostalk <-
    maybe
      (assertFailure ("missing source costalk: " <> show (cmSource morphismValue)))
      pure
      (finiteCostalkAt (cmSource morphismValue) cosheaf)
  targetCostalk <-
    maybe
      (assertFailure ("missing target costalk: " <> show (cmTarget morphismValue)))
      pure
      (finiteCostalkAt (cmTarget morphismValue) cosheaf)
  traverse_
    (assertCompiledSource targetCostalk)
    (finiteCostalkKeys sourceCostalk)
  where
    morphismValue =
      ccMorphism corestrictionValue

    assertCompiledSource targetCostalk sourceKey = do
      sourceValue <-
        maybe
          (assertFailure ("missing source value: " <> show sourceKey))
          pure
          (finiteCostalkValueAt sourceKey =<< finiteCostalkAt (cmSource morphismValue) cosheaf)
      targetValue <- expectRight (chainCorestrictValue morphismValue sourceValue)
      let expectedTargetKey =
            finiteCostalkKeyOf targetValue targetCostalk
          compiledTargetKey =
            corestrictCostalkKey morphismValue sourceKey cosheaf
      assertEqual
        ("compiled corestriction agrees for " <> show (cmWitness morphismValue) <> " / " <> show sourceValue)
        expectedTargetKey
        compiledTargetKey

testBadIdentityMismatch :: Assertion
testBadIdentityMismatch =
  case chainCosheaf (ChainSite ChainGoodSite) chainIdentityMismatchAlgebra of
    Left (FiniteCorestrictionIdentityMismatch morphismValue 0 [()]) ->
      assertEqual "identity mismatch object" (identityAt (ChainSite ChainGoodSite) ChainA) morphismValue
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected identity mismatch"

testBadCompositionMismatch :: Assertion
testBadCompositionMismatch =
  case chainCosheaf (ChainSite ChainGoodSite) chainCompositionMismatchAlgebra of
    Left (FiniteCorestrictionCompositionMismatch outerMorphism innerMorphism compositeMorphism 0 [()]) -> do
      assertEqual "outer morphism" chainBC outerMorphism
      assertEqual "inner morphism" chainAB innerMorphism
      assertEqual "composite morphism" chainAC compositeMorphism
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected composition mismatch"

testMissingComposite :: Assertion
testMissingComposite =
  case chainCosheaf (ChainSite ChainMissingCompositeSite) chainGoodAlgebra of
    Left (FiniteCorestrictionCompositionUndefined outerMorphism innerMorphism) -> do
      assertEqual "outer morphism" chainBC outerMorphism
      assertEqual "inner morphism" chainAB innerMorphism
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected missing composite"

testCoreActionFailure :: Assertion
testCoreActionFailure =
  case chainCosheaf (ChainSite ChainGoodSite) chainCoreFailureAlgebra of
    Left (FiniteCorestrictionFailed morphismValue 0 ChainCoreFailure) ->
      assertEqual "failed morphism" chainAB morphismValue
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected core action failure"

testMissingCostalk :: Assertion
testMissingCostalk =
  case mkFiniteCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra (Map.delete ChainB chainRawCostalks) of
    Left (FiniteCostalkMissing ChainB) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected missing costalk"

testUnknownCostalkObject :: Assertion
testUnknownCostalkObject =
  case mkFiniteCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra (Map.insert ChainGhost [0] chainRawCostalks) of
    Left (FiniteCostalkUnknownObject ChainGhost) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected unknown costalk object"

testNonNormalizedValue :: Assertion
testNonNormalizedValue =
  case mkFiniteCosheaf (ChainSite ChainGoodSite) nonNormalizedAlgebra chainRawCostalks of
    Left (FiniteCostalkValueNotNormalized ChainA 0 1) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected non-normalized value"
  where
    nonNormalizedAlgebra =
      chainGoodAlgebra {fcaNormalize = \_objectValue value -> value + 1}

testDuplicateValue :: Assertion
testDuplicateValue =
  case mkFiniteCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra duplicateCostalks of
    Left (FiniteCostalkDuplicateValue ChainA 0) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected duplicate value"
  where
    duplicateCostalks =
      Map.insert ChainA [0, 0] chainRawCostalks

testUnknownMorphismSource :: Assertion
testUnknownMorphismSource =
  case chainCosheaf (ChainSite ChainUnknownSourceSite) chainGoodAlgebra of
    Left (CosheafSiteIndexInvalid _) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected site-index unknown source failure"

testUnknownMorphismTarget :: Assertion
testUnknownMorphismTarget =
  case chainCosheaf (ChainSite ChainUnknownTargetSite) chainGoodAlgebra of
    Left (CosheafSiteIndexInvalid _) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected site-index unknown target failure"

testOutsideTargetCorestriction :: Assertion
testOutsideTargetCorestriction =
  case chainCosheaf (ChainSite ChainGoodSite) outsideTargetAlgebra of
    Left (FiniteCorestrictionOutsideCostalk morphismValue 0 999) ->
      assertEqual "outside target morphism" chainAB morphismValue
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected outside-target corestriction"
  where
    outsideTargetAlgebra =
      chainGoodAlgebra
        { fcaCorestrict = \morphismValue value ->
            case (cmWitness morphismValue, value) of
              (ChainAB, 0) -> Right 999
              _ -> chainCorestrictValue morphismValue value
        }

chainValuesAt :: ChainObject -> [Int]
chainValuesAt objectValue =
  Map.findWithDefault [] objectValue chainRawCostalks
