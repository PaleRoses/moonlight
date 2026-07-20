{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Descent.ContextSiteSpec
  ( tests,
  )
where

import Data.Foldable (foldlM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sort, subsequences)
import Data.Set qualified as Set
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError (..),
    classKeysVisibleAtKey,
    classSupportDeltaTouchedCarriers,
    classSupportDeltaTouchedClassKeys,
    classSupportIndexCarrierGeneratorCount,
    classSupportIndexFromEntries,
    classSupportIndexGeneratorBucketCount,
    classSupportIndexInsert,
    classSupportIndexSupportEntryCount,
    contextObjectKeyFor,
    emptyClassSupportIndex,
    extendJoinClosureOverContexts,
    joinClosureOverContexts,
    meetPreparedSupport,
    normalizePreparedSupport,
    preparedSupportObjects,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
    supportCarrierGeneratorCount,
    supportCarrierReachableObjects,
    unionPreparedSupport,
    withPreparedContextSiteFromFiniteLattice,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    branchContextLattice,
    branchContexts,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    normalizeSupport,
    supportBasis,
    supportContains,
    supportMeet,
    supportUnion
  )
import Moonlight.FiniteLattice
  ( singletonContextLattice
  )


tests :: TestTree
tests =
  testGroup
    "context site carrier support"
    [ testCase "carrier containment agrees with the lattice-level definition" testContainmentAgreesWithLattice,
      testCase "carrier normalization agrees with the lattice-level definition" testNormalizeAgreesWithLattice,
      testCase "carrier union agrees with the lattice-level definition" testUnionAgreesWithLattice,
      testCase "carrier meet generates the same upward closure as the lattice-level meet" testMeetAgreesWithLattice,
      testCase "visible class keys mirror support containment" testVisibleKeysMirrorContainment,
      testCase "keyed visibility mirrors carrier support containment" testKeyedVisibilityMirrorsCarrierSupport,
      testCase "carrier storage keeps generators instead of expanded closures" testCarrierStorageKeepsGenerators,
      testCase "support deltas expose compact carrier touches" testSupportDeltaTouchesCarriers,
      testCase "visible class keys reject unknown contexts" testVisibleKeysRejectUnknownContext,
      testCase "incremental support insertion agrees with bulk construction" testIncrementalInsertAgreesWithFromEntries,
      testCase "join closure extension agrees with the full closure oracle" testJoinClosureExtensionAgreesWithOracle
    ]

testJoinClosureExtensionAgreesWithOracle :: Assertion
testJoinClosureExtensionAgreesWithOracle =
  sequence_
    [ let (priorClosure, priorFailures) =
            joinClosureOverContexts branchContextLattice priorBase
          (extendedClosure, freshFailures) =
            extendJoinClosureOverContexts branchContextLattice priorBase priorClosure base
          (oracleClosure, oracleFailures) =
            joinClosureOverContexts branchContextLattice base
       in do
            assertEqual
              ( "expected closure extension from "
                  <> show priorBase
                  <> " to "
                  <> show base
                  <> " to agree with the full closure oracle"
              )
              oracleClosure
              extendedClosure
            assertEqual
              ( "expected accumulated join failures from "
                  <> show priorBase
                  <> " to "
                  <> show base
                  <> " to agree with the oracle failure multiset"
              )
              (sort oracleFailures)
              (sort (priorFailures <> freshFailures))
    | priorBase <- subsequences branchContexts,
      base <- subsequences branchContexts,
      Set.fromList priorBase `Set.isSubsetOf` Set.fromList base
    ]

withBranchSite ::
  (forall owner. PreparedContextSite owner BranchContext -> Assertion) ->
  Assertion
withBranchSite =
  withPreparedContextSiteFromFiniteLattice branchContextLattice

generatorSubsets :: [[BranchContext]]
generatorSubsets =
  filter (not . null) (subsequences branchContexts)

supportCandidates :: IO [SupportBasis BranchContext]
supportCandidates =
  traverse (requireRight . supportBasis branchContextLattice) generatorSubsets

requireRight :: Show errorValue => Either errorValue value -> IO value
requireRight resultValue =
  case resultValue of
    Left errorValue ->
      assertFailure ("expected Right, got " <> show errorValue)
    Right value ->
      pure value

testContainmentAgreesWithLattice :: Assertion
testContainmentAgreesWithLattice =
  withBranchSite $ \branchSite -> do
    candidates <- supportCandidates
    mapM_
      ( \supportValue ->
          mapM_
            ( \contextValue -> do
                contextKey <-
                  requireRight (contextObjectKeyFor branchSite contextValue)
                supportCarrier <-
                  requireRight (supportCarrierFromSupport branchSite supportValue)
                let compiled =
                      supportCarrierContainsKey branchSite supportCarrier contextKey
                algebraic <-
                  requireRight (supportContains branchContextLattice supportValue contextValue)
                compiled @?= algebraic
            )
            branchContexts
      )
      candidates

testNormalizeAgreesWithLattice :: Assertion
testNormalizeAgreesWithLattice =
  withBranchSite $ \branchSite -> do
    candidates <- supportCandidates
    mapM_
      ( \supportValue -> do
          compiled <-
            requireRight (normalizePreparedSupport branchSite supportValue)
          algebraic <-
            requireRight (normalizeSupport branchContextLattice supportValue)
          compiled @?= algebraic
      )
      candidates

testUnionAgreesWithLattice :: Assertion
testUnionAgreesWithLattice =
  withBranchSite $ \branchSite -> do
    candidates <- supportCandidates
    mapM_
      ( \(leftSupport, rightSupport) -> do
          compiled <-
            requireRight (unionPreparedSupport branchSite leftSupport rightSupport)
          algebraic <-
            requireRight (supportUnion branchContextLattice leftSupport rightSupport)
          compiled @?= algebraic
      )
      [ (leftSupport, rightSupport)
        | leftSupport <- candidates,
          rightSupport <- candidates
      ]

testMeetAgreesWithLattice :: Assertion
testMeetAgreesWithLattice =
  withBranchSite $ \branchSite -> do
    candidates <- supportCandidates
    mapM_
      ( \(leftSupport, rightSupport) -> do
          compiled <-
            requireRight (meetPreparedSupport branchSite leftSupport rightSupport)
          algebraic <-
            requireRight (supportMeet branchContextLattice leftSupport rightSupport)
          compiledObjects <-
            requireRight (preparedSupportObjects branchSite compiled)
          algebraicObjects <-
            requireRight (preparedSupportObjects branchSite algebraic)
          compiledObjects @?= algebraicObjects
      )
      [ (leftSupport, rightSupport)
        | leftSupport <- candidates,
          rightSupport <- candidates
      ]

indexedSupportEntries :: IO [(Int, SupportBasis BranchContext)]
indexedSupportEntries =
  fmap (zip [0 ..]) supportCandidates

testVisibleKeysMirrorContainment :: Assertion
testVisibleKeysMirrorContainment =
  withBranchSite $ \branchSite -> do
    entries <- indexedSupportEntries
    builtIndex <-
      requireRight
        (classSupportIndexFromEntries branchSite (IntMap.fromList entries))
    mapM_
      ( \(classKey, supportValue) ->
          mapM_
            ( \contextValue -> do
                contextKey <-
                  requireRight (contextObjectKeyFor branchSite contextValue)
                supportCarrier <-
                  requireRight (supportCarrierFromSupport branchSite supportValue)
                let contained =
                      supportCarrierContainsKey branchSite supportCarrier contextKey
                    visibleKeys =
                      classKeysVisibleAtKey branchSite builtIndex contextKey
                assertBool
                  ("visibility of class " <> show classKey <> " at " <> show contextValue)
                  (IntSet.member classKey visibleKeys == contained)
            )
            branchContexts
      )
      entries

testKeyedVisibilityMirrorsCarrierSupport :: Assertion
testKeyedVisibilityMirrorsCarrierSupport =
  withBranchSite $ \branchSite -> do
    entries <- indexedSupportEntries
    builtIndex <-
      requireRight
        (classSupportIndexFromEntries branchSite (IntMap.fromList entries))
    mapM_
      ( \contextValue -> do
          contextKey <-
            requireRight (contextObjectKeyFor branchSite contextValue)
          let visibleKeys =
                classKeysVisibleAtKey branchSite builtIndex contextKey
          mapM_
            ( \(classKey, supportValue) -> do
                supportCarrier <-
                  requireRight (supportCarrierFromSupport branchSite supportValue)
                let keyedContains =
                      supportCarrierContainsKey branchSite supportCarrier contextKey
                assertBool
                  ("keyed containment of class " <> show classKey <> " at " <> show contextValue)
                  (IntSet.member classKey visibleKeys == keyedContains)
            )
            entries
      )
      branchContexts

testCarrierStorageKeepsGenerators :: Assertion
testCarrierStorageKeepsGenerators =
  withBranchSite $ \branchSite -> do
    localSupport <- requireRight (supportBasis branchContextLattice [BranchLeft])
    supportCarrier <-
      requireRight (supportCarrierFromSupport branchSite localSupport)
    reachableObjects <-
      requireRight (supportCarrierReachableObjects branchSite (Set.fromList branchContexts) supportCarrier)
    assertBool
      "carrier generator footprint should be smaller than reachable closure"
      (supportCarrierGeneratorCount supportCarrier < Set.size reachableObjects)
    (supportIndex, _supportDelta) <-
      requireRight (classSupportIndexInsert branchSite localSupport 99 emptyClassSupportIndex)
    classSupportIndexSupportEntryCount supportIndex @?= 1
    classSupportIndexCarrierGeneratorCount supportIndex @?= 1
    classSupportIndexGeneratorBucketCount supportIndex @?= 1

testSupportDeltaTouchesCarriers :: Assertion
testSupportDeltaTouchesCarriers =
  withBranchSite $ \branchSite -> do
    localSupport <- requireRight (supportBasis branchContextLattice [BranchLeft])
    (_supportIndex, supportDelta) <-
      requireRight (classSupportIndexInsert branchSite localSupport 99 emptyClassSupportIndex)
    classSupportDeltaTouchedClassKeys supportDelta @?= IntSet.singleton 99
    fmap supportCarrierGeneratorCount (IntMap.elems (classSupportDeltaTouchedCarriers supportDelta)) @?= [1]


testVisibleKeysRejectUnknownContext :: Assertion
testVisibleKeysRejectUnknownContext =
  withPreparedContextSiteFromFiniteLattice (singletonContextLattice BranchBase) $ \singleSite ->
    contextObjectKeyFor singleSite BranchLeft
      @?= Left (PreparedContextSupportObjectMissing BranchLeft)

testIncrementalInsertAgreesWithFromEntries :: Assertion
testIncrementalInsertAgreesWithFromEntries =
  withBranchSite $ \branchSite -> do
    entries <- indexedSupportEntries
    bulkIndex <-
      requireRight
        (classSupportIndexFromEntries branchSite (IntMap.fromList entries))
    incrementalIndex <-
      requireRight
        ( foldlM
            ( \indexValue (classKey, supportValue) ->
                fmap fst (classSupportIndexInsert branchSite supportValue classKey indexValue)
            )
            emptyClassSupportIndex
            entries
        )
    incrementalIndex @?= bulkIndex
