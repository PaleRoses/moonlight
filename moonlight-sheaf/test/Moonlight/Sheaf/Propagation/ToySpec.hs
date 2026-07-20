{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Propagation.ToySpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Scope (dirtyScope)
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    certifySectionCompatibility,
  )
import Moonlight.Sheaf.TestFixture.PropagationToy
import Moonlight.Sheaf.TestFixture.PropagationToy.Audit
  ( fullToyCompatibilityAuditAfterPatchWith,
    scopedToyCompatibilityAuditAfterPatchWith,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelObjects,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    initialSheafModelVersion,
    mkObjectIndex,
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( discreteStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "PropagationToy"
    [ testCase "parent patch propagates to child through public sheaf section" testParentPatchPropagates,
      testCase "conflicting pinned child returns typed obstruction" testPinnedConflictRejected,
      testCase "full compatibility audit rejects unpropagated parent-only patch" testFullCompatibilityAuditRejectsUnpropagatedPatch,
      testCase "scoped compatibility audit agrees with full audit on rejection and certification" testScopedAuditAgreesWithFullAudit,
      testCase "extent propagation returns only the prepared descent section" testExtentPropagationShape,
      testCase "event stream propagation agrees with repeated and batch propagation" testEventStreamAgreesWithRepeatedAndBatchPropagation,
      testCase "synthetic chain fanout and diamond descend then audit" testSyntheticShapesDescendAndAudit
    ]

testParentPatchPropagates :: IO ()
testParentPatchPropagates =
  case
    ( withToySheaf $ \sheaf -> do
        section <- initialToySectionWith sheaf (ToyStalk 0)
        propagatedSection <- propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 7)])
        toySectionEntriesWith sheaf propagatedSection
    )
    of
    Left obstruction -> assertFailure ("expected propagation to succeed, received " <> show obstruction)
    Right propagatedEntries ->
      assertEqual
        "propagated entries"
        (Map.fromList [(ParentCell, ToyStalk 7), (ChildCell, ToyStalk 7)])
        propagatedEntries

testPinnedConflictRejected :: IO ()
testPinnedConflictRejected =
  case
    ( withToySheaf $ \sheaf -> do
        section <- initialToySectionWith sheaf (ToyStalk 0)
        _propagatedSection <- propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 7), (ChildCell, ToyStalk 3)])
        pure ()
    )
    of
    Left (ToyPinnedRestrictionConflict ChildCell (ToyStalk 3) (ToyStalk 7)) -> pure ()
    Left obstruction -> assertFailure ("expected pinned conflict, received " <> show obstruction)
    Right () -> assertFailure "expected propagation rejection"

testFullCompatibilityAuditRejectsUnpropagatedPatch :: IO ()
testFullCompatibilityAuditRejectsUnpropagatedPatch =
  case
    withToySheaf $ \sheaf -> do
      section <- initialToySectionWith sheaf (ToyStalk 0)
      fullToyCompatibilityAuditAfterPatchWith sheaf section (toyPatch [(ParentCell, ToyStalk 5)])
  of
    Right SectionCertified -> assertFailure "expected full consistency to reject parent-only direct patch"
    Right (SectionRejected mismatches) ->
      assertBool "child mismatch recorded" (Map.member ChildCell mismatches)
    Left obstruction ->
      assertFailure ("expected certification, received " <> show obstruction)

testScopedAuditAgreesWithFullAudit :: IO ()
testScopedAuditAgreesWithFullAudit = do
  rejecting <- auditPair (toyPatch [(ParentCell, ToyStalk 5)])
  case rejecting of
    Right (SectionRejected mismatches) ->
      assertBool "scoped audit records the incident child mismatch" (Map.member ChildCell mismatches)
    other ->
      assertFailure ("expected scoped rejection with mismatches, received " <> show other)
  certifying <- auditPair (toyPatch [(ParentCell, ToyStalk 4), (ChildCell, ToyStalk 4)])
  case certifying of
    Right SectionCertified -> pure ()
    other ->
      assertFailure ("expected scoped certification, received " <> show other)
  where
    auditPair patchValue =
      case
        withToySheaf $ \sheaf -> do
          section <- initialToySectionWith sheaf (ToyStalk 0)
          let scopedOutcome = scopedToyCompatibilityAuditAfterPatchWith sheaf section patchValue
          pure
            ( fullToyCompatibilityAuditAfterPatchWith sheaf section patchValue,
              scopedOutcome
            )
      of
        Left obstruction ->
          assertFailure ("expected initial section, received " <> show obstruction)
        Right (fullOutcome, scopedOutcome) -> do
          assertEqual
            "scoped audit verdict matches full audit"
            fullOutcome
            scopedOutcome
          pure scopedOutcome

testExtentPropagationShape :: IO ()
testExtentPropagationShape =
  case
    ( withToySheaf $ \sheaf -> do
        section <- initialToySectionWith sheaf (ToyStalk 0)
        propagatedSection <- propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 2)])
        toySectionEntriesWith sheaf propagatedSection
    )
    of
    Left obstruction -> assertFailure ("expected propagation to succeed, received " <> show obstruction)
    Right propagatedEntries ->
      assertEqual "accepted child" (Map.fromList [(ParentCell, ToyStalk 2), (ChildCell, ToyStalk 2)]) propagatedEntries

testEventStreamAgreesWithRepeatedAndBatchPropagation :: IO ()
testEventStreamAgreesWithRepeatedAndBatchPropagation =
  case keyedBatchEntries of
    Left obstruction -> assertFailure ("expected event stream to succeed, received " <> show obstruction)
    Right (repeatedEntries, batchedEntries, eventStreamEntries) -> do
      assertEqual "batch propagation has the same final entries as repeated propagation" repeatedEntries batchedEntries
      assertEqual "event stream transaction has the same final entries as repeated propagation" repeatedEntries eventStreamEntries
  where
    keyedBatchEntries =
      withToySheaf $ \sheaf -> do
        deltas <- toyBenchmarkKeyedPatches sheaf 7
        section0 <- initialToySectionWith sheaf (ToyStalk 0)
        repeatedSection <- foldM (propagateToyKeyedSectionWith sheaf) section0 deltas
        batchedSection <- propagateToyKeyedDeltasWith sheaf section0 deltas
        eventStreamSection <- propagateToyEventStreamWith sheaf section0 deltas
        repeatedEntries <- toySectionEntriesWith sheaf repeatedSection
        batchedEntries <- toySectionEntriesWith sheaf batchedSection
        eventStreamEntries <- toySectionEntriesWith sheaf eventStreamSection
        pure (repeatedEntries, batchedEntries, eventStreamEntries)

newtype SyntheticCell = SyntheticCell Int
  deriving stock (Eq, Ord, Show)

data SyntheticArrow = SyntheticArrow
  { syntheticArrowSource :: !SyntheticCell,
    syntheticArrowTarget :: !SyntheticCell
  }
  deriving stock (Eq, Ord, Show)

newtype SyntheticStalk = SyntheticStalk Int
  deriving stock (Eq, Ord, Show)

testSyntheticShapesDescendAndAudit :: IO ()
testSyntheticShapesDescendAndAudit =
  traverse_
    assertSyntheticShape
    [ (4, chainEdges 4, SyntheticCell 3),
      (4, fanoutEdges 4, SyntheticCell 3),
      (4, diamondEdges, SyntheticCell 3)
    ]

assertSyntheticShape :: (Int, [SyntheticArrow], SyntheticCell) -> IO ()
assertSyntheticShape (cellCount, arrows, targetCell) = do
  syntheticAssertion <-
    expectRight
      ( withSyntheticModel cellCount arrows $ \model -> do
          preparedDescent <- expectRight (prepareSectionDescent model)
          section <- expectRight (mkTotalSectionStore model (zeroSyntheticEntries cellCount))
          sourceKey <- expectRight (keyForSyntheticCell model (SyntheticCell 0))
          descentResult <-
            expectRight
              ( descendPreparedLocalKeyedBatch
                  preparedDescent
                  discreteStalkAlgebra
                  ObserveFinalSection
                  [syntheticDelta model sourceKey]
                  section
              )
          assertEqual
            "target propagated"
            (Right (SyntheticStalk 7))
            (totalStalkAt model targetCell (sdrSection descentResult))
          assertEqual
            "full compatibility audit"
            (Right SectionCertified)
            (certifySectionCompatibility model discreteStalkAlgebra (sdrSection descentResult))
      )
  syntheticAssertion

withSyntheticModel ::
  Int ->
  [SyntheticArrow] ->
  (forall owner. SheafModel owner SyntheticCell SyntheticArrow -> result) ->
  Either String result
withSyntheticModel cellCount arrows useModel =
  either (Left . show) Right
    ( withPreparedSheafModel
        initialSheafModelVersion
        (mkObjectIndex (fmap SyntheticCell [0 .. cellCount - 1]))
        ( \arrow ->
            RestrictionParts
              { partKind = unitIncidenceRestriction,
                partSource = syntheticArrowSource arrow,
                partTarget = syntheticArrowTarget arrow,
                partWitness = arrow
              }
        )
        arrows
        useModel
    )

zeroSyntheticEntries :: Int -> Map.Map SyntheticCell SyntheticStalk
zeroSyntheticEntries cellCount =
  Map.fromList (fmap (\cell -> (cell, SyntheticStalk 0)) (fmap SyntheticCell [0 .. cellCount - 1]))

syntheticDelta :: SheafModel owner SyntheticCell SyntheticArrow -> ObjectKey -> KeyedSectionDelta owner SyntheticStalk
syntheticDelta _model sourceKey =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton (unObjectKey sourceKey)),
      ksdAssignments = IntMap.singleton (unObjectKey sourceKey) (SyntheticStalk 7)
    }

keyForSyntheticCell :: SheafModel owner SyntheticCell SyntheticArrow -> SyntheticCell -> Either String ObjectKey
keyForSyntheticCell model cell =
  case denseIndexKeyOf cell (sheafModelObjects model) of
    Just key ->
      Right key
    Nothing ->
      Left ("missing synthetic cell " <> show cell)

chainEdges :: Int -> [SyntheticArrow]
chainEdges cellCount =
  fmap
    (\sourceOrdinal -> SyntheticArrow (SyntheticCell sourceOrdinal) (SyntheticCell (sourceOrdinal + 1)))
    [0 .. cellCount - 2]

fanoutEdges :: Int -> [SyntheticArrow]
fanoutEdges cellCount =
  fmap
    (\targetOrdinal -> SyntheticArrow (SyntheticCell 0) (SyntheticCell targetOrdinal))
    [1 .. cellCount - 1]

diamondEdges :: [SyntheticArrow]
diamondEdges =
  [ SyntheticArrow (SyntheticCell 0) (SyntheticCell 1),
    SyntheticArrow (SyntheticCell 0) (SyntheticCell 2),
    SyntheticArrow (SyntheticCell 1) (SyntheticCell 3),
    SyntheticArrow (SyntheticCell 2) (SyntheticCell 3)
  ]

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight =
  either (assertFailure . ("expected Right, received " <>) . show) pure
