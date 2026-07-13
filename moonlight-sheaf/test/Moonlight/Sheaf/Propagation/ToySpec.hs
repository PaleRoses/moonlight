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
  ( fullToyCompatibilityAuditAfterPatch,
    scopedToyCompatibilityAuditAfterPatch,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    prepareSheafModel,
    sheafModelFingerprint,
    sheafModelObjects,
    sheafModelVersion,
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
      testCase "event stream aborts atomically on an unknown extent ordinal" testEventStreamUnknownExtentAbortsAtomically,
      testCase "synthetic chain fanout and diamond descend then audit" testSyntheticShapesDescendAndAudit
    ]

testParentPatchPropagates :: IO ()
testParentPatchPropagates =
  case
    ( do
        sheaf <- toySheaf
        section <- initialToySectionWith sheaf (ToyStalk 0)
        propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 7)])
    )
    of
    Left obstruction -> assertFailure ("expected propagation to succeed, received " <> show obstruction)
    Right propagatedSection ->
      assertEqual
        "propagated entries"
        (Right (Map.fromList [(ParentCell, ToyStalk 7), (ChildCell, ToyStalk 7)]))
        (toySectionEntries propagatedSection)

testPinnedConflictRejected :: IO ()
testPinnedConflictRejected =
  case
    ( do
        sheaf <- toySheaf
        section <- initialToySectionWith sheaf (ToyStalk 0)
        propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 7), (ChildCell, ToyStalk 3)])
    )
    of
    Left (ToyPinnedRestrictionConflict ChildCell (ToyStalk 3) (ToyStalk 7)) -> pure ()
    Left obstruction -> assertFailure ("expected pinned conflict, received " <> show obstruction)
    Right propagatedSection -> assertFailure ("expected propagation rejection, received " <> show propagatedSection)

testFullCompatibilityAuditRejectsUnpropagatedPatch :: IO ()
testFullCompatibilityAuditRejectsUnpropagatedPatch =
  case initialToySection (ToyStalk 0) >>= \section -> fullToyCompatibilityAuditAfterPatch section (toyPatch [(ParentCell, ToyStalk 5)]) of
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
      case initialToySection (ToyStalk 0) of
        Left obstruction ->
          assertFailure ("expected initial section, received " <> show obstruction)
        Right section -> do
          let scopedOutcome = scopedToyCompatibilityAuditAfterPatch section patchValue
          assertEqual
            "scoped audit verdict matches full audit"
            (fullToyCompatibilityAuditAfterPatch section patchValue)
            scopedOutcome
          pure scopedOutcome

testExtentPropagationShape :: IO ()
testExtentPropagationShape =
  case
    ( do
        sheaf <- toySheaf
        section <- initialToySectionWith sheaf (ToyStalk 0)
        propagateToySectionWith sheaf section (toyPatch [(ParentCell, ToyStalk 2)])
    )
    of
    Left obstruction -> assertFailure ("expected propagation to succeed, received " <> show obstruction)
    Right propagatedSection ->
      assertEqual "accepted child" (Right (Map.fromList [(ParentCell, ToyStalk 2), (ChildCell, ToyStalk 2)])) (toySectionEntries propagatedSection)

testEventStreamAgreesWithRepeatedAndBatchPropagation :: IO ()
testEventStreamAgreesWithRepeatedAndBatchPropagation =
  case keyedBatchEntries of
    Left obstruction -> assertFailure ("expected event stream to succeed, received " <> show obstruction)
    Right (repeatedEntries, batchedEntries, eventStreamEntries) -> do
      assertEqual "batch propagation has the same final entries as repeated propagation" repeatedEntries batchedEntries
      assertEqual "event stream transaction has the same final entries as repeated propagation" repeatedEntries eventStreamEntries
  where
    keyedBatchEntries = do
      sheaf <- toySheaf
      deltas <- toyBenchmarkKeyedPatches sheaf 7
      section0 <- initialToySectionWith sheaf (ToyStalk 0)
      repeatedSection <- foldM (propagateKeyedDelta sheaf) section0 deltas
      batchedSection <- propagateToyKeyedDeltasWith sheaf section0 deltas
      eventStreamSection <- propagateToyEventStreamWith sheaf section0 deltas
      repeatedEntries <- toySectionEntriesWith sheaf repeatedSection
      batchedEntries <- toySectionEntriesWith sheaf batchedSection
      eventStreamEntries <- toySectionEntriesWith sheaf eventStreamSection
      pure (repeatedEntries, batchedEntries, eventStreamEntries)

    propagateKeyedDelta sheaf sectionValue delta =
      propagateToyKeyedSectionWith sheaf sectionValue delta

testEventStreamUnknownExtentAbortsAtomically :: IO ()
testEventStreamUnknownExtentAbortsAtomically = do
  sheaf <- expectRight toySheaf
  section0 <- expectRight (initialToySectionWith sheaf (ToyStalk 0))
  validDeltas <- expectRight (toyBenchmarkKeyedPatches sheaf 2)
  case validDeltas of
    firstDelta : finalDelta : _ -> do
      let invalidOrdinal = 99
          invalidDelta =
            KeyedSectionDelta
              { ksdModelFingerprint = sheafModelFingerprint (toySheafModel sheaf),
                ksdModelVersion = sheafModelVersion (toySheafModel sheaf),
                ksdExtent = dirtyScope (IntSet.singleton invalidOrdinal),
                ksdAssignments = IntMap.empty
              }
      case propagateToyEventStreamWith sheaf section0 [firstDelta, invalidDelta, finalDelta] of
        Left (ToyPatchSectionStoreFailed (SectionStoreUnknownObjectKey (ObjectKey rejectedOrdinal)))
          | rejectedOrdinal == invalidOrdinal ->
              assertEqual
                "failed stream leaves source section unchanged"
                (Right (Map.fromList [(ParentCell, ToyStalk 0), (ChildCell, ToyStalk 0)]))
                (toySectionEntriesWith sheaf section0)
        Left obstruction ->
          assertFailure ("expected unknown extent obstruction, received " <> show obstruction)
        Right propagatedSection ->
          assertFailure ("expected atomic abort, received " <> show propagatedSection)
    _ ->
      assertFailure ("expected two benchmark deltas, received " <> show validDeltas)

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
  model <- expectRight (mkSyntheticModel cellCount arrows)
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

mkSyntheticModel :: Int -> [SyntheticArrow] -> Either String (SheafModel SyntheticCell SyntheticArrow)
mkSyntheticModel cellCount arrows =
  either
    (Left . show)
    Right
    ( prepareSheafModel
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
    )

zeroSyntheticEntries :: Int -> Map.Map SyntheticCell SyntheticStalk
zeroSyntheticEntries cellCount =
  Map.fromList (fmap (\cell -> (cell, SyntheticStalk 0)) (fmap SyntheticCell [0 .. cellCount - 1]))

syntheticDelta :: SheafModel SyntheticCell SyntheticArrow -> ObjectKey -> KeyedSectionDelta SyntheticStalk
syntheticDelta model sourceKey =
  KeyedSectionDelta
    { ksdModelFingerprint = sheafModelFingerprint model,
      ksdModelVersion = sheafModelVersion model,
      ksdExtent = dirtyScope (IntSet.singleton (unObjectKey sourceKey)),
      ksdAssignments = IntMap.singleton (unObjectKey sourceKey) (SyntheticStalk 7)
    }

keyForSyntheticCell :: SheafModel SyntheticCell SyntheticArrow -> SyntheticCell -> Either String ObjectKey
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
