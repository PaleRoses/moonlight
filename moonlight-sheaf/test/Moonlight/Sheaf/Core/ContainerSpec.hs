{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Core.ContainerSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope (dirtyScope)
import Moonlight.Sheaf.Kernel.Basis
  ( basisCellIndex,
    basisCells,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
    denseIndexValueAt,
  )
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    certifySectionCompatibility,
    certifySectionExtentCompatibility,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind (..),
    RestrictionParts (..),
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniRestriction (..),
    MiniStalk (..),
    miniBasis,
    miniStalkAlgebra,
    withMiniModelAndSection,
    withMiniSheafModel,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "section-store"
    [ testCase "mkTotalSectionStore accepts exact object coverage" testExactCoverageAccepted,
      testCase "ObjectIndex round-trips cells through dense keys" testObjectIndexRoundTrip,
      testCase "mkSheafBasis preserves first occurrence order" testBasisPreservesFirstOccurrenceOrder,
      testCase "mkTotalSectionStore rejects missing cells" testMissingCoverageRejected,
      testCase "mkTotalSectionStore rejects extra cells outside the model" testExtraCellsRejected,
      testCase "batched keyed assignments fuse repeated section writes" testBatchedKeyedAssignments,
      testCase "dense descent propagates through core restriction rows" testDenseDescentPropagates,
      testCase "dense descent observes each keyed event inside one session" testDenseDescentObservesEachEvent,
      testCase "prepared dense descent exposes batch and edit interpreters" testPreparedDenseDescentInterpretersAgree,
      testCase "dense descent rejects dirty extent outside the prepared object universe" testDenseDescentRejectsInvalidDirtyExtent,
      testCase "descent transaction equals repeated per-call descents" testTransactionMatchesRepeatedDescents,
      testCase "descent transaction reads observe prior writes" testTransactionReadsObservePriorWrites,
      testCase "descent transaction aborts atomically on invalid delta" testTransactionAbortsOnInvalidDelta,
      testCase "dense descent reports pinned restriction conflicts" testDenseDescentPinnedConflict,
      testCase "converged descent satisfies every restriction row" testDescentPostconditionCertifies,
      testCase "descent rejection accumulates every failed restriction row" testDescentRejectionAccumulatesAllRows,
      testCase "updateStalkAtChecked rejects out-of-model cells" testInsertRejectsOutOfBasis
    ]

testExactCoverageAccepted :: Assertion
testExactCoverageAccepted =
  withMiniSheafModelAssertion $ \model ->
  case mkTotalSectionStore model (Map.fromList [(Cell0, MiniStalk 0.0), (Cell1, MiniStalk 0.0)]) of
    Left constructionError ->
      assertFailure ("expected exact model coverage to succeed, received " <> show constructionError)
    Right _ ->
      pure ()

testObjectIndexRoundTrip :: Assertion
testObjectIndexRoundTrip = do
  let objectIndex = mkObjectIndex [Cell0, Cell1, Cell0, Ghost]
  case traverse (`denseIndexKeyOf` objectIndex) [Cell0, Cell1, Ghost] of
    Nothing ->
      assertFailure "expected all fixture cells to be indexed"
    Just keys ->
      assertEqual
        "round-tripped cells"
        (Just [Cell0, Cell1, Ghost])
        (traverse (`denseIndexValueAt` objectIndex) keys)

testBasisPreservesFirstOccurrenceOrder :: Assertion
testBasisPreservesFirstOccurrenceOrder = do
  let basis = mkSheafBasis [Cell0, Cell1, Cell0, Ghost, Cell1]
  assertEqual "basis cells" [Cell0, Cell1, Ghost] (basisCells basis)
  assertEqual "Cell0 index" (Just 0) (basisCellIndex Cell0 basis)
  assertEqual "Cell1 index" (Just 1) (basisCellIndex Cell1 basis)
  assertEqual "Ghost index" (Just 2) (basisCellIndex Ghost basis)

testMissingCoverageRejected :: Assertion
testMissingCoverageRejected =
  withMiniSheafModelAssertion $ \model ->
  case mkTotalSectionStore model (Map.fromList [(Cell0, MiniStalk 1.0)]) of
    Left constructionError
      | sceMissingCells constructionError == mempty ->
          assertFailure ("expected missing cells, received " <> show constructionError)
      | otherwise ->
          pure ()
    Right _ ->
      assertFailure "expected mkTotalSectionStore to reject missing model cells"

testExtraCellsRejected :: Assertion
testExtraCellsRejected =
  withMiniSheafModelAssertion $ \model ->
  case
    mkTotalSectionStore
      model
      ( Map.fromList
          [ (Cell0, MiniStalk 0.0),
            (Cell1, MiniStalk 0.0),
            (Ghost, MiniStalk 1.0)
          ]
      ) of
    Left constructionError
      | sceExtraCells constructionError == mempty ->
          assertFailure ("expected extra cells, received " <> show constructionError)
      | otherwise ->
          pure ()
    Right _ ->
      assertFailure "expected mkTotalSectionStore to reject extra cells"

testBatchedKeyedAssignments :: Assertion
testBatchedKeyedAssignments =
  withMiniModelAndSectionAssertion $ \model section -> do
  repeated <-
    expectSectionStore
      ( assignLocalKeyed (miniKeyedDelta 1 (MiniStalk 2.0))
          =<< assignLocalKeyed (miniKeyedDelta 0 (MiniStalk 1.0)) section
      )
  batched <-
    expectSectionStore
      ( assignLocalKeyedBatch
          [miniKeyedDelta 0 (MiniStalk 1.0), miniKeyedDelta 1 (MiniStalk 2.0)]
          section
      )
  assertEqual
    "batched values match repeated writes"
    (Right (Map.fromList [(Cell0, MiniStalk 1.0), (Cell1, MiniStalk 2.0)]))
    (totalSectionEntries model batched)
  assertEqual
    "batched values match repeated section"
    (totalSectionEntries model repeated)
    (totalSectionEntries model batched)
  assertEqual "single fused epoch advance" (SectionEpoch 1) (totalSectionEpoch batched)
  assertEqual "fused extent is union of written keys" (dirtyScope (IntSet.fromList [0, 1])) (totalSectionExtent batched)

miniKeyedDelta :: Int -> MiniStalk -> KeyedSectionDelta owner MiniStalk
miniKeyedDelta objectOrdinal stalk =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

expectSectionStore :: Either (SectionStoreError MiniCell) value -> IO value
expectSectionStore =
  either (assertFailure . ("expected section store update, received " <>) . show) pure

testDenseDescentPropagates :: Assertion
testDenseDescentPropagates =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  descentResult <- expectEither (descendPreparedLocalKeyedBatch preparedDescent miniStalkAlgebra ObserveFinalSection [parentDelta model 7.0] section)
  assertEqual
    "core descent propagated Cell0 to Cell1"
    (Right (Map.fromList [(Cell0, MiniStalk 7.0), (Cell1, MiniStalk 7.0)]))
    (totalSectionEntries model (sdrSection descentResult))
  certifySectionCompatibility model miniStalkAlgebra (sdrSection descentResult)
    @?= Right SectionCertified

testDenseDescentObservesEachEvent :: Assertion
testDenseDescentObservesEachEvent =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  parentProgram <-
    expectSectionStore
      ( prepareSectionObjectProgram
          preparedDescent
          (ObjectKey 0)
          (Vector.fromList (fmap (MiniStalk . fromIntegral) [1 :: Int .. 7]))
      )
  descentResult <-
    expectEither
      ( descendPreparedLocalKeyedBatch
          preparedDescent
          miniStalkAlgebra
          ObserveEachStep
          (fmap (parentDelta model . fromIntegral) [1 :: Int .. 7])
          section
      )
  editDescentResult <-
    expectEither
      ( descendPreparedSectionProgram
          preparedDescent
          miniStalkAlgebra
          parentProgram
          section
      )
  assertEqual "all input events were observed" 7 (sdrObservedSteps descentResult)
  assertEqual "all dense edit events were observed" 7 (sdrObservedSteps editDescentResult)
  assertEqual
    "final observed session value"
    (Right (Map.fromList [(Cell0, MiniStalk 7.0), (Cell1, MiniStalk 7.0)]))
    (totalSectionEntries model (sdrSection descentResult))
  assertEqual
    "final dense edit session value"
    (totalSectionEntries model (sdrSection descentResult))
    (totalSectionEntries model (sdrSection editDescentResult))

testPreparedDenseDescentInterpretersAgree :: Assertion
testPreparedDenseDescentInterpretersAgree =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  preparedBatch <-
    expectEither
      ( descendPreparedLocalKeyedBatch
          preparedDescent
          miniStalkAlgebra
          ObserveEachStep
          (fmap (parentDelta model . fromIntegral) [1 :: Int .. 7])
          section
      )
  preparedObjectProgram <-
    expectSectionStore
      ( prepareSectionObjectProgram
          preparedDescent
          (ObjectKey 0)
          (Vector.fromList (fmap (MiniStalk . fromIntegral) [1 :: Int .. 7]))
      )
  preparedObjectProgramResult <-
    expectEither
      ( descendPreparedSectionProgram
          preparedDescent
          miniStalkAlgebra
          preparedObjectProgram
          section
      )
  preparedEditProgram <- expectSectionStore (prepareSectionProgram preparedDescent (fmap (parentEdit model . fromIntegral) [1 :: Int .. 7]))
  preparedProgramResult <-
    expectEither
      ( descendPreparedSectionProgram
          preparedDescent
          miniStalkAlgebra
          preparedEditProgram
          section
      )
  assertEqual "prepared program length" 7 (preparedSectionProgramLength preparedEditProgram)
  assertEqual
    "prepared batch equals prepared object program"
    (totalSectionEntries model (sdrSection preparedBatch), sdrObservedSteps preparedBatch)
    (totalSectionEntries model (sdrSection preparedObjectProgramResult), sdrObservedSteps preparedObjectProgramResult)
  assertEqual
    "prepared edit program equals prepared object program"
    (totalSectionEntries model (sdrSection preparedObjectProgramResult), sdrObservedSteps preparedObjectProgramResult)
    (totalSectionEntries model (sdrSection preparedProgramResult), sdrObservedSteps preparedProgramResult)

testDenseDescentPinnedConflict :: Assertion
testDenseDescentPinnedConflict =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  case descendPreparedLocalKeyedBatch preparedDescent miniStalkAlgebra ObserveFinalSection [conflictingDelta model] section of
    Left (SectionDescentPinnedConflict Cell1 (MiniStalk 3.0) (MiniStalk 7.0) [_]) ->
      pure ()
    Left descentError ->
      assertFailure ("expected pinned conflict, received " <> show descentError)
    Right descentResult ->
      assertFailure ("expected pinned conflict, received " <> show descentResult)

data DescentEdge
  = EdgeParentToChild
  | EdgeChildToLeaf
  | EdgeParentToLeaf
  deriving stock (Eq, Show)

descentEdgeEndpoints :: DescentEdge -> (MiniCell, MiniCell)
descentEdgeEndpoints edge =
  case edge of
    EdgeParentToChild ->
      (Cell0, Cell1)
    EdgeChildToLeaf ->
      (Cell1, Ghost)
    EdgeParentToLeaf ->
      (Cell0, Ghost)

withDescentStoreModel ::
  [DescentEdge] ->
  (forall owner. SheafModel owner MiniCell MiniRestriction -> result) ->
  Either String result
withDescentStoreModel edges useModel =
  case
    withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex [Cell0, Cell1, Ghost])
      ( \edge ->
          let (sourceCell, targetCell) = descentEdgeEndpoints edge
           in RestrictionParts
                { partKind = PortalRestriction,
                  partSource = sourceCell,
                  partTarget = targetCell,
                  partWitness = MiniRestriction id
                }
      )
      edges
      useModel
  of
    Left modelError -> Left (show modelError)
    Right result -> Right result

zeroDescentSection :: SheafModel owner MiniCell MiniRestriction -> Either (SectionConstructionError MiniCell) (TotalSectionStore owner MiniCell MiniStalk)
zeroDescentSection model =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (Cell0, MiniStalk 0.0),
          (Cell1, MiniStalk 0.0),
          (Ghost, MiniStalk 0.0)
        ]
    )

testDescentPostconditionCertifies :: Assertion
testDescentPostconditionCertifies =
  withDescentStoreModelAssertion [EdgeParentToChild, EdgeChildToLeaf, EdgeParentToLeaf] $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroDescentSection model)
  descentResult <- expectEither (descendPreparedLocalKeyedBatch preparedDescent miniStalkAlgebra ObserveFinalSection [parentDelta model 9.0] section)
  assertEqual
    "descent propagated through the restriction chain"
    (Right (Map.fromList [(Cell0, MiniStalk 9.0), (Cell1, MiniStalk 9.0), (Ghost, MiniStalk 9.0)]))
    (totalSectionEntries model (sdrSection descentResult))
  certifySectionCompatibility model miniStalkAlgebra (sdrSection descentResult)
    @?= Right SectionCertified
  certifySectionExtentCompatibility model miniStalkAlgebra (totalSectionExtent (sdrSection descentResult)) (sdrSection descentResult)
    @?= Right SectionCertified

testDescentRejectionAccumulatesAllRows :: Assertion
testDescentRejectionAccumulatesAllRows =
  withDescentStoreModelAssertion [EdgeParentToChild, EdgeChildToLeaf, EdgeParentToLeaf] $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroDescentSection model)
  childProgram <-
    expectSectionStore
      (prepareSectionObjectProgram preparedDescent (ObjectKey 1) (Vector.singleton (MiniStalk 5.0)))
  case descendPreparedSectionProgram preparedDescent miniStalkAlgebra childProgram section of
    Left (SectionDescentRejected rejections) ->
      Map.keysSet rejections @?= Set.fromList [Cell1, Ghost]
    Left descentError ->
      assertFailure ("expected accumulated descent rejection, received " <> show descentError)
    Right descentResult ->
      assertFailure ("expected accumulated descent rejection, received " <> show descentResult)

testDenseDescentRejectsInvalidDirtyExtent :: Assertion
testDenseDescentRejectsInvalidDirtyExtent =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  case descendPreparedLocalKeyedBatch preparedDescent miniStalkAlgebra ObserveFinalSection [invalidExtentDelta model] section of
    Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey 99))) ->
      pure ()
    Left descentError ->
      assertFailure ("expected invalid dirty extent, received " <> show descentError)
    Right descentResult ->
      assertFailure ("expected invalid dirty extent, received " <> show descentResult)

withCompatibleStoreModel ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> result) ->
  Either String result
withCompatibleStoreModel useModel =
  case
    withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells miniBasis))
      ( \() ->
          RestrictionParts
            { partKind = PortalRestriction,
              partSource = Cell0,
              partTarget = Cell1,
              partWitness = MiniRestriction id
            }
      )
      [()]
      useModel
  of
    Left modelError -> Left (show modelError)
    Right result -> Right result

zeroSectionFor :: SheafModel owner MiniCell MiniRestriction -> Either (SectionConstructionError MiniCell) (TotalSectionStore owner MiniCell MiniStalk)
zeroSectionFor model =
  mkTotalSectionStore model (Map.fromList [(Cell0, MiniStalk 0.0), (Cell1, MiniStalk 0.0)])

testTransactionMatchesRepeatedDescents :: Assertion
testTransactionMatchesRepeatedDescents =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  let deltas =
        fmap (parentDelta model . fromIntegral) [1 :: Int .. 7]
  perCallSection <-
    foldM
      ( \currentSection delta ->
          sdrSection
            <$> expectEither
              ( descendPreparedLocalKeyedBatch
                  preparedDescent
                  miniStalkAlgebra
                  ObserveEachStep
                  [delta]
                  currentSection
              )
      )
      section
      deltas
  ((), transactionResult) <-
    expectEither
      ( runSectionDescentTransaction preparedDescent miniStalkAlgebra section $ \transaction ->
          foldM
            ( \outcome delta ->
                case outcome of
                  Left descentError ->
                    pure (Left descentError)
                  Right () ->
                    transactKeyedSectionDelta transaction delta
            )
            (Right ())
            deltas
      )
  assertEqual
    "transaction entries equal repeated per-call entries"
    (totalSectionEntries model perCallSection)
    (totalSectionEntries model (sdrSection transactionResult))
  assertEqual
    "transaction extent equals repeated per-call extent"
    (totalSectionExtent perCallSection)
    (totalSectionExtent (sdrSection transactionResult))
  assertEqual "transaction observed every event" 7 (sdrObservedSteps transactionResult)

testTransactionReadsObservePriorWrites :: Assertion
testTransactionReadsObservePriorWrites =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  (observedChildren, _) <-
    expectEither
      ( runSectionDescentTransaction preparedDescent miniStalkAlgebra section $ \transaction -> do
          firstOutcome <- transactKeyedSectionDelta transaction (parentDelta model 4)
          case firstOutcome of
            Left descentError ->
              pure (Left descentError)
            Right () -> do
              earlyChild <- transactStalkAt transaction (ObjectKey 1)
              case earlyChild of
                Left descentError ->
                  pure (Left descentError)
                Right earlyValue -> do
                  secondOutcome <- transactKeyedSectionDelta transaction (parentDelta model 9)
                  case secondOutcome of
                    Left descentError ->
                      pure (Left descentError)
                    Right () -> do
                      lateChild <- transactStalkAt transaction (ObjectKey 1)
                      pure (fmap (\lateValue -> (earlyValue, lateValue)) lateChild)
      )
  observedChildren @?= (MiniStalk 4.0, MiniStalk 9.0)

testTransactionAbortsOnInvalidDelta :: Assertion
testTransactionAbortsOnInvalidDelta =
  withCompatibleStoreModelAssertion $ \model -> do
  preparedDescent <- expectEither (prepareSectionDescent model)
  section <- expectEither (zeroSectionFor model)
  case runSectionDescentTransaction preparedDescent miniStalkAlgebra section $ \transaction -> do
    firstOutcome <- transactKeyedSectionDelta transaction (parentDelta model 4)
    case firstOutcome of
      Left descentError ->
        pure (Left descentError)
      Right () ->
        transactKeyedSectionDelta transaction (invalidExtentDelta model) of
    Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey 99))) ->
      pure ()
    Left descentError ->
      assertFailure ("expected unknown ordinal abort, received " <> show descentError)
    Right ((), _) ->
      assertFailure "expected transaction abort, received a committed section"

parentDelta :: SheafModel owner MiniCell MiniRestriction -> Double -> KeyedSectionDelta owner MiniStalk
parentDelta _model value =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton 0),
      ksdAssignments = IntMap.singleton 0 (MiniStalk value)
    }

parentEdit :: SheafModel owner MiniCell MiniRestriction -> Double -> KeyedSectionEdit owner MiniStalk
parentEdit _model value =
  KeyedSectionEdit
    { kseObjectKey = ObjectKey 0,
      kseValue = MiniStalk value
    }

conflictingDelta :: SheafModel owner MiniCell MiniRestriction -> KeyedSectionDelta owner MiniStalk
conflictingDelta _model =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.fromList [0, 1]),
      ksdAssignments =
        IntMap.fromList
          [ (0, MiniStalk 7.0),
            (1, MiniStalk 3.0)
          ]
    }

invalidExtentDelta :: SheafModel owner MiniCell MiniRestriction -> KeyedSectionDelta owner MiniStalk
invalidExtentDelta _model =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton 99),
      ksdAssignments = IntMap.singleton 0 (MiniStalk 7.0)
    }

withMiniSheafModelAssertion ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> Assertion) ->
  Assertion
withMiniSheafModelAssertion useModel =
  either (assertFailure . ("expected mini model, received " <>)) id (withMiniSheafModel useModel)

withMiniModelAndSectionAssertion ::
  ( forall owner.
    SheafModel owner MiniCell MiniRestriction ->
    TotalSectionStore owner MiniCell MiniStalk ->
    Assertion
  ) ->
  Assertion
withMiniModelAndSectionAssertion useFixture =
  either (assertFailure . ("expected mini fixture, received " <>)) id (withMiniModelAndSection useFixture)

withCompatibleStoreModelAssertion ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> Assertion) ->
  Assertion
withCompatibleStoreModelAssertion useModel =
  either (assertFailure . ("expected compatible model, received " <>)) id (withCompatibleStoreModel useModel)

withDescentStoreModelAssertion ::
  [DescentEdge] ->
  (forall owner. SheafModel owner MiniCell MiniRestriction -> Assertion) ->
  Assertion
withDescentStoreModelAssertion edges useModel =
  either (assertFailure . ("expected descent model, received " <>)) id (withDescentStoreModel edges useModel)

expectEither :: Show errorValue => Either errorValue value -> IO value
expectEither =
  either (assertFailure . ("expected Right, received " <>) . show) pure

testInsertRejectsOutOfBasis :: Assertion
testInsertRejectsOutOfBasis =
  withMiniModelAndSectionAssertion $ \model section ->
  case updateStalkAtChecked model Ghost (const (MiniStalk 5.0)) section of
    Left (SectionUpdateOutOfBasis Ghost) ->
      pure ()
    Left failure ->
      assertFailure ("expected out-of-model failure, received " <> show failure)
    Right _ ->
      assertFailure "expected updateStalkAtChecked to reject an out-of-model cell"
