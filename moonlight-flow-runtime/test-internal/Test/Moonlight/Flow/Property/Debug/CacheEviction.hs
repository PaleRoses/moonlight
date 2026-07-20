{-# LANGUAGE GADTs #-}

module Test.Moonlight.Flow.Property.Debug.CacheEviction
  ( tests,
  )
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Carrier.View.Cache
  ( CachedVisibleEntry (..),
    VisibleContextKey (..),
    VisibleSectionCache (..),
  )
import Moonlight.Flow.Patch qualified as Patch
import Moonlight.Flow.Query qualified as Query
import Moonlight.Core qualified as Rel
import Moonlight.Differential.Proposition qualified as Prop
import Moonlight.Differential.Row.Tuple qualified as Tuple
import Moonlight.Flow.Runtime.Apply qualified as RuntimeApply
import Moonlight.Flow.Runtime.Create qualified as RuntimeCreate
import Moonlight.Flow.Runtime.Visible qualified as RuntimeVisible
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeVisibleCache,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Carrier.Store qualified as CarrierStore
import Moonlight.Flow.Runtime.Carrier.Store.Read
  ( visibleContextUncached,
  )
import Moonlight.Flow.Runtime.Spec.Schema qualified as Spec
import Moonlight.Flow.Runtime.Spec.Schema qualified as Schema
import Moonlight.Flow.Runtime.Types qualified as RuntimeTypes
import Test.Moonlight.Flow.Runtime.Diagnostics.Observation
  ( observeRuntimeWithEvidenceView,
  )
import Test.Moonlight.Flow.Trace.EngineClosureFixture
  ( ClosureFixture (..),
    closureContexts,
    closureFixture,
  )
import Test.Moonlight.Flow.Instrument
  ( clearRuntimeFactorCacheState,
    clearRuntimeVisibleCache,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "runtime cache eviction/equality"
    [ testCase "visible cache eviction does not change visible rows" $ withFixture visibleCacheEvictionNoEffect,
      testCase "pinned visible context is maintained before any read fallback" pinnedVisibleContextMaintainedBeforeRead,
      testCase "factor cache eviction preserves rich visible state" $ withFixture factorCacheEvictionPreservesVisibleState
    ]

visibleCacheEvictionNoEffect :: ClosureFixture -> Assertion
visibleCacheEvictionNoEffect fixture = do
  let warmedRuntime =
        warmVisibleCache
          closureContexts
          (cfRichRuntime fixture)
      clearedRuntime =
        clearRuntimeVisibleCache warmedRuntime
      beforeSections =
        fmap (`visibleContextUncached` warmedRuntime) closureContexts
      afterSections =
        fmap (`visibleContextUncached` clearedRuntime) closureContexts
  assertEqual
    "visible cache eviction must not change context-visible rows"
    beforeSections
    afterSections

pinnedVisibleContextMaintainedBeforeRead :: Assertion
pinnedVisibleContextMaintainedBeforeRead = do
  let edge :: Spec.RuntimeAtom String String
      edge =
        Spec.runtimeAtom (Rel.mkAtomId 0) [Rel.mkSlotId 0, Rel.mkSlotId 1]
      label :: Spec.RuntimeAtom String String
      label =
        Spec.runtimeAtom (Rel.mkAtomId 1) [Rel.mkSlotId 1, Rel.mkSlotId 2]
      prop :: Prop.PropositionKey String
      prop =
        Prop.PropositionKey "reachable"
      ctx :: String
      ctx =
        "main"

  queryValue <-
    shouldRight
      ( Query.query
          [ matchAtom edge,
            matchAtom label
          ]
          (Query.select [Rel.mkSlotId 0, Rel.mkSlotId 2])
      )
  planValue <-
    shouldRight (Spec.runtimePlanQuery ctx prop queryValue)
  runtime0 <-
    shouldRight
      ( RuntimeCreate.createRuntime
          ( Spec.runtimeSpec
              (Spec.runtimeSchema [(ctx, Spec.runtimeContextSchema [edge, label] [prop])])
              [planValue]
          )
      )
  initialPatch <-
    shouldRight
      ( Patch.patch
          <$> sequence
            [ Patch.insert edge (rows [[1, 10]]),
              Patch.insert label (rows [[10, 7]])
            ]
      )
  runtime1 <-
    shouldRight (RuntimeApply.applyPatch initialPatch runtime0)

  let runtimePinnedBeforePatch =
        RuntimeVisible.pinVisibleContext ctx runtime1

  case runtimePinnedBeforePatch of
    RuntimeTypes.Runtime kernelBeforePatch -> do
      _pinnedSectionBeforePatch <-
        requirePinnedSection
          ctx
          (runtimeVisibleCache (rdrState kernelBeforePatch))

      updatePatch <-
        shouldRight
          ( Patch.patch
              <$> sequence
                [ Patch.insert edge (rows [[2, 20]]),
                  Patch.insert label (rows [[20, 8]])
                ]
          )
      runtimePinnedAfterPatch <-
        shouldRight (RuntimeApply.applyPatch updatePatch runtimePinnedBeforePatch)

      case runtimePinnedAfterPatch of
        RuntimeTypes.Runtime kernelAfterPatch -> do
          let cacheAfterPatch =
                runtimeVisibleCache (rdrState kernelAfterPatch)
              expectedAfterPatch =
                visibleContextUncached ctx kernelAfterPatch

          assertBool
            "ordinary non-null patch should still advance quotient epoch"
            (Core.rsQuotientEpoch (rdrState kernelBeforePatch) /= Core.rsQuotientEpoch (rdrState kernelAfterPatch))
          assertBool
            "ordinary non-null patch should still advance live epoch"
            (Core.rsLiveEpoch (rdrState kernelBeforePatch) /= Core.rsLiveEpoch (rdrState kernelAfterPatch))
          assertNoLazyContext ctx cacheAfterPatch

          pinnedSectionAfterPatch <-
            requirePinnedSection ctx cacheAfterPatch
          assertEqual
            "pinned section must already be maintained when applyPatch returns"
            expectedAfterPatch
            pinnedSectionAfterPatch

          (runtimeAfterRead, readSection) <-
            shouldRight (RuntimeVisible.visibleContext ctx runtimePinnedAfterPatch)
          case runtimeAfterRead of
            RuntimeTypes.Runtime kernelAfterRead -> do
              assertNoLazyContext ctx (runtimeVisibleCache (rdrState kernelAfterRead))
              assertEqual
                "visibleContext must read the maintained pinned section, not populate lazy epoch cache"
                pinnedSectionAfterPatch
                (RuntimeTypes.unRuntimeSection readSection)

factorCacheEvictionPreservesVisibleState :: ClosureFixture -> Assertion
factorCacheEvictionPreservesVisibleState fixture = do
  assertEqual
    "factor operator-state eviction must not change carrier-store-observable semantics"
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) (cfRichRuntime fixture))
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) (clearRuntimeFactorCacheState (cfRichRuntime fixture)))

warmVisibleCache ::
  Ord ctx =>
  Ord prop =>
  [ctx] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
warmVisibleCache contexts runtime0 =
  List.foldl'
    ( \runtime contextValue ->
        either (const runtime) fst (CarrierStore.visibleContext contextValue runtime)
    )
    runtime0
    contexts

withFixture :: (ClosureFixture -> Assertion) -> Assertion
withFixture assertion =
  case closureFixture of
    Left fixtureError ->
      assertFailure (show fixtureError)
    Right fixture ->
      assertion fixture

rows :: [[Int]] -> [Tuple.RowTupleKey]
rows =
  fmap Tuple.tupleKeyFromInts
{-# INLINE rows #-}

matchAtom :: Spec.RuntimeAtom ctx prop -> Query.Match
matchAtom atomValue =
  Query.match
    ( Query.atomRef
        (Schema.runtimeAtomId atomValue)
        (Schema.rasColumns (Schema.runtimeAtomSchemaDefinition atomValue))
    )
{-# INLINE matchAtom #-}

assertNoLazyContext ::
  (Eq ctx, Show ctx) =>
  ctx ->
  VisibleSectionCache ctx section ->
  Assertion
assertNoLazyContext contextValue cache =
  assertBool
    ("lazy epoch cache should not carry context " <> show contextValue)
    (null matchingKeys)
  where
    matchingKeys =
      filter
        (\keyValue -> vckContext keyValue == contextValue)
        (Map.keys (vscEntries cache))

requirePinnedSection ::
  (Ord ctx, Show ctx) =>
  ctx ->
  VisibleSectionCache ctx section ->
  IO section
requirePinnedSection contextValue cache =
  case Map.lookup contextValue (vscPinned cache) of
    Nothing ->
      assertFailure ("missing pinned visible context " <> show contextValue)
    Just entry ->
      pure (cveSection entry)

shouldRight ::
  Show errorValue =>
  Either errorValue value ->
  IO value
shouldRight =
  either
    (assertFailure . show)
    pure
