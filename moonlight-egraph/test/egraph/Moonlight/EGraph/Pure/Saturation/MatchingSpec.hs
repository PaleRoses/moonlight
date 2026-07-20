{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Pure.Saturation.MatchingSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as Core
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    cegSite,
    contextMerge,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( contextAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context
  ( cegContextRevision,
  )
import Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter (..),
  )
import Moonlight.EGraph.Pure.Relational
  ( emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateAnnotatedDirty,
    wcojPreparedRegionalDeltaMatchCompiledWithRootFilter,
  )
import Moonlight.EGraph.Pure.Rebuild (merge)
import Moonlight.EGraph.Pure.Rebuild (EGraphRebuildDelta (..))
import Moonlight.EGraph.Pure.Rebuild (rebuildWithDelta)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingAdvanceCtx (..),
    MatchingDeltaPayload (..),
    matchingDeltaFromRebuild,
    matchingDeltaFromRebuildWithObstruction,
    matchingDeltaFromTouchedKeys,
    matchingDeltaObstructionInvalidation,
    matchingFrontierFromDelta,
    wcojMatchingAlgebra,
  )
import Moonlight.EGraph.Pure.Types (ClassId (..), classIdKey)
import Moonlight.EGraph.Test.Ring.Core
  ( NodeCount,
    RingF (..),
    ringAdd,
    ringOne,
    ringVar,
    ringZero,
  )
import Moonlight.EGraph.Test.Saturation.Helpers
  ( addXYPattern,
    buildGraph,
    compileRingPatternQuery,
    mkRequest,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.System (emptyGuardCapabilityResolver)
import Moonlight.Rewrite.System (emptyFactDerivationIndex)
import Moonlight.Rewrite.System qualified as LogicStore
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Substrate
  ( trivialLattice,
  )
import Moonlight.Sheaf.Context.Site
  ( UnitContextSiteOwner,
    preparedRegionTable,
    unitPreparedContextSite,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateSummary (..),
    requestAggregateSupportRoots,
    rootResolvedExact,
    rootsSupportedByKeys,
  )
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( LivePruningAdapter (..),
    LivePruningState (..),
    ObstructionInvalidation,
    livePruningMatchingAlgebra,
    obstructionInvalidationFromKeys,
    obstructionInvalidationWithResultsFromKeys,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( PreparedRequestCacheKey,
    mkPreparedRequestCacheKey,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "MatchingTypes"
    [ testCase "regional prepared cache distinguishes root filters" regionalPreparedRootFilterLaw
    , testCase "regional prepared cache repairs provenance after a region-changed row deletion" regionalPreparedRegionChangeDeletionLaw
    , testCase "matchingFrontierFromDelta maps stable/full/incremental correctly" $
        let dirtyKeys =
              IntSet.fromList [1, 2, 3]
         in do
              matchingFrontierFromDelta Delta.cleanDelta
                @?= Delta.cleanScope

              matchingFrontierFromDelta Delta.fullDelta
                @?= Delta.fullScope

              matchingFrontierFromDelta (Delta.scopedDelta (Delta.dirtyScope IntSet.empty) Nothing)
                @?= Delta.cleanScope

              matchingFrontierFromDelta (Delta.scopedDelta (Delta.dirtyScope dirtyKeys) Nothing)
                @?= Delta.dirtyScope dirtyKeys
    , testCase "matchingDeltaFromTouchedKeys dirties add-only rewrite frontiers" $
        let touchedKeys =
              IntSet.fromList [4, 8, 15]
            touchedDelta =
              matchingDeltaFromTouchedKeys touchedKeys
         in do
              Delta.scopeKeys (Delta.scopedDeltaSupport touchedDelta)
                @?= Just touchedKeys

              matchingDeltaObstructionInvalidation touchedDelta
                @?= Just
                  (obstructionInvalidationWithResultsFromKeys ClassId touchedKeys IntSet.empty touchedKeys touchedKeys)
    , testCase "Delta.restrictScope composes by intersection" $
        let a = IntSet.fromList [1, 2, 3]
            b = IntSet.fromList [2, 3, 4]
            nested =
              Delta.restrictScope a $
                Delta.restrictScope b Delta.fullScope
         in do
              nested
                @?= Delta.dirtyScope (IntSet.fromList [2, 3])

              Delta.scopeKeys nested
                @?= Just (IntSet.fromList [2, 3])

              Delta.restrictScope IntSet.empty Delta.fullScope
                @?= Delta.cleanScope
    , testCase "Delta.scopeKeys respects restricted full frontier" $
        let frontier =
              Delta.restrictScope
                (IntSet.fromList [10, 20, 30])
                Delta.fullScope
         in Delta.scopeKeys frontier
              @?= Just (IntSet.fromList [10, 20, 30])
    , testCase "incremental delta semigroup unions impacted classes and obstruction invalidation" $
        let leftPayload =
              obstructionInvalidationFromKeys ClassId (IntSet.fromList [1, 2]) (IntSet.fromList [7]) (IntSet.fromList [10])
            rightPayload =
              obstructionInvalidationFromKeys ClassId (IntSet.fromList [2, 3]) (IntSet.fromList [8]) (IntSet.fromList [10, 11])
            leftDelta =
              Delta.scopedDelta
                (Delta.dirtyScope (IntSet.fromList [100, 101]))
                (Just (MatchingDeltaPayload (Just leftPayload) IntMap.empty))
            rightDelta =
              Delta.scopedDelta
                (Delta.dirtyScope (IntSet.fromList [101, 102]))
                (Just (MatchingDeltaPayload (Just rightPayload) IntMap.empty))
            combined = leftDelta <> rightDelta
         in do
              Delta.scopeKeys (Delta.scopedDeltaSupport combined)
                @?= Just (IntSet.fromList [100, 101, 102])

              matchingDeltaObstructionInvalidation combined
                @?= Just
                  (obstructionInvalidationFromKeys ClassId (IntSet.fromList [1, 2, 3]) (IntSet.fromList [7, 8]) (IntSet.fromList [10, 11]))
    , testCase "matchingDeltaFromRebuildWithObstruction preserves sheaf payload even when class impact is empty" $
        let payload =
              obstructionInvalidationFromKeys ClassId (IntSet.singleton 1) (IntSet.singleton 2) (IntSet.singleton 3)
            emptyRebuild =
              EGraphRebuildDelta
                { erdDirtyResultKeys = IntSet.empty,
                  erdImpactedClassKeys = IntSet.empty,
                  erdTopologyClassKeys = IntSet.empty
                }
            dirtyRebuild =
              EGraphRebuildDelta
                { erdDirtyResultKeys = IntSet.empty,
                  erdImpactedClassKeys = IntSet.singleton 9,
                  erdTopologyClassKeys = IntSet.empty
                }
         in do
              matchingDeltaFromRebuildWithObstruction emptyRebuild (Just payload)
                @?= Delta.payloadDelta (MatchingDeltaPayload (Just payload) IntMap.empty)

              matchingDeltaFromRebuildWithObstruction emptyRebuild Nothing
                @?= Delta.cleanDelta

              matchingDeltaFromRebuildWithObstruction dirtyRebuild (Just payload)
                @?= Delta.scopedDelta
                  (Delta.dirtyScope (IntSet.singleton 9))
                  (Just (MatchingDeltaPayload (Just payload) IntMap.empty))
    , testCase "aggregate dirty-key lookup descends through the support index" $
        let rootSupport =
              Map.fromList
                [ (1, IntSet.fromList [10, 20]),
                  (2, IntSet.fromList [20, 30]),
                  (3, IntSet.singleton 40)
                ]
            aggregateSummary :: RequestAggregateSummary Int () ()
            aggregateSummary =
              RequestAggregateSummary
                { rasRootResolutions =
                    Map.fromSet
                      (const (rootResolvedExact ()))
                      (Map.keysSet rootSupport),
                  rasRootSupport =
                    rootSupport,
                  rasSupportRoots =
                    requestAggregateSupportRoots rootSupport
                }
         in do
              rootsSupportedByKeys (IntSet.singleton 20) aggregateSummary
                @?= Set.fromList [1, 2]

              rootsSupportedByKeys (IntSet.fromList [40, 50]) aggregateSummary
                @?= Set.singleton 3
    , testCase "live pruning exact replay uses the current prepared scope, not stale union" $
        let requestFor rootKeys =
              ToyRequest
                { trKey = 7,
                  trScope = Delta.dirtyScope (IntSet.fromList rootKeys),
                  trRoots = Set.fromList [1, 2],
                  trRetain = True
                }
            matchingAlgebra =
              toyLivePruningAlgebra
            (state1, preparedQueries1) =
              GenericMatching.maPrepareQueries
                matchingAlgebra
                (GenericMatching.maInitialState matchingAlgebra)
                Delta.cleanDelta
                ()
                [requestFor [1]]
            (stateAfterFirstRun, firstResult) =
              GenericMatching.maRunQueries matchingAlgebra state1 () preparedQueries1
            (state2, preparedQueries2) =
              GenericMatching.maPrepareQueries
                matchingAlgebra
                stateAfterFirstRun
                Delta.cleanDelta
                ()
                [requestFor [2]]
            (_stateAfterSecondRun, secondResult) =
              GenericMatching.maRunQueries matchingAlgebra state2 () preparedQueries2
         in do
              firstResult @?= Right [[1]]
              secondResult @?= Right [[2]]
    , testCase "live pruning keeps only reusable current request states" $
        let reusableRequest =
              ToyRequest
                { trKey = 1,
                  trScope = Delta.fullScope,
                  trRoots = Set.singleton 1,
                  trRetain = True
                }
            ephemeralRequest =
              ToyRequest
                { trKey = 2,
                  trScope = Delta.fullScope,
                  trRoots = Set.singleton 2,
                  trRetain = False
                }
            staleRequest =
              ToyRequest
                { trKey = 3,
                  trScope = Delta.fullScope,
                  trRoots = Set.singleton 3,
                  trRetain = True
                }
            matchingAlgebra =
              toyLivePruningAlgebra
            initialState =
              GenericMatching.maInitialState matchingAlgebra
            (preparedState, _preparedQueries) =
              GenericMatching.maPrepareQueries
                matchingAlgebra
                initialState
                Delta.cleanDelta
                ()
                [reusableRequest, ephemeralRequest, staleRequest]
            advancedState =
              GenericMatching.maAdvanceState
                matchingAlgebra
                Delta.cleanDelta
                ToyAdvance
                preparedState
            (currentCoverState, _currentPreparedQueries) =
              GenericMatching.maPrepareQueries
                matchingAlgebra
                advancedState
                Delta.cleanDelta
                ()
                [reusableRequest]
         in do
              Map.keysSet (lpsRequests preparedState)
                @?= Set.fromList (toyRequestKey <$> [reusableRequest, ephemeralRequest, staleRequest])

              Map.keysSet (lpsRequests advancedState)
                @?= Set.fromList (toyRequestKey <$> [reusableRequest, staleRequest])

              Map.keysSet (lpsRequests currentCoverState)
                @?= Set.singleton (toyRequestKey reusableRequest)
    , testCase "wcoj matching algebra carries prepared cache through rebuild repair" $
        let ringNum value = Fix (Num value)
         in case
              buildGraph
                [ ringNum 1
                , ringNum 2
                , ringAdd (ringNum 1) (ringNum 2)
                ]
            of
              Right (graph0, [c1, c2, _]) -> do
                compiledQuery <-
                  case compileRingPatternQuery addXYPattern of
                    Left unboundVars ->
                      assertFailure ("failed to compile ring query: " <> show unboundVars)
                    Right query ->
                      pure query
                let (rebuildDelta, rebuiltGraph) =
                      rebuildWithDelta (merge c1 c2 graph0)
                    matchingAlgebra =
                      wcojMatchingAlgebra emptyGuardCapabilityResolver
                    matchingWorld graph =
                      GenericMatching.MatchWorld
                        { GenericMatching.mwGraph = graph,
                          GenericMatching.mwFacts = LogicStore.emptyFactStore,
                          GenericMatching.mwFactDerivations = emptyFactDerivationIndex,
                          GenericMatching.mwCapabilities = emptyGuardCapabilityResolver,
                          GenericMatching.mwProofContext = Nothing,
                          GenericMatching.mwIteration = 0
                        }
                    request0 =
                      mkRequest matchingAlgebra GenericMatching.BaseSite Nothing compiledQuery graph0
                    (preparedState0, preparedQueries0) =
                      GenericMatching.maPrepareQueries
                        matchingAlgebra
                        (GenericMatching.maInitialState matchingAlgebra)
                        Delta.fullDelta
                        (matchingWorld graph0)
                        [request0]
                    (stateAfterRun0, firstResult) =
                      GenericMatching.maRunQueries matchingAlgebra preparedState0 (matchingWorld graph0) preparedQueries0
                    advancedState =
                      GenericMatching.maAdvanceState
                        matchingAlgebra
                        (matchingDeltaFromRebuild rebuildDelta)
                        MatchingAdvanceCtx
                          { macGraph = rebuiltGraph,
                            macCanonicalize = id,
                            macContextSite = Nothing,
                            macContextRevision = Nothing
                          }
                        stateAfterRun0
                    request1 =
                      mkRequest matchingAlgebra GenericMatching.BaseSite Nothing compiledQuery rebuiltGraph
                    (preparedState1, preparedQueries1) =
                      GenericMatching.maPrepareQueries
                        matchingAlgebra
                        advancedState
                        Delta.fullDelta
                        (matchingWorld rebuiltGraph)
                        [request1]
                    (_stateAfterRun1, secondResult) =
                      GenericMatching.maRunQueries matchingAlgebra preparedState1 (matchingWorld rebuiltGraph) preparedQueries1
                    assertNonEmptyMatches label result =
                      case result of
                        Right (matches : _) ->
                          assertBool (label <> " should produce matches") (not (null matches))
                        Right [] ->
                          assertFailure (label <> " produced no match batches")
                        Left obstruction ->
                          assertFailure (label <> " failed: " <> show obstruction)
                 in do
                      assertNonEmptyMatches "initial prepared wcoj query" firstResult
                      assertNonEmptyMatches "repaired prepared wcoj query" secondResult
              Right _ ->
                assertFailure "expected buildGraph to return three classes"
              Left allocationError ->
                assertFailure ("buildGraph allocation failed: " <> show allocationError)
    ]

regionalPreparedRootFilterLaw :: Assertion
regionalPreparedRootFilterLaw =
  case
    buildGraph
      [ ringAdd (ringVar "x") ringZero,
        ringAdd ringOne ringZero
      ]
  of
    Right (baseGraph, firstRoot : _) -> do
      compiledQuery <-
        either
          (assertFailure . ("failed to compile root-filter query: " <>) . show)
          pure
          (compileRingPatternQuery addXYPattern)
      let contextGraph :: ContextEGraph UnitContextSiteOwner RingF NodeCount ()
          contextGraph =
            emptyContextEGraphFromSite unitPreparedContextSite baseGraph
          runWith rootFilter preparedState =
            wcojPreparedRegionalDeltaMatchCompiledWithRootFilter
              rootFilter
              (preparedRegionTable (cegSite contextGraph))
              (contextAnnotatedDeltaBuckets contextGraph)
              (cegContextRevision contextGraph)
              compiledQuery
              baseGraph
              preparedState
          selectedRootFilter =
            RestrictedRootClasses (IntSet.singleton (classIdKey firstRoot))
      (allRootState, allRootMatches) <-
        expectRegionalRight
          "all-root regional WCOJ failed"
          (runWith AllRootClasses emptyEGraphPreparedMatchState)
      (_reusedState, reusedMatches) <-
        expectRegionalRight
          "reused regional WCOJ root filter failed"
          (runWith selectedRootFilter allRootState)
      (_freshState, freshMatches) <-
        expectRegionalRight
          "fresh regional WCOJ root filter failed"
          (runWith selectedRootFilter emptyEGraphPreparedMatchState)
      assertBool "all-root fixture should expose both roots" (length allRootMatches >= 2)
      Set.fromList reusedMatches @?= Set.fromList freshMatches
      Set.fromList (fmap (classIdKey . regionalMatchRoot) reusedMatches)
        @?= Set.singleton (classIdKey firstRoot)
    Right _ ->
      assertFailure "regional root-filter fixture did not build a root class"
    Left allocationError ->
      assertFailure ("regional root-filter fixture allocation failed: " <> show allocationError)

regionalPreparedRegionChangeDeletionLaw :: Assertion
regionalPreparedRegionChangeDeletionLaw =
  case
    buildGraph
      [ ringVar "x",
        ringOne,
        ringZero,
        ringAdd (ringVar "x") ringZero,
        ringAdd ringOne ringZero,
        ringAdd (ringAdd (ringVar "x") ringOne) ringZero,
        ringAdd (ringAdd ringOne ringZero) ringZero
      ]
  of
    Right (baseGraph, variableClass : oneClass : zeroClass : _) -> do
      compiledQuery <-
        either
          (assertFailure . ("failed to compile region-change query: " <>) . show)
          pure
          (compileRingPatternQuery nestedAddPattern)
      firstContextGraph <-
        expectRegionalRight
          "failed to install initial regional quotient"
          (contextMerge () variableClass oneClass (emptyContextEGraphFromSite unitPreparedContextSite baseGraph))
      secondContextGraph <-
        expectRegionalRight
          "failed to advance regional quotient"
          (contextMerge () oneClass zeroClass firstContextGraph)
      let dirtyKeys =
            IntSet.fromList
              [ classIdKey variableClass,
                classIdKey oneClass,
                classIdKey zeroClass
              ]
          runWith contextGraph rootFilter preparedState =
            wcojPreparedRegionalDeltaMatchCompiledWithRootFilter
              rootFilter
              (preparedRegionTable (cegSite contextGraph))
              (contextAnnotatedDeltaBuckets contextGraph)
              (cegContextRevision contextGraph)
              compiledQuery
              baseGraph
              preparedState
      (initialState, initialMatches) <-
        expectRegionalRight
          "initial region-change regional WCOJ failed"
          (runWith firstContextGraph AllRootClasses emptyEGraphPreparedMatchState)
      (_directState, directMatches) <-
        expectRegionalRight
          "direct region-change regional WCOJ repair failed"
          (runWith secondContextGraph AllRootClasses initialState)
      (repairedState, _deltaMatches) <-
        expectRegionalRight
          "region-change regional WCOJ delta repair failed"
          ( runWith
              secondContextGraph
              (RestrictedRootClasses dirtyKeys)
              (markEGraphPreparedMatchStateAnnotatedDirty dirtyKeys initialState)
          )
      (_reusedState, reusedMatches) <-
        expectRegionalRight
          "reused region-change regional WCOJ failed"
          (runWith secondContextGraph AllRootClasses repairedState)
      (_freshState, freshMatches) <-
        expectRegionalRight
          "fresh region-change regional WCOJ failed"
          (runWith secondContextGraph AllRootClasses emptyEGraphPreparedMatchState)
      assertBool
        "the regional quotient change must alter the query result"
        (Set.fromList initialMatches /= Set.fromList freshMatches)
      Set.fromList directMatches @?= Set.fromList freshMatches
      Set.fromList reusedMatches @?= Set.fromList freshMatches
    Right _ ->
      assertFailure "region-change regional fixture did not build its child classes"
    Left allocationError ->
      assertFailure ("region-change regional fixture allocation failed: " <> show allocationError)

nestedAddPattern :: Pattern RingF
nestedAddPattern =
  PatternNode
    ( Add
        ( PatternNode
            ( Add
                (PatternVar (Core.mkPatternVar 0))
                (PatternVar (Core.mkPatternVar 1))
            )
        )
        (PatternVar (Core.mkPatternVar 2))
    )

regionalMatchRoot :: (ClassId, substitution, region) -> ClassId
regionalMatchRoot (rootClass, _, _) =
  rootClass

expectRegionalRight :: Show obstruction => String -> Either obstruction value -> IO value
expectRegionalRight label =
  either
    (assertFailure . ((label <> ": ") <>) . show)
    pure

data ToyRequest runtime = ToyRequest
  { trKey :: !Int,
    trScope :: !(Delta.Scope IntSet.IntSet),
    trRoots :: !(Set.Set Int),
    trRetain :: !Bool
  }

data ToyAdvance runtime = ToyAdvance

type ToyPreparedRequestKey = PreparedRequestCacheKey ()

type ToyLivePruningAlgebra =
  GenericMatching.MatchingAlgebra
    ()
    (LivePruningState () () Int () () ())
    IntSet.IntSet
    (ObstructionInvalidation Int)
    ()
    ToyRequest
    ToyAdvance
    ()
    Int

type ToyInnerMatchingAlgebra =
  GenericMatching.MatchingAlgebra
    ()
    ()
    IntSet.IntSet
    (ObstructionInvalidation Int)
    ()
    ToyRequest
    ToyAdvance
    ()
    Int

toyRequestKey :: ToyRequest runtime -> ToyPreparedRequestKey
toyRequestKey request =
  mkPreparedRequestCacheKey (trKey request) () Nothing

toyAggregateSummary ::
  Set.Set Int ->
  RequestAggregateSummary Int () ()
toyAggregateSummary roots =
  let rootSupport =
        Map.fromSet IntSet.singleton roots
   in RequestAggregateSummary
        { rasRootResolutions =
            Map.fromSet (const (rootResolvedExact ())) roots,
          rasRootSupport =
            rootSupport,
          rasSupportRoots =
            requestAggregateSupportRoots rootSupport
        }

toyLivePruningAdapter ::
  LivePruningAdapter () ToyRequest ToyAdvance Int () () Int () ()
toyLivePruningAdapter =
  LivePruningAdapter
    { lpaRequestKey =
        toyRequestKey,
      lpaRequestRoots =
        \() -> trRoots,
      lpaRetainRequestState =
        trRetain,
      lpaRootKey =
        id,
      lpaCanonicalizeRoot =
        \ToyAdvance -> id,
      lpaRefreshRequest =
        \_matchingDelta () request _affectedRoots _priorState ->
          Right (toyAggregateSummary (trRoots request)),
      lpaExactMatches =
        \() _request rootValue _rootResolution -> [rootValue]
    }

toyInnerMatchingAlgebra :: ToyInnerMatchingAlgebra
toyInnerMatchingAlgebra =
  GenericMatching.MatchingAlgebra
    { GenericMatching.maInitialState = (),
      GenericMatching.maEnvironment = (),
      GenericMatching.maPrepareQueries =
        \state _matchingDelta _world requests ->
          ( state,
            ( \request ->
                GenericMatching.MatchingQuery
                  (trScope request)
                  request
            )
              <$> requests
          ),
      GenericMatching.maRunQueries =
        \state _world preparedQueries ->
          ( state,
            Right ((const [] <$> preparedQueries))
          ),
      GenericMatching.maPreviewQuery = \_state _world _preparedQuery -> Nothing,
      GenericMatching.maAdvanceState = \_matchingDelta _advance state -> state,
      GenericMatching.maReplayDiagnostics = const Nothing
    }

toyLivePruningAlgebra :: ToyLivePruningAlgebra
toyLivePruningAlgebra =
  livePruningMatchingAlgebra Just toyLivePruningAdapter toyInnerMatchingAlgebra
