{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( ZipMatch (..),
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Algebra
  ( JoinSemilattice (..),
  )
import Moonlight.Core
  ( ConstructorTag,
    HasConstructorTag (..),
    Pattern (..),
    zipSameNodeShape,
  )
import Moonlight.Core
  ( mkQuotientEpoch,
  )
import Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec,
    semilatticeAnalysis,
  )
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( addTerm,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
    merge,
    rebuildWithDelta,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedBase,
    EGraphPreparedMatchState,
    QueryPlan,
    atomizeCompiledPatternQuery,
    buildPreparedBase,
    emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateDirty,
    patchPreparedBaseWith,
    preparedBaseRowBlocks,
    preparedPlanCacheSize,
    quotientPatchFromRowDeltas,
    wcojPreparedDeltaMatchCompiledWithRoots,
    wcojPreparedMatchCompiledWithRootFilter,
    wcojPreparedMatchCompiledWithRoots,
    wcojMatchCompiledWithRootFilter,
    wcojMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Relational.Direct
  ( DirectPatternShape (..),
    classifyCompiledPatternQuery,
    directPatternMatches,
  )
import Moonlight.EGraph.Pure.Relational.Source
  ( dirtyEGraphRowsByAtomFromStructuralStore,
  )
import Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter (AllRootClasses, RestrictedRootClasses),
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphRevision,
    eGraphRevisionValue,
    eGraphStore,
    emptyEGraph,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Execution.Direct qualified as RelRuntime
import Moonlight.Flow.Storage.Plan qualified as FlowStoragePlan
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForQuery,
  )
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..),
    atomPatchRows
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
    plainRowPatchFromList
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Storage.Relation
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RewriteCondition (..),
    combineCompiledGuards,
    compileGuard,
    guardEquivalent,
    guardTrue,
  )
import Moonlight.Core
  ( Substitution,
    emptySubstitution,
    insertSubst
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    conjunctivePatternQuery,
    compilePatternQuery,
    guardedPatternQuery,
    singlePatternQuery,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain tests

preparedBaseRows ::
  Int ->
  EGraphPreparedBase SurfaceKind RingF ->
  Either RowBuildError (IntMap (RowBlock 'Canonical))
preparedBaseRows =
  preparedBaseRowBlocks

tests :: TestTree
tests =
  testGroup
    "moonlight-egraph:relational"
    [ testCase "variable-only compiled query enumerates roots through relational execution" variableOnlyRelationalQueryAssertion,
      testCase "restricted compiled query matches global filtered relational results" restrictedRelationalQueryAssertion,
      testCase "restricted compiled query canonicalizes stale merged roots" restrictedRelationalQueryCanonicalizesMergedRootAssertion,
      testCase "database-backed prepared matcher enforces repeated child equality" repeatedChildPreparedMatcherAssertion,
      testCase "database-backed prepared matcher applies root restriction" restrictedPreparedMatcherAssertion,
      testCase "real rebuild repair emits quotient patch matching rebuilt atom rows" quotientPatchAssertion,
      testCase "prepared cached matcher agrees with cold matcher across rebuild repair" preparedCachedMatcherRepairAssertion,
      testCase "prepared cached matcher follows database snapshot after rebuild repair" preparedCachedMatcherDatabaseSnapshotAssertion,
      testCase "prepared delta matcher is driven by the database snapshot state" preparedDeltaMatcherAssertion,
      testCase "direct tree matcher agrees with relational matching on nested tree patterns" directTreeMatcherAssertion,
      testCase "direct tree matcher preserves guarded query result structure" directTreeMatcherGuardedAssertion,
      testCase "direct hierarchical delta surfaces a clean root above a dirty inner child" directHierarchicalDeltaAssertion,
      testCase "aot store override composes with monolithic store builds" aotStoreOverrideCompositionAssertion,
      testCase "aot specialized full matcher agrees exactly with the interpreted matcher" aotSpecializedFullMatchAssertion,
      testCase "aot specialized delta matcher agrees exactly with the interpreted delta oracle" aotSpecializedDeltaMatchAssertion,
      testCase "aot incremental factor cache delta agrees exactly with the interpreted delta oracle" aotIncrementalFactorCacheDeltaAssertion,
      testCase "guard-inclusive plan keys separate shared pattern lists" guardInclusivePlanCacheKeyAssertion,
      QC.testProperty "shared prepared cache agrees with fresh structural matching across generated rebuild cycles" $
        QC.withNumTests 30 preparedSharedCacheMatchesFreshLaw,
      QC.testProperty "direct delta matching agrees with relational delta matching across generated rebuild cycles" $
        QC.withNumTests 30 directDeltaAgreesWithRelationalDeltaLaw
    ]

variableOnlyRelationalQueryAssertion :: Assertion
variableOnlyRelationalQueryAssertion = do
  (graph, classIds) <- expectRight (buildGraph egraphTerms)
  compiledQuery <- expectRight (compileRingPatternQuery (PatternVar (EGraph.mkPatternVar 0)))
  relationalMatches <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph)
  Set.fromList relationalMatches
    @?= Set.fromList
      ( fmap
          (\classId -> (classId, insertSubst (EGraph.mkPatternVar 0) classId emptySubstitution))
          classIds
      )

restrictedRelationalQueryAssertion :: Assertion
restrictedRelationalQueryAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms) of
    Right (graph, _oneClass : _twoClass : _threeClass : addRoot : _otherRoots) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      globalMatches <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph)
      restrictedMatches <-
        expectRight
          ( wcojMatchCompiledWithRootFilter
              (RestrictedRootClasses (IntSet.singleton (classIdKey addRoot)))
              compiledQuery
              graph
          )
      Set.fromList restrictedMatches
        @?= Set.filter
          (\(root, _) -> classIdKey root == classIdKey addRoot)
          (Set.fromList globalMatches)
    Right _ ->
      assertFailure "expected graph with the add-root and disconnected component"
    Left allocationError -> fixtureAllocationFailure allocationError

restrictedRelationalQueryCanonicalizesMergedRootAssertion :: Assertion
restrictedRelationalQueryCanonicalizesMergedRootAssertion =
  case buildGraph egraphTerms of
    Right (graph0, oneRoot : twoRoot : _threeRoot : _add13Root : add23Root : _remainingRoots) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      let (_rebuildDelta, _repairIndex, graph1) =
            rebuildWithDelta (merge oneRoot twoRoot graph0)
          canonicalAddRoot =
            canonicalizeClassId graph1 add23Root
      staleRootMatches <-
        expectRight
          ( wcojMatchCompiledWithRootFilter
              (RestrictedRootClasses (IntSet.singleton (classIdKey add23Root)))
              compiledQuery
              graph1
          )
      canonicalRootMatches <-
        expectRight
          ( wcojMatchCompiledWithRootFilter
              (RestrictedRootClasses (IntSet.singleton (classIdKey canonicalAddRoot)))
              compiledQuery
              graph1
          )
      Set.fromList staleRootMatches @?= Set.fromList canonicalRootMatches
      assertBool "stale merged root should still select its canonical class" (not (null staleRootMatches))
    Right _ ->
      assertFailure "expected graph with merged add roots"
    Left allocationError -> fixtureAllocationFailure allocationError

repeatedChildPreparedMatcherAssertion :: Assertion
repeatedChildPreparedMatcherAssertion =
  case buildGraph repeatedChildTerms of
    Right (graph, _fourRoot : _fiveRoot : repeatedAddRoot : _mixedAddRoot : _remainingRoots) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXXPattern)
      (_preparedState, preparedMatches) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRoots
              compiledQuery
              graph
              emptyEGraphPreparedMatchState
          )
      fmap fst (Set.toAscList (Set.fromList preparedMatches)) @?= [repeatedAddRoot]
    Right _ ->
      assertFailure "expected repeated-child fixture roots"
    Left allocationError -> fixtureAllocationFailure allocationError

restrictedPreparedMatcherAssertion :: Assertion
restrictedPreparedMatcherAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms) of
    Right (graph, _oneClass : _twoClass : _threeClass : addRoot : _otherRoots) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      (_preparedState, preparedMatches) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRootFilter
              (RestrictedRootClasses (IntSet.singleton (classIdKey addRoot)))
              compiledQuery
              graph
              emptyEGraphPreparedMatchState
          )
      assertBool "restricted database-backed prepared matcher should emit matches" (not (null preparedMatches))
      Set.map fst (Set.fromList preparedMatches) @?= Set.singleton addRoot
    Right _ ->
      assertFailure "expected graph with the add-root and disconnected component"
    Left allocationError -> fixtureAllocationFailure allocationError

quotientPatchAssertion :: Assertion
quotientPatchAssertion =
  case buildGraph egraphTerms of
    Right (graph0, c1 : c2 : _remainingClasses) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      queryPlan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
      let initialPreparedBase = buildPreparedBase queryPlan graph0
      let (rebuildDelta, _repairIndex, rebuiltGraph) = rebuildWithDelta (merge c1 c2 graph0)
          (_patchedPreparedBase, atomInputDeltas) =
            patchPreparedBaseWith rebuiltGraph (erdDirtyResultKeys rebuildDelta) initialPreparedBase
      let repairPatch =
            quotientPatchFromRowDeltas
              (mkQuotientEpoch 1)
              (mkQuotientEpoch 2)
              (erdDirtyResultKeys rebuildDelta)
              (erdTopologyClassKeys rebuildDelta)
              atomInputDeltas
      let rebuiltPreparedBase = buildPreparedBase queryPlan rebuiltGraph
      initialRows <-
        expectRight $
          fmap (fmap rowBlockToRowDelta) (preparedBaseRows 0 initialPreparedBase)
      expectedRows <-
        expectRight $
          fmap (fmap rowBlockToRowDelta) (preparedBaseRows 0 rebuiltPreparedBase)
      assertBool "expected real rebuild to emit atom deltas" (not (IntMap.null (qpEvents repairPatch)))
      foldPatchRows initialRows repairPatch @?= expectedRows
    Right _ ->
      assertFailure "expected at least two egraph classes"
    Left allocationError -> fixtureAllocationFailure allocationError

preparedCachedMatcherRepairAssertion :: Assertion
preparedCachedMatcherRepairAssertion =
  case buildGraph egraphTerms of
    Right (graph0, c1 : c2 : _remainingClasses) -> do
      compiledQuery <- expectRight (compileRingConjunctivePatternQuery (addXYPattern :| [PatternVar (EGraph.mkPatternVar 9)]))
      coldMatches0 <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph0)
      (preparedState0, preparedMatches0) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRoots
              compiledQuery
              graph0
              emptyEGraphPreparedMatchState
          )
      Set.fromList preparedMatches0 @?= Set.fromList coldMatches0

      let (rebuildDelta, _repairIndex, rebuiltGraph) =
            rebuildWithDelta (merge c1 c2 graph0)
          repairedPreparedState =
            markEGraphPreparedMatchStateDirty
              (erdDirtyResultKeys rebuildDelta)
              preparedState0
      coldMatches1 <- expectRight (wcojMatchCompiledWithRoots compiledQuery rebuiltGraph)
      (_preparedState1, preparedMatches1) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRoots
              compiledQuery
              rebuiltGraph
              repairedPreparedState
          )
      Set.fromList preparedMatches1 @?= Set.fromList coldMatches1
    Right _ ->
      assertFailure "expected at least two egraph classes"
    Left allocationError -> fixtureAllocationFailure allocationError

preparedCachedMatcherDatabaseSnapshotAssertion :: Assertion
preparedCachedMatcherDatabaseSnapshotAssertion =
  case buildGraph egraphTerms of
    Right (graph0, c1 : c2 : _remainingClasses) -> do
      compiledQuery <- expectRight (compileRingConjunctivePatternQuery (addXYPattern :| [PatternVar (EGraph.mkPatternVar 9)]))
      (preparedState0, preparedMatches0) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRoots
              compiledQuery
              graph0
              emptyEGraphPreparedMatchState
          )
      coldMatches0 <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph0)
      Set.fromList preparedMatches0 @?= Set.fromList coldMatches0

      let (rebuildDelta, _repairIndex, rebuiltGraph) =
            rebuildWithDelta (merge c1 c2 graph0)
          dirtyPreparedState =
            markEGraphPreparedMatchStateDirty
              (erdDirtyResultKeys rebuildDelta)
              preparedState0
      coldMatches1 <- expectRight (wcojMatchCompiledWithRoots compiledQuery rebuiltGraph)
      (_preparedState1, preparedMatches1) <-
        expectRight
          ( wcojPreparedMatchCompiledWithRoots
              compiledQuery
              rebuiltGraph
              dirtyPreparedState
          )
      Set.fromList preparedMatches1 @?= Set.fromList coldMatches1
    Right _ ->
      assertFailure "expected at least two egraph classes"
    Left allocationError -> fixtureAllocationFailure allocationError

preparedDeltaMatcherAssertion :: Assertion
preparedDeltaMatcherAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms) of
    Right (graph, _oneClass : _twoClass : _threeClass : addRoot : _otherRoots) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      coldMatches <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph)
      (preparedState0, deltaMatches0) <-
        expectRight
          ( wcojPreparedDeltaMatchCompiledWithRoots
              compiledQuery
              graph
              emptyEGraphPreparedMatchState
          )
      Set.fromList deltaMatches0 @?= Set.fromList coldMatches

      (preparedState1, unchangedMatches) <-
        expectRight
          ( wcojPreparedDeltaMatchCompiledWithRoots
              compiledQuery
              graph
              preparedState0
          )
      unchangedMatches @?= []

      let dirtyState =
            markEGraphPreparedMatchStateDirty
              (IntSet.singleton (classIdKey addRoot))
              preparedState1
      (_preparedState2, dirtyDeltaMatches) <-
        expectRight
          ( wcojPreparedDeltaMatchCompiledWithRoots
              compiledQuery
              graph
              dirtyState
          )
      Set.fromList dirtyDeltaMatches
        @?= Set.filter
          (\(root, _) -> root == addRoot)
          (Set.fromList coldMatches)
    Right _ ->
      assertFailure "expected graph with the add-root and disconnected component"
    Left allocationError -> fixtureAllocationFailure allocationError

directTreeMatcherAssertion :: Assertion
directTreeMatcherAssertion =
  case buildGraph (egraphTerms <> nestedTerms) of
    Right (graph, _classIds) -> do
      compiledQuery <- expectRight (compileRingPatternQuery nestedAddPattern)
      relationalMatches <- relationalOracleMatches nestedAddPattern graph
      let directMatches =
            directPatternMatches
              AllRootClasses
              graph
              (classifyCompiledPatternQuery compiledQuery)
      Set.fromList directMatches @?= Set.fromList relationalMatches
      assertBool "nested direct matcher should emit the nested add root" (not (null directMatches))
    Left allocationError ->
      fixtureAllocationFailure allocationError

directTreeMatcherGuardedAssertion :: Assertion
directTreeMatcherGuardedAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms <> nestedTerms) of
    Right (graph, _classIds) -> do
      assertGuardedResidualUnevaluated graph addXYPattern (DirectSingleAtomTree addXYPattern)
      assertGuardedResidualUnevaluated graph nestedAddPattern (DirectHierarchicalTree nestedAddPattern)
    Left allocationError ->
      fixtureAllocationFailure allocationError

assertGuardedResidualUnevaluated ::
  EGraph RingF NodeCount ->
  Pattern RingF ->
  DirectPatternShape RingF ->
  Assertion
assertGuardedResidualUnevaluated graph patternValue expectedShape = do
  relationalMatches <- relationalOracleMatches patternValue graph
  assertBool "guarded residual fixture must produce matches" (not (null relationalMatches))
  Foldable.for_
    [ RewriteCondition guardTrue,
      RewriteCondition (guardEquivalent (EGraph.mkPatternVar 0) (EGraph.mkPatternVar 1))
    ]
    ( \condition -> do
        compiledQuery <- expectRight (compileRingGuardedPatternQueryWith patternValue condition)
        classifyCompiledPatternQuery compiledQuery @?= expectedShape
        (_dispatchedState, dispatchedMatches) <-
          expectRight
            ( wcojPreparedMatchCompiledWithRootFilter
                AllRootClasses
                compiledQuery
                graph
                emptyEGraphPreparedMatchState
            )
        Set.fromList dispatchedMatches @?= Set.fromList relationalMatches
        unpreparedMatches <-
          expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph)
        Set.fromList unpreparedMatches @?= Set.fromList relationalMatches
    )

relationalOracleMatches ::
  Pattern RingF ->
  EGraph RingF NodeCount ->
  IO [(ClassId, Substitution)]
relationalOracleMatches patternValue graph = do
  oracleQuery <- expectRight (compileRingPatternQuery patternValue)
  either assertFailureReturning pure (relationalJoinOracleMatches oracleQuery graph)

assertFailureReturning :: String -> IO [(ClassId, Substitution)]
assertFailureReturning obstruction = do
  assertFailure obstruction
  pure []

relationalJoinOracleMatches ::
  RingCompiledQuery ->
  EGraph RingF NodeCount ->
  Either String [(ClassId, Substitution)]
relationalJoinOracleMatches compiledQuery graph = do
  queryPlan <- first show (atomizeCompiledPatternQuery compiledQuery)
  sections <-
    first
      show
      (preparedBaseRowBlocks (eGraphRevisionValue (eGraphRevision graph)) (buildPreparedBase queryPlan graph))
  first
    show
    ( RelRuntime.evalPlanOutputsWithRootSelection
        queryPlan
        RelRuntime.RuntimeAllRoots
        sections
    )

relationalJoinDeltaOracleMatches ::
  RingCompiledQuery ->
  IntSet.IntSet ->
  EGraph RingF NodeCount ->
  Either String [(ClassId, Substitution)]
relationalJoinDeltaOracleMatches compiledQuery dirtyKeys graph = do
  queryPlan <- first show (atomizeCompiledPatternQuery compiledQuery)
  dirtySections <-
    traverse
      (relationalJoinDirtySection graph queryPlan)
      ( IntMap.toAscList
          ( dirtyEGraphRowsByAtomFromStructuralStore
              queryPlan
              (canonicalizeClassId graph)
              (eGraphStore graph)
              dirtyKeys
          )
      )
  case mapMaybe id dirtySections of
    [] ->
      Right []
    selectedDirtySections -> do
      fullSections <-
        first
          show
          (preparedBaseRowBlocks (eGraphRevisionValue (eGraphRevision graph)) (buildPreparedBase queryPlan graph))
      let dirtyAtomKeys =
            IntSet.fromList (fmap fst selectedDirtySections)
          baseSections =
            IntMap.restrictKeys fullSections (relationalJoinFullSectionAtomKeys queryPlan dirtyAtomKeys)
      fmap (Set.toAscList . Set.fromList . foldMap id) $
        traverse
          (relationalJoinMatchesWithDirtySection queryPlan baseSections)
          selectedDirtySections

relationalJoinDirtySection ::
  EGraph RingF NodeCount ->
  QueryPlan SurfaceKind RingF ->
  (Int, IntMap [RowTupleKey]) ->
  Either String (Maybe (Int, RowBlock 'Canonical))
relationalJoinDirtySection graph queryPlan (atomKey, rowsByResult) =
  if IntMap.null rowsByResult
    then Right Nothing
    else
      case relationalJoinQueryAtomSpec atomKey queryPlan of
        Nothing ->
          Right Nothing
        Just atomSpec ->
          fmap (Just . (,) atomKey) $
            first
              show
              ( atomRowsFromTupleKeys
                  (relationalJoinRowIdentity graph queryPlan atomSpec)
                  (RelPlan.asColumns atomSpec)
                  (foldMap id (IntMap.elems rowsByResult))
              )

relationalJoinMatchesWithDirtySection ::
  QueryPlan SurfaceKind RingF ->
  IntMap (RowBlock 'Canonical) ->
  (Int, RowBlock 'Canonical) ->
  Either String [(ClassId, Substitution)]
relationalJoinMatchesWithDirtySection queryPlan baseSections (atomKey, dirtyRows) =
  first
    show
    ( RelRuntime.evalPlanOutputsWithRootSelection
        queryPlan
        RelRuntime.RuntimeAllRoots
        (IntMap.insert atomKey dirtyRows baseSections)
    )

relationalJoinFullSectionAtomKeys ::
  QueryPlan SurfaceKind RingF ->
  IntSet.IntSet ->
  IntSet.IntSet
relationalJoinFullSectionAtomKeys queryPlan dirtyAtomKeys =
  if IntSet.size dirtyAtomKeys <= 1
    then IntSet.difference queryAtomKeys dirtyAtomKeys
    else queryAtomKeys
  where
    queryAtomKeys =
      IntSet.fromList
        (fmap (RelPlan.queryAtomKey . RelPlan.asQueryAtomId) (Vector.toList (RelPlan.qpAtoms queryPlan)))

relationalJoinQueryAtomSpec ::
  Int ->
  QueryPlan SurfaceKind RingF ->
  Maybe (RelPlan.AtomSpec (RingF ()) (ENode RingF) ClassId)
relationalJoinQueryAtomSpec atomKey queryPlan =
  case filter ((== atomKey) . RelPlan.queryAtomKey . RelPlan.asQueryAtomId) (Vector.toList (RelPlan.qpAtoms queryPlan)) of
    atomSpec : _ ->
      Just atomSpec
    [] ->
      Nothing

relationalJoinRowIdentity ::
  EGraph RingF NodeCount ->
  QueryPlan SurfaceKind RingF ->
  RelPlan.AtomSpec (RingF ()) (ENode RingF) ClassId ->
  RowBlockIdentity
relationalJoinRowIdentity graph queryPlan atomSpec =
  rowBlockIdentityForQuery
    (eGraphRevisionValue (eGraphRevision graph))
    (RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec))
    (RelPlan.qpFingerprint queryPlan)
    (RelPlan.qpId queryPlan)
    0

directHierarchicalDeltaAssertion :: Assertion
directHierarchicalDeltaAssertion =
  case buildGraph (egraphTerms <> nestedTerms <> [ringNum 7]) of
    Right (graph0, _oneClass : _twoClass : threeClass : _add13Root : _add23Root : nestedRoot : sevenClass : _remainingRoots) -> do
      plainQuery <- expectRight (compileRingPatternQuery nestedAddPattern)
      guardedQuery <-
        expectRight
          ( compileRingGuardedPatternQueryWith
              nestedAddPattern
              (RewriteCondition (guardEquivalent (EGraph.mkPatternVar 0) (EGraph.mkPatternVar 2)))
          )
      classifyCompiledPatternQuery plainQuery @?= DirectHierarchicalTree nestedAddPattern
      classifyCompiledPatternQuery guardedQuery @?= DirectHierarchicalTree nestedAddPattern
      oracleBootstrap <- either assertFailureReturning pure (relationalJoinOracleMatches plainQuery graph0)
      (plainState0, plainBootstrap) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots plainQuery graph0 emptyEGraphPreparedMatchState)
      (guardedState0, guardedBootstrap) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots guardedQuery graph0 emptyEGraphPreparedMatchState)
      Set.fromList plainBootstrap @?= Set.fromList oracleBootstrap
      Set.fromList guardedBootstrap @?= Set.fromList plainBootstrap
      (plainState1, plainQuiescent) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots plainQuery graph0 plainState0)
      (guardedState1, guardedQuiescent) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots guardedQuery graph0 guardedState0)
      plainQuiescent @?= []
      guardedQuiescent @?= []
      let (rebuildDelta, _repairIndex, graph1) =
            rebuildWithDelta (merge threeClass sevenClass graph0)
          dirtyKeys =
            erdDirtyResultKeys rebuildDelta
          plainDirtyState =
            markEGraphPreparedMatchStateDirty dirtyKeys plainState1
          guardedDirtyState =
            markEGraphPreparedMatchStateDirty dirtyKeys guardedState1
      oracleDelta <- either assertFailureReturning pure (relationalJoinDeltaOracleMatches plainQuery dirtyKeys graph1)
      (_plainState2, plainDelta) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots plainQuery graph1 plainDirtyState)
      (_guardedState2, guardedDelta) <-
        expectRight (wcojPreparedDeltaMatchCompiledWithRoots guardedQuery graph1 guardedDirtyState)
      Set.fromList plainDelta @?= Set.fromList oracleDelta
      Set.fromList guardedDelta @?= Set.fromList plainDelta
      assertBool
        "dirty inner add must surface the clean nested root through the parent walk"
        (Set.member (canonicalizeClassId graph1 nestedRoot) (Set.map fst (Set.fromList plainDelta)))
    Right _ ->
      assertFailure "expected graph with nested root and disjoint leaf"
    Left allocationError -> fixtureAllocationFailure allocationError

aotStoreOverrideCompositionAssertion :: Assertion
aotStoreOverrideCompositionAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms) of
    Right (graph, _oneClass : _twoClass : _threeClass : addRoot : _otherRoots) -> do
      compiledQuery <- expectRight (compileRingConjunctivePatternQuery (addXYPattern :| [addYZPattern]))
      queryPlan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
      compiledStoragePlan <-
        expectRight
          (FlowStoragePlan.compileStoragePlan (FlowStoragePlan.storagePlanFromQueryPlan queryPlan))
      sections <-
        expectRight
          (preparedBaseRows (eGraphRevisionValue (eGraphRevision graph)) (buildPreparedBase queryPlan graph))
      baseStore <- expectRight (RelRuntime.evalPlanPreparedStore compiledStoragePlan sections)
      Foldable.for_ (IntMap.toAscList sections) $ \(atomKey, block) ->
        RelRuntime.evalPlanStoreWithSectionOverride compiledStoragePlan atomKey block baseStore
          @?= Right baseStore
      let dirtyRowsByAtom =
            dirtyEGraphRowsByAtomFromStructuralStore
              queryPlan
              (canonicalizeClassId graph)
              (eGraphStore graph)
              (IntSet.singleton (classIdKey addRoot))
      dirtySections <-
        expectRight
          (traverse (relationalJoinDirtySection graph queryPlan) (IntMap.toAscList dirtyRowsByAtom))
      let selectedDirtySections =
            mapMaybe id dirtySections
      assertBool
        "dirty override fixture must select at least one dirty atom"
        (not (null selectedDirtySections))
      Foldable.for_ selectedDirtySections $ \(atomKey, dirtyBlock) -> do
        overridden <-
          expectRight
            (RelRuntime.evalPlanStoreWithSectionOverride compiledStoragePlan atomKey dirtyBlock baseStore)
        monolithic <-
          expectRight
            (RelRuntime.evalPlanPreparedStore compiledStoragePlan (IntMap.insert atomKey dirtyBlock sections))
        overridden @?= monolithic
        specialized <-
          expectRight
            ( RelRuntime.evalPlanOutputsFromPreparedStore
                queryPlan
                (RelRuntime.evalPlanPreparedDecomp queryPlan)
                RelRuntime.RuntimeAllRoots
                overridden
            )
        interpreted <-
          expectRight
            ( RelRuntime.evalPlanOutputsWithRootSelection
                queryPlan
                RelRuntime.RuntimeAllRoots
                (IntMap.insert atomKey dirtyBlock sections)
            )
        specialized @?= interpreted
    Right _ ->
      assertFailure "expected graph with the add-root and disconnected component"
    Left allocationError -> fixtureAllocationFailure allocationError

aotSpecializedFullMatchAssertion :: Assertion
aotSpecializedFullMatchAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms <> nestedTerms) of
    Right (graph, _oneClass : _twoClass : _threeClass : addRoot : _otherRoots) ->
      Foldable.for_ aotFixtureQueries $ \eitherQuery -> do
        compiledQuery <- expectRight eitherQuery
        interpreted <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph)
        (_preparedState, specialized) <-
          expectRight
            (wcojPreparedMatchCompiledWithRoots compiledQuery graph emptyEGraphPreparedMatchState)
        specialized @?= interpreted
        let rootFilter =
              RestrictedRootClasses (IntSet.singleton (classIdKey addRoot))
        interpretedRestricted <-
          expectRight (wcojMatchCompiledWithRootFilter rootFilter compiledQuery graph)
        (_restrictedState, specializedRestricted) <-
          expectRight
            ( wcojPreparedMatchCompiledWithRootFilter
                rootFilter
                compiledQuery
                graph
                emptyEGraphPreparedMatchState
            )
        specializedRestricted @?= interpretedRestricted
    Right _ ->
      assertFailure "expected graph with the add-root and disconnected component"
    Left allocationError -> fixtureAllocationFailure allocationError

aotSpecializedDeltaMatchAssertion :: Assertion
aotSpecializedDeltaMatchAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms) of
    Right (graph0, c1 : c2 : _remainingClasses) ->
      Foldable.for_ aotFixtureQueries $ \eitherQuery -> do
        compiledQuery <- expectRight eitherQuery
        interpretedBootstrap <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph0)
        (preparedState0, bootstrap) <-
          expectRight
            (wcojPreparedDeltaMatchCompiledWithRoots compiledQuery graph0 emptyEGraphPreparedMatchState)
        bootstrap @?= interpretedBootstrap
        let (rebuildDelta, _repairIndex, graph1) =
              rebuildWithDelta (merge c1 c2 graph0)
            dirtyKeys =
              erdDirtyResultKeys rebuildDelta
            dirtyState =
              markEGraphPreparedMatchStateDirty dirtyKeys preparedState0
        (_preparedState1, deltaMatches) <-
          expectRight
            (wcojPreparedDeltaMatchCompiledWithRoots compiledQuery graph1 dirtyState)
        oracleDelta <-
          either assertFailureReturning pure (relationalJoinDeltaOracleMatches compiledQuery dirtyKeys graph1)
        deltaMatches @?= oracleDelta
    Right _ ->
      assertFailure "expected at least two egraph classes"
    Left allocationError -> fixtureAllocationFailure allocationError

aotIncrementalFactorCacheDeltaAssertion :: Assertion
aotIncrementalFactorCacheDeltaAssertion =
  case buildGraph (egraphTerms <> disconnectedTerms <> nestedTerms) of
    Right (graph0, c1 : c2 : c3 : _remainingClasses) ->
      Foldable.for_ aotFixtureQueries $ \eitherQuery -> do
        compiledQuery <- expectRight eitherQuery
        (preparedState0, bootstrap) <-
          expectRight
            (wcojPreparedDeltaMatchCompiledWithRoots compiledQuery graph0 emptyEGraphPreparedMatchState)
        interpretedBootstrap <- expectRight (wcojMatchCompiledWithRoots compiledQuery graph0)
        bootstrap @?= interpretedBootstrap
        let (rebuildDelta1, _repairIndex1, graph1) =
              rebuildWithDelta (merge c1 c2 graph0)
            dirtyState1 =
              markEGraphPreparedMatchStateDirty (erdDirtyResultKeys rebuildDelta1) preparedState0
        (preparedState1, delta1) <-
          expectRight
            (wcojPreparedDeltaMatchCompiledWithRoots compiledQuery graph1 dirtyState1)
        oracleDelta1 <-
          either assertFailureReturning pure (relationalJoinDeltaOracleMatches compiledQuery (erdDirtyResultKeys rebuildDelta1) graph1)
        delta1 @?= oracleDelta1
        let (rebuildDelta2, _repairIndex2, graph2) =
              rebuildWithDelta (merge c2 c3 graph1)
            dirtyState2 =
              markEGraphPreparedMatchStateDirty (erdDirtyResultKeys rebuildDelta2) preparedState1
        (_preparedState2, delta2) <-
          expectRight
            (wcojPreparedDeltaMatchCompiledWithRoots compiledQuery graph2 dirtyState2)
        oracleDelta2 <-
          either assertFailureReturning pure (relationalJoinDeltaOracleMatches compiledQuery (erdDirtyResultKeys rebuildDelta2) graph2)
        delta2 @?= oracleDelta2
    Right _ ->
      assertFailure "expected at least three egraph classes"
    Left allocationError -> fixtureAllocationFailure allocationError

guardInclusivePlanCacheKeyAssertion :: Assertion
guardInclusivePlanCacheKeyAssertion = do
  (graph, _classes) <- expectRight (buildGraph (egraphTerms <> disconnectedTerms))
  plainQuery <- expectRight (compileRingPatternQuery addXYPattern)
  guardedQuery <-
    expectRight
      ( compileRingGuardedPatternQueryWith
          addXYPattern
          (RewriteCondition (guardEquivalent (EGraph.mkPatternVar 0) (EGraph.mkPatternVar 1)))
      )
  (plainState, _plainMatches) <-
    expectRight (wcojPreparedMatchCompiledWithRoots plainQuery graph emptyEGraphPreparedMatchState)
  preparedPlanCacheSize plainState @?= 1
  (guardedState, guardedMatches) <-
    expectRight (wcojPreparedMatchCompiledWithRoots guardedQuery graph plainState)
  preparedPlanCacheSize guardedState @?= 2
  interpretedGuarded <- expectRight (wcojMatchCompiledWithRoots guardedQuery graph)
  guardedMatches @?= interpretedGuarded

aotFixtureQueries :: [Either [EGraph.PatternVar] RingCompiledQuery]
aotFixtureQueries =
  [ compileRingConjunctivePatternQuery (addXYPattern :| [PatternVar (EGraph.mkPatternVar 9)]),
    compileRingConjunctivePatternQuery (addXYPattern :| [addYZPattern]),
    compileRingConjunctivePatternQuery (nestedAddPattern :| [PatternVar (EGraph.mkPatternVar 9)])
  ]

type DeltaLawQueryPair :: Type
data DeltaLawQueryPair = DeltaLawQueryPair
  { dlqpPlainQuery :: !RingCompiledQuery,
    dlqpPlainState :: !(EGraphPreparedMatchState SurfaceKind RingF)
  }

type DeltaLawState :: Type
data DeltaLawState = DeltaLawState
  { dlsGraph :: !(EGraph RingF NodeCount),
    dlsPairs :: ![DeltaLawQueryPair]
  }

directDeltaAgreesWithRelationalDeltaLaw :: GeneratedRingScenario -> QC.Property
directDeltaAgreesWithRelationalDeltaLaw (GeneratedRingScenario terms mergePairs) =
  case (buildGraph terms, deltaLawQueryPairs) of
    (Left allocationError, _) ->
      QC.counterexample ("generated graph allocation failed: " <> show allocationError) False
    (_, Left compileErrors) ->
      QC.counterexample (show compileErrors) False
    (Right (initialGraph, initialClassIds), Right freshPairs) ->
      case runDeltaLawCycles initialGraph initialClassIds freshPairs of
        Left obstruction ->
          QC.counterexample obstruction False
        Right _ ->
          QC.property True
  where
    runDeltaLawCycles initialGraph initialClassIds freshPairs = do
      bootstrappedPairs <-
        traverse (runDeltaLawQueryPair Nothing initialGraph) freshPairs
      Foldable.foldlM
        (runDeltaLawEditCycle initialClassIds)
        DeltaLawState
          { dlsGraph = initialGraph,
            dlsPairs = bootstrappedPairs
          }
        mergePairs

deltaLawQueryPairs :: Either [EGraph.PatternVar] [DeltaLawQueryPair]
deltaLawQueryPairs =
  traverse mkPair [addXYPattern, nestedAddPattern]
  where
    mkPair patternValue =
      (\plainQuery ->
          DeltaLawQueryPair
            { dlqpPlainQuery = plainQuery,
              dlqpPlainState = emptyEGraphPreparedMatchState
            })
        <$> compileRingPatternQuery patternValue

runDeltaLawEditCycle ::
  [ClassId] ->
  DeltaLawState ->
  (Int, Int) ->
  Either String DeltaLawState
runDeltaLawEditCycle classIds lawState (leftIndex, rightIndex) =
  case (classIdAt leftIndex classIds, classIdAt rightIndex classIds) of
    (Just leftClass, Just rightClass) -> do
      let (rebuildDelta, _repairIndex, rebuiltGraph) =
            rebuildWithDelta (merge leftClass rightClass (dlsGraph lawState))
          dirtyKeys =
            erdDirtyResultKeys rebuildDelta
          markPair pair =
            pair
              { dlqpPlainState = markEGraphPreparedMatchStateDirty dirtyKeys (dlqpPlainState pair)
              }
      nextPairs <-
        traverse (runDeltaLawQueryPair (Just dirtyKeys) rebuiltGraph . markPair) (dlsPairs lawState)
      Right
        DeltaLawState
          { dlsGraph = rebuiltGraph,
            dlsPairs = nextPairs
          }
    _ ->
      Right lawState

runDeltaLawQueryPair ::
  Maybe IntSet.IntSet ->
  EGraph RingF NodeCount ->
  DeltaLawQueryPair ->
  Either String DeltaLawQueryPair
runDeltaLawQueryPair dirtyKeys graph pair = do
  (plainState, plainDelta) <-
    first show (wcojPreparedDeltaMatchCompiledWithRoots (dlqpPlainQuery pair) graph (dlqpPlainState pair))
  oracleDelta <-
    maybe
      (relationalJoinOracleMatches (dlqpPlainQuery pair) graph)
      (\keys -> relationalJoinDeltaOracleMatches (dlqpPlainQuery pair) keys graph)
      dirtyKeys
  if Set.fromList plainDelta == Set.fromList oracleDelta
    then
      Right
        pair
          { dlqpPlainState = plainState
          }
    else
      Left
        ( "direct delta diverged from relational join oracle: direct="
            <> show plainDelta
            <> " oracle="
            <> show oracleDelta
        )

type PreparedSharedCacheLawState :: Type
data PreparedSharedCacheLawState = PreparedSharedCacheLawState
  { psclsGraph :: !(EGraph RingF NodeCount),
    psclsPreparedState :: !(EGraphPreparedMatchState SurfaceKind RingF)
  }

type GeneratedRingScenario :: Type
data GeneratedRingScenario = GeneratedRingScenario ![Fix RingF] ![(Int, Int)]

instance Show GeneratedRingScenario where
  show (GeneratedRingScenario terms mergePairs) =
    "GeneratedRingScenario { termCount = "
      <> show (length terms)
      <> ", mergePairs = "
      <> show mergePairs
      <> " }"

preparedSharedCacheMatchesFreshLaw :: GeneratedRingScenario -> QC.Property
preparedSharedCacheMatchesFreshLaw (GeneratedRingScenario terms mergePairs) =
  case (buildGraph terms, compiledSharedCacheLawQueries) of
    (Left allocationError, _) ->
      QC.counterexample ("generated graph allocation failed: " <> show allocationError) False
    (_, Left compileErrors) ->
      QC.counterexample (show compileErrors) False
    (Right (initialGraph, initialClassIds), Right compiledQueries) ->
      case runPreparedFreshQueries compiledQueries initialGraph emptyEGraphPreparedMatchState of
        Left obstruction ->
          QC.counterexample obstruction False
        Right initialPreparedState ->
          case Foldable.foldlM (runPreparedFreshEditCycle initialClassIds compiledQueries) initialLawState mergePairs of
            Left obstruction ->
              QC.counterexample obstruction False
            Right _ ->
              QC.property True
          where
            initialLawState =
              PreparedSharedCacheLawState
                { psclsGraph = initialGraph,
                  psclsPreparedState = initialPreparedState
                }
runPreparedFreshEditCycle ::
  [ClassId] ->
  [RingCompiledQuery] ->
  PreparedSharedCacheLawState ->
  (Int, Int) ->
  Either String PreparedSharedCacheLawState
runPreparedFreshEditCycle classIds compiledQueries lawState (leftIndex, rightIndex) =
  case (classIdAt leftIndex classIds, classIdAt rightIndex classIds) of
    (Just leftClass, Just rightClass) -> do
      let (rebuildDelta, _repairIndex, rebuiltGraph) =
            rebuildWithDelta (merge leftClass rightClass (psclsGraph lawState))
          dirtyPreparedState =
            markEGraphPreparedMatchStateDirty
              (erdDirtyResultKeys rebuildDelta)
              (psclsPreparedState lawState)
      preparedState <-
        runPreparedFreshQueries compiledQueries rebuiltGraph dirtyPreparedState
      Right
        PreparedSharedCacheLawState
          { psclsGraph = rebuiltGraph,
            psclsPreparedState = preparedState
          }
    _ ->
      Right lawState

runPreparedFreshQueries ::
  [RingCompiledQuery] ->
  EGraph RingF NodeCount ->
  EGraphPreparedMatchState SurfaceKind RingF ->
  Either String (EGraphPreparedMatchState SurfaceKind RingF)
runPreparedFreshQueries compiledQueries graph preparedState =
  Foldable.foldlM (runPreparedFreshQuery graph) preparedState compiledQueries
  where
    runPreparedFreshQuery ::
      EGraph RingF NodeCount ->
      EGraphPreparedMatchState SurfaceKind RingF ->
      RingCompiledQuery ->
      Either String (EGraphPreparedMatchState SurfaceKind RingF)
    runPreparedFreshQuery graphValue currentPreparedState compiledQuery = do
      freshMatches <-
        first show (wcojMatchCompiledWithRoots compiledQuery graphValue)
      (nextPreparedState, preparedMatches) <-
        first show (wcojPreparedMatchCompiledWithRoots compiledQuery graphValue currentPreparedState)
      if preparedMatches == freshMatches
        then Right nextPreparedState
        else
          Left
            ( "prepared matches differed from fresh matches: prepared="
                <> show preparedMatches
                <> " fresh="
                <> show freshMatches
            )

classIdAt :: Int -> [ClassId] -> Maybe ClassId
classIdAt index =
  IntMap.lookup index . IntMap.fromAscList . zip [0 ..]

compiledSharedCacheLawQueries :: Either [EGraph.PatternVar] [RingCompiledQuery]
compiledSharedCacheLawQueries =
  sequenceA
    [ compileRingPatternQuery addXYPattern,
      compileRingPatternQuery addUVPattern,
      compileRingPatternQuery addXXPattern
    ]

rowBlockToRowDelta :: RowBlock 'Canonical -> RowDelta
rowBlockToRowDelta rows =
  plainRowPatchFromList
    ( foldRowBlock
        ( \entries desc ->
            (materializeAtomRow rows desc, MultiplicityChange 1) : entries
        )
        []
        rows
    )

type RingCompiledQuery = CompiledPatternQuery (CompiledGuard SurfaceKind RingF) RingF

type RingF :: Type -> Type
data RingF a
  = Num Int
  | Add a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type RingTag :: Type
data RingTag
  = NumTag Int
  | AddTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag RingF where
  type ConstructorTag RingF = RingTag
  constructorTag ringNode =
    case ringNode of
      Num value ->
        NumTag value
      Add {} ->
        AddTag

instance ZipMatch RingF where
  zipMatch =
    zipSameNodeShape

type NodeCount :: Type
newtype NodeCount = NodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NodeCount where
  join (NodeCount leftCount) (NodeCount rightCount) =
    NodeCount (max leftCount rightCount)

instance QC.Arbitrary GeneratedRingScenario where
  arbitrary = do
    termCount <- QC.chooseInt (3, 9)
    terms <- QC.vectorOf termCount (QC.sized genRingTerm)
    mergePairs <- QC.vectorOf 4 (genMergePair termCount)
    pure (GeneratedRingScenario terms mergePairs)
    where
      genMergePair termCount =
        (,)
          <$> QC.chooseInt (0, termCount - 1)
          <*> QC.chooseInt (0, termCount - 1)

genRingTerm :: Int -> QC.Gen (Fix RingF)
genRingTerm size
  | size <= 0 =
      ringNum <$> QC.chooseInt (0, 5)
  | otherwise =
      QC.frequency
        [ (3, ringNum <$> QC.chooseInt (0, 5)),
          (2, ringAdd <$> child <*> child)
        ]
  where
    child =
      genRingTerm (size `div` 2)

ringAnalysis :: AnalysisSpec RingF NodeCount
ringAnalysis =
  semilatticeAnalysis ringNodeCount

ringNodeCount :: RingF NodeCount -> NodeCount
ringNodeCount ringNode =
  case ringNode of
    Num _ ->
      NodeCount 1
    Add (NodeCount leftCount) (NodeCount rightCount) ->
      NodeCount (leftCount + rightCount + 1)

egraphTerms :: [Fix RingF]
egraphTerms =
  [ ringNum 1,
    ringNum 2,
    ringNum 3,
    ringAdd (ringNum 1) (ringNum 3),
    ringAdd (ringNum 2) (ringNum 3)
  ]

disconnectedTerms :: [Fix RingF]
disconnectedTerms =
  [ ringNum 101,
    ringNum 102,
    ringAdd (ringNum 101) (ringNum 102),
    ringAdd (ringNum 102) (ringNum 101)
  ]

nestedTerms :: [Fix RingF]
nestedTerms =
  [ ringAdd (ringAdd (ringNum 1) (ringNum 3)) (ringNum 2)
  ]

repeatedChildTerms :: [Fix RingF]
repeatedChildTerms =
  [ ringNum 4,
    ringNum 5,
    ringAdd (ringNum 4) (ringNum 4),
    ringAdd (ringNum 4) (ringNum 5)
  ]

ringNum :: Int -> Fix RingF
ringNum value =
  Fix (Num value)

ringAdd :: Fix RingF -> Fix RingF -> Fix RingF
ringAdd leftTerm rightTerm =
  Fix (Add leftTerm rightTerm)

addXYPattern :: Pattern RingF
addXYPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

addXXPattern :: Pattern RingF
addXXPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 0)))

addUVPattern :: Pattern RingF
addUVPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 20)) (PatternVar (EGraph.mkPatternVar 21)))

addYZPattern :: Pattern RingF
addYZPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 2)))

nestedAddPattern :: Pattern RingF
nestedAddPattern =
  PatternNode (Add addXYPattern (PatternVar (EGraph.mkPatternVar 2)))

compileRingPatternQuery :: Pattern RingF -> Either [EGraph.PatternVar] RingCompiledQuery
compileRingPatternQuery patternValue =
  compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue)

compileRingGuardedPatternQueryWith ::
  Pattern RingF ->
  RewriteCondition SurfaceKind RingF ->
  Either [EGraph.PatternVar] RingCompiledQuery
compileRingGuardedPatternQueryWith patternValue condition =
  compilePatternQuery
    combineCompiledGuards
    compileGuard
    (guardedPatternQuery (singlePatternQuery patternValue) condition)

compileRingConjunctivePatternQuery :: NonEmpty (Pattern RingF) -> Either [EGraph.PatternVar] RingCompiledQuery
compileRingConjunctivePatternQuery patternValues =
  compilePatternQuery combineCompiledGuards compileGuard (conjunctivePatternQuery patternValues)

buildGraph :: [Fix RingF] -> Either EGraph.UnionFindAllocationError (EGraph RingF NodeCount, [ClassId])
buildGraph terms =
  fmap (fmap reverse) $
    foldM
      ( \(graph, classIds) term ->
          fmap
            (\(classId, nextGraph) -> (nextGraph, classId : classIds))
            (addTerm term graph)
      )
      (emptyEGraph ringAnalysis, [])
      terms

foldPatchRows :: 
  IntMap RowDelta ->
  QuotientPatch ->
  IntMap RowDelta
foldPatchRows initialRows patch =
  IntMap.unionWith composePlainRowPatch initialRows (fmap atomPatchRows (qpEvents patch))

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure (show errorValue) *> fail "expected Right"
    Right value ->
      pure value

fixtureAllocationFailure :: EGraph.UnionFindAllocationError -> Assertion
fixtureAllocationFailure allocationError =
  assertFailure ("egraph fixture allocation failed: " <> show allocationError)
