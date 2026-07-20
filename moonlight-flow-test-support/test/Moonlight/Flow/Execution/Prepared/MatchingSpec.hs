{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Execution.Prepared.MatchingSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core
  ( QuerySnapshot (..),
    emptyFootprint,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    DenseArrangementPatchError,
    denseProjectedAtomSourceFromRows,
    denseProjectedRowsFromRows,
  )
import Moonlight.Flow.Execution.Direct qualified as Direct
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    prValue,
    runPrepared,
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheLimits (..),
    JoinCacheState (..),
    PreparedCacheKey (..),
    emptyJoinCacheState,
  )
import Moonlight.Flow.Execution.Prepared.Backend (PreparedJoinCacheState)
import Test.Moonlight.Flow.Execution.Prepared.Matching
  ( CachedRequestOps (..),
    runCachedJoinQueryBatchWith,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Execution.Prepared.Request
  ( PreparedRequestView (..),
  )
import Moonlight.Flow.Storage.Relation
  ( atomRowsFromTupleKeys,
    relationFromAtomRows,
  )
import Moonlight.Flow.Storage.Plan qualified as StoragePlan
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    emptyRestriction,
    restrictPinnedRow,
    restrictRootSlot,
  )
import Moonlight.Flow.Storage.Store
  ( storeFromRelations,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
  )
import Test.Moonlight.Flow.Execution.Prepared.CacheProgram
  ( MatchRow (..),
    SimpleRequest (..),
    TestBackend,
    TestPlan,
    atomRow,
    compilePlan,
    contextRequest,
    countBasePrepared,
    countContextPrepared,
    identityProjection,
    joinDatabase,
    mkSnapshot,
    rowKeys,
    testPreparedBackend,
  )
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBlockIdentity (..),
    RowBuildError,
    RowState (Canonical),
  )
import Moonlight.Differential.Row.Block qualified as Row
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

type Ctx :: Type
type Ctx = String

cachedOps :: CachedRequestOps (SimpleRequest Ctx ()) Ctx TestBackend Int () MatchRow
cachedOps =
  CachedRequestOps
    { croQuery = const (),
      croView = \request -> PreparedRequestView (srHost request) (srContext request),
      croFilterMatches = \_ _ -> id
    }

type CachedState :: Type
type CachedState = PreparedJoinCacheState Ctx TestBackend

runCached :: Maybe IntSet.IntSet -> SimpleRequest Ctx () -> CachedState -> (CachedState, [MatchRow])
runCached wantedRoots request st =
  let (st1, matches) =
        expectRightResult
          ( runCachedJoinQueryBatchWith
              compilePlan
              testPreparedBackend
              cachedOps
              wantedRoots
              [request]
              st
          )
   in (st1, maybe [] id (listToMaybe matches))

rowSet :: [MatchRow] -> Set [Int]
rowSet =
  Set.fromList . fmap (rowKeys . mrRow)

type FilteredRequest :: Type -> Type
data FilteredRequest c = FilteredRequest
  { frRequest :: !(SimpleRequest c ()),
    frAllowedRoots :: !(Maybe (Set Int))
  }

filteredCachedOps :: CachedRequestOps (FilteredRequest Ctx) Ctx TestBackend Int () MatchRow
filteredCachedOps =
  CachedRequestOps
    { croQuery = const (),
      croView =
        \request ->
          PreparedRequestView
            (srHost (frRequest request))
            (srContext (frRequest request)),
      croFilterMatches =
        \request _ ->
          filter
            ( \matchRow ->
                maybe
                  True
                  (\allowedRoots -> maybe False (`Set.member` allowedRoots) (rootOf matchRow))
                  (frAllowedRoots request)
            )
    }
  where
    rootOf matchRow =
      case rowKeys (mrRow matchRow) of
        rootKey : _ -> Just rootKey
        [] -> Nothing

runFilteredBatch :: Maybe IntSet.IntSet -> [FilteredRequest Ctx] -> CachedState -> (CachedState, [[MatchRow]])
runFilteredBatch wantedRoots requests =
  expectRightResult . runCachedJoinQueryBatchWith compilePlan testPreparedBackend filteredCachedOps wantedRoots requests

expectRightResult :: Show obstruction => (state, Either obstruction value) -> (state, value)
expectRightResult (stateValue, result) =
  case result of
    Right value ->
      (stateValue, value)
    Left obstruction ->
      error ("unexpected cached join obstruction in test: " <> show obstruction)

expectRight :: Show obstruction => Either obstruction value -> IO value
expectRight result =
  case result of
    Right value ->
      pure value
    Left obstruction ->
      assertFailure ("unexpected row build obstruction in test: " <> show obstruction)

hasContextPrepared :: Ctx -> QueryId -> Int -> JoinCacheState Ctx plan basePrepared contextPrepared repair -> Bool
hasContextPrepared contextValue queryIdValue liveEpochValue stateValue =
  Map.member
    (ContextPreparedKey contextValue queryIdValue liveEpochValue)
    (jcsPrepared stateValue)

type StructuralSourceSpec :: Type
data StructuralSourceSpec = StructuralSourceSpec
  { sssAtomKey :: {-# UNPACK #-} !Int,
    sssColumns :: !(Vector SlotId),
    sssRecipe :: !StalkRecipe,
    sssPhysicalWidth :: {-# UNPACK #-} !Int,
    sssRows :: ![[Int]]
  }

projectedRowsPlan ::
  Word64 ->
  SlotId ->
  Vector SlotId ->
  [StructuralSourceSpec] ->
  Either [PlanBuild.QueryPlanError] TestPlan
projectedRowsPlan digest rootSlotForPlan outputSchema sourceSpecsValue =
  PlanBuild.mkQueryPlan
    ( PlanBuild.QueryPlanInput
        { PlanBuild.qpiDomain = PlanBuild.StructuralQueryPlan,
          PlanBuild.qpiCompiled = (),
          PlanBuild.qpiDigest = digest,
          PlanBuild.qpiAtoms =
            Vector.fromList
              [ mkAtomSpec
                  (mkQueryAtomId (sssAtomKey sourceSpecValue))
                  (mkSourceAtomId (mkAtomId (sssAtomKey sourceSpecValue)))
                  ()
                  0
                  (sssColumns sourceSpecValue)
                  (sssRecipe sourceSpecValue)
                | sourceSpecValue <- sourceSpecsValue
              ],
          PlanBuild.qpiSchemaOrder = Just outputSchema,
          PlanBuild.qpiRootSlot = rootSlotForPlan,
          PlanBuild.qpiOutputs =
            fmap (`PlanBuild.PlanOutputBinding` ()) (Vector.toList outputSchema),
          PlanBuild.qpiResidual = PlanBuild.NoQueryPlanResidual
        }
    )

projectedArrangement :: Int -> StructuralSourceSpec -> Either DenseArrangementPatchError DenseArrangement
projectedArrangement sourceId sourceSpecValue = do
  projectedRows <-
    denseProjectedRowsFromRows
      (physicalLayout (sssPhysicalWidth sourceSpecValue))
      (fmap atomRow (sssRows sourceSpecValue))
  pure
    ( denseProjectedAtomSourceFromRows
        (DenseArrangementId sourceId)
        (mkAtomId (sssAtomKey sourceSpecValue))
        (sssColumns sourceSpecValue)
        (sssRecipe sourceSpecValue)
        projectedRows
    )

physicalLayout :: Int -> Vector SlotId
physicalLayout width =
  Vector.fromList [mkSlotId slotKey | slotKey <- [0 .. width - 1]]

runProjectedPreparedRows ::
  TestPlan ->
  Restriction ->
  [DenseArrangement] ->
  IO (Set [Int])
runProjectedPreparedRows planValue restriction sources =
  fmap (Set.fromList . fmap rowKeys) $
    expectRight $
      prValue
        <$> runPrepared
          PreparedRunSpec
            { prsPlan = planValue,
              prsRestriction = restriction,
              prsStore = storeFromRelations IntMap.empty,
              prsView = unrestrictedView,
              prsAtomDeltas = IntMap.empty,
              prsStructuralSources = Just sources,
              prsOp = PreparedRows Nothing,
              prsMode = PreparedValueOnly
            }

runMaterializedPreparedRows ::
  TestPlan ->
  Restriction ->
  IO (Set [Int])
runMaterializedPreparedRows planValue restriction = do
  database <-
    expectRight $
      joinDatabase
        [ [1, 101],
          [2, 102]
        ]
        [ [1, 201],
          [2, 202]
        ]
        [ [101, 201],
          [102, 202]
        ]
  relations <- expectRight (traverse relationFromAtomRows database)
  fmap (Set.fromList . fmap rowKeys) $
    expectRight $
      prValue
        <$> runPrepared
          PreparedRunSpec
            { prsPlan = planValue,
              prsRestriction = restriction,
              prsStore = storeFromRelations relations,
              prsView = unrestrictedView,
              prsAtomDeltas = IntMap.empty,
              prsStructuralSources = Nothing,
              prsOp = PreparedRows Nothing,
              prsMode = PreparedValueOnly
            }

compiledStoragePlanFor :: TestPlan -> IO StoragePlan.CompiledStoragePlan
compiledStoragePlanFor planValue =
  expectRight (StoragePlan.compileStoragePlan (StoragePlan.storagePlanFromQueryPlan planValue))

directRelation :: Int -> [SlotId] -> [[Int]] -> Either RowBuildError (RowBlock 'Canonical)
directRelation entityKey columns rows =
  atomRowsFromTupleKeys
    ( RowBlockIdentity
        { rowBlockBaseRevision = 0,
          rowBlockOverlayEpoch = 0,
          rowBlockPlanFingerprint = 11,
          rowBlockEntityKey = entityKey,
          rowBlockGeneration = 0
        }
    )
    (Vector.fromList columns)
    (fmap atomRow rows)

directRows ::
  TestPlan ->
  StoragePlan.CompiledStoragePlan ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  IO (Set [Int])
directRows planValue compiled rowsByAtom =
  fmap (Set.fromList . fmap (rowKeys . mrRow)) $
    expectRight $
      Direct.evalPlanOutputsWithCompiledStoragePlanAndRootSelection
        planValue
        compiled
        Direct.RuntimeAllRoots
        rowsByAtom

directRuntimeRows ::
  TestPlan ->
  StoragePlan.CompiledStoragePlan ->
  IntMap.IntMap Direct.RuntimeSection ->
  IO (Set [Int])
directRuntimeRows planValue compiled rowsByAtom =
  fmap (Set.fromList . fmap (rowKeys . mrRow)) $
    expectRight $
      Direct.evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections
        planValue
        compiled
        Direct.RuntimeAllRoots
        rowsByAtom

emptyRuntimeMask :: RowBlock 'Canonical -> RowBlock 'Canonical
emptyRuntimeMask rows =
  Row.emptyRowBlock (Row.rowBlockIdentity rows) (Row.rowBlockLayout rows)

sourceSpec ::
  Int ->
  [SlotId] ->
  [[SlotSource]] ->
  Int ->
  [[Int]] ->
  StructuralSourceSpec
sourceSpec atomKey columns recipe physicalWidth rows =
  StructuralSourceSpec
    { sssAtomKey = atomKey,
      sssColumns = Vector.fromList columns,
      sssRecipe = mkStalkRecipe (Vector.fromList recipe),
      sssPhysicalWidth = physicalWidth,
      sssRows = rows
    }

rootSlotValue :: SlotId
rootSlotValue =
  mkSlotId 0

xSlotValue :: SlotId
xSlotValue =
  mkSlotId 1

ySlotValue :: SlotId
ySlotValue =
  mkSlotId 2

singleAddSourceSpec :: StructuralSourceSpec
singleAddSourceSpec =
  sourceSpec
    0
    [rootSlotValue, xSlotValue, ySlotValue]
    [[SourceResult], [SourceChild 0], [SourceChild 1]]
    3
    [ [10, 1, 2],
      [11, 2, 2],
      [12, 3, 4]
    ]

repeatedChildSourceSpec :: StructuralSourceSpec
repeatedChildSourceSpec =
  sourceSpec
    0
    [rootSlotValue, xSlotValue]
    [[SourceResult], [SourceChild 0, SourceChild 1]]
    3
    [ [10, 1, 2],
      [11, 2, 2],
      [12, 3, 4]
    ]

materializedEquivalentSourceSpecs :: [StructuralSourceSpec]
materializedEquivalentSourceSpecs =
  [ sourceSpec
      0
      [rootSlotValue, xSlotValue]
      [[SourceResult], [SourceChild 0]]
      2
      [ [1, 101],
        [2, 102]
      ],
    sourceSpec
      1
      [rootSlotValue, ySlotValue]
      [[SourceResult], [SourceChild 0]]
      2
      [ [1, 201],
        [2, 202]
      ],
    sourceSpec
      2
      [xSlotValue, ySlotValue]
      [[SourceResult], [SourceChild 0]]
      2
      [ [101, 201],
        [102, 202]
      ]
  ]

tests :: TestTree
tests =
  testGroup
    "CachedAlgebra"
    [ testCase "composed runtime section agrees with materialized base-minus-mask-plus-extras" $ do
        planValue <- expectRight (compilePlan ())
        compiled <- compiledStoragePlanFor planValue
        baseRootX <- expectRight (directRelation 0 [rootSlotValue, xSlotValue] [[1, 101], [2, 102], [3, 103]])
        maskRootX <- expectRight (directRelation 0 [rootSlotValue, xSlotValue] [[2, 102]])
        extraRootX <- expectRight (directRelation 0 [rootSlotValue, xSlotValue] [[4, 104]])
        baseRootY <- expectRight (directRelation 1 [rootSlotValue, ySlotValue] [[1, 201], [2, 202], [3, 203], [4, 204]])
        baseXY <- expectRight (directRelation 2 [xSlotValue, ySlotValue] [[101, 201], [102, 202], [103, 203], [104, 204]])
        materializedRootX <- expectRight (directRelation 0 [rootSlotValue, xSlotValue] [[1, 101], [3, 103], [4, 104]])
        composedRows <-
          directRuntimeRows
            planValue
            compiled
            ( IntMap.fromList
                [ (0, Direct.composedRuntimeSection baseRootX maskRootX (Just extraRootX)),
                  (1, Direct.wholeRuntimeSection baseRootY),
                  (2, Direct.wholeRuntimeSection baseXY)
                ]
            )
        materializedRows <-
          directRows
            planValue
            compiled
            (IntMap.fromList [(0, materializedRootX), (1, baseRootY), (2, baseXY)])
        composedRows @?= materializedRows,
      testCase "empty runtime composition degenerates to whole-block execution" $ do
        planValue <- expectRight (compilePlan ())
        compiled <- compiledStoragePlanFor planValue
        baseRootX <- expectRight (directRelation 0 [rootSlotValue, xSlotValue] [[1, 101], [2, 102]])
        baseRootY <- expectRight (directRelation 1 [rootSlotValue, ySlotValue] [[1, 201], [2, 202]])
        baseXY <- expectRight (directRelation 2 [xSlotValue, ySlotValue] [[101, 201], [102, 202]])
        let wholeRows =
              IntMap.fromList [(0, baseRootX), (1, baseRootY), (2, baseXY)]
            runtimeRows =
              IntMap.map
                (\rows -> Direct.composedRuntimeSection rows (emptyRuntimeMask rows) Nothing)
                wholeRows
        wholeResult <- directRows planValue compiled wholeRows
        runtimeResult <- directRuntimeRows planValue compiled runtimeRows
        runtimeResult @?= wholeResult,
      testCase "projected dense source maps result and child columns" $ do
        planValue <-
          expectRight $
            projectedRowsPlan
              101
              rootSlotValue
              (Vector.fromList [rootSlotValue, xSlotValue, ySlotValue])
              [singleAddSourceSpec]
        sourceArrangement <-
          expectRight (projectedArrangement 0 singleAddSourceSpec)
        rows <-
          runProjectedPreparedRows
            planValue
            emptyRestriction
            [sourceArrangement]
        rows @?= Set.fromList [[10, 1, 2], [11, 2, 2], [12, 3, 4]],
      testCase "projected dense source enforces repeated-source equality" $ do
        planValue <-
          expectRight $
            projectedRowsPlan
              102
              rootSlotValue
              (Vector.fromList [rootSlotValue, xSlotValue])
              [repeatedChildSourceSpec]
        sourceArrangement <-
          expectRight (projectedArrangement 0 repeatedChildSourceSpec)
        rows <-
          runProjectedPreparedRows
            planValue
            emptyRestriction
            [sourceArrangement]
        rows @?= Set.singleton [11, 2],
      testCase "projected dense source applies root-slot restriction" $ do
        planValue <-
          expectRight $
            projectedRowsPlan
              103
              rootSlotValue
              (Vector.fromList [rootSlotValue, xSlotValue, ySlotValue])
              [singleAddSourceSpec]
        sourceArrangement <-
          expectRight (projectedArrangement 0 singleAddSourceSpec)
        rows <-
          runProjectedPreparedRows
            planValue
            (restrictRootSlot rootSlotValue (IntSet.singleton 12))
            [sourceArrangement]
        rows @?= Set.singleton [12, 3, 4],
      testCase "projected dense source applies pinned atom-row restriction" $ do
        planValue <-
          expectRight $
            projectedRowsPlan
              104
              rootSlotValue
              (Vector.fromList [rootSlotValue, xSlotValue, ySlotValue])
              [singleAddSourceSpec]
        sourceArrangement <-
          expectRight (projectedArrangement 0 singleAddSourceSpec)
        rows <-
          runProjectedPreparedRows
            planValue
            (restrictPinnedRow (mkAtomId 0) (atomRow [11, 2, 2]))
            [sourceArrangement]
        rows @?= Set.singleton [11, 2, 2],
      testCase "projected dense source rows agree with materialized prepared rows" $ do
        planValue <-
          expectRight $
            projectedRowsPlan
              105
              rootSlotValue
              (Vector.fromList [rootSlotValue, xSlotValue, ySlotValue])
              materializedEquivalentSourceSpecs
        sourceArrangements <-
          expectRight $
            traverse
              ( \(sourceId, sourceSpecValue) ->
                  projectedArrangement sourceId sourceSpecValue
              )
              (zip [0 ..] materializedEquivalentSourceSpecs)
        projectedRows <-
          runProjectedPreparedRows
            planValue
            emptyRestriction
            sourceArrangements
        materializedRows <-
          runMaterializedPreparedRows
            planValue
            emptyRestriction
        projectedRows @?= materializedRows,
      testCase "reuses plan and base prepared cache across repeated base requests" $ do
        database <-
          expectRight $
            joinDatabase
              [ [1, 101],
                [2, 102]
              ]
              [ [1, 201],
                [2, 202]
              ]
              [ [101, 201],
                [102, 202]
              ]
        let request =
              (SimpleRequest {srHost = database, srContext = Nothing} :: SimpleRequest Ctx ())
            (state1, rows1) = runCached Nothing request emptyJoinCacheState
            (state2, rows2) = runCached Nothing request state1
        rowSet rows1 @?= Set.fromList [[1, 101, 201], [2, 102, 202]]
        rowSet rows2 @?= rowSet rows1
        Map.size (jcsPlanCache state1) @?= 1
        countBasePrepared state1 @?= 1
        countContextPrepared state1 @?= 0
        Map.size (jcsPlanCache state2) @?= 1
        countBasePrepared state2 @?= 1
        countContextPrepared state2 @?= 0,
      testCase "context prepared cache keys use context query id and live epoch but not base revision" $ do
        database <-
          expectRight $
            joinDatabase
              [ [1, 101],
                [2, 102]
              ]
              [ [1, 201],
                [2, 202]
              ]
              [ [101, 201],
                [102, 202]
              ]
        let snapshot0 =
              mkSnapshot
                1
                41
                7
                emptyFootprint
                database
                (identityProjection [[1, 101, 201], [2, 102, 202]])
            request0 = contextRequest "ctx" snapshot0
            (state1, rows1) = runCached Nothing request0 emptyJoinCacheState
            (state2, rows2) =
              runCached
                Nothing
                (contextRequest "ctx" (snapshot0 {baseRevision = 99}))
                state1
            (state3, rows3) =
              runCached
                Nothing
                (contextRequest "other-ctx" snapshot0)
                state2
            (state4, rows4) =
              runCached
                Nothing
                (contextRequest "ctx" (snapshot0 {liveEpoch = 8}))
                state3
            (state5, rows5) =
              runCached
                Nothing
                (contextRequest "ctx" (snapshot0 {queryId = mkQueryId 42}))
                state4
            expectedRows = Set.fromList [[1, 101, 201], [2, 102, 202]]
        rowSet rows1 @?= expectedRows
        rowSet rows2 @?= expectedRows
        rowSet rows3 @?= expectedRows
        rowSet rows4 @?= expectedRows
        rowSet rows5 @?= expectedRows
        countContextPrepared state1 @?= 1
        countContextPrepared state2 @?= 1
        countContextPrepared state3 @?= 2
        countContextPrepared state4 @?= 3
        countContextPrepared state5 @?= 4
        assertBool "original cache key installed" (hasContextPrepared "ctx" (mkQueryId 41) 7 state5)
        assertBool "alternate context key installed" (hasContextPrepared "other-ctx" (mkQueryId 41) 7 state5)
        assertBool "alternate live epoch key installed" (hasContextPrepared "ctx" (mkQueryId 41) 8 state5)
        assertBool "alternate query id key installed" (hasContextPrepared "ctx" (mkQueryId 42) 7 state5),
      testCase "prepared cache eviction is true LRU by touched time" $ do
        baseDatabase <-
          expectRight $
            joinDatabase
              [ [1, 101] ]
              [ [1, 201] ]
              [ [101, 201] ]
        contextDatabase1 <-
          expectRight $
            joinDatabase
              [ [2, 102] ]
              [ [2, 202] ]
              [ [102, 202] ]
        contextDatabase2 <-
          expectRight $
            joinDatabase
              [ [3, 103] ]
              [ [3, 203] ]
              [ [103, 203] ]
        let baseRequest =
              (SimpleRequest {srHost = baseDatabase, srContext = Nothing} :: SimpleRequest Ctx ())
            requestCtx1 =
              contextRequest
                "ctx-1"
                (mkSnapshot 1 1 1 emptyFootprint contextDatabase1 (identityProjection [[2, 102, 202]]))
            requestCtx2 =
              contextRequest
                "ctx-2"
                (mkSnapshot 1 2 1 emptyFootprint contextDatabase2 (identityProjection [[3, 103, 203]]))
            state0 =
              ( emptyJoinCacheState
                  { jcsLimits =
                      JoinCacheLimits
                        { jclMaxPreparedEntries = 2
                        }
                  } ::
                  CachedState
              )
            (state1, _) = runCached Nothing baseRequest state0
            (state2, _) = runCached Nothing requestCtx1 state1
            (state3, _) = runCached Nothing baseRequest state2
            (state4, rows4) = runCached Nothing requestCtx2 state3
        rowSet rows4 @?= Set.fromList [[3, 103, 203]]
        countBasePrepared state4 @?= 1
        countContextPrepared state4 @?= 1
        hasContextPrepared "ctx-1" (mkQueryId 1) 1 state4 @?= False
        hasContextPrepared "ctx-2" (mkQueryId 2) 1 state4 @?= True,
      testCase "frontier restriction limits results to requested roots" $ do
        database <-
          expectRight $
            joinDatabase
              [ [1, 101],
                [2, 102]
              ]
              [ [1, 201],
                [2, 202]
              ]
              [ [101, 201],
                [102, 202]
              ]
        let request =
              (SimpleRequest {srHost = database, srContext = Nothing} :: SimpleRequest Ctx ())
            (_, fullRows) = runCached Nothing request emptyJoinCacheState
            (_, restrictedRows) =
              runCached
                (Just (IntSet.singleton 2))
                request
                emptyJoinCacheState
        rowSet fullRows @?= Set.fromList [[1, 101, 201], [2, 102, 202]]
        rowSet restrictedRows @?= Set.fromList [[2, 102, 202]],
      testCase "batched base requests preserve per-request results while sharing one prepared scope" $ do
        database <-
          expectRight $
            joinDatabase
              [ [1, 101],
                [2, 102]
              ]
              [ [1, 201],
                [2, 202]
              ]
              [ [101, 201],
                [102, 202]
              ]
        let request =
              (SimpleRequest {srHost = database, srContext = Nothing} :: SimpleRequest Ctx ())
            filteredRequests =
              ( [ FilteredRequest request (Just (Set.singleton 1)),
                  FilteredRequest request (Just (Set.singleton 2)),
                  FilteredRequest request Nothing
                ] ::
                  [FilteredRequest Ctx]
              )
            (batchState, batchRows) =
              runFilteredBatch Nothing filteredRequests emptyJoinCacheState
        fmap rowSet batchRows
          @?= [ Set.fromList [[1, 101, 201]],
                Set.fromList [[2, 102, 202]],
                Set.fromList [[1, 101, 201], [2, 102, 202]]
              ]
        Map.size (jcsPlanCache batchState) @?= 1
        countBasePrepared batchState @?= 1
        countContextPrepared batchState @?= 0,
      testCase "batched context requests preserve request-local filtering and reuse context prepared cache" $ do
        database <-
          expectRight $
            joinDatabase
              [ [1, 101],
                [2, 102]
              ]
              [ [1, 201],
                [2, 202]
              ]
              [ [101, 201],
                [102, 202]
              ]
        let snapshot0 =
              mkSnapshot
                1
                41
                7
                emptyFootprint
                database
                (identityProjection [[1, 101, 201], [2, 102, 202]])
            request =
              contextRequest "ctx" snapshot0
            filteredRequests =
              ( [ FilteredRequest request (Just (Set.singleton 1)),
                  FilteredRequest request (Just (Set.singleton 2)),
                  FilteredRequest request Nothing
                ] ::
                  [FilteredRequest Ctx]
              )
            (batchState, batchRows) =
              runFilteredBatch Nothing filteredRequests emptyJoinCacheState
        fmap rowSet batchRows
          @?= [ Set.fromList [[1, 101, 201]],
                Set.fromList [[2, 102, 202]],
                Set.fromList [[1, 101, 201], [2, 102, 202]]
              ]
        Map.size (jcsPlanCache batchState) @?= 1
        countBasePrepared batchState @?= 0
        countContextPrepared batchState @?= 1
        assertBool
          "context prepared cache entry installed once"
          (hasContextPrepared "ctx" (mkQueryId 41) 7 batchState)
    ]
