{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Property.Runtime.BranchSharing
  ( BranchSharingPopulationConfig (..),
    BranchSharingPopulationReport (..),
    BranchSharingPopulationRun,
    branchSharingPopulationConfig,
    branchSharingPopulationRunReport,
    branchSharingSoakConfig,
    runBranchSharingPopulation,
    runBranchSharingPopulationRun,
    keepBranchSharingPopulationRun,
    populationSpec,
    spec,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Foldable
  ( asum,
    traverse_,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Differential.Time
  ( enterRuntimeTimeScope,
    runtimeScopePath,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierSnapshot,
    CarrierStore,
    carrierCurrentAddresses,
    lookupCarrierSnapshot,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    relationalTimeScope,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
    positivePlainRowPatchRows,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    boundaryShape,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDeltas,
  )
import Moonlight.Flow.Carrier.Reuse
  ( ReuseMode (ExactOnly),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeIndexOps,
  )
import System.Mem.StableName
  ( StableName,
    makeStableName,
  )
import Test.Moonlight.Flow.Oracle.Runtime.Program
  ( Ctx,
    Evidence,
    Prop,
    RuntimeProgramCase (..),
    TestRuntime,
    carrierTime,
    currentSnapshot,
    defaultRuntimeTriangleOptions,
    insertAtomSnapshots,
    insertSnapshots,
    rowsOf,
    runtimeFromProgramCases,
    runtimeTriangleCase,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

type Addr =
  CarrierAddr Ctx Carrier Prop

type Snapshot =
  CarrierSnapshot Ctx Carrier Prop RuntimeBoundary Evidence

type Delta =
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence

type RuntimeErr =
  RelationalRuntimeError Ctx Prop RuntimeBoundary Evidence

runtimeIndexStores ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  IntMap.IntMap (CarrierStore ctx Carrier prop boundary evidence)
runtimeIndexStores =
  runtimeIndexOps . rdrState
{-# INLINE runtimeIndexStores #-}

data BranchSharingPopulationConfig = BranchSharingPopulationConfig
  { bspRootBranches :: {-# UNPACK #-} !Int,
    bspNestedBranches :: {-# UNPACK #-} !Int,
    bspTurnoverSteps :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data BranchSharingPopulationReport = BranchSharingPopulationReport
  { bsprActiveTimelines :: {-# UNPACK #-} !Int,
    bsprAnchorSnapshots :: {-# UNPACK #-} !Int,
    bsprGeneratedRowsChecked :: {-# UNPACK #-} !Int,
    bsprDistinctAnchorStableNames :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data BranchSharingPopulationRun = BranchSharingPopulationRun
  { bsrunReport :: !BranchSharingPopulationReport,
    bsrunState :: !BranchPopulationState
  }

data BranchSharingFixture = BranchSharingFixture
  { bsfBase :: !TestRuntime,
    bsfTargetSnapshot :: !Delta,
    bsfTargetAddr :: !Addr,
    bsfAnchorAddrs :: ![Addr]
  }

data BranchTimeline = BranchTimeline
  { btId :: {-# UNPACK #-} !Int,
    btAncestorIds :: ![Int],
    btTime :: !(RelationalCarrierTime Ctx),
    btRuntime :: !TestRuntime
  }

data BranchPopulationState = BranchPopulationState
  { bpsFixture :: !BranchSharingFixture,
    bpsNextId :: {-# UNPACK #-} !Int,
    bpsActive :: !(Map Int BranchTimeline)
  }

spec :: TestTree
spec =
  testGroup
    "runtime-branch-sharing"
    [ testCase
        "unmodified carrier snapshots are physically shared across independently mutated runtime branches"
        branchSharingCurrentSnapshots,
      populationSpec
    ]

populationSpec :: TestTree
populationSpec =
  testCase
    "branch population shares anchors and isolates nested local rows"
    branchSharingPopulationAssertion

branchSharingPopulationConfig :: BranchSharingPopulationConfig
branchSharingPopulationConfig =
  BranchSharingPopulationConfig
    { bspRootBranches = 128,
      bspNestedBranches = 32,
      bspTurnoverSteps = 0
    }

branchSharingSoakConfig :: BranchSharingPopulationConfig
branchSharingSoakConfig =
  BranchSharingPopulationConfig
    { bspRootBranches = 256,
      bspNestedBranches = 64,
      bspTurnoverSteps = 1024
    }

branchSharingCurrentSnapshots :: Assertion
branchSharingCurrentSnapshots =
  runBranchSharingPopulation currentSnapshotConfig *> pure ()
  where
    currentSnapshotConfig =
      BranchSharingPopulationConfig
        { bspRootBranches = 64,
          bspNestedBranches = 0,
          bspTurnoverSteps = 0
        }

branchSharingPopulationAssertion :: Assertion
branchSharingPopulationAssertion =
  runBranchSharingPopulation branchSharingPopulationConfig *> pure ()

runBranchSharingPopulation ::
  BranchSharingPopulationConfig ->
  IO BranchSharingPopulationReport
runBranchSharingPopulation =
  fmap bsrunReport . runBranchSharingPopulationRun

runBranchSharingPopulationRun ::
  BranchSharingPopulationConfig ->
  IO BranchSharingPopulationRun
runBranchSharingPopulationRun rawConfig = do
  let config =
        normalizePopulationConfig rawConfig
  fixture <- buildBranchSharingFixture
  state0 <- initialBranchPopulation config fixture
  state1 <- advanceBranchPopulation (bspTurnoverSteps config) state0
  report <- validateBranchPopulation state1
  pure
    BranchSharingPopulationRun
      { bsrunReport = report,
        bsrunState = state1
      }

keepBranchSharingPopulationRun :: BranchSharingPopulationRun -> IO ()
keepBranchSharingPopulationRun runValue =
  bsprActiveTimelines report
    `seq` bsprAnchorSnapshots report
    `seq` activeSnapshotWitness
    `seq` pure ()
  where
    report =
      bsrunReport runValue

    stateValue =
      bsrunState runValue

    activeSnapshotWitness =
      sum
        [ length (currentCarrierAddrs (btRuntime timeline))
        | timeline <- Map.elems (bpsActive stateValue)
        ]

branchSharingPopulationRunReport ::
  BranchSharingPopulationRun ->
  BranchSharingPopulationReport
branchSharingPopulationRunReport =
  bsrunReport

normalizePopulationConfig :: BranchSharingPopulationConfig -> BranchSharingPopulationConfig
normalizePopulationConfig config =
  config
    { bspRootBranches = max 1 (bspRootBranches config),
      bspNestedBranches = max 0 (bspNestedBranches config),
      bspTurnoverSteps = max 0 (bspTurnoverSteps config)
    }

buildBranchSharingFixture :: IO BranchSharingFixture
buildBranchSharingFixture = do
  runtimeCase <-
    expectRight $
      runtimeTriangleCase
        defaultRuntimeTriangleOptions
        (carrierTime 0 0)
        0
        0
        ()

  base0 <-
    expectRight $
      runtimeFromProgramCases
        [runtimeCase]
        (rpcPlanReuse runtimeCase)
        ExactOnly

  base1 <-
    expectRight $
      insertAtomSnapshots [runtimeCase] base0

  base <-
    expectRight $
      insertSnapshots (rpcCarrierSnapshots runtimeCase) base1

  targetSnapshot <-
    expectJust
      "runtimeTriangleCase produced no atom snapshots"
      (listHeadMaybe (rpcAtomSnapshots runtimeCase))

  let targetAddr =
        deAddr targetSnapshot
      allAddrs =
        currentCarrierAddrs base
      anchorAddrs =
        filter (/= targetAddr) allAddrs

  assertBool
    "expected inserted runtime to contain current carrier snapshots"
    (not (null allAddrs))

  assertBool
    "expected at least one unmodified carrier snapshot anchor"
    (not (null anchorAddrs))

  pure
    BranchSharingFixture
      { bsfBase = base,
        bsfTargetSnapshot = targetSnapshot,
        bsfTargetAddr = targetAddr,
        bsfAnchorAddrs = anchorAddrs
      }

initialBranchPopulation ::
  BranchSharingPopulationConfig ->
  BranchSharingFixture ->
  IO BranchPopulationState
initialBranchPopulation config fixture = do
  roots <-
    traverse
      (spawnRootBranch fixture)
      [1 .. bspRootBranches config]
  let rootMap =
        Map.fromList [(btId timeline, timeline) | timeline <- roots]
      nestedIds =
        [bspRootBranches config + 1 .. bspRootBranches config + bspNestedBranches config]
  nested <-
    traverse
      (spawnNestedFromRoots fixture rootMap)
      (zip [0 :: Int ..] nestedIds)
  let active =
        rootMap <> Map.fromList [(btId timeline, timeline) | timeline <- nested]
  pure
    BranchPopulationState
      { bpsFixture = fixture,
        bpsNextId = bspRootBranches config + bspNestedBranches config + 1,
        bpsActive = active
      }

spawnNestedFromRoots ::
  BranchSharingFixture ->
  Map Int BranchTimeline ->
  (Int, Int) ->
  IO BranchTimeline
spawnNestedFromRoots fixture roots (ordinal, childId) = do
  parent <-
    expectJust
      ("missing root parent for nested branch " <> show childId)
      (Map.lookup parentId roots)
  spawnNestedBranch fixture childId parent
  where
    parentId =
      1 + ordinal `rem` max 1 (Map.size roots)

advanceBranchPopulation ::
  Int ->
  BranchPopulationState ->
  IO BranchPopulationState
advanceBranchPopulation steps state0
  | steps <= 0 =
      pure state0
  | otherwise = do
      foldM
        (\stateValue _step -> turnoverBranchPopulation stateValue)
        state0
        [1 .. steps]

turnoverBranchPopulation :: BranchPopulationState -> IO BranchPopulationState
turnoverBranchPopulation state0 = do
  let (activeAfterRetire, maybeParent) =
        retireAndSelectParent (bpsActive state0)
      newId =
        bpsNextId state0
  timeline <-
    case maybeParent of
      Just parent
        | newId `rem` 4 == 0 ->
            spawnNestedBranch (bpsFixture state0) newId parent
      _ ->
        spawnRootBranch (bpsFixture state0) newId
  pure
    state0
      { bpsNextId = newId + 1,
        bpsActive = Map.insert newId timeline activeAfterRetire
      }

retireAndSelectParent :: Map Int BranchTimeline -> (Map Int BranchTimeline, Maybe BranchTimeline)
retireAndSelectParent active =
  case Map.minViewWithKey active of
    Nothing ->
      (Map.empty, Nothing)
    Just ((_retiredId, _retired), remaining) ->
      (remaining, selectNestedParent remaining)

selectNestedParent :: Map Int BranchTimeline -> Maybe BranchTimeline
selectNestedParent active =
  case List.find ((<= 1) . length . btAncestorIds) candidates of
    Just parent ->
      Just parent
    Nothing ->
      listHeadMaybe candidates
  where
    candidates =
      Map.elems active

spawnRootBranch :: BranchSharingFixture -> Int -> IO BranchTimeline
spawnRootBranch fixture branchId = do
  let eventTime =
        branchTimeLike (bsfTargetSnapshot fixture) branchId [branchId]
  runtime <-
    expectRight $
      fmap fst $
      commitCarrierDeltas
        [branchDeltaLike (bsfTargetSnapshot fixture) eventTime branchId]
        (bsfBase fixture)
  pure
    BranchTimeline
      { btId = branchId,
        btAncestorIds = [branchId],
        btTime = eventTime,
        btRuntime = runtime
      }

spawnNestedBranch :: BranchSharingFixture -> Int -> BranchTimeline -> IO BranchTimeline
spawnNestedBranch fixture branchId parent = do
  let ancestorIds =
        btAncestorIds parent <> [branchId]
      eventTime =
        branchTimeLike (bsfTargetSnapshot fixture) branchId ancestorIds
  runtime <-
    expectRight $
      fmap fst $
      commitCarrierDeltas
        [branchDeltaLike (bsfTargetSnapshot fixture) eventTime branchId]
        (btRuntime parent)
  pure
    BranchTimeline
      { btId = branchId,
        btAncestorIds = ancestorIds,
        btTime = eventTime,
        btRuntime = runtime
      }

validateBranchPopulation :: BranchPopulationState -> IO BranchSharingPopulationReport
validateBranchPopulation stateValue = do
  let fixture =
        bpsFixture stateValue
      timelines =
        Map.elems (bpsActive stateValue)
      generatedIds =
        Set.unions (fmap (Set.fromList . btAncestorIds) timelines)
      generatedRows =
        Map.fromList
          [ (branchId, branchRowForSnapshot (bsfTargetSnapshot fixture) branchId)
          | branchId <- Set.toAscList generatedIds
          ]

  assertBool
    "expected active branch population"
    (not (null timelines))

  assertSnapshotRowsAbsent
    "base target"
    (bsfTargetAddr fixture)
    (Map.elems generatedRows)
    (bsfBase fixture)

  traverse_
    (assertTimelineRows (bsfTargetAddr fixture) generatedRows)
    timelines

  baseNames <-
    snapshotStableNameMap (bsfAnchorAddrs fixture) (bsfBase fixture)

  branchNameMaps <-
    traverse
      (snapshotStableNameMap (bsfAnchorAddrs fixture) . btRuntime)
      timelines

  let distinctAnchorNames =
        distinctStableNameCount (concatMap Map.elems (baseNames : branchNameMaps))

  assertStableSharing baseNames branchNameMaps
  distinctAnchorNames @?= length (bsfAnchorAddrs fixture)

  pure
    BranchSharingPopulationReport
      { bsprActiveTimelines = length timelines,
        bsprAnchorSnapshots = length (bsfAnchorAddrs fixture),
        bsprGeneratedRowsChecked = Map.size generatedRows,
        bsprDistinctAnchorStableNames = distinctAnchorNames
      }

currentCarrierAddrs :: TestRuntime -> [Addr]
currentCarrierAddrs runtime =
  Set.toAscList $
    IntMap.foldl'
      ( \acc store ->
          Set.union (carrierCurrentAddresses store) acc
      )
      Set.empty
      (runtimeIndexStores runtime)
{-# INLINE currentCarrierAddrs #-}

lookupCurrentSnapshotRaw :: Addr -> TestRuntime -> Maybe Snapshot
lookupCurrentSnapshotRaw addr runtime =
  asum
    [ lookupCarrierSnapshot addr store
    | store <- IntMap.elems (runtimeIndexStores runtime)
    ]
{-# INLINE lookupCurrentSnapshotRaw #-}

snapshotStableNameMap ::
  [Addr] ->
  TestRuntime ->
  IO (Map Addr (StableName Snapshot))
snapshotStableNameMap addrs runtime =
  Map.fromList
    <$> traverse
      ( \addr -> do
          snapshot <-
            expectJust
              ("missing raw current snapshot at " <> show addr)
              (lookupCurrentSnapshotRaw addr runtime)
          stableName <-
            makeStableName snapshot
          pure (addr, stableName)
      )
      addrs

assertStableSharing ::
  Map Addr (StableName Snapshot) ->
  [Map Addr (StableName Snapshot)] ->
  Assertion
assertStableSharing baseNames branchNameMaps =
  traverse_
    (assertBranchStableSharing baseNames)
    (zip [1 :: Int ..] branchNameMaps)
{-# INLINE assertStableSharing #-}

assertBranchStableSharing ::
  Map Addr (StableName Snapshot) ->
  (Int, Map Addr (StableName Snapshot)) ->
  Assertion
assertBranchStableSharing baseNames (branchOrdinal, branchNames) =
  traverse_
    assertAddrShared
    (Map.toAscList baseNames)
  where
    assertAddrShared (addr, baseName) =
      case Map.lookup addr branchNames of
        Nothing ->
          assertFailure $
            "branch "
              <> show branchOrdinal
              <> " lost unmodified snapshot at "
              <> show addr
        Just branchName ->
          assertBool
            ( "branch "
                <> show branchOrdinal
                <> " duplicated unmodified snapshot at "
                <> show addr
            )
            (branchName == baseName)
{-# INLINE assertBranchStableSharing #-}

distinctStableNameCount :: [StableName value] -> Int
distinctStableNameCount =
  length . List.foldl' insertDistinct []
  where
    insertDistinct :: Eq value => [value] -> value -> [value]
    insertDistinct names name
      | any (== name) names =
          names
      | otherwise =
          name : names
{-# INLINE distinctStableNameCount #-}

assertTimelineRows ::
  Addr ->
  Map Int RowTupleKey ->
  BranchTimeline ->
  Assertion
assertTimelineRows targetAddr branchRows timeline = do
  snapshot <-
    expectCurrentSnapshot
      ("branch " <> show (btId timeline) <> " target")
      targetAddr
      (btRuntime timeline)

  let actualRows =
        rowMapOf snapshot
      actualScopePath =
        runtimeScopePath (relationalTimeScope (deTime snapshot))
      expectedIds =
        Set.fromList (btAncestorIds timeline)
      expectedScopePath =
        reverse (btAncestorIds timeline)
      expectedRows =
        [ row
        | (branchId, row) <- Map.toAscList branchRows,
          branchId `Set.member` expectedIds
        ]
      foreignRows =
        [ row
        | (branchId, row) <- Map.toAscList branchRows,
          not (branchId `Set.member` expectedIds)
        ]

  deTime snapshot @?= btTime timeline
  actualScopePath @?= expectedScopePath

  traverse_
    (\row -> Map.lookup row actualRows @?= Just (Multiplicity 1))
    expectedRows

  traverse_
    (\row -> Map.lookup row actualRows @?= Nothing)
    foreignRows
{-# INLINE assertTimelineRows #-}

assertSnapshotRowsAbsent ::
  String ->
  Addr ->
  [RowTupleKey] ->
  TestRuntime ->
  Assertion
assertSnapshotRowsAbsent label addr rows runtime = do
  snapshot <-
    expectCurrentSnapshot label addr runtime

  let actualRows =
        rowMapOf snapshot

  traverse_
    ( \row ->
        Map.lookup row actualRows @?= Nothing
    )
    rows
{-# INLINE assertSnapshotRowsAbsent #-}

expectCurrentSnapshot ::
  String ->
  Addr ->
  TestRuntime ->
  IO Delta
expectCurrentSnapshot label addr runtime = do
  maybeSnapshot <-
    expectRight
      (currentSnapshot addr runtime :: Either RuntimeErr (Maybe Delta))

  expectJust
    (label <> " current snapshot missing at " <> show addr)
    maybeSnapshot
{-# INLINE expectCurrentSnapshot #-}

rowMapOf :: Delta -> Map RowTupleKey Multiplicity
rowMapOf =
  positivePlainRowPatchRows . rowsOf
{-# INLINE rowMapOf #-}

branchDeltaLike :: Delta -> RelationalCarrierTime Ctx -> Int -> Delta
branchDeltaLike template eventTime branchId =
  template
    { deTime =
        eventTime,
      deRows =
        plainRowPatchFromList
          [ (branchRowForSnapshot template branchId, MultiplicityChange 1)
          ]
    }
{-# INLINE branchDeltaLike #-}

branchTimeLike ::
  Delta ->
  Int ->
  [Int] ->
  RelationalCarrierTime Ctx
branchTimeLike template branchId ancestorIds =
  List.foldl'
    (flip enterRuntimeTimeScope)
    ( carrierTime
        (caContext (deAddr template))
        (branchStamp branchId)
    )
    ancestorIds
{-# INLINE branchTimeLike #-}

branchStamp :: Int -> Word64
branchStamp branchId =
  100000 + fromIntegral branchId
{-# INLINE branchStamp #-}

branchRowForSnapshot :: Delta -> Int -> RowTupleKey
branchRowForSnapshot snapshot =
  branchRow (snapshotWidth snapshot)
{-# INLINE branchRowForSnapshot #-}

snapshotWidth :: Delta -> Int
snapshotWidth =
  length . bsSchema . boundaryShape . deBoundary
{-# INLINE snapshotWidth #-}

branchRow :: Int -> Int -> RowTupleKey
branchRow width branchId =
  tupleKeyFromRepKeys
    [ RepKey (branchSeed + ordinal)
    | ordinal <- [0 .. width - 1]
    ]
  where
    branchSeed =
      1000000 + branchId * 4099
{-# INLINE branchRow #-}

listHeadMaybe :: [value] -> Maybe value
listHeadMaybe values =
  case values of
    [] ->
      Nothing
    firstValue : _ ->
      Just firstValue
{-# INLINE listHeadMaybe #-}

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure (show errorValue) *> fail "expected Right"
    Right value ->
      pure value
{-# INLINE expectRight #-}

expectJust :: String -> Maybe value -> IO value
expectJust message maybeValue =
  case maybeValue of
    Nothing ->
      assertFailure message *> fail "expected Just"
    Just value ->
      pure value
{-# INLINE expectJust #-}
