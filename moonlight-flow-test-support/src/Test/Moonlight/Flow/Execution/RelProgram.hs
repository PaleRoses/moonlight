{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Moonlight.Flow.Execution.RelProgram
  ( PinnedRows (..),
    RelAtom,
    RelExecutionError (..),
    RelProgram,
    RelProgramError (..),
    RowStats (..),
    assertMechanicalEquivalence,
    assertScaledStats,
    atom,
    generatedTriangleProgram,
    pinned,
    program,
    programDenseRows,
    programDenseSupport,
    programEquivalenceReport,
    programFactorRows,
    programFactorRunCached,
    programFactorSupportCached,
    programStorageExistsPinnedInView,
    programStorageSurface,
    programRawPlanShape,
    programRuntimeAtomInputs,
    programRuntimeAtomRows,
    programRuntimeAtoms,
    programPatch,
    programJoinShape,
    programRuntimePlan,
    programRuntimeSpec,
    programWithQueryId,
    programOracleRows,
    row,
    triangleExpectedStats,
    triangleProgram,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe
  ( isJust,
    mapMaybe,
    maybeToList,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseJoinPlan,
    DenseJoinPlanError,
    denseSourcesFromStorageView,
    mkDenseJoinPlan,
  )
import Moonlight.Flow.Execution.Dense.WCOJ
  ( denseJoinRows,
    denseJoinSupportIds,
  )
import Moonlight.Flow.Execution.Factor.Enumerate
  ( enumerateBagRows,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
    runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorDemand (..),
    FactorCache,
    factorInputFromStoreView,
    FactorRunResult (..),
    FactorRunSpec (..),
    emptyFactorCache,
  )
import Moonlight.Flow.Plan.Physical.Meta
  ( decompFromJoinForest,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
  )
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    PreparedResult (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    runPrepared,
    supportIds,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Physical.Validate
  ( validateJoinMeta,
  )
import Moonlight.Flow.Plan.Shape.Build
  ( queryPlanToPlanShape,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (RawLogical),
  )
import Moonlight.Flow.Patch
  ( Patch,
    PatchError,
  )
import Moonlight.Flow.Patch qualified as Patch
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramError,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtom,
    RuntimePlan,
    RuntimeSpec,
    runtimeAtom,
    runtimeContextSchema,
    runtimeInitialData,
    runtimePlanWithDecomp,
    runtimeSchema,
    runtimeSpec,
    withInitialData,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.View
import Moonlight.Flow.Storage.Restriction
import Moonlight.Flow.Storage.Store
import Moonlight.Differential.Index.RowSet
  ( rowSetFromIntSetCanonical,
    rowSetToList,
    rowSetUnion,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    (@?=),
  )

newtype TestRow = TestRow
  { unTestRow :: RowTupleKey
  }
  deriving stock (Eq, Ord, Show)

instance QueryOutput TestRow Int where
  type OutputVar TestRow Int = ()

  data OutputRecipe TestRow Int = TestRowRecipe

  mkOutputRecipe _ =
    TestRowRecipe

  projectOutputRecipe TestRowRecipe _root bindingValues =
    Right (TestRow (tupleKeyFromInts (Vector.toList bindingValues)))

type TestPlan = QueryPlan () TestRow () () () Int

data RelAtom = RelAtom
  { raId :: !AtomId,
    raColumns :: ![SlotId],
    raRows :: ![[Int]]
  }
  deriving stock (Eq, Show)

data PinnedRows = PinnedRows
  { pinnedAtom :: !AtomId,
    pinnedHit :: !RowTupleKey,
    pinnedMiss :: !RowTupleKey
  }
  deriving stock (Eq, Show)

data RelProgram = RelProgram
  { rpName :: !String,
    rpDigest :: !Word64,
    rpRoot :: !SlotId,
    rpAtoms :: ![RelAtom],
    rpPinned :: !(Maybe PinnedRows)
  }
  deriving stock (Eq, Show)

data RowStats = RowStats
  { rsCount :: !Int,
    rsChecksum :: !Int
  }
  deriving stock (Eq, Show)

data ProgramRun = ProgramRun
  { prPlan :: !TestPlan,
    prStore :: !Store,
    prView :: !View,
    prSchema :: ![SlotId],
    prAtomSchemas :: !(IntMap [SlotId])
  }

data RelProgramError
  = RelProgramPlanInvalid [PlanBuild.QueryPlanError]
  | RelProgramRowBuildInvalid !RowBuildError
  | RelProgramRelationBuildInvalid !RelationPatchError
  | RelProgramDenseJoinPlanInvalid !DenseJoinPlanError
  deriving stock (Eq, Show)

data RelExecutionError
  = RelExecutionBuild !RelProgramError
  | RelExecutionNotFactorizable
  | RelExecutionRuntimeProgram !FactorProgramError
  | RelExecutionPatchInvalid !PatchError
  | RelExecutionFactorRun !FactorRunError
  deriving stock (Show)

atom :: Int -> [Int] -> [[Int]] -> RelAtom
atom atomKey columns rowsValue =
  RelAtom
    { raId = mkAtomId atomKey,
      raColumns = fmap mkSlotId columns,
      raRows = rowsValue
    }

row :: [Int] -> RowTupleKey
row =
  tupleKeyFromInts

pinned :: Int -> [Int] -> [Int] -> PinnedRows
pinned atomKey hit miss =
  PinnedRows
    { pinnedAtom = mkAtomId atomKey,
      pinnedHit = row hit,
      pinnedMiss = row miss
    }

program :: String -> Int -> [RelAtom] -> Maybe PinnedRows -> RelProgram
program name rootSlot atomsValue pinnedRows =
  RelProgram
    { rpName = name,
      rpDigest = programNameDigest name,
      rpRoot = mkSlotId rootSlot,
      rpAtoms = atomsValue,
      rpPinned = pinnedRows
    }

programWithQueryId :: String -> QueryId -> Int -> [RelAtom] -> Maybe PinnedRows -> RelProgram
programWithQueryId name queryId rootSlot atomsValue pinnedRows =
  RelProgram
    { rpName = name,
      rpDigest = fromIntegral (queryIdKey queryId),
      rpRoot = mkSlotId rootSlot,
      rpAtoms = atomsValue,
      rpPinned = pinnedRows
    }

triangleProgram :: String -> Int -> Int -> RelProgram
triangleProgram name rootCount fanout =
  program
    name
    0
    [ atom 0 [0, 1] (triangleABRows rootCount fanout),
      atom 1 [1, 2] (triangleBCRows rootCount fanout),
      atom 2 [0, 2] (triangleACRows rootCount fanout)
    ]
    ( Just
        PinnedRows
          { pinnedAtom = mkAtomId 0,
            pinnedHit = row [triangleA 1, triangleB 1 1],
            pinnedMiss = row [triangleA 1, triangleB 1 (fanout + 1)]
          }
    )

generatedTriangleProgram :: [[Int]] -> [[Int]] -> [[Int]] -> RelProgram
generatedTriangleProgram abRows bcRows acRows =
  program
    "generated-triangle"
    0
    [ atom 0 [0, 1] abRows,
      atom 1 [1, 2] bcRows,
      atom 2 [0, 2] acRows
    ]
    Nothing

assertMechanicalEquivalence :: RelProgram -> Assertion
assertMechanicalEquivalence relProgram =
  assertRight (runProgram relProgram) $ \programRun@ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} -> do
    validateJoinMeta (qpJoinMeta prPlan) @?= Right ()
    let expectedRows = Set.fromList (bruteForceRows prAtomSchemas prSchema prStore prView)
        expectedSupport = normalizeSupportIds (bruteForceSupportIds prAtomSchemas prSchema prStore prView)
        meta = qpJoinMeta prPlan
    fmap Set.fromList (denseRowsFor meta prSchema prStore prView) @?= Right expectedRows
    assertRight (runPreparedRows prPlan emptyRestriction prStore prView) $ \preparedRows ->
      Set.fromList preparedRows @?= expectedRows
    fmap normalizeSupportIds (denseSupportFor meta prSchema prStore prView) @?= Right expectedSupport
    assertRight (runPreparedSupportIds prPlan emptyRestriction prStore prView) $ \preparedSupport ->
      normalizeSupportIds preparedSupport @?= expectedSupport
    assertFactorizedRows programRun expectedRows
    assertFactorizedSupport programRun expectedSupport
    Foldable.traverse_ (assertPinnedRows programRun) (rpPinned relProgram)

assertScaledStats :: RelProgram -> RowStats -> Assertion
assertScaledStats relProgram expected =
  assertRight (runProgram relProgram) $ \ProgramRun {prPlan, prStore, prView, prSchema} -> do
    let meta = qpJoinMeta prPlan
    fmap rowStats (denseRowsFor meta prSchema prStore prView) @?= Right expected
    assertRight (runPreparedRows prPlan emptyRestriction prStore prView) $ \preparedRows ->
      rowStats preparedRows @?= expected

programEquivalenceReport :: RelProgram -> Either String ()
programEquivalenceReport relProgram = do
  programRun@ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} <- first show (runProgram relProgram)
  let expected = Set.fromList (bruteForceRows prAtomSchemas prSchema prStore prView)
      prepared = fmap Set.fromList (runPreparedRows prPlan emptyRestriction prStore prView)
      dense = fmap Set.fromList (denseRowsFor (qpJoinMeta prPlan) prSchema prStore prView)
      factorized = fmap Set.fromList (factorRows programRun)
  if prepared == Right expected && dense == Right expected && factorized == Right expected
    then Right ()
    else
      Left
        ( "expected="
            <> show expected
            <> "\nprepared="
            <> show prepared
            <> "\ndense="
            <> show dense
            <> "\nfactorized="
            <> show factorized
        )

triangleExpectedStats :: Int -> Int -> RowStats
triangleExpectedStats rootCount fanout =
  let roots = max 0 rootCount
      width = max 0 fanout
      rootSum = arithmeticSum roots
      widthSum = arithmeticSum width
      widthSquared = width * width
      aPart = widthSquared * (triangleStride * rootSum + roots)
      bPart = width * (width * (triangleStride * rootSum + 100 * roots) + roots * widthSum)
      cPart = width * (width * (triangleStride * rootSum + 10000 * roots) + roots * widthSum)
   in RowStats
        { rsCount = roots * widthSquared,
          rsChecksum = aPart + bPart + cPart
        }

runProgram :: RelProgram -> Either RelProgramError ProgramRun
runProgram relProgram = do
  planValue <- programPlan relProgram
  databaseValue <- first RelProgramRowBuildInvalid (programDatabase relProgram)
  relationsValue <- first RelProgramRelationBuildInvalid (traverse relationFromAtomRows databaseValue)
  let storeValue = storeFromRelations relationsValue
      viewValue = unrestrictedView
      schemaValue = programSchema relProgram
      atomSchemasValue = IntMap.fromList (programAtomSchemas relProgram)
  pure
    ProgramRun
      { prPlan = planValue,
        prStore = storeValue,
        prView = viewValue,
        prSchema = schemaValue,
        prAtomSchemas = atomSchemasValue
      }

programStorageSurface :: RelProgram -> Either RelProgramError (Store, View)
programStorageSurface =
  fmap (\ProgramRun {prStore, prView} -> (prStore, prView)) . runProgram

programRawPlanShape :: RelProgram -> Either RelProgramError (PlanShape 'RawLogical)
programRawPlanShape =
  fmap (queryPlanToPlanShape . prPlan) . runProgram

programJoinShape :: RelProgram -> Either RelProgramError JoinShape
programJoinShape =
  fmap (jmShape . qpJoinMeta . prPlan) . runProgram

programDenseRows :: RelProgram -> Either RelProgramError [RowTupleKey]
programDenseRows relProgram = do
  ProgramRun {prPlan, prStore, prView, prSchema} <- runProgram relProgram
  first RelProgramDenseJoinPlanInvalid (denseRowsFor (qpJoinMeta prPlan) prSchema prStore prView)

programOracleRows :: RelProgram -> Either RelProgramError [RowTupleKey]
programOracleRows relProgram = do
  ProgramRun {prStore, prView, prSchema, prAtomSchemas} <- runProgram relProgram
  pure (bruteForceRows prAtomSchemas prSchema prStore prView)

programStorageExistsPinnedInView :: RelProgram -> Store -> View -> Int -> [Int] -> Either RelExecutionError Bool
programStorageExistsPinnedInView relProgram store view atomKey rowValue = do
  plan <- first RelExecutionBuild (programPlan relProgram)
  first RelExecutionFactorRun (runPreparedExistsPinned plan (mkAtomId atomKey) (row rowValue) store view)

programDenseSupport :: RelProgram -> Either RelProgramError SupportIds
programDenseSupport relProgram = do
  ProgramRun {prPlan, prStore, prView, prSchema} <- runProgram relProgram
  first RelProgramDenseJoinPlanInvalid (denseSupportFor (qpJoinMeta prPlan) prSchema prStore prView)

programFactorRows :: RelProgram -> Either RelExecutionError [RowTupleKey]
programFactorRows relProgram = do
  programRun <- first RelExecutionBuild (runProgram relProgram)
  first RelExecutionFactorRun (factorRows programRun)

programRuntimeAtoms :: RelProgram -> [RuntimeAtom ctx prop]
programRuntimeAtoms =
  fmap fst . programRuntimeAtomRows

programRuntimeAtomInputs :: RelProgram -> [(AtomId, [SlotId], [RowTupleKey])]
programRuntimeAtomInputs =
  fmap atomInput . rpAtoms
  where
    atomInput :: RelAtom -> (AtomId, [SlotId], [RowTupleKey])
    atomInput RelAtom {raId, raColumns, raRows} =
      (raId, raColumns, fmap row raRows)
{-# INLINE programRuntimeAtomInputs #-}

programRuntimeAtomRows :: RelProgram -> [(RuntimeAtom ctx prop, [RowTupleKey])]
programRuntimeAtomRows =
  fmap atomRows . rpAtoms
  where
    atomRows :: RelAtom -> (RuntimeAtom ctx prop, [RowTupleKey])
    atomRows RelAtom {raId, raColumns, raRows} =
      (runtimeAtom raId raColumns, fmap row raRows)

programPatch :: RelProgram -> Either RelExecutionError Patch
programPatch =
  first RelExecutionPatchInvalid
    . fmap Patch.patch
    . traverse (uncurry Patch.insert)
    . programRuntimeAtomRows

programRuntimePlan ::
  ctx ->
  prop ->
  RelProgram ->
  Either RelExecutionError (RuntimePlan ctx prop)
programRuntimePlan contextValue propValue relProgram = do
  ProgramRun {prPlan, prSchema, prAtomSchemas} <- first RelExecutionBuild (runProgram relProgram)
  decomp <- maybe (Left RelExecutionNotFactorizable) Right (factorDecompFor prPlan prAtomSchemas prSchema)
  first
    RelExecutionRuntimeProgram
    (runtimePlanWithDecomp contextValue (PropositionKey propValue) prPlan decomp)

programRuntimeSpec ::
  (Ord ctx, Ord prop) =>
  ctx ->
  prop ->
  RelProgram ->
  Either RelExecutionError (RuntimeSpec ctx prop)
programRuntimeSpec contextValue propValue relProgram = do
  planValue <- programRuntimePlan contextValue propValue relProgram
  patchValue <- programPatch relProgram
  pure
    ( withInitialData
        (runtimeInitialData patchValue)
        ( runtimeSpec
            ( runtimeSchema
                [ ( contextValue,
                    runtimeContextSchema
                      (programRuntimeAtoms relProgram)
                      [PropositionKey propValue]
                  )
                ]
            )
            [planValue]
        )
    )

programFactorRunCached :: FactorCache -> RelProgram -> Either RelExecutionError (FactorRunResult SupportIds)
programFactorRunCached cache relProgram = do
  ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} <- first RelExecutionBuild (runProgram relProgram)
  decomp <- maybe (Left RelExecutionNotFactorizable) Right (factorDecompFor prPlan prAtomSchemas prSchema)
  first RelExecutionFactorRun (runFactorSupport cache decomp prStore prView)

programFactorSupportCached :: FactorCache -> RelProgram -> Either RelExecutionError (FactorCache, SupportIds)
programFactorSupportCached cache relProgram = do
  result <- programFactorRunCached cache relProgram
  pure (frrCache result, frrSupport result)

programPlan :: RelProgram -> Either RelProgramError TestPlan
programPlan relProgram =
  first RelProgramPlanInvalid $
    PlanBuild.mkQueryPlan
      ( PlanBuild.QueryPlanInput
          { PlanBuild.qpiDomain = PlanBuild.StructuralQueryPlan,
            PlanBuild.qpiCompiled = (),
            PlanBuild.qpiDigest = programDigest relProgram,
            PlanBuild.qpiAtoms = Vector.fromList (fmap atomSpec (rpAtoms relProgram)),
            PlanBuild.qpiSchemaOrder = Just (Vector.fromList (programSchema relProgram)),
            PlanBuild.qpiRootSlot = rpRoot relProgram,
            PlanBuild.qpiOutputs = fmap (`PlanBuild.PlanOutputBinding` ()) (programSchema relProgram),
            PlanBuild.qpiResidual = PlanBuild.NoQueryPlanResidual
          }
      )

atomSpec :: RelAtom -> AtomSpec () () Int
atomSpec RelAtom {raId, raColumns} =
  mkAtomSpec
    (mkQueryAtomId (atomIdKey raId))
    (mkSourceAtomId raId)
    ()
    0
    (Vector.fromList raColumns)
    (mkStalkRecipe (Vector.replicate (length raColumns) []))

programDatabase :: RelProgram -> Either RowBuildError (IntMap (RowBlock 'Canonical))
programDatabase =
  fmap IntMap.fromList . traverse relationEntry . rpAtoms
  where
    relationEntry RelAtom {raId, raColumns, raRows} =
      fmap
        (\relation -> (atomIdKey raId, relation))
        ( atomRowsFromTupleKeys
          (rowBlockIdentityForAtom 0 0 0 raId 0)
          (Vector.fromList raColumns)
          (fmap row raRows)
        )

programSchema :: RelProgram -> [SlotId]
programSchema =
  fmap mkSlotId
    . IntSet.toAscList
    . Foldable.foldMap (IntSet.fromList . fmap slotIdKey . raColumns)
    . rpAtoms

programAtomSchemas :: RelProgram -> [(Int, [SlotId])]
programAtomSchemas =
  fmap (\RelAtom {raId, raColumns} -> (atomIdKey raId, raColumns)) . rpAtoms

programDigest :: RelProgram -> Word64
programDigest =
  rpDigest

programNameDigest :: String -> Word64
programNameDigest =
  List.foldl'
    (\acc ch -> acc * 167 + fromIntegral (fromEnum ch))
    2166136261

runPreparedRows :: TestPlan -> Restriction -> Store -> View -> Either FactorRunError [RowTupleKey]
runPreparedRows plan restriction store view =
  runPreparedValue plan restriction store view (PreparedRows Nothing)

runPreparedSupportIds :: TestPlan -> Restriction -> Store -> View -> Either FactorRunError SupportIds
runPreparedSupportIds plan restriction store view =
  supportIds <$> runPreparedValue plan restriction store view PreparedSupport

runPreparedExistsPinned :: TestPlan -> AtomId -> RowTupleKey -> Store -> View -> Either FactorRunError Bool
runPreparedExistsPinned plan atomId rowValue store view =
  runPreparedValue plan emptyRestriction store view (PreparedExistsPinned atomId rowValue)

runPreparedValue :: TestPlan -> Restriction -> Store -> View -> PreparedOp a -> Either FactorRunError a
runPreparedValue plan restriction store view op =
  prValue
    <$> runPrepared
      PreparedRunSpec
        { prsPlan = plan,
          prsRestriction = restriction,
          prsStore = store,
          prsView = view,
          prsAtomDeltas = IntMap.empty,
          prsStructuralSources = Nothing,
          prsOp = op,
          prsMode = PreparedValueOnly
        }

denseStoragePinnedExists :: TestPlan -> [SlotId] -> AtomId -> RowTupleKey -> Store -> View -> Either DenseJoinPlanError Bool
denseStoragePinnedExists plan schema atomId rowValue store =
  fmap supportAllRelationsFeasible
    . denseSupportFor (qpJoinMeta plan) schema store
    . applyRestriction (restrictPinnedRow atomId rowValue) store

denseStoragePlanFor :: JoinMeta -> [SlotId] -> Store -> View -> Either DenseJoinPlanError DenseJoinPlan
denseStoragePlanFor meta schema store view =
  mkDenseJoinPlan schema schema (denseSourcesFromStorageView meta store view)

denseRowsFor :: JoinMeta -> [SlotId] -> Store -> View -> Either DenseJoinPlanError [RowTupleKey]
denseRowsFor meta schema store =
  fmap denseJoinRows . denseStoragePlanFor meta schema store

denseSupportFor :: JoinMeta -> [SlotId] -> Store -> View -> Either DenseJoinPlanError SupportIds
denseSupportFor meta schema store =
  fmap denseJoinSupportIds . denseStoragePlanFor meta schema store

assertFactorizedRows :: ProgramRun -> Set.Set RowTupleKey -> Assertion
assertFactorizedRows programRun expected =
  assertRight (factorRows programRun) $ \rowsValue ->
    Set.fromList rowsValue @?= expected

assertFactorizedSupport :: ProgramRun -> SupportIds -> Assertion
assertFactorizedSupport ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} expected =
  Foldable.traverse_
    ( \decomp ->
        fmap (normalizeSupportIds . frrSupport) (runFactorSupport emptyFactorCache decomp prStore prView)
          @?= Right expected
    )
    (factorDecompFor prPlan prAtomSchemas prSchema)

factorRows :: ProgramRun -> Either FactorRunError [RowTupleKey]
factorRows ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} =
  case factorDecompFor prPlan prAtomSchemas prSchema of
    Nothing ->
      Right []
    Just decomp ->
      runFactorRows emptyFactorCache prSchema decomp prStore prView

runFactorRows :: FactorCache -> [SlotId] -> DecompPlan -> Store -> View -> Either FactorRunError [RowTupleKey]
runFactorRows cache schema decomp store view =
  fmap
    (enumerateBagRows schema decomp . frrPreSealCache)
    (runFactorWithInput decomp store view mempty cache FactorDemandRows)

runFactorSupport :: FactorCache -> DecompPlan -> Store -> View -> Either FactorRunError (FactorRunResult SupportIds)
runFactorSupport cache decomp store view =
  runFactorWithInput decomp store view mempty cache FactorDemandSupport

runFactorWithInput ::
  DecompPlan ->
  Store ->
  View ->
  IntMap RowDelta ->
  FactorCache ->
  FactorDemand support ->
  Either FactorRunError (FactorRunResult support)
runFactorWithInput decomp store view atomDeltas cache demand =
  runFactor
    FactorRunSpec
      { frsDecomp = decomp,
        frsInput =
          factorInputFromStoreView store view atomDeltas,
        frsCache = cache,
        frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
        frsDemand = demand
      }

factorDecompFor :: TestPlan -> IntMap [SlotId] -> [SlotId] -> Maybe DecompPlan
factorDecompFor plan atomSchemas schema =
  foldJoinShape
    (Just (exactDenseDecomp atomSchemas schema))
    (Just . (`decompFromJoinForest` atomSchemas))
    Just
    (jmShape (qpJoinMeta plan))

exactDenseDecomp :: IntMap [SlotId] -> [SlotId] -> DecompPlan
exactDenseDecomp atomSchemas schema =
  mkDecompPlan
    rootBag
    (IntMap.singleton rootBagKey rootBagPlan)
    IntMap.empty
    IntMap.empty
    mempty
    (IntMap.map (const rootBag) atomSchemas)
  where
    rootBagKey =
      0

    rootBag =
      BagId rootBagKey

    rootBagPlan =
      mkDecompBag
        rootBag
        schema
        (IntMap.keysSet atomSchemas)

assertPinnedRows :: ProgramRun -> PinnedRows -> Assertion
assertPinnedRows ProgramRun {prPlan, prStore, prView, prSchema, prAtomSchemas} PinnedRows {pinnedAtom, pinnedHit, pinnedMiss} = do
  assertPinned pinnedHit
  assertPinned pinnedMiss
  where
    oracle rowValue =
      not
        . null
        $ bruteForceRows
          prAtomSchemas
          prSchema
          prStore
          (applyRestriction (restrictPinnedRow pinnedAtom rowValue) prStore prView)

    assertPinned rowValue = do
      let expected = oracle rowValue
      denseStoragePinnedExists prPlan prSchema pinnedAtom rowValue prStore prView @?= Right expected
      runPreparedExistsPinned prPlan pinnedAtom rowValue prStore prView @?= Right expected

bruteForceRows :: IntMap [SlotId] -> [SlotId] -> Store -> View -> [RowTupleKey]
bruteForceRows atomSchemas fullSchema store view =
  mapMaybe
    (\(BruteWitness env _) -> envToAtomRow fullSchema env)
    (bruteWitnesses atomSchemas store view)

bruteForceSupportIds :: IntMap [SlotId] -> [SlotId] -> Store -> View -> SupportIds
bruteForceSupportIds atomSchemas fullSchema store view =
  IntMap.unionsWith rowSetUnion $
    fmap
      (\(BruteWitness _ support) -> support)
      ( filter
          (\(BruteWitness env _) -> isJust (envToAtomRow fullSchema env))
          (bruteWitnesses atomSchemas store view)
      )

type Env = IntMap RepKey

data BruteWitness = BruteWitness !Env !SupportIds

bruteWitnesses :: IntMap [SlotId] -> Store -> View -> [BruteWitness]
bruteWitnesses atomSchemas store view =
  List.foldl'
    (expandWitnesses store view)
    [BruteWitness IntMap.empty IntMap.empty]
    (IntMap.toAscList atomSchemas)

expandWitnesses :: Store -> View -> [BruteWitness] -> (Int, [SlotId]) -> [BruteWitness]
expandWitnesses store view witnesses (atomKey, schema) =
  witnesses >>= expandWitness store view atomKey schema

expandWitness :: Store -> View -> Int -> [SlotId] -> BruteWitness -> [BruteWitness]
expandWitness store view atomKey schema (BruteWitness env support) = do
  relation <- maybeToList (IntMap.lookup atomKey (storeRelations store))
  rowId <- rowSetToList (viewRows store view atomKey)
  let rowKey =
        rowIdInt rowId
  rowValue <- maybeToList (rowForId relation rowId)
  envValue <- maybeToList (extendEnvWithRow schema rowValue env)
  let supportValue =
        IntMap.insertWith
          rowSetUnion
          atomKey
          (rowSetFromIntSetCanonical (IntSet.singleton rowKey))
          support
  pure (BruteWitness envValue supportValue)

extendEnvWithRow :: [SlotId] -> RowTupleKey -> Env -> Maybe Env
extendEnvWithRow schema rowValue env
  | length schema /= length values = Nothing
  | otherwise = foldM insertOne env (zip schema values)
  where
    values = tupleKeyToRepKeys rowValue

    insertOne :: Env -> (SlotId, RepKey) -> Maybe Env
    insertOne envValue (sid, value) =
      case IntMap.lookup (slotIdKey sid) envValue of
        Nothing -> Just (IntMap.insert (slotIdKey sid) value envValue)
        Just existing
          | existing == value -> Just envValue
          | otherwise -> Nothing

envToAtomRow :: [SlotId] -> Env -> Maybe RowTupleKey
envToAtomRow schema env =
  tupleKeyFromRepKeys <$> traverse (\sid -> IntMap.lookup (slotIdKey sid) env) schema

rowStats :: Foldable rows => rows RowTupleKey -> RowStats
rowStats =
  Foldable.foldl'
    (\stats rowValue -> RowStats (rsCount stats + 1) (rsChecksum stats + rowChecksum rowValue))
    (RowStats 0 0)

rowChecksum :: RowTupleKey -> Int
rowChecksum =
  tupleKeyFoldlInts' (+) 0

triangleABRows :: Int -> Int -> [[Int]]
triangleABRows rootCount fanout =
  concatMap
    (\rootKey -> fmap (\bKey -> [triangleA rootKey, triangleB rootKey bKey]) [1 .. fanout])
    [1 .. rootCount]

triangleBCRows :: Int -> Int -> [[Int]]
triangleBCRows rootCount fanout =
  concatMap
    (\rootKey -> concatMap (\bKey -> fmap (\cKey -> [triangleB rootKey bKey, triangleC rootKey cKey]) [1 .. fanout]) [1 .. fanout])
    [1 .. rootCount]

triangleACRows :: Int -> Int -> [[Int]]
triangleACRows rootCount fanout =
  concatMap
    (\rootKey -> fmap (\cKey -> [triangleA rootKey, triangleC rootKey cKey]) [1 .. fanout])
    [1 .. rootCount]

triangleA :: Int -> Int
triangleA rootKey =
  rootKey * triangleStride + 1

triangleB :: Int -> Int -> Int
triangleB rootKey bKey =
  rootKey * triangleStride + 100 + bKey

triangleC :: Int -> Int -> Int
triangleC rootKey cKey =
  rootKey * triangleStride + 10000 + cKey

triangleStride :: Int
triangleStride = 100000

arithmeticSum :: Int -> Int
arithmeticSum n =
  n * (n + 1) `quot` 2

assertRight :: Show left => Either left right -> (right -> Assertion) -> Assertion
assertRight eitherValue action =
  case eitherValue of
    Left err -> assertFailure (show err)
    Right right -> action right
