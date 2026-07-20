module Test.Moonlight.Flow.Property.Execution
  ( matchCompletenessVsOracle,
    matchSoundnessVsOracle,
    matchMultipathConsistency,
    wideExactFallbackPreparedParity,
    denseProjectedFoldAggregatesDuplicates,
    denseSupportProjectedFoldStreamsDuplicateLeaves,
    denseSelectedOutputDomainRejectsCrossProductLeaves,
    preparedProvenanceSumsPreserveCellBindings,
    provenanceSupportMemoRowEstimateTracksEvaluation,
    provenanceSupportMemoRemapPreservesRowEstimate,
    provenanceSupportMemoWrongScopeIsFreshMiss,
    provenanceMarkingHandlesDeepAlternatingChain,
    contributionIndexPreservesSemanticAdjacency,
    contributionIndexEqualityIgnoresInternalContributionIds,
    incrementalJoinTelemetryCountsDuplicateWitnessReplay,
    incrementalFusedDeltaWCOJSharesMultiDirtyTraversal,
    incrementalFusedDeltaWCOJOwnsOverlappingDirtyLeafOnce,
    incrementalRegionRepairDeletesAbsentProjection,
    incrementalRegionRepairUsesContributionIndexForHiddenDuplicateDeletion,
    incrementalScalarRepairUsesContributionIndexForDeletion,
    boundedPessimismWorkMargin,
    structuralCutoffPredictionsHold,
    executionProperties,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    DenseJoinPlanError,
    SourceBundle (..),
    denseAtomSource,
  )
import Moonlight.Flow.Execution.Dense.WCOJ
  ( DenseLeafWitness (..),
    foldProjectDenseWCOJ,
    foldProjectDenseWCOJSelectedWitnesses,
    foldProjectDenseWCOJWitnesses,
    joinProjectDenseWCOJ,
  )
import Moonlight.Flow.Execution.Factor.Incremental
  ( IncrementalUpdateTrace (..),
    buildFactorFromSourceBundles,
    updateFactorIncremental,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
    factorDeltaFromCellPatches,
  )
import Moonlight.Flow.Execution.Factor.Contribution
  ( FactorContribution (..),
    FactorContributionIndex,
    FactorSourceCell (..),
    emptyFactorContributionIndex,
    factorContributionIndexSupportKeysForSourceCells,
    insertFactorContribution,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvId,
    ProvNode (..),
    ProvVal (..),
    ProvenanceObstruction,
    emptyProvArena
  )
import Moonlight.Flow.Execution.Observe.Provenance.Arena
  ( nodeAt,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Args
  ( provArgsLength,
    provArgsToIds,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Value
  ( pvAtom,
    pvPlus,
    pvTimes
  )
import Moonlight.Flow.Execution.Observe.Provenance.Support
  ( ProvSupport,
    ProvSupportEvalStats (..),
    emptyProvSupportMemo,
    provSupportMemoRowEstimate,
    evalProvSupportWithMemo,
    pruneProvSupportMemo,
    remapProvSupportMemo,
    validateProvSupportMemo
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( markProvRoots,
    compactProvArena,
    remapProvVal
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
    RepKey,
    RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToRepKeys,
  )
import Moonlight.Flow.Plan.Query.Core
  ( AtomId,
    SlotId,
    atomIdKey,
    exactJoinShape,
    mkAtomId,
    mkSlotId,
    slotIdKey,
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Store
  ( storeFromRelations,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
  )
import Test.Moonlight.Flow.Execution.BoundedPessimism
  ( boundedPessimismWorkGuarantee,
  )
import Test.Moonlight.Flow.Execution.StructuralCutoff
  ( structuralCutoffWitness,
  )
import Test.Moonlight.Flow.Execution.RelProgram
  ( RelProgram,
    assertMechanicalEquivalence,
    atom,
    pinned,
    program,
    programDenseRows,
    programEquivalenceReport,
    programFactorRows,
    programJoinShape,
  )
import Test.Moonlight.Flow.Gen.Execution
  ( genPathProgram,
    genTriangleProgram,
  )
import Test.Moonlight.Flow.Oracle.Execution
  ( oracleRows,
  )
import Test.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    forAll,
    once,
    property,
    (===),
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
import Test.Tasty.QuickCheck
  ( testProperty,
  )

data DenseSourceBuildError
  = DenseSourceRowBuild !RowBuildError
  | DenseSourceRelationBuild !RelationPatchError
  deriving stock (Eq, Show)

data PreparedCellFixture
  = ProjectedCellAlternatives
  | JoinedCellAlternatives
  deriving stock (Eq, Ord, Show)

data PreparedSourceFixture = PreparedSourceFixture
  { psfAtomKey :: !Int,
    psfSchema :: ![SlotId],
    psfRows :: ![RowTupleKey]
  }
  deriving stock (Eq, Show)

data ProvenanceBindingRole
  = EmittedCellBinding
  | AtomRowBinding !AtomId
  deriving stock (Eq, Show)

type CellAtomRows = IntMap.IntMap (Set.Set RowTupleKey)

data ProvenanceCellLawError
  = ProvenanceCellFixtureSourceInvalid !PreparedCellFixture !Int !DenseSourceBuildError
  | ProvenanceCellDenseJoinPlanInvalid !DenseJoinPlanError
  | ProvenanceCellArityMismatch !ProvenanceBindingRole !Int !Int
  | ProvenanceCellArenaInvalid !ProvenanceObstruction
  | ProvenanceCellCycle !ProvId
  | ProvenanceCellAtomSchemaMissing !AtomId
  | ProvenanceCellBindingDisagreement !AtomId !SlotId !RepKey !RepKey
  | ProvenanceCellEvaluatorDisagreement !AssignmentTupleKey !CellAtomRows !CellAtomRows
  deriving stock (Eq, Show)

assertRight :: Show obstruction => Either obstruction value -> (value -> Assertion) -> Assertion
assertRight result assertion =
  either (assertFailure . show) assertion result

data ProvenanceInspection = ProvenanceInspection
  { piAtomRows :: !CellAtomRows,
    piSumNodes :: !Int,
    piNonVacuousSumNodes :: !Int
  }
  deriving stock (Eq, Show)

instance Semigroup ProvenanceInspection where
  left <> right =
    ProvenanceInspection
      { piAtomRows = IntMap.unionWith Set.union (piAtomRows left) (piAtomRows right),
        piSumNodes = piSumNodes left + piSumNodes right,
        piNonVacuousSumNodes =
          piNonVacuousSumNodes left + piNonVacuousSumNodes right
      }

instance Monoid ProvenanceInspection where
  mempty =
    ProvenanceInspection
      { piAtomRows = IntMap.empty,
        piSumNodes = 0,
        piNonVacuousSumNodes = 0
      }

expectedFactorDelta ::
  [SlotId] ->
  Factor ->
  Factor ->
  FactorDelta
expectedFactorDelta outputSchema oldFactor newFactor =
  factorDeltaFromCellPatches outputSchema $
    expectedFactorCellPatches
      (indexedRowsPayloadMap oldFactor)
      (indexedRowsPayloadMap newFactor)
{-# INLINE expectedFactorDelta #-}

expectedFactorCellPatches ::
  Map.Map AssignmentTupleKey ProvVal ->
  Map.Map AssignmentTupleKey ProvVal ->
  Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal)
expectedFactorCellPatches oldRows newRows =
  Map.mergeWithKey
    changedCell
    deletedCells
    insertedCells
    oldRows
    newRows
  where
    changedCell ::
      AssignmentTupleKey ->
      ProvVal ->
      ProvVal ->
      Maybe (CorePatch.CellPatch ProvVal)
    changedCell _key oldValue newValue
      | oldValue == newValue =
          Nothing
      | otherwise =
          Just (CorePatch.replace oldValue newValue)

    deletedCells ::
      Map.Map AssignmentTupleKey ProvVal ->
      Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal)
    deletedCells =
      Map.map CorePatch.delete

    insertedCells ::
      Map.Map AssignmentTupleKey ProvVal ->
      Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal)
    insertedCells =
      Map.map CorePatch.insert
{-# INLINE expectedFactorCellPatches #-}

-- Proves semantic-surface invariant: dense/prepared/factorized outputs are
-- complete versus bounded brute-force enumeration.
matchCompletenessVsOracle :: Property
matchCompletenessVsOracle =
  forAll genTriangleProgram $ \relProgram ->
    case (oracleRows relProgram, programDenseRows relProgram, programFactorRows relProgram) of
      (Right oracle, Right denseRows, Right factorRows) ->
        let oracleSet =
              Set.fromList oracle
         in conjoin
              [ Set.isSubsetOf oracleSet (Set.fromList denseRows) === True,
                Set.isSubsetOf oracleSet (Set.fromList factorRows) === True
              ]
      other ->
        counterexample (show other) (property False)

-- Proves semantic-surface invariant: dense/prepared/factorized outputs are
-- sound versus bounded brute-force enumeration.
matchSoundnessVsOracle :: Property
matchSoundnessVsOracle =
  forAll genTriangleProgram $ \relProgram ->
    case (oracleRows relProgram, programDenseRows relProgram, programFactorRows relProgram) of
      (Right oracle, Right denseRows, Right factorRows) ->
        let oracleSet =
              Set.fromList oracle
         in conjoin
              [ Set.isSubsetOf (Set.fromList denseRows) oracleSet === True,
                Set.isSubsetOf (Set.fromList factorRows) oracleSet === True
              ]
      other ->
        counterexample (show other) (property False)

-- Proves semantic-surface invariant: existing multipath consistency remains but
-- is now subordinate to the oracle properties above.
matchMultipathConsistency :: Property
matchMultipathConsistency =
  forAll genPathProgram $ \relProgram ->
    case programEquivalenceReport relProgram of
      Right () ->
        property True
      Left report ->
        counterexample report False

wideExactFallbackPreparedParity :: Assertion
wideExactFallbackPreparedParity = do
  assertEqual
    "wide cyclic query must force the ExactJoin fallback"
    (Right exactJoinShape)
    (programJoinShape wideExactFallbackProgram)
  assertMechanicalEquivalence wideExactFallbackProgram

wideExactFallbackProgram :: RelProgram
wideExactFallbackProgram =
  program
    "wide-exact-fallback"
    0
    [ atom atomKey [leftSlot, rightSlot] [[1, 1], [2, 2]]
      | (atomKey, (leftSlot, rightSlot)) <- zip [0 ..] wideExactFallbackPairs
    ]
    (Just (pinned 0 [1, 1] [1, 2]))

wideExactFallbackPairs :: [(Int, Int)]
wideExactFallbackPairs =
  [(leftSlot, rightSlot) | leftSlot <- [0 .. 8], rightSlot <- [leftSlot + 1 .. 8]]

denseProjectedFoldAggregatesDuplicates :: Assertion
denseProjectedFoldAggregatesDuplicates =
  case denseSource
      0
      [slotProjected, slotHidden]
      [ row [projectedValue, hiddenLeft],
        row [projectedValue, hiddenRight]
      ] of
    Left err ->
      assertFailure (show err)
    Right source ->
      let outputSchema =
            [slotProjected]
          expectedKeys =
            Set.singleton (assignmentKey [projectedValue])
          projectedResults = do
            (_arenaFolded, foldedRows) <-
              foldProjectDenseWCOJ
                outputSchema
                [source]
                emptyProvArena
                Map.empty
                ( \key value arena rows ->
                    case Map.lookup key rows of
                      Nothing ->
                        (arena, Map.insert key value rows)
                      Just oldValue ->
                        let (arena1, mergedValue) =
                              pvPlus oldValue value arena
                         in (arena1, Map.insert key mergedValue rows)
                )
            (_arenaFactor, factor) <-
              joinProjectDenseWCOJ outputSchema [source] emptyProvArena
            pure (foldedRows, factor)
       in assertRight projectedResults $ \(foldedRows, factor) -> do
            assertEqual
              "folded projected keys"
              expectedKeys
              (Map.keysSet foldedRows)
            assertEqual
              "foldProjectDenseWCOJ must expose the same pvPlus-aggregated rows as joinProjectDenseWCOJ"
              (indexedRowsPayloadMap factor)
              foldedRows

denseSupportProjectedFoldStreamsDuplicateLeaves :: Assertion
denseSupportProjectedFoldStreamsDuplicateLeaves =
  case denseSource
      0
      [slotProjected, slotHidden]
      [ row [projectedValue, hiddenLeft],
        row [projectedValue, hiddenRight]
      ] of
    Left err ->
      assertFailure (show err)
    Right source ->
      let outputSchema =
            [slotProjected]
          sources =
            [source]
          streamedResult =
            foldProjectDenseWCOJWitnesses
              outputSchema
              sources
              emptyProvArena
              []
              ( \key witness arena cells ->
                  pure
                    ( arena,
                      (key, dlwSupportCells witness) : cells
                    )
              )
          projectedKey =
            assignmentKey [projectedValue]
          expectedSupport =
            Set.fromList
              [ FactorSourceCell
                  { fscSourceId = 0,
                    fscKey = assignmentKey [projectedValue, hiddenLeft]
                  },
                FactorSourceCell
                  { fscSourceId = 0,
                    fscKey = assignmentKey [projectedValue, hiddenRight]
                  }
              ]
       in assertRight streamedResult $ \(_arenaFolded, streamedCells) -> do
            assertEqual
              "support-aware projected fold streams one cell per WCOJ leaf"
              2
              (length streamedCells)
            assertEqual
              "streamed duplicate leaves retain the projected output key"
              (Set.singleton projectedKey)
              (Set.fromList (fmap fst streamedCells))
            assertEqual
              "streamed duplicate leaves retain source-cell support without a projected-cell map"
              expectedSupport
              (Set.unions (fmap snd streamedCells))

denseSelectedOutputDomainRejectsCrossProductLeaves :: Assertion
denseSelectedOutputDomainRejectsCrossProductLeaves =
  case denseSource
      0
      [slotProjected, slotHidden]
      [ row [projectedValue, hiddenLeft],
        row [projectedValue, hiddenRight],
        row [tenantValue, hiddenLeft],
        row [tenantValue, hiddenRight]
      ] of
    Left err ->
      assertFailure (show err)
    Right source ->
      let outputSchema =
            [slotProjected, slotHidden]
          selectedKeys =
            Set.fromList
              [ assignmentKey [projectedValue, hiddenLeft],
                assignmentKey [tenantValue, hiddenRight]
              ]
          streamedResult =
            foldProjectDenseWCOJSelectedWitnesses
              defaultRepairTelemetryConfig
              outputSchema
              selectedKeys
              [source]
              emptyProvArena
              []
              (\key _witness arena keys -> pure (arena, key : keys))
       in assertRight streamedResult $ \(_arenaFolded, streamedKeys) ->
            assertEqual
              "selected-output domain must reject per-slot cross-product leaves"
              selectedKeys
              (Set.fromList streamedKeys)

-- The factor builder is the prepared evaluator's unique provenance-cell
-- constructor.  These fixtures cover both a projected atom and a projected
-- join, so the walk reaches sums whose arms are leaves and sums whose arms are
-- products.  Every atom below every sum must restrict to the emitted binding.
preparedProvenanceSumsPreserveCellBindings :: Property
preparedProvenanceSumsPreserveCellBindings =
  once $
    case traverse inspectPreparedCellFixture allPreparedCellFixtures of
      Left obstruction ->
        counterexample (show obstruction) (property False)
      Right inspections ->
        let inspection =
              mconcat inspections
         in counterexample
              "prepared provenance fixtures must exercise a sum with at least two distinct atom rows"
              (property (piNonVacuousSumNodes inspection > 0))

allPreparedCellFixtures :: [PreparedCellFixture]
allPreparedCellFixtures =
  [ ProjectedCellAlternatives,
    JoinedCellAlternatives
  ]

preparedCellFixtureSpec ::
  PreparedCellFixture ->
  ([SlotId], [PreparedSourceFixture])
preparedCellFixtureSpec fixture =
  case fixture of
    ProjectedCellAlternatives ->
      ( [slotProjected],
        [ PreparedSourceFixture
            { psfAtomKey = 0,
              psfSchema = [slotProjected, slotHidden],
              psfRows =
                [ row [projectedValue, hiddenLeft],
                  row [projectedValue, hiddenRight]
                ]
            }
        ]
      )
    JoinedCellAlternatives ->
      ( [slotTenant, slotRole],
        [ PreparedSourceFixture
            { psfAtomKey = 0,
              psfSchema = [slotTenant, slotRole],
              psfRows =
                [ row [tenantValue, survivingRole],
                  row [tenantValue, deletedRole]
                ]
            },
          PreparedSourceFixture
            { psfAtomKey = 1,
              psfSchema = [slotTenant, slotHidden],
              psfRows =
                [ row [tenantValue, hiddenLeft],
                  row [tenantValue, hiddenRight]
                ]
            }
        ]
      )

inspectPreparedCellFixture ::
  PreparedCellFixture ->
  Either ProvenanceCellLawError ProvenanceInspection
inspectPreparedCellFixture fixture = do
  sources <- traverse buildSource sourceFixtures
  let atomSchemas =
        IntMap.fromList
          [ (psfAtomKey sourceFixture, psfSchema sourceFixture)
            | sourceFixture <- sourceFixtures
          ]
      bundles =
        fmap (`SourceBundle` Set.empty) sources
  (arena, factor, _contributions) <-
    first ProvenanceCellDenseJoinPlanInvalid
      (buildFactorFromSourceBundles outputSchema bundles emptyProvArena)
  mconcat
    <$> traverse
      (inspectProvenanceCell atomSchemas outputSchema arena)
      (Map.toAscList (indexedRowsPayloadMap factor))
  where
    (outputSchema, sourceFixtures) =
      preparedCellFixtureSpec fixture

    buildSource sourceFixture =
      first
        (ProvenanceCellFixtureSourceInvalid fixture (psfAtomKey sourceFixture))
        ( denseSource
            (psfAtomKey sourceFixture)
            (psfSchema sourceFixture)
            (psfRows sourceFixture)
        )

inspectProvenanceCell ::
  IntMap.IntMap [SlotId] ->
  [SlotId] ->
  ProvArena ->
  (AssignmentTupleKey, ProvVal) ->
  Either ProvenanceCellLawError ProvenanceInspection
inspectProvenanceCell atomSchemas outputSchema arena (outputKey, rootValue) = do
  outputBinding <-
    provenanceBinding
      EmittedCellBinding
      outputSchema
      (tupleKeyToRepKeys outputKey)
  inspection <-
    inspectProvenanceValue atomSchemas outputBinding arena Set.empty rootValue
  validateCellAtomRows atomSchemas outputBinding (piAtomRows inspection)
  (evaluatedSupport, _memo, _stats) <-
    first ProvenanceCellArenaInvalid
      (evalProvSupportWithMemo arena rootValue emptyProvSupportMemo)
  let evaluatedAtomRows =
        normalizeProvSupport evaluatedSupport
  if piAtomRows inspection == evaluatedAtomRows
    then Right inspection
    else
      Left
        ( ProvenanceCellEvaluatorDisagreement
            outputKey
            (piAtomRows inspection)
            evaluatedAtomRows
        )

inspectProvenanceValue ::
  IntMap.IntMap [SlotId] ->
  IntMap.IntMap RepKey ->
  ProvArena ->
  Set.Set ProvId ->
  ProvVal ->
  Either ProvenanceCellLawError ProvenanceInspection
inspectProvenanceValue atomSchemas outputBinding arena active value =
  case value of
    PVZero ->
      Right mempty
    PVOne ->
      Right mempty
    PVObstructed obstruction ->
      Left (ProvenanceCellArenaInvalid obstruction)
    PVRef provId ->
      inspectProvenanceRef atomSchemas outputBinding arena active provId

inspectProvenanceRef ::
  IntMap.IntMap [SlotId] ->
  IntMap.IntMap RepKey ->
  ProvArena ->
  Set.Set ProvId ->
  ProvId ->
  Either ProvenanceCellLawError ProvenanceInspection
inspectProvenanceRef atomSchemas outputBinding arena active provId
  | Set.member provId active =
      Left (ProvenanceCellCycle provId)
  | otherwise = do
      node <-
        first ProvenanceCellArenaInvalid (nodeAt arena provId)
      inspectProvenanceNode
        atomSchemas
        outputBinding
        arena
        (Set.insert provId active)
        node

inspectProvenanceNode ::
  IntMap.IntMap [SlotId] ->
  IntMap.IntMap RepKey ->
  ProvArena ->
  Set.Set ProvId ->
  ProvNode ->
  Either ProvenanceCellLawError ProvenanceInspection
inspectProvenanceNode atomSchemas outputBinding arena active node =
  case node of
    PNAtom atomId atomRow ->
      Right
        mempty
          { piAtomRows =
              IntMap.singleton (atomIdKey atomId) (Set.singleton atomRow)
          }
    PNSum arguments -> do
      inspection <-
        mconcat
          <$> traverse
            (inspectProvenanceRef atomSchemas outputBinding arena active)
            (provArgsToIds arguments)
      validateCellAtomRows atomSchemas outputBinding (piAtomRows inspection)
      let nonVacuousCount =
            if provArgsLength arguments >= 2 && provenanceAtomRowCount (piAtomRows inspection) >= 2
              then 1
              else 0
      Right
        inspection
          { piSumNodes = piSumNodes inspection + 1,
            piNonVacuousSumNodes =
              piNonVacuousSumNodes inspection + nonVacuousCount
          }
    PNProd arguments ->
      mconcat
        <$> traverse
          (inspectProvenanceRef atomSchemas outputBinding arena active)
          (provArgsToIds arguments)

validateCellAtomRows ::
  IntMap.IntMap [SlotId] ->
  IntMap.IntMap RepKey ->
  CellAtomRows ->
  Either ProvenanceCellLawError ()
validateCellAtomRows atomSchemas outputBinding atomRows =
  traverse_ validateAtomRows (IntMap.toAscList atomRows)
  where
    validateAtomRows (rawAtomId, rowsValue) = do
      let atomId =
            mkAtomId rawAtomId
      atomSchema <-
        maybe
          (Left (ProvenanceCellAtomSchemaMissing atomId))
          Right
          (IntMap.lookup rawAtomId atomSchemas)
      traverse_
        (validateAtomRow atomId atomSchema)
        (Set.toAscList rowsValue)

    validateAtomRow atomId atomSchema atomRow = do
      atomBinding <-
        provenanceBinding
          (AtomRowBinding atomId)
          atomSchema
          (tupleKeyToRepKeys atomRow)
      traverse_
        (validateSharedSlot atomId atomBinding)
        (IntMap.toAscList outputBinding)

    validateSharedSlot atomId atomBinding (slotKey, outputValue) =
      case IntMap.lookup slotKey atomBinding of
        Nothing ->
          Right ()
        Just atomValue
          | atomValue == outputValue ->
              Right ()
          | otherwise ->
              Left
                ( ProvenanceCellBindingDisagreement
                    atomId
                    (mkSlotId slotKey)
                    outputValue
                    atomValue
                )

provenanceBinding ::
  ProvenanceBindingRole ->
  [SlotId] ->
  [RepKey] ->
  Either ProvenanceCellLawError (IntMap.IntMap RepKey)
provenanceBinding role schema values
  | length schema == length values =
      Right
        ( IntMap.fromList
            [ (slotIdKey slotId, value)
              | (slotId, value) <- zip schema values
            ]
        )
  | otherwise =
      Left
        ( ProvenanceCellArityMismatch
            role
            (length schema)
            (length values)
        )

normalizeProvSupport :: ProvSupport -> CellAtomRows
normalizeProvSupport =
  fmap (Set.fromList . HashSet.toList)

provenanceAtomRowCount :: CellAtomRows -> Int
provenanceAtomRowCount =
  IntMap.foldl' (\total rowsValue -> total + Set.size rowsValue) 0

provenanceSupportMemoRowEstimateTracksEvaluation :: Assertion
provenanceSupportMemoRowEstimateTracksEvaluation =
  case evalProvSupportWithMemo supportMemoArena supportMemoRoot emptyProvSupportMemo of
    Left obstruction ->
      assertFailure (show obstruction)
    Right (support, memo, _stats) -> do
      assertEqual
        "support memo row estimate equals freshly evaluated support rows"
        (supportRowCount support)
        (provSupportMemoRowEstimate memo)
      assertEqual
        "support memo validates after evaluation"
        (Right ())
        (validateProvSupportMemo supportMemoArena memo)

provenanceSupportMemoRemapPreservesRowEstimate :: Assertion
provenanceSupportMemoRemapPreservesRowEstimate =
  case evalProvSupportWithMemo supportMemoArena supportMemoRoot emptyProvSupportMemo of
    Left obstruction ->
      assertFailure (show obstruction)
    Right (_support, memo0, _stats) ->
      let !memo1 =
            pruneProvSupportMemo [supportMemoRoot] supportMemoArena memo0
          !rowEstimate0 =
            provSupportMemoRowEstimate memo1
       in case compactProvArena [supportMemoRoot] supportMemoArena of
            Left obstruction ->
              assertFailure (show obstruction)
            Right (arena1, remap, _gcStats) ->
              case remapProvSupportMemo supportMemoArena arena1 remap memo1 of
                Left obstruction ->
                  assertFailure (show obstruction)
                Right memo2 -> do
                  assertEqual
                    "support memo row estimate survives compaction remap"
                    rowEstimate0
                    (provSupportMemoRowEstimate memo2)
                  assertEqual
                    "remapped support memo validates against compacted arena"
                    (Right ())
                    (validateProvSupportMemo arena1 memo2)

provenanceSupportMemoWrongScopeIsFreshMiss :: Assertion
provenanceSupportMemoWrongScopeIsFreshMiss =
  case evalProvSupportWithMemo supportMemoArena supportMemoRoot emptyProvSupportMemo of
    Left obstruction ->
      assertFailure (show obstruction)
    Right (support0, memo0, _stats0) ->
      case compactProvArena [supportMemoRoot] supportMemoArena of
        Left obstruction ->
          assertFailure (show obstruction)
        Right (arena1, remap, _gcStats) ->
          case remapProvVal remap supportMemoRoot of
            Left obstruction ->
              assertFailure (show obstruction)
            Right root1 ->
              case evalProvSupportWithMemo arena1 root1 memo0 of
                Left obstruction ->
                  assertFailure (show obstruction)
                Right (support1, memo1, stats1) -> do
                  assertEqual
                    "stale pre-compaction memo must not change compacted support"
                    support0
                    support1
                  assertEqual
                    "stale pre-compaction memo must not be trusted after scope changes"
                    0
                    (pseMemoHits stats1)
                  assertEqual
                    "fresh memo validates against compacted arena after stale input miss"
                    (Right ())
                    (validateProvSupportMemo arena1 memo1)

provenanceMarkingHandlesDeepAlternatingChain :: Assertion
provenanceMarkingHandlesDeepAlternatingChain =
  case markProvRoots deepMarkArena [deepMarkRoot] of
    Left obstruction ->
      assertFailure (show obstruction)
    Right marked ->
      assertEqual
        "deep alternating provenance chain marks every reachable atom and composite"
        (2 * deepMarkDepth + 1)
        (IntSet.size marked)

contributionIndexPreservesSemanticAdjacency :: Assertion
contributionIndexPreservesSemanticAdjacency = do
  let sourceCellA =
        FactorSourceCell
          { fscSourceId = 0,
            fscKey = assignmentKey [projectedValue, hiddenLeft]
          }
      sourceCellB =
        FactorSourceCell
          { fscSourceId = 0,
            fscKey = assignmentKey [projectedValue, hiddenRight]
          }
      outputA =
        assignmentKey [projectedValue]
      outputB =
        assignmentKey [tenantValue]
      contributionFor sourceCell =
        FactorContribution
          { fctValue = PVOne,
            fctSupportCells = Set.singleton sourceCell
          }
      (_arena0, base, _change0) =
        foldl'
          ( \(!arena, !indexValue, !_change) (outputKey, sourceCell) ->
              insertFactorContribution
                defaultRepairTelemetryConfig
                outputKey
                (contributionFor sourceCell)
                arena
                indexValue
          )
          (emptyProvArena, emptyFactorContributionIndex, mempty)
          [ (outputB, sourceCellA),
            (outputA, sourceCellB)
          ]
  assertEqual
    "patch drops only the removed output edge for the shared source cell"
    (Set.singleton outputB)
    (factorContributionIndexSupportKeysForSourceCells (Set.singleton sourceCellA) base)
  assertEqual
    "patch keeps the selected output edge for the surviving source cell"
    (Set.singleton outputA)
    (factorContributionIndexSupportKeysForSourceCells (Set.singleton sourceCellB) base)

contributionIndexEqualityIgnoresInternalContributionIds :: Assertion
contributionIndexEqualityIgnoresInternalContributionIds = do
  let sourceCellA =
        FactorSourceCell
          { fscSourceId = 0,
            fscKey = assignmentKey [tenantValue, projectedValue]
          }
      sourceCellB =
        FactorSourceCell
          { fscSourceId = 1,
            fscKey = assignmentKey [tenantValue, hiddenRight]
          }
      outputKey =
        assignmentKey [tenantValue]
      contributionFor sourceCell =
        FactorContribution
          { fctValue = PVOne,
            fctSupportCells = Set.singleton sourceCell
          }
      buildIndex :: [FactorSourceCell] -> FactorContributionIndex
      buildIndex orderedCells =
        let (_, indexValue, _) =
              foldl'
                ( \(!arena, !index0, !_change) sourceCell ->
                    insertFactorContribution
                      defaultRepairTelemetryConfig
                      outputKey
                      (contributionFor sourceCell)
                      arena
                      index0
                )
                (emptyProvArena, emptyFactorContributionIndex, mempty)
                orderedCells
         in indexValue
      leftIndex =
        buildIndex [sourceCellA, sourceCellB]
      rightIndex =
        buildIndex [sourceCellB, sourceCellA]
  assertEqual
    "semantic contribution-index equality is independent of internal contribution-id allocation order"
    leftIndex
    rightIndex

incrementalRegionRepairDeletesAbsentProjection :: Assertion
incrementalRegionRepairDeletesAbsentProjection =
  case
    (,,,)
      <$> denseSource 0 [slotTenant] [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [ row [tenantValue, survivingRole],
          row [tenantValue, deletedRole]
        ]
      <*> denseSource 0 [slotTenant] [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [row [tenantValue, survivingRole]]
    of
      Left err ->
        assertFailure (show err)
      Right (oldTenantSource, oldRoleSource, newTenantSource, newRoleSource) ->
        let outputSchema =
              [slotTenant, slotRole]
            oldBundles =
              [ SourceBundle oldTenantSource Set.empty,
                SourceBundle oldRoleSource Set.empty
              ]
            currentBundles =
              [ SourceBundle
                  newTenantSource
                  (Set.singleton (assignmentKey [tenantValue])),
                SourceBundle
                  newRoleSource
                  (Set.singleton (assignmentKey [tenantValue, deletedRole]))
              ]
            expectedKeys =
              Set.singleton (assignmentKey [tenantValue, survivingRole])
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles outputSchema oldBundles emptyProvArena
              (_arenaIncremental, incrementalFactor, _newContributions, _delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, _freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [ SourceBundle newTenantSource Set.empty,
                    SourceBundle newRoleSource Set.empty
                  ]
                  arenaOld
              pure (incrementalFactor, freshFactor, traceValue)
         in assertRight factorResults $ \(incrementalFactor, freshFactor, traceValue) -> do
              assertEqual
                "incremental factor rows must equal fresh rebuild after dirty-region deletion"
                (indexedRowsPayloadMap freshFactor)
                (indexedRowsPayloadMap incrementalFactor)
              assertEqual
                "deleted projection removed while sibling projection survives"
                expectedKeys
                (Map.keysSet (indexedRowsPayloadMap incrementalFactor))
              assertBool
                "expected non-zero affected keys"
                (iutAffectedKeys traceValue > 0)
              assertBool
                "expected non-zero recomputed cells"
                (iutRecomputedCells traceValue > 0)

incrementalJoinTelemetryCountsDuplicateWitnessReplay :: Assertion
incrementalJoinTelemetryCountsDuplicateWitnessReplay =
  case
    (,)
      <$> denseSource
        0
        [slotProjected, slotHidden]
        [row [projectedValue, hiddenLeft]]
      <*> denseSource
        0
        [slotProjected, slotHidden]
        [ row [projectedValue, hiddenLeft],
          row [projectedValue, hiddenRight]
        ]
    of
      Left err ->
        assertFailure (show err)
      Right (oldSource, newSource) ->
        let outputSchema =
              [slotProjected]
            oldBundles =
              [SourceBundle oldSource Set.empty]
            currentBundles =
              [ SourceBundle
                  newSource
                  (Set.singleton (assignmentKey [projectedValue, hiddenRight]))
              ]
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles outputSchema oldBundles emptyProvArena
              (_arenaIncremental, incrementalFactor, _newContributions, _delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, _freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [SourceBundle newSource Set.empty]
                  arenaOld
              pure (incrementalFactor, freshFactor, traceValue)
         in assertRight factorResults $ \(incrementalFactor, freshFactor, traceValue) -> do
              assertEqual
                "incremental factor rows must equal fresh rebuild after duplicate insertion"
                (indexedRowsPayloadMap freshFactor)
                (indexedRowsPayloadMap incrementalFactor)
              assertEqual
                "duplicate insertion recomputes one projected output cell"
                1
                (iutRecomputedCells traceValue)
              assertEqual
                "join telemetry must count only dirty delta WCOJ leaves"
                1
                (iutJoinLeaves traceValue)

incrementalFusedDeltaWCOJSharesMultiDirtyTraversal :: Assertion
incrementalFusedDeltaWCOJSharesMultiDirtyTraversal =
  case
    (,,,)
      <$> denseSource
        0
        [slotTenant]
        [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [row [tenantValue, survivingRole]]
      <*> denseSource
        0
        [slotTenant]
        [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [ row [tenantValue, survivingRole],
          row [tenantValue, deletedRole]
        ]
    of
      Left err ->
        assertFailure (show err)
      Right (oldTenantSource, oldRoleSource, newTenantSource, newRoleSource) ->
        let outputSchema =
              [slotTenant, slotRole]
            oldBundles =
              [ SourceBundle oldTenantSource Set.empty,
                SourceBundle oldRoleSource Set.empty
              ]
            tenantDirtyKey =
              assignmentKey [tenantValue]
            insertedRoleDirtyKey =
              assignmentKey [tenantValue, deletedRole]
            currentBundles =
              [ SourceBundle newTenantSource (Set.singleton tenantDirtyKey),
                SourceBundle newRoleSource (Set.singleton insertedRoleDirtyKey)
              ]
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles outputSchema oldBundles emptyProvArena
              (_arenaIncremental, incrementalFactor, newContributions, delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [ SourceBundle newTenantSource Set.empty,
                    SourceBundle newRoleSource Set.empty
                  ]
                  arenaOld
              pure
                ( oldFactor,
                  incrementalFactor,
                  newContributions,
                  delta,
                  traceValue,
                  freshFactor,
                  freshContributions
                )
         in assertRight factorResults $
              \(oldFactor, incrementalFactor, newContributions, delta, traceValue, freshFactor, freshContributions) -> do
                assertEqual
                  "fused dirty-aware WCOJ repair must equal fresh rebuild with two dirty sources"
                  (indexedRowsPayloadMap freshFactor)
                  (indexedRowsPayloadMap incrementalFactor)
                assertEqual
                  "fused dirty-aware WCOJ contribution index must equal fresh rebuild"
                  freshContributions
                  newContributions
                assertEqual
                  "fused dirty-aware WCOJ factor delta must match old-to-fresh cell transition"
                  (expectedFactorDelta outputSchema oldFactor freshFactor)
                  delta
                assertEqual
                  "two dirty sources must share one WCOJ traversal"
                  1
                  (iutJoinRuns traceValue)
                assertEqual
                  "multi-dirty repair emits one current witness per affected leaf"
                  2
                  (iutJoinLeaves traceValue)

incrementalFusedDeltaWCOJOwnsOverlappingDirtyLeafOnce :: Assertion
incrementalFusedDeltaWCOJOwnsOverlappingDirtyLeafOnce =
  case
    (,,,)
      <$> denseSource
        0
        [slotTenant]
        [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [row [tenantValue, survivingRole]]
      <*> denseSource
        0
        [slotTenant]
        [row [tenantValue]]
      <*> denseSource
        1
        [slotTenant, slotRole]
        [row [tenantValue, survivingRole]]
    of
      Left err ->
        assertFailure (show err)
      Right (oldTenantSource, oldRoleSource, newTenantSource, newRoleSource) ->
        let outputSchema =
              [slotTenant, slotRole]
            oldBundles =
              [ SourceBundle oldTenantSource Set.empty,
                SourceBundle oldRoleSource Set.empty
              ]
            currentBundles =
              [ SourceBundle newTenantSource (Set.singleton (assignmentKey [tenantValue])),
                SourceBundle newRoleSource (Set.singleton (assignmentKey [tenantValue, survivingRole]))
              ]
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles outputSchema oldBundles emptyProvArena
              (_arenaIncremental, incrementalFactor, newContributions, delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [ SourceBundle newTenantSource Set.empty,
                    SourceBundle newRoleSource Set.empty
                  ]
                  arenaOld
              pure
                ( oldFactor,
                  incrementalFactor,
                  newContributions,
                  delta,
                  traceValue,
                  freshFactor,
                  freshContributions
                )
         in assertRight factorResults $
              \(oldFactor, incrementalFactor, newContributions, delta, traceValue, freshFactor, freshContributions) -> do
                assertEqual
                  "overlapping dirty leaf repair must equal fresh rebuild"
                  (indexedRowsPayloadMap freshFactor)
                  (indexedRowsPayloadMap incrementalFactor)
                assertEqual
                  "overlapping dirty leaf contribution index must equal fresh rebuild"
                  freshContributions
                  newContributions
                assertEqual
                  "overlapping dirty leaf delta must match old-to-fresh cell transition"
                  (expectedFactorDelta outputSchema oldFactor freshFactor)
                  delta
                assertEqual
                  "overlapping dirty sources must share one WCOJ traversal"
                  1
                  (iutJoinRuns traceValue)
                assertEqual
                  "overlapping dirty sources emit one current witness for the leaf"
                  1
                  (iutJoinLeaves traceValue)

incrementalRegionRepairUsesContributionIndexForHiddenDuplicateDeletion :: Assertion
incrementalRegionRepairUsesContributionIndexForHiddenDuplicateDeletion =
  case
    (,)
      <$> denseSource
        0
        [slotProjected, slotHidden]
        [ row [projectedValue, hiddenLeft],
          row [projectedValue, hiddenRight]
        ]
      <*> denseSource
        0
        [slotProjected, slotHidden]
        [row [projectedValue, hiddenRight]]
    of
      Left err ->
        assertFailure (show err)
      Right (oldSource, newSource) ->
        let outputSchema =
              [slotProjected]
            projectedKey =
              assignmentKey [projectedValue]
            deletedSourceCell =
              FactorSourceCell
                { fscSourceId = 0,
                  fscKey = assignmentKey [projectedValue, hiddenLeft]
                }
            syntheticFilterCell =
              FactorSourceCell
                { fscSourceId = 1,
                  fscKey = projectedKey
                }
            currentBundles =
              [ SourceBundle
                  newSource
                  (Set.singleton (assignmentKey [projectedValue, hiddenLeft]))
              ]
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [SourceBundle oldSource Set.empty]
                  emptyProvArena
              (_arenaIncremental, incrementalFactor, newContributions, delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [SourceBundle newSource Set.empty]
                  arenaOld
              pure
                ( oldFactor,
                  oldContributions,
                  incrementalFactor,
                  newContributions,
                  delta,
                  traceValue,
                  freshFactor,
                  freshContributions
                )
         in assertRight factorResults $
              \(oldFactor, oldContributions, incrementalFactor, newContributions, delta, traceValue, freshFactor, freshContributions) -> do
                assertEqual
                  "FactorContributionIndex maps a deleted hidden source cell to its projected output"
                  (Set.singleton projectedKey)
                  (factorContributionIndexSupportKeysForSourceCells (Set.singleton deletedSourceCell) oldContributions)
                assertEqual
                  "contribution-index locality repair must equal fresh rebuild after hidden duplicate deletion"
                  (indexedRowsPayloadMap freshFactor)
                  (indexedRowsPayloadMap incrementalFactor)
                assertEqual
                  "contribution-index locality repair must leave final contribution index equal to fresh rebuild"
                  freshContributions
                  newContributions
                assertEqual
                  "contribution-index locality factor delta must match old-to-fresh cell transition"
                  (expectedFactorDelta outputSchema oldFactor freshFactor)
                  delta
                assertEqual
                  "selected-output filter cursor must not enter FactorContributionIndex"
                  Set.empty
                  (factorContributionIndexSupportKeysForSourceCells (Set.singleton syntheticFilterCell) newContributions)
                assertBool
                  "contribution-index locality repair touched the projected duplicate"
                  (iutAffectedKeys traceValue > 0 && iutRecomputedCells traceValue > 0)

incrementalScalarRepairUsesContributionIndexForDeletion :: Assertion
incrementalScalarRepairUsesContributionIndexForDeletion =
  case
    (,)
      <$> denseSource
        0
        [slotHidden]
        [ row [hiddenLeft],
          row [hiddenRight]
        ]
      <*> denseSource
        0
        [slotHidden]
        [row [hiddenRight]]
    of
      Left err ->
        assertFailure (show err)
      Right (oldSource, newSource) ->
        let outputSchema =
              [] :: [SlotId]
            scalarKey =
              assignmentKey []
            deletedSourceCell =
              FactorSourceCell
                { fscSourceId = 0,
                  fscKey = assignmentKey [hiddenLeft]
                }
            currentBundles =
              [ SourceBundle
                  newSource
                  (Set.singleton (assignmentKey [hiddenLeft]))
              ]
            factorResults = do
              (arenaOld, oldFactor, oldContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [SourceBundle oldSource Set.empty]
                  emptyProvArena
              (_arenaIncremental, incrementalFactor, newContributions, delta, traceValue) <-
                updateFactorIncremental
                  defaultRepairTelemetryConfig
                  outputSchema
                  currentBundles
                  oldFactor
                  oldContributions
                  arenaOld
              (_arenaFresh, freshFactor, freshContributions) <-
                buildFactorFromSourceBundles
                  outputSchema
                  [SourceBundle newSource Set.empty]
                  arenaOld
              pure
                ( oldFactor,
                  oldContributions,
                  incrementalFactor,
                  newContributions,
                  delta,
                  traceValue,
                  freshFactor,
                  freshContributions
                )
         in assertRight factorResults $
              \(oldFactor, oldContributions, incrementalFactor, newContributions, delta, traceValue, freshFactor, freshContributions) -> do
                assertEqual
                  "scalar output retains source support for deletion locality"
                  (Set.singleton scalarKey)
                  (factorContributionIndexSupportKeysForSourceCells (Set.singleton deletedSourceCell) oldContributions)
                assertEqual
                  "scalar delete fallback must still repair the root output"
                  (indexedRowsPayloadMap freshFactor)
                  (indexedRowsPayloadMap incrementalFactor)
                assertEqual
                  "scalar delete final contribution index must equal fresh rebuild"
                  freshContributions
                  newContributions
                assertEqual
                  "scalar delete factor delta must match old-to-fresh cell transition"
                  (expectedFactorDelta outputSchema oldFactor freshFactor)
                  delta
                assertEqual
                  "scalar delete recomputes exactly the root output cell"
                  1
                  (iutRecomputedCells traceValue)
                assertEqual
                  "scalar support drops the deleted source cell after repair"
                  Set.empty
                  (factorContributionIndexSupportKeysForSourceCells (Set.singleton deletedSourceCell) newContributions)

boundedPessimismWorkMargin :: Assertion
boundedPessimismWorkMargin =
  boundedPessimismWorkGuarantee

structuralCutoffPredictionsHold :: Assertion
structuralCutoffPredictionsHold =
  structuralCutoffWitness

executionProperties :: TestTree
executionProperties =
  testGroup
      "matching"
      [ testProperty "complete vs bounded brute oracle" matchCompletenessVsOracle,
        testProperty "sound vs bounded brute oracle" matchSoundnessVsOracle,
        testProperty "multipath consistency (prepared/dense/factor)" matchMultipathConsistency,
        testCase "wide exact fallback preserves prepared/factor parity" wideExactFallbackPreparedParity,
        testCase "dense projected fold aggregates duplicate projected rows" denseProjectedFoldAggregatesDuplicates,
        testCase "support-aware projected fold streams duplicate WCOJ leaves" denseSupportProjectedFoldStreamsDuplicateLeaves,
        testCase "selected output domain rejects cross-product leaves" denseSelectedOutputDomainRejectsCrossProductLeaves,
        testProperty "prepared provenance sums preserve emitted cell bindings" preparedProvenanceSumsPreserveCellBindings,
        testCase "provenance support memo row estimate tracks evaluation" provenanceSupportMemoRowEstimateTracksEvaluation,
        testCase "provenance support memo remap preserves row estimate" provenanceSupportMemoRemapPreservesRowEstimate,
        testCase "provenance support memo wrong scope is a fresh miss" provenanceSupportMemoWrongScopeIsFreshMiss,
        testCase "provenance marking handles deep alternating chain" provenanceMarkingHandlesDeepAlternatingChain,
        testCase "contribution index patch preserves semantic adjacency" contributionIndexPreservesSemanticAdjacency,
        testCase "contribution index equality ignores internal contribution ids" contributionIndexEqualityIgnoresInternalContributionIds,
        testCase "incremental join telemetry counts duplicate witness replay" incrementalJoinTelemetryCountsDuplicateWitnessReplay,
        testCase "incremental fused delta WCOJ shares two dirty sources" incrementalFusedDeltaWCOJSharesMultiDirtyTraversal,
        testCase "incremental fused delta WCOJ owns overlapping dirty leaf once" incrementalFusedDeltaWCOJOwnsOverlappingDirtyLeafOnce,
        testCase "incremental region repair deletes absent projection and preserves sibling" incrementalRegionRepairDeletesAbsentProjection,
        testCase "incremental region repair uses FactorContributionIndex for hidden duplicate deletion" incrementalRegionRepairUsesContributionIndexForHiddenDuplicateDeletion,
        testCase "incremental scalar repair uses FactorContributionIndex for deletion" incrementalScalarRepairUsesContributionIndexForDeletion,
        testCase "bounded pessimism work margin vs fresh factor reference" boundedPessimismWorkMargin,
        testCase "structural cutoff predictions hold per edit" structuralCutoffPredictionsHold
      ]

denseSource ::
  Int ->
  [SlotId] ->
  [RowTupleKey] ->
  Either DenseSourceBuildError DenseArrangement
denseSource atomKey schema rowsValue =
  let atomId =
        mkAtomId atomKey
   in do
        block <-
          first
            DenseSourceRowBuild
            ( atomRowsFromTupleKeys
                (rowBlockIdentityForAtom 0 0 0 atomId 0)
                (Vector.fromList schema)
                rowsValue
            )
        relation <-
          first DenseSourceRelationBuild (relationFromAtomRows block)
        pure
          ( denseAtomSource
              (DenseArrangementId atomKey)
              (storeFromRelations (IntMap.singleton atomKey relation))
              unrestrictedView
              atomId
          )

row :: [Int] -> RowTupleKey
row =
  tupleKeyFromInts

assignmentKey :: [Int] -> AssignmentTupleKey
assignmentKey =
  tupleKeyFromInts

supportRowCount :: IntMap.IntMap (HashSet.HashSet RowTupleKey) -> Int
supportRowCount =
  IntMap.foldl' (\ !total rowsValue -> total + HashSet.size rowsValue) 0

supportMemoArena :: ProvArena
supportMemoRoot :: ProvVal
(supportMemoArena, supportMemoRoot) =
  let (!arena1, !leftValue) =
        pvAtom (mkAtomId 700) (row [700, 1]) emptyProvArena
      (!arena2, !rightValue) =
        pvAtom (mkAtomId 700) (row [700, 2]) arena1
      (!arena3, !guardValue) =
        pvAtom (mkAtomId 701) (row [701, 1]) arena2
      (!arena4, !sumValue) =
        pvPlus leftValue rightValue arena3
   in pvTimes sumValue guardValue arena4

deepMarkDepth :: Int
deepMarkDepth =
  8192

deepMarkArena :: ProvArena
deepMarkRoot :: ProvVal
(deepMarkArena, deepMarkRoot) =
  foldl' step seed [1 .. deepMarkDepth]
  where
    seed =
      pvAtom (mkAtomId 800) (row [800, 0]) emptyProvArena

    step (!arena, !currentRoot) ix =
      let (!arena1, !leafValue) =
            pvAtom (mkAtomId (800 + ix)) (row [800, ix]) arena
       in if even ix
            then pvPlus currentRoot leafValue arena1
            else pvTimes currentRoot leafValue arena1

slotProjected, slotHidden, slotTenant, slotRole :: SlotId
slotProjected = mkSlotId 0
slotHidden = mkSlotId 1
slotTenant = mkSlotId 10
slotRole = mkSlotId 11

projectedValue, hiddenLeft, hiddenRight, tenantValue, survivingRole, deletedRole :: Int
projectedValue = 100
hiddenLeft = 201
hiddenRight = 202
tenantValue = 300
survivingRole = 401
deletedRole = 402
