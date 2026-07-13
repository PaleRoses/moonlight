{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Boundary.BlueRoseSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Control.Monad
  ( foldM,
  )
import Data.Maybe
  ( mapMaybe,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Sequence
  ( Seq,
  )
import Data.Word
  ( Word64,
  )
import Data.Sequence qualified as Seq
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..)
  )
import Moonlight.Core hiding (QueryAtom)
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( Substitution,
  )
import Moonlight.Core
  ( BoundaryOps (..),
  )
import Moonlight.Core
  ( LiveEpoch,
    QueryId,
    QuotientEpoch,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
    nextLiveEpoch,
    nextQuotientEpoch,
  )
import Moonlight.EGraph.Boundary.RewriteHolonomySpec
  ( BlueRoseBatchOracle (..),
    BlueRoseCase (..),
    BlueRoseExpectations (..),
    BlueRoseRepairTrace (..),
    BlueRoseRewriteTrace (..),
    BlueRoseRuntimeView (..),
    blueRoseTest,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
    rebuildWithDelta,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedBase,
    EGraphRelationalMatchObstruction,
    PatternAtomizeObstruction,
    QueryPlan,
    atomizeCompiledPatternQuery,
    buildPreparedBase,
    patchPreparedBaseWith,
    preparedBaseRowBlocks,
    quotientPatchFromRowDeltas,
    wcojMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Rewrite.Env
  ( emptyEGraphRewriteEnv,
  )
import Moonlight.EGraph.Pure.Rewrite.Program
  ( runExecutableRewriteMatchesEGraphCommitted,
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    RewriteRuleId (..),
  )
import Moonlight.EGraph.Test.Ring.Core qualified as Ring
import Moonlight.EGraph.Test.Saturation.Helpers
  ( buildGraph,
    compileRingPatternQuery,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    carrierAddr,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (FactorNodeRoot),
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (..),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseIndex),
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreError,
    carrierCurrentAddresses,
    commitCarrierDelta,
    emptyCarrierStore,
    validateCarrierStore,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierCurrentDeltaLatestTraceNow,
    visibleCarrierNow,
    visibleContextNow,
  )
import Moonlight.Flow.Model.Delta
  ( AtomPatch,
    QuotientPatch (..),
    atomPatchRows,
    mkAtomPatch
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
    RowDeltaError,
    rowDeltaNull
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    applyMultiplicityChange,
    multiplicityValue,
    negateMultiplicityChange,
    positiveMultiplicityChange
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    composePlainRowPatch,
    emptyPlainRowPatch,
    plainRowPatchChangeMap,
    plainRowPatchFromList,
    plainRowPatchFromMultiplicityMap,
    subtractPlainRowPatch
  )
import Moonlight.Flow.Model.Scope
  ( DepsDelta (..),
    ImpactedDelta (..),
    RelationalScope (..),
    ResultsDelta (..),
    TopoDelta (..),
    scopeDeps,
    scopeTopo,
  )
import Moonlight.Differential.Row.Delta
  ( RowBlockDeltaError,
    rowBlockToRowDelta,
  )
import Moonlight.Differential.Row.Block
  ( RowBuildError,
  )
import Data.Fix
  ( Fix,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
  )
import Moonlight.Core hiding (QueryAtom)
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RewriteCondition,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.Rewrite.Runtime
  ( RulePlan (..),
    RewriteApplicationError,
  )
import Moonlight.Rewrite.System
  ( RawRewriteRule (..),
  )
import Moonlight.Rewrite.System
  ( RewriteError )
import Moonlight.Saturation.Substrate (compileRewriteRules)
import Moonlight.Sheaf.Obstruction
  ( CoverCohomologyReport (..),
    CoverNerve (..),
    Nerve1Cochain (..),
    NerveEdge (..),
    WitnessStalk,
    holonomyCoverCohomologyReport,
    orientedCycleEdges,
    witnessSingleton,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


data BlueRoseRule
  = BlueTint
  | SeamReflect
  | MedallionClose
  | Seam50Repair
  deriving stock (Eq, Ord, Show)

data BlueRoseContext
  = RoseVoid
  | Petal0
  | Petal1
  | Petal2
  | Petal3
  | Petal4
  | Petal5
  | WholeRose
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

instance JoinSemilattice BlueRoseContext where
  join =
    blueRoseJoin

instance BoundedJoinSemilattice BlueRoseContext where
  bottom =
    RoseVoid

instance MeetSemilattice BlueRoseContext where
  meet =
    blueRoseMeet

instance BoundedMeetSemilattice BlueRoseContext where
  top =
    WholeRose

instance Lattice BlueRoseContext

data BlueRoseProp
  = GlyphProp
  | TransportProp
  | WitnessProp
  | MedallionProp
  deriving stock (Eq, Ord, Show)

data BlueRoseBoundary = BlueRoseBoundary
  deriving stock (Eq, Ord, Show)

instance BoundaryOps BlueRoseBoundary where
  type BoundaryOverlap BlueRoseBoundary = ()

  overlapBetweenBoundary _ _ =
    ()

  restrictBoundaryRaw _ _ =
    BlueRoseBoundary

  compatibleBoundaryRaw _ _ =
    Right BlueRoseBoundary

  subsumesBoundaryRaw _ _ =
    True

newtype BlueRoseEvidence = BlueRoseEvidence
  { blueRoseEvidenceRules :: Set BlueRoseRule
  }
  deriving stock (Eq, Ord, Show)

data BlueRoseMutation = BlueTintPetal2Mutation
  deriving stock (Eq, Ord, Show)

data BlueRoseProgram = BlueRoseProgram
  { brpBlueTint :: !(RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF),
    brpSeamReflect :: !(RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF),
    brpMedallionClose :: !(RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF),
    brpQueryPlan :: !(QueryPlan SurfaceKind Ring.RingF)
  }

data BlueRoseWorld = BlueRoseWorld
  { brwGraph :: !(EGraph Ring.RingF Ring.NodeCount),
    brwPreparedBase :: !(EGraphPreparedBase SurfaceKind Ring.RingF),
    brwEpoch :: !QuotientEpoch,
    brwCarrierRows :: !(Map BlueRoseAddr (Map RowTupleKey Multiplicity))
  }

data BlueRoseRewriteRun = BlueRoseRewriteRun
  { brwrRowsBefore :: !(IntMap RowDelta),
    brwrRowsAfter :: !(IntMap RowDelta),
    brwrMatchCounts :: !(Map BlueRoseRule Int),
    brwrDirtyKeys :: !IntSet,
    brwrDirtyTopo :: !IntSet
  }
  deriving stock (Eq, Show)

data BlueRoseRepairRun = BlueRoseRepairRun
  { brrrInsertedRows :: !(Map BlueRoseAddr (Map RowTupleKey Multiplicity))
  }
  deriving stock (Eq, Show)

data BlueRoseRuntime = BlueRoseRuntime
  { brrtQuotientEpoch :: !QuotientEpoch,
    brrtLiveEpoch :: !LiveEpoch,
    brrtNextSeq :: !Word64,
    brrtQueue :: !(Seq ()),
    brrtIndex :: !(CarrierStore BlueRoseContext Carrier BlueRoseProp BlueRoseBoundary BlueRoseEvidence)
  }

data BlueRoseRuntimeError
  = BlueRosePatchEpochMismatch !QuotientEpoch !QuotientEpoch
  | BlueRosePatchDidNotAdvance !QuotientEpoch !QuotientEpoch
  | BlueRoseCarrierIndexError !(CarrierStoreError BlueRoseContext Carrier BlueRoseProp BlueRoseBoundary BlueRoseEvidence)
  | BlueRoseCarrierUnderflow !BlueRoseAddr !RowTupleKey !Multiplicity !MultiplicityChange
  deriving stock (Eq, Show)

data BlueRoseRewriteError
  = BlueRoseRewriteCompileError !BlueRoseRule !(RewriteError SurfaceKind Ring.RingF)
  | BlueRoseRewriteMatchError !BlueRoseRule !EGraphRelationalMatchObstruction
  | BlueRoseRewriteNoMatch !BlueRoseRule
  | BlueRoseRewriteUnexpectedMatchCount !BlueRoseRule !Int !Int
  | BlueRoseRewriteApplyError !BlueRoseRule !RewriteApplicationError
  | BlueRoseRewriteRowsError !BlueRosePreparedRowsError
  | BlueRoseRewritePatchError !RowDeltaError
  | BlueRoseRewriteRuntimeProjectionError !BlueRoseRuntimeError
  deriving stock (Eq, Show)

data BlueRoseRepairError
  = BlueRoseRepairNoObstruction
  | BlueRoseRepairPatchError !RowDeltaError
  | BlueRoseRepairRuntimeProjectionError !BlueRoseRuntimeError
  deriving stock (Eq, Show)

data BlueRosePreparedRowsError
  = BlueRosePreparedRowsBlockError !RowBuildError
  | BlueRosePreparedRowsDeltaError !RowBlockDeltaError
  deriving stock (Eq, Show)

data BlueRoseFixtureError
  = BlueRoseFixtureCompileError !BlueRoseRule !(RewriteError SurfaceKind Ring.RingF)
  | BlueRoseFixtureCompiledRuleCount !BlueRoseRule !Int
  | BlueRoseFixturePatternVariables ![PatternVar]
  | BlueRoseFixtureAtomizeError !PatternAtomizeObstruction
  | BlueRoseFixtureRowsError !BlueRosePreparedRowsError
  | BlueRoseFixtureRuntimeError !BlueRoseRuntimeError
  | BlueRoseFixtureRewriteError !BlueRoseRewriteError
  | BlueRoseFixtureAllocationError !UnionFindAllocationError
  deriving stock (Eq, Show)

data BlueRoseDescentReport = BlueRoseDescentReport
  deriving stock (Eq, Show)

data BlueRoseObstruction = BlueRoseObstruction
  deriving stock (Eq, Show)

data BlueRoseWitness = BlueRoseCycleWitness
  deriving stock (Eq, Ord, Show)

type BlueRoseAddr = CarrierAddr BlueRoseContext Carrier BlueRoseProp

type BlueRoseHolonomyCase =
  BlueRoseCase
    BlueRoseRule
    BlueRoseProgram
    BlueRoseMutation
    BlueRoseWorld
    BlueRoseRewriteRun
    BlueRoseRewriteError
    RowDeltaError
    BlueRoseRepairRun
    BlueRoseRepairError
    BlueRoseRuntime
    BlueRoseRuntimeError
    BlueRoseContext
    BlueRoseProp
    BlueRoseEvidence
    BlueRoseDescentReport
    BlueRoseObstruction
    BlueRoseWitness

tests :: TestTree
tests =
  testGroup
    "blue rose"
    [ case blueRoseRewriteHolonomy of
        Left fixtureError ->
          testCase "rewrite-produced-holonomy" $
            assertFailure ("failed to build Blue Rose fixture: " <> show fixtureError)
        Right blueRose ->
          blueRoseTest blueRose
    ]

blueRoseRewriteHolonomy :: Either BlueRoseFixtureError BlueRoseHolonomyCase
blueRoseRewriteHolonomy = do
  programValue <- buildBlueRoseProgram
  initialWorldValue <- buildInitialBlueRoseWorld programValue
  initialRuntimeValue <- buildInitialBlueRoseRuntime initialWorldValue
  (_, mutationTrace) <-
    first BlueRoseFixtureRewriteError $
      runBlueRoseRewriteMutation programValue BlueTintPetal2Mutation initialWorldValue
  pure
    BlueRoseCase
      { brcName = "rewrite-produced-holonomy",
        brcInitialWorld = initialWorldValue,
        brcRewriteProgram = programValue,
        brcMutation = BlueTintPetal2Mutation,
        brcRunRewriteMutation = runBlueRoseRewriteMutation,
        brcRewritePatchOracle = rewritePatchOracle,
        brcInitialRuntime = initialRuntimeValue,
        brcRuntimeView = blueRoseRuntimeView,
        brcObstructionReport = blueRoseHolonomyReportFromRuntime,
        brcRepair = repairBlueRoseObstruction,
        brcBatchOracle = batchBlueRoseOracle,
        brcEvidenceRules = blueRoseEvidenceRules,
        brcExpectations = blueRoseExpectations mutationTrace
      }

blueRoseExpectations :: BlueRoseRewriteTrace BlueRoseRule BlueRoseRewriteRun -> BlueRoseExpectations BlueRoseRule BlueRoseContext BlueRoseProp
blueRoseExpectations mutationTrace =
  let mutationPatch =
        brtPatch mutationTrace
   in BlueRoseExpectations
        { breExpectedMutationFiredRules = Set.fromList [BlueTint, SeamReflect],
          breExpectedMutationBlockedRules = Set.singleton MedallionClose,
          breExpectedMutationDirtyKeys = scopeDeps (qpScope mutationPatch),
          breExpectedMutationDirtyTopo = scopeTopo (qpScope mutationPatch),
          breExpectedInitialH1Supports = Set.singleton blueRoseCycleSupport,
          breExpectedRepairAddrs = Set.fromList [seam50TransportCarrierAddr, rosetteCycleWitnessCarrierAddr],
          breExpectedRepairFiredRules = Set.singleton Seam50Repair,
          breExpectedRepairBlockedRules = Set.empty,
          breMaxRepairAddrs = 2,
          breUntouchedCarrierFloor = 0,
          breVisibleContextsToCheck = Set.fromList [WholeRose, Petal0, Petal1, Petal2, Petal3, Petal4, Petal5],
          breVisibleCarriersToCheck = Set.fromList [petal2GlyphCarrierAddr, seam50TransportCarrierAddr, rosetteCycleWitnessCarrierAddr, globalMedallionCarrierAddr]
        }

buildBlueRoseProgram :: Either BlueRoseFixtureError BlueRoseProgram
buildBlueRoseProgram = do
  blueTint <- compileBlueRoseRule BlueTint blueTintRewriteRule
  seamReflect <- compileBlueRoseRule SeamReflect seamReflectRewriteRule
  medallionClose <- compileBlueRoseRule MedallionClose medallionCloseRewriteRule
  compiledQuery <- first BlueRoseFixturePatternVariables (compileRingPatternQuery blueRoseCarrierPattern)
  queryPlan <- first BlueRoseFixtureAtomizeError (atomizeCompiledPatternQuery compiledQuery)
  pure
    BlueRoseProgram
      { brpBlueTint = blueTint,
        brpSeamReflect = seamReflect,
        brpMedallionClose = medallionClose,
        brpQueryPlan = queryPlan
      }

buildInitialBlueRoseWorld :: BlueRoseProgram -> Either BlueRoseFixtureError BlueRoseWorld
buildInitialBlueRoseWorld programValue = do
  graphValue <-
    first BlueRoseFixtureAllocationError (fst <$> buildGraph [wholeRoseTerm])
  let preparedBase =
        buildPreparedBase (brpQueryPlan programValue) graphValue
  rows <- first BlueRoseFixtureRowsError (preparedBaseRows preparedBase)
  carrierRows <- first BlueRoseFixtureRuntimeError (carrierRowsFromAtomRows rows)
  pure
    BlueRoseWorld
      { brwGraph = graphValue,
        brwPreparedBase = preparedBase,
        brwEpoch = initialQuotientEpoch,
        brwCarrierRows = carrierRows
      }

buildInitialBlueRoseRuntime :: BlueRoseWorld -> Either BlueRoseFixtureError BlueRoseRuntime
buildInitialBlueRoseRuntime worldValue =
  first BlueRoseFixtureRuntimeError $
    seedBlueRoseRuntime (brwCarrierRows worldValue)

runBlueRoseRewriteMutation ::
  BlueRoseProgram ->
  BlueRoseMutation ->
  BlueRoseWorld ->
  Either BlueRoseRewriteError (BlueRoseWorld, BlueRoseRewriteTrace BlueRoseRule BlueRoseRewriteRun)
runBlueRoseRewriteMutation programValue mutationValue worldValue =
  case mutationValue of
    BlueTintPetal2Mutation -> do
      rowsBefore <- first BlueRoseRewriteRowsError (preparedBaseRows (brwPreparedBase worldValue))
      let graph0 =
            brwGraph worldValue
      (graph1, blueTintDelta, blueTintFired, blueTintBlocked, blueTintCount) <-
        applyBlueRoseRule BlueTint (brpBlueTint programValue) graph0
      (graph2, seamReflectDelta, seamReflectFired, seamReflectBlocked, seamReflectCount) <-
        applyBlueRoseRule SeamReflect (brpSeamReflect programValue) graph1
      requireMatchCount BlueTint 1 blueTintCount
      requireMatchCount SeamReflect 2 seamReflectCount
      medallionBlocked <-
        blockedIfNoMatch MedallionClose (brpMedallionClose programValue) graph2
      let rebuildDelta =
            blueTintDelta <> seamReflectDelta
          rebuiltGraph =
            graph2
          (preparedAfter, atomInputDeltas) =
            patchPreparedBaseWith rebuiltGraph (erdDirtyResultKeys rebuildDelta) (brwPreparedBase worldValue)
      rowsAfter <- first BlueRoseRewriteRowsError (preparedBaseRows preparedAfter)
      let patchValue =
            quotientPatchFromRowDeltas
              (brwEpoch worldValue)
              (nextQuotientEpoch (brwEpoch worldValue))
              (erdDirtyResultKeys rebuildDelta)
              (erdTopologyClassKeys rebuildDelta)
              atomInputDeltas
      carrierRowsAfter <-
        first BlueRoseRewriteRuntimeProjectionError $
          applyQuotientPatchToCarrierRows patchValue (brwCarrierRows worldValue)
      let firedRules =
            blueTintFired <> seamReflectFired
          blockedRules =
            blueTintBlocked <> seamReflectBlocked <> medallionBlocked
          rewriteRun =
            BlueRoseRewriteRun
              { brwrRowsBefore = rowsBefore,
                brwrRowsAfter = rowsAfter,
                brwrMatchCounts = Map.fromList [(BlueTint, blueTintCount), (SeamReflect, seamReflectCount)],
                brwrDirtyKeys = erdDirtyResultKeys rebuildDelta,
                brwrDirtyTopo = erdTopologyClassKeys rebuildDelta
              }
          worldAfter =
            worldValue
              { brwGraph = rebuiltGraph,
                brwPreparedBase = preparedAfter,
                brwEpoch = etAfter (qpEpoch patchValue),
                brwCarrierRows = carrierRowsAfter
              }
      pure
        ( worldAfter,
          BlueRoseRewriteTrace
            { brtRun = rewriteRun,
              brtPatch = patchValue,
              brtFiredRules = firedRules,
              brtBlockedRules = blockedRules
            }
        )

applyBlueRoseRule ::
  BlueRoseRule ->
  RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF ->
  EGraph Ring.RingF Ring.NodeCount ->
  Either BlueRoseRewriteError (EGraph Ring.RingF Ring.NodeCount, EGraphRebuildDelta, Set BlueRoseRule, Set BlueRoseRule, Int)
applyBlueRoseRule ruleName compiledRule graphValue =
  case relationalRuleMatches ruleName compiledRule graphValue of
    Left matchError ->
      Left matchError
    Right [] ->
      Left (BlueRoseRewriteNoMatch ruleName)
    Right matches -> do
      rewriteCommit <-
        first (BlueRoseRewriteApplyError ruleName) $
          runExecutableRewriteMatchesEGraphCommitted
            emptyEGraphRewriteEnv
            (fmap (\(rootClassId, substitution) -> ExecutableRewriteMatch compiledRule rootClassId Nothing Nothing substitution) matches)
            graphValue
      let (rebuildDelta, _repairIndex, graphAfter) =
            rebuildWithDelta (emrGraph rewriteCommit)
      pure (graphAfter, rebuildDelta, Set.singleton ruleName, Set.empty, length matches)


requireMatchCount :: BlueRoseRule -> Int -> Int -> Either BlueRoseRewriteError ()
requireMatchCount ruleName expectedCount actualCount =
  if actualCount == expectedCount
    then Right ()
    else Left (BlueRoseRewriteUnexpectedMatchCount ruleName expectedCount actualCount)

blockedIfNoMatch ::
  BlueRoseRule ->
  RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF ->
  EGraph Ring.RingF Ring.NodeCount ->
  Either BlueRoseRewriteError (Set BlueRoseRule)
blockedIfNoMatch ruleName compiledRule graphValue =
  case relationalRuleMatches ruleName compiledRule graphValue of
    Left matchError ->
      Left matchError
    Right [] ->
      Right (Set.singleton ruleName)
    Right _ ->
      Right Set.empty

relationalRuleMatches ::
  BlueRoseRule ->
  RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF ->
  EGraph Ring.RingF Ring.NodeCount ->
  Either BlueRoseRewriteError [(ClassId, Substitution)]
relationalRuleMatches ruleName compiledRule =
  first (BlueRoseRewriteMatchError ruleName)
    . wcojMatchCompiledWithRoots (rpQuery compiledRule)

rewritePatchOracle ::
  BlueRoseProgram ->
  BlueRoseMutation ->
  BlueRoseWorld ->
  BlueRoseWorld ->
  BlueRoseRewriteRun ->
  Either RowDeltaError QuotientPatch
rewritePatchOracle _programValue _mutationValue worldBefore worldAfter rewriteRun =
  patchFromRows
    (brwEpoch worldBefore)
    (brwEpoch worldAfter)
    (brwrDirtyKeys rewriteRun)
    (brwrDirtyTopo rewriteRun)
    (brwrRowsBefore rewriteRun)
    (brwrRowsAfter rewriteRun)

repairBlueRoseObstruction ::
  BlueRoseWorld ->
  BlueRoseRuntime ->
  CoverCohomologyReport BlueRoseContext BlueRoseDescentReport BlueRoseObstruction (WitnessStalk BlueRoseWitness) ->
  Either BlueRoseRepairError (BlueRoseWorld, BlueRoseRepairTrace BlueRoseRule BlueRoseRepairRun BlueRoseContext BlueRoseProp)
repairBlueRoseObstruction worldValue _runtimeValue reportValue =
  case corH1Obstructions reportValue of
    [] ->
      Left BlueRoseRepairNoObstruction
    _ -> do
      patchValue <-
        first BlueRoseRepairPatchError $
          patchFromRows
            (brwEpoch worldValue)
            (nextQuotientEpoch (brwEpoch worldValue))
            (IntSet.fromList [repairSeamAtomKey, repairWitnessAtomKey])
            (IntSet.singleton rosetteCycleTopoKey)
            IntMap.empty
            ( IntMap.fromList
                [ (repairSeamAtomKey, plainRowPatchFromList [(repairSeamRow, MultiplicityChange 1)]),
                  (repairWitnessAtomKey, plainRowPatchFromList [(repairWitnessRow, MultiplicityChange 1)])
                ]
            )
      carrierRowsAfter <-
        first BlueRoseRepairRuntimeProjectionError $
          applyQuotientPatchToCarrierRows patchValue (brwCarrierRows worldValue)
      let insertedRows =
            Map.fromList
              [ (seam50TransportCarrierAddr, Map.singleton repairSeamRow (Multiplicity 1)),
                (rosetteCycleWitnessCarrierAddr, Map.singleton repairWitnessRow (Multiplicity 1))
              ]
          worldAfter =
            worldValue
              { brwEpoch = etAfter (qpEpoch patchValue),
                brwCarrierRows = carrierRowsAfter
              }
      pure
        ( worldAfter,
          BlueRoseRepairTrace
            { brrRun = BlueRoseRepairRun {brrrInsertedRows = insertedRows},
              brrPatch = patchValue,
              brrTouchedAddrs = Map.keysSet insertedRows,
              brrFiredRules = Set.singleton Seam50Repair,
              brrBlockedRules = Set.empty
            }
        )

blueRoseRuntimeView :: BlueRoseRuntimeView BlueRoseRuntime BlueRoseContext BlueRoseProp BlueRoseEvidence BlueRoseRuntimeError
blueRoseRuntimeView =
  BlueRoseRuntimeView
    { brvApplyQuotientPatch = applyBlueRoseRuntimePatch,
      brvDrainRuntimeQueue = Right,
      brvRuntimeQueueDrained = Seq.null . brrtQueue,
      brvAssertCarrierIndexReplay = assertBlueRoseCarrierIndexReplay,
      brvQueryVisibleCarrier = \addr runtimeValue -> visibleCarrierNow addr (brrtIndex runtimeValue),
      brvQueryVisibleContext = \contextValue runtimeValue -> visibleContextNow contextValue (brrtIndex runtimeValue),
      brvCarrierEvidence = carrierEvidenceFromRuntime,
      brvLiveCarrierAddrs = carrierCurrentAddresses . brrtIndex
    }

carrierEvidenceFromRuntime :: BlueRoseRuntime -> [BlueRoseEvidence]
carrierEvidenceFromRuntime runtimeValue =
  mapMaybe
    (\addr -> deEvidence <$> carrierCurrentDeltaLatestTraceNow addr (brrtIndex runtimeValue))
    (Set.toAscList (carrierCurrentAddresses (brrtIndex runtimeValue)))

applyBlueRoseRuntimePatch :: QuotientPatch -> BlueRoseRuntime -> Either BlueRoseRuntimeError BlueRoseRuntime
applyBlueRoseRuntimePatch patchValue runtimeValue = do
  if etBefore (qpEpoch patchValue) == brrtQuotientEpoch runtimeValue
    then Right ()
    else Left (BlueRosePatchEpochMismatch (brrtQuotientEpoch runtimeValue) (etBefore (qpEpoch patchValue)))
  if etAfter (qpEpoch patchValue) > etBefore (qpEpoch patchValue)
    then Right ()
    else Left (BlueRosePatchDidNotAdvance (etBefore (qpEpoch patchValue)) (etAfter (qpEpoch patchValue)))
  runtimeAfterAtoms <-
    foldM
      (\currentRuntime (atomKey, atomDelta) -> insertRuntimeAtomDelta patchValue atomKey atomDelta currentRuntime)
      runtimeValue
      (IntMap.toAscList (qpEvents patchValue))
  pure
    runtimeAfterAtoms
      { brrtQuotientEpoch = etAfter (qpEpoch patchValue),
        brrtLiveEpoch = nextLiveEpoch (brrtLiveEpoch runtimeValue),
        brrtQueue = Seq.empty
      }

insertRuntimeAtomDelta :: QuotientPatch -> Int -> AtomPatch -> BlueRoseRuntime -> Either BlueRoseRuntimeError BlueRoseRuntime
insertRuntimeAtomDelta patchValue atomKey atomDelta runtimeValue =
  insertRuntimeCarrierDelta
    patchValue
    (blueRoseCarrierAddrForAtomKey atomKey)
    (blueRoseEvidenceForAtomKey atomKey)
    (atomPatchRows atomDelta)
    runtimeValue

insertRuntimeCarrierDelta ::
  QuotientPatch ->
  BlueRoseAddr ->
  BlueRoseEvidence ->
  RowDelta ->
  BlueRoseRuntime ->
  Either BlueRoseRuntimeError BlueRoseRuntime
insertRuntimeCarrierDelta patchValue addr evidenceValue rows runtimeValue = do
  nextIndex <-
    first BlueRoseCarrierIndexError $
      commitCarrierDelta
        blueRoseContextLattice
        RelationalCarrierDelta
          { deAddr = addr,
            deTime = blueRoseEventTime addr patchValue runtimeValue,
            deSupport = principalSupport (caContext addr),
            deBoundary = BlueRoseBoundary,
            deEvidence = evidenceValue,
            deRows = rows,
            deOrigin =
              RelationalOrigin
                { roEvent = OriginLocal blueRoseQueryId,
                  roRoute = emptyDerivationRoute
                },
            deScope = qpScope patchValue,
            dePayload = ()
          }
        (brrtIndex runtimeValue)
  pure
    runtimeValue
      { brrtNextSeq = brrtNextSeq runtimeValue + 1,
        brrtIndex = nextIndex
      }

seedBlueRoseRuntime :: Map BlueRoseAddr (Map RowTupleKey Multiplicity) -> Either BlueRoseRuntimeError BlueRoseRuntime
seedBlueRoseRuntime carrierRows =
  foldM
    (\runtimeValue (addr, rows) ->
       insertRuntimeCarrierDelta
         seedPatch
         addr
         (BlueRoseEvidence Set.empty)
         (plainRowPatchFromMultiplicityMap rows)
         runtimeValue
    )
    emptyBlueRoseRuntime
    (Map.toAscList carrierRows)

emptyBlueRoseRuntime :: BlueRoseRuntime
emptyBlueRoseRuntime =
  BlueRoseRuntime
    { brrtQuotientEpoch = initialQuotientEpoch,
      brrtLiveEpoch = initialLiveEpoch,
      brrtNextSeq = 0,
      brrtQueue = Seq.empty,
      brrtIndex = emptyCarrierStore
    }

seedPatch :: QuotientPatch
seedPatch =
  QuotientPatch
    { qpEpoch =
        EpochTransition
          { etBefore = initialQuotientEpoch,
            etAfter = initialQuotientEpoch
          },
      qpScope = mempty,
      qpAtomScopeByAtom = IntMap.empty,
      qpEvents = IntMap.empty
    }

blueRoseEventTime ::
  BlueRoseAddr ->
  QuotientPatch ->
  BlueRoseRuntime ->
  RelationalCarrierTime BlueRoseContext
blueRoseEventTime addr patchValue runtimeValue =
  mkRelationalCarrierTime
    (caContext addr)
    (etAfter (qpEpoch patchValue))
    (nextLiveEpoch (brrtLiveEpoch runtimeValue))
    PhaseIndex
    (frontierStamp (brrtNextSeq runtimeValue))

assertBlueRoseCarrierIndexReplay :: BlueRoseRuntime -> Assertion
assertBlueRoseCarrierIndexReplay runtimeValue =
  validateCarrierStore blueRoseContextLattice (brrtIndex runtimeValue)
    @?= Right ()

blueRoseHolonomyReportFromRuntime ::
  BlueRoseRuntime ->
  CoverCohomologyReport BlueRoseContext BlueRoseDescentReport BlueRoseObstruction (WitnessStalk BlueRoseWitness)
blueRoseHolonomyReportFromRuntime runtimeValue =
  holonomyCoverCohomologyReport
    blueRoseNerve
    BlueRoseDescentReport
    (blueRoseRosetteTransition runtimeValue)

blueRoseRosetteTransition ::
  BlueRoseRuntime ->
  Nerve1Cochain BlueRoseContext (WitnessStalk BlueRoseWitness)
blueRoseRosetteTransition runtimeValue =
  Nerve1Cochain
    ( if rosetteSealed runtimeValue
        then Map.empty
        else
          Map.fromList
            [ (orientedEdge, blueRoseWitness)
              | orientedEdge <- NonEmpty.toList (orientedCycleEdges blueRoseCycle)
            ]
    )

rosetteSealed :: BlueRoseRuntime -> Bool
rosetteSealed runtimeValue =
  not (Map.null (visibleCarrierNow seam50TransportCarrierAddr (brrtIndex runtimeValue)))
    && not (Map.null (visibleCarrierNow rosetteCycleWitnessCarrierAddr (brrtIndex runtimeValue)))

batchBlueRoseOracle :: BlueRoseWorld -> BlueRoseBatchOracle BlueRoseContext BlueRoseProp
batchBlueRoseOracle worldValue =
  BlueRoseBatchOracle
    { bboVisibleContexts = visibleContextsFromCarrierRows (brwCarrierRows worldValue),
      bboVisibleCarriers = brwCarrierRows worldValue
    }

visibleContextsFromCarrierRows :: Map BlueRoseAddr (Map RowTupleKey Multiplicity) -> Map BlueRoseContext (RelationalSection BlueRoseContext Carrier BlueRoseProp)
visibleContextsFromCarrierRows =
  Map.foldrWithKey
    (\addr rows sections ->
       if Map.null rows
         then sections
         else
          Map.insertWith
            mergeRelationalSection
            (caContext addr)
            (RelationalSection (Map.singleton addr (plainRowPatchFromMultiplicityMap rows)))
            sections
    )
    Map.empty

mergeRelationalSection ::
  RelationalSection BlueRoseContext Carrier BlueRoseProp ->
  RelationalSection BlueRoseContext Carrier BlueRoseProp ->
  RelationalSection BlueRoseContext Carrier BlueRoseProp
mergeRelationalSection leftSection rightSection =
  RelationalSection
    { rsCarriers = Map.unionWith composePlainRowPatch (rsCarriers leftSection) (rsCarriers rightSection)
    }

preparedBaseRows :: EGraphPreparedBase SurfaceKind Ring.RingF -> Either BlueRosePreparedRowsError (IntMap RowDelta)
preparedBaseRows preparedBase =
  first BlueRosePreparedRowsBlockError
    (preparedBaseRowBlocks 0 preparedBase)
    >>= first BlueRosePreparedRowsDeltaError . traverse rowBlockToRowDelta

carrierRowsFromAtomRows :: IntMap RowDelta -> Either BlueRoseRuntimeError (Map BlueRoseAddr (Map RowTupleKey Multiplicity))
carrierRowsFromAtomRows atomRows =
  foldM
    (\carrierRows (atomKey, rows) -> applyRowsAt (blueRoseCarrierAddrForAtomKey atomKey) rows carrierRows)
    Map.empty
    (IntMap.toAscList atomRows)

applyQuotientPatchToCarrierRows :: QuotientPatch -> Map BlueRoseAddr (Map RowTupleKey Multiplicity) -> Either BlueRoseRuntimeError (Map BlueRoseAddr (Map RowTupleKey Multiplicity))
applyQuotientPatchToCarrierRows patchValue carrierRows =
  foldM
    (\rows (atomKey, atomDelta) -> applyRowsAt (blueRoseCarrierAddrForAtomKey atomKey) (atomPatchRows atomDelta) rows)
    carrierRows
    (IntMap.toAscList (qpEvents patchValue))

applyRowsAt :: BlueRoseAddr -> RowDelta -> Map BlueRoseAddr (Map RowTupleKey Multiplicity) -> Either BlueRoseRuntimeError (Map BlueRoseAddr (Map RowTupleKey Multiplicity))
applyRowsAt addr rows carrierRows = do
  nextRows <-
    foldM
      (\currentRows (rowValue, deltaMultiplicity) -> applyMultiplicityAt addr rowValue deltaMultiplicity currentRows)
      (Map.findWithDefault Map.empty addr carrierRows)
      (Map.toAscList (plainRowPatchChangeMap rows))
  pure (alterEmptyMap addr nextRows carrierRows)

applyMultiplicityAt :: BlueRoseAddr -> RowTupleKey -> MultiplicityChange -> Map RowTupleKey Multiplicity -> Either BlueRoseRuntimeError (Map RowTupleKey Multiplicity)
applyMultiplicityAt addr rowValue deltaMultiplicity rows =
  let currentMultiplicity =
        Map.findWithDefault (Multiplicity 0) rowValue rows
   in case applyMultiplicityChange currentMultiplicity deltaMultiplicity of
        Nothing ->
          Left (BlueRoseCarrierUnderflow addr rowValue currentMultiplicity deltaMultiplicity)
        Just nextMultiplicity ->
          Right (alterMultiplicity rowValue nextMultiplicity rows)

alterEmptyMap :: Ord key => key -> Map valueKey value -> Map key (Map valueKey value) -> Map key (Map valueKey value)
alterEmptyMap key rows =
  if Map.null rows
    then Map.delete key
    else Map.insert key rows

alterMultiplicity :: RowTupleKey -> Multiplicity -> Map RowTupleKey Multiplicity -> Map RowTupleKey Multiplicity
alterMultiplicity rowValue multiplicity rows =
  if multiplicityValue multiplicity == 0
    then Map.delete rowValue rows
    else Map.insert rowValue multiplicity rows

patchFromRows ::
  QuotientEpoch ->
  QuotientEpoch ->
  IntSet ->
  IntSet ->
  IntMap RowDelta ->
  IntMap RowDelta ->
  Either RowDeltaError QuotientPatch
patchFromRows epochBefore epochAfter dirtyKeys dirtyTopo beforeRows afterRows = do
  atomDeltas <-
    foldM
      (\deltas atomKey -> do
         maybeDelta <- canonicalDeltaForRows (rowDeltaAt atomKey beforeRows) (rowDeltaAt atomKey afterRows)
         pure
           ( case maybeDelta of
               Nothing -> deltas
               Just atomDelta -> IntMap.insert atomKey atomDelta deltas
           )
      )
      IntMap.empty
      (IntSet.toAscList (IntMap.keysSet beforeRows <> IntMap.keysSet afterRows))
  let patchScope =
        mempty
          { rsDeps = DepsDelta dirtyKeys,
            rsTopo = TopoDelta dirtyTopo,
            rsResults = ResultsDelta dirtyKeys,
            rsImpacted = ImpactedDelta dirtyKeys
          }
  pure
    QuotientPatch
      { qpEpoch =
          EpochTransition
            { etBefore = epochBefore,
              etAfter = epochAfter
            },
        qpScope = patchScope,
        qpAtomScopeByAtom = fmap (const patchScope) atomDeltas,
        qpEvents = atomDeltas
      }

canonicalDeltaForRows :: RowDelta -> RowDelta -> Either RowDeltaError (Maybe AtomPatch)
canonicalDeltaForRows beforeRows afterRows =
  let diffRows =
        subtractPlainRowPatch afterRows beforeRows
   in if rowDeltaNull diffRows
        then Right Nothing
        else Just <$> mkAtomPatch (negativeRowsAsPositive diffRows) (positiveRows diffRows)

rowDeltaAt :: Int -> IntMap RowDelta -> RowDelta
rowDeltaAt atomKey =
  IntMap.findWithDefault emptyPlainRowPatch atomKey

positiveRows :: RowDelta -> Map RowTupleKey Multiplicity
positiveRows =
  Map.mapMaybe positiveMultiplicityChange . plainRowPatchChangeMap

negativeRowsAsPositive :: RowDelta -> Map RowTupleKey Multiplicity
negativeRowsAsPositive =
  Map.mapMaybe (positiveMultiplicityChange . negateMultiplicityChange) . plainRowPatchChangeMap

compileBlueRoseRule :: BlueRoseRule -> RawRewriteRule (RewriteCondition SurfaceKind Ring.RingF) Ring.RingF -> Either BlueRoseFixtureError (RulePlan (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF)
compileBlueRoseRule ruleName rewriteRule =
  case first (BlueRoseFixtureCompileError ruleName) (compileRewriteRules @(EGraphU SurfaceKind Ring.RingF Ring.NodeCount ()) [rewriteRule]) of
    Right [compiledRule] -> Right compiledRule
    Right compiledRules -> Left (BlueRoseFixtureCompiledRuleCount ruleName (length compiledRules))
    Left compileError -> Left compileError

blueTintRewriteRule :: RawRewriteRule (RewriteCondition SurfaceKind Ring.RingF) Ring.RingF
blueTintRewriteRule =
  RawRewriteRule
    { rrId = RewriteRuleId 101,
      rrLhs = PatternNode (Ring.Add (PatternVar xVar) (PatternNode Ring.RZero)),
      rrRhs = PatternVar xVar,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

seamReflectRewriteRule :: RawRewriteRule (RewriteCondition SurfaceKind Ring.RingF) Ring.RingF
seamReflectRewriteRule =
  RawRewriteRule
    { rrId = RewriteRuleId 102,
      rrLhs = PatternNode (Ring.Mul (PatternVar xVar) (PatternNode Ring.ROne)),
      rrRhs = PatternVar xVar,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

medallionCloseRewriteRule :: RawRewriteRule (RewriteCondition SurfaceKind Ring.RingF) Ring.RingF
medallionCloseRewriteRule =
  RawRewriteRule
    { rrId = RewriteRuleId 103,
      rrLhs = PatternNode (Ring.Neg (PatternNode (Ring.Neg (PatternVar xVar)))),
      rrRhs = PatternVar xVar,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

blueRoseCarrierPattern :: Pattern Ring.RingF
blueRoseCarrierPattern =
  PatternNode
    ( Ring.Add
        (PatternNode (Ring.Add (PatternVar xVar) (PatternNode Ring.RZero)))
        ( PatternNode
            ( Ring.Add
                (PatternNode (Ring.Mul (PatternVar yVar) (PatternNode Ring.ROne)))
                (PatternNode (Ring.Mul (PatternVar zVar) (PatternNode Ring.ROne)))
            )
        )
    )

wholeRoseTerm :: Fix Ring.RingF
wholeRoseTerm =
  Ring.ringAdd
    (Ring.ringAdd (Ring.ringVar "petal2") Ring.ringZero)
    ( Ring.ringAdd
        (Ring.ringMul (Ring.ringVar "seam12") Ring.ringOne)
        (Ring.ringMul (Ring.ringVar "seam23") Ring.ringOne)
    )

xVar :: EGraph.PatternVar
xVar =
  EGraph.mkPatternVar 0

yVar :: EGraph.PatternVar
yVar =
  EGraph.mkPatternVar 1

zVar :: EGraph.PatternVar
zVar =
  EGraph.mkPatternVar 2

blueRoseCarrierAddrForAtomKey :: Int -> BlueRoseAddr
blueRoseCarrierAddrForAtomKey atomKey
  | atomKey == repairSeamAtomKey = seam50TransportCarrierAddr
  | atomKey == repairWitnessAtomKey = rosetteCycleWitnessCarrierAddr
  | otherwise = petal2GlyphCarrierAddr

blueRoseEvidenceForAtomKey :: Int -> BlueRoseEvidence
blueRoseEvidenceForAtomKey atomKey
  | atomKey == repairSeamAtomKey || atomKey == repairWitnessAtomKey = BlueRoseEvidence (Set.singleton Seam50Repair)
  | otherwise = BlueRoseEvidence (Set.fromList [BlueTint, SeamReflect])

petal2GlyphCarrierAddr :: BlueRoseAddr
petal2GlyphCarrierAddr =
  carrierAddr
    Petal2
    (PropositionKey GlyphProp)
    (QueryCarrier blueRoseQueryId (QueryAtom (mkAtomId 2)))

seam50TransportCarrierAddr :: BlueRoseAddr
seam50TransportCarrierAddr =
  carrierAddr
    Petal5
    (PropositionKey TransportProp)
    (QueryCarrier blueRoseRepairQueryId (QueryAtom (mkAtomId 50)))

rosetteCycleWitnessCarrierAddr :: BlueRoseAddr
rosetteCycleWitnessCarrierAddr =
  carrierAddr
    WholeRose
    (PropositionKey WitnessProp)
    (QueryCarrier blueRoseRepairQueryId (QueryFactor FactorNodeRoot))

globalMedallionCarrierAddr :: BlueRoseAddr
globalMedallionCarrierAddr =
  carrierAddr
    WholeRose
    (PropositionKey MedallionProp)
    (QueryCarrier (mkQueryId 777) (QueryFactor FactorNodeRoot))

blueRoseQueryId :: QueryId
blueRoseQueryId =
  mkQueryId 700

blueRoseRepairQueryId :: QueryId
blueRoseRepairQueryId =
  mkQueryId 701

repairSeamAtomKey :: Int
repairSeamAtomKey =
  900

repairWitnessAtomKey :: Int
repairWitnessAtomKey =
  901

rosetteCycleTopoKey :: Int
rosetteCycleTopoKey =
  60000

repairSeamRow :: RowTupleKey
repairSeamRow =
  tupleKeyFromRepKeys [RepKey 50, RepKey 5]

repairWitnessRow :: RowTupleKey
repairWitnessRow =
  tupleKeyFromRepKeys [RepKey 6]

blueRoseCycle :: NonEmpty BlueRoseContext
blueRoseCycle =
  Petal0 :| [Petal1, Petal2, Petal3, Petal4, Petal5]

blueRoseCycleSupport :: Set BlueRoseContext
blueRoseCycleSupport =
  Set.fromList [Petal0, Petal1, Petal2, Petal3, Petal4, Petal5]

blueRoseNerve :: CoverNerve BlueRoseContext
blueRoseNerve =
  CoverNerve
    { cnVertices = [Petal0, Petal1, Petal2, Petal3, Petal4, Petal5],
      cnEdges = Set.fromList blueRoseNerveEdges,
      cnAdjacency = blueRoseAdjacency,
      cnFundamentalCycles = [blueRoseCycle]
    }

blueRoseNerveEdges :: [NerveEdge BlueRoseContext]
blueRoseNerveEdges =
  [ NerveEdge Petal0 Petal1,
    NerveEdge Petal1 Petal2,
    NerveEdge Petal2 Petal3,
    NerveEdge Petal3 Petal4,
    NerveEdge Petal4 Petal5,
    NerveEdge Petal5 Petal0
  ]

blueRoseAdjacency :: Map BlueRoseContext (Set BlueRoseContext)
blueRoseAdjacency =
  Map.fromListWith
    Set.union
    ( foldMap
        (\NerveEdge {neLeftContext, neRightContext} ->
           [ (neLeftContext, Set.singleton neRightContext),
             (neRightContext, Set.singleton neLeftContext)
           ]
        )
        blueRoseNerveEdges
    )

blueRoseWitness :: WitnessStalk BlueRoseWitness
blueRoseWitness =
  witnessSingleton BlueRoseCycleWitness 1

blueRoseContextLattice :: ContextLattice BlueRoseContext
blueRoseContextLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid BlueRoseContext lattice fixture: " <> show compileError)

blueRoseJoin :: BlueRoseContext -> BlueRoseContext -> BlueRoseContext
blueRoseJoin leftContext rightContext
  | leftContext == RoseVoid = rightContext
  | rightContext == RoseVoid = leftContext
  | leftContext == rightContext = leftContext
  | otherwise = WholeRose

blueRoseMeet :: BlueRoseContext -> BlueRoseContext -> BlueRoseContext
blueRoseMeet leftContext rightContext
  | leftContext == WholeRose = rightContext
  | rightContext == WholeRose = leftContext
  | leftContext == rightContext = leftContext
  | otherwise = RoseVoid
