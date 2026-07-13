{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.EGraph.Boundary.RewriteHolonomySpec
  ( BlueRoseRewriteTrace (..),
    BlueRoseRepairTrace (..),
    BlueRoseBatchOracle (..),
    BlueRoseRuntimeView (..),
    BlueRoseExpectations (..),
    BlueRoseCase (..),
    blueRoseTest,
  )
where

import Data.Bifunctor (first)
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..)
  )
import Moonlight.Delta.Signed
  ( Multiplicity
  )
import Moonlight.Flow.Model.Scope
  ( scopeDeps,
    scopeTopo,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Sheaf.Obstruction
  ( CohomologyObservation (..),
    CoverCohomologyReport,
    WitnessStalk,
    runCohomologyObservation,
  )
import Moonlight.Pale.Test.Site.Assertion
  ( expectRight,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    testCase,
    (@?=),
  )

data BlueRoseRewriteTrace rule rewriteRun = BlueRoseRewriteTrace
  { brtRun :: !rewriteRun,
    brtPatch :: !QuotientPatch,
    brtFiredRules :: !(Set rule),
    brtBlockedRules :: !(Set rule)
  }
  deriving stock (Eq, Show)

data BlueRoseRepairTrace rule repairRun ctx prop = BlueRoseRepairTrace
  { brrRun :: !repairRun,
    brrPatch :: !QuotientPatch,
    brrTouchedAddrs :: !(Set (CarrierAddr ctx Carrier prop)),
    brrFiredRules :: !(Set rule),
    brrBlockedRules :: !(Set rule)
  }
  deriving stock (Eq, Show)

data BlueRoseBatchOracle ctx prop = BlueRoseBatchOracle
  { bboVisibleContexts :: !(Map ctx (RelationalSection ctx Carrier prop)),
    bboVisibleCarriers ::
      !(Map (CarrierAddr ctx Carrier prop) (Map RowTupleKey Multiplicity))
  }
  deriving stock (Eq, Show)

data BlueRoseRuntimeView runtime ctx prop evidence runtimeErr = BlueRoseRuntimeView
  { brvApplyQuotientPatch :: !(
      QuotientPatch ->
      runtime ->
      Either runtimeErr runtime
    ),
    brvDrainRuntimeQueue :: !(
      runtime ->
      Either runtimeErr runtime
    ),
    brvRuntimeQueueDrained :: !(runtime -> Bool),
    brvAssertCarrierIndexReplay :: !(runtime -> Assertion),
    brvQueryVisibleCarrier :: !(
      CarrierAddr ctx Carrier prop ->
      runtime ->
      Map RowTupleKey Multiplicity
    ),
    brvQueryVisibleContext :: !(
      ctx ->
      runtime ->
      RelationalSection ctx Carrier prop
    ),
    brvCarrierEvidence :: !(runtime -> [evidence]),
    brvLiveCarrierAddrs :: !(runtime -> Set (CarrierAddr ctx Carrier prop))
  }

data BlueRoseExpectations rule ctx prop = BlueRoseExpectations
  { breExpectedMutationFiredRules :: !(Set rule),
    breExpectedMutationBlockedRules :: !(Set rule),
    breExpectedMutationDirtyKeys :: !IntSet,
    breExpectedMutationDirtyTopo :: !IntSet,
    breExpectedInitialH1Supports :: !(Set (Set ctx)),
    breExpectedRepairAddrs :: !(Set (CarrierAddr ctx Carrier prop)),
    breExpectedRepairFiredRules :: !(Set rule),
    breExpectedRepairBlockedRules :: !(Set rule),
    breMaxRepairAddrs :: !Int,
    breUntouchedCarrierFloor :: !Int,
    breVisibleContextsToCheck :: !(Set ctx),
    breVisibleCarriersToCheck :: !(Set (CarrierAddr ctx Carrier prop))
  }
  deriving stock (Eq, Show)

data BlueRoseCase
  rule
  rewriteProgram
  rewriteMutation
  rewriteWorld
  rewriteRun
  rewriteErr
  rewritePatchErr
  repairRun
  repairErr
  runtime
  runtimeErr
  ctx
  prop
  evidence
  descent
  obstruction
  witness = BlueRoseCase
  { brcName :: !String,
    brcInitialWorld :: !rewriteWorld,
    brcRewriteProgram :: !rewriteProgram,
    brcMutation :: !rewriteMutation,
    brcRunRewriteMutation ::
      rewriteProgram ->
      rewriteMutation ->
      rewriteWorld ->
      Either rewriteErr (rewriteWorld, BlueRoseRewriteTrace rule rewriteRun),
    brcRewritePatchOracle ::
      rewriteProgram ->
      rewriteMutation ->
      rewriteWorld ->
      rewriteWorld ->
      rewriteRun ->
      Either rewritePatchErr QuotientPatch,
    brcInitialRuntime :: !runtime,
    brcRuntimeView :: !(BlueRoseRuntimeView runtime ctx prop evidence runtimeErr),
    brcObstructionReport ::
      runtime ->
      CoverCohomologyReport ctx descent obstruction (WitnessStalk witness),
    brcRepair ::
      rewriteWorld ->
      runtime ->
      CoverCohomologyReport ctx descent obstruction (WitnessStalk witness) ->
      Either repairErr (rewriteWorld, BlueRoseRepairTrace rule repairRun ctx prop),
    brcBatchOracle ::
      rewriteWorld ->
      BlueRoseBatchOracle ctx prop,
    brcEvidenceRules ::
      evidence ->
      Set rule,
    brcExpectations :: !(BlueRoseExpectations rule ctx prop)
  }

blueRoseTest ::
  forall rule rewriteProgram rewriteMutation rewriteWorld rewriteRun rewriteErr rewritePatchErr repairRun repairErr runtime runtimeErr ctx prop evidence descent obstruction witness.
  ( Ord rule,
    Show rule,
    Ord ctx,
    Show ctx,
    Ord prop,
    Show prop,
    Show rewriteErr,
    Show rewritePatchErr,
    Show repairErr,
    Show runtimeErr
  ) =>
  BlueRoseCase
    rule
    rewriteProgram
    rewriteMutation
    rewriteWorld
    rewriteRun
    rewriteErr
    rewritePatchErr
    repairRun
    repairErr
    runtime
    runtimeErr
    ctx
    prop
    evidence
    descent
    obstruction
    witness ->
  TestTree
blueRoseTest blueRose =
  testGroup
    ("blue rose rewrite holonomy: " <> brcName blueRose)
    [ testCase
        "rewrite creates no local conflict, creates H1 obstruction, repair clears it carrier-locally"
        (runBlueRose blueRose)
    ]

runBlueRose ::
  forall rule rewriteProgram rewriteMutation rewriteWorld rewriteRun rewriteErr rewritePatchErr repairRun repairErr runtime runtimeErr ctx prop evidence descent obstruction witness.
  ( Ord rule,
    Show rule,
    Ord ctx,
    Show ctx,
    Ord prop,
    Show prop,
    Show rewriteErr,
    Show rewritePatchErr,
    Show repairErr,
    Show runtimeErr
  ) =>
  BlueRoseCase
    rule
    rewriteProgram
    rewriteMutation
    rewriteWorld
    rewriteRun
    rewriteErr
    rewritePatchErr
    repairRun
    repairErr
    runtime
    runtimeErr
    ctx
    prop
    evidence
    descent
    obstruction
    witness ->
  Assertion
runBlueRose br = do
  (worldAfterMutation, mutationTrace) <-
    expectRightWith "run rewrite mutation" $
      brcRunRewriteMutation br
        (brcRewriteProgram br)
        (brcMutation br)
        (brcInitialWorld br)

  let expectations =
        brcExpectations br
      runtimeView =
        brcRuntimeView br
      mutationPatch =
        brtPatch mutationTrace

  expectedRewritePatch <-
    expectRightWith "recompute rewrite-produced quotient patch" $
      brcRewritePatchOracle br
        (brcRewriteProgram br)
        (brcMutation br)
        (brcInitialWorld br)
        worldAfterMutation
        (brtRun mutationTrace)

  mutationPatch @?= expectedRewritePatch

  brtFiredRules mutationTrace
    @?= breExpectedMutationFiredRules expectations

  brtBlockedRules mutationTrace
    @?= breExpectedMutationBlockedRules expectations

  scopeDeps (qpScope mutationPatch)
    @?= breExpectedMutationDirtyKeys expectations

  scopeTopo (qpScope mutationPatch)
    @?= breExpectedMutationDirtyTopo expectations

  assertBool
    "rewrite mutation patch must contain at least one atom delta"
    (not (IntMap.null (qpEvents mutationPatch)))

  runtimeAfterMutation <-
    expectRightWith "apply rewrite-produced quotient patch" $
      brvApplyQuotientPatch runtimeView mutationPatch (brcInitialRuntime br)

  assertRuntimeDrained runtimeView runtimeAfterMutation
  brvAssertCarrierIndexReplay runtimeView runtimeAfterMutation
  assertVisibleEqualsBatch br worldAfterMutation runtimeAfterMutation

  assertRewriteEvidencePresent
    (brcEvidenceRules br)
    (breExpectedMutationFiredRules expectations)
    runtimeView
    runtimeAfterMutation

  let reportAfterMutation =
        brcObstructionReport br runtimeAfterMutation

  assertBlueRoseObstructed expectations reportAfterMutation

  (worldAfterRepair, repairTrace) <-
    expectRightWith "repair blue rose obstruction" $
      brcRepair br worldAfterMutation runtimeAfterMutation reportAfterMutation

  assertRepairConeExact expectations repairTrace

  runtimeAfterRepair <-
    expectRightWith "apply repair-produced quotient patch" $
      brvApplyQuotientPatch runtimeView (brrPatch repairTrace) runtimeAfterMutation

  assertRuntimeDrained runtimeView runtimeAfterRepair
  brvAssertCarrierIndexReplay runtimeView runtimeAfterRepair
  assertVisibleEqualsBatch br worldAfterRepair runtimeAfterRepair

  assertUntouchedCarriersStable
    runtimeView
    expectations
    runtimeAfterMutation
    runtimeAfterRepair

  let reportAfterRepair =
        brcObstructionReport br runtimeAfterRepair

  assertBlueRoseRepaired reportAfterRepair

assertBlueRoseObstructed ::
  (Ord ctx, Show ctx) =>
  BlueRoseExpectations rule ctx prop ->
  CoverCohomologyReport ctx descent obstruction (WitnessStalk witness) ->
  Assertion
assertBlueRoseObstructed expectations report = do
  assertBool
    "expected no local C1 conflicts; Blue Rose failure must be global/non-descending"
    (null (runCohomologyObservation ObserveLocalC1Conflicts report))

  assertBool
    "expected at least one H1 obstruction"
    (not (null (runCohomologyObservation ObserveH1Obstructions report)))

  runCohomologyObservation ObserveH1Supports report
    @?= breExpectedInitialH1Supports expectations

assertBlueRoseRepaired ::
  Ord ctx =>
  CoverCohomologyReport ctx descent obstruction (WitnessStalk witness) ->
  Assertion
assertBlueRoseRepaired report = do
  assertBool
    "repair must not introduce local C1 conflicts"
    (null (runCohomologyObservation ObserveLocalC1Conflicts report))

  assertBool
    "repair must clear every H1 obstruction"
    (null (runCohomologyObservation ObserveH1Obstructions report))

assertRepairConeExact ::
  (Ord rule, Show rule, Ord ctx, Show ctx, Ord prop, Show prop) =>
  BlueRoseExpectations rule ctx prop ->
  BlueRoseRepairTrace rule repairRun ctx prop ->
  Assertion
assertRepairConeExact expectations repairTrace = do
  brrTouchedAddrs repairTrace
    @?= breExpectedRepairAddrs expectations

  brrFiredRules repairTrace
    @?= breExpectedRepairFiredRules expectations

  brrBlockedRules repairTrace
    @?= breExpectedRepairBlockedRules expectations

  assertBool
    ( "repair widened past configured carrier cone: "
        <> show (Set.size (brrTouchedAddrs repairTrace))
        <> " > "
        <> show (breMaxRepairAddrs expectations)
    )
    (Set.size (brrTouchedAddrs repairTrace) <= breMaxRepairAddrs expectations)

  assertBool
    "repair patch must contain at least one atom delta"
    (not (IntMap.null (qpEvents (brrPatch repairTrace))))

assertUntouchedCarriersStable ::
  (Ord ctx, Show ctx, Ord prop, Show prop) =>
  BlueRoseRuntimeView runtime ctx prop evidence runtimeErr ->
  BlueRoseExpectations rule ctx prop ->
  runtime ->
  runtime ->
  Assertion
assertUntouchedCarriersStable runtimeView expectations before after = do
  let touched =
        breExpectedRepairAddrs expectations
      universe =
        brvLiveCarrierAddrs runtimeView before <> brvLiveCarrierAddrs runtimeView after
      untouched =
        Set.difference universe touched
      unstable =
        Set.filter
          (\addr -> brvQueryVisibleCarrier runtimeView addr before /= brvQueryVisibleCarrier runtimeView addr after)
          untouched

  assertBool
    ( "expected at least "
        <> show (breUntouchedCarrierFloor expectations)
        <> " untouched carriers, got "
        <> show (Set.size untouched)
    )
    (Set.size untouched >= breUntouchedCarrierFloor expectations)

  assertBool
    ("untouched carriers changed: " <> show unstable)
    (Set.null unstable)

assertVisibleEqualsBatch ::
  (Ord ctx, Show ctx, Ord prop, Show prop) =>
  BlueRoseCase
    rule
    rewriteProgram
    rewriteMutation
    rewriteWorld
    rewriteRun
    rewriteErr
    rewritePatchErr
    repairRun
    repairErr
    runtime
    runtimeErr
    ctx
    prop
    evidence
    descent
    obstruction
    witness ->
  rewriteWorld ->
  runtime ->
  Assertion
assertVisibleEqualsBatch br world runtime = do
  let expectations =
        brcExpectations br
      runtimeView =
        brcRuntimeView br
      oracle =
        brcBatchOracle br world

  traverse_
    (\contextValue ->
       brvQueryVisibleContext runtimeView contextValue runtime
         @?= Map.findWithDefault
           emptyRelationalSection
           contextValue
           (bboVisibleContexts oracle)
    )
    (Set.toAscList (breVisibleContextsToCheck expectations))

  traverse_
    (\addr ->
       brvQueryVisibleCarrier runtimeView addr runtime
         @?= Map.findWithDefault
           Map.empty
           addr
           (bboVisibleCarriers oracle)
    )
    (Set.toAscList (breVisibleCarriersToCheck expectations))

assertRuntimeDrained ::
  Show runtimeErr =>
  BlueRoseRuntimeView runtime ctx prop evidence runtimeErr ->
  runtime ->
  Assertion
assertRuntimeDrained runtimeView runtime = do
  assertBool
    "runtime queue must be empty after applyQuotientPatch"
    (brvRuntimeQueueDrained runtimeView runtime)

  assertRight_ "runtime queue remains drainable" $
    brvDrainRuntimeQueue runtimeView runtime

assertRewriteEvidencePresent ::
  (Ord rule, Show rule) =>
  (evidence -> Set rule) ->
  Set rule ->
  BlueRoseRuntimeView runtime ctx prop evidence runtimeErr ->
  runtime ->
  Assertion
assertRewriteEvidencePresent evidenceRules expectedRules runtimeView runtime = do
  let observedRules =
        foldMap evidenceRules (brvCarrierEvidence runtimeView runtime)

  assertBool
    ( "carrier evidence lost rewrite provenance; expected subset "
        <> show expectedRules
        <> ", observed "
        <> show observedRules
    )
    (expectedRules `Set.isSubsetOf` observedRules)

emptyRelationalSection ::
  RelationalSection ctx carrier prop
emptyRelationalSection =
  RelationalSection
    { rsCarriers = Map.empty
    }

assertRight_ ::
  Show err =>
  String ->
  Either err value ->
  Assertion
assertRight_ label result =
  expectRightWith label result *> pure ()

expectRightWith ::
  Show err =>
  String ->
  Either err value ->
  IO value
expectRightWith label result =
  expectRight (first (\err -> label <> ": " <> show err) result)
