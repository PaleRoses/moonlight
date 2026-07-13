module CircuitSpec
  ( tests,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Void
  ( Void,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Circuit
  ( Circuit,
    CircuitAdvanceError (..),
    CircuitBatch,
    CircuitBuilder,
    CircuitOutputs,
    CircuitOutputError,
    ForeignKernel (..),
    IndexedNode,
    InputPort,
    Node,
    advanceCircuit,
    aggregateNode,
    buildCircuit,
    distinctNode,
    emptyCircuitBatch,
    evaluateCircuit,
    feedInput,
    filterNode,
    fixpointNode,
    foreignNode,
    indexByNode,
    indexedOutputDelta,
    inputNode,
    joinNodes,
    mapNode,
    nodeId,
    outputDelta,
    withSealedCircuit,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
  )
import Moonlight.Differential.Operator.Join
  ( indexedDeltaJoin,
  )
import Moonlight.Differential.Operator.Linear
  ( indexBy,
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
    "circuit carrier"
    [ testCase "Linear pipeline pushes deltas through map and filter" linearPipelineTracksDeltas,
      testCase "Join is bilinear across arrival order and same-batch deltas" joinBilinearAcrossBatches,
      testCase "Fan-out joins over one shared input agree with hand-computed deltas" fanOutJoinsOverSharedInputMatchHandComputedDeltas,
      testCase "Sealed joins agree byte-for-byte with the private-integral reference" sealedJoinsAgreeWithPrivateIntegralReference,
      testCase "Aggregate retires the old reduced value and emits the new one" aggregateRetiresOldReduced,
      testCase "Distinct clamps multiplicity and reports support departure only" distinctClampsSupport,
      testCase "Fixpoint closure advances incrementally through insert and retract" closureAdvancesIncrementally,
      testCase "Fixpoint rederives a doubly-supported apex when one bridge retracts" closureRederivesAcrossADiamond,
      testCase "Fixpoint reports typed divergence when the budget exhausts" closureDivergesUnderBudget,
      testCase "Foreign fault surfaces typed and leaves the prior circuit standing" foreignFaultIsTransactional,
      testCase "Feeding one port twice in a batch merges the deltas additively" feedInputMergesAdditively,
      testCase "Advance replay integrates to the eager denotation" replayAgreesWithDenotation
    ]

withCircuit ::
  (forall s. CircuitBuilder s fault Int (ports s)) ->
  (forall s. Circuit s fault Int -> ports s -> Assertion) ->
  Assertion
withCircuit builder consume =
  case buildCircuit builder of
    Left refusal ->
      assertFailure ("circuit build refused: " <> show refusal)
    Right sealed ->
      withSealedCircuit sealed consume

advanceOrFail ::
  Show fault =>
  CircuitBatch s Int ->
  Circuit s fault Int ->
  IO (CircuitOutputs s Int, Circuit s fault Int)
advanceOrFail batch circuit =
  either
    (\refusal -> assertFailure ("advance refused: " <> show refusal))
    pure
    (advanceCircuit batch circuit)

zsetShould ::
  (Show a, Ord a) =>
  String ->
  [(a, Int)] ->
  Either CircuitOutputError (ZSet.ZSet a Int) ->
  Assertion
zsetShould label expected actual =
  assertEqual label (Right expected) (fmap ZSet.zsetToAscList actual)

data LinearPorts s
  = LinearPorts (InputPort s Int) (Node s Int)

linearPipelineTracksDeltas :: Assertion
linearPipelineTracksDeltas =
  withCircuit @Void builder $ \circuit (LinearPorts source evens) -> do
    (firstOut, afterFirst) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(1, 1), (2, 1)]) emptyCircuitBatch)
        circuit
    zsetShould "first batch keeps only the even image" [(12, 1)] (outputDelta evens firstOut)
    (secondOut, _) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(2, -1), (4, 1)]) emptyCircuitBatch)
        afterFirst
    zsetShould
      "second batch retracts and inserts through the pipeline"
      [(12, -1), (14, 1)]
      (outputDelta evens secondOut)
  where
    builder :: CircuitBuilder s Void Int (LinearPorts s)
    builder = do
      (source, raw) <- inputNode
      shifted <- mapNode (+ 10) raw
      evens <- filterNode even shifted
      pure (LinearPorts source evens)

data JoinPorts s
  = JoinPorts
      (InputPort s (Int, String))
      (InputPort s (Int, Char))
      (IndexedNode s Int (Int, String))
      (Node s (Int, (Int, String), (Int, Char)))

joinBilinearAcrossBatches :: Assertion
joinBilinearAcrossBatches =
  withCircuit @Void builder $ \circuit (JoinPorts left right leftIndex joined) -> do
    (firstOut, afterFirst) <-
      advanceOrFail
        (feedInput left (ZSet.zsetFromList [((1, "x"), 1)]) emptyCircuitBatch)
        circuit
    zsetShould "left alone joins nothing" [] (outputDelta joined firstOut)
    assertEqual
      "indexed output mirrors the arranged delta"
      (Right (ZSet.indexedZSetToAscList (indexBy fst (ZSet.zsetFromList [((1, "x"), 1)]))))
      (fmap ZSet.indexedZSetToAscList (indexedOutputDelta leftIndex firstOut))
    (secondOut, afterSecond) <-
      advanceOrFail
        (feedInput right (ZSet.zsetFromList [((1, 'c'), 1)]) emptyCircuitBatch)
        afterFirst
    zsetShould
      "right arrival joins against the integrated left"
      [((1, (1, "x"), (1, 'c')), 1)]
      (outputDelta joined secondOut)
    (thirdOut, afterThird) <-
      advanceOrFail
        (feedInput left (ZSet.zsetFromList [((1, "x"), -1)]) emptyCircuitBatch)
        afterSecond
    zsetShould
      "left retraction retracts the joined row"
      [((1, (1, "x"), (1, 'c')), -1)]
      (outputDelta joined thirdOut)
    (fourthOut, _) <-
      advanceOrFail
        ( feedInput left (ZSet.zsetFromList [((2, "y"), 1)]) $
            feedInput right (ZSet.zsetFromList [((2, 'd'), 1)]) emptyCircuitBatch
        )
        afterThird
    zsetShould
      "same-batch deltas join exactly once"
      [((2, (2, "y"), (2, 'd')), 1)]
      (outputDelta joined fourthOut)
  where
    builder :: CircuitBuilder s Void Int (JoinPorts s)
    builder = do
      (left, leftRows) <- inputNode
      (right, rightRows) <- inputNode
      leftIndex <- indexByNode fst leftRows
      rightIndex <- indexByNode fst rightRows
      joined <- joinNodes leftIndex rightIndex
      pure (JoinPorts left right leftIndex joined)

type SharedXRow = (Int, String)

type SharedYRow = (Int, Char)

type SharedZRow = (Int, Bool)

type SharedRawBatch =
  ( [(SharedXRow, Int)],
    [(SharedYRow, Int)],
    [(SharedZRow, Int)]
  )

data FanOutPorts s
  = FanOutPorts
      (InputPort s SharedXRow)
      (InputPort s SharedYRow)
      (InputPort s SharedZRow)
      (Node s (Int, SharedXRow, SharedYRow))
      (Node s (Int, SharedXRow, SharedZRow))
      (Node s (Int, SharedXRow, SharedXRow))

fanOutJoinsOverSharedInputMatchHandComputedDeltas :: Assertion
fanOutJoinsOverSharedInputMatchHandComputedDeltas =
  withCircuit @Void builder $ \circuit (FanOutPorts x y z j1 j2 jSelf) -> do
    let batch xRows yRows zRows =
          feedInput x (ZSet.zsetFromList xRows) $
            feedInput y (ZSet.zsetFromList yRows) $
              feedInput z (ZSet.zsetFromList zRows) emptyCircuitBatch
    (firstOut, afterFirst) <-
      advanceOrFail
        (batch [((1, "a"), 1), ((2, "b"), 1)] [((1, 'p'), 1)] [((2, True), 1)])
        circuit
    zsetShould
      "batch 1 j1"
      [((1, (1, "a"), (1, 'p')), 1)]
      (outputDelta j1 firstOut)
    zsetShould
      "batch 1 j2"
      [((2, (2, "b"), (2, True)), 1)]
      (outputDelta j2 firstOut)
    zsetShould
      "batch 1 jSelf"
      [((1, (1, "a"), (1, "a")), 1), ((2, (2, "b"), (2, "b")), 1)]
      (outputDelta jSelf firstOut)
    (secondOut, afterSecond) <-
      advanceOrFail
        (batch [((1, "a"), -1)] [] [])
        afterFirst
    zsetShould
      "batch 2 j1"
      [((1, (1, "a"), (1, 'p')), -1)]
      (outputDelta j1 secondOut)
    zsetShould
      "batch 2 j2"
      []
      (outputDelta j2 secondOut)
    zsetShould
      "batch 2 jSelf"
      [((1, (1, "a"), (1, "a")), -1)]
      (outputDelta jSelf secondOut)
    (thirdOut, _) <-
      advanceOrFail
        (batch [] [((2, 'q'), 1)] [((1, False), 1)])
        afterSecond
    zsetShould
      "batch 3 j1"
      [((2, (2, "b"), (2, 'q')), 1)]
      (outputDelta j1 thirdOut)
    zsetShould
      "batch 3 j2"
      []
      (outputDelta j2 thirdOut)
    zsetShould
      "batch 3 jSelf"
      []
      (outputDelta jSelf thirdOut)
  where
    builder :: CircuitBuilder s Void Int (FanOutPorts s)
    builder = do
      (x, xRows) <- inputNode
      (y, yRows) <- inputNode
      (z, zRows) <- inputNode
      xi <- indexByNode fst xRows
      yi <- indexByNode fst yRows
      zi <- indexByNode fst zRows
      j1 <- joinNodes xi yi
      j2 <- joinNodes xi zi
      jSelf <- joinNodes xi xi
      pure (FanOutPorts x y z j1 j2 jSelf)

data ReferencePorts s
  = ReferencePorts
      (InputPort s SharedXRow)
      (InputPort s SharedYRow)
      (InputPort s SharedZRow)
      (Node s (Int, SharedXRow, SharedYRow))
      (Node s (Int, SharedXRow, SharedZRow))
      (Node s (Int, SharedYRow, SharedZRow))
      (Node s (Int, SharedXRow, SharedXRow))

type JoinReference left right =
  ( ZSet.IndexedZSet Int left Int,
    ZSet.IndexedZSet Int right Int
  )

data PrivateReference
  = PrivateReference
      (JoinReference SharedXRow SharedYRow)
      (JoinReference SharedXRow SharedZRow)
      (JoinReference SharedYRow SharedZRow)
      (JoinReference SharedXRow SharedXRow)

data ReferenceOutputs
  = ReferenceOutputs
      (ZSet.ZSet (Int, SharedXRow, SharedYRow) Int)
      (ZSet.ZSet (Int, SharedXRow, SharedZRow) Int)
      (ZSet.ZSet (Int, SharedYRow, SharedZRow) Int)
      (ZSet.ZSet (Int, SharedXRow, SharedXRow) Int)

sealedJoinsAgreeWithPrivateIntegralReference :: Assertion
sealedJoinsAgreeWithPrivateIntegralReference =
  withCircuit @Void builder $ \circuit ports -> do
    _ <-
      foldM
        (advanceReferenceBatch ports)
        (circuit, emptyPrivateReference)
        (zip [1 :: Int ..] privateIntegralReferenceScript)
    pure ()
  where
    builder :: CircuitBuilder s Void Int (ReferencePorts s)
    builder = do
      (x, xRows) <- inputNode
      (y, yRows) <- inputNode
      (z, zRows) <- inputNode
      xi <- indexByNode fst xRows
      yi <- indexByNode fst yRows
      zi <- indexByNode fst zRows
      j1 <- joinNodes xi yi
      j2 <- joinNodes xi zi
      j3 <- joinNodes yi zi
      jSelf <- joinNodes xi xi
      pure (ReferencePorts x y z j1 j2 j3 jSelf)

privateIntegralReferenceScript :: [SharedRawBatch]
privateIntegralReferenceScript =
  [ ( [((1, "a"), 1), ((2, "b"), 1)],
      [((1, 'p'), 1)],
      []
    ),
    ( [],
      [((2, 'q'), 1)],
      [((2, True), 1), ((1, False), 1)]
    ),
    ( [((1, "a"), -1), ((3, "c"), 1)],
      [],
      [((1, False), -1)]
    ),
    ( [((2, "b"), -1)],
      [((1, 'p'), -1)],
      [((3, True), 1)]
    ),
    ( [((3, "c"), 1)],
      [((3, 'r'), 1)],
      [((2, True), -1)]
    )
  ]

advanceReferenceBatch ::
  ReferencePorts s ->
  (Circuit s Void Int, PrivateReference) ->
  (Int, SharedRawBatch) ->
  IO (Circuit s Void Int, PrivateReference)
advanceReferenceBatch ports@(ReferencePorts x y z _ _ _ _) (circuit, reference) (batchNumber, rawBatch) = do
  let (expected, nextReference) = privateReferenceStep reference rawBatch
  (out, nextCircuit) <- advanceOrFail (sharedCircuitBatch x y z rawBatch) circuit
  assertReferenceOutputs batchNumber ports out expected
  pure (nextCircuit, nextReference)

sharedCircuitBatch ::
  InputPort s SharedXRow ->
  InputPort s SharedYRow ->
  InputPort s SharedZRow ->
  SharedRawBatch ->
  CircuitBatch s Int
sharedCircuitBatch x y z (xRows, yRows, zRows) =
  feedInput x (ZSet.zsetFromList xRows) $
    feedInput y (ZSet.zsetFromList yRows) $
      feedInput z (ZSet.zsetFromList zRows) emptyCircuitBatch

emptyPrivateReference :: PrivateReference
emptyPrivateReference =
  PrivateReference (mempty, mempty) (mempty, mempty) (mempty, mempty) (mempty, mempty)

privateReferenceStep ::
  PrivateReference ->
  SharedRawBatch ->
  (ReferenceOutputs, PrivateReference)
privateReferenceStep (PrivateReference j1Reference j2Reference j3Reference jSelfReference) (xRows, yRows, zRows) =
  let dx = indexBy fst (ZSet.zsetFromList xRows)
      dy = indexBy fst (ZSet.zsetFromList yRows)
      dz = indexBy fst (ZSet.zsetFromList zRows)
      (j1Out, nextJ1Reference) = privateJoinStep j1Reference (dx, dy)
      (j2Out, nextJ2Reference) = privateJoinStep j2Reference (dx, dz)
      (j3Out, nextJ3Reference) = privateJoinStep j3Reference (dy, dz)
      (jSelfOut, nextJSelfReference) = privateJoinStep jSelfReference (dx, dx)
   in ( ReferenceOutputs j1Out j2Out j3Out jSelfOut,
        PrivateReference nextJ1Reference nextJ2Reference nextJ3Reference nextJSelfReference
      )

privateJoinStep ::
  (Ord left, Ord right) =>
  JoinReference left right ->
  (ZSet.IndexedZSet Int left Int, ZSet.IndexedZSet Int right Int) ->
  (ZSet.ZSet (Int, left, right) Int, JoinReference left right)
privateJoinStep (il, ir) (dl, dr) =
  (indexedDeltaJoin il dl ir dr, (il <> dl, ir <> dr))

assertReferenceOutputs ::
  Int ->
  ReferencePorts s ->
  CircuitOutputs s Int ->
  ReferenceOutputs ->
  Assertion
assertReferenceOutputs
  batchNumber
  (ReferencePorts _ _ _ j1 j2 j3 jSelf)
  out
  (ReferenceOutputs expectedJ1 expectedJ2 expectedJ3 expectedJSelf) = do
    assertEqual
      ("batch " <> show batchNumber <> " j1")
      (Right (ZSet.zsetToAscList expectedJ1))
      (fmap ZSet.zsetToAscList (outputDelta j1 out))
    assertEqual
      ("batch " <> show batchNumber <> " j2")
      (Right (ZSet.zsetToAscList expectedJ2))
      (fmap ZSet.zsetToAscList (outputDelta j2 out))
    assertEqual
      ("batch " <> show batchNumber <> " j3")
      (Right (ZSet.zsetToAscList expectedJ3))
      (fmap ZSet.zsetToAscList (outputDelta j3 out))
    assertEqual
      ("batch " <> show batchNumber <> " jSelf")
      (Right (ZSet.zsetToAscList expectedJSelf))
      (fmap ZSet.zsetToAscList (outputDelta jSelf out))

data AggregatePorts s
  = AggregatePorts (InputPort s (Char, Int)) (Node s (Char, Int))

aggregateRetiresOldReduced :: Assertion
aggregateRetiresOldReduced =
  withCircuit @Void builder $ \circuit (AggregatePorts source sized) -> do
    (firstOut, afterFirst) <-
      advanceOrFail
        ( feedInput
            source
            (ZSet.zsetFromList [(('a', 1), 1), (('a', 2), 1), (('b', 7), 1)])
            emptyCircuitBatch
        )
        circuit
    zsetShould
      "fresh groups emit their reduced values"
      [(('a', 2), 1), (('b', 1), 1)]
      (outputDelta sized firstOut)
    (secondOut, afterSecond) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(('a', 3), 1)]) emptyCircuitBatch)
        afterFirst
    zsetShould
      "a grown group retires its old reduced value"
      [(('a', 2), -1), (('a', 3), 1)]
      (outputDelta sized secondOut)
    (thirdOut, _) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(('b', 7), -1)]) emptyCircuitBatch)
        afterSecond
    zsetShould
      "a vanished group retires without a replacement"
      [(('b', 1), -1)]
      (outputDelta sized thirdOut)
  where
    builder :: CircuitBuilder s Void Int (AggregatePorts s)
    builder = do
      (source, rows) <- inputNode
      grouped <- indexByNode fst rows
      sized <- aggregateNode ZSet.zsetSize grouped
      pure (AggregatePorts source sized)

data DistinctPorts s
  = DistinctPorts (InputPort s String) (Node s String)

distinctClampsSupport :: Assertion
distinctClampsSupport =
  withCircuit @Void builder $ \circuit (DistinctPorts source support) -> do
    (firstOut, afterFirst) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [("x", 2)]) emptyCircuitBatch)
        circuit
    zsetShould "multiplicity two clamps to one" [("x", 1)] (outputDelta support firstOut)
    (secondOut, afterSecond) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [("x", -1)]) emptyCircuitBatch)
        afterFirst
    zsetShould
      "a retraction that keeps support is silent"
      []
      (outputDelta support secondOut)
    (thirdOut, _) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [("x", -1)]) emptyCircuitBatch)
        afterSecond
    zsetShould
      "the final retraction reports support departure"
      [("x", -1)]
      (outputDelta support thirdOut)
  where
    builder :: CircuitBuilder s Void Int (DistinctPorts s)
    builder = do
      (source, rows) <- inputNode
      support <- distinctNode rows
      pure (DistinctPorts source support)

data ClosurePorts s
  = ClosurePorts (InputPort s (Int, Int)) (Node s (Int, Int))

closureBuilder ::
  SemiNaiveBudget ->
  CircuitBuilder s Void Int (ClosurePorts s)
closureBuilder budget = do
  (edgesPort, edges) <- inputNode
  closure <-
    fixpointNode budget edges $ \frontier -> do
      byTarget <- indexByNode snd frontier
      bySource <- indexByNode fst edges
      hops <- joinNodes byTarget bySource
      mapNode (\(_, (source, _), (_, target)) -> (source, target)) hops
  pure (ClosurePorts edgesPort closure)

closureAdvancesIncrementally :: Assertion
closureAdvancesIncrementally =
  withCircuit @Void (closureBuilder (SemiNaiveBudget 64)) $
    \circuit (ClosurePorts edges closure) -> do
      (firstOut, afterFirst) <-
        advanceOrFail
          (feedInput edges (ZSet.zsetFromList [((1, 2), 1), ((2, 3), 1)]) emptyCircuitBatch)
          circuit
      zsetShould
        "the first batch emits the whole closure"
        [((1, 2), 1), ((1, 3), 1), ((2, 3), 1)]
        (outputDelta closure firstOut)
      (secondOut, afterSecond) <-
        advanceOrFail
          (feedInput edges (ZSet.zsetFromList [((3, 4), 1)]) emptyCircuitBatch)
          afterFirst
      zsetShould
        "a new edge emits exactly the new reachability"
        [((1, 4), 1), ((2, 4), 1), ((3, 4), 1)]
        (outputDelta closure secondOut)
      (thirdOut, _) <-
        advanceOrFail
          (feedInput edges (ZSet.zsetFromList [((2, 3), -1)]) emptyCircuitBatch)
          afterSecond
      zsetShould
        "an edge retraction retracts every path through it"
        [((1, 3), -1), ((1, 4), -1), ((2, 3), -1), ((2, 4), -1)]
        (outputDelta closure thirdOut)

closureRederivesAcrossADiamond :: Assertion
closureRederivesAcrossADiamond =
  withCircuit @Void (closureBuilder (SemiNaiveBudget 64)) $
    \circuit (ClosurePorts edges closure) -> do
      (firstOut, afterFirst) <-
        advanceOrFail
          ( feedInput
              edges
              ( ZSet.zsetFromList
                  [((1, 2), 1), ((1, 3), 1), ((2, 4), 1), ((3, 4), 1)]
              )
              emptyCircuitBatch
          )
          circuit
      zsetShould
        "the diamond closes with a doubly-supported apex"
        [((1, 2), 1), ((1, 3), 1), ((1, 4), 1), ((2, 4), 1), ((3, 4), 1)]
        (outputDelta closure firstOut)
      (secondOut, afterSecond) <-
        advanceOrFail
          (feedInput edges (ZSet.zsetFromList [((2, 4), -1)]) emptyCircuitBatch)
          afterFirst
      zsetShould
        "retracting one bridge rederives the apex through the other"
        [((2, 4), -1)]
        (outputDelta closure secondOut)
      (thirdOut, _) <-
        advanceOrFail
          (feedInput edges (ZSet.zsetFromList [((3, 4), -1)]) emptyCircuitBatch)
          afterSecond
      zsetShould
        "retracting the last bridge lets the unsupported apex fall"
        [((1, 4), -1), ((3, 4), -1)]
        (outputDelta closure thirdOut)

closureDivergesUnderBudget :: Assertion
closureDivergesUnderBudget =
  withCircuit @Void (closureBuilder (SemiNaiveBudget 1)) $
    \circuit (ClosurePorts edges closure) ->
      case advanceCircuit
        ( feedInput
            edges
            (ZSet.zsetFromList [((1, 2), 1), ((2, 3), 1), ((3, 4), 1)])
            emptyCircuitBatch
        )
        circuit of
        Right _ ->
          assertFailure "budget one accepted a two-round closure"
        Left (CircuitForeignFault faultedId fault) ->
          assertFailure
            ("expected divergence, got foreign fault at " <> show faultedId <> ": " <> show fault)
        Left
          CircuitFixpointDiverged
            { divergedNodeId = diverged,
              divergedRoundsSpent = rounds,
              divergedResidualSize = residual,
              divergedAccumulatedSize = accumulated
            } -> do
            assertEqual "the fixpoint node is named" (nodeId closure) diverged
            assertEqual "the budget was spent" 1 rounds
            -- The residual/accumulated SIZES are the divergence witness of a
            -- particular saturation schedule, not the divergence contract.
            -- Eager semi-naive and the DRed advance path exhaust budget 1 at
            -- different iteration offsets (DRed's insert seeds its frontier
            -- with one immediate-consequence pass, so it reaches further per
            -- budget unit); both are honest witnesses of "budget 1 cannot
            -- confirm this closure". The contract is: a non-empty unfinished
            -- frontier and a partial accumulation bounded by the seed floor
            -- and the true closure (six facts on this three-edge chain).
            assertBool "an unfinished frontier is reported" (residual > 0)
            assertBool
              "the partial accumulation lies between the seed and the closure"
              (accumulated >= 3 && accumulated <= 6)
        Left obstruction ->
          assertFailure ("expected divergence, got " <> show obstruction)

data ForeignPorts s
  = ForeignPorts (InputPort s Int) (Node s Int)

foreignFaultIsTransactional :: Assertion
foreignFaultIsTransactional =
  withCircuit builder $ \circuit (ForeignPorts source guarded) -> do
    (firstOut, afterFirst) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(1, 1)]) emptyCircuitBatch)
        circuit
    zsetShould "clean deltas pass through" [(1, 1)] (outputDelta guarded firstOut)
    case advanceCircuit
      (feedInput source (ZSet.zsetFromList [(13, 1)]) emptyCircuitBatch)
      afterFirst of
      Right _ ->
        assertFailure "the guard kernel accepted thirteen"
      Left (CircuitFixpointDiverged {divergedNodeId = diverged}) ->
        assertFailure ("expected a foreign fault, got divergence at " <> show diverged)
      Left (CircuitForeignFault faultedId fault) -> do
        assertEqual "the foreign node is named" (nodeId guarded) faultedId
        assertEqual "the caller-owned fault is carried" "refuses thirteen" fault
      Left obstruction ->
        assertFailure ("expected foreign fault, got " <> show obstruction)
    (retryOut, _) <-
      advanceOrFail
        (feedInput source (ZSet.zsetFromList [(2, 1)]) emptyCircuitBatch)
        afterFirst
    zsetShould
      "the prior circuit value stands after a refused advance"
      [(2, 1)]
      (outputDelta guarded retryOut)
  where
    builder :: CircuitBuilder s String Int (ForeignPorts s)
    builder = do
      (source, rows) <- inputNode
      guarded <- foreignNode guardKernel rows
      pure (ForeignPorts source guarded)

    guardKernel :: ForeignKernel String Int Int Int
    guardKernel =
      kernel
      where
        kernel =
          ForeignKernel
            { foreignStep = \delta ->
                if any ((== 13) . fst) (ZSet.zsetToAscList delta)
                  then Left "refuses thirteen"
                  else Right (delta, kernel),
              foreignDenote = id
            }

feedInputMergesAdditively :: Assertion
feedInputMergesAdditively =
  withCircuit @Void builder $ \circuit (LinearPorts source echo) -> do
    (out, _) <-
      advanceOrFail
        ( feedInput source (ZSet.zsetFromList [(1, 1)]) $
            feedInput source (ZSet.zsetFromList [(1, 2), (5, 1)]) emptyCircuitBatch
        )
        circuit
    zsetShould
      "two feeds to one port add"
      [(1, 3), (5, 1)]
      (outputDelta echo out)
  where
    builder :: CircuitBuilder s Void Int (LinearPorts s)
    builder = do
      (source, rows) <- inputNode
      pure (LinearPorts source rows)

replayAgreesWithDenotation :: Assertion
replayAgreesWithDenotation =
  withCircuit @Void builder $ \circuit (JoinPorts left right _ joined) -> do
    let leftBatches =
          [ ZSet.zsetFromList [((1, "x"), 1), ((2, "y"), 1)],
            ZSet.zsetFromList [((1, "x"), -1)],
            ZSet.zsetFromList [((3, "z"), 1)]
          ]
        rightBatches =
          [ ZSet.zsetFromList [((2, 'c'), 1)],
            ZSet.zsetFromList [((1, 'b'), 1), ((3, 'e'), 1)],
            ZSet.zsetFromList [((2, 'c'), -1), ((3, 'f'), 1)]
          ]
        batchAt index =
          feedInput left (leftBatches !! index) $
            feedInput right (rightBatches !! index) emptyCircuitBatch
    (firstOut, afterFirst) <- advanceOrFail (batchAt 0) circuit
    (secondOut, afterSecond) <- advanceOrFail (batchAt 1) afterFirst
    (thirdOut, _) <- advanceOrFail (batchAt 2) afterSecond
    let integratedOutput =
          fmap ZSet.zsetUnions
            ( traverse
                (outputDelta joined)
                [firstOut, secondOut, thirdOut]
            )
        wholeBatch =
          feedInput left (ZSet.zsetUnions leftBatches) $
            feedInput right (ZSet.zsetUnions rightBatches) emptyCircuitBatch
    case evaluateCircuit wholeBatch circuit of
      Left refusal ->
        assertFailure ("eager evaluation refused: " <> show refusal)
      Right eagerOut ->
        assertEqual
          "replayed advance integrates to the eager denotation"
          (fmap ZSet.zsetToAscList (outputDelta joined eagerOut))
          (fmap ZSet.zsetToAscList integratedOutput)
  where
    builder :: CircuitBuilder s Void Int (JoinPorts s)
    builder = do
      (left, leftRows) <- inputNode
      (right, rightRows) <- inputNode
      leftIndex <- indexByNode fst leftRows
      rightIndex <- indexByNode fst rightRows
      joined <- joinNodes leftIndex rightIndex
      pure (JoinPorts left right leftIndex joined)
