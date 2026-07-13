{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The circuit denotation-seal law family: advance-replay over a generated
-- batch sequence integrates to the eager denotation, quantified over
-- representative circuit shapes.  Feedback is never exempt — the fixpoint shape
-- carries the same seal as the acyclic plane.
module Moonlight.Differential.Effect.Laws.Circuit
  ( lawBundles,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Void
  ( Void,
  )
import Moonlight.Differential.Algebra.ZSet
  ( ZSet,
    zsetFromList,
    zsetSize,
    zsetToAscList,
    zsetUnions,
  )
import Moonlight.Differential.Circuit
  ( Circuit,
    CircuitBatch,
    CircuitBuilder,
    ForeignKernel (..),
    InputPort,
    Node,
    aggregateNode,
    buildCircuit,
    distinctNode,
    emptyCircuitBatch,
    feedInput,
    filterNode,
    fixpointNode,
    foreignNode,
    indexByNode,
    inputNode,
    joinNodes,
    mapNode,
    withSealedCircuit,
  )
import Moonlight.Differential.Collection
  ( collectionFromList,
    collectionToAscList,
    distinctCollection,
    filterCollection,
    indexCollectionBy,
    iterateCollection,
    joinCollections,
    mapCollection,
  )
import Moonlight.Differential.Effect.Harness.Circuit
  ( advanceAgreesWithIncrementalizeOfDenotation,
    advanceReplayIntegratesToDenotation,
    circuitEagerAgreesWithCollection,
  )
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "circuit"
      [ quickCheckLawDefinition CircuitLinearAdvanceAgreesWithIncrementalize propCircuitLinear,
        quickCheckLawDefinition CircuitLinearAdvanceIsIncrementalizeOfDenotation propCircuitLinearIncrementalize,
        quickCheckLawDefinition CircuitJoinAdvanceAgreesWithIncrementalize propCircuitJoin,
        quickCheckLawDefinition CircuitSharedArrangementAdvanceAgreesWithIncrementalize propCircuitSharedArrangement,
        quickCheckLawDefinition CircuitAggregateAdvanceAgreesWithIncrementalize propCircuitAggregate,
        quickCheckLawDefinition CircuitDistinctAdvanceAgreesWithIncrementalize propCircuitDistinct,
        quickCheckLawDefinition CircuitFixpointAdvanceAgreesWithIncrementalize propCircuitFixpoint,
        quickCheckLawDefinition CircuitLawfulForeignKernelAdvanceAgreesWithIncrementalize propCircuitForeign,
        quickCheckLawDefinition CircuitEagerDenotationAgreesWithCollectionAlgebra propCircuitEagerCollection
      ]
  ]

-- Shared runner: build the shape, seal it, and check the denotation predicate
-- at the chosen output over the assembled batch sequence.  All three rank-2
-- arguments are independent of the existential region variable, so the pure raw
-- data captured by the generators threads through untouched.
runCircuitLaw ::
  forall (ports :: Type -> Type) value.
  (Ord value, Show value) =>
  (forall s. CircuitBuilder s Void Int (ports s)) ->
  (forall s. ports s -> Node s value) ->
  (forall s. ports s -> ([CircuitBatch s Int], CircuitBatch s Int)) ->
  QC.Property
runCircuitLaw builder pickOutput pickBatches =
  case buildCircuit builder of
    Left refusal ->
      QC.counterexample ("circuit build refused: " <> show refusal) False
    Right sealed ->
      withSealedCircuit sealed $ \circuit ports ->
        let (batches, wholeBatch) = pickBatches ports
         in advanceReplayIntegratesToDenotation circuit (pickOutput ports) batches wholeBatch

-- A single input port feeds every batch its step delta; the whole batch feeds
-- the integrated delta.  Reused by every single-input shape.
singlePortBatches ::
  Ord value =>
  InputPort s value ->
  [[(value, Int)]] ->
  ([CircuitBatch s Int], CircuitBatch s Int)
singlePortBatches port steps =
  ( [feedInput port (zsetFromList step) emptyCircuitBatch | step <- steps],
    feedInput port (zsetUnions (fmap zsetFromList steps)) emptyCircuitBatch
  )

--------------------------------------------------------------------------------
-- Linear plane: map then filter.  A linear query is delta-transparent, so the
-- integral seal witnesses that the pipeline never smuggles integrate/
-- differentiate ceremony onto the linear plane.
--------------------------------------------------------------------------------

data LinearPorts s
  = LinearPorts (InputPort s Int) (Node s Int)

propCircuitLinear :: QC.Property
propCircuitLinear =
  QC.forAll (genSteps (genSignedRow (QC.choose (0, 9)))) $ \steps ->
    runCircuitLaw
      linearBuilder
      (\(LinearPorts _ output) -> output)
      (\(LinearPorts source _) -> singlePortBatches source steps)

linearBuilder :: CircuitBuilder s Void Int (LinearPorts s)
linearBuilder = do
  (source, raw) <- inputNode
  shifted <- mapNode (+ 10) raw
  evens <- filterNode even shifted
  pure (LinearPorts source evens)

--------------------------------------------------------------------------------
-- The literal incrementalize witness: per-batch advance deltas on the same
-- linear pipeline equal @incrementalize@ — the named Stream-calculus transform
-- — of the pipeline's pointwise denotation over the delta stream.  The integral
-- flagship above is the integral of this statement; this binds it by name.
--------------------------------------------------------------------------------

propCircuitLinearIncrementalize :: QC.Property
propCircuitLinearIncrementalize =
  QC.forAll (genSteps (genSignedRow (QC.choose (0, 9)))) $ \steps ->
    case buildCircuit linearBuilder of
      Left refusal ->
        QC.counterexample ("circuit build refused: " <> show refusal) False
      Right sealed ->
        withSealedCircuit sealed $ \circuit (LinearPorts source output) ->
          advanceAgreesWithIncrementalizeOfDenotation
            circuit
            output
            source
            linearDenotation
            (fmap zsetFromList steps)

-- The pipeline's pure denotation, stated independently of the builder: shift
-- by ten, keep the even values.
linearDenotation :: ZSet Int Int -> ZSet Int Int
linearDenotation input =
  zsetFromList
    [ (value + 10, weight)
    | (value, weight) <- zsetToAscList input,
      even (value + 10)
    ]

--------------------------------------------------------------------------------
-- Bilinear join: the Leibniz delta rule integrated back to the full join.
--------------------------------------------------------------------------------

data JoinPorts s
  = JoinPorts
      (InputPort s (Int, String))
      (InputPort s (Int, Char))
      (Node s (Int, (Int, String), (Int, Char)))

propCircuitJoin :: QC.Property
propCircuitJoin =
  QC.forAll (genSteps genJoinStep) $ \steps ->
    runCircuitLaw
      joinBuilder
      (\(JoinPorts _ _ output) -> output)
      (\(JoinPorts left right _) -> joinBatches left right steps)
  where
    genJoinStep =
      (,)
        <$> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ["p", "q"])
        <*> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ['a', 'b'])

joinBuilder :: CircuitBuilder s Void Int (JoinPorts s)
joinBuilder = do
  (left, leftRows) <- inputNode
  (right, rightRows) <- inputNode
  leftIndex <- indexByNode fst leftRows
  rightIndex <- indexByNode fst rightRows
  joined <- joinNodes leftIndex rightIndex
  pure (JoinPorts left right joined)

joinBatches ::
  InputPort s (Int, String) ->
  InputPort s (Int, Char) ->
  [([((Int, String), Int)], [((Int, Char), Int)])] ->
  ([CircuitBatch s Int], CircuitBatch s Int)
joinBatches left right steps =
  ( [ feedInput left (zsetFromList leftStep) (feedInput right (zsetFromList rightStep) emptyCircuitBatch)
      | (leftStep, rightStep) <- steps
    ],
    feedInput
      left
      (zsetUnions (fmap (zsetFromList . fst) steps))
      (feedInput right (zsetUnions (fmap (zsetFromList . snd) steps)) emptyCircuitBatch)
  )

data SharedPorts s
  = SharedPorts
      (InputPort s (Int, String))
      (InputPort s (Int, Char))
      (InputPort s (Int, Bool))
      (Node s (Int, (Int, String), (Int, Char)))
      (Node s (Int, (Int, String), (Int, Bool)))
      (Node s (Int, (Int, Char), (Int, Bool)))
      (Node s (Int, (Int, String), (Int, String)))

propCircuitSharedArrangement :: QC.Property
propCircuitSharedArrangement =
  QC.forAll (genSteps genSharedStep) $ \steps ->
    QC.conjoin
      [ runCircuitLaw
          sharedBuilder
          (\(SharedPorts _ _ _ output _ _ _) -> output)
          (\(SharedPorts x y z _ _ _ _) -> sharedBatches x y z steps),
        runCircuitLaw
          sharedBuilder
          (\(SharedPorts _ _ _ _ output _ _) -> output)
          (\(SharedPorts x y z _ _ _ _) -> sharedBatches x y z steps),
        runCircuitLaw
          sharedBuilder
          (\(SharedPorts _ _ _ _ _ output _) -> output)
          (\(SharedPorts x y z _ _ _ _) -> sharedBatches x y z steps),
        runCircuitLaw
          sharedBuilder
          (\(SharedPorts _ _ _ _ _ _ output) -> output)
          (\(SharedPorts x y z _ _ _ _) -> sharedBatches x y z steps)
      ]
  where
    genSharedStep =
      (,,)
        <$> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ["p", "q"])
        <*> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ['a', 'b'])
        <*> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements [True, False])

sharedBuilder :: CircuitBuilder s Void Int (SharedPorts s)
sharedBuilder = do
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
  pure (SharedPorts x y z j1 j2 j3 jSelf)

sharedBatches ::
  InputPort s (Int, String) ->
  InputPort s (Int, Char) ->
  InputPort s (Int, Bool) ->
  [([((Int, String), Int)], [((Int, Char), Int)], [((Int, Bool), Int)])] ->
  ([CircuitBatch s Int], CircuitBatch s Int)
sharedBatches x y z steps =
  ( [ feedInput x (zsetFromList xStep) $
        feedInput y (zsetFromList yStep) $
          feedInput z (zsetFromList zStep) emptyCircuitBatch
      | (xStep, yStep, zStep) <- steps
    ],
    feedInput
      x
      (zsetUnions (fmap (\(xStep, _, _) -> zsetFromList xStep) steps))
      ( feedInput
          y
          (zsetUnions (fmap (\(_, yStep, _) -> zsetFromList yStep) steps))
          (feedInput z (zsetUnions (fmap (\(_, _, zStep) -> zsetFromList zStep) steps)) emptyCircuitBatch)
      )
  )

--------------------------------------------------------------------------------
-- Aggregate: group-by then reduce by support size.  The organ retires the old
-- reduced value and emits the new one; integration must recover the reduction.
--------------------------------------------------------------------------------

data AggregatePorts s
  = AggregatePorts (InputPort s (Char, Int)) (Node s (Char, Int))

propCircuitAggregate :: QC.Property
propCircuitAggregate =
  QC.forAll (genSteps (genSignedRow ((,) <$> QC.elements ['a', 'b'] <*> QC.choose (1, 3)))) $ \steps ->
    runCircuitLaw
      aggregateBuilder
      (\(AggregatePorts _ output) -> output)
      (\(AggregatePorts source _) -> singlePortBatches source steps)

aggregateBuilder :: CircuitBuilder s Void Int (AggregatePorts s)
aggregateBuilder = do
  (source, rows) <- inputNode
  grouped <- indexByNode fst rows
  sized <- aggregateNode zsetSize grouped
  pure (AggregatePorts source sized)

--------------------------------------------------------------------------------
-- Distinct: multiplicity clamped to support.  Generated over the positive cone
-- distinct is defined on; support-departure under retraction is pinned by the
-- CircuitSpec fixture, which stays in-domain (insert then retract).
--------------------------------------------------------------------------------

data DistinctPorts s
  = DistinctPorts (InputPort s String) (Node s String)

propCircuitDistinct :: QC.Property
propCircuitDistinct =
  QC.forAll (genSteps (genPositiveRow (QC.elements ["a", "b", "c"]))) $ \steps ->
    runCircuitLaw
      distinctBuilder
      (\(DistinctPorts _ output) -> output)
      (\(DistinctPorts source _) -> singlePortBatches source steps)

distinctBuilder :: CircuitBuilder s Void Int (DistinctPorts s)
distinctBuilder = do
  (source, rows) <- inputNode
  support <- distinctNode rows
  pure (DistinctPorts source support)

--------------------------------------------------------------------------------
-- Fixpoint: transitive closure.  Generated over acyclic edge sets (i < j) so
-- the weighted semi-naive closure converges within budget; both the eager and
-- the incremental paths run the same organ, so the seal holds for feedback too.
--------------------------------------------------------------------------------

data ClosurePorts s
  = ClosurePorts (InputPort s (Int, Int)) (Node s (Int, Int))

propCircuitFixpoint :: QC.Property
propCircuitFixpoint =
  QC.forAll (genSteps (genPositiveRow genAcyclicEdge)) $ \steps ->
    runCircuitLaw
      closureBuilder
      (\(ClosurePorts _ output) -> output)
      (\(ClosurePorts source _) -> singlePortBatches source steps)
  where
    genAcyclicEdge = do
      source <- QC.choose (1, 4)
      target <- QC.choose (source + 1, 5)
      pure (source, target)

closureBuilder :: CircuitBuilder s Void Int (ClosurePorts s)
closureBuilder = do
  (edgesPort, edges) <- inputNode
  closure <-
    fixpointNode (SemiNaiveBudget 64) edges $ \frontier -> do
      byTarget <- indexByNode snd frontier
      bySource <- indexByNode fst edges
      hops <- joinNodes byTarget bySource
      mapNode (\(_, (source, _), (_, target)) -> (source, target)) hops
  pure (ClosurePorts edgesPort closure)

--------------------------------------------------------------------------------
-- Foreign node: the obligation is caller-owned.  The substrate seals a foreign
-- kernel iff its step rule is the delta rule of its denotation; an unlawful pair
-- is the caller's fault and the substrate never rescues it.  The witness is a
-- lawful kernel — weight doubling, which is linear and therefore delta-
-- transparent — so the same integral seal holds through the foreign boundary.
--------------------------------------------------------------------------------

data ForeignPorts s
  = ForeignPorts (InputPort s Int) (Node s Int)

propCircuitForeign :: QC.Property
propCircuitForeign =
  QC.forAll (genSteps (genSignedRow (QC.choose (0, 9)))) $ \steps ->
    runCircuitLaw
      foreignBuilder
      (\(ForeignPorts _ output) -> output)
      (\(ForeignPorts source _) -> singlePortBatches source steps)

foreignBuilder :: CircuitBuilder s Void Int (ForeignPorts s)
foreignBuilder = do
  (source, rows) <- inputNode
  doubled <- foreignNode doublingKernel rows
  pure (ForeignPorts source doubled)

doublingKernel :: ForeignKernel Void Int Int Int
doublingKernel = kernel
  where
    kernel =
      ForeignKernel
        { foreignStep = \delta -> Right (zsetUnions [delta, delta], kernel),
          foreignDenote = \collection -> zsetUnions [collection, collection]
        }

--------------------------------------------------------------------------------
-- Spec/value agreement: the eager circuit denotation equals the Collection
-- reference algebra.  Covers every shape whose builder combinator has an exact
-- Collection namesake with a matching value type: linear (map/filter), join
-- (joinCollections), distinct (distinctCollection), and fixpoint
-- (iterateCollection — the same semiNaiveFixpoint organ at the same budget, its
-- body mirrored combinator-for-combinator).  Aggregate is deliberately ABSENT:
-- the circuit's @aggregateNode zsetSize@ emits @(key, size)@ pairs at weight
-- one, whereas Collection's @countCollectionByKey@ carries the count in the
-- weight of a bare @key@ value — a different denotation, not a namesake.  The
-- circuit aggregate is strictly the more general node (any reducer); its eager
-- faithfulness is the operator-level GroupViewAdvanceRebuildsIntegratedView.
--------------------------------------------------------------------------------

propCircuitEagerCollection :: QC.Property
propCircuitEagerCollection =
  QC.conjoin
    [ linearEagerCheck,
      joinEagerCheck,
      distinctEagerCheck,
      fixpointEagerCheck
    ]

linearEagerCheck :: QC.Property
linearEagerCheck =
  QC.forAll (genSteps (genSignedRow (QC.choose (0, 9)))) $ \steps ->
    runCircuitEagerLaw
      linearBuilder
      (\(LinearPorts _ output) -> output)
      (\(LinearPorts source _) -> snd (singlePortBatches source steps))
      (collectionToAscList (filterCollection even (mapCollection (+ 10) (collectionFromList (concat steps)))))

joinEagerCheck :: QC.Property
joinEagerCheck =
  QC.forAll (genSteps genJoinStep) $ \steps ->
    runCircuitEagerLaw
      joinBuilder
      (\(JoinPorts _ _ output) -> output)
      (\(JoinPorts left right _) -> snd (joinBatches left right steps))
      ( collectionToAscList
          ( joinCollections
              (indexCollectionBy fst (collectionFromList (concatMap fst steps)))
              (indexCollectionBy fst (collectionFromList (concatMap snd steps)))
          )
      )
  where
    genJoinStep =
      (,)
        <$> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ["p", "q"])
        <*> genSignedRow ((,) <$> QC.choose (0, 3) <*> QC.elements ['a', 'b'])

distinctEagerCheck :: QC.Property
distinctEagerCheck =
  QC.forAll (genSteps (genPositiveRow (QC.elements ["a", "b", "c"]))) $ \steps ->
    runCircuitEagerLaw
      distinctBuilder
      (\(DistinctPorts _ output) -> output)
      (\(DistinctPorts source _) -> snd (singlePortBatches source steps))
      (collectionToAscList (distinctCollection (collectionFromList (concat steps))))

fixpointEagerCheck :: QC.Property
fixpointEagerCheck =
  QC.forAll (genSteps (genPositiveRow genAcyclicEdge)) $ \steps ->
    let edges = collectionFromList (concat steps)
        closureStep frontier =
          mapCollection (\(_, (source, _), (_, target)) -> (source, target))
            (joinCollections (indexCollectionBy snd frontier) (indexCollectionBy fst edges))
     in case iterateCollection (SemiNaiveBudget 64) closureStep edges of
          Left _ ->
            QC.counterexample "collection iterate diverged (acyclic generation must converge)" False
          Right closure ->
            runCircuitEagerLaw
              closureBuilder
              (\(ClosurePorts _ output) -> output)
              (\(ClosurePorts source _) -> snd (singlePortBatches source steps))
              (collectionToAscList closure)
  where
    genAcyclicEdge = do
      source <- QC.choose (1, 4)
      target <- QC.choose (source + 1, 5)
      pure (source, target)

-- Build, seal, and check the eager-denotation predicate at the chosen output
-- over the integrated batch, against the pre-computed Collection denotation.
runCircuitEagerLaw ::
  forall (ports :: Type -> Type) value.
  (Ord value, Show value) =>
  (forall s. CircuitBuilder s Void Int (ports s)) ->
  (forall s. ports s -> Node s value) ->
  (forall s. ports s -> CircuitBatch s Int) ->
  [(value, Int)] ->
  QC.Property
runCircuitEagerLaw builder pickOutput pickWhole collectionDenotation =
  case buildCircuit builder of
    Left refusal ->
      QC.counterexample ("circuit build refused: " <> show refusal) False
    Right sealed ->
      withSealedCircuit sealed $ \circuit ports ->
        circuitEagerAgreesWithCollection circuit (pickOutput ports) (pickWhole ports) collectionDenotation

--------------------------------------------------------------------------------
-- Generators.  A batch sequence is a short list of short rows; signed rows
-- exercise retraction, positive rows stay in a delta operator's positive cone.
--------------------------------------------------------------------------------

genSteps :: QC.Gen step -> QC.Gen [step]
genSteps genStep = do
  count <- QC.choose (0, 4)
  QC.vectorOf count genStep

genSignedRow :: QC.Gen value -> QC.Gen [(value, Int)]
genSignedRow genValue = do
  count <- QC.choose (0, 4)
  QC.vectorOf count ((,) <$> genValue <*> QC.elements [-2, -1, 1, 2])

genPositiveRow :: QC.Gen value -> QC.Gen [(value, Int)]
genPositiveRow genValue = do
  count <- QC.choose (0, 4)
  QC.vectorOf count ((,) <$> genValue <*> QC.choose (1, 3))
