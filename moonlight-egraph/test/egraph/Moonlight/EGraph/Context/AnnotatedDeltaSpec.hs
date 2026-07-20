{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Public laws for the regional annotated quotient.
--
-- The eager graph interpreter lives in test-algebras.  Production is observed
-- only through typed point queries and opaque row projections.
module Moonlight.EGraph.Context.AnnotatedDeltaSpec
  ( tests,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (foldlM, traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId (..),
    Operator (..),
    UnionFindAllocationError,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextDeltaError (..),
    ContextMutationTrace (..),
    ContextRebaseBatch,
    ContextRebaseReport (..),
    activateContext,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextAnalysisValueAt,
    contextMerge,
    contextRepresentativeAt,
    withEmptyContextEGraph,
    materializeAmbientPayloadFor,
    planContextMerges,
    stageContextMerges,
    stageGlobalMerge,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Change (emtInsertedClassKeys, observedClassUnionPairs)
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    absorbedRowsAtKey,
    annotatedEquivalentRegion,
    annotatedInhabitedRegion,
    annotatedRepresentativeKeyAt,
    annotatedRowsAtKey,
    contextAnnotatedDeltaBuckets,
    deriveAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegSite,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Relational.Source (structuralRowsForOperator)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
    eGraphAnalysis,
    eGraphStore,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    coarseAnatomyLattice,
  )
import Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice,
  )
import Moonlight.Sheaf.Context.Region
  ( regionMemberKey,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    contextObjectKeyFor,
    preparedRegionTable,
  )
import Moonlight.Sheaf.Section.Context.Payload
  ( payloadMapToAnalysisMap,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "regional annotated quotient"
    [ testCase "regional rows equal the eager pointwise oracle" testRegionalRowsAgreeWithOracle,
      testCase "regional representatives induce the eager partitions" testRegionalRepresentativesAgreeWithOracle,
      testCase "cached and freshly derived sections agree observably" testCachedSectionAgreesWithFreshDerivation,
      testCase "equivalence regions are exactly their pointwise verdicts" testEquivalentRegionPointwiseLaw,
      testCase "contextual analysis and payloads equal eager materialization" testContextualAnalysisOracle,
      testCase "bulk author descent equals sequential eager points and preserves trace order" testBulkAuthorDescent,
      testCase "batched asMake/asJoinChanged repair equals stepwise repair" testBatchedAnalysisOracle,
      testCase "contextual construction after a changed merge is rejected" testConstructionAfterMergeRejected,
      testCase "base-revision recompilation subsumes a frozen local transaction" testBaseRevisionSubsumesLocalTrace,
      testCase "contextual SCC analysis agrees on a quotient-created cycle" testCyclicAnalysisOracle,
      testCase "a singleton site is the ordinary quotient degeneration" testSingletonDegeneration
    ]

type ContextEGraphAnatomy owner =
  ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion

type AuthoredScript = [(AnatomyRegion, (ClassId, ClassId))]

data ArithFixture = ArithFixture
  { afBase :: !(EGraph Arith.ArithF Arith.NodeCount),
    afClassA :: !ClassId,
    afClassB :: !ClassId,
    afClassC :: !ClassId,
    afClassD :: !ClassId
  }

arithFixture :: Either UnionFindAllocationError ArithFixture
arithFixture = do
  let graph0 = emptyEGraph Arith.analysisSpec
  (classA, graph1) <- addTerm (Arith.numTerm 1) graph0
  (classB, graph2) <- addTerm (Arith.numTerm 2) graph1
  (classC, graph3) <- addTerm (Arith.numTerm 3) graph2
  (classD, graph4) <- addTerm (Arith.numTerm 4) graph3
  (_, graph5) <- addTerm (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 1)) graph4
  (_, graph6) <- addTerm (Arith.addTermNode (Arith.numTerm 3) (Arith.numTerm 3)) graph5
  pure (ArithFixture graph6 classA classB classC classD)

armScript :: ArithFixture -> AuthoredScript
armScript fixture =
  [ (ArmLeft, (afClassA fixture, afClassB fixture)),
    (ArmRight, (afClassB fixture, afClassC fixture))
  ]

layeredScript :: ArithFixture -> AuthoredScript
layeredScript fixture =
  armScript fixture
    <> [ (Torso, (afClassC fixture, afClassD fixture)),
         (Upper, (afClassA fixture, afClassD fixture))
       ]

anatomyContexts :: [AnatomyRegion]
anatomyContexts = [minBound .. maxBound]

fixtureTags :: [Arith.ArithF ()]
fixtureTags =
  [ Arith.Num 1,
    Arith.Num 2,
    Arith.Num 3,
    Arith.Num 4,
    Arith.Add () ()
  ]

fixtureClasses :: ArithFixture -> [ClassId]
fixtureClasses fixture =
  [ afClassA fixture,
    afClassB fixture,
    afClassC fixture,
    afClassD fixture
  ]

withBuiltContextGraph ::
  ArithFixture ->
  AuthoredScript ->
  (forall owner. ContextEGraphAnatomy owner -> Assertion) ->
  Assertion
withBuiltContextGraph fixture authoredScript useContextGraph =
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \emptyContextGraph ->
    either
      assertFailure
      useContextGraph
      ( foldlM
          ( \contextGraph (contextValue, (leftClass, rightClass)) ->
              either
                (Left . show)
                Right
                (contextMerge contextValue leftClass rightClass contextGraph)
          )
          emptyContextGraph
          authoredScript
      )

expectRight :: Show obstruction => Either obstruction value -> IO value
expectRight = either (assertFailure . show) pure

contextKeyAt ::
  (Ord c, Show c) =>
  c ->
  ContextEGraph owner f a c ->
  Either String (ContextObjectKey owner)
contextKeyAt contextValue contextGraph =
  either (Left . show) Right (contextObjectKeyFor (cegSite contextGraph) contextValue)

canonicalRowSet ::
  EGraph Arith.ArithF analysis ->
  [(Int, [Int])] ->
  Set (Int, [Int])
canonicalRowSet graph =
  Set.fromList
    . fmap
      ( \(rootKey, childKeys) ->
          ( canonicalKey rootKey,
            fmap canonicalKey childKeys
          )
      )
  where
    canonicalKey = classIdKey . canonicalizeClassId graph . ClassId

regionalCompositeRows ::
  ContextEGraph owner Arith.ArithF analysis c ->
  AnnotatedDeltaBuckets owner Arith.ArithF ->
  ContextObjectKey owner ->
  Arith.ArithF () ->
  [(Int, [Int])]
regionalCompositeRows contextGraph buckets contextKey tag =
  Set.toAscList
    ( Set.union
        (Set.difference baseRows absorbedRows)
        variantRows
    )
  where
    baseRows =
      Set.fromList
        (structuralRowsForOperator (eGraphStore (cegBase contextGraph)) (Operator tag))
    absorbedRows = Set.fromList (absorbedRowsAtKey tag contextKey buckets)
    variantRows = Set.fromList (annotatedRowsAtKey tag contextKey buckets)

assertRowsAgreeAt ::
  (Ord c, Show c) =>
  ContextEGraph owner Arith.ArithF analysis c ->
  AnnotatedDeltaBuckets owner Arith.ArithF ->
  c ->
  Assertion
assertRowsAgreeAt contextGraph buckets contextValue = do
  contextKey <- either assertFailure pure (contextKeyAt contextValue contextGraph)
  eagerGraph <- expectRight (materializedContextGraphAt contextValue contextGraph)
  traverse_
    ( \tag ->
        canonicalRowSet eagerGraph (regionalCompositeRows contextGraph buckets contextKey tag)
          @?= canonicalRowSet
            eagerGraph
            (structuralRowsForOperator (eGraphStore eagerGraph) (Operator tag))
    )
    fixtureTags

testRegionalRowsAgreeWithOracle :: Assertion
testRegionalRowsAgreeWithOracle = do
  fixture <- expectRight arithFixture
  traverse_
    ( \script ->
        withBuiltContextGraph fixture script $ \contextGraph -> do
          buckets <- expectRight (deriveAnnotatedDeltaBuckets contextGraph)
          traverse_ (assertRowsAgreeAt contextGraph buckets) anatomyContexts
    )
    [armScript fixture, layeredScript fixture]

unorderedPairs :: [a] -> [(a, a)]
unorderedPairs values =
  case values of
    [] -> []
    value : remainingValues ->
      fmap ((,) value) remainingValues <> unorderedPairs remainingValues

testRegionalRepresentativesAgreeWithOracle :: Assertion
testRegionalRepresentativesAgreeWithOracle = do
  fixture <- expectRight arithFixture
  withBuiltContextGraph fixture (layeredScript fixture) $ \contextGraph -> do
    buckets <- expectRight (deriveAnnotatedDeltaBuckets contextGraph)
    traverse_
      ( \contextValue -> do
          contextKey <- either assertFailure pure (contextKeyAt contextValue contextGraph)
          eagerGraph <- expectRight (materializedContextGraphAt contextValue contextGraph)
          traverse_
            ( \(leftClass, rightClass) ->
                let regionalEquivalent =
                      annotatedRepresentativeKeyAt contextKey buckets (classIdKey leftClass)
                        == annotatedRepresentativeKeyAt contextKey buckets (classIdKey rightClass)
                    eagerEquivalent =
                      canonicalizeClassId eagerGraph leftClass
                        == canonicalizeClassId eagerGraph rightClass
                 in assertBool
                      ("partition mismatch at " <> show contextValue <> " for " <> show (leftClass, rightClass))
                      (regionalEquivalent == eagerEquivalent)
            )
            (unorderedPairs (fixtureClasses fixture))
      )
      anatomyContexts

observableSignature ::
  ArithFixture ->
  ContextEGraphAnatomy owner ->
  AnnotatedDeltaBuckets owner Arith.ArithF ->
  Either String [(AnatomyRegion, [Int], [(Arith.ArithF (), [(Int, [Int])])])]
observableSignature fixture contextGraph buckets =
  traverse signatureAt anatomyContexts
  where
    signatureAt contextValue = do
      contextKey <- contextKeyAt contextValue contextGraph
      pure
        ( contextValue,
          fmap
            (annotatedRepresentativeKeyAt contextKey buckets . classIdKey)
            (fixtureClasses fixture),
          fmap
            (\tag -> (tag, regionalCompositeRows contextGraph buckets contextKey tag))
            fixtureTags
        )

testCachedSectionAgreesWithFreshDerivation :: Assertion
testCachedSectionAgreesWithFreshDerivation = do
  fixture <- expectRight arithFixture
  withBuiltContextGraph fixture (layeredScript fixture) $ \contextGraph -> do
    freshBuckets <- expectRight (deriveAnnotatedDeltaBuckets contextGraph)
    observableSignature fixture contextGraph (contextAnnotatedDeltaBuckets contextGraph)
      @?= observableSignature fixture contextGraph freshBuckets

testEquivalentRegionPointwiseLaw :: Assertion
testEquivalentRegionPointwiseLaw = do
  fixture <- expectRight arithFixture
  let leftClass = afClassA fixture
      rightClass = afClassC fixture
  withBuiltContextGraph fixture (armScript fixture) $ \contextGraph -> do
    buckets <- expectRight (deriveAnnotatedDeltaBuckets contextGraph)
    let regionTable = preparedRegionTable (cegSite contextGraph)
        equivalentRegion =
          annotatedEquivalentRegion
            regionTable
            buckets
            (classIdKey leftClass)
            (classIdKey rightClass)
    traverse_
      ( \contextValue -> do
          contextKey <- either assertFailure pure (contextKeyAt contextValue contextGraph)
          leftRepresentative <- expectRight (contextRepresentativeAt contextValue leftClass contextGraph)
          rightRepresentative <- expectRight (contextRepresentativeAt contextValue rightClass contextGraph)
          regionMemberKey equivalentRegion contextKey
            @?= (leftRepresentative == rightRepresentative)
      )
      anatomyContexts

newtype BatchAnalysis = BatchAnalysis (Set Int)
  deriving stock (Eq, Ord, Show)

batchAnalysisSpec :: AnalysisSpec Arith.ArithF BatchAnalysis
batchAnalysisSpec =
  AnalysisSpec
    { asMake = batchAnalysisFromNode,
      asJoin = batchAnalysisJoin,
      asJoinChanged = \existing incoming ->
        let joined = batchAnalysisJoin existing incoming
         in (joined, joined /= existing)
    }

batchAnalysisFromNode :: Arith.ArithF BatchAnalysis -> BatchAnalysis
batchAnalysisFromNode arithNode =
  case arithNode of
    Arith.Num literalValue -> BatchAnalysis (Set.singleton literalValue)
    Arith.Var variableIndex -> BatchAnalysis (Set.singleton (negate variableIndex - 1))
    Arith.Add leftAnalysis rightAnalysis -> batchAnalysisJoin leftAnalysis rightAnalysis
    Arith.Mul leftAnalysis rightAnalysis -> batchAnalysisJoin leftAnalysis rightAnalysis
    Arith.Neg childAnalysis -> childAnalysis

batchAnalysisJoin :: BatchAnalysis -> BatchAnalysis -> BatchAnalysis
batchAnalysisJoin (BatchAnalysis leftValues) (BatchAnalysis rightValues) =
  BatchAnalysis (Set.union leftValues rightValues)

data BatchAnalysisFixture owner = BatchAnalysisFixture
  { bafGraph :: !(ContextEGraph owner Arith.ArithF BatchAnalysis AnatomyRegion),
    bafClassA :: !ClassId,
    bafClassB :: !ClassId,
    bafClassC :: !ClassId,
    bafAddAB :: !ClassId,
    bafAddBC :: !ClassId
  }

batchAnalysisFixture ::
  (forall owner. BatchAnalysisFixture owner -> result) ->
  Either UnionFindAllocationError result
batchAnalysisFixture useFixture = do
  let graph0 = emptyEGraph batchAnalysisSpec
  (classA, graph1) <- addTerm (Arith.numTerm 1) graph0
  (classB, graph2) <- addTerm (Arith.numTerm 2) graph1
  (classC, graph3) <- addTerm (Arith.numTerm 3) graph2
  (addAB, graph4) <- addTerm (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 2)) graph3
  (addBC, graph5) <- addTerm (Arith.addTermNode (Arith.numTerm 2) (Arith.numTerm 3)) graph4
  pure $
    withEmptyContextEGraph coarseAnatomyLattice graph5 $ \contextGraph ->
      useFixture
        BatchAnalysisFixture
          { bafGraph = contextGraph,
            bafClassA = classA,
            bafClassB = classB,
            bafClassC = classC,
            bafAddAB = addAB,
            bafAddBC = addBC
          }

withBatchAnalysisFixture ::
  (forall owner. BatchAnalysisFixture owner -> Assertion) ->
  Assertion
withBatchAnalysisFixture useFixture =
  either (assertFailure . show) id (batchAnalysisFixture useFixture)

batchAnalysisClasses :: BatchAnalysisFixture owner -> [ClassId]
batchAnalysisClasses fixture =
  [ bafClassA fixture,
    bafClassB fixture,
    bafClassC fixture,
    bafAddAB fixture,
    bafAddBC fixture
  ]

assertAnalysisAgreesAt ::
  AnatomyRegion ->
  [ClassId] ->
  ContextEGraph owner Arith.ArithF BatchAnalysis AnatomyRegion ->
  Assertion
assertAnalysisAgreesAt contextValue probeClasses contextGraph = do
  eagerGraph <- expectRight (materializedContextGraphAt contextValue contextGraph)
  payloads <- expectRight (materializeAmbientPayloadFor contextValue contextGraph)
  let payloadAnalysis = payloadMapToAnalysisMap payloads
  traverse_
    ( \classId -> do
        representative <- expectRight (contextRepresentativeAt contextValue classId contextGraph)
        contextualAnalysis <- expectRight (contextAnalysisValueAt contextValue classId contextGraph)
        let eagerRepresentative = canonicalizeClassId eagerGraph classId
            eagerAnalysis =
              IntMap.lookup (classIdKey eagerRepresentative) (eGraphAnalysis eagerGraph)
                <|> IntMap.lookup (classIdKey classId) (eGraphAnalysis eagerGraph)
            payloadValue = IntMap.lookup (classIdKey representative) payloadAnalysis
        contextualAnalysis @?= eagerAnalysis
        payloadValue @?= eagerAnalysis
    )
    probeClasses

assertAnalysisOracle ::
  ContextEGraph owner Arith.ArithF BatchAnalysis AnatomyRegion ->
  [ClassId] ->
  Assertion
assertAnalysisOracle contextGraph probeClasses =
  traverse_
    (\contextValue -> assertAnalysisAgreesAt contextValue probeClasses contextGraph)
    anatomyContexts

testContextualAnalysisOracle :: Assertion
testContextualAnalysisOracle = withBatchAnalysisFixture $ \fixture -> do
  contextGraph <-
    expectRight
      ( contextMerge
          ArmLeft
          (bafClassA fixture)
          (bafClassB fixture)
          (bafGraph fixture)
      )
  assertAnalysisOracle contextGraph (batchAnalysisClasses fixture)

testBulkAuthorDescent :: Assertion
testBulkAuthorDescent = do
  fixture <- expectRight arithFixture
  let leftClass = afClassA fixture
      rightClass = afClassB fixture
      expectedPair = (leftClass, rightClass)
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \initialGraph -> do
    let initialBatch = beginContextRebaseBatch initialGraph
    sequentialArmLeft <-
      expectRight (contextMerge ArmLeft leftClass rightClass initialGraph)
    sequentialGraph <-
      expectRight (contextMerge ArmRight leftClass rightClass sequentialArmLeft)
    bulkPlan <-
      expectRight (planContextMerges [ArmLeft, ArmLeft, ArmRight] leftClass rightClass initialBatch)
    bulkBatch <- expectRight (stageContextMerges bulkPlan initialBatch)
    (bulkReport, bulkGraph) <- expectRight (commitContextRebaseBatch bulkBatch)
    observableSignature fixture bulkGraph (contextAnnotatedDeltaBuckets bulkGraph)
      @?= observableSignature fixture sequentialGraph (contextAnnotatedDeltaBuckets sequentialGraph)
    traverse_
      (assertRowsAgreeAt bulkGraph (contextAnnotatedDeltaBuckets bulkGraph))
      anatomyContexts
    let traceValue = crrTrace bulkReport
    observedClassUnionPairs (cmtObservedLocalUnions traceValue)
      @?= [expectedPair, expectedPair]
    fmap observedClassUnionPairs (Map.lookup ArmLeft (cmtObservedLocalUnionsByContext traceValue))
      @?= Just [expectedPair]
    fmap observedClassUnionPairs (Map.lookup ArmRight (cmtObservedLocalUnionsByContext traceValue))
      @?= Just [expectedPair]

testBatchedAnalysisOracle :: Assertion
testBatchedAnalysisOracle = withBatchAnalysisFixture $ \fixture -> do
  let initialGraph = bafGraph fixture
      firstMerge = (bafClassA fixture, bafClassB fixture)
      secondMerge = (bafClassB fixture, bafClassC fixture)
      initialBatch = beginContextRebaseBatch initialGraph
  stepwiseGraph1 <-
    expectRight (contextMerge ArmLeft (fst firstMerge) (snd firstMerge) initialGraph)
  stepwiseGraph2 <-
    expectRight (contextMerge ArmLeft (fst secondMerge) (snd secondMerge) stepwiseGraph1)
  firstPlan <-
    expectRight (planContextMerges [ArmLeft] (fst firstMerge) (snd firstMerge) initialBatch)
  stagedFirst <- expectRight (stageContextMerges firstPlan initialBatch)
  secondPlan <-
    expectRight (planContextMerges [ArmLeft] (fst secondMerge) (snd secondMerge) stagedFirst)
  stagedSecond <- expectRight (stageContextMerges secondPlan stagedFirst)
  (_, batchedGraph) <- expectRight (commitContextRebaseBatch stagedSecond)
  assertAnalysisOracle stepwiseGraph2 (batchAnalysisClasses fixture)
  assertAnalysisOracle batchedGraph (batchAnalysisClasses fixture)
  traverse_
    ( \contextValue ->
        traverse_
          ( \classId ->
              contextAnalysisValueAt contextValue classId stepwiseGraph2
                @?= contextAnalysisValueAt contextValue classId batchedGraph
          )
          (batchAnalysisClasses fixture)
    )
    anatomyContexts

testConstructionAfterMergeRejected :: Assertion
testConstructionAfterMergeRejected = withBatchAnalysisFixture $ \(fixture :: BatchAnalysisFixture owner) -> do
  let initialGraph = bafGraph fixture
      initialBatch = beginContextRebaseBatch initialGraph
      assertConstructionRejected :: String -> ContextRebaseBatch owner Arith.ArithF a AnatomyRegion -> IO ()
      assertConstructionRejected label batchValue =
        case stageTermAtContext ArmRight (Arith.numTerm 99) batchValue of
          Left ContextConstructionAfterMerge -> pure ()
          Left otherError ->
            assertFailure (label <> " returned the wrong obstruction: " <> show otherError)
          Right _ ->
            assertFailure (label <> " admitted a contextual construction after a changed merge")
  localPlan <-
    expectRight
      ( planContextMerges
          [ArmLeft]
          (bafClassA fixture)
          (bafClassB fixture)
          initialBatch
      )
  localMergeBatch <- expectRight (stageContextMerges localPlan initialBatch)
  globalMergeBatch <-
    expectRight
      ( stageGlobalMerge
          (bafClassA fixture)
          (bafClassB fixture)
          initialBatch
      )
  assertConstructionRejected "changed local merge" localMergeBatch
  assertConstructionRejected "changed global merge" globalMergeBatch

testBaseRevisionSubsumesLocalTrace :: Assertion
testBaseRevisionSubsumesLocalTrace = withBatchAnalysisFixture $ \fixture -> do
  let insertedTerm = Arith.numTerm 7
      expectedPair = (bafClassA fixture, bafClassB fixture)
  initialGraph <-
    foldlM
      (\contextGraph contextValue -> expectRight (activateContext contextValue contextGraph))
      (bafGraph fixture)
      [ArmLeft, ArmRight, Torso]
  let initialBatch = beginContextRebaseBatch initialGraph
  frozenLocalPlan <-
    expectRight
      ( planContextMerges
          [ArmLeft]
          (fst expectedPair)
          (snd expectedPair)
          initialBatch
      )
  (bulkInsertedClass, constructionBatch) <-
    expectRight (stageTermAtContext ArmRight insertedTerm initialBatch)
  bulkMergeBatch <-
    expectRight (stageContextMerges frozenLocalPlan constructionBatch)
  (bulkReport, bulkGraph) <-
    expectRight (commitContextRebaseBatch bulkMergeBatch)

  (stepwiseInsertedClass, stepwiseConstructionBatch) <-
    expectRight
      ( stageTermAtContext
          ArmRight
          insertedTerm
          (beginContextRebaseBatch initialGraph)
      )
  (_, stepwiseConstructedGraph) <-
    expectRight (commitContextRebaseBatch stepwiseConstructionBatch)
  stepwiseGraph <-
    expectRight
      ( contextMerge
          ArmLeft
          (fst expectedPair)
          (snd expectedPair)
          stepwiseConstructedGraph
      )

  bulkInsertedClass @?= stepwiseInsertedClass
  let traceValue = crrTrace bulkReport
      probeClasses = bulkInsertedClass : batchAnalysisClasses fixture
      comparisonTags = Arith.Num 7 : fixtureTags
      bulkBuckets = contextAnnotatedDeltaBuckets bulkGraph
      stepwiseBuckets = contextAnnotatedDeltaBuckets stepwiseGraph
  IntSet.size (emtInsertedClassKeys (cmtBaseTrace traceValue)) @?= 1
  observedClassUnionPairs (cmtObservedLocalUnions traceValue) @?= [expectedPair]
  fmap observedClassUnionPairs (cmtObservedLocalUnionsByContext traceValue)
    @?= Map.singleton ArmLeft [expectedPair]
  assertAnalysisOracle bulkGraph probeClasses
  assertAnalysisOracle stepwiseGraph probeClasses
  traverse_
    ( \contextValue -> do
        bulkKey <- either assertFailure pure (contextKeyAt contextValue bulkGraph)
        stepwiseKey <- either assertFailure pure (contextKeyAt contextValue stepwiseGraph)
        traverse_
          ( \classId -> do
              contextRepresentativeAt contextValue classId bulkGraph
                @?= contextRepresentativeAt contextValue classId stepwiseGraph
              contextAnalysisValueAt contextValue classId bulkGraph
                @?= contextAnalysisValueAt contextValue classId stepwiseGraph
          )
          probeClasses
        traverse_
          ( \tag ->
              regionalCompositeRows bulkGraph bulkBuckets bulkKey tag
                @?= regionalCompositeRows stepwiseGraph stepwiseBuckets stepwiseKey tag
          )
          comparisonTags
    )
    anatomyContexts

testCyclicAnalysisOracle :: Assertion
testCyclicAnalysisOracle = withBatchAnalysisFixture $ \fixture -> do
  cyclicGraph <-
    expectRight
      ( contextMerge
          Torso
          (bafClassA fixture)
          (bafAddAB fixture)
          (bafGraph fixture)
      )
  assertAnalysisOracle cyclicGraph (batchAnalysisClasses fixture)

type SoleRegion :: Type
data SoleRegion = Sole
  deriving stock (Eq, Ord, Show)

soleLattice :: ContextLattice SoleRegion
soleLattice = singletonContextLattice Sole

testSingletonDegeneration :: Assertion
testSingletonDegeneration = do
  fixture <- expectRight arithFixture
  withEmptyContextEGraph soleLattice (afBase fixture) $ \emptyContextGraph -> do
    soleKey <- either assertFailure pure (contextKeyAt Sole emptyContextGraph)
    emptyBuckets <- expectRight (deriveAnnotatedDeltaBuckets emptyContextGraph)
    regionMemberKey (annotatedInhabitedRegion emptyBuckets) soleKey @?= False
    annotatedRepresentativeKeyAt soleKey emptyBuckets (classIdKey (afClassA fixture))
      @?= classIdKey (afClassA fixture)
    traverse_
      ( \tag -> do
          absorbedRowsAtKey tag soleKey emptyBuckets @?= []
          annotatedRowsAtKey tag soleKey emptyBuckets @?= []
      )
      fixtureTags
    mergedContextGraph <-
      expectRight
        ( contextMerge
            Sole
            (afClassA fixture)
            (afClassB fixture)
            emptyContextGraph
        )
    leftRepresentative <- expectRight (contextRepresentativeAt Sole (afClassA fixture) mergedContextGraph)
    rightRepresentative <- expectRight (contextRepresentativeAt Sole (afClassB fixture) mergedContextGraph)
    leftRepresentative @?= rightRepresentative
    assertRowsAgreeAt
      mergedContextGraph
      (contextAnnotatedDeltaBuckets mergedContextGraph)
      Sole

{-# LANGUAGE RankNTypes #-}
