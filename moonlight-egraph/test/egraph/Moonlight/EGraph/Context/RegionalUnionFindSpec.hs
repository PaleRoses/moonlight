{-# LANGUAGE RankNTypes #-}

-- | Public-behavior laws for the regional contextual quotient.
--
-- The kernel module is intentionally hidden.  These tests exercise descent,
-- gluing, compression, and symbolic-site compactness through 'ContextEGraph'
-- point queries, with the eager materialized interpreter as the independent
-- differential oracle.
module Moonlight.EGraph.Context.RegionalUnionFindSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldlM, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
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
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextAnalysisValueAt,
    contextMerge,
    contextRepresentativeAt,
    withEmptyContextEGraph,
    emptyContextEGraphFromSite,
    planContextMerges,
    stageContextMerges,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    AnnotatedDeltaMetrics (..),
    absorbedRowsAtKey,
    annotatedDeltaMetrics,
    annotatedRowsAtKey,
    contextAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegSite,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
    eGraphAnalysis,
    eGraphStore,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Diamond
  ( DiamondCtx (..),
  )
import Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
  )
import Moonlight.EGraph.Test.Context.Powerset
  ( PowersetTwinWorkload (..),
    powersetProbeContexts,
    powersetSubsets,
    powersetTwinAtoms,
    powersetTwinWorkload,
    symbolicPowersetSite,
  )
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.EGraph.Pure.Saturation.Rebuild
  ( RoundRebuildReport (..),
    runRoundRebuildReport,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Relational.Source
  ( structuralRowsForOperator,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext,
    singletonContextLattice,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    SiteEnumerability (..),
    contextObjectKeyFor,
    preparedRegionTable,
    preparedSiteEnumerability,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Moonlight.Sheaf.Descent.Context
  ( DescentReport (..),
    fullDescentCheck,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "regional union-find through the contextual API"
    [ testCase "sibling regions remain isolated" testSiblingIsolation,
      testCase "diamond overlap glues only on the meet of active regions" testDiamondOverlap,
      testCase "compression preserves the non-overlap residual" testResidualCompression,
      testCase "child-first and ancestor-first authoring are identical" testChildAncestorOrderIndependence,
      testCase "a singleton site degenerates to an ordinary quotient" testSingletonBehavior,
      testCase "dense and symbolic powerset sites decode identically" testDenseSymbolicAgreement,
      testCase "twenty symbolic atoms stay region-compact" testTwentyAtomCompactness,
      QC.testProperty
        "1,000 authored scripts agree with eager materialization and all orderings"
        (QC.withNumTests 1000 propRegionalScripts),
      QC.testProperty
        "generated chain/tree/diamond-stack sites agree with the eager oracle"
        (QC.withNumTests 120 propGeneratedScaleGeometries),
      QC.testProperty
        "generated symbolic powerset sites agree with the eager oracle"
        (QC.withNumTests 120 propGeneratedSymbolicPowersets)
    ]

data RegionalFixture owner context = RegionalFixture
  { rfClasses :: !(IntMap ClassId),
    rfGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount context)
  }

data RegionalStep = RegionalStep
  { rsContext :: !DiamondCtx,
    rsLeftKey :: !Int,
    rsRightKey :: !Int
  }
  deriving stock (Eq, Show)

newtype RegionalScript = RegionalScript [RegionalStep]
  deriving stock (Eq, Show)

data GeneratedMerge = GeneratedMerge
  { gmContextOrdinal :: !Int,
    gmLeftKey :: !Int,
    gmRightKey :: !Int
  }
  deriving stock (Eq, Show)

newtype GeneratedGeometryScript = GeneratedGeometryScript
  { ggsMerges :: [GeneratedMerge]
  }
  deriving stock (Eq, Show)

data GeometryLawReport = GeometryLawReport
  { glrRepresentativesAgreeWithOracle :: !Bool,
    glrRowsAgreeWithOracle :: !Bool,
    glrAnalysisAgreesWithOracle :: !Bool,
    glrDescentSatisfied :: !Bool,
    glrNoOpRebuildDeltaEmpty :: !Bool,
    glrNoOpRepresentativesStable :: !Bool,
    glrNoOpRowsStable :: !Bool,
    glrNoOpAnalysisStable :: !Bool
  }
  deriving stock (Eq, Show)

completeGeometryLawReport :: GeometryLawReport
completeGeometryLawReport =
  GeometryLawReport
    { glrRepresentativesAgreeWithOracle = True,
      glrRowsAgreeWithOracle = True,
      glrAnalysisAgreesWithOracle = True,
      glrDescentSatisfied = True,
      glrNoOpRebuildDeltaEmpty = True,
      glrNoOpRepresentativesStable = True,
      glrNoOpRowsStable = True,
      glrNoOpAnalysisStable = True
    }

instance QC.Arbitrary RegionalStep where
  arbitrary =
    RegionalStep
      <$> QC.elements [DBottom, DLeft, DRight, DTop]
      <*> QC.chooseInt (0, 3)
      <*> QC.chooseInt (0, 3)
  shrink step =
    [ step {rsLeftKey = leftKey}
      | leftKey <- QC.shrink (rsLeftKey step),
        leftKey >= 0,
        leftKey <= 3
    ]
      <> [ step {rsRightKey = rightKey}
           | rightKey <- QC.shrink (rsRightKey step),
             rightKey >= 0,
             rightKey <= 3
         ]
      <> [step {rsContext = DBottom} | rsContext step /= DBottom]

instance QC.Arbitrary RegionalScript where
  arbitrary =
    RegionalScript <$> QC.resize 8 (QC.listOf QC.arbitrary)
  shrink (RegionalScript steps) =
    RegionalScript <$> QC.shrinkList QC.shrink steps

instance QC.Arbitrary GeneratedMerge where
  arbitrary =
    GeneratedMerge
      <$> QC.chooseInt (0, 15)
      <*> QC.chooseInt (0, 3)
      <*> QC.chooseInt (0, 3)
  shrink generatedMerge =
    [ generatedMerge {gmContextOrdinal = contextOrdinal}
      | contextOrdinal <- shrinkBounded 0 15 (gmContextOrdinal generatedMerge)
    ]
      <> [ generatedMerge {gmLeftKey = leftKey}
           | leftKey <- shrinkBounded 0 3 (gmLeftKey generatedMerge)
         ]
      <> [ generatedMerge {gmRightKey = rightKey}
           | rightKey <- shrinkBounded 0 3 (gmRightKey generatedMerge)
         ]

instance QC.Arbitrary GeneratedGeometryScript where
  arbitrary =
    GeneratedGeometryScript <$> QC.resize 8 (QC.listOf1 QC.arbitrary)
  shrink (GeneratedGeometryScript merges) =
    GeneratedGeometryScript <$> QC.shrinkList QC.shrink merges

shrinkBounded :: Int -> Int -> Int -> [Int]
shrinkBounded lowerBound upperBound =
  filter (\value -> value >= lowerBound && value <= upperBound) . QC.shrink

diamondLattice :: Either String (ContextLattice DiamondCtx)
diamondLattice = first show latticeContext

arithBase :: Either UnionFindAllocationError (IntMap ClassId, EGraph Arith.ArithF Arith.NodeCount)
arithBase = do
  let graph0 = emptyEGraph Arith.analysisSpec
  (classA, graph1) <- addTerm (Arith.numTerm 1) graph0
  (classB, graph2) <- addTerm (Arith.numTerm 2) graph1
  (classC, graph3) <- addTerm (Arith.numTerm 3) graph2
  (classD, graph4) <- addTerm (Arith.numTerm 4) graph3
  (_, graph5) <- addTerm (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 2)) graph4
  (_, graph6) <- addTerm (Arith.addTermNode (Arith.numTerm 3) (Arith.numTerm 4)) graph5
  pure (IntMap.fromList [(0, classA), (1, classB), (2, classC), (3, classD)], graph6)

diamondFixture ::
  (forall owner. RegionalFixture owner DiamondCtx -> result) ->
  Either String result
diamondFixture useFixture = do
  contextLattice <- diamondLattice
  (classes, baseGraph) <- first show arithBase
  pure $
    withEmptyContextEGraph contextLattice baseGraph $ \contextGraph ->
      useFixture
        RegionalFixture
          { rfClasses = classes,
            rfGraph = contextGraph
          }

withDiamondFixture ::
  (forall owner. RegionalFixture owner DiamondCtx -> Assertion) ->
  Assertion
withDiamondFixture useFixture =
  either (assertFailure . show) id (diamondFixture useFixture)

buildGeneratedScaleGeometries ::
  GeneratedGeometryScript ->
  Either String [(Scale.ScaleSiteShape, Int, GeometryLawReport)]
buildGeneratedScaleGeometries script =
  traverse buildGeometry generatedScaleSiteCover
  where
    buildGeometry (shape, extent) = do
      site <- first show (generatedScaleSite shape extent)
      (classes, baseGraph) <- first show arithBase
      let contexts = Scale.scaleSiteContexts site
      withEmptyContextEGraph (Scale.scaleSiteLattice site) baseGraph $ \contextGraph -> do
        finalFixture <-
          applyGeneratedMerges
            contexts
            RegionalFixture
              { rfClasses = classes,
                rfGraph = contextGraph
              }
            (ggsMerges script)
        lawReport <- geometryLawReport contexts finalFixture
        pure (shape, extent, lawReport)

generatedScaleSiteCover :: [(Scale.ScaleSiteShape, Int)]
generatedScaleSiteCover =
  [(Scale.ScaleChain, contextCount) | contextCount <- [3 .. 7]]
    <> [(Scale.ScaleTree, contextCount) | contextCount <- [4 .. 7]]
    <> [(Scale.ScaleDiamondStack, diamondCount) | diamondCount <- [1 .. 3]]

generatedScaleSite :: Scale.ScaleSiteShape -> Int -> Either Scale.ScaleSiteError Scale.ScaleSite
generatedScaleSite shape extent =
  case shape of
    Scale.ScaleChain -> Scale.scaledChain extent
    Scale.ScaleTree -> Scale.scaledTree extent
    Scale.ScaleDiamondStack -> Scale.scaledDiamondStack extent

buildGeneratedPowersetGeometries ::
  GeneratedGeometryScript ->
  Either String [(Int, GeometryLawReport)]
buildGeneratedPowersetGeometries script =
  traverse buildGeometry [1 .. 4]
  where
    buildGeometry atomCount = do
      let atoms = take atomCount "abcd"
          contexts = powersetSubsets atoms
      (classes, baseGraph) <- first show arithBase
      contextCover <-
        maybe
          (Left "generated powerset context cover is empty")
          Right
          (NonEmpty.nonEmpty contexts)
      preparedResult <-
        first show $
          symbolicPowersetSite atoms $ \symbolicSite -> do
            finalFixture <-
              applyGeneratedMerges
                contextCover
                RegionalFixture
                  { rfClasses = classes,
                    rfGraph = emptyContextEGraphFromSite symbolicSite baseGraph
                  }
                (ggsMerges script)
            lawReport <- geometryLawReport contextCover finalFixture
            pure (atomCount, lawReport)
      preparedResult

applyGeneratedMerges ::
  (Ord context, Show context) =>
  NonEmpty context ->
  RegionalFixture owner context ->
  [GeneratedMerge] ->
  Either String (RegionalFixture owner context)
applyGeneratedMerges contexts =
  foldlM applyGeneratedMerge
  where
    contextCount = NonEmpty.length contexts
    indexedContexts =
      IntMap.fromList (zip [0 ..] (NonEmpty.toList contexts))
    applyGeneratedMerge fixture generatedMerge = do
      contextValue <-
        maybe
          ( Left
              ( "generated context ordinal did not normalize into the closed cover: "
                  <> show (gmContextOrdinal generatedMerge)
              )
          )
          Right
          (IntMap.lookup (gmContextOrdinal generatedMerge `mod` contextCount) indexedContexts)
      leftClass <- classAt (gmLeftKey generatedMerge) fixture
      rightClass <- classAt (gmRightKey generatedMerge) fixture
      nextGraph <-
        first show
          (contextMerge contextValue leftClass rightClass (rfGraph fixture))
      pure fixture {rfGraph = nextGraph}

geometryLawReport ::
  (Ord context, Show context) =>
  NonEmpty context ->
  RegionalFixture owner context ->
  Either String GeometryLawReport
geometryLawReport contexts fixture = do
  pointwiseReports <-
    traverse (`pointwiseOracleLaw` fixture) (NonEmpty.toList contexts)
  noOpReport <-
    first show
      (runRoundRebuildReport (emptyGeometrySaturatingGraph (rfGraph fixture)))
  let rebuiltFixture =
        fixture
          { rfGraph = sceContextGraph (rrrGraph noOpReport)
          }
  noOpReports <-
    traverse
      (\contextValue -> noOpStabilityAt contextValue fixture rebuiltFixture)
      (NonEmpty.toList contexts)
  pure
    GeometryLawReport
      { glrRepresentativesAgreeWithOracle =
          all (\(representativesAgree, _, _) -> representativesAgree) pointwiseReports,
        glrRowsAgreeWithOracle =
          all (\(_, rowsAgree, _) -> rowsAgree) pointwiseReports,
        glrAnalysisAgreesWithOracle =
          all (\(_, _, analysisAgrees) -> analysisAgrees) pointwiseReports,
        glrDescentSatisfied =
          drSatisfied (fullDescentCheck (rfGraph fixture))
            && drSatisfied (fullDescentCheck (rfGraph rebuiltFixture)),
        glrNoOpRebuildDeltaEmpty = rrrRebuildDelta noOpReport == mempty,
        glrNoOpRepresentativesStable =
          all (\(representativesStable, _, _) -> representativesStable) noOpReports,
        glrNoOpRowsStable =
          all (\(_, rowsStable, _) -> rowsStable) noOpReports,
        glrNoOpAnalysisStable =
          all (\(_, _, analysisStable) -> analysisStable) noOpReports
      }

pointwiseOracleLaw ::
  (Ord context, Show context) =>
  context ->
  RegionalFixture owner context ->
  Either String (Bool, Bool, Bool)
pointwiseOracleLaw contextValue fixture = do
  eagerGraph <-
    first show (materializedContextGraphAt contextValue (rfGraph fixture))
  representativesAgree <-
    representativesAgreeWithOracleAt contextValue eagerGraph fixture
  rowsAgree <- regionalRowsAgreeAt contextValue eagerGraph fixture
  analysisAgrees <- analysisAgreesWithOracleAt contextValue eagerGraph fixture
  pure (representativesAgree, rowsAgree, analysisAgrees)

representativesAgreeWithOracleAt ::
  (Ord context, Show context) =>
  context ->
  EGraph Arith.ArithF Arith.NodeCount ->
  RegionalFixture owner context ->
  Either String Bool
representativesAgreeWithOracleAt contextValue eagerGraph fixture =
  and
    <$> traverse
      ( \(leftKey, rightKey) -> do
          leftClass <- classAt leftKey fixture
          rightClass <- classAt rightKey fixture
          regionalEquivalent <-
            equivalentAt contextValue leftKey rightKey fixture
          let eagerEquivalent =
                canonicalizeClassId eagerGraph leftClass
                  == canonicalizeClassId eagerGraph rightClass
          pure (regionalEquivalent == eagerEquivalent)
      )
      [ (leftKey, rightKey)
        | leftKey <- IntMap.keys (rfClasses fixture),
          rightKey <- IntMap.keys (rfClasses fixture)
      ]

analysisAgreesWithOracleAt ::
  (Ord context, Show context) =>
  context ->
  EGraph Arith.ArithF Arith.NodeCount ->
  RegionalFixture owner context ->
  Either String Bool
analysisAgreesWithOracleAt contextValue eagerGraph fixture =
  and
    <$> traverse
      ( \(_, classId) -> do
          regionalAnalysis <-
            first show
              (contextAnalysisValueAt contextValue classId (rfGraph fixture))
          let eagerRepresentative = canonicalizeClassId eagerGraph classId
              eagerAnalysis =
                IntMap.lookup (classIdKey eagerRepresentative) (eGraphAnalysis eagerGraph)
          pure (regionalAnalysis == eagerAnalysis)
      )
      (IntMap.toAscList (rfClasses fixture))

noOpStabilityAt ::
  (Ord context, Show context) =>
  context ->
  RegionalFixture owner context ->
  RegionalFixture owner context ->
  Either String (Bool, Bool, Bool)
noOpStabilityAt contextValue beforeFixture afterFixture = do
  representativesStable <-
    and
      <$> traverse
        ( \classKey ->
            (==)
              <$> representativeAt contextValue classKey beforeFixture
              <*> representativeAt contextValue classKey afterFixture
        )
        (IntMap.keys (rfClasses beforeFixture))
  rowsBefore <- regionalRowSignatureAt contextValue beforeFixture
  rowsAfter <- regionalRowSignatureAt contextValue afterFixture
  analysisStable <-
    and
      <$> traverse
        ( \classId ->
            (==)
              <$> first show (contextAnalysisValueAt contextValue classId (rfGraph beforeFixture))
              <*> first show (contextAnalysisValueAt contextValue classId (rfGraph afterFixture))
        )
        (IntMap.elems (rfClasses beforeFixture))
  pure (representativesStable, rowsBefore == rowsAfter, analysisStable)

regionalRowSignatureAt ::
  (Ord context, Show context) =>
  context ->
  RegionalFixture owner context ->
  Either String [Set (Int, [Int])]
regionalRowSignatureAt contextValue fixture = do
  let contextGraph = rfGraph fixture
      buckets = contextAnnotatedDeltaBuckets contextGraph
  contextKey <-
    first show (contextObjectKeyFor (cegSite contextGraph) contextValue)
  pure
    ( fmap
        (\tag -> Set.fromList (regionalCompositeRowsForTag contextGraph buckets contextKey tag))
        regionalFixtureTags
    )

emptyGeometrySaturatingGraph ::
  ContextEGraph owner Arith.ArithF Arith.NodeCount context ->
  SaturatingContextEGraph owner () Arith.ArithF Arith.NodeCount context
emptyGeometrySaturatingGraph =
  emptySaturatingContextEGraph

classAt :: Int -> RegionalFixture owner context -> Either String ClassId
classAt classKey fixture =
  maybe
    (Left ("generated class key is outside the closed fixture: " <> show classKey))
    Right
    (IntMap.lookup classKey (rfClasses fixture))

applyStep ::
  RegionalFixture owner DiamondCtx ->
  RegionalStep ->
  Either String (RegionalFixture owner DiamondCtx)
applyStep fixture step = do
  leftClass <- classAt (rsLeftKey step) fixture
  rightClass <- classAt (rsRightKey step) fixture
  nextGraph <-
    first show
      (contextMerge (rsContext step) leftClass rightClass (rfGraph fixture))
  pure fixture {rfGraph = nextGraph}

applyScript ::
  RegionalFixture owner DiamondCtx ->
  [RegionalStep] ->
  Either String (RegionalFixture owner DiamondCtx)
applyScript = foldlM applyStep

applyScriptBatched ::
  RegionalFixture owner DiamondCtx ->
  [RegionalStep] ->
  Either String (RegionalFixture owner DiamondCtx)
applyScriptBatched fixture steps = do
  stagedBatch <-
    foldlM
      ( \batchValue step -> do
          leftClass <- classAt (rsLeftKey step) fixture
          rightClass <- classAt (rsRightKey step) fixture
          mergePlan <-
            first show (planContextMerges [rsContext step] leftClass rightClass batchValue)
          first show (stageContextMerges mergePlan batchValue)
      )
      (beginContextRebaseBatch (rfGraph fixture))
      steps
  committedGraph <-
    case steps of
      [] -> Right (rfGraph fixture)
      _ -> snd <$> first show (commitContextRebaseBatch stagedBatch)
  pure fixture {rfGraph = committedGraph}

expectRight :: Show obstruction => Either obstruction value -> IO value
expectRight = either (assertFailure . show) pure

representativeAt ::
  (Ord context, Show context) =>
  context ->
  Int ->
  RegionalFixture owner context ->
  Either String ClassId
representativeAt contextValue classKey fixture = do
  classId <- classAt classKey fixture
  first show (contextRepresentativeAt contextValue classId (rfGraph fixture))

equivalentAt ::
  (Ord context, Show context) =>
  context ->
  Int ->
  Int ->
  RegionalFixture owner context ->
  Either String Bool
equivalentAt contextValue leftKey rightKey fixture =
  (==)
    <$> representativeAt contextValue leftKey fixture
    <*> representativeAt contextValue rightKey fixture

expectEquivalent ::
  (Ord context, Show context) =>
  Bool ->
  context ->
  Int ->
  Int ->
  RegionalFixture owner context ->
  Assertion
expectEquivalent expected contextValue leftKey rightKey fixture =
  equivalentAt contextValue leftKey rightKey fixture
    @?= Right expected

withFixtureAfter ::
  [RegionalStep] ->
  (forall owner. RegionalFixture owner DiamondCtx -> Assertion) ->
  Assertion
withFixtureAfter steps useFixture =
  withDiamondFixture $ \baseFixture ->
    either assertFailure useFixture (applyScript baseFixture steps)

testSiblingIsolation :: Assertion
testSiblingIsolation =
  withFixtureAfter [RegionalStep DLeft 0 1] $ \fixture -> do
  expectEquivalent True DLeft 0 1 fixture
  expectEquivalent True DTop 0 1 fixture
  expectEquivalent False DRight 0 1 fixture
  expectEquivalent False DBottom 0 1 fixture

overlapScript :: [RegionalStep]
overlapScript =
  [ RegionalStep DLeft 0 1,
    RegionalStep DRight 1 2
  ]

testDiamondOverlap :: Assertion
testDiamondOverlap =
  withFixtureAfter overlapScript $ \fixture -> do
  expectEquivalent True DLeft 0 1 fixture
  expectEquivalent False DLeft 1 2 fixture
  expectEquivalent True DRight 1 2 fixture
  expectEquivalent False DRight 0 1 fixture
  expectEquivalent True DTop 0 2 fixture
  expectEquivalent False DBottom 0 2 fixture

testResidualCompression :: Assertion
testResidualCompression =
  withFixtureAfter overlapScript $ \forwardFixture ->
    withFixtureAfter (reverse overlapScript) $ \reverseFixture -> do
      traverse_
        ( \contextValue ->
            traverse_
              ( \(leftKey, rightKey) ->
                  equivalentAt contextValue leftKey rightKey forwardFixture
                    @?= equivalentAt contextValue leftKey rightKey reverseFixture
              )
              [(0, 1), (1, 2), (0, 2)]
        )
        [DBottom, DLeft, DRight, DTop]
      expectEquivalent True DLeft 0 1 forwardFixture
      expectEquivalent False DLeft 0 2 forwardFixture
      expectEquivalent True DTop 0 2 forwardFixture

testChildAncestorOrderIndependence :: Assertion
testChildAncestorOrderIndependence =
  withDiamondFixture $ \baseFixture -> do
    let childFirst =
          [ RegionalStep DLeft 0 1,
            RegionalStep DTop 1 2
          ]
    stepwise <- expectRight (applyScript baseFixture childFirst)
    reversed <- expectRight (applyScript baseFixture (reverse childFirst))
    batched <- expectRight (applyScriptBatched baseFixture childFirst)
    partitionSignature stepwise @?= partitionSignature reversed
    partitionSignature stepwise @?= partitionSignature batched

type SoleContext :: Type
data SoleContext = SoleContext
  deriving stock (Eq, Ord, Show)

testSingletonBehavior :: Assertion
testSingletonBehavior = do
  (classes, baseGraph) <- expectRight arithBase
  withEmptyContextEGraph (singletonContextLattice SoleContext) baseGraph $ \contextGraph -> do
    let fixture =
          RegionalFixture
            { rfClasses = classes,
              rfGraph = contextGraph
            }
    leftClass <- expectRight (classAt 0 fixture)
    rightClass <- expectRight (classAt 1 fixture)
    mergedGraph <- expectRight (contextMerge SoleContext leftClass rightClass (rfGraph fixture))
    let mergedFixture = fixture {rfGraph = mergedGraph}
    expectEquivalent True SoleContext 0 1 mergedFixture
    eagerGraph <- expectRight (materializedContextGraphAt SoleContext mergedGraph)
    canonicalizeClassId eagerGraph leftClass @?= canonicalizeClassId eagerGraph rightClass

testDenseSymbolicAgreement :: Assertion
testDenseSymbolicAgreement =
  either (assertFailure . show) id $
    powersetTwinWorkload powersetTwinAtoms $ \workload -> do
      let denseGraph = ptwDenseGraph workload
          symbolicGraph = ptwSymbolicGraph workload
          classA = ptwClassA workload
          classB = ptwClassB workload
      traverse_
        ( \contextValue ->
            ( (==)
                <$> contextRepresentativeAt contextValue classA denseGraph
                <*> contextRepresentativeAt contextValue classB denseGraph
            )
              @?= ( (==)
                      <$> contextRepresentativeAt contextValue classA symbolicGraph
                      <*> contextRepresentativeAt contextValue classB symbolicGraph
                  )
        )
        (powersetProbeContexts powersetTwinAtoms)

type TwentyAtomContext = Set Int

testTwentyAtomCompactness :: Assertion
testTwentyAtomCompactness = do
  (classes, baseGraph) <- expectRight arithBase
  either (assertFailure . show) id $
    withPreparedContextSiteFromPowersetAtoms ([0 .. 19] :: [Int]) $ \symbolicSite -> do
      preparedSiteEnumerability symbolicSite @?= SiteImplicitPowerset 20
      let graph0 = emptyContextEGraphFromSite symbolicSite baseGraph
      classA <- expectRight (lookupClass 0 classes)
      classB <- expectRight (lookupClass 1 classes)
      classC <- expectRight (lookupClass 2 classes)
      graph1 <- expectRight (contextMerge (Set.singleton 0) classA classB graph0)
      graph2 <- expectRight (contextMerge (Set.singleton 1) classB classC graph1)
      let fixture = RegionalFixture classes graph2
          overlapContext = Set.fromList [0, 1]
          extendedOverlap = Set.fromList [0, 1, 19]
      expectEquivalent True (Set.singleton 0) 0 1 fixture
      expectEquivalent False (Set.singleton 0) 0 2 fixture
      expectEquivalent True (Set.singleton 1) 1 2 fixture
      expectEquivalent True overlapContext 0 2 fixture
      expectEquivalent True extendedOverlap 0 2 fixture
      expectEquivalent False Set.empty 0 1 fixture
      expectEquivalent False (Set.singleton 19) 0 1 fixture
      let metrics =
            annotatedDeltaMetrics
              (preparedRegionTable symbolicSite)
              (contextAnnotatedDeltaBuckets graph2)
      assertBool
        ("regional parent cubes grew as if the 20-atom powerset had been enumerated: " <> show metrics)
        (annotatedDeltaParentRegionCubeCount metrics < 64)

lookupClass :: Int -> IntMap ClassId -> Either String ClassId
lookupClass classKey =
  maybe
    (Left ("missing class " <> show classKey))
    Right
    . IntMap.lookup classKey

partitionSignature :: RegionalFixture owner DiamondCtx -> Either String [[Int]]
partitionSignature fixture =
  traverse signatureAt [DBottom, DLeft, DRight, DTop]
  where
    signatureAt contextValue =
      fmap classIdKey
        <$> traverse
          (\classKey -> representativeAt contextValue classKey fixture)
          [0 .. 3]

partitionLaw :: RegionalFixture owner DiamondCtx -> Either String Bool
partitionLaw fixture = do
  eagerGraphs <-
    traverse
      (\contextValue -> first show (materializedContextGraphAt contextValue (rfGraph fixture)))
      [DBottom, DLeft, DRight, DTop]
  and
    <$> traverse
      ( \(contextValue, eagerGraph) -> do
          representativesAgree <-
            and
              <$> traverse
              ( \(leftKey, rightKey) -> do
                  leftClass <- classAt leftKey fixture
                  rightClass <- classAt rightKey fixture
                  regionalEquivalent <- equivalentAt contextValue leftKey rightKey fixture
                  let eagerEquivalent =
                        canonicalizeClassId eagerGraph leftClass
                          == canonicalizeClassId eagerGraph rightClass
                  pure (regionalEquivalent == eagerEquivalent)
              )
              [(leftKey, rightKey) | leftKey <- [0 .. 3], rightKey <- [0 .. 3]]
          rowsAgree <- regionalRowsAgreeAt contextValue eagerGraph fixture
          pure (representativesAgree && rowsAgree)
      )
      (zip [DBottom, DLeft, DRight, DTop] eagerGraphs)

regionalRowsAgreeAt ::
  (Ord context, Show context) =>
  context ->
  EGraph Arith.ArithF Arith.NodeCount ->
  RegionalFixture owner context ->
  Either String Bool
regionalRowsAgreeAt contextValue eagerGraph fixture = do
  contextKey <-
    first show
      (contextObjectKeyFor (cegSite contextGraph) contextValue)
  pure
    ( all
        (\tag -> regionalRowsForTag contextKey tag == eagerRowsForTag tag)
        regionalFixtureTags
    )
  where
    contextGraph = rfGraph fixture
    buckets = contextAnnotatedDeltaBuckets contextGraph
    canonicalRows = canonicalRowSet eagerGraph
    regionalRowsForTag contextKey tag =
      canonicalRows (regionalCompositeRowsForTag contextGraph buckets contextKey tag)
    eagerRowsForTag tag =
      canonicalRows
        (structuralRowsForOperator (eGraphStore eagerGraph) (Operator tag))

regionalCompositeRowsForTag ::
  ContextEGraph owner Arith.ArithF analysis context ->
  AnnotatedDeltaBuckets owner Arith.ArithF ->
  ContextObjectKey owner ->
  Arith.ArithF () ->
  [(Int, [Int])]
regionalCompositeRowsForTag contextGraph buckets contextKey tag =
  Set.toAscList
    ( Set.union
        ( Set.difference
            (Set.fromList baseRows)
            (Set.fromList (absorbedRowsAtKey tag contextKey buckets))
        )
        (Set.fromList (annotatedRowsAtKey tag contextKey buckets))
    )
  where
    baseRows =
      structuralRowsForOperator
        (eGraphStore (cegBase contextGraph))
        (Operator tag)

regionalFixtureTags :: [Arith.ArithF ()]
regionalFixtureTags =
  [ Arith.Num 1,
    Arith.Num 2,
    Arith.Num 3,
    Arith.Num 4,
    Arith.Add () ()
  ]

canonicalRowSet ::
  EGraph Arith.ArithF Arith.NodeCount ->
  [(Int, [Int])] ->
  Set (Int, [Int])
canonicalRowSet graph =
  Set.fromList
    . fmap
      ( \(rootKey, childKeys) ->
          (canonicalKey rootKey, fmap canonicalKey childKeys)
      )
  where
    canonicalKey = classIdKey . canonicalizeClassId graph . ClassId

propRegionalScripts :: RegionalScript -> QC.Property
propRegionalScripts (RegionalScript steps) =
  either
    (\fixtureError -> QC.counterexample fixtureError False)
    id
    ( diamondFixture $ \baseFixture ->
      case
          ( applyScript baseFixture steps,
            applyScript baseFixture (reverse steps),
            applyScriptBatched baseFixture steps
          )
        of
          (Right forwardFixture, Right reverseFixture, Right batchedFixture) ->
            QC.counterexample
              ( "script="
                  <> show steps
                  <> "\nforward="
                  <> show (partitionSignature forwardFixture)
                  <> "\nreverse="
                  <> show (partitionSignature reverseFixture)
                  <> "\nbatched="
                  <> show (partitionSignature batchedFixture)
              )
              ( QC.conjoin
                  [ partitionLaw forwardFixture QC.=== Right True,
                    partitionSignature forwardFixture QC.=== partitionSignature reverseFixture,
                    partitionSignature forwardFixture QC.=== partitionSignature batchedFixture
                  ]
              )
          outcomes ->
            QC.counterexample ("typed regional construction failed: " <> showScriptOutcomes outcomes) False
    )

propGeneratedScaleGeometries :: GeneratedGeometryScript -> QC.Property
propGeneratedScaleGeometries script =
  case buildGeneratedScaleGeometries script of
    Left constructionFailure ->
      QC.counterexample
        ("generated scale geometry construction failed: " <> constructionFailure <> "\nscript=" <> show script)
        False
    Right geometries ->
      QC.conjoin
        ( fmap
            ( \(shape, extent, lawReport) ->
                QC.counterexample
                  ( "generated scale geometry="
                      <> show (shape, extent)
                      <> "\nscript="
                      <> show script
                  )
                  (lawReport QC.=== completeGeometryLawReport)
            )
            geometries
        )

propGeneratedSymbolicPowersets :: GeneratedGeometryScript -> QC.Property
propGeneratedSymbolicPowersets script =
  case buildGeneratedPowersetGeometries script of
    Left constructionFailure ->
      QC.counterexample
        ("generated symbolic powerset construction failed: " <> constructionFailure <> "\nscript=" <> show script)
        False
    Right geometries ->
      QC.conjoin
        ( fmap
            ( \(atomCount, lawReport) ->
                QC.counterexample
                  ( "generated symbolic powerset atom-count="
                      <> show atomCount
                      <> "\nscript="
                      <> show script
                  )
                  (lawReport QC.=== completeGeometryLawReport)
            )
            geometries
        )

showScriptOutcomes ::
  ( Either String (RegionalFixture owner DiamondCtx),
    Either String (RegionalFixture owner DiamondCtx),
    Either String (RegionalFixture owner DiamondCtx)
  ) ->
  String
showScriptOutcomes (forwardOutcome, reverseOutcome, batchedOutcome) =
  showOutcome forwardOutcome
    <> " / "
    <> showOutcome reverseOutcome
    <> " / "
    <> showOutcome batchedOutcome
  where
    showOutcome :: Either String value -> String
    showOutcome = either id (const "success")
{-# LANGUAGE RankNTypes #-}
