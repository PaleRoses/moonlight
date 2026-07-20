module Moonlight.EGraph.Extraction.ExtractionSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isNothing)
import Moonlight.Core (find)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    appendEGraphMutationTrace,
    emptyEGraphMutationTrace,
  )
import Moonlight.EGraph.Pure.Context (emptyContextEGraphFromSite)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
    emptyProofGraphWithRetention,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionCacheObstruction (..),
    ExtractionWorkBudget (..),
    ExtractionResult (erClass, erCost, erTerm),
    StableExtractionSnapshot,
    advanceExtractionChoiceCache,
    depthCost,
    extract,
    extractAll,
    extractAllCached,
    extractCached,
    extractionChoiceCacheFromStableGraph,
    liftCostAlgebra,
    stableExtractionSnapshotFromEGraph,
    termCost,
    termSize,
  )
import Moonlight.EGraph.Pure.Extraction.Rewrite
  ( extractGuided,
    extractGuidedWithProof,
    extractWithProof,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm, insertTermTracked)
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked, rebuildTracked)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, eGraphUnionFind)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Test.Arith.Core (ArithF, NodeCount, numTerm)
import Moonlight.EGraph.Test.Arith.Cost (analysisAwareCost, arithCost)
import Moonlight.EGraph.Test.Arith.Fixture
  ( assertArithTerm,
    commutedAddGuidance,
    nestedAdd,
    one,
    onePlusZero,
    retainAddZeroGuidance,
    seedArith,
    seedArithTerms,
    zero,
    zeroPlusOne,
  )
import Moonlight.EGraph.Test.Arith.Rules (addCommuteRule, addZeroRightRule)
import Moonlight.EGraph.Test.Assertions
  ( expectEqualitySaturation,
    requireExtraction,
    requireMaybe,
  )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Config (testConfig, toBudget)
import Moonlight.EGraph.Test.Saturation
  ( EGraphSaturationReport,
    ExtractionPlan (ExtractByAnalysisCost, ExtractByCost),
    ProofSaturationSpec (..),
    SaturationBudget,
    genericJoinSaturationSpec,
    psrProofGraph,
    runEqualitySaturation,
    runProofSaturationSpec,
    runSaturationSpec,
    saturationReportBaseGraph,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofAnnotationBuilder (..),
    ProofQueryError (ProofUnavailableForRetention),
    ProofRetention (KeepNoProof),
  )
import Moonlight.Saturation.Substrate (TrivialContext, trivialLattice)
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner, unitPreparedContextSite)
import Moonlight.Pale.Test.Site.Core (canonicalTestBudget)
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

tests :: TestTree
tests =
  testGroup "extraction" . hunitCases $
    [ HUnitCase "extract selects the cheapest representative in class" $
        assertOnePlusZeroExtraction (ExtractByCost arithCost) $ \targetClass saturationReport extractionResult -> do
          erClass extractionResult @?= rootClassIn saturationReport targetClass
          erCost extractionResult @?= 1,
      HUnitCase "depthCost measures depth rather than term size" $ do
        termCost depthCost nestedAdd @?= 3
        termSize nestedAdd @?= 5,
      HUnitCase "analysis-aware extraction can inspect child analysis values" $
        assertOnePlusZeroExtraction (ExtractByAnalysisCost analysisAwareCost) (\_ _ _ -> pure ()),
      HUnitCase "extractAll includes successful class extractions" $ do
        graph <- expectRight (seedArithTerms [one, zero, onePlusZero])
        extractionSnapshot <- stableSnapshot graph
        IntMap.size (extractAll arithCost extractionSnapshot) @?= 3,
      HUnitCase "extraction choice cache absorbs the empty trace and serves cached extraction" $ do
        (oneClassId, graph) <- expectRight (seedArith one)
        extractionCache <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra graph)
        advancedCache <- expectCache (advanceExtractionChoiceCache cacheBudget (emptyEGraphMutationTrace graph) graph extractionCache)
        assertBool
          "empty trace advance must preserve every cached extraction"
          (extractAllCached advancedCache == extractAllCached extractionCache)
        cachedExtraction <- requireMaybe "expected cached extraction" (extractCached oneClassId extractionCache)
        assertArithTerm one (erTerm cachedExtraction),
      HUnitCase "extraction choice cache advances through insertion and committed merge traces" $ do
        (oneClassId, graph1) <- expectRight (seedArith one)
        EGraphMutationResult {emrGraph = insertedGraph, emrTrace = insertTrace} <-
          expectRight (insertTermTracked (numTerm 2) graph1)
        (zeroClassId, graph2) <- expectRight (addTerm zero graph1)
        let EGraphMutationResult {emrGraph = dirtyGraph, emrTrace = mergeTrace} = equateClassesTracked oneClassId zeroClassId graph2
            EGraphMutationResult {emrGraph = rebuiltGraph, emrTrace = rebuildTrace} = rebuildTracked dirtyGraph
            committedMergeTrace = appendEGraphMutationTrace mergeTrace rebuildTrace
        baseCache <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra graph1)
        advancedInsertCache <- expectCache (advanceExtractionChoiceCache cacheBudget insertTrace insertedGraph baseCache)
        freshInsertCache <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra insertedGraph)
        assertBool
          "insertion advance must equal the fresh cache on the after-graph"
          (extractAllCached advancedInsertCache == extractAllCached freshInsertCache)
        mergeCache <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra graph2)
        advancedMergeCache <- expectCache (advanceExtractionChoiceCache cacheBudget committedMergeTrace rebuiltGraph mergeCache)
        freshMergeCache <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra rebuiltGraph)
        assertBool
          "committed merge advance must equal the fresh cache on the after-graph"
          (extractAllCached advancedMergeCache == extractAllCached freshMergeCache)
        case advanceExtractionChoiceCache cacheBudget committedMergeTrace rebuiltGraph baseCache of
          Left (ExtractionCacheLineageMismatch _ _) -> pure ()
          Left obstruction -> assertFailure ("expected lineage mismatch, got " <> show obstruction)
          Right _ -> assertFailure "a trace from a foreign lineage must obstruct cache advance",
      HUnitCase "dirty graph does not materialize an extraction snapshot" $ do
        (oneClassId, graph1) <- expectRight (seedArith one)
        (zeroClassId, graph2) <- expectRight (addTerm zero graph1)
        let EGraphMutationResult {emrGraph = dirtyGraph} = equateClassesTracked oneClassId zeroClassId graph2
        isNothing (stableExtractionSnapshotFromEGraph dirtyGraph) @?= True,
      HUnitCase "guided extraction can prefer checkpoint-shaped representatives over default deterministic choices" $ do
        (sumClassId, graph) <- expectRight (seedArith onePlusZero)
        saturationReport <- expectEqualitySaturation (runSaturationSpec @SurfaceKind (genericJoinSaturationSpec defaultBudget) [addCommuteRule] graph)
        extractionSnapshot <- stableSnapshot (saturationReportBaseGraph saturationReport)
        plainExtraction <- requireMaybe "expected plain extraction result" (extract arithCost sumClassId extractionSnapshot)
        guidedExtraction <- requireMaybe "expected guided extraction result" (extractGuided commutedAddGuidance arithCost sumClassId extractionSnapshot)
        assertArithTerm onePlusZero (erTerm plainExtraction)
        assertArithTerm zeroPlusOne (erTerm guidedExtraction),
      HUnitCase "recursive guided extraction and proof witnesses agree across canonical cycles" $ do
        (sumClassId, graph) <- expectRight (seedArith onePlusZero)
        proofReport <- expectEqualitySaturation (runProofSaturationSpec unitProofSaturationSpec [addZeroRightRule] (unitProofGraph graph))
        let proofEvidenceGraph = psrProofGraph proofReport
            plainGuidedExtraction =
              stableExtractionSnapshotFromEGraph (cegBase (sceContextGraph (pgGraph proofEvidenceGraph)))
                >>= extractGuided retainAddZeroGuidance arithCost sumClassId
        requireExtraction plainGuidedExtraction >>= assertArithTerm onePlusZero . erTerm
        proofGuidedExtraction <-
          expectRight
            (extractGuidedWithProof (cegBase . sceContextGraph) retainAddZeroGuidance arithCost sumClassId proofEvidenceGraph)
            >>= requireMaybe "expected proof-backed guided extraction result"
        assertArithTerm onePlusZero (erTerm proofGuidedExtraction),
      HUnitCase "proof-aware extraction preserves proof retention failure" $ do
        (oneClassId, graph) <- expectRight (seedArith one)
        case extractWithProof id arithCost oneClassId (emptyProofGraphWithRetention KeepNoProof graph) of
          Left (ProofUnavailableForRetention KeepNoProof) ->
            pure ()
          Left proofError ->
            assertFailure ("expected KeepNoProof obstruction, got " <> show proofError)
          Right _ ->
            assertFailure "proof retention failure was erased"
    ]

assertOnePlusZeroExtraction ::
  ExtractionPlan ArithF NodeCount Int ->
  (ClassId -> EGraphSaturationReport SurfaceKind ArithF NodeCount -> ExtractionResult ArithF Int -> Assertion) ->
  Assertion
assertOnePlusZeroExtraction extractionPlan extraAssertions = do
  (targetClass, graph) <- expectRight (seedArith onePlusZero)
  (maybeExtraction, saturationReport) <-
    expectEqualitySaturation $
      runEqualitySaturation
        (genericJoinSaturationSpec defaultBudget)
        extractionPlan
        onePlusZero
        [addZeroRightRule]
        graph
  extractionResult <- requireExtraction maybeExtraction
  assertArithTerm one (erTerm extractionResult)
  extraAssertions targetClass saturationReport extractionResult

stableSnapshot :: EGraph ArithF NodeCount -> IO (StableExtractionSnapshot ArithF NodeCount)
stableSnapshot graph =
  requireMaybe "expected stable extraction snapshot" (stableExtractionSnapshotFromEGraph graph)

cacheBudget :: ExtractionWorkBudget
cacheBudget =
  ExtractionWorkBudget 1024

arithChoiceAlgebra :: AnalysisCostAlgebra ArithF NodeCount Int
arithChoiceAlgebra =
  liftCostAlgebra arithCost

expectCache :: Either ExtractionCacheObstruction result -> IO result
expectCache =
  either (assertFailure . ("expected extraction choice cache, got obstruction: " <>) . show) pure

rootClassIn :: EGraphSaturationReport SurfaceKind ArithF NodeCount -> ClassId -> ClassId
rootClassIn saturationReport classId =
  fst (find classId (eGraphUnionFind (saturationReportBaseGraph saturationReport)))

unitProofGraph :: EGraph ArithF NodeCount -> SaturatingProofEGraph UnitContextSiteOwner SurfaceKind ArithF NodeCount TrivialContext ()
unitProofGraph =
  emptySaturatingProofEGraph . emptyContextEGraphFromSite unitPreparedContextSite

unitProofSaturationSpec :: ProofSaturationSpec UnitContextSiteOwner SurfaceKind ArithF NodeCount TrivialContext ()
unitProofSaturationSpec =
  ProofSaturationSpec
    { pssSaturation = testConfig canonicalTestBudget,
      pssGuidance = Nothing,
      pssProofBuilder = ProofAnnotationBuilder (const ()),
      pssActiveContext = Nothing
    }

defaultBudget :: SaturationBudget
defaultBudget =
  toBudget canonicalTestBudget
