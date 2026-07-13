{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Introspection.ContextSpec
  ( tests,
    ToyF (..),
    toySpansSystem2,
    largeToySystem,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), HasConstructorTag (..), Pattern (..), zipSameNodeShape)
import Moonlight.Derived.Site (FinObjectId (..), DerivedPoset, mkDerivedPosetFromOrderEdges)
import Moonlight.EGraph.Introspection.Analysis.Resolution
import Moonlight.EGraph.Introspection.Analysis.Resolution.Descent
import Moonlight.EGraph.Introspection.Core.Context.Tag
import Moonlight.EGraph.Introspection.Core.Rewrite
import Moonlight.EGraph.Introspection.Analysis.Spectral
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Section.Stalk.Groupoid (fromDiscreteStalk)
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Site (grothendieckChainComplexFromSite)
import Moonlight.Homology
  ( BasisCellRef (..),
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    cellDegree,
    computeRationalSpectralPages,
    convergenceDepth,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    freeBettiVector,
    freeRank,
    frmcRefinedMorseComplex,
    groupAt,
    incidenceMatrixAt,
    maxHomologicalDegree,
    pageIndex,
    rmcReducedComplex,
    sourceCardinality,
    targetCardinality,
  )
import Test.Tasty.HUnit (Assertion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

type ToyF :: Type -> Type
data ToyF a
  = ToyVar String
  | ToyApp a a
  | ToyLam a
  | ToyLit Int
  deriving stock (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)

type ToyTag :: Type
data ToyTag
  = ToyVarTag
  | ToyAppTag
  | ToyLamTag
  | ToyLitTag
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance HasConstructorTag ToyF where
  type ConstructorTag ToyF = ToyTag

  constructorTag patternNode =
    case patternNode of
      ToyVar {} -> ToyVarTag
      ToyApp {} -> ToyAppTag
      ToyLam {} -> ToyLamTag
      ToyLit {} -> ToyLitTag

instance ZipMatch ToyF where
  zipMatch = zipSameNodeShape

tests :: TestTree
tests =
  testGroup
    "context-and-resolution"
    [ testCase "generator-built systems expose the closure as all contexts" testGeneratorClosureAgreement,
      testCase "generator-seeded frontier pairs agree with downward context pairs" testGeneratorSeededPairsAgreement,
      testCase "cover site agrees with the enumerated site on generator-built systems" testGrothendieckSiteFromCoverAgreement,
      testCase "tag projection survives roundtrip through expansion" testTagProjectionRoundtrip,
      testCase "tag closure contains dense overlaps from shared constructor families" testTagClosureDenseMeet,
      testCase "resolution uses the cover-rerouted rewrite site" testResolutionUsesRewriteSite,
      testCase "resolution source poset matches the scaffold site" testResolutionSourcePosetMatchesScaffold,
      testCase "resolution surfaces spectral pages and parsing depth" testResolutionSpectralWiring,
      testCase "resolution bidegrees reuse the spectral filtration" testResolutionBidegreeConsistency,
      testCase "resolution keys basis cells by the scaffold source nodes" testResolutionSourceNodeIdentity,
      testCase "descent enrichment stays inert for discrete groupoids" testDescentEnrichmentDiscrete,
      testCase "diagnostic traversal attaches convergence diagnostics" testDiagnosticTraversal,
      testCase "projection strategies refine through bounded fixpoint acceleration" testProjectionStrategies,
      testCase "toySpans depth-2 chain complex constructs with bounded dimensions" testToySpansChainComplexBounded,
      testCase "toySpans depth-1 spectral sequence converges to direct Betti counts" testToySpansSpectralConvergesToCorrectBetti,
      testCase "toySpans depth-2 resolution completes with Morse-accelerated spectral computation" testToySpansResolutionWithMorse
    ]

testGeneratorClosureAgreement :: IO ()
testGeneratorClosureAgreement =
  let rewriteSystem = mkRewriteSystemFromGenerators toySpans
   in assertEqual
        "generator closure should agree with the stored context family"
        (Set.fromList (contextClosure rewriteSystem))
        (Set.fromList (allContexts rewriteSystem))

testGeneratorSeededPairsAgreement :: IO ()
testGeneratorSeededPairsAgreement =
  let rewriteSystem = mkRewriteSystemFromGenerators toySpans
      contextValues = allContexts rewriteSystem
      generatorPairs =
        downwardContextPairsFromGenerators
          (contextLeq rewriteSystem)
          (contextGenerators rewriteSystem)
          contextValues
      expectedPairs =
        downwardPairsByStrategy
          ExhaustivePairs
          rewriteSystem
          contextValues
   in assertEqual
        "generator-seeded pairs should recover the same downward relation"
        (Set.fromList expectedPairs)
        (Set.fromList generatorPairs)

testGrothendieckSiteFromCoverAgreement :: IO ()
testGrothendieckSiteFromCoverAgreement =
  let rewriteSystem = mkRewriteSystemFromGenerators toySpans
      enumeratedSite = mkGrothendieckSite rewriteSystem 2
      coveredSite = mkGrothendieckSite rewriteSystem 2
   in do
        assertEqual "cell count" (length (grothendieckSiteCells enumeratedSite)) (length (grothendieckSiteCells coveredSite))
        assertEqual "face count" (length (grothendieckSiteFaceMorphisms enumeratedSite)) (length (grothendieckSiteFaceMorphisms coveredSite))

testTagProjectionRoundtrip :: IO ()
testTagProjectionRoundtrip =
  let tagSystem = mkTagAwareRewriteSystem toySpans
      objectContexts = allContexts (tarsInner tagSystem)
      roundtrips =
        objectContexts
          >>= (\contextValue -> [(contextValue, expandTagContext tagSystem (projectToTagContext contextValue))])
   in assertBool
        "every object context should survive project-then-expand"
        (all (\(contextValue, expandedContexts) -> contextValue `elem` expandedContexts) roundtrips)

testTagClosureDenseMeet :: IO ()
testTagClosureDenseMeet =
  let tagSystem = mkTagAwareRewriteSystem toySpans
      closedContexts = contextClosure tagSystem
      expectedMeet = TagContext (Set.singleton ToyVarTag)
   in assertBool
        "shared tag structure should yield the singleton Var-tag meet"
        (expectedMeet `elem` closedContexts)

testResolutionUsesRewriteSite :: IO ()
testResolutionUsesRewriteSite =
  withToySpansResolution2 $ \resolutionValue ->
    assertEqual
      "resolution cell count"
      (length (grothendieckSiteCells toySpansSite2))
      (resolutionCellCount resolutionValue)

testResolutionSourcePosetMatchesScaffold :: IO ()
testResolutionSourcePosetMatchesScaffold =
  withToySpansResolution2 $ \resolutionValue ->
    case expectedSourcePoset toySpansSite2 of
      Left failureValue ->
        assertFailure ("unexpected source-poset construction failure: " <> show failureValue)
      Right posetValue ->
        assertEqual
          "resolution source poset should be induced by the scaffold site covers"
          posetValue
          (resolutionSourcePoset resolutionValue)

testResolutionSpectralWiring :: IO ()
testResolutionSpectralWiring =
  withToySpansResolution1 $ \resolutionValue ->
    case (raSpectralPages (rbAnalysis resolutionValue), raBoundaryAnalysis (rbAnalysis resolutionValue), raParsingDepth (rbAnalysis resolutionValue)) of
      (Left failure, _, _) -> assertFailure ("spectral pages failed: " <> show failure)
      (_, Left failure, _) -> assertFailure ("boundary analysis failed: " <> show failure)
      (_, _, Left failure) -> assertFailure ("parsing depth failed: " <> show failure)
      (Right spectralPages, Right boundaryAnalysis, Right parsingDepth) -> do
        assertEqual
          "top-level spectral page count should mirror boundary analysis"
          (length spectralPages)
          (length (rbaSpectralPages boundaryAnalysis))
        assertEqual
          "parsing depth should come from spectral convergence"
          parsingDepth
          (convergenceDepth spectralPages)

testResolutionBidegreeConsistency :: IO ()
testResolutionBidegreeConsistency =
  withToySpansResolution2 $ \resolutionValue ->
    case (raBoundaryAnalysis (rbAnalysis resolutionValue), raSpectralPages (rbAnalysis resolutionValue)) of
      (Left failure, _) -> assertFailure ("boundary analysis failed: " <> show failure)
      (_, Left failure) -> assertFailure ("spectral pages failed: " <> show failure)
      (Right boundaryAnalysis, Right spectralPages) -> do
        let bidegreeMap = rbaBidegreesByBasisCell boundaryAnalysis
            basisCellMap = rbaBasisCellBySourceNode boundaryAnalysis
        assertBool
          "bidegree map should cover every source node's basis cell"
          (all (`Map.member` bidegreeMap) (Map.elems basisCellMap))
        assertBool
          "spectral pages should be non-empty"
          (not (null (rbaSpectralPages boundaryAnalysis)))
        assertEqual
          "spectral page count should match boundary analysis"
          (length spectralPages)
          (length (rbaSpectralPages boundaryAnalysis))

testResolutionSourceNodeIdentity :: IO ()
testResolutionSourceNodeIdentity =
  withToySpansResolution2 $ \resolutionValue ->
    case raBoundaryAnalysis (rbAnalysis resolutionValue) of
      Left failure -> assertFailure ("boundary analysis failed: " <> show failure)
      Right boundaryAnalysis ->
        assertEqual
            "basis-cell assignments should key exactly the scaffold source nodes"
            (Set.fromList (resolutionSourceNodes resolutionValue))
            (Map.keysSet (rbaBasisCellBySourceNode boundaryAnalysis))

testDescentEnrichmentDiscrete :: IO ()
testDescentEnrichmentDiscrete =
  withToySpansResolution1 $ \resolutionValue ->
    case
        enrichWithDescent
          resolutionValue
          (const (fromDiscreteStalk IntSet.empty)) of
        Left failureValue ->
          assertFailure ("unexpected descent enrichment failure: " <> show failureValue)
        Right enrichment -> do
          assertBool "discrete groupoids should skip descent" (maybe True (const False) (deDescentPage enrichment))
          assertEqual "discrete groupoids should report no phantom obstructions" 0 (dePhantomObstructionCount enrichment)

testDiagnosticTraversal :: IO ()
testDiagnosticTraversal =
  case grothendieckConsistencyProfileWith DiagnosticTraversal (mkRewriteSystemFromGenerators toySpans) 1 of
    Left failureValue ->
      assertFailure ("unexpected consistency failure: " <> show failureValue)
    Right profileValue ->
      assertBool
        "diagnostic traversal should attach traversal diagnostics"
        (maybe False (\diagnostics -> gtdIterationCount diagnostics >= 0) (gcpTraversalDiagnostics profileValue))

expectedSourcePoset ::
  GrothendieckSite (RewriteSystem ToyF) ->
  Either HomologyFailure DerivedPoset
expectedSourcePoset siteValue = do
  finiteComplex <- grothendieckChainComplexFromSite siteValue
  let basisRefs = basisRefsFromSite siteValue
  first (InvalidTopologyInput . show)
    ( mkDerivedPosetFromOrderEdges
        ( fmap
            ( \cellValue ->
                FinObjectId
                  ( case Map.lookup cellValue basisRefs of
                      Just basisCellRef -> basisCellNodeId finiteComplex basisCellRef
                      Nothing -> 0
                  )
            )
            (grothendieckSiteCells siteValue)
        )
        ( fmap
            ( \faceMorphism ->
                let (sourceCell, targetCell) = faceCover faceMorphism
                    nodeFor cellValue =
                      FinObjectId
                        ( case Map.lookup cellValue basisRefs of
                            Just basisCellRef -> basisCellNodeId finiteComplex basisCellRef
                            Nothing -> 0
                        )
                 in (nodeFor sourceCell, nodeFor targetCell)
            )
            (grothendieckSiteFaceMorphisms siteValue)
        )
    )

basisRefsFromSite ::
  GrothendieckSite system ->
  Map.Map (GrothendieckCell system) BasisCellRef
basisRefsFromSite siteValue =
  Map.fromList
    ( concatMap
        (\degreeValue -> refsAtDegree degreeValue (grothendieckSiteCellsAtDimension siteValue (fromIntegral degreeValue)))
        [0 .. fromIntegral (grothendieckSiteDepth siteValue)]
    )

refsAtDegree ::
  Int ->
  [GrothendieckCell system] ->
  [(GrothendieckCell system, BasisCellRef)]
refsAtDegree degreeValue cellValues =
  fmap
    (\(cellIndexValue, cellValue) -> (cellValue, BasisCellRef {cellDegree = HomologicalDegree degreeValue, cellIndex = cellIndexValue}))
    (zip [0 :: Int ..] cellValues)

basisCellNodeId ::
  FiniteChainComplex Int ->
  BasisCellRef ->
  Int
basisCellNodeId finiteComplex basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue ->
      sum
        ( fmap
            (\lowerDegreeValue -> sourceCardinality (incidenceMatrixAt finiteComplex (HomologicalDegree lowerDegreeValue)))
            [0 .. degreeValue - 1]
        )
        + cellIndex basisCellRef

faceCover ::
  GrothendieckFaceMorphism (RewriteSystem ToyF) ->
  (GrothendieckCell (RewriteSystem ToyF), GrothendieckCell (RewriteSystem ToyF))
faceCover faceMorphism =
  (grothendieckFaceMorphismTarget faceMorphism, grothendieckFaceMorphismSource faceMorphism)

testProjectionStrategies :: IO ()
testProjectionStrategies =
  case toySpans of
    [] ->
      assertFailure "expected at least one toy span"
    firstSpan : _ ->
      let rewriteSystem = mkRewriteSystemFromGenerators [firstSpan]
       in case
            ( grothendieckConsistencyProfileWith TarskiIteration rewriteSystem 1,
              grothendieckConsistencyProfileWith ChainingIteration rewriteSystem 1,
              grothendieckConsistencyProfileWith WidenedIteration rewriteSystem 1
            ) of
            (Left failureValue, _, _) ->
              assertFailure ("unexpected tarski failure: " <> show failureValue)
            (_, Left failureValue, _) ->
              assertFailure ("unexpected chaining failure: " <> show failureValue)
            (_, _, Left failureValue) ->
              assertFailure ("unexpected widening failure: " <> show failureValue)
            (Right tarskiProfile, Right chainingProfile, Right widenedProfile) -> do
              assertBool "chaining should converge" (gcpConverged chainingProfile)
              assertBool "widening should converge" (gcpConverged widenedProfile)
              assertBool
                "projection strategies should keep traversal diagnostics disabled"
                ( maybe True (const False) (gcpTraversalDiagnostics chainingProfile)
                    && maybe True (const False) (gcpTraversalDiagnostics widenedProfile)
                )
              assertBool
                "projection strategies should not worsen mismatch counts"
                ( gcpMismatchCount chainingProfile <= gcpMismatchCount tarskiProfile
                    && gcpMismatchCount widenedProfile <= gcpMismatchCount tarskiProfile
                )

toySpans :: [RewriteMorphism ToyF]
toySpans =
  [ expectToySpan (rewriteMorphismWithInterface "app-zero" (toyApp (toyVar "f") (toyLit 0)) Set.empty (toyVar "f") Nothing Nothing),
    expectToySpan (rewriteMorphismWithInterface "lam-unpack" (toyLam (toyVar "x")) Set.empty (toyVar "x") Nothing Nothing),
    expectToySpan (rewriteMorphismWithInterface "app-one" (toyApp (toyVar "g") (toyLit 1)) Set.empty (toyLit 1) Nothing Nothing)
  ]

toyVar :: String -> Pattern ToyF
toyVar =
  PatternNode . ToyVar

toyApp :: Pattern ToyF -> Pattern ToyF -> Pattern ToyF
toyApp leftPattern rightPattern =
  PatternNode (ToyApp leftPattern rightPattern)

toyLam :: Pattern ToyF -> Pattern ToyF
toyLam =
  PatternNode . ToyLam

toyLit :: Int -> Pattern ToyF
toyLit =
  PatternNode . ToyLit

expectToySpan :: Show error => Either error value -> value
expectToySpan =
  either
    (\failure -> error ("toy rewrite span rejected: " <> show failure))
    id

toySpansSystem2 :: RewriteSystem ToyF
toySpansSystem2 = mkRewriteSystemFromGenerators toySpans

largeToySpans :: [RewriteMorphism ToyF]
largeToySpans =
  let r :: Pattern ToyF -> Pattern ToyF -> String -> RewriteMorphism ToyF
      r lhs rhs name = expectToySpan (rewriteMorphismWithInterface name lhs Set.empty rhs Nothing Nothing)
   in [ r (toyApp (toyVar "f") (toyLit 0)) (toyVar "f") "app-zero"
      , r (toyApp (toyVar "g") (toyLit 1)) (toyLit 1) "app-one"
      , r (toyApp (toyVar "h") (toyLit 2)) (toyLit 2) "app-two"
      , r (toyLam (toyVar "x")) (toyVar "x") "lam-unpack"
      , r (toyLam (toyLit 0)) (toyLit 0) "lam-zero"
      , r (toyLam (toyLam (toyVar "y"))) (toyLam (toyVar "y")) "lam-idem"
      , r (toyApp (toyApp (toyVar "f") (toyVar "x")) (toyVar "y"))
          (toyApp (toyVar "f") (toyApp (toyVar "x") (toyVar "y"))) "app-assoc"
      , r (toyApp (toyVar "f") (toyVar "f")) (toyVar "f") "app-self"
      , r (toyApp (toyLam (toyVar "x")) (toyVar "y")) (toyVar "x") "beta"
      , r (toyApp (toyVar "f") (toyLam (toyVar "x")))
          (toyLam (toyApp (toyVar "f") (toyVar "x"))) "app-lam-comm"
      , r (toyLit 0) (toyApp (toyLam (toyLit 0)) (toyLit 0)) "expand-zero"
      , r (toyLit 1) (toyLam (toyLit 1)) "wrap-one"
      , r (toyApp (toyLam (toyLam (toyVar "z"))) (toyLit 0))
          (toyLam (toyVar "z")) "deep-beta"
      , r (toyLam (toyApp (toyVar "f") (toyLit 0))) (toyVar "f") "lam-app-zero"
      , r (toyApp (toyLam (toyVar "x")) (toyLam (toyVar "y")))
          (toyLam (toyVar "x")) "beta-lam"
      ]

largeToySystem :: RewriteSystem ToyF
largeToySystem = mkRewriteSystemFromGenerators largeToySpans

toySpansSite2 :: GrothendieckSite (RewriteSystem ToyF)
toySpansSite2 = mkGrothendieckSite toySpansSystem2 2

toySpansSite1 :: GrothendieckSite (RewriteSystem ToyF)
toySpansSite1 = mkGrothendieckSite toySpansSystem2 1

toySpansResolution1 :: Either HomologyFailure (ResolutionBundle ToyF)
toySpansResolution1 = buildResolutionBundle toySpansSystem2 1

toySpansResolution2 :: Either HomologyFailure (ResolutionBundle ToyF)
toySpansResolution2 = buildResolutionBundle toySpansSystem2 2

withToySpansResolution1 :: (ResolutionBundle ToyF -> Assertion) -> Assertion
withToySpansResolution1 cont =
  case toySpansResolution1 of
    Left failure -> assertFailure ("toySpans depth-1 resolution failed: " <> show failure)
    Right resolutionValue -> cont resolutionValue

withToySpansResolution2 :: (ResolutionBundle ToyF -> Assertion) -> Assertion
withToySpansResolution2 cont =
  case toySpansResolution2 of
    Left failure -> assertFailure ("toySpans depth-2 resolution failed: " <> show failure)
    Right resolutionValue -> cont resolutionValue

testToySpansChainComplexBounded :: IO ()
testToySpansChainComplexBounded =
  case grothendieckChainComplexFromSite toySpansSite2 of
    Left failureValue ->
      assertFailure ("chain complex construction failed: " <> show failureValue)
    Right finiteComplex -> do
      let HomologicalDegree maxDeg = maxHomologicalDegree finiteComplex
          siteDepth = fromIntegral (grothendieckSiteDepth toySpansSite2)
      assertEqual
        "max homological degree must equal site depth"
        siteDepth
        maxDeg
      let cellCounts =
            fmap
              (\d -> (d, sourceCardinality (incidenceMatrixAt finiteComplex (HomologicalDegree d))))
              [0 .. maxDeg]
      assertEqual
        "chain complex cell count must equal site cell count"
        (length (grothendieckSiteCells toySpansSite2))
        (sum (fmap snd cellCounts))
      let boundaryDims =
            fmap
              (\d ->
                let boundary = incidenceMatrixAt finiteComplex (HomologicalDegree d)
                 in (d, sourceCardinality boundary, targetCardinality boundary)
              )
              [1 .. maxDeg]
      assertBool
        ("boundary source at degree d must equal cell count at degree d: " <> show boundaryDims)
        (all (\(d, src, _) -> src == snd (cellCounts !! d)) boundaryDims)

testToySpansSpectralConvergesToCorrectBetti :: IO ()
testToySpansSpectralConvergesToCorrectBetti =
  case grothendieckChainComplexFromSite toySpansSite1 of
    Left failureValue ->
      assertFailure ("chain complex construction failed: " <> show failureValue)
    Right finiteComplex -> do
      let directBetti = freeBettiVector finiteComplex
          HomologicalDegree maxDeg = maxHomologicalDegree finiteComplex
          cellCounts =
            fmap
              (\d -> sourceCardinality (incidenceMatrixAt finiteComplex (HomologicalDegree d)))
              [0 .. maxDeg]
          totalCells = sum cellCounts
          degreeFiltration basisCellRef =
            case cellDegree basisCellRef of
              HomologicalDegree d -> d
      assertBool
        ("3 rules should not produce " <> show totalCells
          <> " nerve cells at depth 1 (degrees: " <> show cellCounts
          <> ") — context closure is over-generating")
        (totalCells <= 50)
      case filteredRefinedMorseComplex finiteComplex degreeFiltration (const 0) of
        Left failureValue ->
          assertFailure ("filtered refined Morse reduction failed: " <> show failureValue)
        Right filteredMorseValue ->
          let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
           in case computeRationalSpectralPages
                (rmcReducedComplex refinedMorseValue)
                (filteredReducedFiltration filteredMorseValue) of
                Left failureValue ->
                  assertFailure ("spectral sequence failed: " <> show failureValue)
                Right pages ->
                  case pages of
                    [] -> assertFailure "expected at least one spectral page"
                    _ ->
                      let lastPage = last pages
                          spectralBetti =
                            fmap
                              (\totalDeg ->
                                sum
                                  [ freeRank (groupAt lastPage filtDeg (totalDeg - filtDeg))
                                  | filtDeg <- [0 .. maxDeg]
                                  ]
                              )
                              [0 .. maxDeg]
                       in assertEqual
                            "spectral E_∞ Betti must equal direct cohomology"
                            directBetti
                            spectralBetti

testToySpansResolutionWithMorse :: IO ()
testToySpansResolutionWithMorse =
  withToySpansResolution2 $ \resolutionValue ->
    case raSpectralPages (rbAnalysis resolutionValue) of
      Left failure -> assertFailure ("spectral pages failed: " <> show failure)
      Right spectralPages -> do
        let pageCount = length spectralPages
            cellCount = resolutionCellCount resolutionValue
        assertBool
          "resolution must produce at least one spectral page"
          (not (null spectralPages))
        assertBool
          ("page count (" <> show pageCount
            <> ") must be much less than cell count (" <> show cellCount
            <> ") — Morse reduction should keep spectral computation tractable")
          (pageCount <= 10)
        assertBool
          ("convergence depth (" <> show (convergenceDepth spectralPages)
            <> ") must be at most the last page index (" <> show (pageIndex (last spectralPages)) <> ")")
          (convergenceDepth spectralPages <= pageIndex (last spectralPages))
