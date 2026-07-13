module Moonlight.EGraph.Introspection.NerveSpec.Global.Homology
  ( tests,
  )
where

import Moonlight.Core (ZipMatch (..))
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.NerveSpec.Global.Prelude hiding (reducedComplex)
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (mkLocalClosed)
import Numeric.Natural (Natural)
import Moonlight.Derived.Site (FinObjectId (..))
import Data.IntSet qualified as IS
import Moonlight.Sheaf.Site (grothendieckChainComplexFromSite)
import Moonlight.Sheaf.Site.Analysis.Microsupport
  ( ContextOrderComplex (..),
    buildContextOrderComplex,
    localMicrosupport,
    localMicrosupportFromGenerators,
  )
import Moonlight.Sheaf.Site
  ( LinearizedRestrictionModel,
    buildLinearizedRestrictionModel,
    interfaceStalkBasisLinearization,
    linearizedRestrictionComparableRestrictions,
    linearizedRestrictionStalkDimensions,
  )
import Moonlight.EGraph.Introspection.Analysis.Obstruction qualified as IntrospectionObstruction
import Moonlight.EGraph.Introspection.Analysis.Obstruction
  ( nerveObstructionTower,
    nerveObstructions,
    nerveObstructionsAtDegree,
  )
import Moonlight.EGraph.Introspection.Analysis.Reduction (reducedComplex)
import Moonlight.EGraph.Introspection.ContextSpec (toySpansSystem2, largeToySystem)
import System.CPUTime
import Moonlight.Derived.Morse (hypercohomologyDims, prepareMicrosupport)
import Moonlight.Derived.Complex (mkLocallyRestrictableDerived)
import Moonlight.Derived.Failure (derivedFailureToMoonlightError)

tests :: TestTree
tests =
  testGroup
    "homology"
    [ testCase "termination analysis keeps certificates separate from heuristics" testTerminationAnalysis,
      testCase "free-face reduction preserves Betti data without enlarging the carrier" testReduction,
      testCase "Morse critical counts dominate Betti numbers on the reversible fixture" testMorseCriticalCountBound,
      testCase "collapsed derivations retain composed witness evidence" testCollapsedDerivationWitness,
      testCase "degree-1 obstruction representatives satisfy the cocycle condition" testObstructionCocycleCondition,
      testCase "acyclic rewrite chains have no degree-1 obstructions" testAcyclicChainNoObstructions,
      testCase "H¹ obstruction extraction maps cocycles back to supporting Grothendieck cells" testObstructionExtraction,
      testCase "resolution construction caches scaffold, Morse, Leray, and microsupport data" testResolution,
      testCase "spectral gate never prunes a node that microsupport keeps (single rule)" testSpectralGateSubsumptionSingleRule,
      testCase "spectral gate never prunes a node that microsupport keeps (acyclic chain)" testSpectralGateSubsumptionAcyclicChain,
      testCase "spectral gate preserves microsupport-critical nodes (reversible depth 1)" testSpectralGateSubsumptionMultiRule,
      testCase "spectral gate provides independent filtering beyond microsupport (single rule)" testSpectralGateIndependenceSingleRule,
      testCase "spectral gate provides independent filtering beyond microsupport (acyclic chain)" testSpectralGateIndependenceAcyclicChain,
      testCase "spectral-pruned nodes retain witness classification (reversible depth 1)" testSpectralPrunedNodeIsObstructed,
      testCase "verdier gate provides independent filtering beyond microsupport (single rule)" testVerdierIndependenceSingleRule,
      testCase "verdier gate provides independent filtering beyond microsupport (reversible depth 1)" testVerdierIndependenceReversible,
      testCase "verdier gate provides independent filtering beyond microsupport (acyclic chain depth 2)" testVerdierIndependenceAcyclicChain,
      testCase "verdier gate provides independent filtering beyond microsupport (reversible depth 2)" testVerdierIndependenceReversibleDepth2,
      testCase "rewrite-stalk linearization exposes typed feature bases" testInterfaceStalkLinearization,
      testCase "order complex litmus: single rule" (orderComplexLitmusFor singleRuleSystem 1),
      testCase "order complex litmus: reversible" (orderComplexLitmusFor reversibleSystem 1),
      testCase "order complex litmus: acyclic chain" (orderComplexLitmusFor identifiedAcyclicChainSystem 2),
      testCase "order complex litmus: disjoint" (orderComplexLitmusFor disjointSystem 1),
      testCase "local microsaturation: single rule" (localMicrosaturationFor singleRuleSystem 1),
      testCase "local microsaturation: reversible" (localMicrosaturationFor reversibleSystem 1),
      testCase "local microsaturation: acyclic chain" (localMicrosaturationFor identifiedAcyclicChainSystem 2),
      testCase "local microsaturation: disjoint" (localMicrosaturationFor disjointSystem 1),
      testCase "local microsaturation: toySpans (3 rules, β₁=64)" testToySpansLocalMicrosaturation,
      testCase "local microsaturation: 12 rules (arith)" testTwelveRuleLocalMicrosaturation,
      testCase "generator-level soundness: toySpans" testGeneratorSoundnessToySpans,
      testCase "generator-level soundness: single rule" (generatorSoundnessFor singleRuleSystem 1),
      testCase "generator-level soundness: reversible" (generatorSoundnessFor reversibleSystem 1),
      testCase "generator-level soundness: disjoint" (generatorSoundnessFor disjointSystem 1),
      testCase "generator-only: 15 rules (no closure)" testLargeToyGeneratorOnly,
      testCase "reduced vanishing: disjoint rules are non-critical" testReducedVanishingDisjoint,
      testCase "reduced vanishing: single rule is non-critical" testReducedVanishingSingleRule
    ]

testTerminationAnalysis :: Assertion
testTerminationAnalysis =
  case (analyzeTermination acyclicChainSystem 2, analyzeTermination disjointSystem 1, identifiedMultiContextSystemResult) of
    (Left failure, _, _) ->
      assertFailure (show failure)
    (_, Left failure, _) ->
      assertFailure (show failure)
    (_, _, Left failure) ->
      assertFailure (show failure)
    (Right acyclicAnalysis, Right disconnectedAnalysis, Right runtimeRewriteSystem) ->
      case (analyzeTermination runtimeRewriteSystem 2, analyzeTerminationWithTrace acyclicTrace runtimeRewriteSystem 2) of
        (Left failure, _) ->
          assertFailure (show failure)
        (_, Left failure) ->
          assertFailure (show failure)
        (Right baseRuntimeAnalysis, Right runtimeAnalysis) -> do
          assertBool
            "expected the chain example to be structurally acyclic"
            (clAcyclic (taCertificate acyclicAnalysis))
          assertEqual
            "expected the chain example to have zero cycle rank"
            0
            (hlCycleRank (taHeuristic acyclicAnalysis))
          assertEqual
            "expected Grothendieck Tarski consistency to populate the heuristic layer without contaminating certificates"
            (Just 1.0)
            (hlConsistencyRadius (taHeuristic acyclicAnalysis))
          assertEqual
            "expected unimplemented obstruction checks to stay explicit"
            Nothing
            (clObstructionFree (taCertificate acyclicAnalysis))
          assertEqual
            "expected the certificate layer to carry verified single-context nilpotence evidence"
            SingleContextNilpotent
            (clCoboundaryNilpotenceEvidence (taCertificate acyclicAnalysis))
          assertEqual
            "expected the disconnected example to preserve its component heuristic"
            2
            (hlConnectedComponents (taHeuristic disconnectedAnalysis))
          assertBool
            "expected heuristic density to remain available independently of certificate fields"
            ( maybe False (> 0) (hlRestrictionDensity (taHeuristic disconnectedAnalysis))
                && clConfluent (taCertificate disconnectedAnalysis) == Nothing
            )
          assertEqual
            "expected execution-derived counts to remain dynamic-only heuristic data"
            (Just 4, Just 2)
            ( hlExecutionVertexCount (taHeuristic disconnectedAnalysis),
              hlExecutionTransitionCount (taHeuristic disconnectedAnalysis)
            )
          assertEqual
            "expected non-trace termination analysis to leave runtime-relative heuristic fields unresolved"
            (Nothing, Nothing, Nothing, Nothing)
            ( hlObservedGroundedMorphismGap (taHeuristic baseRuntimeAnalysis),
              hlObservedGroundedChainCoverage (taHeuristic baseRuntimeAnalysis),
              hlRuntimeAmbiguityPressure (taHeuristic baseRuntimeAnalysis),
              hlRuntimeUnmappedGroundedNodeCount (taHeuristic baseRuntimeAnalysis)
            )
          assertEqual
            "expected trace enrichment to preserve the certificate layer"
            (taCertificate baseRuntimeAnalysis)
            (taCertificate runtimeAnalysis)
          assertEqual
            "expected trace enrichment to preserve scheduler-aware structural heuristics"
            ( hlInfluenceEdgeCount (taHeuristic baseRuntimeAnalysis),
              hlInfluenceBoundedEdgeCount (taHeuristic baseRuntimeAnalysis),
              hlInfluenceCooldownPressure (taHeuristic baseRuntimeAnalysis),
              hlStaticDynamicGap (taHeuristic baseRuntimeAnalysis)
            )
            ( hlInfluenceEdgeCount (taHeuristic runtimeAnalysis),
              hlInfluenceBoundedEdgeCount (taHeuristic runtimeAnalysis),
              hlInfluenceCooldownPressure (taHeuristic runtimeAnalysis),
              hlStaticDynamicGap (taHeuristic runtimeAnalysis)
            )
          assertEqual
            "expected runtime-aware termination heuristics to expose no observed grounded morphism gap on the fully observed identified fixture"
            (Just 0)
            (hlObservedGroundedMorphismGap (taHeuristic runtimeAnalysis))
          assertEqual
            "expected runtime-aware termination heuristics to expose full observed grounded chain coverage on the fully observed identified fixture"
            (Just 1.0)
            (fmap boundedRatioValue (hlObservedGroundedChainCoverage (taHeuristic runtimeAnalysis)))
          assertEqual
            "expected runtime-aware termination heuristics to expose zero ambiguity pressure on the identified fixture"
            (Just 0.0)
            (fmap boundedRatioValue (hlRuntimeAmbiguityPressure (taHeuristic runtimeAnalysis)))
          assertEqual
            "expected runtime-aware termination heuristics to expose zero unmapped grounded nodes on the identified fixture"
            (Just 0)
            (hlRuntimeUnmappedGroundedNodeCount (taHeuristic runtimeAnalysis))
          assertBool
            "expected observed grounded chain coverage to remain a normalized ratio"
            (maybe False (\coverageValue -> coverageValue >= 0.0 && coverageValue <= 1.0) (fmap boundedRatioValue (hlObservedGroundedChainCoverage (taHeuristic runtimeAnalysis))))
          assertBool
            "expected runtime ambiguity pressure to remain a normalized ratio"
            (maybe False (\pressureValue -> pressureValue >= 0.0 && pressureValue <= 1.0) (fmap boundedRatioValue (hlRuntimeAmbiguityPressure (taHeuristic runtimeAnalysis))))

testReduction :: Assertion
testReduction =
  case reduceNerve singleRuleSystem 1 of
    Left failure ->
      assertFailure (show failure)
    Right reductionValue -> do
      assertEqual
        "expected free-face collapse to preserve the Betti vector"
        (normalizeBettiVector (freeBettiVector (mrOriginalComplex reductionValue)))
        (normalizeBettiVector (freeBettiVector (reducedComplex reductionValue)))
      assertBool
        "expected the reduced complex not to exceed the original carrier size"
        (complexCellCount (reducedComplex reductionValue) <= complexCellCount (mrOriginalComplex reductionValue))
      assertBool
        "expected Morse critical analysis to retain at least one critical cell"
        (not (null (mrCriticalCells reductionValue)))
      assertEqual
        "expected reduced retention to be indexed by Morse critical cells"
        (mrCriticalCells reductionValue)
        (mrRetainedCells reductionValue)
      assertEqual
        "expected the reduction provenance to agree with the underlying Morse complex"
        (mrMatching reductionValue)
        (mcMatching (mrMorseComplex reductionValue))

testMorseCriticalCountBound :: Assertion
testMorseCriticalCountBound =
  case reduceNerve reversibleSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right reductionValue ->
      assertBool
        "expected Morse critical counts to dominate Betti numbers in every degree"
        (morseCriticalCountBoundHolds reductionValue)

testCollapsedDerivationWitness :: Assertion
testCollapsedDerivationWitness =
  case reduceNerve reversibleSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right reductionValue -> do
      let collapsedWitnesses = collapsedDerivations reductionValue
      assertBool
        "expected the reversible fixture to collapse at least one derivable pair"
        (not (null collapsedWitnesses))
      assertBool
        "expected every collapsed derivation to carry composed witness evidence"
        (all (collapsedWitnessIsComposed . third) collapsedWitnesses)

testObstructionCocycleCondition :: Assertion
testObstructionCocycleCondition =
  case (grothendieckChainComplexFromSite (mkGrothendieckSite reversibleSystem 2), nerveObstructions reversibleSystem 2) of
    (Left failure, _) ->
      assertFailure (show failure)
    (_, Left failure) ->
      assertFailure (show failure)
    (Right chainComplexValue, Right obstructionClasses) ->
      assertBool
        "expected every extracted degree-1 obstruction representative to land in the coboundary kernel"
        (all (obstructionRepresentativeClosed chainComplexValue) obstructionClasses)

testAcyclicChainNoObstructions :: Assertion
testAcyclicChainNoObstructions =
  case nerveObstructions acyclicChainSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right obstructionClasses ->
      assertBool
        "expected an acyclic rewrite chain to have trivial degree-1 obstruction extraction"
        (null obstructionClasses)

testObstructionExtraction :: Assertion
testObstructionExtraction =
  let explicitRepresentative =
        RepresentativeChain
          { representativeDegree = HomologicalDegree 1,
            representativeTerms = [(1, 0)]
          }
   in case
        ( nerveObstructions reversibleSystem 2,
          nerveObstructionsAtDegree reversibleSystem 2 (HomologicalDegree 1),
          nerveObstructionTower reversibleSystem 2 [HomologicalDegree 1],
          nerveObstructions singleRuleSystem 1,
          IntrospectionObstruction.interpretObstructionRepresentative reversibleSystem 2 explicitRepresentative
        ) of
    (Left failure, _, _, _, _) ->
      assertFailure (show failure)
    (_, Left failure, _, _, _) ->
      assertFailure (show failure)
    (_, _, Left failure, _, _) ->
      assertFailure (show failure)
    (_, _, _, Left failure, _) ->
      assertFailure (show failure)
    (_, _, _, _, Left failure) ->
      assertFailure (show failure)
    (Right reversibleObstructions, Right degreeOneObstructions, Right towerObstructions, Right singleRuleObstructions, Right interpretedObstruction) -> do
      assertBool
        "expected the single-rule system to have no degree-1 obstruction classes"
        (null singleRuleObstructions)
      assertBool
        "expected the reversible fixture to keep actual H¹ extraction honest when no degree-1 classes survive"
        (null reversibleObstructions)
      assertBool
        "expected the degree-indexed obstruction API to agree with the degree-1 default"
        ( obstructionSignatures reversibleObstructions == obstructionSignatures degreeOneObstructions
            && case towerObstructions of
              [towerDegreeOne] ->
                obstructionSignatures degreeOneObstructions == obstructionSignatures towerDegreeOne
              _ -> False
        )
      assertBool
        "expected the explicit representative interpretation to stay in degree 1 and land on 1-cells"
        ( IntrospectionObstruction.ocDegree interpretedObstruction == HomologicalDegree 1
            && all ((== 1) . grothendieckCellDimension) (IntrospectionObstruction.ocSupportingCells interpretedObstruction)
        )
      let interpretationValue = IntrospectionObstruction.ocInterpretation interpretedObstruction
          witnessValues = fmap snd (IntrospectionObstruction.oiWitnessEvidence interpretationValue)
          derivedProfile = IntrospectionObstruction.ocDerivedProfile interpretedObstruction
          harmonicLoops = IntrospectionObstruction.oiHarmonicLoops interpretationValue
      assertBool
        "expected explicit representative interpretation to align cells, coefficients, and witness evidence"
        ( length (IntrospectionObstruction.ocSupportingCells interpretedObstruction) == length (IntrospectionObstruction.oiCellEvaluations interpretationValue)
            && length (IntrospectionObstruction.ocSupportingCells interpretedObstruction) == length (IntrospectionObstruction.oiWitnessEvidence interpretationValue)
            && all
              ( \witnessValue ->
                  case witnessValue of
                    TerminalWitness -> False
                    _ -> True
              )
              witnessValues
        )
      assertBool
        "expected obstruction enrichment to carry a non-empty constant-coefficient derived transport profile"
        ( not (IntMap.null (IntrospectionObstruction.cdpAmbientHypercohomology derivedProfile))
            && not (IntMap.null (IntrospectionObstruction.cdpSupportHypercohomology derivedProfile))
            && not (IntMap.null (IntrospectionObstruction.cdpExtendedSupportHypercohomology derivedProfile))
        )
      assertBool
        "expected harmonic enrichment to report only degree-1 loops when available"
        ( all ((== HomologicalDegree 1) . harmonicLoopDegree) harmonicLoops
            && maybe True (const (null harmonicLoops)) (IntrospectionObstruction.oiHarmonicFailure interpretationValue)
        )

testResolution :: Assertion
testResolution =
  case (buildResolutionBundle singleRuleSystem 1, reduceNerve singleRuleSystem 1) of
    (Left failure, _) ->
      assertFailure (show failure)
    (_, Left failure) ->
      assertFailure (show failure)
    (Right resolutionValue, Right reductionValue) -> do
      assertEqual
        "expected the resolution to retain the rewrite-system witness"
        (length (rsContexts (rkRewriteSystem (rbKernel resolutionValue))))
        (length (rsContexts singleRuleSystem))
      assertEqual
        "expected the stalk cache to cover every Grothendieck cell exactly once"
        (resolutionCellCount resolutionValue)
        (Map.size (rkStalkCache (rbKernel resolutionValue)))
      case raMorse (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("morse computation failed: " <> show failure)
        Right morseValue ->
          assertEqual
            "expected the resolution Morse cache to agree with standalone reduction provenance"
            (mcMatching morseValue)
            (mrMatching reductionValue)
      case raLerayProfile (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("leray profile failed: " <> show failure)
        Right lerayProfile ->
          assertBool
            "expected the Leray profile to contain at least one graded entry"
            (not (IntMap.null lerayProfile))
      case raBoundaryAnalysis (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("boundary analysis failed: " <> show failure)
        Right boundaryAnalysis -> do
          assertEqual
            "expected the boundary analysis to cover every source node"
            (resolutionCellCount resolutionValue)
            (Map.size (rbaBasisCellBySourceNode boundaryAnalysis))
          assertBool
            "expected the boundary analysis to compute at least one spectral page"
            (not (null (rbaSpectralPages boundaryAnalysis)))
          assertBool
            "expected the boundary analysis to compute at least one poset-cohomology degree"
            (not (null (rbaPosetCohomologyDims boundaryAnalysis)))
          assertBool
            "expected the boundary analysis to carry stalk dimensions for every source node"
            (Map.size (rbaStalkDimensions boundaryAnalysis) == resolutionCellCount resolutionValue)
          assertBool
            "expected the boundary analysis to materialize comparable restrictions"
            (not (Map.null (rbaComparableRestrictions boundaryAnalysis)))
      case explicitGrothendieckCochain (mkGrothendieckSite singleRuleSystem 1) of
        Left _ -> pure ()
        Right coboundaryCache ->
          assertBool
            "expected the cached coboundary to remain nilpotent"
            (checkCoboundaryNilpotence coboundaryCache)
      let microsupportValue = rkMicrosupport (rbKernel resolutionValue)
      assertEqual
        "expected the microsupport summary counts to balance"
        (mrCriticalCount microsupportValue + mrNoncriticalCount microsupportValue)
        (length (mrCriticalFibers microsupportValue))
      assertEqual
        "expected contextual microsupport fibers to align with the rewrite-context lattice"
        (length (rsContexts (rkRewriteSystem (rbKernel resolutionValue))))
        (length (mrCriticalFibers microsupportValue))

testSpectralGateSubsumptionSingleRule :: Assertion
testSpectralGateSubsumptionSingleRule =
  spectralGateSubsumptionFor singleRuleSystem 1

explicitGrothendieckCochain siteValue =
  buildGrothendieckCochainArtifact
    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
    Right
    (MaterializedSite siteValue)

testSpectralGateSubsumptionAcyclicChain :: Assertion
testSpectralGateSubsumptionAcyclicChain =
  spectralGateSubsumptionFor identifiedAcyclicChainSystem 2

spectralGateSubsumptionFor :: (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) => RewriteSystem f -> Natural -> Assertion
spectralGateSubsumptionFor rewriteSystem depth =
  case buildResolutionBundle rewriteSystem depth of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raBoundaryAnalysis (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("boundary analysis failed: " <> show failure)
        Right boundaryAnalysis -> do
          let microsupportCritical =
                Set.fromList
                  [ nodeId
                  | (FinObjectId ordinal, Critical) <- mrCriticalFibers (rkMicrosupport (rbKernel resolutionValue)),
                    let nodeId = RegionNodeId ordinal
                  ]
              spectralPages = rbaSpectralPages boundaryAnalysis
              basisCellByNode = rbaBasisCellBySourceNode boundaryAnalysis
              bidegreesByCell = rbaBidegreesByBasisCell boundaryAnalysis
              finalPageNumber = convergenceDepth spectralPages
              oracle =
                SpectralPruningOracle
                  { spoPages = spectralPages,
                    spoBidegreeOfCell =
                      \basisCellRef -> Map.lookup basisCellRef bidegreesByCell
                  }
              spectralKeeps =
                Set.fromList
                  [ nodeId
                  | nodeId <- resolutionSourceNodes resolutionValue,
                    case Map.lookup nodeId basisCellByNode of
                      Nothing -> True
                      Just cellRef ->
                        spectralPruningGate oracle finalPageNumber id cellRef
                  ]
              microsupportKeptButSpectralPruned =
                Set.difference microsupportCritical spectralKeeps
          assertEqual
            ("spectral gate should not prune any node that microsupport keeps — "
              <> "if non-empty, spectral gate provides information beyond microsupport: "
              <> show microsupportKeptButSpectralPruned)
            Set.empty
            microsupportKeptButSpectralPruned

testSpectralGateIndependenceSingleRule :: Assertion
testSpectralGateIndependenceSingleRule =
  spectralGateIndependenceFor
    singleRuleSystem
    1
    (Set.fromList [RegionNodeId 1, RegionNodeId 2])

testSpectralGateIndependenceAcyclicChain :: Assertion
testSpectralGateIndependenceAcyclicChain =
  spectralGateIndependenceFor
    identifiedAcyclicChainSystem
    2
    (Set.fromList [RegionNodeId 2, RegionNodeId 3, RegionNodeId 4, RegionNodeId 5, RegionNodeId 6])

spectralGateIndependenceFor ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Set.Set RegionNodeId ->
  Assertion
spectralGateIndependenceFor rewriteSystem depth expectedSpectralOnlyPrunes =
  case buildResolutionBundle rewriteSystem depth of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raBoundaryAnalysis (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("boundary analysis failed: " <> show failure)
        Right boundaryAnalysis -> do
          let microsupportNoncritical =
                Set.fromList
                  [ RegionNodeId ordinal
                  | (FinObjectId ordinal, NonCritical) <- mrCriticalFibers (rkMicrosupport (rbKernel resolutionValue))
                  ]
              spectralPages = rbaSpectralPages boundaryAnalysis
              basisCellByNode = rbaBasisCellBySourceNode boundaryAnalysis
              bidegreesByCell = rbaBidegreesByBasisCell boundaryAnalysis
              finalPageNumber = convergenceDepth spectralPages
              oracle =
                SpectralPruningOracle
                  { spoPages = spectralPages,
                    spoBidegreeOfCell =
                      \basisCellRef -> Map.lookup basisCellRef bidegreesByCell
                  }
              spectralPrunes =
                Set.fromList
                  [ nodeId
                  | nodeId <- resolutionSourceNodes resolutionValue,
                    case Map.lookup nodeId basisCellByNode of
                      Nothing -> False
                      Just cellRef ->
                        not (spectralPruningGate oracle finalPageNumber id cellRef)
                  ]
              spectralPrunesButMicrosupportKeeps =
                Set.difference spectralPrunes microsupportNoncritical
          assertEqual
            ("spectral gate should expose the filtered-Morse-only pruning section; got "
              <> show spectralPrunesButMicrosupportKeeps)
            expectedSpectralOnlyPrunes
            spectralPrunesButMicrosupportKeeps

testSpectralGateSubsumptionMultiRule :: Assertion
testSpectralGateSubsumptionMultiRule =
  case buildResolutionBundle reversibleSystem 1 of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raBoundaryAnalysis (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("boundary analysis failed: " <> show failure)
        Right boundaryAnalysis -> do
          let microsupportCritical =
                Set.fromList
                  [ RegionNodeId ordinal
                  | (FinObjectId ordinal, Critical) <- mrCriticalFibers (rkMicrosupport (rbKernel resolutionValue))
                  ]
              spectralPages = rbaSpectralPages boundaryAnalysis
              basisCellByNode = rbaBasisCellBySourceNode boundaryAnalysis
              bidegreesByCell = rbaBidegreesByBasisCell boundaryAnalysis
              finalPageNumber = convergenceDepth spectralPages
              oracle =
                SpectralPruningOracle
                  { spoPages = spectralPages,
                    spoBidegreeOfCell =
                      \basisCellRef -> Map.lookup basisCellRef bidegreesByCell
                  }
              spectralKeeps =
                Set.fromList
                  [ nodeId
                  | nodeId <- resolutionSourceNodes resolutionValue,
                    case Map.lookup nodeId basisCellByNode of
                      Nothing -> True
                      Just cellRef ->
                        spectralPruningGate oracle finalPageNumber id cellRef
                  ]
              microsupportKeptButSpectralPruned =
                Set.difference microsupportCritical spectralKeeps
          case raRepresentativeCocycles (rbAnalysis resolutionValue) (HomologicalDegree 1) of
            Left failure ->
              assertFailure ("cocycles failed: " <> show failure)
            Right cocycles ->
              assertBool
                ("reversible system should have H1 cocycles for nontrivial cohomology, got "
                  <> show (length cocycles))
                (not (null cocycles))
          assertEqual
            ("spectral should not prune what microsupport keeps: " <> show microsupportKeptButSpectralPruned)
            Set.empty
            microsupportKeptButSpectralPruned

testSpectralPrunedNodeIsObstructed :: Assertion
testSpectralPrunedNodeIsObstructed =
  case buildResolutionBundle reversibleSystem 1 of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raBoundaryAnalysis (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("boundary analysis failed: " <> show failure)
        Right boundaryAnalysis -> do
          let spectralPages = rbaSpectralPages boundaryAnalysis
              basisCellByNode = rbaBasisCellBySourceNode boundaryAnalysis
              bidegreesByCell = rbaBidegreesByBasisCell boundaryAnalysis
              finalPageNumber = convergenceDepth spectralPages
              oracle =
                SpectralPruningOracle
                  { spoPages = spectralPages,
                    spoBidegreeOfCell =
                      \basisCellRef -> Map.lookup basisCellRef bidegreesByCell
                  }
              witnessByNode = resolutionWitnessClassesBySourceNode resolutionValue
              spectralPrunedNodes =
                [ nodeId
                | nodeId <- resolutionSourceNodes resolutionValue,
                  case Map.lookup nodeId basisCellByNode of
                    Nothing -> False
                    Just cellRef ->
                      not (spectralPruningGate oracle finalPageNumber id cellRef)
                ]
          assertBool
            "expected at least one spectrally-pruned node on the reversible system"
            (not (null spectralPrunedNodes))
          let prunedWithWitness =
                fmap
                  (\nodeId -> (nodeId, Map.lookup nodeId witnessByNode))
                  spectralPrunedNodes
          assertBool
            ("every spectrally-pruned node should retain a witness classification, got: "
              <> show prunedWithWitness)
            (all
              (isJust . snd)
              prunedWithWitness
            )

testVerdierIndependenceSingleRule :: Assertion
testVerdierIndependenceSingleRule =
  verdierIndependenceFor singleRuleSystem 1

testVerdierIndependenceReversible :: Assertion
testVerdierIndependenceReversible =
  verdierIndependenceFor reversibleSystem 1

testVerdierIndependenceAcyclicChain :: Assertion
testVerdierIndependenceAcyclicChain =
  verdierIndependenceFor identifiedAcyclicChainSystem 2

testVerdierIndependenceReversibleDepth2 :: Assertion
testVerdierIndependenceReversibleDepth2 =
  verdierIndependenceFor reversibleSystem 2

verdierIndependenceFor :: (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) => RewriteSystem f -> Natural -> Assertion
verdierIndependenceFor rewriteSystem depth =
  case buildResolutionBundle rewriteSystem depth of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue -> do
      derivedComplex <-
        case resolutionDerivedComplex resolutionValue of
          Left failure ->
            assertFailure ("resolution derived complex failed: " <> show failure)
          Right derivedValue ->
            pure derivedValue
      verdierDecisions <-
        traverse
          ( \nodeId@(RegionNodeId ordinal) ->
              case
                first show (mkLocalClosed (resolutionSourcePoset resolutionValue) (IS.singleton ordinal))
                  >>= first show . verdierLocalClosedGate (prepareVerdierPruning (resolutionSourcePoset resolutionValue) derivedComplex)
              of
                Left failure -> assertFailure ("Verdier decision failed: " <> failure)
                Right keepValue -> pure (nodeId, keepValue)
          )
          (resolutionSourceNodes resolutionValue)
      let microsupportCritical =
            Set.fromList
              [ RegionNodeId ordinal
              | (FinObjectId ordinal, Critical) <- mrCriticalFibers (rkMicrosupport (rbKernel resolutionValue))
              ]
          microsupportNoncritical =
            Set.fromList
              [ RegionNodeId ordinal
              | (FinObjectId ordinal, NonCritical) <- mrCriticalFibers (rkMicrosupport (rbKernel resolutionValue))
              ]
          verdierKeeps =
            Set.fromList
              [ nodeIdValue
              | (nodeIdValue, True) <- verdierDecisions
              ]
          verdierPrunes =
            Set.fromList (resolutionSourceNodes resolutionValue) `Set.difference` verdierKeeps
          microsupportKeptButVerdierPruned =
            Set.difference microsupportCritical verdierKeeps
          verdierPrunedButMicrosupportKept =
            Set.difference verdierPrunes microsupportNoncritical
      assertEqual
        ("verdier should not prune any node microsupport keeps "
          <> "(if non-empty, Verdier provides independent filtering — "
          <> "SS(D(F)) ≠ SS(F) on this complex): "
          <> show microsupportKeptButVerdierPruned)
        Set.empty
        microsupportKeptButVerdierPruned
      assertEqual
        ("verdier should not independently prune beyond microsupport "
          <> "(if non-empty, Verdier is stricter than microsupport): "
          <> show verdierPrunedButMicrosupportKept)
        Set.empty
        verdierPrunedButMicrosupportKept

testInterfaceStalkLinearization :: Assertion
testInterfaceStalkLinearization =
  let sourceStalk :: InterfaceStalk ()
      sourceStalk =
        InterfaceStalk
          { rsBoundNames = Set.singleton (interfaceNameFromString "x"),
            rsDeletedNames = Set.singleton (interfaceNameFromString "y"),
            rsCreatedNames = Set.empty,
            rsGuarded = True,
            rsWitness = TerminalWitness,
            rsCellDimension = 1
          }
      targetStalk :: InterfaceStalk ()
      targetStalk =
        InterfaceStalk
          { rsBoundNames = Set.singleton (interfaceNameFromString "x"),
            rsDeletedNames = Set.empty,
            rsCreatedNames = Set.empty,
            rsGuarded = False,
            rsWitness = TerminalWitness,
            rsCellDimension = 0
          }
      linearizedRestrictions =
        buildLinearizedRestrictionModel
          (Map.fromList [("source", sourceStalk), ("target", targetStalk)])
          (\sourceCell targetCell -> sourceCell == targetCell || (sourceCell == ("source" :: String) && targetCell == "target"))
          interfaceStalkBasisLinearization ::
          LinearizedRestrictionModel String Int
   in do
        assertEqual
          "expected the source stalk to linearize into bound, deleted, guarded, and witness coordinates"
          (Just 4)
          (Map.lookup "source" (linearizedRestrictionStalkDimensions linearizedRestrictions))
        assertEqual
          "expected the target stalk to retain the shared bound and witness coordinates"
          (Just 2)
          (Map.lookup "target" (linearizedRestrictionStalkDimensions linearizedRestrictions))
        case Map.lookup ("source", "target") (linearizedRestrictionComparableRestrictions linearizedRestrictions) of
          Nothing ->
            assertFailure "expected a comparable restriction from the source stalk to the target stalk"
          Just incidence ->
            assertEqual
              "expected the comparable projection to preserve the shared bound and witness coordinates"
              [(0, 0, 1), (3, 1, 1)]
              ( fmap
                  (\entryValue -> (sourceIndex entryValue, targetIndex entryValue, boundaryCoefficient entryValue))
                  (boundaryEntries incidence)
              )

orderComplexLitmusFor ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Assertion
orderComplexLitmusFor rewriteSystem depth =
  case buildResolutionBundle rewriteSystem depth of
    Left failure ->
      assertFailure ("grothendieck resolution failed: " <> show failure)
    Right resolutionValue -> do
      grothendieckDerived <-
        case resolutionDerivedComplex resolutionValue of
          Left failure ->
            assertFailure ("grothendieck derived failed: " <> show failure)
          Right derivedValue ->
            pure derivedValue
      let grothendieckBetti = hypercohomologyDims grothendieckDerived
          grothendieckCritical = resolutionCriticalMicrosupportNodes resolutionValue
          grothendieckCellCount = resolutionCellCount resolutionValue
          contextPosetResult = contextPosetFromRewriteSystem rewriteSystem
      case contextPosetResult of
        Left failure ->
          assertFailure ("context poset failed: " <> show failure)
        Right contextPoset ->
          case buildContextOrderComplex contextPoset of
            Left failure ->
              assertFailure ("order complex failed: " <> show failure)
            Right orderComplex -> do
              orderDerived <-
                case derivedFromFiniteChainComplex (cocChainComplex orderComplex) of
                  Left failure ->
                    assertFailure ("order derived failed: " <> show failure)
                  Right derivedValue ->
                    pure derivedValue
              let orderBetti = hypercohomologyDims orderDerived
                  orderSimplexCounts = cocSimplexCount orderComplex
                  totalOrderSimplices = sum (IntMap.elems orderSimplexCounts)
                  orderMicrosupport =
                    first derivedFailureToMoonlightError
                      (mkLocallyRestrictableDerived (cocSourcePoset orderComplex) orderDerived)
                      >>= first derivedFailureToMoonlightError
                        . prepareMicrosupport
                          (cocSourcePoset orderComplex)
                          contextPoset
                          (either (error . show) id . cocProjection orderComplex)
                      >>= computeMicrosupport
              assertBool
                ( "order complex should be smaller than grothendieck nerve: "
                    <> "order=" <> show totalOrderSimplices
                    <> " grothendieck=" <> show grothendieckCellCount
                    <> " by-degree=" <> show (IntMap.toList orderSimplexCounts)
                )
                True
              let contextCount = length (allContexts rewriteSystem)
              case (grothendieckBetti, orderBetti) of
                (Right gBetti, Right oBetti) ->
                  assertBool
                    ( "contexts=" <> show contextCount
                        <> " order-simplices=" <> show (IntMap.toList orderSimplexCounts)
                        <> " grothendieck-cells=" <> show grothendieckCellCount
                        <> " grothendieck-betti=" <> show (IntMap.toList gBetti)
                        <> " order-betti=" <> show (IntMap.toList oBetti)
                    )
                    True
                (Left gErr, _) ->
                  assertFailure ("grothendieck betti failed: " <> show gErr)
                (_, Left oErr) ->
                  assertFailure ("order betti failed: " <> show oErr)
              case orderMicrosupport of
                Left failure ->
                  assertFailure ("order complex microsupport failed: " <> show failure)
                Right orderMicrosupportResult -> do
                  let orderCritical =
                        Set.fromList
                          [ RegionNodeId ordinal
                          | (FinObjectId ordinal, Critical) <- mrCriticalFibers orderMicrosupportResult
                          ]
                  assertBool
                    ( "grothendieck-critical=" <> show grothendieckCritical
                        <> " order-critical=" <> show orderCritical
                    )
                    True

localMicrosaturationFor ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Assertion
localMicrosaturationFor rewriteSystem depth =
  case buildResolutionBundle rewriteSystem depth of
    Left failure ->
      assertFailure ("resolution failed: " <> show failure)
    Right resolutionValue ->
      case localMicrosupportFromGenerators rewriteSystem of
        Left failure ->
          assertFailure ("generator microsaturation failed: " <> show failure)
        Right genResult -> do
          let resolutionCritical = resolutionCriticalMicrosupportNodes resolutionValue
              genCritical =
                Set.fromList
                  [ RegionNodeId ordinal
                  | (FinObjectId ordinal, Critical) <- mrCriticalFibers genResult
                  ]
          assertEqual
            ( "resolution must use generator-only microsupport:"
                <> " resolution=" <> show resolutionCritical
                <> " gen=" <> show genCritical
            )
            resolutionCritical
            genCritical

twelveRuleSystem :: RewriteSystem ArithF
twelveRuleSystem =
  let v0 :: Pattern ArithF
      v0 = PatternVar (EGraph.mkPatternVar 0)
      v1 :: Pattern ArithF
      v1 = PatternVar (EGraph.mkPatternVar 1)
      sumTerm :: Pattern ArithF -> Pattern ArithF -> Pattern ArithF
      sumTerm a b = PatternNode (Add a b)
      mulTerm :: Pattern ArithF -> Pattern ArithF -> Pattern ArithF
      mulTerm a b = PatternNode (Mul a b)
      negTerm :: Pattern ArithF -> Pattern ArithF
      negTerm a = PatternNode (Neg a)
      lit n = PatternNode (Num n)
      bothSupport = Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1]
      singleSupport = Set.singleton (EGraph.mkPatternVar 0)
      ruleSpan :: Pattern ArithF -> Pattern ArithF -> Set.Set EGraph.PatternVar -> String -> RewriteMorphism ArithF
      ruleSpan lhs rhs iface name =
        expectHomologySpan (rewriteMorphismWithInterface name lhs iface rhs Nothing Nothing)
   in mkRewriteSystem
        [ ruleSpan (sumTerm v0 (lit 0)) v0 singleSupport "add-zero"
        , ruleSpan (sumTerm v0 v1) (sumTerm v1 v0) bothSupport "add-commute"
        , ruleSpan (sumTerm (sumTerm v0 v1) (lit 1)) (sumTerm v0 (sumTerm v1 (lit 1))) bothSupport "add-assoc"
        , ruleSpan (mulTerm v0 (lit 1)) v0 singleSupport "mul-one"
        , ruleSpan (mulTerm v0 v1) (mulTerm v1 v0) bothSupport "mul-commute"
        , ruleSpan (mulTerm v0 (sumTerm v1 (lit 1))) (sumTerm (mulTerm v0 v1) v0) bothSupport "distribute"
        , ruleSpan (negTerm (negTerm v0)) v0 singleSupport "double-neg"
        , ruleSpan (sumTerm v0 (negTerm v0)) (lit 0) Set.empty "add-neg-cancel"
        , ruleSpan (mulTerm v0 (lit 0)) (lit 0) Set.empty "mul-zero"
        , ruleSpan (negTerm (sumTerm v0 v1)) (sumTerm (negTerm v0) (negTerm v1)) bothSupport "neg-distribute"
        , ruleSpan (mulTerm (negTerm v0) v1) (negTerm (mulTerm v0 v1)) bothSupport "neg-mul-left"
        , ruleSpan (sumTerm v0 v0) (mulTerm (lit 2) v0) singleSupport "double-to-mul"
        ]

expectHomologySpan :: Show error => Either error value -> value
expectHomologySpan =
  either
    (\failure -> error ("homology rewrite span rejected: " <> show failure))
    id

testTwelveRuleLocalMicrosaturation :: Assertion
testTwelveRuleLocalMicrosaturation = do
  t0 <- getCPUTime
  case localMicrosupport twelveRuleSystem of
    Left failure -> do
      t1 <- getCPUTime
      let ms = fromIntegral (t1 - t0) / 1e9 :: Double
      assertFailure ("12-rule local microsaturation failed in " <> show ms <> "ms: " <> show failure)
    Right localResult -> do
      let localCritical =
            Set.fromList
              [ RegionNodeId ordinal
              | (FinObjectId ordinal, Critical) <- mrCriticalFibers localResult
              ]
          localFiberCount =
            mrCriticalCount localResult + mrNoncriticalCount localResult
      t1 <- seq localFiberCount (seq (Set.size localCritical) getCPUTime)
      let localMs = fromIntegral (t1 - t0) / 1e9 :: Double
      assertBool
        ( "12-rule local microsaturation:"
            <> " time=" <> show localMs <> "ms"
            <> " fibers=" <> show localFiberCount
            <> " critical=" <> show (Set.size localCritical)
        )
        (localMs < 5000)

testToySpansLocalMicrosaturation :: Assertion
testToySpansLocalMicrosaturation =
  case (localMicrosupportFromGenerators toySpansSystem2, buildResolutionBundle toySpansSystem2 1) of
    (Left failure, _) ->
      assertFailure ("generator microsaturation failed: " <> show failure)
    (_, Left failure) ->
      assertFailure ("resolution failed: " <> show failure)
    (Right genResult, Right resolutionValue) -> do
      let resolutionCritical = resolutionCriticalMicrosupportNodes resolutionValue
          genCritical =
            Set.fromList
              [ RegionNodeId ordinal
              | (FinObjectId ordinal, Critical) <- mrCriticalFibers genResult
              ]
      assertEqual
        "toySpans: resolution microsupport must match generator-only"
        resolutionCritical
        genCritical

testGeneratorSoundnessToySpans :: Assertion
testGeneratorSoundnessToySpans =
  case (localMicrosupport toySpansSystem2, localMicrosupportFromGenerators toySpansSystem2) of
    (Left failure, _) ->
      assertFailure ("closed-lattice failed: " <> show failure)
    (_, Left failure) ->
      assertFailure ("generator-only failed: " <> show failure)
    (Right closedResult, Right genResult) -> do
      let genOrdinals =
            Set.fromList [RegionNodeId o | (FinObjectId o, _) <- mrCriticalFibers genResult]
          closedCriticalAtGenerators =
            Set.fromList
              [ RegionNodeId o
              | (FinObjectId o, Critical) <- mrCriticalFibers closedResult
              , Set.member (RegionNodeId o) genOrdinals
              ]
          genCritical =
            Set.fromList
              [RegionNodeId o | (FinObjectId o, Critical) <- mrCriticalFibers genResult]
      assertBool
        ( "generator-level soundness: closed∩generators ⊆ gen-critical"
            <> " closed∩gen=" <> show closedCriticalAtGenerators
            <> " gen=" <> show genCritical
        )
        (Set.isSubsetOf closedCriticalAtGenerators genCritical)

generatorSoundnessFor ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Assertion
generatorSoundnessFor rewriteSystem depth =
  case (buildResolutionBundle rewriteSystem depth, localMicrosupportFromGenerators rewriteSystem) of
    (Left failure, _) ->
      assertFailure ("resolution failed: " <> show failure)
    (_, Left failure) ->
      assertFailure ("generator-only failed: " <> show failure)
    (Right resolutionValue, Right genResult) -> do
      let genOrdinals =
            Set.fromList [RegionNodeId o | (FinObjectId o, _) <- mrCriticalFibers genResult]
          grothendieckCriticalAtGenerators =
            Set.intersection (resolutionCriticalMicrosupportNodes resolutionValue) genOrdinals
          genCritical =
            Set.fromList
              [RegionNodeId o | (FinObjectId o, Critical) <- mrCriticalFibers genResult]
      assertBool
        ( "generator-level soundness vs grothendieck:"
            <> " grothendieck∩gen=" <> show grothendieckCriticalAtGenerators
            <> " gen=" <> show genCritical
        )
        (Set.isSubsetOf grothendieckCriticalAtGenerators genCritical)

testLargeToyGeneratorOnly :: Assertion
testLargeToyGeneratorOnly = do
  t0 <- getCPUTime
  case localMicrosupportFromGenerators largeToySystem of
    Left failure -> do
      t1 <- getCPUTime
      let ms = fromIntegral (t1 - t0) / 1e9 :: Double
      assertFailure ("15-rule generator-only failed in " <> show ms <> "ms: " <> show failure)
    Right genResult -> do
      let fiberCount = mrCriticalCount genResult + mrNoncriticalCount genResult
      t1 <- seq fiberCount getCPUTime
      let ms = fromIntegral (t1 - t0) / 1e9 :: Double
      assertBool
        ( "15-rule generator-only:"
            <> " time=" <> show ms <> "ms"
            <> " fibers=" <> show fiberCount
            <> " critical=" <> show (mrCriticalCount genResult)
        )
        (ms < 1000)

testReducedVanishingDisjoint :: Assertion
testReducedVanishingDisjoint =
  case localMicrosupportFromGenerators disjointSystem of
    Left failure ->
      assertFailure ("disjoint microsupport failed: " <> show failure)
    Right result ->
      assertEqual
        "disjoint rules must have zero critical fibers"
        0
        (mrCriticalCount result)

testReducedVanishingSingleRule :: Assertion
testReducedVanishingSingleRule =
  case localMicrosupportFromGenerators singleRuleSystem of
    Left failure ->
      assertFailure ("single rule microsupport failed: " <> show failure)
    Right result ->
      assertEqual
        "single rule must have zero critical fibers"
        0
        (mrCriticalCount result)
