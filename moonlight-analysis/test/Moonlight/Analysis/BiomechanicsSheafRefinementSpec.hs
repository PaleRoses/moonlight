{-# LANGUAGE PackageImports #-}
module Moonlight.Analysis.BiomechanicsSheafRefinementSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Moonlight.Analysis
  ( BiomechanicalAnatomicalBlueprintProgram (..),
    BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBoneConstraint,
    BiomechanicalCandidateMaterializationFailure (..),
    BiomechanicalBlueprint (..),
    BiomechanicalBlueprintInvariantViolation (..),
    BiomechanicalElasticSpectralSignature (..),
    BiomechanicalGraphSpectralSignature (..),
    BiomechanicalRefinementDetail (..),
    BiomechanicalRefinementModel,
    BiomechanicalRankDimension (..),
    BiomechanicalRankPolicy (..),
    BiomechanicalRank (..),
    BiomechanicalRoundLimit,
    BiomechanicalScore (..),
    BiomechanicalScorePolicy,
    BiomechanicalSpectralSignature (..),
    BiomechanicalBoneName (..),
    BiomechanicalJointName (..),
    BiomechanicalLexicographicRankOrder,
    BiomechanicalSolvePolicy (..),
    BiomechanicalSite (..),
    BiomechanicalSolveFailure (..),
    BiomechanicalStalk (..),
    BiomechanicalStructuralName (..),
    BiomechanicalTolerance,
    MinimumBiomechanicalJointCount,
    SheafRefiner (..),
    Vec3 (..),
    biomechanicalRefinerWithAnatomy,
    defaultBiomechanicalSolvePolicy,
    biomechanicalRefinerWithPolicies,
    biomechanicalRefinerWithSolvePolicy,
    biomechanicalRefiner,
    defaultBiomechanicalAnatomicalBlueprintProgram,
    defaultBiomechanicalScorePolicy,
    defaultBiomechanicalSpectralPolicy,
    materializeBiomechanicalCandidate,
    mkBiomechanicalAnchorFidelityEnergy,
    mkBiomechanicalBoneConstraint,
    mkBiomechanicalElasticStrainEnergy,
    mkBiomechanicalLexicographicRankOrder,
    mkBiomechanicalRefinementModel,
    mkBiomechanicalRefinementModelWithAnatomy,
    mkBiomechanicalRoundLimit,
    mkBiomechanicalScorePolicy,
    mkBiomechanicalSpectralPolicyExtended,
    mkBiomechanicalStructuralCoherenceEnergy,
    mkBiomechanicalTolerance,
    mkBiomechanicalVolumetricPreservationEnergy,
    mkMinimumBiomechanicalJointCount,
    prepareBiomechanicalModel,
    refineBiomechanicalCompiledWithMatcher,
    refineSheafCompiledWithMatcher,
    solveBiomechanicalCandidateDetailed,
    withBiomechanicalRankPolicy,
  )
import Moonlight.Analysis.SheafRefinement (SheafRefinementModel (..))
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement
  ( graphSpectralDistance,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compilePatternQuery,
    singlePatternQuery,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    combineCompiledGuards,
    compileGuard,
  )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Core (Substitution (..))
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    emptyEGraph,
  )
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Core
  ( ContinuousBinding (..),
    ContinuousSubstitution (..),
    FuzzyMatch (..),
    FuzzyRank (..),
    RefinementCandidate (..),
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount,
    analysisSpec,
  )
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Refiner (CompiledSeedMatcher)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertBool, assertFailure, testCase)

assertApproxEqual :: String -> Double -> Double -> Assertion
assertApproxEqual message expectedValue actualValue =
  assertBool
    message
    (abs (expectedValue - actualValue) <= 1.0e-9)

mkGraphSignature :: [Int] -> [Int] -> [Double] -> BiomechanicalGraphSpectralSignature
mkGraphSignature positiveSupportSizes negativeSupportSizes supportCriticalities =
  BiomechanicalGraphSpectralSignature
    { bgssEigenvalues = [],
      bgssSpectralGap = 0.0,
      bgssPositiveSupportSizes = positiveSupportSizes,
      bgssNegativeSupportSizes = negativeSupportSizes,
      bgssSupportCriticalities = supportCriticalities
    }

tests :: TestTree
tests =
  testGroup
    "biomechanics-sheaf-refinement"
    [ testCase "minimum biomechanical joint count rejects negative values" $
        mkMinimumBiomechanicalJointCount (-1) @?= Nothing,
      testCase "graph spectral distance treats empty support summaries as zero drift" $
        assertApproxEqual
          "expected empty signatures to have zero graph spectral drift"
          0.0
          (graphSpectralDistance (mkGraphSignature [] [] []) (mkGraphSignature [] [] [])),
      testCase "graph spectral distance averages padded criticalities for mismatched support lengths" $
        assertApproxEqual
          "expected missing criticalities to pad with unit weight before averaging"
          1.8
          ( graphSpectralDistance
              (mkGraphSignature [3] [] [0.2])
              (mkGraphSignature [] [] [])
          ),
      testCase "graph spectral distance defaults missing weight entries to one" $
        assertApproxEqual
          "expected missing support criticalities to fall back to unit-weight support drift"
          4.0
          ( graphSpectralDistance
              (mkGraphSignature [2, 4] [] [])
              (mkGraphSignature [] [] [])
          ),
      testCase "compiled-query refinement solves a simple two-joint chain while preserving the exact witness" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        matchValue <-
          expectSingleMatch
            "expected one biomechanical refined match"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery reachableTargetRefiner) compiledQuery emptyGraph)
        fmRootClass matchValue @?= ClassId 8
        fmDiscreteSubstitution matchValue @?= expectedDiscreteSubstitution
        bmdJointCount (fmDetail matchValue) @?= 2
        bmdBoneCount (fmDetail matchValue) @?= 1
        bmdSatisfiedRestrictionCount (fmDetail matchValue) @?= 2
        assertBool
          "expected nonnegative combined spectral drift"
          (bmdSpectralDrift (fmDetail matchValue) >= 0.0)
        assertBool
          "expected target residual to improve over the anchored effector distance"
          (bmdEndEffectorResidual (fmDetail matchValue) < jointDistance reachableTarget (Vec3 1.0 0.0 0.0))
        assertBool
          "expected nonnegative biomechanical strain energy"
          (bmdStrainEnergy (fmDetail matchValue) >= 0.0)
        assertBool
          "expected nonnegative biomechanical topological energy"
          (bmdTopologicalEnergy (fmDetail matchValue) >= 0.0)
        assertBool
          "expected nonnegative biomechanical anchor-fidelity energy"
          (bmdAnchorFidelityEnergy (fmDetail matchValue) >= 0.0)
        assertBool
          "expected nonnegative biomechanical structural coherence energy"
          (bmdStructuralCoherenceEnergy (fmDetail matchValue) >= 0.0)
        assertBool
          "expected nonnegative biomechanical volumetric preservation energy"
          (bmdVolumetricPreservationEnergy (fmDetail matchValue) >= 0.0)
        let graphSignature = bssGraphSignature (bmdSpectralSignature (fmDetail matchValue))
        assertBool
          "expected graph spectral support summaries for each eigenvalue"
          ( length (bgssPositiveSupportSizes graphSignature) == length (bgssEigenvalues graphSignature)
              && length (bgssNegativeSupportSizes graphSignature) == length (bgssEigenvalues graphSignature)
              && length (bgssSupportCriticalities graphSignature) == length (bgssEigenvalues graphSignature)
          )
        assertBool
          "expected all support criticalities in (0,1]"
          (all (\critValue -> critValue > 0.0 && critValue <= 1.0) (bgssSupportCriticalities graphSignature))
        case bssElasticSignature (bmdSpectralSignature (fmDetail matchValue)) of
          Just elasticSignature ->
            do
              assertBool
                "expected solved biomechanics to expose elastic spectral modes"
                (not (null (bessEigenvalues elasticSignature)))
              assertBool
                "expected nonnegative elastic localization summary"
                (bessMeanLocalization elasticSignature >= 0.0)
              assertBool
                "expected nonnegative structural-mode penalty"
                (bessStructuralModePenalty elasticSignature >= 0.0)
              assertBool
                "expected nonnegative volumetric-mode penalty"
                (bessVolumetricModePenalty elasticSignature >= 0.0)
          Nothing ->
            assertFailure "expected the solved biomechanics detail to carry an elastic spectral signature"
        expectJointBindingSite 0 (BiomechanicalJointSite (EGraph.mkPatternVar 0) (ClassId 80)) matchValue
        expectJointBindingSite 1 (BiomechanicalJointSite (EGraph.mkPatternVar 1) (ClassId 81)) matchValue,
      testCase "named shoulder/hip/hand blueprint refines successfully" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        matchValue <-
          expectSingleMatch
            "expected one anatomical biomechanical refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery branchedTargetRefiner) compiledQuery emptyGraph)
        fmRootClass matchValue @?= anatomicalBlueprintRootClass
        fmDiscreteSubstitution matchValue @?= expectedBranchedDiscreteSubstitution
        bmdJointCount (fmDetail matchValue) @?= 3
        bmdBoneCount (fmDetail matchValue) @?= 2
        bmdSatisfiedRestrictionCount (fmDetail matchValue) @?= 4
        assertBool
          "expected nonnegative combined spectral drift"
          (bmdSpectralDrift (fmDetail matchValue) >= 0.0)
        assertBool
          "expected the named hand effector residual to improve over its anchored target distance"
          (bmdEndEffectorResidual (fmDetail matchValue) < jointDistance branchedTarget handAnchorPosition)
        expectJointBindingSite 0 shoulderJointSite matchValue
        expectJointBindingSite 1 hipJointSite matchValue
        expectJointBindingSite 2 handJointSite matchValue
        case lookupContinuousBinding 2 matchValue of
          Just continuousBinding ->
            case cbPayload continuousBinding of
              JointBiomechanicalStalk _ solvedPosition ->
                assertBool
                  "expected the named hand effector to move toward its target"
                  (jointDistance solvedPosition branchedTarget < jointDistance handAnchorPosition branchedTarget)
              _ ->
                assertFailure "expected the named hand effector binding to carry a joint stalk"
          Nothing ->
            assertFailure "expected the named hand effector binding to be present",
      testCase "domain-owned anatomy program can retarget the named effector joint" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        let model =
              mkBiomechanicalRefinementModelWithAnatomy
                shoulderEffectorAnatomyProgram
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                shoulderTarget
                lookupBranchedJointAnchor
                lookupBranchedBoneConstraint
            compiledBlueprint = compileSheafBlueprint (prepareBiomechanicalModel compiledQuery model)
        bmbInvariantViolations compiledBlueprint @?= []
        babEffectorJoint (bmbAnatomicalBlueprint compiledBlueprint)
          @?= Just (BiomechanicalJointName shoulderPatternVar (Just (pack "shoulder"))),
      testCase "invalid authored anatomy blueprint is reported and rejected before candidate materialization" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        let model =
              mkBiomechanicalRefinementModelWithAnatomy
                invalidEffectorAnatomyProgram
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                branchedTarget
                lookupBranchedJointAnchor
                lookupBranchedBoneConstraint
            compiledBlueprint = compileSheafBlueprint (prepareBiomechanicalModel compiledQuery model)
        bmbInvariantViolations compiledBlueprint
          @?= [EffectorJointMissingFromBlueprint invalidEffectorJointName]
        refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery invalidEffectorRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "missing joint anchors surface explicit candidate materialization failures" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        let model =
              mkBiomechanicalRefinementModel
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                reachableTarget
                lookupIncompleteJointAnchor
                lookupBoneConstraint
              :: BiomechanicalRefinementModel
            compiledBlueprint = compileSheafBlueprint (prepareBiomechanicalModel compiledQuery model)
              :: BiomechanicalBlueprint
        materializeBiomechanicalCandidate model compiledBlueprint (ClassId 8, expectedDiscreteSubstitution)
          @?= Left [MissingBiomechanicalJointAnchor (BiomechanicalJointSite (EGraph.mkPatternVar 1) (ClassId 81)) (ClassId 81)],
      testCase "missing bone constraints surface explicit candidate materialization failures" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        let model =
              mkBiomechanicalRefinementModel
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                reachableTarget
                lookupJointAnchor
                lookupMissingBoneConstraint
              :: BiomechanicalRefinementModel
            compiledBlueprint = compileSheafBlueprint (prepareBiomechanicalModel compiledQuery model)
              :: BiomechanicalBlueprint
        materializeBiomechanicalCandidate model compiledBlueprint (ClassId 8, expectedDiscreteSubstitution)
          @?= Left [MissingBiomechanicalBoneConstraint expectedDefaultBoneName (BiomechanicalJointSite (EGraph.mkPatternVar 0) (ClassId 80)) (BiomechanicalJointSite (EGraph.mkPatternVar 1) (ClassId 81))],
      testCase "solve-time missing anchors surface explicit solve failures" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        let model =
              mkBiomechanicalRefinementModel
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                reachableTarget
                lookupJointAnchor
                lookupBoneConstraint
              :: BiomechanicalRefinementModel
            compiledBlueprint = compileSheafBlueprint (prepareBiomechanicalModel compiledQuery model)
              :: BiomechanicalBlueprint
        candidate <-
          case materializeBiomechanicalCandidate model compiledBlueprint (ClassId 8, expectedDiscreteSubstitution) of
            Right candidateValue ->
              pure candidateValue
            Left failures ->
              assertFailure ("expected successful candidate materialization, got " <> show failures)
        let candidateWithoutEffectorAnchor =
              candidate
                { rcAnchors =
                    Map.delete
                      (BiomechanicalJointSite (EGraph.mkPatternVar 1) (ClassId 81))
                      (rcAnchors candidate)
                }
        solveBiomechanicalCandidateDetailed compiledBlueprint candidateWithoutEffectorAnchor
          @?= Left [MissingBiomechanicalAnchorPosition (BiomechanicalJointSite (EGraph.mkPatternVar 1) (ClassId 81))],
      testCase "refinement rejects chains with missing bone constraints" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery missingConstraintRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "missing named shoulder-to-hand bone rejects the anatomical blueprint" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery branchedMissingConstraintRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "specialized biomechanics backend helper matches the generic sheaf refinement helper" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        refineBiomechanicalCompiledWithMatcher (seedMatcher branchedSeedBackend) branchedTargetRefiner compiledQuery emptyGraph
          @?= refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery branchedTargetRefiner) compiledQuery emptyGraph,
      testCase "higher effector solve weight improves end-effector residual for a farther target" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        defaultMatch <-
          expectSingleMatch
            "expected one default-solve refined match"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery fartherTargetRefiner) compiledQuery emptyGraph)
        higherEffectorMatch <-
          expectSingleMatch
            "expected one higher-effector-weight refined match"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery higherEffectorSolvePolicyRefiner) compiledQuery emptyGraph)
        assertBool
          "expected higher effector weight to reduce the farther-target residual"
          (bmdEndEffectorResidual (fmDetail higherEffectorMatch) < bmdEndEffectorResidual (fmDetail defaultMatch)),
      testCase "higher volumetric solve weight worsens branched volumetric preservation energy" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        defaultMatch <-
          expectSingleMatch
            "expected one default branched refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery branchedTargetRefiner) compiledQuery emptyGraph)
        higherVolumetricMatch <-
          expectSingleMatch
            "expected one higher-volumetric-weight branched refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery higherVolumetricSolvePolicyRefiner) compiledQuery emptyGraph)
        assertBool
          "expected higher volumetric weight to increase branched volumetric-preservation energy"
          (bmdVolumetricPreservationEnergy (fmDetail higherVolumetricMatch) > bmdVolumetricPreservationEnergy (fmDetail defaultMatch)),
      testCase "typed score policy can emphasize volumetric preservation without changing the solved score vector" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        defaultMatch <-
          expectSingleMatch
            "expected one default-score branched refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery branchedTargetRefiner) compiledQuery emptyGraph)
        volumetricScoreMatch <-
          expectSingleMatch
            "expected one volumetric-score branched refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery volumetricHeavyScorePolicyRefiner) compiledQuery emptyGraph)
        fmScore volumetricScoreMatch @?= fmScore defaultMatch
        assertBool
          "expected the volumetric-heavy score policy to increase the volumetric rank component for the same solved score vector"
          ( bmrVolumetricPreservationComponent (unFuzzyRank (fmRank volumetricScoreMatch))
              > bmrVolumetricPreservationComponent (unFuzzyRank (fmRank defaultMatch))
          )
        assertBool
          "expected the volumetric-heavy score policy to worsen the scalar rank for the same solved score vector"
          (bmrTotal (unFuzzyRank (fmRank volumetricScoreMatch)) > bmrTotal (unFuzzyRank (fmRank defaultMatch))),
      testCase "strict elastic spectral policy rejects ill-conditioned biomechanics solves before acceptance" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        let strictSpectralPolicy =
              case mkBiomechanicalSpectralPolicyExtended 4 1.0e-6 1.0e-12 1.0e12 0 0.0 1.0e-6 0 0.0 of
                Just value ->
                  value
                Nothing ->
                  error "invalid strict spectral policy fixture"
            strictSpectralRefiner =
              biomechanicalRefinerWithPolicies
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                defaultBiomechanicalScorePolicy
                strictSpectralPolicy
                reachableTarget
                lookupJointAnchor
                lookupBoneConstraint
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery strictSpectralRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "rank policy can prefer lexicographic residual priority over scalar total" $ do
        let totalRankModel =
              mkBiomechanicalRefinementModel
                minimumBiomechanicalJointCount
                biomechanicalRoundLimit
                biomechanicalTolerance
                reachableTarget
                lookupJointAnchor
                lookupBoneConstraint
            lexicographicRankModel =
              withBiomechanicalRankPolicy
                (LexicographicBiomechanicalRankPolicy residualFirstRankOrder)
                totalRankModel
            totalBetterRank =
              FuzzyRank
                BiomechanicalRank
                  { bmrResidualComponent = 5.0,
                    bmrAnchorFidelityComponent = 0.0,
                    bmrElasticStrainComponent = 0.0,
                    bmrStructuralCoherenceComponent = 0.0,
                    bmrVolumetricPreservationComponent = 0.0,
                    bmrSpectralDriftComponent = 0.0,
                    bmrTotal = 5.0
                  }
            residualBetterRank =
              FuzzyRank
                BiomechanicalRank
                  { bmrResidualComponent = 1.0,
                    bmrAnchorFidelityComponent = 0.0,
                    bmrElasticStrainComponent = 0.0,
                    bmrStructuralCoherenceComponent = 0.0,
                    bmrVolumetricPreservationComponent = 10.0,
                    bmrSpectralDriftComponent = 0.0,
                    bmrTotal = 11.0
                  }
        compareSheafRanks totalRankModel totalBetterRank residualBetterRank
          @?= LT
        compareSheafRanks lexicographicRankModel totalBetterRank residualBetterRank
          @?= GT,
      testCase "pareto rank policy prefers componentwise dominance before scalar fallback" $ do
        let paretoRankModel =
              withBiomechanicalRankPolicy
                ParetoBiomechanicalRankPolicy
                ( mkBiomechanicalRefinementModel
                    minimumBiomechanicalJointCount
                    biomechanicalRoundLimit
                    biomechanicalTolerance
                    reachableTarget
                    lookupJointAnchor
                    lookupBoneConstraint
                )
            dominantRank =
              FuzzyRank
                BiomechanicalRank
                  { bmrResidualComponent = 1.0,
                    bmrAnchorFidelityComponent = 1.0,
                    bmrElasticStrainComponent = 1.0,
                    bmrStructuralCoherenceComponent = 1.0,
                    bmrVolumetricPreservationComponent = 1.0,
                    bmrSpectralDriftComponent = 1.0,
                    bmrTotal = 6.0
                  }
            dominatedRank =
              FuzzyRank
                BiomechanicalRank
                  { bmrResidualComponent = 2.0,
                    bmrAnchorFidelityComponent = 1.0,
                    bmrElasticStrainComponent = 1.0,
                    bmrStructuralCoherenceComponent = 1.0,
                    bmrVolumetricPreservationComponent = 1.0,
                    bmrSpectralDriftComponent = 1.0,
                    bmrTotal = 7.0
                  }
        compareSheafRanks paretoRankModel dominantRank dominatedRank
          @?= LT,
      testCase "higher stiffness on a mismatched shoulder-to-hand rest length worsens score and rank" $ do
        compiledQuery <- expectCompiledQuery branchedPattern
        lowerStiffnessMatch <-
          expectSingleMatch
            "expected one low-stiffness anatomical refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery lowStiffnessMismatchRefiner) compiledQuery emptyGraph)
        higherStiffnessMatch <-
          expectSingleMatch
            "expected one high-stiffness anatomical refined match"
            (refineSheafCompiledWithMatcher (seedMatcher branchedSeedBackend) (prepareBiomechanicalRefiner compiledQuery highStiffnessMismatchRefiner) compiledQuery emptyGraph)
        assertBool
          "expected higher stiffness to increase the strain-energy score"
          (bmsStrainEnergy (fmScore higherStiffnessMatch) > bmsStrainEnergy (fmScore lowerStiffnessMatch))
        assertBool
          "expected higher stiffness to worsen the scalar rank"
          (bmrTotal (unFuzzyRank (fmRank higherStiffnessMatch)) > bmrTotal (unFuzzyRank (fmRank lowerStiffnessMatch))),
      testCase "farther targets rank and score worse than reachable targets" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        reachableMatch <-
          expectSingleMatch
            "expected one refined match for the reachable target"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery reachableTargetRefiner) compiledQuery emptyGraph)
        fartherMatch <-
          expectSingleMatch
            "expected one refined match for the farther target"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareBiomechanicalRefiner compiledQuery fartherTargetRefiner) compiledQuery emptyGraph)
        assertBool
          "expected farther target residual to exceed reachable target residual"
          (bmsEndEffectorResidual (fmScore fartherMatch) > bmsEndEffectorResidual (fmScore reachableMatch))
        assertBool
          "expected farther target rank to exceed reachable target rank"
          (bmrTotal (unFuzzyRank (fmRank fartherMatch)) > bmrTotal (unFuzzyRank (fmRank reachableMatch)))
    ]

emptyGraph :: EGraph ArithF NodeCount
emptyGraph = emptyEGraph analysisSpec

prepareBiomechanicalRefiner ::
  CompiledPatternQuery (CompiledGuard () ArithF) ArithF ->
  SheafRefiner BiomechanicalRefinementModel ->
  SheafRefiner BiomechanicalRefinementModel
prepareBiomechanicalRefiner compiledQuery (SheafRefiner model) =
  SheafRefiner (prepareBiomechanicalModel compiledQuery model)

expectCompiledQuery :: Pattern ArithF -> IO (CompiledPatternQuery (CompiledGuard () ArithF) ArithF)
expectCompiledQuery patternValue =
  case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
    Left unboundPatternVars ->
      assertFailure
        ("expected compiled query to validate, got unbound vars " <> show unboundPatternVars)
    Right queryValue ->
      pure queryValue

expectSingleMatch ::
  String ->
  [FuzzyMatch BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail score rank] ->
  IO (FuzzyMatch BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail score rank)
expectSingleMatch failureMessage refinedMatches =
  case refinedMatches of
    [matchValue] ->
      pure matchValue
    _ ->
      assertFailure failureMessage

expectJointBindingSite ::
  Int ->
  BiomechanicalSite ->
  FuzzyMatch BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail score rank ->
  IO ()
expectJointBindingSite patternKey expectedSite matchValue =
  case lookupContinuousBinding patternKey matchValue of
    Just continuousBinding ->
      cbSite continuousBinding @?= expectedSite
    Nothing ->
      assertFailure ("expected continuous binding for pattern key " <> show patternKey)

samplePattern :: Pattern ArithF
samplePattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

branchedPattern :: Pattern ArithF
branchedPattern =
  PatternNode
    ( Add
        (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
        (PatternVar (EGraph.mkPatternVar 2))
    )

type SeedBackend :: Type
data SeedBackend = SeedBackend
  { compiledSeeds :: [(ClassId, Substitution)]
  }

seedMatcher :: SeedBackend -> CompiledSeedMatcher ArithF
seedMatcher backend _ _ =
  compiledSeeds backend

seedBackend :: SeedBackend
seedBackend =
  SeedBackend
    { compiledSeeds =
        [ (ClassId 8, expectedDiscreteSubstitution)
        ]
    }

branchedSeedBackend :: SeedBackend
branchedSeedBackend =
  SeedBackend
    { compiledSeeds =
        [ (anatomicalBlueprintRootClass, expectedBranchedDiscreteSubstitution)
        ]
    }

reachableTargetRefiner :: SheafRefiner BiomechanicalRefinementModel
reachableTargetRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    chainSolveRoundLimit
    biomechanicalTolerance
    reachableTarget
    lookupJointAnchor
    lookupBoneConstraint

fartherTargetRefiner :: SheafRefiner BiomechanicalRefinementModel
fartherTargetRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    chainSolveRoundLimit
    biomechanicalTolerance
    fartherTarget
    lookupJointAnchor
    lookupBoneConstraint

missingConstraintRefiner :: SheafRefiner BiomechanicalRefinementModel
missingConstraintRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    reachableTarget
    lookupJointAnchor
    lookupMissingBoneConstraint

branchedTargetRefiner :: SheafRefiner BiomechanicalRefinementModel
branchedTargetRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    branchedTarget
    lookupBranchedJointAnchor
    lookupBranchedBoneConstraint

invalidEffectorRefiner :: SheafRefiner BiomechanicalRefinementModel
invalidEffectorRefiner =
  biomechanicalRefinerWithAnatomy
    invalidEffectorAnatomyProgram
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    branchedTarget
    lookupBranchedJointAnchor
    lookupBranchedBoneConstraint

branchedMissingConstraintRefiner :: SheafRefiner BiomechanicalRefinementModel
branchedMissingConstraintRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    branchedTarget
    lookupBranchedJointAnchor
    lookupMissingBranchedBoneConstraint

lowStiffnessMismatchRefiner :: SheafRefiner BiomechanicalRefinementModel
lowStiffnessMismatchRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    stiffnessComparisonTarget
    lookupBranchedJointAnchor
    lookupLowStiffnessMismatchBoneConstraint

highStiffnessMismatchRefiner :: SheafRefiner BiomechanicalRefinementModel
highStiffnessMismatchRefiner =
  biomechanicalRefiner
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    stiffnessComparisonTarget
    lookupBranchedJointAnchor
    lookupHighStiffnessMismatchBoneConstraint

higherEffectorSolvePolicyRefiner :: SheafRefiner BiomechanicalRefinementModel
higherEffectorSolvePolicyRefiner =
  biomechanicalRefinerWithSolvePolicy
    higherEffectorSolvePolicy
    minimumBiomechanicalJointCount
    chainSolveRoundLimit
    biomechanicalTolerance
    fartherTarget
    lookupJointAnchor
    lookupBoneConstraint

higherVolumetricSolvePolicyRefiner :: SheafRefiner BiomechanicalRefinementModel
higherVolumetricSolvePolicyRefiner =
  biomechanicalRefinerWithSolvePolicy
    higherVolumetricSolvePolicy
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    branchedTarget
    lookupBranchedJointAnchor
    lookupBranchedBoneConstraint

volumetricHeavyScorePolicyRefiner :: SheafRefiner BiomechanicalRefinementModel
volumetricHeavyScorePolicyRefiner =
  biomechanicalRefinerWithPolicies
    minimumBiomechanicalJointCount
    biomechanicalRoundLimit
    biomechanicalTolerance
    volumetricHeavyScorePolicy
    defaultBiomechanicalSpectralPolicy
    branchedTarget
    lookupBranchedJointAnchor
    lookupBranchedBoneConstraint

minimumBiomechanicalJointCount :: MinimumBiomechanicalJointCount
minimumBiomechanicalJointCount =
  case mkMinimumBiomechanicalJointCount 2 of
    Just value ->
      value
    Nothing ->
      error "invalid minimum biomechanical joint count fixture"

biomechanicalRoundLimit :: BiomechanicalRoundLimit
biomechanicalRoundLimit =
  case mkBiomechanicalRoundLimit 12 of
    Just value ->
      value
    Nothing ->
      error "invalid biomechanical round limit fixture"

chainSolveRoundLimit :: BiomechanicalRoundLimit
chainSolveRoundLimit =
  case mkBiomechanicalRoundLimit 64 of
    Just value ->
      value
    Nothing ->
      error "invalid chain solve round limit fixture"

biomechanicalTolerance :: BiomechanicalTolerance
biomechanicalTolerance =
  case mkBiomechanicalTolerance 1.0e-9 of
    Just value ->
      value
    Nothing ->
      error "invalid biomechanical tolerance fixture"

higherEffectorSolvePolicy :: BiomechanicalSolvePolicy
higherEffectorSolvePolicy =
  typedSolvePolicyFixture
    "invalid higher effector solve policy fixture"
    1.0
    24.0
    1.0
    1.5
    3.0
    1.0e-9

higherVolumetricSolvePolicy :: BiomechanicalSolvePolicy
higherVolumetricSolvePolicy =
  typedSolvePolicyFixture
    "invalid higher volumetric solve policy fixture"
    1.0
    4.0
    1.0
    1.5
    12.0
    1.0e-9

volumetricHeavyScorePolicy :: BiomechanicalScorePolicy
volumetricHeavyScorePolicy =
  case mkBiomechanicalScorePolicy 1.0 1.0 1.0 1.0 12.0 1.0 of
    Just value ->
      value
    Nothing ->
      error "invalid volumetric-heavy score policy fixture"

residualFirstRankOrder :: BiomechanicalLexicographicRankOrder
residualFirstRankOrder =
  case
    mkBiomechanicalLexicographicRankOrder
      [ ResidualRankDimension,
        AnchorFidelityRankDimension,
        ElasticStrainRankDimension,
        StructuralCoherenceRankDimension,
        VolumetricPreservationRankDimension,
        SpectralDriftRankDimension
      ]
  of
    Just value ->
      value
    Nothing ->
      error "invalid residual-first biomechanical rank order fixture"

typedSolvePolicyFixture :: String -> Double -> Double -> Double -> Double -> Double -> Double -> BiomechanicalSolvePolicy
typedSolvePolicyFixture failureMessage jointAnchorWeight effectorTargetWeight boneWeight structuralWeight volumetricWeight regularizationWeight =
  case
    ( mkBiomechanicalAnchorFidelityEnergy jointAnchorWeight effectorTargetWeight,
      mkBiomechanicalElasticStrainEnergy boneWeight,
      mkBiomechanicalStructuralCoherenceEnergy structuralWeight,
      mkBiomechanicalVolumetricPreservationEnergy volumetricWeight
    ) of
    (Just anchorFidelityEnergy, Just elasticStrainEnergy, Just structuralCoherenceEnergy, Just volumetricPreservationEnergy) ->
      BiomechanicalSolvePolicy
        { bslpAnchorFidelity = anchorFidelityEnergy,
          bslpElasticStrain = elasticStrainEnergy,
          bslpStructuralCoherence = structuralCoherenceEnergy,
          bslpVolumetricPreservation = volumetricPreservationEnergy,
          bslpRegularizationWeight = regularizationWeight,
          bslpPreconditionerFamily = bslpPreconditionerFamily defaultBiomechanicalSolvePolicy
        }
    _ ->
      error failureMessage

reachableTarget :: Vec3
reachableTarget = Vec3 0.0 1.0 0.0

fartherTarget :: Vec3
fartherTarget = Vec3 0.0 2.0 0.0

branchedTarget :: Vec3
branchedTarget = Vec3 0.5 1.0 0.0

shoulderTarget :: Vec3
shoulderTarget = shoulderAnchorPosition

stiffnessComparisonTarget :: Vec3
stiffnessComparisonTarget = handAnchorPosition

shoulderEffectorAnatomyProgram :: BiomechanicalAnatomicalBlueprintProgram
shoulderEffectorAnatomyProgram =
  defaultBiomechanicalAnatomicalBlueprintProgram
    { babpJointNameForVar = labeledJointName,
      babpBoneNameForJoints = labeledBoneName,
      babpStructuralNameForPath = labeledStructuralName,
      babpEffectorJointForNames = selectShoulderEffector
    }

invalidEffectorAnatomyProgram :: BiomechanicalAnatomicalBlueprintProgram
invalidEffectorAnatomyProgram =
  defaultBiomechanicalAnatomicalBlueprintProgram
    { babpJointNameForVar = labeledJointName,
      babpBoneNameForJoints = labeledBoneName,
      babpStructuralNameForPath = labeledStructuralName,
      babpEffectorJointForNames = const (Just invalidEffectorJointName)
    }

invalidEffectorJointName :: BiomechanicalJointName
invalidEffectorJointName =
  BiomechanicalJointName (EGraph.mkPatternVar 999) (Just (pack "ghost"))

labeledJointName :: EGraph.PatternVar -> BiomechanicalJointName
labeledJointName patternVar =
  BiomechanicalJointName patternVar (jointLabel patternVar)

jointLabel :: EGraph.PatternVar -> Maybe Text
jointLabel patternVar
  | patternVar == shoulderPatternVar =
      Just (pack "shoulder")
  | patternVar == hipPatternVar =
      Just (pack "hip")
  | patternVar == handPatternVar =
      Just (pack "hand")
  | otherwise =
      Nothing

labeledBoneName :: [Int] -> BiomechanicalJointName -> BiomechanicalJointName -> BiomechanicalBoneName
labeledBoneName path sourceJoint targetJoint =
  BiomechanicalBoneName
    { biomechanicalBoneNamePath = path,
      biomechanicalBoneNameLabel = Just (pack (boneLabelText sourceJoint targetJoint)),
      bbnSourceJoint = sourceJoint,
      bbnTargetJoint = targetJoint
    }

boneLabelText :: BiomechanicalJointName -> BiomechanicalJointName -> String
boneLabelText sourceJoint targetJoint =
  jointLabelText sourceJoint <> "-" <> jointLabelText targetJoint

jointLabelText :: BiomechanicalJointName -> String
jointLabelText jointName =
  case biomechanicalJointNameLabel jointName of
    Just labelText ->
      unpack labelText
    Nothing ->
      show (biomechanicalJointPatternVar jointName)

labeledStructuralName :: [Int] -> [BiomechanicalJointName] -> BiomechanicalStructuralName
labeledStructuralName path incidentJoints =
  BiomechanicalStructuralName
    { biomechanicalStructuralPath = path,
      biomechanicalStructuralNameLabel = Just (pack ("support-" <> show (length incidentJoints)))
    }

selectShoulderEffector :: [BiomechanicalJointName] -> Maybe BiomechanicalJointName
selectShoulderEffector =
  foldr
    (\jointName maybeSelectedJoint ->
        if biomechanicalJointPatternVar jointName == shoulderPatternVar
          then Just jointName
          else maybeSelectedJoint
    )
    Nothing

lookupJointAnchor :: ClassId -> Maybe Vec3
lookupJointAnchor classId
  | classId == ClassId 80 =
      Just (Vec3 0.0 0.0 0.0)
  | classId == ClassId 81 =
      Just (Vec3 1.0 0.0 0.0)
  | otherwise =
      Nothing

lookupIncompleteJointAnchor :: ClassId -> Maybe Vec3
lookupIncompleteJointAnchor classId
  | classId == ClassId 80 =
      Just (Vec3 0.0 0.0 0.0)
  | otherwise =
      Nothing

lookupBranchedJointAnchor :: ClassId -> Maybe Vec3
lookupBranchedJointAnchor classId
  | classId == shoulderClassId =
      Just shoulderAnchorPosition
  | classId == hipClassId =
      Just hipAnchorPosition
  | classId == handClassId =
      Just handAnchorPosition
  | otherwise =
      Nothing

lookupBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupBoneConstraint leftClassId rightClassId
  | (leftClassId, rightClassId) `elem` constrainedJointPairs =
      Just boneConstraint
  | otherwise =
      Nothing

lookupMissingBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupMissingBoneConstraint _ _ = Nothing

lookupBranchedBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupBranchedBoneConstraint leftClassId rightClassId
  | (leftClassId, rightClassId) `elem` branchedConstrainedJointPairs =
      Just boneConstraint
  | otherwise =
      Nothing

lookupMissingBranchedBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupMissingBranchedBoneConstraint leftClassId rightClassId
  | (leftClassId, rightClassId) `elem` chainOnlyBranchedJointPairs =
      Just boneConstraint
  | otherwise =
      Nothing

lookupLowStiffnessMismatchBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupLowStiffnessMismatchBoneConstraint leftClassId rightClassId
  | (leftClassId, rightClassId) `elem` shoulderToHipJointPairs =
      Just boneConstraint
  | (leftClassId, rightClassId) `elem` shoulderToHandJointPairs =
      Just lowStiffnessMismatchBoneConstraint
  | otherwise =
      Nothing

lookupHighStiffnessMismatchBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
lookupHighStiffnessMismatchBoneConstraint leftClassId rightClassId
  | (leftClassId, rightClassId) `elem` shoulderToHipJointPairs =
      Just boneConstraint
  | (leftClassId, rightClassId) `elem` shoulderToHandJointPairs =
      Just highStiffnessMismatchBoneConstraint
  | otherwise =
      Nothing

boneConstraint :: BiomechanicalBoneConstraint
boneConstraint =
  case mkBiomechanicalBoneConstraint 1.0 4.0 of
    Just value ->
      value
    Nothing ->
      error "invalid biomechanical bone constraint fixture"

lowStiffnessMismatchBoneConstraint :: BiomechanicalBoneConstraint
lowStiffnessMismatchBoneConstraint =
  case mkBiomechanicalBoneConstraint 0.5 0.5 of
    Just value ->
      value
    Nothing ->
      error "invalid low-stiffness mismatch bone constraint fixture"

highStiffnessMismatchBoneConstraint :: BiomechanicalBoneConstraint
highStiffnessMismatchBoneConstraint =
  case mkBiomechanicalBoneConstraint 0.5 12.0 of
    Just value ->
      value
    Nothing ->
      error "invalid high-stiffness mismatch bone constraint fixture"

constrainedJointPairs :: [(ClassId, ClassId)]
constrainedJointPairs =
  [ (ClassId 80, ClassId 81),
    (ClassId 81, ClassId 80)
  ]

branchedConstrainedJointPairs :: [(ClassId, ClassId)]
branchedConstrainedJointPairs =
  shoulderToHipJointPairs <> shoulderToHandJointPairs

chainOnlyBranchedJointPairs :: [(ClassId, ClassId)]
chainOnlyBranchedJointPairs =
  shoulderToHipJointPairs

expectedDiscreteSubstitution :: Substitution
expectedDiscreteSubstitution =
  Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)])

expectedBranchedDiscreteSubstitution :: Substitution
expectedBranchedDiscreteSubstitution =
  Substitution
    (IntMap.fromList [(0, shoulderClassId), (1, hipClassId), (2, handClassId)])

expectedDefaultBoneName :: BiomechanicalBoneName
expectedDefaultBoneName =
  BiomechanicalBoneName
    { biomechanicalBoneNamePath = [],
      biomechanicalBoneNameLabel = Nothing,
      bbnSourceJoint = BiomechanicalJointName (EGraph.mkPatternVar 0) Nothing,
      bbnTargetJoint = BiomechanicalJointName (EGraph.mkPatternVar 1) Nothing
    }

anatomicalBlueprintRootClass :: ClassId
anatomicalBlueprintRootClass = ClassId 9

shoulderPatternVar :: EGraph.PatternVar
shoulderPatternVar = EGraph.mkPatternVar 0

hipPatternVar :: EGraph.PatternVar
hipPatternVar = EGraph.mkPatternVar 1

handPatternVar :: EGraph.PatternVar
handPatternVar = EGraph.mkPatternVar 2

shoulderClassId :: ClassId
shoulderClassId = ClassId 80

hipClassId :: ClassId
hipClassId = ClassId 81

handClassId :: ClassId
handClassId = ClassId 82

shoulderJointSite :: BiomechanicalSite
shoulderJointSite = BiomechanicalJointSite shoulderPatternVar shoulderClassId

hipJointSite :: BiomechanicalSite
hipJointSite = BiomechanicalJointSite hipPatternVar hipClassId

handJointSite :: BiomechanicalSite
handJointSite = BiomechanicalJointSite handPatternVar handClassId

shoulderAnchorPosition :: Vec3
shoulderAnchorPosition = Vec3 0.0 0.0 0.0

hipAnchorPosition :: Vec3
hipAnchorPosition = Vec3 1.0 0.0 0.0

handAnchorPosition :: Vec3
handAnchorPosition = Vec3 0.0 1.0 0.0

shoulderToHipJointPairs :: [(ClassId, ClassId)]
shoulderToHipJointPairs =
  [ (shoulderClassId, hipClassId),
    (hipClassId, shoulderClassId)
  ]

shoulderToHandJointPairs :: [(ClassId, ClassId)]
shoulderToHandJointPairs =
  [ (shoulderClassId, handClassId),
    (handClassId, shoulderClassId)
  ]

lookupContinuousBinding ::
  Int ->
  FuzzyMatch BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail score rank ->
  Maybe (ContinuousBinding BiomechanicalSite BiomechanicalStalk)
lookupContinuousBinding patternKey matchValue =
  IntMap.lookup patternKey (unContinuousSubstitution (fmContinuousSubstitution matchValue))

jointDistance :: Vec3 -> Vec3 -> Double
jointDistance (Vec3 leftX leftY leftZ) (Vec3 rightX rightY rightZ) =
  sqrt
    ( (leftX - rightX) * (leftX - rightX)
        + (leftY - rightY) * (leftY - rightY)
        + (leftZ - rightZ) * (leftZ - rightZ)
    )
