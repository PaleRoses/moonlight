{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module Moonlight.EGraph.Saturation.GoldenPathSpec.Fixture
  ( module Moonlight.EGraph.Saturation.CohomologicalSpec.Fixture.Section,
    pairIdentityRule,
    commutePairRule,
    goldenCohomologicalSaturationConfig,
    goldenGenericJoinSaturationConfig,
    goldenProofBuilder,
    goldenSaturationBudget,
    singleRootContext,
    obstructingContext,
    manyPairContext,
    renderTestFix,
    testDepthCost
  )
where

import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.System
  ( RawRewriteRule (..)
  )
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder, defaultProofAnnotationBuilder)
import Moonlight.EGraph.Test.Saturation
  ( SaturationBudget (..),
    EGraphSaturationConfig,
    SaturationConfig (..),
    genericJoinSaturationConfig,
    scMatchingStrategy
  )
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingRequest)
import Moonlight.EGraph.Pure.Types (ClassId, RewriteRuleId (..), classIdKey)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality
import Moonlight.EGraph.Saturation.Cohomological.Types
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (crRoot),
    CandidateStalk (..),
    OccurrenceId (..),
    emptyCapabilityLabelAlgebra,
    emptyTypedCapabilityEnvironment,
    mkSectionCertificationAlgebraWithCachePolicy,
    regionCarrierPlanFromList
  )
import Moonlight.EGraph.Saturation.CohomologicalSpec.Fixture.Section
import Data.Fix (Fix (..))
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Saturation.Matching qualified as GenericMatching

goldenProofBuilder :: ProofAnnotationBuilder TestScope ()
goldenProofBuilder =
  defaultProofAnnotationBuilder

goldenSaturationBudget :: SaturationBudget
goldenSaturationBudget =
  SaturationBudget
    { sbMaxIterations = 30,
      sbMaxNodes = 10000
    }

goldenGenericJoinSaturationConfig :: EGraphSaturationConfig owner ScopeCtx TestF () TestScope
goldenGenericJoinSaturationConfig =
  genericJoinSaturationConfig goldenSaturationBudget

goldenCohomologicalSaturationConfig :: CohomologicalBackend owner TestScope TestF -> EGraphSaturationConfig owner ScopeCtx TestF () TestScope
goldenCohomologicalSaturationConfig backend =
  goldenGenericJoinSaturationConfig
    { scMatchingStrategy = cohomologicalMatchingStrategy backend
    }

pairIdentityRule :: RawRewriteRule (RewriteCondition ScopeCtx TestF) TestF
pairIdentityRule =
  RawRewriteRule
    { rrId = RewriteRuleId 0,
      rrLhs = PatternNode (Pair (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Lit 0))),
      rrRhs = PatternVar (EGraph.mkPatternVar 0),
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

commutePairRule :: RawRewriteRule (RewriteCondition ScopeCtx TestF) TestF
commutePairRule =
  RawRewriteRule
    { rrId = RewriteRuleId 1,
      rrLhs = PatternNode (Pair (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))),
      rrRhs = PatternNode (Pair (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 0))),
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

patternVariableFor :: Pattern f -> Maybe EGraph.PatternVar
patternVariableFor patternValue =
  case patternValue of
    PatternVar patternVar -> Just patternVar
    _ -> Nothing

patternOccurrenceAt :: OccurrenceId -> [Int] -> Pattern TestF -> PatternOccurrence TestF
patternOccurrenceAt occurrenceId occurrencePath patternValue =
  PatternOccurrence
    { poId = occurrenceId,
      poPath = occurrencePath,
      poPattern = patternValue,
      poBoundVariable = patternVariableFor patternValue
    }

singleRootContext ::
  ClassId ->
  ClassId ->
  ClassId ->
  EGraphSectionCertification owner c TestF
singleRootContext pairRoot leftChild rightChild =
  mkSectionCertificationAlgebraWithCachePolicy
    (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
    ( \patternValue ->
        case patternValue of
          PatternNode (Pair leftPattern rightPattern) ->
            [ patternOccurrenceAt (OccurrenceId 0) [0] leftPattern,
              patternOccurrenceAt (OccurrenceId 1) [1] rightPattern
            ]
          _ ->
            [patternOccurrenceAt (OccurrenceId 0) [] patternValue]
    )
    ( \_ _ ->
        regionCarrierPlanFromList [mkFineRegion pairRoot 801]
    )
    (\_ _ _ -> [])
    ( \_ occurrenceValue _ ->
        case unOccurrenceId (poId occurrenceValue) of
          0 -> CandidateStalk (IntSet.singleton (classIdKey leftChild))
          1 -> CandidateStalk (IntSet.singleton (classIdKey rightChild))
          _ -> CandidateStalk IntSet.empty
    )
    ( \_ _ _ ->
        CandidateStalk
          (IntSet.fromList [classIdKey pairRoot, classIdKey leftChild, classIdKey rightChild])
    )
    (const 41)
    (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)

obstructingContext ::
  ClassId ->
  (ClassId, ClassId) ->
  ClassId ->
  (ClassId, ClassId) ->
  EGraphSectionCertification owner c TestF
obstructingContext obstructedRoot (obstructedLeft, obstructedRight) allowedRoot (allowedLeft, allowedRight) =
  let occurrenceDomain occurrenceId rootClass =
        if rootClass == obstructedRoot
          then CandidateStalk IntSet.empty
          else
            case unOccurrenceId occurrenceId of
              0 -> CandidateStalk (IntSet.singleton (classIdKey allowedLeft))
              1 -> CandidateStalk (IntSet.singleton (classIdKey allowedRight))
              _ -> CandidateStalk IntSet.empty
      allClassKeys =
        IntSet.fromList
          [ classIdKey obstructedRoot,
            classIdKey obstructedLeft,
            classIdKey obstructedRight,
            classIdKey allowedRoot,
            classIdKey allowedLeft,
            classIdKey allowedRight
          ]
   in mkSectionCertificationAlgebraWithCachePolicy
        (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
        ( \patternValue ->
            case patternValue of
              PatternNode (Pair leftPattern rightPattern) ->
                [ patternOccurrenceAt (OccurrenceId 0) [0] leftPattern,
                  patternOccurrenceAt (OccurrenceId 1) [1] rightPattern
                ]
              _ ->
                [patternOccurrenceAt (OccurrenceId 0) [] patternValue]
        )
        ( \_ _ ->
            regionCarrierPlanFromList
              [ mkFineRegion obstructedRoot 901,
                mkFineRegion allowedRoot 902
              ]
        )
        (\_ _ _ -> [])
        ( \_ occurrenceValue regionValue ->
            occurrenceDomain (poId occurrenceValue) (crRoot regionValue)
        )
        (\_ _ _ -> CandidateStalk allClassKeys)
        (const 43)
        (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)

manyPairContext ::
  [(ClassId, (ClassId, ClassId))] ->
  EGraphSectionCertification owner c TestF
manyPairContext pairAssignmentsList =
  let pairAssignments =
        Map.fromList pairAssignmentsList
      occurrenceDomain occurrenceId rootClass =
        maybe
          (CandidateStalk IntSet.empty)
          (\(leftChild, rightChild) ->
             case unOccurrenceId occurrenceId of
               0 -> CandidateStalk (IntSet.singleton (classIdKey leftChild))
               1 -> CandidateStalk (IntSet.singleton (classIdKey rightChild))
               _ -> CandidateStalk IntSet.empty
          )
          (Map.lookup rootClass pairAssignments)
      allClassKeys =
        IntSet.fromList
          ( foldMap
              (\(rootClass, (leftChild, rightChild)) ->
                 [classIdKey rootClass, classIdKey leftChild, classIdKey rightChild]
              )
              (Map.toList pairAssignments)
          )
   in mkSectionCertificationAlgebraWithCachePolicy
        (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
        ( \patternValue ->
            case patternValue of
              PatternNode (Pair leftPattern rightPattern) ->
                [ patternOccurrenceAt (OccurrenceId 0) [0] leftPattern,
                  patternOccurrenceAt (OccurrenceId 1) [1] rightPattern
                ]
              _ ->
                [patternOccurrenceAt (OccurrenceId 0) [] patternValue]
        )
        (\_ _ -> regionCarrierPlanFromList (fmap (\rootClass -> mkFineRegion rootClass (classIdKey rootClass)) (Map.keys pairAssignments)))
        (\_ _ _ -> [])
        (\_ occurrenceValue regionValue -> occurrenceDomain (poId occurrenceValue) (crRoot regionValue))
        (\_ _ _ -> CandidateStalk allClassKeys)
        (const 47)
        (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)

renderTestFix :: Fix TestF -> String
renderTestFix term =
  case term of
    Fix (Lit number) -> show number
    Fix (Pair leftTerm rightTerm) ->
      "(" <> renderTestFix leftTerm <> "," <> renderTestFix rightTerm <> ")"

testDepthCost :: CostAlgebra TestF Int
testDepthCost =
  CostAlgebra
    ( \testNode ->
        case testNode of
          Lit _ -> 0
          Pair leftCost rightCost -> 1 + max leftCost rightCost
    )
