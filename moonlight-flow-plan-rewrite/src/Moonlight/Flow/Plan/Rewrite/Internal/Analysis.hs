{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Analysis
  ( planAnalysisSpec,
    emptyPlanAnalysis,
    analysisForPlanClass,
    knownShapeDigestInClass,
    addPlanENode,
    addPlanENodeTracked,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    UnionFindAllocationError,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec (..),
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( addENode,
    insertENodeTracked,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphAnalysis,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Reuse.Factor.Signature
  ( PlanFactorRootSignature,
  )
import Moonlight.Flow.Plan.Reuse.Factor.Signature qualified as FactorSignature
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanNode (..),
  )
import Moonlight.Flow.Plan.Shape
  ( CanonBagShape (..),
    CanonSeparator (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CoverPayload (..),
    PlanShape (..),
    PlanStage (..),
    ProjectionPayload (..),
    RestrictionPayload (..),
  )

planAnalysisSpec :: AnalysisSpec PlanNode PlanAnalysis
planAnalysisSpec =
  AnalysisSpec
    { asMake = makePlanAnalysis,
      asJoin = joinPlanAnalysis,
      asJoinChanged = \left right ->
        let joined = joinPlanAnalysis left right
         in (joined, joined /= left)
    }
{-# INLINE planAnalysisSpec #-}

emptyPlanAnalysis :: PlanAnalysis
emptyPlanAnalysis =
  PlanAnalysis
    { paCanonicalCandidate = Nothing,
      paKnownShapeDigests = Set.empty,
      paRootShapeDigests = Set.empty,
      paFactorRootSignatures = Set.empty
    }
{-# INLINE emptyPlanAnalysis #-}

canonicalPlanAnalysis :: PlanShape 'Canonical -> PlanAnalysis
canonicalPlanAnalysis planShape =
  PlanAnalysis
    { paCanonicalCandidate = Just planShape,
      paKnownShapeDigests = Set.singleton digestValue,
      paRootShapeDigests = Set.singleton digestValue,
      paFactorRootSignatures = Set.empty
    }
  where
    digestValue =
      psDigest planShape
{-# INLINE canonicalPlanAnalysis #-}

makePlanAnalysis :: PlanNode PlanAnalysis -> PlanAnalysis
makePlanAnalysis node =
  case node of
    PlanRawLogicalNode _ ->
      emptyPlanAnalysis
    PlanCanonicalNode planShape ->
      canonicalPlanAnalysis planShape
    PlanBagNode bag ->
      rootPlanAnalysis (cbgDigest bag) emptyPlanAnalysis
    PlanSeparatorNode separator ->
      rootPlanAnalysisWithKnown
        (csepDigest separator)
        [cbgDigest (csepChild separator), cbgDigest (csepParent separator)]
        emptyPlanAnalysis
    PlanFactorNode planShape planAnalysis fragmentAnalysis structureAnalysis ->
      addFactorRootSignatureFrom planShape planAnalysis fragmentAnalysis $
        rootPlanAnalysis
          (psDigest planShape)
          (joinPlanAnalysis planAnalysis (joinPlanAnalysis fragmentAnalysis structureAnalysis))
    PlanFragmentNode planShape ->
      rootPlanAnalysis (psDigest planShape) emptyPlanAnalysis
    PlanProjectionNode planShape sourceAnalysis ->
      setFactorRootSignatures
        (FactorSignature.projectFactorRootSignatures (psPayload planShape) (paFactorRootSignatures sourceAnalysis)) $
        rootPlanAnalysisWithKnown
          (psDigest planShape)
          [ppSourceShape (psPayload planShape), ppTargetShape (psPayload planShape)]
          sourceAnalysis
    PlanRestrictionNode planShape sourceAnalysis ->
      setFactorRootSignatures
        (FactorSignature.restrictFactorRootSignatures (psPayload planShape) (paFactorRootSignatures sourceAnalysis)) $
        rootPlanAnalysisWithKnown
          (psDigest planShape)
          [rpSourceShape (psPayload planShape), rpTargetShape (psPayload planShape)]
          sourceAnalysis
    PlanAmalgamationNode planShape memberAnalyses ->
      rootPlanAnalysisWithKnown
        (psDigest planShape)
        (cpTargetShape (psPayload planShape) : Set.toAscList (cpMembers (psPayload planShape)))
        (foldr joinPlanAnalysis emptyPlanAnalysis memberAnalyses)
    PlanCoverageTransformNode planShape sourceAnalysis ->
      rootPlanAnalysis (psDigest planShape) sourceAnalysis
{-# INLINE makePlanAnalysis #-}

rootPlanAnalysis :: StableDigest128 -> PlanAnalysis -> PlanAnalysis
rootPlanAnalysis =
  addRootShapeDigest
{-# INLINE rootPlanAnalysis #-}

rootPlanAnalysisWithKnown :: StableDigest128 -> [StableDigest128] -> PlanAnalysis -> PlanAnalysis
rootPlanAnalysisWithKnown rootDigest knownDigests =
  addRootShapeDigest rootDigest . addKnownShapeDigests knownDigests
{-# INLINE rootPlanAnalysisWithKnown #-}

addFactorRootSignatureFrom ::
  PlanShape 'FactorShape ->
  PlanAnalysis ->
  PlanAnalysis ->
  PlanAnalysis ->
  PlanAnalysis
addFactorRootSignatureFrom planShape planAnalysis fragmentAnalysis analysis =
  case FactorSignature.factorRootSignatureFrom planShape (planAnalysisRootRepresentative planAnalysis) (planAnalysisRootRepresentative fragmentAnalysis) of
    Nothing ->
      analysis
    Just signature ->
      addFactorRootSignature signature analysis
{-# INLINE addFactorRootSignatureFrom #-}

joinPlanAnalysis :: PlanAnalysis -> PlanAnalysis -> PlanAnalysis
joinPlanAnalysis left right =
  PlanAnalysis
    { paCanonicalCandidate =
        minimumCanonicalCandidate
          (paCanonicalCandidate left)
          (paCanonicalCandidate right),
      paKnownShapeDigests =
        Set.union
          (paKnownShapeDigests left)
          (paKnownShapeDigests right),
      paRootShapeDigests =
        Set.union
          (paRootShapeDigests left)
          (paRootShapeDigests right),
      paFactorRootSignatures =
        Set.union
          (paFactorRootSignatures left)
          (paFactorRootSignatures right)
    }
{-# INLINE joinPlanAnalysis #-}

addKnownShapeDigests :: [StableDigest128] -> PlanAnalysis -> PlanAnalysis
addKnownShapeDigests digests analysis =
  analysis {paKnownShapeDigests = Set.union (Set.fromList digests) (paKnownShapeDigests analysis)}
{-# INLINE addKnownShapeDigests #-}

addRootShapeDigest :: StableDigest128 -> PlanAnalysis -> PlanAnalysis
addRootShapeDigest digestValue analysis =
  analysis
    { paRootShapeDigests =
        Set.insert digestValue (paRootShapeDigests analysis),
      paKnownShapeDigests =
        Set.insert digestValue (paKnownShapeDigests analysis)
    }
{-# INLINE addRootShapeDigest #-}

setFactorRootSignatures :: Set PlanFactorRootSignature -> PlanAnalysis -> PlanAnalysis
setFactorRootSignatures signatures analysis =
  analysis {paFactorRootSignatures = signatures}
{-# INLINE setFactorRootSignatures #-}

addFactorRootSignature :: PlanFactorRootSignature -> PlanAnalysis -> PlanAnalysis
addFactorRootSignature signature analysis =
  analysis
    { paFactorRootSignatures =
        Set.insert signature (paFactorRootSignatures analysis)
    }
{-# INLINE addFactorRootSignature #-}

planAnalysisRootRepresentative :: PlanAnalysis -> Maybe StableDigest128
planAnalysisRootRepresentative =
  Set.lookupMin . paRootShapeDigests
{-# INLINE planAnalysisRootRepresentative #-}

minimumCanonicalCandidate :: Maybe (PlanShape 'Canonical) -> Maybe (PlanShape 'Canonical) -> Maybe (PlanShape 'Canonical)
minimumCanonicalCandidate left right =
  case (left, right) of
    (Nothing, Nothing) -> Nothing
    (Just candidate, Nothing) -> Just candidate
    (Nothing, Just candidate) -> Just candidate
    (Just leftCandidate, Just rightCandidate)
      | psDigest leftCandidate <= psDigest rightCandidate -> Just leftCandidate
      | otherwise -> Just rightCandidate
{-# INLINE minimumCanonicalCandidate #-}

addPlanENode ::
  PlanNode ClassId ->
  EGraph PlanNode PlanAnalysis ->
  Either UnionFindAllocationError (ClassId, EGraph PlanNode PlanAnalysis)
addPlanENode node graph =
  addENode (ENode node) (analysisForPlanENode graph node) graph
{-# INLINE addPlanENode #-}

addPlanENodeTracked ::
  PlanNode ClassId ->
  EGraph PlanNode PlanAnalysis ->
  Either UnionFindAllocationError (EGraphMutationResult PlanNode PlanAnalysis ClassId)
addPlanENodeTracked node graph =
  insertENodeTracked (ENode node) (analysisForPlanENode graph node) graph
{-# INLINE addPlanENodeTracked #-}

analysisForPlanENode ::
  EGraph PlanNode PlanAnalysis ->
  PlanNode ClassId ->
  PlanAnalysis
analysisForPlanENode graph node =
  makePlanAnalysis (fmap (analysisForPlanClass graph) node)
{-# INLINE analysisForPlanENode #-}

analysisForPlanClass :: EGraph PlanNode PlanAnalysis -> ClassId -> PlanAnalysis
analysisForPlanClass graph classId =
  IntMap.findWithDefault
    emptyPlanAnalysis
    (classIdKey (canonicalizeClassId graph classId))
    (eGraphAnalysis graph)
{-# INLINE analysisForPlanClass #-}

knownShapeDigestInClass ::
  EGraph PlanNode PlanAnalysis ->
  ClassId ->
  StableDigest128 ->
  Bool
knownShapeDigestInClass graph classId digestValue =
  Set.member
    digestValue
    (paKnownShapeDigests (analysisForPlanClass graph classId))
{-# INLINE knownShapeDigestInClass #-}
