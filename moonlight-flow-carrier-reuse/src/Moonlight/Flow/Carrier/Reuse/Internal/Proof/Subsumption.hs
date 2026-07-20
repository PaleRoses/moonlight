{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Carrier.Reuse.Internal.Proof.Subsumption
  ( ContainmentReuseError (..),
    SemanticExactReuseError (..),
    verifyContainmentReuse,
    verifySemanticEquivalentReuse,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Execution.Subsumption.CQContainment
  ( CQContainmentError,
  )
import Moonlight.Flow.Execution.Subsumption.Containment
  ( ContainmentProofError (..),
    SemanticExactProofError (..),
    compileContainmentProof,
    compileSemanticExactProof,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( BoundaryProjectionProof,
    ContainmentAtomWitness (..),
  )
import Moonlight.Core
  ( SlotId,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    BoundaryProjectionError,
    SchemaProjectionError,
    projectionIsomorphic,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Plan.Rewrite
  ( PlanReuseShapeKey,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualContainmentRejection,
    ResidualImplicationProof (..),
    ResidualTheoryRegistry,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    ResidualShape,
    psDigest,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape (..),
    SubsumptionEntry (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( reuseExactValidityMatchesRequest,
    reuseTemporalViewMatchesRequest,
  )

data ContainmentReuseError ctx prop
  = ContainmentReuseValidityMismatch
  | ContainmentReuseResidualRejected !ResidualShape !ResidualShape !ResidualContainmentRejection
  | ContainmentReuseDomainMismatch !StableDigest128 !StableDigest128
  | ContainmentReuseContextMismatch !ctx !ctx
  | ContainmentReusePropMismatch
  | ContainmentReuseObstructedCoverage !CoverageFact
  | ContainmentReuseCQRejected !CQContainmentError
  | ContainmentReuseSchemaProjectionError !(SchemaProjectionError SlotId CanonSlot)
  | ContainmentReuseBoundaryProjectionError !BoundaryProjectionError
  | ContainmentReuseBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | ContainmentReuseDerivedTargetAddressMismatch
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)
data SemanticExactReuseError ctx prop
  = SemanticExactReuseKeyMismatch !PlanReuseShapeKey !PlanReuseShapeKey
  | SemanticExactReuseValidityMismatch
  | SemanticExactReuseContextMismatch !ctx !ctx
  | SemanticExactReusePropMismatch
  | SemanticExactReuseObstructedCoverage !CoverageFact
  | SemanticExactReuseProofRejected !SemanticExactProofError
  | SemanticExactReuseDerivedTargetAddressMismatch
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)
verifySemanticEquivalentReuse ::
  Eq ctx =>
  Eq prop =>
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  SubsumptionEntry ctx prop ->
  Either (SemanticExactReuseError ctx prop) (ReuseWitness ctx prop)
verifySemanticEquivalentReuse registry request entry = do
  unless (seShapeKey entry == rfsShapeKey request) $
    Left (SemanticExactReuseKeyMismatch (rfsShapeKey request) (seShapeKey entry))
  unless (reuseExactValidityMatchesRequest (rfsValidity request) (seValidity entry)) $
    Left SemanticExactReuseValidityMismatch
  unless (caContext (seCarrier entry) == caContext (rfsTargetCarrier request)) $
    Left
      ( SemanticExactReuseContextMismatch
          (caContext (rfsTargetCarrier request))
          (caContext (seCarrier entry))
      )
  unless (caProp (seCarrier entry) == caProp (rfsTargetCarrier request)) $
    Left SemanticExactReusePropMismatch
  unless (exactCoverageReusable (seCoverageHint entry)) $
    Left (SemanticExactReuseObstructedCoverage (seCoverageHint entry))
  (proof, atomWitness, slotProjection, residualProof, boundaryProof) <-
    first SemanticExactReuseProofRejected $
      compileSemanticExactProof
        registry
        (seShape entry)
        (seBoundary entry)
        (rfsShape request)
        (rfsBoundary request)
  let projection =
        BoundaryProjection slotProjection
      witness0 =
        ReuseWitness
          { rwKind = EquivalentReuse,
            rwWitnessKinds =
              witnessKindsForProof
                EquivalentReuse
                (Just atomWitness)
                residualProof
                boundaryProof
                projection,
            rwSourceCarrier = seCarrier entry,
            rwTargetCarrier = rfsTargetCarrier request,
            rwSourceShape = seShape entry,
            rwTargetShape = rfsShape request,
            rwProjection = projection,
            rwContainmentProof = proof,
            rwAtomProof = Just atomWitness,
            rwResidualProof = residualProof,
            rwBoundaryProof = boundaryProof,
            rwDigest = SubsumptionWitnessDigest (StableDigest128 0 0)
          }
      witness =
        witness0 {rwDigest = subsumptionWitnessDigest witness0}
      expectedDerivedTarget =
        derivedAddrForReuse (rfsTargetCarrier request) witness
  unless (caCarrier expectedDerivedTarget == caCarrier (derivedAddrForReuse (rwTargetCarrier witness) witness)) $
    Left
      ( SemanticExactReuseDerivedTargetAddressMismatch
          expectedDerivedTarget
          (derivedAddrForReuse (rwTargetCarrier witness) witness)
      )
  pure witness
{-# INLINE verifySemanticEquivalentReuse #-}
exactCoverageReusable :: CoverageFact -> Bool
exactCoverageReusable coverage =
  case coverage of
    ExactLocal ->
      True
    ExactRestricted ->
      True
    ExactAmalgamated ->
      True
    LowerBound ->
      False
    Obstructed {} ->
      False
{-# INLINE exactCoverageReusable #-}
verifyContainmentReuse ::
  Eq ctx =>
  Eq prop =>
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  SubsumptionEntry ctx prop ->
  Either (ContainmentReuseError ctx prop) (ReuseWitness ctx prop)
verifyContainmentReuse registry request entry = do
  unless (reuseTemporalViewMatchesRequest (rfsValidity request) (seValidity entry)) $
    Left ContainmentReuseValidityMismatch
  unless (caContext (seCarrier entry) == caContext (rfsTargetCarrier request)) $
    Left (ContainmentReuseContextMismatch (caContext (rfsTargetCarrier request)) (caContext (seCarrier entry)))
  unless (caProp (seCarrier entry) == caProp (rfsTargetCarrier request)) $
    Left ContainmentReusePropMismatch
  unless (containmentCoverageReusable (seCoverageHint entry)) $
    Left (ContainmentReuseObstructedCoverage (seCoverageHint entry))
  (proof, atomWitness, slotProjection, residualProof, boundaryProof) <-
    first liftContainmentProofError $
      compileContainmentProof
        registry
        (seShape entry)
        (seBoundary entry)
        (rfsShape request)
        (rfsBoundary request)
  let reuseKind =
        if psDigest (seShape entry) == psDigest (rfsShape request)
          then EquivalentReuse
          else ContainmentReuse
      projection =
        BoundaryProjection slotProjection
      witness0 =
        ReuseWitness
          { rwKind = reuseKind,
            rwWitnessKinds =
              witnessKindsForProof
                reuseKind
                (Just atomWitness)
                residualProof
                boundaryProof
                projection,
            rwSourceCarrier = seCarrier entry,
            rwTargetCarrier = rfsTargetCarrier request,
            rwSourceShape = seShape entry,
            rwTargetShape = rfsShape request,
            rwProjection = projection,
            rwContainmentProof = proof,
            rwAtomProof = Just atomWitness,
            rwResidualProof = residualProof,
            rwBoundaryProof = boundaryProof,
            rwDigest = SubsumptionWitnessDigest (StableDigest128 0 0)
          }
      witness =
        witness0 {rwDigest = subsumptionWitnessDigest witness0}
      expectedDerivedTarget =
        derivedAddrForReuse (rfsTargetCarrier request) witness
  unless (caCarrier expectedDerivedTarget == caCarrier (derivedAddrForReuse (rwTargetCarrier witness) witness)) $
    Left (ContainmentReuseDerivedTargetAddressMismatch expectedDerivedTarget (derivedAddrForReuse (rwTargetCarrier witness) witness))
  pure witness
{-# INLINE verifyContainmentReuse #-}
witnessKindsForProof ::
  ReuseKind ->
  Maybe ContainmentAtomWitness ->
  ResidualImplicationProof ->
  BoundaryProjectionProof ->
  BoundaryProjection CanonSlot ->
  [ContainmentWitnessKind]
witnessKindsForProof reuseKind atomProof residualProof _boundaryProof projection =
  Set.toAscList
    ( Set.fromList
        ( slotKinds
            <> atomKinds
            <> residualKinds
            <> boundaryKinds
            <> reuseKinds
        )
    )
  where
    slotKinds =
      [ WitnessSlotIsomorphism
        | projectionIsomorphic (bpSchemaProjection projection)
      ]
    atomKinds =
      case atomProof of
        Nothing ->
          []
        Just (StructuralAtomEmbedding _) ->
          [WitnessStructuralAtomEmbedding]
        Just (CQHomomorphism _) ->
          [WitnessCQHomomorphism]
    residualKinds =
      case residualProof of
        ResidualTheoryImplies {} ->
          [WitnessResidualImplication]
        ResidualCrossTheoryImplies {} ->
          [WitnessResidualImplication]
        ResidualEqualDigest {} ->
          []
        ResidualBothNone ->
          []
    boundaryKinds =
      [WitnessBoundaryProjection]
    reuseKinds =
      case reuseKind of
        EquivalentReuse ->
          []
        ContainmentReuse ->
          []
        ExactByCoverReuse ->
          [WitnessExactCover]
{-# INLINE witnessKindsForProof #-}
liftContainmentProofError ::
  ContainmentProofError ->
  ContainmentReuseError ctx prop
liftContainmentProofError proofError =
  case proofError of
    ContainmentProofResidualRejected source requested rejection ->
      ContainmentReuseResidualRejected source requested rejection
    ContainmentProofDomainMismatch source requested ->
      ContainmentReuseDomainMismatch source requested
    ContainmentProofCQRejected err ->
      ContainmentReuseCQRejected err
    ContainmentProofSchemaProjectionError err ->
      ContainmentReuseSchemaProjectionError err
    ContainmentProofBoundaryProjectionError err ->
      ContainmentReuseBoundaryProjectionError err
    ContainmentProofBoundaryMismatch expected actual ->
      ContainmentReuseBoundaryMismatch expected actual
{-# INLINE liftContainmentProofError #-}
