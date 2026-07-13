{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Subsumption.Containment
  ( AtomEmbedding (..),
    AtomEmbeddingError (..),
    ResidualImplicationProof (..),
    ContainmentProof (..),
    ContainmentProofError (..),
    SemanticExactProofError (..),

    compileAtomEmbedding,
    compileContainmentProof,
    compileSemanticExactProof,
    containmentAtomWitnessWords,
  )
where

import Control.Monad
  ( foldM,
    unless,
  )
import Data.Bifunctor
  ( first,
  )
import Moonlight.Flow.Execution.Subsumption.CQContainment
  ( CQContainmentError,
    compileCQContainment,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( AtomEmbedding (..),
    AtomEmbeddingError (..),
    BoundaryProjectionProof (..),
    CQContainmentWitness (..),
    ContainmentAtomWitness (..),
    ContainmentProof (..),
    ResidualImplicationProof (..),
  )

import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    boundaryDigest,
    BoundaryShape (..),
    boundaryShape,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    BoundaryProjectionProfile (bppBoundaryExact),
    BoundaryProjectionError,
    SchemaProjection,
    SchemaProjectionError,
    compileSchemaProjectionByCanonicalSchema,
    compileSchemaProjectionByCanonicalMap,
    projectRelationalBoundary,
    projectRelationalBoundaryWithProfile,
    projectionProfile,
    ProjectionProfile (..),
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonicalSlotWords,
    canonAtomMultisetWords,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Shape
  ( CanonAtom,
    CanonAtomMultiset,
    LogicalQueryShape (..),
    factorShapeAtoms,
    factorShapeLogical,
    factorShapeOutputSchema,
    factorShapePlanDigest,
    factorShapeResidual,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    PlanShape,
    PlanStage (..),
    canonSlotKey,
    psDigest,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualContainmentProof (..),
    ResidualContainmentRejection (..),
    ResidualShape,
    ResidualTheoryRegistry,
    residualContainmentProof,
    residualImplicationProofWords,
  )

data ContainmentProofError
  = ContainmentProofResidualRejected
      !ResidualShape
      !ResidualShape
      !ResidualContainmentRejection
  | ContainmentProofDomainMismatch !StableDigest128 !StableDigest128
  | ContainmentProofCQRejected !CQContainmentError
  | ContainmentProofSchemaProjectionError !(SchemaProjectionError SlotId CanonSlot)
  | ContainmentProofBoundaryProjectionError !BoundaryProjectionError
  | ContainmentProofBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  deriving stock (Eq, Show)

data SemanticExactProofError
  = SemanticExactProofResidualMismatch !ResidualShape !ResidualShape
  | SemanticExactProofResidualRejected
      !ResidualShape
      !ResidualShape
      !ResidualContainmentRejection
  | SemanticExactProofDomainMismatch !StableDigest128 !StableDigest128
  | SemanticExactProofAtomMismatch !CanonAtomMultiset !CanonAtomMultiset
  | SemanticExactProofAtomEmbeddingError !AtomEmbeddingError
  | SemanticExactProofSchemaProjectionError !(SchemaProjectionError SlotId CanonSlot)
  | SemanticExactProofBoundaryProjectionError !BoundaryProjectionError
  | SemanticExactProofBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | SemanticExactProofBoundaryNotExact !RuntimeBoundary !RuntimeBoundary
  deriving stock (Eq, Show)

compileAtomEmbedding ::
  CanonAtomMultiset ->
  CanonAtomMultiset ->
  Either AtomEmbeddingError AtomEmbedding
compileAtomEmbedding sourceAtoms targetAtoms = do
  remainder <-
    foldM subtractTargetAtom sourceAtoms (Map.toAscList targetAtoms)
  let normalizedRemainder =
        Map.filter (> 0) remainder
      digestValue =
        stableDigest128
          ( [0x61746f6d456d6265]
              <> canonAtomMultisetWords targetAtoms
              <> canonAtomMultisetWords normalizedRemainder
          )
  pure
    AtomEmbedding
      { aeRequiredAtoms = targetAtoms,
        aeSourceRemainder = normalizedRemainder,
        aeDigest = digestValue
      }
{-# INLINE compileAtomEmbedding #-}

subtractTargetAtom ::
  CanonAtomMultiset ->
  (CanonAtom, Int) ->
  Either AtomEmbeddingError CanonAtomMultiset
subtractTargetAtom source (atomValue, targetMultiplicity) =
  if targetMultiplicity <= 0
    then Left (AtomEmbeddingNonPositiveTargetMultiplicity atomValue targetMultiplicity)
    else
      case Map.lookup atomValue source of
        Nothing ->
          Left (AtomEmbeddingMissingSourceMultiplicity atomValue 0 targetMultiplicity)
        Just sourceMultiplicity
          | sourceMultiplicity < 0 ->
              Left (AtomEmbeddingNegativeSourceMultiplicity atomValue sourceMultiplicity)
          | sourceMultiplicity < targetMultiplicity ->
              Left
                ( AtomEmbeddingMissingSourceMultiplicity
                    atomValue
                    sourceMultiplicity
                    targetMultiplicity
                )
          | sourceMultiplicity == targetMultiplicity ->
              Right (Map.delete atomValue source)
          | otherwise ->
              Right (Map.insert atomValue (sourceMultiplicity - targetMultiplicity) source)
{-# INLINE subtractTargetAtom #-}

compileContainmentProof ::
  ResidualTheoryRegistry ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  Either
    ContainmentProofError
    ( ContainmentProof,
      ContainmentAtomWitness,
      SchemaProjection SlotId CanonSlot,
      ResidualImplicationProof,
      BoundaryProjectionProof
    )
compileContainmentProof residualRegistry sourceShape sourceBoundary requestedShape requestedBoundary = do
  residualProof <-
    case
      residualContainmentProof
        residualRegistry
        (factorShapeResidual sourceShape)
        (factorShapeResidual requestedShape)
    of
      ResidualContainmentAccepted proof ->
        Right proof

      ResidualContainmentRejected rejection ->
        Left
          ( ContainmentProofResidualRejected
              (factorShapeResidual sourceShape)
              (factorShapeResidual requestedShape)
              rejection
          )

  unless (samePlanDomain sourceShape requestedShape) $
    Left
      ( ContainmentProofDomainMismatch
        (factorShapePlanDigest sourceShape)
        (factorShapePlanDigest requestedShape)
      )

  (atomWitness, slotProjection) <-
    compileContainmentShapeProof
      sourceShape
      sourceBoundary
      requestedShape
      requestedBoundary

  let projectionDigest =
        ppDigest (projectionProfile canonSlotKey canonicalSlotWords slotProjection)

  projectedBoundary <-
    first ContainmentProofBoundaryProjectionError $
      projectRelationalBoundary
        (BoundaryProjection slotProjection)
        sourceBoundary

  unless (projectedBoundary == requestedBoundary) $
    Left (ContainmentProofBoundaryMismatch requestedBoundary projectedBoundary)

  let boundaryProof =
        boundaryProjectionProof
          sourceBoundary
          requestedBoundary
          projectionDigest
      proofDigest =
        containmentProofDigest
          sourceShape
          requestedShape
          atomWitness
          slotProjection
          residualProof
          boundaryProof
      proof =
        ContainmentProof
          { cpSourceShape = sourceShape,
            cpRequestedShape = requestedShape,
            cpSlotProjection = slotProjection,
            cpAtomEmbedding = atomWitness,
            cpResidualProof = residualProof,
            cpBoundaryProof = boundaryProof,
            cpProjectionDigest = proofDigest
          }

  pure (proof, atomWitness, slotProjection, residualProof, boundaryProof)
{-# INLINE compileContainmentProof #-}

compileSemanticExactProof ::
  ResidualTheoryRegistry ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  Either
    SemanticExactProofError
    ( ContainmentProof,
      ContainmentAtomWitness,
      SchemaProjection SlotId CanonSlot,
      ResidualImplicationProof,
      BoundaryProjectionProof
    )
compileSemanticExactProof residualRegistry sourceShape sourceBoundary requestedShape requestedBoundary = do
  unless (factorShapeResidual sourceShape == factorShapeResidual requestedShape) $
    Left
      ( SemanticExactProofResidualMismatch
        (factorShapeResidual sourceShape)
        (factorShapeResidual requestedShape)
      )

  residualProof <-
    case
      residualContainmentProof
        residualRegistry
        (factorShapeResidual sourceShape)
        (factorShapeResidual requestedShape)
    of
      ResidualContainmentAccepted proof ->
        Right proof

      ResidualContainmentRejected rejection ->
        Left
          ( SemanticExactProofResidualRejected
              (factorShapeResidual sourceShape)
              (factorShapeResidual requestedShape)
              rejection
          )

  unless (samePlanDomain sourceShape requestedShape) $
    Left
      ( SemanticExactProofDomainMismatch
        (factorShapePlanDigest sourceShape)
        (factorShapePlanDigest requestedShape)
      )

  unless (factorShapeAtoms sourceShape == factorShapeAtoms requestedShape) $
    Left
      ( SemanticExactProofAtomMismatch
        (factorShapeAtoms sourceShape)
        (factorShapeAtoms requestedShape)
      )

  atomEmbedding <-
    first SemanticExactProofAtomEmbeddingError $
      compileAtomEmbedding
        (factorShapeAtoms sourceShape)
        (factorShapeAtoms requestedShape)

  slotProjection <-
    first SemanticExactProofSchemaProjectionError $
      compileSchemaProjectionByCanonicalSchema
        canonSlotKey
        (factorShapeOutputSchema sourceShape)
        (boundarySchema sourceBoundary)
        (factorShapeOutputSchema requestedShape)
        (boundarySchema requestedBoundary)

  (projectedBoundary, boundaryProfile) <-
    first SemanticExactProofBoundaryProjectionError $
      projectRelationalBoundaryWithProfile
        (BoundaryProjection slotProjection)
        sourceBoundary

  unless (projectedBoundary == requestedBoundary) $
    Left (SemanticExactProofBoundaryMismatch requestedBoundary projectedBoundary)

  unless (bppBoundaryExact boundaryProfile) $
    Left (SemanticExactProofBoundaryNotExact sourceBoundary projectedBoundary)

  let atomWitness =
        StructuralAtomEmbedding atomEmbedding
      projectionDigest =
        ppDigest (projectionProfile canonSlotKey canonicalSlotWords slotProjection)
      boundaryProof =
        boundaryProjectionProof
          sourceBoundary
          requestedBoundary
          projectionDigest
      proofDigest =
        containmentProofDigest
          sourceShape
          requestedShape
          atomWitness
          slotProjection
          residualProof
          boundaryProof
      proof =
        ContainmentProof
          { cpSourceShape = sourceShape,
            cpRequestedShape = requestedShape,
            cpSlotProjection = slotProjection,
            cpAtomEmbedding = atomWitness,
            cpResidualProof = residualProof,
            cpBoundaryProof = boundaryProof,
            cpProjectionDigest = proofDigest
          }

  pure (proof, atomWitness, slotProjection, residualProof, boundaryProof)
{-# INLINE compileSemanticExactProof #-}

boundaryProjectionProof ::
  RuntimeBoundary ->
  RuntimeBoundary ->
  StableDigest128 ->
  BoundaryProjectionProof
boundaryProjectionProof sourceBoundary requestedBoundary projectionDigest =
  let sourceDigest =
        stableDigest128
          ( [fromInteger (0x626f756e64536f75726365 :: Integer)]
              <> stableDigestWords (boundaryDigest sourceBoundary)
          )
      requestedDigest =
        stableDigest128
          ( [0x626f756e64526571]
              <> stableDigestWords (boundaryDigest requestedBoundary)
          )
      exact =
        sourceBoundary == requestedBoundary
      proof0 =
        BoundaryProjectionProof
          { bppSourceBoundaryDigest = sourceDigest,
            bppRequestedBoundaryDigest = requestedDigest,
            bppProjectionDigest = projectionDigest,
            bppExact = exact,
            bppDigest = projectionDigest
          }
   in proof0
        { bppDigest =
            stableDigest128
              ( [fromInteger (0x626f756e6450726f6f66 :: Integer)]
                  <> stableDigestWords sourceDigest
                  <> stableDigestWords requestedDigest
                  <> stableDigestWords projectionDigest
                  <> [if exact then 1 else 0]
              )
        }
{-# INLINE boundaryProjectionProof #-}

samePlanDomain ::
  PlanShape 'FactorShape ->
  PlanShape 'FactorShape ->
  Bool
samePlanDomain sourceShape targetShape =
  lqsDomain (factorShapeLogical sourceShape)
    == lqsDomain (factorShapeLogical targetShape)
{-# INLINE samePlanDomain #-}

samePlanClass ::
  PlanShape 'FactorShape ->
  PlanShape 'FactorShape ->
  Bool
samePlanClass sourceShape targetShape =
  factorShapePlanDigest sourceShape
    == factorShapePlanDigest targetShape
{-# INLINE samePlanClass #-}

compileContainmentShapeProof ::
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  Either
    ContainmentProofError
    (ContainmentAtomWitness, SchemaProjection SlotId CanonSlot)
compileContainmentShapeProof sourceShape sourceBoundary requestedShape requestedBoundary =
  case structuralShapeProof sourceShape sourceBoundary requestedShape requestedBoundary of
    Just proof ->
      Right proof
    Nothing ->
      homomorphicShapeProof sourceShape sourceBoundary requestedShape requestedBoundary
{-# INLINE compileContainmentShapeProof #-}

structuralShapeProof ::
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  Maybe (ContainmentAtomWitness, SchemaProjection SlotId CanonSlot)
structuralShapeProof sourceShape sourceBoundary requestedShape requestedBoundary
  | not (samePlanClass sourceShape requestedShape) =
      Nothing
  | otherwise =
      case compileAtomEmbedding
        (factorShapeAtoms sourceShape)
        (factorShapeAtoms requestedShape) of
        Left _ ->
          Nothing
        Right atomEmbedding ->
          case compileSchemaProjectionByCanonicalSchema
            canonSlotKey
            (factorShapeOutputSchema sourceShape)
            (boundarySchema sourceBoundary)
            (factorShapeOutputSchema requestedShape)
            (boundarySchema requestedBoundary) of
            Left _ ->
              Nothing
            Right slotProjection ->
              Just
                ( StructuralAtomEmbedding atomEmbedding,
                  slotProjection
                )
{-# INLINE structuralShapeProof #-}

homomorphicShapeProof ::
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  Either
    ContainmentProofError
    (ContainmentAtomWitness, SchemaProjection SlotId CanonSlot)
homomorphicShapeProof sourceShape sourceBoundary requestedShape requestedBoundary = do
  cqWitness <-
    first ContainmentProofCQRejected $
      compileCQContainment
        (factorShapeAtoms sourceShape)
        (factorShapeAtoms requestedShape)
        (factorShapeOutputSchema sourceShape)
        (factorShapeOutputSchema requestedShape)

  slotProjection <-
    first ContainmentProofSchemaProjectionError $
      compileSchemaProjectionByCanonicalMap
        canonSlotKey
        (cqwSlotMap cqWitness)
        (factorShapeOutputSchema sourceShape)
        (boundarySchema sourceBoundary)
        (factorShapeOutputSchema requestedShape)
        (boundarySchema requestedBoundary)

  pure
    ( CQHomomorphism cqWitness,
      slotProjection
    )
{-# INLINE homomorphicShapeProof #-}

containmentProofDigest ::
  PlanShape 'FactorShape ->
  PlanShape 'FactorShape ->
  ContainmentAtomWitness ->
  SchemaProjection SlotId CanonSlot ->
  ResidualImplicationProof ->
  BoundaryProjectionProof ->
  StableDigest128
containmentProofDigest sourceShape requestedShape atomWitness slotProjection residualProof boundaryProof =
  stableDigest128
    ( [0x636f6e7461696e]
        <> stableDigestWords (psDigest sourceShape)
        <> stableDigestWords (psDigest requestedShape)
        <> containmentAtomWitnessWords atomWitness
        <> stableDigestWords (ppDigest (projectionProfile canonSlotKey canonicalSlotWords slotProjection))
        <> residualImplicationProofWords residualProof
        <> stableDigestWords (bppDigest boundaryProof)
    )
{-# INLINE containmentProofDigest #-}

containmentAtomWitnessWords ::
  ContainmentAtomWitness ->
  [Word64]
containmentAtomWitnessWords atomWitness =
  case atomWitness of
    StructuralAtomEmbedding atomEmbedding ->
      [0x01] <> stableDigestWords (aeDigest atomEmbedding)
    CQHomomorphism cqWitness ->
      [0x02] <> stableDigestWords (cqwDigest cqWitness)
{-# INLINE containmentAtomWitnessWords #-}


boundarySchema ::
  RuntimeBoundary ->
  [SlotId]
boundarySchema =
  bsSchema . boundaryShape
{-# INLINE boundarySchema #-}
