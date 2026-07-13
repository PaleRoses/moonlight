{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Plan.Shape.Build
  ( mkPlanShape,
    queryPlanToPlanShape,
    queryPlanToOutputErasedPlanShape,

    mkCanonBagShape,
    mkCanonSeparator,

    mkFactorShape,
    ProjectionShapeError (..),
    compileProjectionPayload,
    projectionPayloadFromMorphism,
    compileProjectionShape,
    checkedProjectionShapeFromPayload,
    mkProjectionShape,
    projectedShapeDigest,
    mkRestrictionShape,
    restrictedShapeDigest,
    commutedProjectionRestrictionDigest,
    mkCoverShape,
    mkCoverageTransformShape,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Bifunctor
  ( first,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.Set
  ( Set,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlan,
  )
import Moonlight.Flow.Model.Schema
  ( schemaSlots,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( SchemaProjection,
    SchemaProjectionError,
    compileSchemaProjectionByCanonicalMap,
    spSourceCanonicalSchema,
    spTargetCanonicalSchema,
    spTargetToSource,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonAtomMultiset,
    CanonBagShape (..),
    CanonSeparator (..),
    CanonicalBoundaryShape,
    FactorShapePayload (..),
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonAtomMultisetWords,
    canonicalSlotWords,
    coverPayloadWords,
    coverageTransformPayloadWords,
    factorShapePayloadWords,
    intMapIntSetWords,
    logicalPlanTermWords,
    projectionOperationWords,
    projectionPayloadWords,
    restrictionPayloadWords,
    shapeListWords,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    CoverPayload (..),
    CoverageTransformPayload,
    PlanShape (..),
    PlanShapePayload,
    PlanStage (..),
    ProjectionPayload (..),
    RestrictionPayload (..),
    ResidualShape,
    canonSlotKey,
    queryPlanToLogicalPlanTerm,
    queryPlanToOutputErasedLogicalPlanTerm,
  )

mkPlanShape ::
  PlanShapePayload stage ~ payload =>
  (payload -> [Word64]) ->
  payload ->
  PlanShape stage
mkPlanShape payloadWords payload =
  PlanShape
    { psDigest = stableDigest128 (payloadWords payload),
      psPayload = payload
    }
{-# INLINE mkPlanShape #-}

queryPlanToPlanShape ::
  QueryPlan compiled output guard tag tuple key ->
  PlanShape 'RawLogical
queryPlanToPlanShape =
  mkPlanShape logicalPlanTermWords . queryPlanToLogicalPlanTerm
{-# INLINE queryPlanToPlanShape #-}

queryPlanToOutputErasedPlanShape ::
  QueryPlan compiled output guard tag tuple key ->
  PlanShape 'RawLogical
queryPlanToOutputErasedPlanShape =
  mkPlanShape logicalPlanTermWords . queryPlanToOutputErasedLogicalPlanTerm
{-# INLINE queryPlanToOutputErasedPlanShape #-}

mkCanonBagShape ::
  [CanonSlot] ->
  CanonAtomMultiset ->
  CanonBagShape
mkCanonBagShape slots atoms =
  let digestValue =
        stableDigest128
          ( [0x626167]
              <> shapeListWords canonicalSlotWords slots
              <> canonAtomMultisetWords atoms
          )
   in CanonBagShape
        { cbgSlots = slots,
          cbgAtoms = atoms,
          cbgDigest = digestValue
        }
{-# INLINE mkCanonBagShape #-}

mkCanonSeparator ::
  CanonBagShape ->
  CanonBagShape ->
  [CanonSlot] ->
  CanonSeparator
mkCanonSeparator child parent slots =
  let digestValue =
        stableDigest128
          ( [0x736570]
              <> stableDigestWords (cbgDigest child)
              <> stableDigestWords (cbgDigest parent)
              <> shapeListWords canonicalSlotWords slots
          )
   in CanonSeparator
        { csepChild = child,
          csepParent = parent,
          csepSlots = slots,
          csepDigest = digestValue
        }
{-# INLINE mkCanonSeparator #-}

mkFactorShape ::
  PlanShape 'Canonical ->
  PlanShape 'Fragment ->
  CanonAtomMultiset ->
  [CanonSlot] ->
  [CanonSlot] ->
  Maybe CanonSeparator ->
  CanonicalBoundaryShape ->
  ResidualShape ->
  PlanShape 'FactorShape
mkFactorShape planShape fragment atoms sourceSchema outputSchema maybeSeparator boundaryShapeValue residualShape =
  mkPlanShape
    factorShapePayloadWords
    FactorShapePayload
      { fspPlan = planShape,
        fspFragment = fragment,
        fspAtoms = atoms,
        fspSourceSchema = sourceSchema,
        fspOutputSchema = outputSchema,
        fspSeparator = maybeSeparator,
        fspBoundary = boundaryShapeValue,
        fspResidual = residualShape
      }
{-# INLINE mkFactorShape #-}

data ProjectionShapeError
  = ProjectionShapeSchemaError !(SchemaProjectionError CanonSlot CanonSlot)
  | ProjectionShapeTargetIndexMissing !Int
  | ProjectionShapeSourceIndexMissing !Int
  deriving stock (Eq, Ord, Show)

compileProjectionPayload ::
  StableDigest128 ->
  StableDigest128 ->
  [CanonSlot] ->
  [CanonSlot] ->
  IntMap CanonSlot ->
  Either ProjectionShapeError ProjectionPayload
compileProjectionPayload sourceShape targetShape sourceSchema targetSchema slotMap =
  projectionPayloadFromMorphism sourceShape targetShape
    =<< firstProjectionError
      ( compileSchemaProjectionByCanonicalMap
          canonSlotKey
          slotMap
          sourceSchema
          sourceSchema
          targetSchema
          targetSchema
      )
{-# INLINE compileProjectionPayload #-}

projectionPayloadFromMorphism ::
  StableDigest128 ->
  StableDigest128 ->
  SchemaProjection CanonSlot CanonSlot ->
  Either ProjectionShapeError ProjectionPayload
projectionPayloadFromMorphism sourceShape targetShape projection = do
  slotMap <-
    projectionPayloadSlotMap projection
  pure
    ProjectionPayload
      { ppSourceShape = sourceShape,
        ppTargetShape = targetShape,
        ppSourceSchema = schemaSlots (spSourceCanonicalSchema projection),
        ppTargetSchema = schemaSlots (spTargetCanonicalSchema projection),
        ppSlotMap = slotMap
      }
{-# INLINE projectionPayloadFromMorphism #-}

compileProjectionShape ::
  StableDigest128 ->
  StableDigest128 ->
  [CanonSlot] ->
  [CanonSlot] ->
  IntMap CanonSlot ->
  Either ProjectionShapeError (PlanShape 'Projection)
compileProjectionShape sourceShape targetShape sourceSchema targetSchema slotMap =
  mkPlanShape projectionPayloadWords
    <$> compileProjectionPayload sourceShape targetShape sourceSchema targetSchema slotMap
{-# INLINE compileProjectionShape #-}

checkedProjectionShapeFromPayload ::
  ProjectionPayload ->
  Either ProjectionShapeError (PlanShape 'Projection)
checkedProjectionShapeFromPayload payload =
  compileProjectionShape
    (ppSourceShape payload)
    (ppTargetShape payload)
    (ppSourceSchema payload)
    (ppTargetSchema payload)
    (ppSlotMap payload)
{-# INLINE checkedProjectionShapeFromPayload #-}

mkProjectionShape ::
  StableDigest128 ->
  StableDigest128 ->
  SchemaProjection CanonSlot CanonSlot ->
  Either ProjectionShapeError (PlanShape 'Projection)
mkProjectionShape sourceShape targetShape =
  fmap (mkPlanShape projectionPayloadWords)
    . projectionPayloadFromMorphism sourceShape targetShape
{-# INLINE mkProjectionShape #-}

projectionPayloadSlotMap ::
  SchemaProjection CanonSlot CanonSlot ->
  Either ProjectionShapeError (IntMap CanonSlot)
projectionPayloadSlotMap projection =
  IntMap.fromAscList <$> traverse readMapping (IntMap.toAscList (spTargetToSource projection))
  where
    sourceSlotsByIndex =
      slotsByIndex (schemaSlots (spSourceCanonicalSchema projection))

    targetSlotsByIndex =
      slotsByIndex (schemaSlots (spTargetCanonicalSchema projection))

    readMapping (targetIndex, sourceIndex) = do
      targetSlot <-
        maybe
          (Left (ProjectionShapeTargetIndexMissing targetIndex))
          Right
          (IntMap.lookup targetIndex targetSlotsByIndex)
      sourceSlot <-
        maybe
          (Left (ProjectionShapeSourceIndexMissing sourceIndex))
          Right
          (IntMap.lookup sourceIndex sourceSlotsByIndex)
      pure (canonSlotKey targetSlot, sourceSlot)
{-# INLINE projectionPayloadSlotMap #-}

slotsByIndex :: [CanonSlot] -> IntMap CanonSlot
slotsByIndex =
  IntMap.fromAscList . zip [0 :: Int ..]
{-# INLINE slotsByIndex #-}

firstProjectionError ::
  Either (SchemaProjectionError CanonSlot CanonSlot) value ->
  Either ProjectionShapeError value
firstProjectionError =
  first ProjectionShapeSchemaError
{-# INLINE firstProjectionError #-}

projectedShapeDigest ::
  StableDigest128 ->
  ProjectionPayload ->
  StableDigest128
projectedShapeDigest sourceShape payload =
  stableDigest128
    ( [fromInteger 0x70726f6a5368617065]
        <> stableDigestWords sourceShape
        <> projectionOperationWords payload
    )
{-# INLINE projectedShapeDigest #-}

mkRestrictionShape ::
  StableDigest128 ->
  StableDigest128 ->
  IntMap IntSet ->
  PlanShape 'Restriction
mkRestrictionShape sourceShape targetShape pinnedSlots =
  mkPlanShape
    restrictionPayloadWords
    RestrictionPayload
      { rpSourceShape = sourceShape,
        rpTargetShape = targetShape,
        rpPinnedSlots = pinnedSlots
      }
{-# INLINE mkRestrictionShape #-}

restrictedShapeDigest ::
  StableDigest128 ->
  RestrictionPayload ->
  StableDigest128
restrictedShapeDigest sourceShape payload =
  stableDigest128
    ( [fromInteger 0x726573745368617065]
        <> stableDigestWords sourceShape
        <> intMapIntSetWords (rpPinnedSlots payload)
    )
{-# INLINE restrictedShapeDigest #-}

commutedProjectionRestrictionDigest ::
  ProjectionPayload ->
  RestrictionPayload ->
  StableDigest128
commutedProjectionRestrictionDigest projection restriction =
  stableDigest128
    ( [fromInteger 0x70726f6a52657374436f6d]
        <> stableDigestWords (ppSourceShape projection)
        <> stableDigestWords (ppTargetShape projection)
        <> stableDigestWords (rpSourceShape restriction)
        <> stableDigestWords (rpTargetShape restriction)
        <> projectionOperationWords projection
        <> intMapIntSetWords (rpPinnedSlots restriction)
    )
{-# INLINE commutedProjectionRestrictionDigest #-}

mkCoverShape ::
  StableDigest128 ->
  StableDigest128 ->
  Set StableDigest128 ->
  PlanShape 'Cover
mkCoverShape familyDigest targetShape members =
  mkPlanShape
    coverPayloadWords
    CoverPayload
      { cpFamilyDigest = familyDigest,
        cpTargetShape = targetShape,
        cpMembers = members
      }
{-# INLINE mkCoverShape #-}

mkCoverageTransformShape ::
  CoverageTransformPayload ->
  PlanShape 'CoverageTransform
mkCoverageTransformShape =
  mkPlanShape coverageTransformPayloadWords
{-# INLINE mkCoverageTransformShape #-}
