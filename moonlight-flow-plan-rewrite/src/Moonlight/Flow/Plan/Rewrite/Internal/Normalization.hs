{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Normalization
  ( normalizeFactorShapeWithState,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Fix
  ( Fix,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Plan.Reuse.Factor.Signature qualified as FactorSignature
import Moonlight.Flow.Plan.Rewrite.Internal.Analysis
  ( analysisForPlanClass,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Saturation
  ( rewritePlanSaturationState,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.State
  ( canonicalizePlanClass,
    insertPlanTerm,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( FactorShapeNormalization (..),
    FactorShapeNormalizationProof (..),
    PlanAnalysis (..),
    PlanReuseShapeKey (..),
    PlanSaturationError (..),
    PlanSaturationState (..),
    fsnpKey,
    planReuseShapeKeyWords,
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode,
    factorPlanTerm,
    projectionPlanTerm,
    restrictionPlanTerm,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEquivalenceStep,
    PlanRewriteSystem (..),
    classWord,
    equivalenceStepWords,
    mkPlanRewriteSystem,
    PlanEqualityLaw (..),
  )
import Moonlight.Flow.Plan.Shape
  ( FactorShapePayload (..),
    cbsSensitiveSlots,
  )
import Moonlight.Flow.Plan.Shape.Boundary.Canonical qualified as Boundary
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (..),
    RestrictionPayload (..),
  )
import Moonlight.Flow.Plan.Rewrite.Transform.ProjectionRestriction
  ( identitySlotMap,
  )

normalizeFactorShapeWithState ::
  SaturationBudget ->
  PlanSaturationState ->
  PlanShape 'FactorShape ->
  Either PlanSaturationError (PlanSaturationState, FactorShapeNormalization)
normalizeFactorShapeWithState rewriteBudget state0 shape = do
  let rewriteSystem =
        factorReuseNormalizationPlanRewriteSystem
  normalizationTerm <-
    factorReuseNormalizationTerm shape
  (factorClass0, state1) <-
    insertPlanTerm normalizationTerm state0

  (state2, rewriteSteps) <-
    rewritePlanSaturationState rewriteBudget rewriteSystem state1

  let factorClass1 =
        canonicalizePlanClass state2 factorClass0

  normalization <-
    factorShapeNormalizationForClass
      rewriteSystem
      shape
      factorClass1
      rewriteSteps
      state2

  pure (state2, normalization)

factorReuseNormalizationPlanRewriteSystem :: PlanRewriteSystem
factorReuseNormalizationPlanRewriteSystem =
  mkPlanRewriteSystem
    ( Set.fromList
        [ LawProjectionId,
          LawProjectionCompose,
          LawRestrictionId,
          LawRestrictionCompose
        ]
    )

factorReuseNormalizationTerm :: PlanShape 'FactorShape -> Either PlanSaturationError (Fix PlanNode)
factorReuseNormalizationTerm shape = do
  projectionPayload0 <-
    first PlanSaturationProjectionShapeError $
      ShapeBuild.compileProjectionPayload
        restrictionTargetDigest
        restrictionTargetDigest
        (fspSourceSchema payload)
        (fspOutputSchema payload)
        projectionSlotMap
  let projectionTargetDigest =
        if fspSourceSchema payload == fspOutputSchema payload
          then restrictionTargetDigest
          else
            ShapeBuild.projectedShapeDigest
              restrictionTargetDigest
              projectionPayload0
  projectionShape <-
    first PlanSaturationProjectionShapeError $
      ShapeBuild.compileProjectionShape
        restrictionTargetDigest
        projectionTargetDigest
        (fspSourceSchema payload)
        (fspOutputSchema payload)
        projectionSlotMap
  pure
    ( projectionPlanTerm projectionShape $
        restrictionPlanTerm restrictionShape $
          factorPlanTerm baseShape
    )
  where
    payload =
      psPayload shape

    sourceBoundary =
      Boundary.mkCanonicalBoundaryShape
        (fspSourceSchema payload)
        (cbsSensitiveSlots (fspBoundary payload))
        Map.empty

    baseShape =
      ShapeBuild.mkFactorShape
        (fspPlan payload)
        (fspFragment payload)
        (fspAtoms payload)
        (fspSourceSchema payload)
        (fspSourceSchema payload)
        (fspSeparator payload)
        sourceBoundary
        (fspResidual payload)

    pinnedSlots =
      Boundary.canonicalBoundaryPinnedSlots (fspBoundary payload)

    restrictionTargetDigest =
      if IntMap.null pinnedSlots
        then psDigest baseShape
        else
          ShapeBuild.restrictedShapeDigest
            (psDigest baseShape)
            restrictionPayload0

    restrictionShape =
      ShapeBuild.mkRestrictionShape
        (psDigest baseShape)
        restrictionTargetDigest
        pinnedSlots

    restrictionPayload0 =
      RestrictionPayload
        { rpSourceShape = psDigest baseShape,
          rpTargetShape = psDigest baseShape,
          rpPinnedSlots = pinnedSlots
        }

    projectionSlotMap =
      identitySlotMap (fspOutputSchema payload)

factorShapeNormalizationForClass ::
  PlanRewriteSystem ->
  PlanShape 'FactorShape ->
  PlanClassId (PlanShape 'FactorShape) ->
  [PlanEquivalenceStep] ->
  PlanSaturationState ->
  Either PlanSaturationError FactorShapeNormalization
factorShapeNormalizationForClass rewriteSystem shape factorClass steps state =
  case Set.lookupMin (paFactorRootSignatures analysis) of
    Nothing ->
      Left (PlanSaturationNoFactorRepresentative factorClass)

    Just signature ->
      let representativeDigest =
            FactorSignature.pfrsDigest signature
          key =
            PlanReuseShapeKey
              { prskRewriteSystemDigest = prsDigest rewriteSystem,
                prskRepresentativeDigest = representativeDigest
              }
          proof =
            factorShapeNormalizationProof
              key
              (psDigest shape)
              factorClass
              steps
       in Right
            FactorShapeNormalization
              { fsnProof = proof
              }
  where
    analysis =
      analysisForPlanClass
        (pssGraph state)
        (unPlanClassId factorClass)

factorShapeNormalizationProof ::
  PlanReuseShapeKey ->
  StableDigest128 ->
  PlanClassId (PlanShape 'FactorShape) ->
  [PlanEquivalenceStep] ->
  FactorShapeNormalizationProof
factorShapeNormalizationProof key sourceDigest factorClass steps =
  proof0
    { fsnpDigest =
        stableDigest128
          ( [0x666163746f72, 0x4e6f726d50726f6f, 0x66]
              <> stableDigestWords (fsnpSourceDigest proof0)
              <> planReuseShapeKeyWords (fsnpKey proof0)
              <> [classWord (fsnpClassId proof0)]
              <> stableDigestWords (fsnpStepDigest proof0)
          )
    }
  where
    stepDigest =
      stableDigest128
        ( [0x666163746f72, 0x4e6f726d53746570, 0x73]
            <> foldMap equivalenceStepWords steps
        )

    proof0 =
      FactorShapeNormalizationProof
        { fsnpSourceDigest = sourceDigest,
          fsnpKey = key,
          fsnpClassId = factorClass,
          fsnpStepDigest = stepDigest,
          fsnpDigest = StableDigest128 0 0
        }
