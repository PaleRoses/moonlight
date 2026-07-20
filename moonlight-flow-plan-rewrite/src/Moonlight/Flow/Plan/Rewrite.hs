{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite
  ( PlanClassId (..),
    PlanNode (..),
    PlanAnalysis (..),
    PlanSaturationState (..),
    PlanSaturationError (..),
    SaturationBudget (..),

    PlanRewriteSystem (..),
    PlanEqualityLaw (..),
    PlanLawProof (..),
    PlanEquivalenceStep (..),
    PlanEquivalenceProof (..),
    PlanEGraphResult (..),
    PlanReuseShapeKey (..),
    planReuseShapeKeyWords,
    FactorShapeNormalizationProof (..),
    fsnpRepresentativeDigest,
    fsnpRewriteSystemDigest,
    FactorShapeNormalization (..),
    fsnInputDigest,
    fsnClassId,
    fsnKey,
    fsnRepresentativeDigest,

    emptyPlanSaturationState,
    semanticNormalizationPlanRewriteSystem,
    mkPlanRewriteSystem,
    mkPlanLawProof,

    rawLogicalPlanTerm,
    canonicalPlanTerm,
    factorPlanTerm,
    fragmentPlanTerm,
    projectionPlanTerm,
    restrictionPlanTerm,
    amalgamationPlanTerm,
    coverageTransformPlanTerm,

    insertPlanTerm,
    rewritePlanSaturationState,
    canonicalizePlanClass,
    planClassCanonicalShape,

    saturatePlanShape,
    saturatePlanShapeWithState,
    normalizeFactorShapeWithState,

    projectionRestrictionCommutes,
    restrictionProjectionCommutes,
    coverageTransformCompose,
    coverSingletonEliminates,
    extractCanonicalPlanShape,
    extractCanonicalPlanKey,
    extractCanonicalPlanShapeWithProof,
  )
where

import Moonlight.Saturation.Core
  ( SaturationBudget (..),
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Normalization
  ( normalizeFactorShapeWithState,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Saturate
  ( extractCanonicalPlanKey,
    extractCanonicalPlanShape,
    extractCanonicalPlanShapeWithProof,
    saturatePlanShape,
    saturatePlanShapeWithState,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Saturation
  ( rewritePlanSaturationState,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.State
  ( canonicalizePlanClass,
    emptyPlanSaturationState,
    insertPlanTerm,
    planClassCanonicalShape,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( FactorShapeNormalization (..),
    FactorShapeNormalizationProof (..),
    PlanAnalysis (..),
    PlanEGraphResult (..),
    PlanReuseShapeKey (..),
    PlanSaturationError (..),
    PlanSaturationState (..),
    fsnClassId,
    fsnInputDigest,
    fsnKey,
    fsnRepresentativeDigest,
    fsnpRepresentativeDigest,
    fsnpRewriteSystemDigest,
    planReuseShapeKeyWords,
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode (..),
    amalgamationPlanTerm,
    canonicalPlanTerm,
    coverageTransformPlanTerm,
    factorPlanTerm,
    fragmentPlanTerm,
    projectionPlanTerm,
    rawLogicalPlanTerm,
    restrictionPlanTerm,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEqualityLaw (..),
    PlanEquivalenceProof (..),
    PlanEquivalenceStep (..),
    PlanLawProof (..),
    PlanRewriteSystem (..),
    mkPlanLawProof,
    mkPlanRewriteSystem,
    semanticNormalizationPlanRewriteSystem,
  )
import Moonlight.Flow.Plan.Rewrite.Transform.Coverage
  ( coverageTransformCompose,
    coverSingletonEliminates,
  )
import Moonlight.Flow.Plan.Rewrite.Transform.ProjectionRestriction
  ( projectionRestrictionCommutes,
    restrictionProjectionCommutes,
  )
