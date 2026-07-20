{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Saturate
  ( saturatePlanShape,
    saturatePlanShapeWithState,
    extractCanonicalPlanShape,
    extractCanonicalPlanKey,
    extractCanonicalPlanShapeWithProof,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Canonicalization
  ( applyCanonicalizationMerge,
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
  ( PlanEGraphResult (..),
    PlanSaturationError (..),
    PlanSaturationState (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    canonicalPlanTerm,
    rawLogicalPlanTerm,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEquivalenceProof,
    PlanRewriteSystem,
    planEquivalenceProof,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult (..),
  )
import Moonlight.Flow.Plan.Shape.CanonicalKey qualified as Canonical
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (..),
  )

saturatePlanShape ::
  SaturationBudget ->
  PlanRewriteSystem ->
  PlanShape 'RawLogical ->
  Either PlanSaturationError PlanEGraphResult
saturatePlanShape rewriteBudget rewriteSystem =
  saturatePlanShapeWithState rewriteBudget rewriteSystem emptyPlanSaturationState

saturatePlanShapeWithState ::
  SaturationBudget ->
  PlanRewriteSystem ->
  PlanSaturationState ->
  PlanShape 'RawLogical ->
  Either PlanSaturationError PlanEGraphResult
saturatePlanShapeWithState rewriteBudget rewriteSystem state0 term = do
  (rawClass, state1) <- insertPlanTerm (rawLogicalPlanTerm term) state0
  (memo1, canonicalizationResult) <-
    first PlanSaturationCanonicalizationError $
      Canonical.canonicalizationResultFromPlanShapeMemoized
        (pssCanonicalizationMemo state1)
        term
  let canonicalShape = crPlan canonicalizationResult
  (canonicalClass, state2) <-
    insertPlanTerm
      (canonicalPlanTerm canonicalShape)
      state1 {pssCanonicalizationMemo = memo1}
  let (state3, canonicalSteps) = applyCanonicalizationMerge rewriteSystem canonicalShape rawClass canonicalClass state2
  (state4, rewriteSteps) <- rewritePlanSaturationState rewriteBudget rewriteSystem state3
  let finalRawClass = canonicalizePlanClass state4 rawClass
      finalCanonicalClass = canonicalizePlanClass state4 canonicalClass
  extractedShape <-
    maybe
      (Left (PlanSaturationNoCanonicalCandidate rawClass))
      Right
      (planClassCanonicalShape state4 finalRawClass)
  let proof =
        planEquivalenceProof
          rewriteSystem
          (Canonical.planShapeInputDigest term)
          (psDigest extractedShape)
          (canonicalSteps <> rewriteSteps)
  pure
    PlanEGraphResult
      { pegrRewriteSystem = rewriteSystem,
        pegrLogicalClass = finalRawClass,
        pegrCanonicalClass = finalCanonicalClass,
        pegrCanonicalPlanShape = extractedShape,
        pegrCanonicalProof = proof,
        pegrState = state4
      }


extractCanonicalPlanShape ::
  PlanEGraphResult ->
  PlanShape 'Canonical
extractCanonicalPlanShape =
  pegrCanonicalPlanShape

extractCanonicalPlanKey ::
  PlanEGraphResult ->
  StableDigest128
extractCanonicalPlanKey =
  psDigest . pegrCanonicalPlanShape

extractCanonicalPlanShapeWithProof ::
  PlanEGraphResult ->
  PlanClassId (PlanShape 'RawLogical) ->
  Either PlanSaturationError (PlanShape 'Canonical, PlanEquivalenceProof)
extractCanonicalPlanShapeWithProof result classId =
  if canonicalizePlanClass (pegrState result) classId == pegrLogicalClass result
    then Right (pegrCanonicalPlanShape result, pegrCanonicalProof result)
    else Left (PlanSaturationNoCanonicalCandidate classId)
