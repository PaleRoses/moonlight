{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Verify
  ( PlanProofError (..),
    verifyPlanEquivalenceProof,
  )
where

import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Flow.Plan.Shape.CanonicalKey qualified as Canonical
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Plan.Rewrite
  ( PlanEGraphResult (..),
    PlanEqualityLaw,
    PlanEquivalenceProof (..),
    PlanEquivalenceStep (..),
    PlanRewriteSystem (..),
    pegrCanonicalPlanShape,
    pepDigest,
    pepRewriteSystemDigest,
    pepSourceDigest,
    pepSteps,
    pepTargetDigest,
    planClassCanonicalShape,
    plpDigest,
    plpLaw,
    plpSideConditionDigest,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (Canonical, RawLogical),
    psDigest,
  )

data PlanProofError
  = PlanProofSourceDigestMismatch !StableDigest128 !StableDigest128
  | PlanProofTargetDigestMismatch !StableDigest128 !StableDigest128
  | PlanProofRewriteSystemDigestMismatch !StableDigest128 !StableDigest128
  | PlanProofResultRewriteSystemMismatch
  | PlanProofDisabledLaw !PlanEqualityLaw
  | PlanProofDigestMismatch !StableDigest128 !StableDigest128
  | PlanProofFinalClassMissingTarget
  | PlanProofResultTargetMismatch !(PlanShape 'Canonical) !(PlanShape 'Canonical)
  deriving stock (Eq, Show)

verifyPlanEquivalenceProof ::
  PlanRewriteSystem ->
  PlanShape 'RawLogical ->
  PlanShape 'Canonical ->
  PlanEGraphResult ->
  Either PlanProofError ()
verifyPlanEquivalenceProof rewriteSystem input target result = do
  let proof = pegrCanonicalProof result
  expectEqual
    (PlanProofSourceDigestMismatch (Canonical.planShapeInputDigest input))
    (Canonical.planShapeInputDigest input)
    (pepSourceDigest proof)
  expectEqual
    (PlanProofTargetDigestMismatch (psDigest target))
    (psDigest target)
    (pepTargetDigest proof)
  expectEqual
    (PlanProofRewriteSystemDigestMismatch (prsDigest rewriteSystem))
    (prsDigest rewriteSystem)
    (pepRewriteSystemDigest proof)
  if pegrRewriteSystem result == rewriteSystem
    then Right ()
    else Left PlanProofResultRewriteSystemMismatch
  traverse_ assertLawEnabled (proofLaws proof)
  expectEqual
    (PlanProofDigestMismatch (expectedProofDigest proof))
    (expectedProofDigest proof)
    (pepDigest proof)
  case planClassCanonicalShape (pegrState result) (pegrLogicalClass result) of
    Just finalTarget
      | finalTarget == target -> Right ()
      | otherwise -> Left (PlanProofResultTargetMismatch target finalTarget)
    Nothing -> Left PlanProofFinalClassMissingTarget
  if pegrCanonicalPlanShape result == target
    then Right ()
    else Left (PlanProofResultTargetMismatch target (pegrCanonicalPlanShape result))
  where
    assertLawEnabled law =
      if Set.member law (prsEnabledLaws rewriteSystem)
        then Right ()
        else Left (PlanProofDisabledLaw law)

expectEqual :: (Eq value) => (value -> PlanProofError) -> value -> value -> Either PlanProofError ()
expectEqual buildError expected actual =
  if expected == actual
    then Right ()
    else Left (buildError actual)

proofLaws :: PlanEquivalenceProof -> [PlanEqualityLaw]
proofLaws =
  fmap stepLaw . pepSteps
  where
    stepLaw step =
      case step of
        EqStepAppliedLaw proof -> plpLaw proof

expectedProofDigest :: PlanEquivalenceProof -> StableDigest128
expectedProofDigest proof =
  stableDigest128
    ( [0x657150726f6f66]
        <> stableDigestWords (pepSourceDigest proof)
        <> stableDigestWords (pepTargetDigest proof)
        <> stableDigestWords (pepRewriteSystemDigest proof)
        <> foldMap equivalenceStepWords (pepSteps proof)
    )

equivalenceStepWords :: PlanEquivalenceStep -> [Word64]
equivalenceStepWords step =
  case step of
    EqStepAppliedLaw proof ->
      [0x6c617750726f6f66]
        <> stableDigestWords (plpDigest proof)
        <> stableDigestWords (plpSideConditionDigest proof)
