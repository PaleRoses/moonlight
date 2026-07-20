{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanRewriteSystem (..),
    PlanEqualityLaw (..),
    PlanLawProof (..),
    PlanEquivalenceStep (..),
    PlanEquivalenceProof (..),
    semanticNormalizationPlanRewriteSystem,
    mkPlanRewriteSystem,
    mkPlanLawProof,
    planLawProofDigest,
    planEquivalenceProof,
    planEquivalenceProofDigest,
    equivalenceStepWords,
    rewriteSystemDigest,
    lawWord,
    classWord,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( classIdKey,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestMaybeWords,
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
  )

type PlanEqualityLaw :: Type
data PlanEqualityLaw
  = LawAtomOrder
  | LawAlphaCanonical
  | LawProjectionId
  | LawProjectionCompose
  | LawRestrictionId
  | LawRestrictionCompose
  | LawProjectionRestrictionCommute
  | LawRestrictionProjectionCommute
  | LawProjectionRestrictionFuse
  | LawCoverMemberOrder
  | LawCoverSingleton
  | LawCoverageTransformId
  | LawCoverageTransformCompose
  deriving stock (Eq, Ord, Show, Read)

type PlanLawProof :: Type
data PlanLawProof = PlanLawProof
  { plpLaw :: !PlanEqualityLaw,
    plpSourceClass :: !(PlanClassId ()),
    plpTargetClass :: !(PlanClassId ()),
    plpSlotMapDigest :: !(Maybe StableDigest128),
    plpBoundaryBeforeDigest :: !(Maybe StableDigest128),
    plpBoundaryAfterDigest :: !(Maybe StableDigest128),
    plpResidualProofDigest :: !(Maybe StableDigest128),
    plpCoverProofDigest :: !(Maybe StableDigest128),
    plpSideConditionDigest :: !StableDigest128,
    plpDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanRewriteSystem :: Type
data PlanRewriteSystem = PlanRewriteSystem
  { prsDigest :: !StableDigest128,
    prsEnabledLaws :: !(Set PlanEqualityLaw)
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanEquivalenceStep :: Type
data PlanEquivalenceStep
  = EqStepAppliedLaw !PlanLawProof
  deriving stock (Eq, Ord, Show, Read)

type PlanEquivalenceProof :: Type
data PlanEquivalenceProof = PlanEquivalenceProof
  { pepSourceDigest :: !StableDigest128,
    pepTargetDigest :: !StableDigest128,
    pepRewriteSystemDigest :: !StableDigest128,
    pepSteps :: ![PlanEquivalenceStep],
    pepDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

semanticNormalizationPlanRewriteSystem ::
  PlanRewriteSystem
semanticNormalizationPlanRewriteSystem =
  mkPlanRewriteSystem
    ( Set.fromList
        [ LawAlphaCanonical,
          LawAtomOrder,
          LawProjectionId,
          LawProjectionCompose,
          LawRestrictionId,
          LawRestrictionCompose,
          LawProjectionRestrictionCommute,
          LawRestrictionProjectionCommute,
          LawProjectionRestrictionFuse,
          LawCoverMemberOrder,
          LawCoverSingleton,
          LawCoverageTransformId,
          LawCoverageTransformCompose
        ]
    )
{-# INLINE semanticNormalizationPlanRewriteSystem #-}

mkPlanRewriteSystem ::
  Set PlanEqualityLaw ->
  PlanRewriteSystem
mkPlanRewriteSystem laws =
  let rewriteSystem0 =
        PlanRewriteSystem
          { prsEnabledLaws = laws,
            prsDigest = StableDigest128 0 0
          }
   in rewriteSystem0 {prsDigest = rewriteSystemDigest rewriteSystem0}
{-# INLINE mkPlanRewriteSystem #-}

mkPlanLawProof ::
  PlanEqualityLaw ->
  PlanClassId () ->
  PlanClassId () ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  StableDigest128 ->
  PlanLawProof
mkPlanLawProof law sourceClass targetClass slotMapDigest boundaryBeforeDigest boundaryAfterDigest residualProofDigest coverProofDigest sideConditionDigest =
  let proof0 =
        PlanLawProof
          { plpLaw = law,
            plpSourceClass = sourceClass,
            plpTargetClass = targetClass,
            plpSlotMapDigest = slotMapDigest,
            plpBoundaryBeforeDigest = boundaryBeforeDigest,
            plpBoundaryAfterDigest = boundaryAfterDigest,
            plpResidualProofDigest = residualProofDigest,
            plpCoverProofDigest = coverProofDigest,
            plpSideConditionDigest = sideConditionDigest,
            plpDigest = StableDigest128 0 0
          }
   in proof0 {plpDigest = planLawProofDigest proof0}
{-# INLINE mkPlanLawProof #-}

planLawProofDigest ::
  PlanLawProof ->
  StableDigest128
planLawProofDigest proof =
  stableDigest128
    ( [0x706c61774f6f66, lawWord (plpLaw proof)]
        <> planClassIdWords (plpSourceClass proof)
        <> planClassIdWords (plpTargetClass proof)
        <> digestMaybeWords 0 1 stableDigestWords (plpSlotMapDigest proof)
        <> digestMaybeWords 0 1 stableDigestWords (plpBoundaryBeforeDigest proof)
        <> digestMaybeWords 0 1 stableDigestWords (plpBoundaryAfterDigest proof)
        <> digestMaybeWords 0 1 stableDigestWords (plpResidualProofDigest proof)
        <> digestMaybeWords 0 1 stableDigestWords (plpCoverProofDigest proof)
        <> stableDigestWords (plpSideConditionDigest proof)
    )
{-# INLINE planLawProofDigest #-}

planEquivalenceProof ::
  PlanRewriteSystem ->
  StableDigest128 ->
  StableDigest128 ->
  [PlanEquivalenceStep] ->
  PlanEquivalenceProof
planEquivalenceProof rewriteSystem sourceDigest targetDigest steps =
  let proof0 =
        PlanEquivalenceProof
          { pepSourceDigest = sourceDigest,
            pepTargetDigest = targetDigest,
            pepRewriteSystemDigest = prsDigest rewriteSystem,
            pepSteps = steps,
            pepDigest = targetDigest
          }
   in proof0 {pepDigest = planEquivalenceProofDigest proof0}
{-# INLINE planEquivalenceProof #-}

planEquivalenceProofDigest ::
  PlanEquivalenceProof ->
  StableDigest128
planEquivalenceProofDigest proof =
  stableDigest128
    ( [0x657150726f6f66]
        <> stableDigestWords (pepSourceDigest proof)
        <> stableDigestWords (pepTargetDigest proof)
        <> stableDigestWords (pepRewriteSystemDigest proof)
        <> foldMap equivalenceStepWords (pepSteps proof)
    )
{-# INLINE planEquivalenceProofDigest #-}

equivalenceStepWords ::
  PlanEquivalenceStep ->
  [Word64]
equivalenceStepWords step =
  case step of
    EqStepAppliedLaw proof ->
      [0x6c617750726f6f66]
        <> stableDigestWords (plpDigest proof)
        <> stableDigestWords (plpSideConditionDigest proof)
{-# INLINE equivalenceStepWords #-}

rewriteSystemDigest ::
  PlanRewriteSystem ->
  StableDigest128
rewriteSystemDigest rewriteSystem =
  stableDigest128
    ( [0x7277725379734c, wordOfInt (Set.size (prsEnabledLaws rewriteSystem))]
        <> fmap lawWord (Set.toAscList (prsEnabledLaws rewriteSystem))
    )
{-# INLINE rewriteSystemDigest #-}

lawWord ::
  PlanEqualityLaw ->
  Word64
lawWord law =
  case law of
    LawAtomOrder -> 0x01
    LawAlphaCanonical -> 0x02
    LawProjectionId -> 0x03
    LawProjectionCompose -> 0x04
    LawRestrictionId -> 0x05
    LawRestrictionCompose -> 0x06
    LawProjectionRestrictionCommute -> 0x07
    LawRestrictionProjectionCommute -> 0x08
    LawProjectionRestrictionFuse -> 0x09
    LawCoverMemberOrder -> 0x0a
    LawCoverSingleton -> 0x0b
    LawCoverageTransformId -> 0x0c
    LawCoverageTransformCompose -> 0x0d
{-# INLINE lawWord #-}

classWord ::
  PlanClassId stage ->
  Word64
classWord =
  wordOfInt . classIdKey . unPlanClassId
{-# INLINE classWord #-}

planClassIdWords ::
  PlanClassId () ->
  [Word64]
planClassIdWords classId =
  [classWord classId]
{-# INLINE planClassIdWords #-}
