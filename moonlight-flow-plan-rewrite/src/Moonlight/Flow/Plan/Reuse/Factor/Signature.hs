{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Reuse.Factor.Signature
  ( PlanFactorRootSignature,
    pfrsDigest,
    factorRootSignatureFrom,
    projectFactorRootSignatures,
    restrictFactorRootSignatures,
    planFactorRootSignatureWords,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Plan.Shape.Boundary.Canonical
  ( projectCanonicalBoundary,
    restrictCanonicalBoundary,
  )
import Moonlight.Flow.Plan.Shape.Encode qualified as ShapeEncode
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestListWords,
    digestMaybeWords,
  )
import Moonlight.Flow.Plan.Residual
  ( residualShapeWords,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalBoundaryShape,
    FactorShapePayload (..),
    cbsDigest,
    cbsSchema,
    cbsSensitiveSlots,
    cbsSlotKeys,
    csepDigest,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    PlanShape (..),
    PlanStage (..),
    ProjectionPayload,
    RestrictionPayload,
  )

type PlanFactorRootSignature :: Type
data PlanFactorRootSignature = PlanFactorRootSignature
  { pfrsPlanRepresentative :: !StableDigest128,
    pfrsFragmentRepresentative :: !StableDigest128,
    pfrsAtomsDigest :: !StableDigest128,
    pfrsSourceSchema :: ![CanonSlot],
    pfrsOutputSchema :: ![CanonSlot],
    pfrsSeparatorDigest :: !(Maybe StableDigest128),
    pfrsBoundary :: !CanonicalBoundaryShape,
    pfrsResidualDigest :: !StableDigest128,
    pfrsDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

factorRootSignatureFrom ::
  PlanShape 'FactorShape ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe PlanFactorRootSignature
factorRootSignatureFrom shape maybePlanRepresentative maybeFragmentRepresentative = do
  planRepresentative <-
    maybePlanRepresentative
  fragmentRepresentative <-
    maybeFragmentRepresentative

  let payload =
        psPayload shape
      atomsDigest =
        stableDigest128
          (ShapeEncode.canonAtomMultisetWords (fspAtoms payload))
      residualDigest =
        stableDigest128
          (residualShapeWords (fspResidual payload))
      signature0 =
        PlanFactorRootSignature
          { pfrsPlanRepresentative = planRepresentative,
            pfrsFragmentRepresentative = fragmentRepresentative,
            pfrsAtomsDigest = atomsDigest,
            pfrsSourceSchema = fspSourceSchema payload,
            pfrsOutputSchema = fspOutputSchema payload,
            pfrsSeparatorDigest = fmap csepDigest (fspSeparator payload),
            pfrsBoundary = fspBoundary payload,
            pfrsResidualDigest = residualDigest,
            pfrsDigest = StableDigest128 0 0
          }

  Just (factorSignatureWithDigest signature0)
{-# INLINE factorRootSignatureFrom #-}

projectFactorRootSignatures ::
  ProjectionPayload ->
  Set PlanFactorRootSignature ->
  Set PlanFactorRootSignature
projectFactorRootSignatures projection =
  Set.map
    ( \signature ->
        factorSignatureWithBoundary
          (projectCanonicalBoundary projection (pfrsBoundary signature))
          signature
    )
{-# INLINE projectFactorRootSignatures #-}

restrictFactorRootSignatures ::
  RestrictionPayload ->
  Set PlanFactorRootSignature ->
  Set PlanFactorRootSignature
restrictFactorRootSignatures restriction =
  Set.map
    ( \signature ->
        factorSignatureWithBoundary
          (restrictCanonicalBoundary restriction (pfrsBoundary signature))
          signature
    )
{-# INLINE restrictFactorRootSignatures #-}

factorSignatureWithBoundary ::
  CanonicalBoundaryShape ->
  PlanFactorRootSignature ->
  PlanFactorRootSignature
factorSignatureWithBoundary boundary signature =
  factorSignatureWithDigest
    signature
      { pfrsSourceSchema = cbsSchema boundary,
        pfrsOutputSchema = cbsSchema boundary,
        pfrsBoundary = boundary
      }
{-# INLINE factorSignatureWithBoundary #-}

factorSignatureWithDigest ::
  PlanFactorRootSignature ->
  PlanFactorRootSignature
factorSignatureWithDigest signature =
  signature
    { pfrsDigest =
        stableDigest128
          (planFactorRootSignatureWords signature)
    }
{-# INLINE factorSignatureWithDigest #-}

planFactorRootSignatureWords ::
  PlanFactorRootSignature ->
  [Word64]
planFactorRootSignatureWords signature =
  [fromInteger 0x666163746f7253656d526f6f74]
    <> stableDigestWords (pfrsPlanRepresentative signature)
    <> stableDigestWords (pfrsFragmentRepresentative signature)
    <> stableDigestWords (pfrsAtomsDigest signature)
    <> digestListWords 0x02 ShapeEncode.canonicalSlotWords (pfrsSourceSchema signature)
    <> digestListWords 0x02 ShapeEncode.canonicalSlotWords (pfrsOutputSchema signature)
    <> digestMaybeWords 0 1 stableDigestWords (pfrsSeparatorDigest signature)
    <> digestListWords 0x02 ShapeEncode.canonicalSlotWords (Set.toAscList (cbsSensitiveSlots boundary))
    <> slotKeysWords (cbsSlotKeys boundary)
    <> stableDigestWords (cbsDigest boundary)
    <> stableDigestWords (pfrsResidualDigest signature)
  where
    boundary =
      pfrsBoundary signature
{-# INLINE planFactorRootSignatureWords #-}

slotKeysWords ::
  Map CanonSlot (Set Int) ->
  [Word64]
slotKeysWords slotKeys =
  [0x31, wordOfInt (Map.size slotKeys)]
    <> foldMap slotKeyWords (Map.toAscList slotKeys)
  where
    slotKeyWords (slot, keys) =
      ShapeEncode.canonicalSlotWords slot
        <> [wordOfInt (Set.size keys)]
        <> fmap wordOfInt (Set.toAscList keys)
{-# INLINE slotKeysWords #-}
