{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Subsumption
  ( ReuseKind (..),
    ContainmentWitnessKind (..),
    ReuseWitness (..),
    ReuseDecision (..),
    subsumptionWitnessDigest,
    CoverageProjectionRule (..),
    coverageProjectionRuleDigest,
    coverageProjectionWitnessKinds,
    containmentCoverageReusable,
    CarrierReuseKeyPayload (..),
    CarrierReuseId (..),
    carrierReuseIdDigest,
    carrierReuseKeyPayload,
    CarrierReuse (..),
    reuseWitnessKindStack,
    explainCarrierReuse,
    carrierReuseId,
    derivedCarrierForReuse,
    derivedAddrForReuse,
    carrierReuseExpectedTarget,
    carrierReuseFromWitness,
    CarrierReuseError (..),
    ProjectionError (..),
  )
where

import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    DerivedCarrierId (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
    ObstructionTokenSet,
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId (..),
    CarrierReuseKeyPayload (..),
    carrierReuseIdDigest,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Digest
  ( boundaryProjectionProofWords,
    boundaryProjectionWords,
    carrierAddrPayloadWords,
    containmentProofWords,
    maybePayloadWords,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Projection
  ( ProjectionError (..),
  )
import Moonlight.Flow.Execution.Subsumption.Containment
  ( containmentAtomWitnessWords,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( AtomEmbeddingProof,
    BoundaryProjectionProof,
    ContainmentProof,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    BoundaryProjectionError,
    BoundaryProjectionProfile,
  )
import Moonlight.Flow.Internal.Digest
  ( stableHashString64,
    wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    boundaryDigest,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualImplicationProof,
    residualImplicationProofWords,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    PlanShape,
    PlanStage (..),
    psDigest,
  )

type ReuseKind :: Type
data ReuseKind
  = EquivalentReuse
  | ContainmentReuse
  | ExactByCoverReuse
  deriving stock (Eq, Ord, Show, Read)

type ContainmentWitnessKind :: Type
data ContainmentWitnessKind
  = WitnessSlotIsomorphism
  | WitnessStructuralAtomEmbedding
  | WitnessCQHomomorphism
  | WitnessResidualImplication
  | WitnessBoundaryProjection
  | WitnessExactCover
  | WitnessLowerBoundOnly
  deriving stock (Eq, Ord, Show, Read)

type ReuseWitness :: Type -> Type -> Type
data ReuseWitness ctx prop = ReuseWitness
  { rwKind :: !ReuseKind,
    rwWitnessKinds :: ![ContainmentWitnessKind],
    rwSourceCarrier :: !(CarrierAddr ctx Carrier prop),
    rwTargetCarrier :: !(CarrierAddr ctx Carrier prop),
    rwSourceShape :: !(PlanShape 'FactorShape),
    rwTargetShape :: !(PlanShape 'FactorShape),
    rwProjection :: !(BoundaryProjection CanonSlot),
    rwContainmentProof :: !ContainmentProof,
    rwAtomProof :: !(Maybe AtomEmbeddingProof),
    rwResidualProof :: !ResidualImplicationProof,
    rwBoundaryProof :: !BoundaryProjectionProof,
    rwDigest :: !SubsumptionWitnessDigest
  }
  deriving stock (Eq, Ord, Show)

type ReuseDecision :: Type
data ReuseDecision
  = ReusePreserveExact ![ContainmentWitnessKind]
  | ReuseDowngradeToLowerBound ![ContainmentWitnessKind]
  | ReuseExactByCover ![ContainmentWitnessKind]
  | ReuseRejected !String
  | ReuseObstructed !ObstructionTokenSet
  deriving stock (Eq, Ord, Show, Read)

subsumptionWitnessDigest :: ReuseWitness ctx prop -> SubsumptionWitnessDigest
subsumptionWitnessDigest witness =
  SubsumptionWitnessDigest
    ( stableDigest128
        ( [0x737562576974]
            <> reuseKindWords (rwKind witness)
            <> witnessKindStackWords (rwWitnessKinds witness)
            <> carrierAddrPayloadWords (rwSourceCarrier witness)
            <> carrierAddrPayloadWords (rwTargetCarrier witness)
            <> stableDigestWords (psDigest (rwSourceShape witness))
            <> stableDigestWords (psDigest (rwTargetShape witness))
            <> boundaryProjectionWords (rwProjection witness)
            <> containmentProofWords (rwContainmentProof witness)
            <> maybePayloadWords containmentAtomWitnessWords (rwAtomProof witness)
            <> residualImplicationProofWords (rwResidualProof witness)
            <> boundaryProjectionProofWords (rwBoundaryProof witness)
        )
    )
{-# INLINE subsumptionWitnessDigest #-}

reuseKindWords :: ReuseKind -> [Word64]
reuseKindWords kind =
  case kind of
    EquivalentReuse ->
      [0x01]
    ContainmentReuse ->
      [0x02]
    ExactByCoverReuse ->
      [0x03]
{-# INLINE reuseKindWords #-}

witnessKindStackWords :: [ContainmentWitnessKind] -> [Word64]
witnessKindStackWords kinds =
  [0x776b696e6473, wordOfInt (length kinds)]
    <> fmap witnessKindWord kinds
{-# INLINE witnessKindStackWords #-}

witnessKindWord :: ContainmentWitnessKind -> Word64
witnessKindWord kind =
  case kind of
    WitnessSlotIsomorphism ->
      0x01
    WitnessStructuralAtomEmbedding ->
      0x02
    WitnessCQHomomorphism ->
      0x03
    WitnessResidualImplication ->
      0x04
    WitnessBoundaryProjection ->
      0x05
    WitnessExactCover ->
      0x06
    WitnessLowerBoundOnly ->
      0x07
{-# INLINE witnessKindWord #-}

data CoverageProjectionRule
  = PreserveExact
  | DowngradeToLowerBound
  | ExactByCover
  | ObstructProjection !ObstructionTokenSet
  deriving stock (Eq, Ord, Show, Read)

coverageProjectionRuleDigest :: CoverageProjectionRule -> StableDigest128
coverageProjectionRuleDigest rule =
  stableDigest128 $
    case rule of
      PreserveExact ->
        [fromInteger 0x70726573657276654578616374]
      DowngradeToLowerBound ->
        [fromInteger 0x646f776e67726164654c42]
      ExactByCover ->
        [fromInteger 0x65786163744279436f766572]
      ObstructProjection tokens ->
        [fromInteger 0x6f6273747275637450726f6a, stableHashString64 (show tokens)]
{-# INLINE coverageProjectionRuleDigest #-}

coverageProjectionWitnessKinds :: CoverageProjectionRule -> [ContainmentWitnessKind]
coverageProjectionWitnessKinds rule =
  case rule of
    PreserveExact ->
      []
    DowngradeToLowerBound ->
      [WitnessLowerBoundOnly]
    ExactByCover ->
      [WitnessExactCover]
    ObstructProjection _tokens ->
      []
{-# INLINE coverageProjectionWitnessKinds #-}

containmentCoverageReusable ::
  CoverageFact ->
  Bool
containmentCoverageReusable coverage =
  case coverage of
    Obstructed {} ->
      False
    _ ->
      True
{-# INLINE containmentCoverageReusable #-}

type CarrierReuse :: Type -> Type -> Type
data CarrierReuse ctx prop = CarrierReuse
  { cruWitness :: !(ReuseWitness ctx prop),
    cruTargetBoundary :: !RuntimeBoundary,
    cruTargetViewDigest :: !(Maybe StableDigest128),
    cruCoverageRule :: !CoverageProjectionRule,
    cruWitnessDeps :: !IntSet.IntSet,
    cruWitnessTopo :: !IntSet.IntSet
  }
  deriving stock (Eq, Show)

reuseWitnessKindStack :: CarrierReuse ctx prop -> [ContainmentWitnessKind]
reuseWitnessKindStack reuse =
  rwWitnessKinds (cruWitness reuse) <> coverageProjectionWitnessKinds (cruCoverageRule reuse)
{-# INLINE reuseWitnessKindStack #-}

explainCarrierReuse :: CarrierReuse ctx prop -> ReuseDecision
explainCarrierReuse reuse =
  case cruCoverageRule reuse of
    PreserveExact ->
      ReusePreserveExact (reuseWitnessKindStack reuse)
    DowngradeToLowerBound ->
      ReuseDowngradeToLowerBound (reuseWitnessKindStack reuse)
    ExactByCover ->
      ReuseExactByCover (reuseWitnessKindStack reuse)
    ObstructProjection tokens ->
      ReuseObstructed tokens
{-# INLINE explainCarrierReuse #-}

carrierReuseKeyPayload :: CarrierReuse ctx prop -> CarrierReuseKeyPayload ctx prop
carrierReuseKeyPayload reuse =
  CarrierReuseKeyPayload
    { crkpSource = rwSourceCarrier witness,
      crkpWitnessTarget = rwTargetCarrier witness,
      crkpExpectedTarget = carrierReuseExpectedTarget reuse,
      crkpWitnessDigest = rwDigest witness,
      crkpSourceShapeDigest = psDigest (rwSourceShape witness),
      crkpTargetShapeDigest = psDigest (rwTargetShape witness),
      crkpTargetBoundaryDigest = boundaryDigest (cruTargetBoundary reuse),
      crkpTargetViewDigest = cruTargetViewDigest reuse,
      crkpCoverageRuleDigest = coverageProjectionRuleDigest (cruCoverageRule reuse)
    }
  where
    witness =
      cruWitness reuse
{-# INLINE carrierReuseKeyPayload #-}

carrierReuseId :: CarrierReuse ctx prop -> CarrierReuseId ctx prop
carrierReuseId reuse =
  let payload =
        carrierReuseKeyPayload reuse
   in CarrierReuseId
        { cridPayload = payload,
          cridDigest = carrierReuseKeyPayloadDigest payload
        }
{-# INLINE carrierReuseId #-}

carrierReuseKeyPayloadDigest ::
  CarrierReuseKeyPayload ctx prop ->
  StableDigest128
carrierReuseKeyPayloadDigest payload =
  stableDigest128
    ( [0x6372754b6579]
        <> carrierAddrPayloadWords (crkpSource payload)
        <> carrierAddrPayloadWords (crkpWitnessTarget payload)
        <> maybePayloadWords carrierAddrPayloadWords (crkpExpectedTarget payload)
        <> stableDigestWords (unSubsumptionWitnessDigest (crkpWitnessDigest payload))
        <> stableDigestWords (crkpSourceShapeDigest payload)
        <> stableDigestWords (crkpTargetShapeDigest payload)
        <> stableDigestWords (crkpTargetBoundaryDigest payload)
        <> maybePayloadWords stableDigestWords (crkpTargetViewDigest payload)
        <> stableDigestWords (crkpCoverageRuleDigest payload)
    )
{-# INLINE carrierReuseKeyPayloadDigest #-}

derivedCarrierForReuse :: ReuseWitness ctx prop -> Carrier
derivedCarrierForReuse witness =
  DerivedCarrier
    DerivedCarrierId
      { dciWitness = rwDigest witness,
        dciShape = psDigest (rwTargetShape witness)
      }
{-# INLINE derivedCarrierForReuse #-}

derivedAddrForReuse ::
  CarrierAddr ctx Carrier prop ->
  ReuseWitness ctx prop ->
  CarrierAddr ctx Carrier prop
derivedAddrForReuse target witness =
  target {caCarrier = derivedCarrierForReuse witness}
{-# INLINE derivedAddrForReuse #-}

carrierReuseExpectedTarget ::
  CarrierReuse ctx prop ->
  Maybe (CarrierAddr ctx Carrier prop)
carrierReuseExpectedTarget reuse =
  case cruCoverageRule reuse of
    PreserveExact ->
      Just (rwTargetCarrier witness)
    DowngradeToLowerBound ->
      Just (derivedAddrForReuse (rwTargetCarrier witness) witness)
    ExactByCover ->
      Just (rwTargetCarrier witness)
    ObstructProjection {} ->
      Nothing
  where
    witness =
      cruWitness reuse
{-# INLINE carrierReuseExpectedTarget #-}

carrierReuseFromWitness ::
  CoverageProjectionRule ->
  RuntimeBoundary ->
  Maybe StableDigest128 ->
  IntSet.IntSet ->
  IntSet.IntSet ->
  ReuseWitness ctx prop ->
  CarrierReuse ctx prop
carrierReuseFromWitness coverageRule targetBoundary targetViewDigest witnessDeps witnessTopo witness =
  CarrierReuse
    { cruWitness = witness,
      cruTargetBoundary = targetBoundary,
      cruTargetViewDigest = targetViewDigest,
      cruCoverageRule = coverageRule,
      cruWitnessDeps = witnessDeps,
      cruWitnessTopo = witnessTopo
    }
{-# INLINE carrierReuseFromWitness #-}

data CarrierReuseError ctx prop evidence
  = CarrierReuseRowsFailed !ProjectionError
  | CarrierReuseBoundaryFailed !BoundaryProjectionError
  | CarrierReuseBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | CarrierReuseAddressPolicyFailed !CoverageProjectionRule
  | CarrierReuseSourceCoverageNotExact !(CarrierReuseId ctx prop)
  | CarrierReuseExactProjectionNotPreserved !CoverageProjectionRule !BoundaryProjectionProfile
  | CarrierReuseSupportProjectionFailed !CoverageProjectionRule !(CarrierAddr ctx Carrier prop)
  | CarrierReuseEvidenceProjectionFailed !CoverageProjectionRule !(CarrierAddr ctx Carrier prop)
  | CarrierReuseObstructed !ObstructionTokenSet
  deriving stock (Eq, Show)
