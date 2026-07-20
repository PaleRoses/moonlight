{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Moonlight.Flow.Plan.Residual
  ( ResidualTheoryId (..),
    ResidualProofDigest (..),
    ResidualNormalForm (..),
    ResidualShape (..),
    QueryPlanResidual (..),
    queryPlanResidualGuard,
    queryPlanResidualIdentityDigest,
    queryPlanResidualShape,
    RawResidualPayload (..),
    rawResidualPayloadAs,
    RawResidual (..),
    ResidualTheoryError (..),
    ResidualTheoryOps (..),
    ResidualCrossTheoryOps (..),
    ResidualTheoryRegistry (..),
    emptyResidualTheoryRegistry,
    normalizeRawResidual,
    ResidualImplicationProof (..),
    ResidualContainmentProof (..),
    ResidualContainmentRejection (..),
    residualContainmentProof,
    ResidualCandidateKey (..),
    residualCandidateKey,
    residualCandidateKeysForRequest,
    residualShapeWords,
    residualTheoryIdWords,
    residualNormalFormWords,
    residualProofDigestWords,
    residualImplicationProofWords,
    residualContainmentProofWords,
    residualContainmentRejectionWords,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Typeable
  ( Typeable,
    cast,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigestWords,
  )

type ResidualTheoryId :: Type
newtype ResidualTheoryId = ResidualTheoryId
  { unResidualTheoryId :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)

type ResidualProofDigest :: Type
newtype ResidualProofDigest = ResidualProofDigest
  { unResidualProofDigest :: StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type ResidualNormalForm :: Type
data ResidualNormalForm = ResidualNormalForm
  { rnfDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type ResidualShape :: Type
data ResidualShape
  = ResidualNone
  | ResidualDigestOnly {-# UNPACK #-} !Word64 ![Word64]
  | ResidualTheory !ResidualTheoryId !ResidualNormalForm
  deriving stock (Eq, Ord, Show, Read)

type QueryPlanResidual :: Type -> Type
data QueryPlanResidual guard
  = NoQueryPlanResidual
  | QueryPlanResidual
      { qprGuard :: !guard,
        qprIdentityDigest :: {-# UNPACK #-} !Word64,
        qprShape :: !ResidualShape
      }

deriving stock instance Eq guard => Eq (QueryPlanResidual guard)

deriving stock instance Ord guard => Ord (QueryPlanResidual guard)

deriving stock instance Show guard => Show (QueryPlanResidual guard)

deriving stock instance Read guard => Read (QueryPlanResidual guard)

queryPlanResidualGuard :: QueryPlanResidual guard -> Maybe guard
queryPlanResidualGuard =
  \case
    NoQueryPlanResidual ->
      Nothing
    QueryPlanResidual {qprGuard = guardValue} ->
      Just guardValue
{-# INLINE queryPlanResidualGuard #-}

queryPlanResidualIdentityDigest :: QueryPlanResidual guard -> Maybe Word64
queryPlanResidualIdentityDigest =
  \case
    NoQueryPlanResidual ->
      Nothing
    QueryPlanResidual {qprIdentityDigest = digestValue} ->
      Just digestValue
{-# INLINE queryPlanResidualIdentityDigest #-}

queryPlanResidualShape :: QueryPlanResidual guard -> ResidualShape
queryPlanResidualShape =
  \case
    NoQueryPlanResidual ->
      ResidualNone
    QueryPlanResidual {qprShape = shapeValue} ->
      shapeValue
{-# INLINE queryPlanResidualShape #-}

type RawResidualPayload :: Type
data RawResidualPayload =
  forall raw.
  Typeable raw =>
  RawResidualPayload !raw

rawResidualPayloadAs :: Typeable raw => RawResidualPayload -> Maybe raw
rawResidualPayloadAs (RawResidualPayload payload) =
  cast payload
{-# INLINE rawResidualPayloadAs #-}

type RawResidual :: Type
data RawResidual
  = RawResidualDigest {-# UNPACK #-} !Word64 ![Word64]
  | RawResidualTheory !ResidualTheoryId !RawResidualPayload

type ResidualTheoryError :: Type
data ResidualTheoryError
  = ResidualTheoryUnknown !ResidualTheoryId
  | ResidualTheoryPayloadRejected !ResidualTheoryId !String
  deriving stock (Eq, Ord, Show, Read)

type ResidualTheoryOps :: Type
data ResidualTheoryOps = ResidualTheoryOps
  { rtoNormalize ::
      RawResidualPayload ->
      Either ResidualTheoryError ResidualNormalForm,
    rtoEquivalent ::
      ResidualNormalForm ->
      ResidualNormalForm ->
      Maybe ResidualProofDigest,
    rtoImplies ::
      ResidualNormalForm ->
      ResidualNormalForm ->
      Maybe ResidualProofDigest
  }

type ResidualCrossTheoryOps :: Type
data ResidualCrossTheoryOps = ResidualCrossTheoryOps
  { rctoImplies ::
      ResidualNormalForm ->
      ResidualNormalForm ->
      Maybe ResidualProofDigest
  }

type ResidualTheoryRegistry :: Type
data ResidualTheoryRegistry = ResidualTheoryRegistry
  { rtrTheories :: !(Map ResidualTheoryId ResidualTheoryOps),
    rtrCrossTheory :: !(Map (ResidualTheoryId, ResidualTheoryId) ResidualCrossTheoryOps)
  }

emptyResidualTheoryRegistry :: ResidualTheoryRegistry
emptyResidualTheoryRegistry =
  ResidualTheoryRegistry
    { rtrTheories = Map.empty,
      rtrCrossTheory = Map.empty
    }
{-# INLINE emptyResidualTheoryRegistry #-}

normalizeRawResidual ::
  ResidualTheoryRegistry ->
  RawResidual ->
  Either ResidualTheoryError ResidualShape
normalizeRawResidual registry =
  \case
    RawResidualDigest digestValue identityWords ->
      Right (ResidualDigestOnly digestValue identityWords)
    RawResidualTheory theoryId payload ->
      case Map.lookup theoryId (rtrTheories registry) of
        Nothing ->
          Left (ResidualTheoryUnknown theoryId)
        Just ops ->
          ResidualTheory theoryId <$> rtoNormalize ops payload
{-# INLINE normalizeRawResidual #-}

type ResidualImplicationProof :: Type
data ResidualImplicationProof
  = ResidualBothNone
  | ResidualEqualDigest {-# UNPACK #-} !Word64
  | ResidualTheoryImplies !ResidualTheoryId !ResidualProofDigest
  | ResidualCrossTheoryImplies !ResidualTheoryId !ResidualTheoryId !ResidualProofDigest
  deriving stock (Eq, Ord, Show, Read)

type ResidualContainmentProof :: Type
data ResidualContainmentProof
  = ResidualContainmentAccepted !ResidualImplicationProof
  | ResidualContainmentRejected !ResidualContainmentRejection
  deriving stock (Eq, Ord, Show, Read)

type ResidualContainmentRejection :: Type
data ResidualContainmentRejection
  = ResidualNoneMismatch !ResidualShape !ResidualShape
  | ResidualDigestMismatch {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | ResidualDigestCollision {-# UNPACK #-} !Word64
  | ResidualDigestTheoryMismatch
  | ResidualTheoryMissing !ResidualTheoryId
  | ResidualTheoryImplicationMissing !ResidualTheoryId
  | ResidualCrossTheoryMissing !ResidualTheoryId !ResidualTheoryId
  deriving stock (Eq, Ord, Show, Read)

residualContainmentProof ::
  ResidualTheoryRegistry ->
  ResidualShape ->
  ResidualShape ->
  ResidualContainmentProof
residualContainmentProof registry sourceResidual requestedResidual =
  case (sourceResidual, requestedResidual) of
    (ResidualNone, ResidualNone) ->
      accepted ResidualBothNone
    (ResidualNone, _) ->
      rejected (ResidualNoneMismatch sourceResidual requestedResidual)
    (_, ResidualNone) ->
      rejected (ResidualNoneMismatch sourceResidual requestedResidual)
    (ResidualDigestOnly sourceDigest sourceWords, ResidualDigestOnly requestedDigest requestedWords)
      | sourceDigest /= requestedDigest ->
          rejected (ResidualDigestMismatch sourceDigest requestedDigest)
      | sourceWords == requestedWords ->
          accepted (ResidualEqualDigest sourceDigest)
      | otherwise ->
          rejected (ResidualDigestCollision sourceDigest)
    (ResidualDigestOnly {}, ResidualTheory {}) ->
      rejected ResidualDigestTheoryMismatch
    (ResidualTheory {}, ResidualDigestOnly {}) ->
      rejected ResidualDigestTheoryMismatch
    (ResidualTheory sourceTheory sourceNormal, ResidualTheory requestedTheory requestedNormal)
      | sourceTheory == requestedTheory ->
          sameTheoryProof sourceTheory sourceNormal requestedNormal
      | otherwise ->
          crossTheoryProof sourceTheory requestedTheory sourceNormal requestedNormal
  where
    accepted =
      ResidualContainmentAccepted
    rejected =
      ResidualContainmentRejected
    sameTheoryProof theoryId sourceNormal requestedNormal =
      case Map.lookup theoryId (rtrTheories registry) of
        Nothing ->
          rejected (ResidualTheoryMissing theoryId)
        Just ops ->
          case rtoImplies ops sourceNormal requestedNormal of
            Nothing ->
              rejected (ResidualTheoryImplicationMissing theoryId)
            Just proofDigest ->
              accepted (ResidualTheoryImplies theoryId proofDigest)
    crossTheoryProof sourceTheory requestedTheory sourceNormal requestedNormal =
      case Map.lookup (sourceTheory, requestedTheory) (rtrCrossTheory registry) of
        Nothing ->
          rejected (ResidualCrossTheoryMissing sourceTheory requestedTheory)
        Just ops ->
          case rctoImplies ops sourceNormal requestedNormal of
            Nothing ->
              rejected (ResidualTheoryImplicationMissing sourceTheory)
            Just proofDigest ->
              accepted (ResidualCrossTheoryImplies sourceTheory requestedTheory proofDigest)
{-# INLINE residualContainmentProof #-}

type ResidualCandidateKey :: Type
data ResidualCandidateKey
  = ResidualCandidateNone
  | ResidualCandidateDigest {-# UNPACK #-} !Word64
  | ResidualCandidateTheory !ResidualTheoryId
  deriving stock (Eq, Ord, Show, Read)

residualCandidateKey :: ResidualShape -> ResidualCandidateKey
residualCandidateKey =
  \case
    ResidualNone ->
      ResidualCandidateNone
    ResidualDigestOnly digestValue _ ->
      ResidualCandidateDigest digestValue
    ResidualTheory theoryId _ ->
      ResidualCandidateTheory theoryId
{-# INLINE residualCandidateKey #-}

residualCandidateKeysForRequest ::
  ResidualTheoryRegistry ->
  ResidualShape ->
  [ResidualCandidateKey]
residualCandidateKeysForRequest registry =
  \case
    ResidualNone ->
      [ResidualCandidateNone]
    ResidualDigestOnly digestValue _ ->
      [ResidualCandidateDigest digestValue]
    ResidualTheory requestedTheory _ ->
      ResidualCandidateTheory requestedTheory
        : [ ResidualCandidateTheory sourceTheory
          | ((sourceTheory, targetTheory), _) <- Map.toAscList (rtrCrossTheory registry),
            targetTheory == requestedTheory
          ]
{-# INLINE residualCandidateKeysForRequest #-}

residualShapeWords :: ResidualShape -> [Word64]
residualShapeWords =
  \case
    ResidualNone ->
      [0x07]
    ResidualDigestOnly digestValue identityWords ->
      0x08 : digestValue : fromIntegral (length identityWords) : identityWords
    ResidualTheory theoryId normalForm ->
      [0x09]
        <> residualTheoryIdWords theoryId
        <> residualNormalFormWords normalForm
{-# INLINE residualShapeWords #-}

residualTheoryIdWords :: ResidualTheoryId -> [Word64]
residualTheoryIdWords (ResidualTheoryId theoryWord) =
  [0x0a, theoryWord]
{-# INLINE residualTheoryIdWords #-}

residualNormalFormWords :: ResidualNormalForm -> [Word64]
residualNormalFormWords normalForm =
  [0x0b] <> stableDigestWords (rnfDigest normalForm)
{-# INLINE residualNormalFormWords #-}

residualProofDigestWords :: ResidualProofDigest -> [Word64]
residualProofDigestWords (ResidualProofDigest digestValue) =
  [0x0c] <> stableDigestWords digestValue
{-# INLINE residualProofDigestWords #-}

residualImplicationProofWords :: ResidualImplicationProof -> [Word64]
residualImplicationProofWords =
  \case
    ResidualBothNone ->
      [0x20]
    ResidualEqualDigest digestValue ->
      [0x21, digestValue]
    ResidualTheoryImplies theoryId proofDigest ->
      [0x22]
        <> residualTheoryIdWords theoryId
        <> residualProofDigestWords proofDigest
    ResidualCrossTheoryImplies sourceTheory requestedTheory proofDigest ->
      [0x23]
        <> residualTheoryIdWords sourceTheory
        <> residualTheoryIdWords requestedTheory
        <> residualProofDigestWords proofDigest
{-# INLINE residualImplicationProofWords #-}

residualContainmentProofWords :: ResidualContainmentProof -> [Word64]
residualContainmentProofWords =
  \case
    ResidualContainmentAccepted proof ->
      [0x30] <> residualImplicationProofWords proof
    ResidualContainmentRejected rejection ->
      [0x31] <> residualContainmentRejectionWords rejection
{-# INLINE residualContainmentProofWords #-}

residualContainmentRejectionWords :: ResidualContainmentRejection -> [Word64]
residualContainmentRejectionWords =
  \case
    ResidualNoneMismatch source requested ->
      [0x40]
        <> residualShapeWords source
        <> residualShapeWords requested
    ResidualDigestMismatch sourceDigest requestedDigest ->
      [0x41, sourceDigest, requestedDigest]
    ResidualDigestCollision digestValue ->
      [0x46, digestValue]
    ResidualDigestTheoryMismatch ->
      [0x42]
    ResidualTheoryMissing theoryId ->
      [0x43] <> residualTheoryIdWords theoryId
    ResidualTheoryImplicationMissing theoryId ->
      [0x44] <> residualTheoryIdWords theoryId
    ResidualCrossTheoryMissing sourceTheory requestedTheory ->
      [0x45]
        <> residualTheoryIdWords sourceTheory
        <> residualTheoryIdWords requestedTheory
{-# INLINE residualContainmentRejectionWords #-}
