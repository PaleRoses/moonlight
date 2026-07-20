module Moonlight.Sheaf.Obstruction.Cohomological.Types.Core
  ( RegionScale (..),
    RegionNodeId (..),
    Anchor (..),
    CandidateRegion (crRoot, crMembers, crDepth, crScale, crNodeId, crFingerprint),
    CandidateRegionSeed (crsRoot, crsNodeId, crsContextOrdinal, crsFingerprint),
    CandidateRegionSeedKey (..),
    mkCandidateRegion,
    mkCandidateRegionWithNode,
    mkCandidateRegionSeed,
    mkCandidateRegionSeedWithContext,
    candidateRegionSeedKey,
    candidateRegionSeedFromKey,
    CandidateStalk (..),
    OccurrenceId (..),
    ConstraintId (..),
    CycleId (..),
    ExactLabelCode (..),
    anchorDomain,
  )
where

import Data.Kind (Type)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (RegionNodeId (..))

type RegionScale :: Type
data RegionScale
  = CoarseRegion
  | FineRegion
  deriving stock (Eq, Ord, Show, Read)

type Anchor :: Type -> Type
data Anchor occurrence
  = RootAnchor
  | OccurrenceAnchor !occurrence
  deriving stock (Eq, Ord, Show, Read)

type CandidateRegion :: Type -> Type
data CandidateRegion root = CandidateRegion
  { crRoot :: !root,
    crMembers :: !IntSet,
    crDepth :: !Int,
    crScale :: !RegionScale,
    crNodeId :: !(Maybe RegionNodeId),
    crFingerprint :: !Int
  }
  deriving stock (Eq, Show, Read)

type CandidateRegionSeed :: Type -> Type
data CandidateRegionSeed root = CandidateRegionSeed
  { crsRoot :: !root,
    crsNodeId :: !RegionNodeId,
    crsContextOrdinal :: !(Maybe Int),
    crsFingerprint :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type CandidateRegionSeedKey :: Type -> Type
data CandidateRegionSeedKey root = CandidateRegionSeedKey
  { crskRoot :: !root,
    crskNodeId :: !RegionNodeId,
    crskContextOrdinal :: !(Maybe Int),
    crskFingerprint :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

mkCandidateRegion ::
  root ->
  IntSet ->
  Int ->
  RegionScale ->
  Int ->
  CandidateRegion root
mkCandidateRegion rootValue memberSet depthValue scaleValue fingerprintValue =
  CandidateRegion
    { crRoot = rootValue,
      crMembers = memberSet,
      crDepth = depthValue,
      crScale = scaleValue,
      crNodeId = Nothing,
      crFingerprint = fingerprintValue
    }

mkCandidateRegionWithNode ::
  root ->
  IntSet ->
  Int ->
  RegionScale ->
  RegionNodeId ->
  Int ->
  CandidateRegion root
mkCandidateRegionWithNode rootValue memberSet depthValue scaleValue nodeIdValue fingerprintValue =
  CandidateRegion
    { crRoot = rootValue,
      crMembers = memberSet,
      crDepth = depthValue,
      crScale = scaleValue,
      crNodeId = Just nodeIdValue,
      crFingerprint = fingerprintValue
    }

mkCandidateRegionSeed ::
  root ->
  RegionNodeId ->
  Int ->
  CandidateRegionSeed root
mkCandidateRegionSeed rootValue nodeIdValue fingerprintValue =
  mkCandidateRegionSeedWithContext rootValue nodeIdValue fingerprintValue Nothing

mkCandidateRegionSeedWithContext ::
  root ->
  RegionNodeId ->
  Int ->
  Maybe Int ->
  CandidateRegionSeed root
mkCandidateRegionSeedWithContext rootValue nodeIdValue fingerprintValue contextOrdinalValue =
  CandidateRegionSeed
    { crsRoot = rootValue,
      crsNodeId = nodeIdValue,
      crsContextOrdinal = contextOrdinalValue,
      crsFingerprint = fingerprintValue
    }

candidateRegionSeedKey ::
  CandidateRegionSeed root ->
  CandidateRegionSeedKey root
candidateRegionSeedKey candidateRegionSeed =
  CandidateRegionSeedKey
    { crskRoot = crsRoot candidateRegionSeed,
      crskNodeId = crsNodeId candidateRegionSeed,
      crskContextOrdinal = crsContextOrdinal candidateRegionSeed,
      crskFingerprint = crsFingerprint candidateRegionSeed
    }

candidateRegionSeedFromKey ::
  CandidateRegionSeedKey root ->
  CandidateRegionSeed root
candidateRegionSeedFromKey candidateRegionSeedKeyValue =
  ( mkCandidateRegionSeed
      (crskRoot candidateRegionSeedKeyValue)
      (crskNodeId candidateRegionSeedKeyValue)
      (crskFingerprint candidateRegionSeedKeyValue)
  )
    { crsContextOrdinal = crskContextOrdinal candidateRegionSeedKeyValue
    }

type CandidateStalk :: Type
newtype CandidateStalk = CandidateStalk
  { unCandidateStalk :: IntSet
  }
  deriving stock (Eq, Show, Read)

type OccurrenceId :: Type
newtype OccurrenceId = OccurrenceId
  { unOccurrenceId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type ConstraintId :: Type
newtype ConstraintId = ConstraintId
  { unConstraintId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type CycleId :: Type
newtype CycleId = CycleId
  { unCycleId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type ExactLabelCode :: Type
data ExactLabelCode
  = ClassLabelCode !Int
  | FiniteLabelCode !Integer
  | TupleLabelCode ![ExactLabelCode]
  deriving stock (Eq, Ord, Show, Read)

anchorDomain :: Int -> Map OccurrenceId IntSet -> Anchor OccurrenceId -> IntSet
anchorDomain rootKey occurrenceDomains anchorValue =
  case anchorValue of
    RootAnchor -> IntSet.singleton rootKey
    OccurrenceAnchor occurrenceId -> Map.findWithDefault IntSet.empty occurrenceId occurrenceDomains
