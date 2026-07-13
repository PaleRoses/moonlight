{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Control.Weight
  ( CriticalityRank (..),
    nonCriticalPriorityRank,
    criticalPriorityRank,
    priorityRankFromBool,
    EvidenceCount (..),
    zeroEvidenceCount,
    oneEvidenceCount,
    evidenceCountFromInt,
    evidenceCountToInt,
    PriorityEvidence (..),
    priorityEvidence,
    structuralPriorityEvidence,
    observedTransitionPriorityEvidence,
    observedScheduledPriorityEvidence,
    observedScheduledPriorityEvidenceNatural,
    PriorityProfile,
    emptyPriorityProfile,
    priorityProfileNull,
    singletonPriorityProfile,
    priorityProfileFromList,
    priorityProfileToList,
    lookupPriorityEvidence,
    mapPriorityProfileKeys,
    expandPriorityProfileKeys,
    comparePriorityEvidence,
    priorityEvidenceKey,
    PriorityObservation,
    emptyPriorityObservation,
    combinePriorityObservations,
    contramapPriorityObservation,
  )
where

import Data.Bool (bool)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Numeric.Natural (Natural)

type CriticalityRank :: Type
newtype CriticalityRank = CriticalityRank
  { criticalityRankValue :: Natural
  }
  deriving stock (Eq, Ord, Show, Read)

nonCriticalPriorityRank :: CriticalityRank
nonCriticalPriorityRank =
  CriticalityRank 0

criticalPriorityRank :: CriticalityRank
criticalPriorityRank =
  CriticalityRank 1

priorityRankFromBool :: Bool -> CriticalityRank
priorityRankFromBool =
  bool nonCriticalPriorityRank criticalPriorityRank

type EvidenceCount :: Type
newtype EvidenceCount = EvidenceCount
  { evidenceCountValue :: Natural
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup EvidenceCount where
  EvidenceCount leftCount <> EvidenceCount rightCount =
    EvidenceCount (leftCount + rightCount)

instance Monoid EvidenceCount where
  mempty =
    zeroEvidenceCount

zeroEvidenceCount :: EvidenceCount
zeroEvidenceCount =
  EvidenceCount 0

oneEvidenceCount :: EvidenceCount
oneEvidenceCount =
  EvidenceCount 1

evidenceCountFromInt :: Int -> EvidenceCount
evidenceCountFromInt =
  EvidenceCount . fromIntegral . max 0

evidenceCountToInt :: EvidenceCount -> Int
evidenceCountToInt =
  naturalToBoundedInt . evidenceCountValue

naturalToBoundedInt :: Natural -> Int
naturalToBoundedInt naturalValue =
  fromInteger (min (toInteger (maxBound :: Int)) (toInteger naturalValue))

type PriorityEvidence :: Type
data PriorityEvidence = PriorityEvidence
  { peStructuralInfluence :: !EvidenceCount,
    peObservedTransitionCount :: !EvidenceCount,
    peObservedScheduledCount :: !EvidenceCount,
    peCriticalityRank :: !CriticalityRank
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup PriorityEvidence where
  leftEvidence <> rightEvidence =
    PriorityEvidence
      { peStructuralInfluence =
          peStructuralInfluence leftEvidence <> peStructuralInfluence rightEvidence,
        peObservedTransitionCount =
          peObservedTransitionCount leftEvidence <> peObservedTransitionCount rightEvidence,
        peObservedScheduledCount =
          peObservedScheduledCount leftEvidence <> peObservedScheduledCount rightEvidence,
        peCriticalityRank =
          max
            (peCriticalityRank leftEvidence)
            (peCriticalityRank rightEvidence)
      }

instance Monoid PriorityEvidence where
  mempty =
    PriorityEvidence
      { peStructuralInfluence = zeroEvidenceCount,
        peObservedTransitionCount = zeroEvidenceCount,
        peObservedScheduledCount = zeroEvidenceCount,
        peCriticalityRank = nonCriticalPriorityRank
      }

priorityEvidence ::
  Int ->
  Int ->
  Int ->
  CriticalityRank ->
  PriorityEvidence
priorityEvidence structuralInfluence observedTransitionCount observedScheduledCount criticalityRank =
  PriorityEvidence
    { peStructuralInfluence = evidenceCountFromInt structuralInfluence,
      peObservedTransitionCount = evidenceCountFromInt observedTransitionCount,
      peObservedScheduledCount = evidenceCountFromInt observedScheduledCount,
      peCriticalityRank = criticalityRank
    }

structuralPriorityEvidence :: Int -> PriorityEvidence
structuralPriorityEvidence structuralInfluence =
  priorityEvidence structuralInfluence 0 0 nonCriticalPriorityRank

observedTransitionPriorityEvidence :: Int -> PriorityEvidence
observedTransitionPriorityEvidence observedTransitionCount =
  priorityEvidence 0 observedTransitionCount 0 nonCriticalPriorityRank

observedScheduledPriorityEvidence :: Int -> CriticalityRank -> PriorityEvidence
observedScheduledPriorityEvidence =
  priorityEvidence 0 0

observedScheduledPriorityEvidenceNatural :: Natural -> CriticalityRank -> PriorityEvidence
observedScheduledPriorityEvidenceNatural observedScheduledCount criticalityRank =
  PriorityEvidence
    { peStructuralInfluence = zeroEvidenceCount,
      peObservedTransitionCount = zeroEvidenceCount,
      peObservedScheduledCount = EvidenceCount observedScheduledCount,
      peCriticalityRank = criticalityRank
    }

type PriorityProfile :: Type -> Type
newtype PriorityProfile group = PriorityProfile
  { sppGroupPriorities :: Map group PriorityEvidence
  }
  deriving stock (Eq, Ord, Show)

instance Ord group => Semigroup (PriorityProfile group) where
  PriorityProfile leftPriorities <> PriorityProfile rightPriorities =
    normalizePriorityMap
      (Map.unionWith (<>) leftPriorities rightPriorities)

instance Ord group => Monoid (PriorityProfile group) where
  mempty =
    emptyPriorityProfile

emptyPriorityProfile :: PriorityProfile group
emptyPriorityProfile =
  PriorityProfile Map.empty

priorityProfileNull :: PriorityProfile group -> Bool
priorityProfileNull =
  Map.null . sppGroupPriorities

singletonPriorityProfile ::
  group ->
  PriorityEvidence ->
  PriorityProfile group
singletonPriorityProfile group priority
  | priority == mempty =
      emptyPriorityProfile
  | otherwise =
      PriorityProfile (Map.singleton group priority)

priorityProfileFromList ::
  Ord group =>
  [(group, PriorityEvidence)] ->
  PriorityProfile group
priorityProfileFromList =
  normalizePriorityMap
    . Map.fromListWith (<>)

priorityProfileToList ::
  PriorityProfile group ->
  [(group, PriorityEvidence)]
priorityProfileToList =
  Map.toAscList . sppGroupPriorities

normalizePriorityMap ::
  Map group PriorityEvidence ->
  PriorityProfile group
normalizePriorityMap =
  PriorityProfile
    . Map.filter (/= mempty)

lookupPriorityEvidence ::
  Ord group =>
  group ->
  PriorityProfile group ->
  PriorityEvidence
lookupPriorityEvidence group =
  Map.findWithDefault mempty group . sppGroupPriorities

mapPriorityProfileKeys ::
  Ord group' =>
  (group -> group') ->
  PriorityProfile group ->
  PriorityProfile group'
mapPriorityProfileKeys projectGroup =
  priorityProfileFromList
    . fmap projectEntry
    . Map.toAscList
    . sppGroupPriorities
  where
    projectEntry (group, priority) =
      (projectGroup group, priority)

expandPriorityProfileKeys ::
  Ord group' =>
  (group -> NonEmpty group') ->
  PriorityProfile group ->
  PriorityProfile group'
expandPriorityProfileKeys expandGroup =
  priorityProfileFromList
    . concatMap expandEntry
    . Map.toAscList
    . sppGroupPriorities
  where
    expandEntry (group, priority) =
      fmap
        (\expandedGroup -> (expandedGroup, priority))
        (NonEmpty.toList (expandGroup group))

comparePriorityEvidence ::
  PriorityEvidence ->
  PriorityEvidence ->
  Ordering
comparePriorityEvidence =
  comparing priorityEvidenceKey

-- | The descending sort key realising 'comparePriorityEvidence'. Compute it
-- once per group when ordering many groups; comparing precomputed keys
-- avoids re-deriving the tuple inside every comparison. O(1).
priorityEvidenceKey ::
  PriorityEvidence ->
  (Down CriticalityRank, Down EvidenceCount, Down EvidenceCount, Down EvidenceCount)
priorityEvidenceKey evidence =
  ( Down (peCriticalityRank evidence),
    Down (peObservedTransitionCount evidence),
    Down (peObservedScheduledCount evidence),
    Down (peStructuralInfluence evidence)
  )

type PriorityObservation :: Type -> Type -> Type
type PriorityObservation source group =
  source -> PriorityProfile group

emptyPriorityObservation :: PriorityObservation source group
emptyPriorityObservation =
  const emptyPriorityProfile

combinePriorityObservations ::
  Ord group =>
  [PriorityObservation source group] ->
  PriorityObservation source group
combinePriorityObservations observations source =
  Foldable.foldl'
    (\profile observation -> profile <> observation source)
    emptyPriorityProfile
    observations

contramapPriorityObservation ::
  (source' -> source) ->
  PriorityObservation source group ->
  PriorityObservation source' group
contramapPriorityObservation projectSource observation =
  observation . projectSource
