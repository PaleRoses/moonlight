-- | Evidence feedback: how per-round observations update the dynamic
-- priority profile. Accumulating policies act on the current profile by the
-- profile monoid; a replacing policy discards it. Replacement wins over
-- accumulation within a round, and policies of the same mode combine by
-- '<>' — the profile monoid is commutative, so policy order is irrelevant.
module Moonlight.Control.Engine.Evidence
  ( PriorityUpdateMode (..),
    EvidencePolicy (..),
    noEvidencePolicy,
    applyEvidencePolicies,
  )
where

import Data.Foldable qualified as Foldable

import Moonlight.Control.Weight
  ( PriorityObservation,
    PriorityProfile,
    emptyPriorityProfile,
  )

-- | Whether observed evidence accumulates onto the dynamic profile or
-- replaces it.
data PriorityUpdateMode
  = AccumulateDynamicPriority
  | ReplaceDynamicPriority
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | One feedback rule: observe a source into a profile, with an update mode,
-- declaring whether it needs the schedule trace to be populated.
data EvidencePolicy source group = EvidencePolicy
  { epObserve :: !(PriorityObservation source group),
    epUpdateMode :: !PriorityUpdateMode,
    epNeedsScheduleTrace :: !Bool
  }

-- | The policy that observes nothing. O(1).
noEvidencePolicy :: EvidencePolicy source group
noEvidencePolicy =
  EvidencePolicy
    { epObserve = const emptyPriorityProfile,
      epUpdateMode = AccumulateDynamicPriority,
      epNeedsScheduleTrace = False
    }

data EvidenceProfiles group = EvidenceProfiles
  { epHasReplacement :: !Bool,
    epReplacementProfile :: !(PriorityProfile group),
    epAccumulatedProfile :: !(PriorityProfile group)
  }

-- | Fold all policies over one source and update the current profile. O(p·k)
-- for @p@ policies producing profiles of @k@ entries.
applyEvidencePolicies ::
  Ord group =>
  [EvidencePolicy source group] ->
  source ->
  PriorityProfile group ->
  PriorityProfile group
applyEvidencePolicies policies source currentProfile =
  let profiles =
        Foldable.foldl'
          collectEvidenceProfile
          EvidenceProfiles
            { epHasReplacement = False,
              epReplacementProfile = emptyPriorityProfile,
              epAccumulatedProfile = emptyPriorityProfile
            }
          policies
   in if epHasReplacement profiles
        then epReplacementProfile profiles <> epAccumulatedProfile profiles
        else currentProfile <> epAccumulatedProfile profiles
  where
    collectEvidenceProfile profiles policy =
      case epUpdateMode policy of
        AccumulateDynamicPriority ->
          profiles
            { epAccumulatedProfile =
                epAccumulatedProfile profiles <> epObserve policy source
            }
        ReplaceDynamicPriority ->
          profiles
            { epHasReplacement = True,
              epReplacementProfile =
                epReplacementProfile profiles <> epObserve policy source
            }
