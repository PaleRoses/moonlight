{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | The unification seam of the algebra: a 'Modality' is the modal context
-- scoped over program regions by 'Moonlight.Control.Class.scoped' — the
-- product of a guidance 'Gate' and a scheduling-weight 'PriorityProfile',
-- each already a lawful monoid. Scoping with a composed modality is
-- equal by the monoid-action law to scoping twice:
-- @'scoped' (a '<>' b) = 'scoped' a . 'scoped' b@.
module Moonlight.Control.Modality
  ( Modality (..),
    gateContext,
    weightContext,
    gated,
    weighted,
    gateIsUnit,
  )
where

import Moonlight.Control.Class (Control (..))
import Moonlight.Control.Gate
  ( Gate (..),
    MatchSelector (..),
    noGate,
  )
import Moonlight.Control.Weight
  ( PriorityProfile,
    emptyPriorityProfile,
  )

-- | A guidance gate and a scheduling weight, composed componentwise.
data Modality view group match traceEntry schedulerKey = Modality
  { modalityGate :: !(Gate view group match traceEntry schedulerKey),
    modalityWeight :: !(PriorityProfile schedulerKey)
  }

instance Ord schedulerKey => Semigroup (Modality view group match traceEntry schedulerKey) where
  leftModality <> rightModality =
    Modality
      { modalityGate = modalityGate leftModality <> modalityGate rightModality,
        modalityWeight = modalityWeight leftModality <> modalityWeight rightModality
      }

instance Ord schedulerKey => Monoid (Modality view group match traceEntry schedulerKey) where
  mempty =
    Modality
      { modalityGate = noGate,
        modalityWeight = emptyPriorityProfile
      }

-- | A modality carrying only a gate. O(1).
gateContext ::
  Gate view group match traceEntry schedulerKey ->
  Modality view group match traceEntry schedulerKey
gateContext gate =
  Modality
    { modalityGate = gate,
      modalityWeight = emptyPriorityProfile
    }

-- | A modality carrying only a scheduling weight. O(1).
weightContext ::
  PriorityProfile schedulerKey ->
  Modality view group match traceEntry schedulerKey
weightContext weight =
  Modality
    { modalityGate = noGate,
      modalityWeight = weight
    }

-- | Scope a guidance gate over a program region. O(1).
gated ::
  (Control c, ContextOf c ~ Modality view group match traceEntry schedulerKey) =>
  Gate view group match traceEntry schedulerKey ->
  c ->
  c
gated gate = scoped (gateContext gate)

-- | Scope a scheduling weight over a program region. O(1).
weighted ::
  (Control c, ContextOf c ~ Modality view group match traceEntry schedulerKey) =>
  PriorityProfile schedulerKey ->
  c ->
  c
weighted weight = scoped (weightContext weight)

-- | Whether a gate is representationally the unit: an unnamed selector that
-- preserves counts. True for 'noGate' and 'mempty'; validation behaviour is
-- not inspected. O(1).
gateIsUnit :: Gate view group match traceEntry schedulerKey -> Bool
gateIsUnit gate =
  matchSelectorPreservesCount (gateSelector gate)
    && null (matchSelectorName (gateSelector gate))
