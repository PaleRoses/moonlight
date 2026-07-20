{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( RestrictionFailure (..),
    PropagationFailure (..),
    CohomologicalFailure (..),
    CarrierObstructionEvidence (..),
  )
where

import Data.Map.Strict
  ( Map,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Delta.Signed
  ( Multiplicity
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

data RestrictionFailure ctx carrier prop boundary = RestrictionFailure
  { rfSourceCarrier :: !(CarrierAddr ctx carrier prop),
    rfTargetCarrier :: !(CarrierAddr ctx carrier prop),
    rfFailedBoundary :: !boundary,
    rfDowngradeReason :: !String
  }
  deriving stock (Eq, Show)

data PropagationFailure ctx carrier prop = PropagationFailure
  { pfCarrier :: !(CarrierAddr ctx carrier prop),
    pfReason :: !String
  }
  deriving stock (Eq, Show)

data CohomologicalFailure ctx carrier prop boundary = CohomologicalFailure
  { cfCarrier :: !(CarrierAddr ctx carrier prop),
    cfBoundary :: !boundary,
    cfReason :: !String
  }
  deriving stock (Eq, Show)

data CarrierObstructionEvidence ctx carrier prop boundary evidence
  = StructuralMismatch
      !(CarrierAddr ctx carrier prop)
      !(CarrierAddr ctx carrier prop)
      !boundary
      !boundary
      !boundary
  | CarrierRowProjectionMismatch
      !ctx
      !boundary
      !RowTupleKey
  | CarrierMultiplicityMismatch
      !ctx
      !RowTupleKey
      !Multiplicity
      !Multiplicity
  | ContextBarrier
      !(CarrierAddr ctx carrier prop)
      !(CarrierAddr ctx carrier prop)
      !(Map RowTupleKey Multiplicity)
      !(Map RowTupleKey Multiplicity)
  | RestrictionBarrier
      !(RestrictionFailure ctx carrier prop boundary)
  | PropagationBarrier
      !(PropagationFailure ctx carrier prop)
  | CohomologicalObstruction
      !(CohomologicalFailure ctx carrier prop boundary)
  deriving stock (Eq, Show)
