{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding (..),
    RuntimeFactorState (..),
    emptyRuntimeFactorState,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Moonlight.Core
  ( QueryId,
    SlotId,
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairStats,
    emptyRuntimeRepairStats,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )

type RuntimeQueryBinding :: Type
data RuntimeQueryBinding = RuntimeQueryBinding
  { rqbRepairKey :: !RepairProgramKey,
    rqbFullSchema :: ![SlotId],
    rqbOutputSlots :: ![SlotId]
  }
  deriving stock (Eq, Show)

type RuntimeFactorState :: Type
data RuntimeFactorState = RuntimeFactorState
  { rfsPrograms :: !(Map RepairProgramKey FactorProgram),
    rfsQueryBindings :: !(Map QueryId RuntimeQueryBinding),
    rfsRepairStats :: !RuntimeRepairStats
  }

emptyRuntimeFactorState ::
  Map RepairProgramKey FactorProgram ->
  Map QueryId RuntimeQueryBinding ->
  RuntimeFactorState
emptyRuntimeFactorState programs queryBindings =
  RuntimeFactorState
    { rfsPrograms = programs,
      rfsQueryBindings = queryBindings,
      rfsRepairStats = emptyRuntimeRepairStats
    }
{-# INLINE emptyRuntimeFactorState #-}
