module Moonlight.Sheaf.Obstruction.Cohomological.Types.Capability
  ( ModalityCoverage (..),
    isCompleteModalityCoverage,
  )
where

import Data.Kind (Type)
type ModalityCoverage :: Type -> Type -> Type
data ModalityCoverage tag conflict = ModalityCoverage
  { smcMissingEnvironmentBindings :: ![tag],
    smcMissingRegisteredModalities :: ![tag],
    smcProjectionConflicts :: ![conflict]
  }
  deriving stock (Eq, Show, Read)

isCompleteModalityCoverage :: ModalityCoverage tag conflict -> Bool
isCompleteModalityCoverage coverage =
  null (smcMissingEnvironmentBindings coverage)
    && null (smcMissingRegisteredModalities coverage)
    && null (smcProjectionConflicts coverage)
