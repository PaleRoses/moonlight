{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierProjectOperator,
    RuntimeCarrierRestrictOperator,
    RuntimeCarrierOperators (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Delta.Operator
  ( Operator,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectError,
    CarrierProjectState,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismError,
  )
import Moonlight.Flow.Model.Event
  ( LocalRelationalEvent,
  )
import Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )

type RuntimeCarrierProjectOperator :: Type -> Type -> Type -> Type -> Type
type RuntimeCarrierProjectOperator ctx prop boundary evidence =
  Operator
    (RuntimeEventTime ctx)
    (CarrierProjectState ctx prop boundary evidence)
    LocalRelationalEvent
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
    CarrierProjectError

type RuntimeCarrierRestrictOperator :: Type -> Type -> Type -> Type -> Type
type RuntimeCarrierRestrictOperator ctx prop boundary evidence =
  Operator
    (RuntimeEventTime ctx)
    (CarrierMorphismRuntime ctx Carrier prop boundary evidence)
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
    (CarrierMorphismError ctx Carrier prop boundary evidence)

type RuntimeCarrierOperators :: Type -> Type -> Type -> Type -> Type
data RuntimeCarrierOperators ctx prop boundary evidence = RuntimeCarrierOperators
  { rcoProjectOperator :: !(RuntimeCarrierProjectOperator ctx prop boundary evidence),
    rcoRestrictOperator :: !(RuntimeCarrierRestrictOperator ctx prop boundary evidence)
  }
