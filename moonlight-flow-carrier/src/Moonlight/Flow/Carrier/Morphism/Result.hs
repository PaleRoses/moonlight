{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismOutput (..),
    CarrierMorphismDiagnostic (..),
    CarrierMorphismError (..),
    emptyCarrierMorphismOutput,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationError,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionDiagnostic,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuseError,
  )

type CarrierMorphismOutput :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismOutput ctx carrier prop boundary evidence = CarrierMorphismOutput
  { cmoEmitted ::
      ![RelationalCarrierDelta ctx carrier prop boundary evidence],
    cmoDiagnostics ::
      ![CarrierMorphismDiagnostic ctx carrier prop boundary evidence]
  }
  deriving stock (Eq, Show)

type CarrierMorphismDiagnostic :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismDiagnostic ctx carrier prop boundary evidence
  = CarrierMorphismRestrictionDiagnostic
      !(CarrierRestrictionDiagnostic ctx carrier prop)
  | CarrierMorphismReuseDiagnostic
      !(CarrierReuseError ctx prop evidence)
  | CarrierMorphismAmalgamationDiagnostic
      !(AmalgamationError ctx carrier prop boundary evidence)
  | CarrierMorphismSuppressedEmpty
      !(CarrierAddr ctx carrier prop)
  deriving stock (Eq, Show)

type CarrierMorphismError :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismError ctx carrier prop boundary evidence
  = CarrierMorphismRestrictionError
      !(CarrierRestrictionDiagnostic ctx carrier prop)
  | CarrierMorphismReuseError
      !(CarrierReuseError ctx prop evidence)
  | CarrierMorphismAmalgamationError
      !(AmalgamationError ctx carrier prop boundary evidence)
  deriving stock (Eq, Show)

emptyCarrierMorphismOutput ::
  CarrierMorphismOutput ctx carrier prop boundary evidence
emptyCarrierMorphismOutput =
  CarrierMorphismOutput
    { cmoEmitted = [],
      cmoDiagnostics = []
    }
{-# INLINE emptyCarrierMorphismOutput #-}
