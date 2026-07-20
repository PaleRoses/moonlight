{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Core.Types
  ( CarrierMorphismPlan (..),
    CarrierMorphism (..),
    CarrierMorphismState (..),
    emptyCarrierMorphismState,
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
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( RelationalOrigin,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type CarrierMorphismPlan :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismPlan profile ctx carrier prop boundary = CarrierMorphismPlan
  { cmpTarget :: !(CarrierAddr ctx carrier prop),
    cmpBoundary :: !boundary,
    cmpProfile :: !profile
  }

type CarrierMorphism :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphism profile ctx carrier prop boundary evidence err = CarrierMorphism
  { cmPrepare ::
      RelationalCarrierDelta ctx carrier prop boundary evidence ->
      Either err (CarrierMorphismPlan profile ctx carrier prop boundary),
    cmRows ::
      profile ->
      RowDelta ->
      Either err RowDelta,
    cmTime ::
      RelationalCarrierTime ctx ->
      RelationalCarrierTime ctx,
    cmSupport ::
      profile ->
      SupportBasis ctx ->
      Either err (SupportBasis ctx),
    cmEvidence ::
      profile ->
      evidence ->
      Either err evidence,
    cmOrigin ::
      RelationalOrigin ctx carrier prop ->
      RelationalOrigin ctx carrier prop,
    cmScope ::
      RelationalScope ->
      RelationalScope
  }

type CarrierMorphismState :: Type
data CarrierMorphismState = CarrierMorphismState
  deriving stock (Eq, Ord, Show, Read)

emptyCarrierMorphismState :: CarrierMorphismState
emptyCarrierMorphismState =
  CarrierMorphismState
{-# INLINE emptyCarrierMorphismState #-}
