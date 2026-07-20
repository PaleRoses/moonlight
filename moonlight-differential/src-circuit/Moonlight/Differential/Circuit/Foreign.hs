{-# LANGUAGE StandaloneKindSignatures #-}

-- | Foreign node contract: a caller-owned Mealy delta rule paired with the
-- eager denotation it is obligated to agree with; the substrate never
-- verifies the obligation, it only states and harnesses it.
module Moonlight.Differential.Circuit.Foreign
  ( ForeignKernel (..),
    ForeignKernel2 (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Algebra.ZSet
  ( ZSet,
  )

type ForeignKernel :: Type -> Type -> Type -> Type -> Type
data ForeignKernel fault weight a b = ForeignKernel
  { foreignStep ::
      ZSet a weight ->
      Either fault (ZSet b weight, ForeignKernel fault weight a b),
    foreignDenote ::
      ZSet a weight ->
      ZSet b weight
  }

type ForeignKernel2 :: Type -> Type -> Type -> Type -> Type -> Type
data ForeignKernel2 fault weight a b c = ForeignKernel2
  { foreignStep2 ::
      ZSet a weight ->
      ZSet b weight ->
      Either fault (ZSet c weight, ForeignKernel2 fault weight a b c),
    foreignDenote2 ::
      ZSet a weight ->
      ZSet b weight ->
      ZSet c weight
  }
