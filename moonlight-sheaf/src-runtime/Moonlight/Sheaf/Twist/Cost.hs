module Moonlight.Sheaf.Twist.Cost
  ( CostOverlay (..),
    costOverlay,
    constantCostOverlay,
    guardedCostOverlay,
    applyCostOverlay,
  )
where

import Data.Kind (Type)

type CostOverlay :: Type -> Type -> Type
newtype CostOverlay ctx algebra = CostOverlay
  { runCostOverlay :: ctx -> algebra -> algebra
  }

instance Semigroup (CostOverlay ctx algebra) where
  leftOverlay <> rightOverlay =
    CostOverlay
      (\contextValue -> runCostOverlay leftOverlay contextValue . runCostOverlay rightOverlay contextValue)

instance Monoid (CostOverlay ctx algebra) where
  mempty = CostOverlay (const id)

costOverlay :: (ctx -> algebra -> algebra) -> CostOverlay ctx algebra
costOverlay =
  CostOverlay

constantCostOverlay :: (algebra -> algebra) -> CostOverlay ctx algebra
constantCostOverlay overlayFunction =
  CostOverlay (const overlayFunction)

guardedCostOverlay :: (ctx -> Bool) -> (algebra -> algebra) -> CostOverlay ctx algebra
guardedCostOverlay predicate overlayFunction =
  CostOverlay (\contextValue -> if predicate contextValue then overlayFunction else id)

applyCostOverlay :: ctx -> CostOverlay ctx algebra -> algebra -> algebra
applyCostOverlay contextValue overlayValue =
  runCostOverlay overlayValue contextValue
