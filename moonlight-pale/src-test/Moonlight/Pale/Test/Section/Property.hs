module Moonlight.Pale.Test.Section.Property
  ( etaQuickCheck,
    etaHedgehog,
  )
where

import qualified Hedgehog as HH
import Prelude (Bool, Show, ($))
import qualified Test.Tasty.QuickCheck as QC

etaQuickCheck :: QC.Testable prop => prop -> QC.Property
etaQuickCheck = QC.property

etaHedgehog :: Show a => HH.Gen a -> (a -> Bool) -> HH.Property
etaHedgehog generator predicate = HH.property $ do
  value <- HH.forAll generator
  HH.assert (predicate value)
