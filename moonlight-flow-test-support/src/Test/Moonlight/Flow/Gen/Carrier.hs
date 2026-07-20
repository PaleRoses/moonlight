{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Gen.Carrier
  ( CarrierWorkload (..),
    CarrierInvalidWorkload (..),
    genValidCarrierWorkload,
    genInvalidCarrierWorkload,
  )
where

import Test.Moonlight.Flow.Workload (CarrierParams (..))
import Test.QuickCheck (Gen, chooseInt)

data CarrierWorkload = CarrierWorkload
  { cwInsertCount :: !Int,
    cwRetractCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data CarrierInvalidWorkload
  = RowMultiplicityUnderflowWorkload
  | CoverageMultiplicityUnderflowWorkload
  | FactContributionUnderflowWorkload
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

genValidCarrierWorkload :: CarrierParams -> Gen CarrierWorkload
genValidCarrierWorkload params = do
  insertCount <- chooseInt (1, max 1 (min 1024 (cpOperations params)))
  retractCount <- chooseInt (0, insertCount)
  pure CarrierWorkload {cwInsertCount = insertCount, cwRetractCount = retractCount}

genInvalidCarrierWorkload :: Gen CarrierInvalidWorkload
genInvalidCarrierWorkload = do
  tag <- chooseInt (0, 2 :: Int)
  pure $ case tag of
    0 -> RowMultiplicityUnderflowWorkload
    1 -> CoverageMultiplicityUnderflowWorkload
    _ -> FactContributionUnderflowWorkload
