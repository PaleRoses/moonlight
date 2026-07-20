{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Workload
  ( WorkloadTier (..),
    WorkloadParams (..),
    CarrierParams (..),
    PlanParams (..),
    FactDistParams (..),
    KeySkew (..),
    MultiplicityProfile (..),
    unitParams,
    smallParams,
    mediumParams,
    largeParams,
    soakParams,
    carrierParams,
    planParams,
    factDistParams,
  )
where

import Data.Kind (Type)

-- | Named size tier.  Tests must carry this value so toy fixtures cannot
-- impersonate load coverage.
type WorkloadTier :: Type
data WorkloadTier
  = UnitTier
  | SmallTier
  | MediumTier
  | LargeTier
  | SoakTier
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type CarrierParams :: Type
data CarrierParams = CarrierParams
  { cpOperations :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanParams :: Type
data PlanParams = PlanParams
  { ppAtomCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type KeySkew :: Type
data KeySkew
  = UniformKeys
  | HotKeySkew
  deriving stock (Eq, Ord, Show, Read)

type MultiplicityProfile :: Type
data MultiplicityProfile
  = SetSemantics
  | SignedMultiplicity
  deriving stock (Eq, Ord, Show, Read)

type FactDistParams :: Type
data FactDistParams = FactDistParams
  { fdpInputRows :: !Int,
    fdpMaxOutputRows :: !Int,
    fdpMaxJoinArity :: !Int,
    fdpKeySkew :: !KeySkew,
    fdpMultiplicityProfile :: !MultiplicityProfile
  }
  deriving stock (Eq, Ord, Show, Read)


type WorkloadParams :: Type
data WorkloadParams = WorkloadParams
  { wpTier :: !WorkloadTier,
    wpCarrier :: !CarrierParams,
    wpPlan :: !PlanParams,
    wpFacts :: !FactDistParams
  }
  deriving stock (Eq, Ord, Show, Read)

unitParams :: WorkloadParams
unitParams =
  WorkloadParams
    { wpTier = UnitTier,
      wpCarrier = CarrierParams 100,
      wpPlan = PlanParams 3,
      wpFacts = FactDistParams 1_000 1_000 3 UniformKeys SetSemantics
    }

smallParams :: WorkloadParams
smallParams =
  WorkloadParams
    { wpTier = SmallTier,
      wpCarrier = CarrierParams 1_000,
      wpPlan = PlanParams 5,
      wpFacts = FactDistParams 10_000 10_000 4 UniformKeys SetSemantics
    }

mediumParams :: WorkloadParams
mediumParams =
  WorkloadParams
    { wpTier = MediumTier,
      wpCarrier = CarrierParams 10_000,
      wpPlan = PlanParams 10,
      wpFacts = FactDistParams 100_000 100_000 6 HotKeySkew SignedMultiplicity
    }

largeParams :: WorkloadParams
largeParams =
  WorkloadParams
    { wpTier = LargeTier,
      wpCarrier = CarrierParams 100_000,
      wpPlan = PlanParams 20,
      wpFacts = FactDistParams 1_000_000 100_000 8 HotKeySkew SignedMultiplicity
    }

soakParams :: WorkloadParams
soakParams =
  WorkloadParams
    { wpTier = SoakTier,
      wpCarrier = CarrierParams 1_000_000,
      wpPlan = PlanParams 50,
      wpFacts = FactDistParams 10_000_000 100_000 12 HotKeySkew SignedMultiplicity
    }

carrierParams :: WorkloadParams -> CarrierParams
carrierParams = wpCarrier

planParams :: WorkloadParams -> PlanParams
planParams = wpPlan

factDistParams :: WorkloadParams -> FactDistParams
factDistParams = wpFacts
