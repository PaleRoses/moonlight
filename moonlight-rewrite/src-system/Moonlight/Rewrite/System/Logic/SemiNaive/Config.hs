{-# LANGUAGE DerivingStrategies #-}

-- | Configuration and accounting for semi-naive fact closure.
-- Owns round-retention policy, fact/derivation/round limits, limit
-- obstructions, and retained-round accumulation.
-- Contracts: the default keeps all rounds with no limits, and limit checks
-- compare accumulated closure stats rather than just current deltas.
module Moonlight.Rewrite.System.Logic.SemiNaive.Config
  ( RoundRetention (..),
    FactClosureLimits (..),
    noFactClosureLimits,
    SemiNaiveConfig (..),
    defaultSemiNaiveConfig,
    FactClosureStats (..),
    FactClosureLimit (..),
    FactClosureRunError (..),
    checkFactClosureLimits,
    RoundAccumulator,
    emptyRoundAccumulator,
    recordRetainedRound,
    retainedRounds,
  )
where

import Data.Foldable (asum)
import Data.Kind (Type)
import Moonlight.Control.Count
  ( naturalToBoundedInt,
  )
import Numeric.Natural (Natural)

type RoundRetention :: Type
data RoundRetention
  = KeepNoRounds
  | KeepAllRounds
  | KeepRecentRounds !Natural
  deriving stock (Eq, Ord, Show, Read)

type FactClosureLimits :: Type
data FactClosureLimits = FactClosureLimits
  { fclMaxRounds :: !(Maybe Natural),
    fclMaxFacts :: !(Maybe Natural),
    fclMaxDerivations :: !(Maybe Natural)
  }
  deriving stock (Eq, Ord, Show, Read)

noFactClosureLimits :: FactClosureLimits
noFactClosureLimits =
  FactClosureLimits
    { fclMaxRounds = Nothing,
      fclMaxFacts = Nothing,
      fclMaxDerivations = Nothing
    }

type SemiNaiveConfig :: Type
data SemiNaiveConfig = SemiNaiveConfig
  { sncRoundRetention :: !RoundRetention,
    sncLimits :: !FactClosureLimits
  }
  deriving stock (Eq, Ord, Show, Read)

defaultSemiNaiveConfig :: SemiNaiveConfig
defaultSemiNaiveConfig =
  SemiNaiveConfig
    { sncRoundRetention = KeepAllRounds,
      sncLimits = noFactClosureLimits
    }

type FactClosureStats :: Type
data FactClosureStats = FactClosureStats
  { fcsRoundsCompleted :: {-# UNPACK #-} !Int,
    fcsFactCount :: {-# UNPACK #-} !Int,
    fcsDeltaFactCount :: {-# UNPACK #-} !Int,
    fcsDerivationCount :: {-# UNPACK #-} !Int,
    fcsDeltaDerivationCount :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type FactClosureLimit :: Type
data FactClosureLimit
  = MaxRoundsExceeded !Natural
  | MaxFactsExceeded !Natural
  | MaxDerivationsExceeded !Natural
  deriving stock (Eq, Ord, Show, Read)

type FactClosureRunError :: Type -> Type
data FactClosureRunError obstruction
  = FactClosureMatcherError !obstruction
  | FactClosureLimitExceeded !FactClosureLimit !FactClosureStats
  deriving stock (Eq, Ord, Show, Read)

checkFactClosureLimits ::
  FactClosureLimits ->
  FactClosureStats ->
  Either (FactClosureRunError obstruction) ()
checkFactClosureLimits limits stats =
  case asum checks of
    Nothing ->
      Right ()
    Just exceeded ->
      Left (FactClosureLimitExceeded exceeded stats)
  where
    checks =
      [ exceededNatural
          (MaxRoundsExceeded <$> fclMaxRounds limits)
          (fcsRoundsCompleted stats),
        exceededNatural
          (MaxFactsExceeded <$> fclMaxFacts limits)
          (fcsFactCount stats),
        exceededNatural
          (MaxDerivationsExceeded <$> fclMaxDerivations limits)
          (fcsDerivationCount stats)
      ]

exceededNatural ::
  Maybe FactClosureLimit ->
  Int ->
  Maybe FactClosureLimit
exceededNatural maybeLimit observed =
  case maybeLimit of
    Nothing ->
      Nothing
    Just limitError ->
      if intToNatural observed > limitBound limitError
        then Just limitError
        else Nothing

limitBound :: FactClosureLimit -> Natural
limitBound limit =
  case limit of
    MaxRoundsExceeded bound ->
      bound
    MaxFactsExceeded bound ->
      bound
    MaxDerivationsExceeded bound ->
      bound

intToNatural :: Int -> Natural
intToNatural value
  | value <= 0 =
      0
  | otherwise =
      fromIntegral value

type RoundAccumulator :: Type -> Type
data RoundAccumulator round = RoundAccumulator
  { raRetention :: !RoundRetention,
    raNewestFirst :: ![round]
  }

emptyRoundAccumulator :: RoundRetention -> RoundAccumulator round
emptyRoundAccumulator retention =
  RoundAccumulator
    { raRetention = retention,
      raNewestFirst = []
    }

recordRetainedRound ::
  round ->
  RoundAccumulator round ->
  RoundAccumulator round
recordRetainedRound roundValue accumulator =
  case raRetention accumulator of
    KeepNoRounds ->
      accumulator
    KeepAllRounds ->
      accumulator
        { raNewestFirst = roundValue : raNewestFirst accumulator
        }
    KeepRecentRounds bound ->
      accumulator
        { raNewestFirst =
            take
              (naturalToBoundedInt bound)
              (roundValue : raNewestFirst accumulator)
        }

retainedRounds :: RoundAccumulator round -> [round]
retainedRounds accumulator =
  reverse (raNewestFirst accumulator)
