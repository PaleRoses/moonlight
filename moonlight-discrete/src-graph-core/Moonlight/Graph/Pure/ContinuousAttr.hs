module Moonlight.Graph.Pure.ContinuousAttr
  ( ContinuousAttr (..),
    ContinuousDelta (MkContinuousDelta, continuousDeltaAdd, continuousDeltaMul),
    materializeAttr,
    applyContinuousDelta,
    foldDeltasByKey,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moonlight.Algebra (Action (..))
import Moonlight.Graph.Pure.Types (AttrKey)

type ContinuousAttr :: Type
data ContinuousAttr = ContinuousAttr
  { continuousBase :: Double,
    continuousPendingAdd :: Double,
    continuousPendingMul :: Double
  }
  deriving stock (Eq, Show)

type ContinuousDelta :: Type
data ContinuousDelta = MkContinuousDelta
  { continuousDeltaAdd :: Double,
    continuousDeltaMul :: Double
  }
  deriving stock (Eq, Show)

materializeAttr :: ContinuousAttr -> Double
materializeAttr continuousAttr =
  (continuousBase continuousAttr + continuousPendingAdd continuousAttr)
    * continuousPendingMul continuousAttr

applyContinuousDelta :: ContinuousDelta -> ContinuousAttr -> ContinuousAttr
applyContinuousDelta continuousDelta continuousAttr =
  continuousAttr
    { continuousPendingAdd =
        continuousPendingAdd continuousAttr + continuousDeltaAdd continuousDelta,
      continuousPendingMul =
        continuousPendingMul continuousAttr * continuousDeltaMul continuousDelta
    }

foldDeltasByKey :: Foldable f => f (AttrKey, ContinuousDelta) -> Map AttrKey ContinuousDelta
foldDeltasByKey =
  foldr
    (\(attrKey, continuousDelta) -> Map.insertWith (<>) attrKey continuousDelta)
    Map.empty

instance Semigroup ContinuousDelta where
  (<>) leftDelta rightDelta =
    MkContinuousDelta
      { continuousDeltaAdd =
          continuousDeltaAdd leftDelta + continuousDeltaAdd rightDelta,
        continuousDeltaMul =
          continuousDeltaMul leftDelta * continuousDeltaMul rightDelta
      }


instance Monoid ContinuousDelta where
  mempty =
    MkContinuousDelta
      { continuousDeltaAdd = 0.0,
        continuousDeltaMul = 1.0
      }

instance Action ContinuousDelta ContinuousAttr where
  act = applyContinuousDelta
