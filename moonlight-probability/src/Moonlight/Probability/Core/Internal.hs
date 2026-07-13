module Moonlight.Probability.Core.Internal
  ( Prob (..),
    PositiveProb (..),
    LogProb (..),
  )
where

import Data.Kind (Type)
import Numeric.Log (Log)
import Prelude

type Prob :: Type
newtype Prob = Prob
  { unProb :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type PositiveProb :: Type
newtype PositiveProb = PositiveProb
  { unPositiveProb :: Prob
  }
  deriving stock (Eq, Ord, Show)

type LogProb :: Type
newtype LogProb = LogProb (Log Double)
  deriving newtype (Eq, Ord, Show)
