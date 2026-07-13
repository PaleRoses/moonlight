module PriorityProfileConstructor where

import Data.Map.Strict qualified as Map
import Moonlight.Control.Weight
  ( PriorityProfile,
  )

bad :: PriorityProfile Int
bad =
  PriorityProfile Map.empty
