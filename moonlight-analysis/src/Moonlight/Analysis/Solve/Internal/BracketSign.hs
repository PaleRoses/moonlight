module Moonlight.Analysis.Solve.Internal.BracketSign
  ( sameSign,
    bracketContainsRootFromValues,
  )
where

import Moonlight.Core (AdditiveGroup, AdditiveMonoid (..))
import Prelude

sameSign :: (Ord a, AdditiveGroup a) => a -> a -> Bool
sameSign left right =
  (left > zero && right > zero) || (left < zero && right < zero)

bracketContainsRootFromValues :: (Ord a, AdditiveGroup a) => a -> a -> Bool
bracketContainsRootFromValues leftValue rightValue =
  leftValue == zero || rightValue == zero || not (sameSign leftValue rightValue)
