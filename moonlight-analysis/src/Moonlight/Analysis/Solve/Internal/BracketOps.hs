module Moonlight.Analysis.Solve.Internal.BracketOps
  ( BracketState (..),
    mkBracketState,
    bracketMidpoint,
    updateBracketState,
    withinBracketState,
    clampToBracketState,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Solve.Internal.BracketSign (sameSign)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field (..),
    MultiplicativeMonoid (..),
  )
import Prelude

type BracketState :: Type -> Type
data BracketState a = BracketState
  { bracketLower :: !a,
    bracketLowerValue :: !a,
    bracketUpper :: !a,
    bracketUpperValue :: !a
  }
  deriving stock (Eq, Show)

mkBracketState :: a -> a -> a -> a -> BracketState a
mkBracketState lower lowerValue upper upperValue =
  BracketState
    { bracketLower = lower,
      bracketLowerValue = lowerValue,
      bracketUpper = upper,
      bracketUpperValue = upperValue
    }

bracketMidpoint :: (Field a) => BracketState a -> Maybe a
bracketMidpoint bracketState = midpoint (bracketLower bracketState) (bracketUpper bracketState)

updateBracketState :: (Ord a, AdditiveGroup a) => a -> a -> BracketState a -> BracketState a
updateBracketState candidate candidateValue bracketState =
  if sameSign candidateValue (bracketLowerValue bracketState)
    then
      BracketState
        { bracketLower = candidate,
          bracketLowerValue = candidateValue,
          bracketUpper = bracketUpper bracketState,
          bracketUpperValue = bracketUpperValue bracketState
        }
    else
      BracketState
        { bracketLower = bracketLower bracketState,
          bracketLowerValue = bracketLowerValue bracketState,
          bracketUpper = candidate,
          bracketUpperValue = candidateValue
        }

withinBracketState :: (Ord a) => BracketState a -> a -> Bool
withinBracketState bracketState value =
  bracketLower bracketState <= value && value <= bracketUpper bracketState

clampToBracketState :: (Ord a) => BracketState a -> a -> a
clampToBracketState bracketState value
  | value < bracketLower bracketState = bracketLower bracketState
  | value > bracketUpper bracketState = bracketUpper bracketState
  | otherwise = value

midpoint :: (Field a) => a -> a -> Maybe a
midpoint left right =
  tryDiv (add left right) (add one one)
