
module Moonlight.Analysis.Dual
  ( Dual (..),
    Elementary (..),
    liftDual,
    mapDual,
    primalValue,
    tangentValue,
    diff,
    derivative,
    sinDual,
    cosDual,
    expDual,
    tryLogDual,
    trySqrtDual,
    tryPowDual,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra (Semiring)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field (..),
    Magnitude,
    Metric (..),
    MultiplicativeMonoid (..),
    Ring,
  )
import Prelude hiding (exp, log, sin, sqrt)
import qualified Prelude

type Dual :: Type -> Type -> Type
data Dual s a = Dual
  { primal :: !a,
    tangent :: !a
  }
  deriving stock (Eq, Show)

type Elementary :: Type -> Constraint
class Field a => Elementary a where
  elementarySin :: a -> a
  elementaryCos :: a -> a
  elementaryExp :: a -> a
  tryElementaryLog :: a -> Maybe a
  tryElementarySqrt :: a -> Maybe a
  tryElementaryPow :: a -> a -> Maybe a

instance Elementary Double where
  elementarySin = Prelude.sin
  elementaryCos = Prelude.cos
  elementaryExp = Prelude.exp
  tryElementaryLog value
    | fieldValueValid value && value > 0.0 = Just (Prelude.log value)
    | otherwise = Nothing
  tryElementarySqrt value
    | fieldValueValid value && value >= 0.0 = Just (Prelude.sqrt value)
    | otherwise = Nothing
  tryElementaryPow base exponentValue
    | fieldValueValid base && fieldValueValid exponentValue && base > 0.0 =
        let powered = base Prelude.** exponentValue
         in if fieldValueValid powered then Just powered else Nothing
    | otherwise = Nothing

instance Elementary Float where
  elementarySin = Prelude.sin
  elementaryCos = Prelude.cos
  elementaryExp = Prelude.exp
  tryElementaryLog value
    | fieldValueValid value && value > 0.0 = Just (Prelude.log value)
    | otherwise = Nothing
  tryElementarySqrt value
    | fieldValueValid value && value >= 0.0 = Just (Prelude.sqrt value)
    | otherwise = Nothing
  tryElementaryPow base exponentValue
    | fieldValueValid base && fieldValueValid exponentValue && base > 0.0 =
        let powered = base Prelude.** exponentValue
         in if fieldValueValid powered then Just powered else Nothing
    | otherwise = Nothing

instance AdditiveMonoid a => AdditiveMonoid (Dual s a) where
  zero = Dual zero zero
  add (Dual leftPrimal leftTangent) (Dual rightPrimal rightTangent) =
    Dual (add leftPrimal rightPrimal) (add leftTangent rightTangent)

instance AdditiveGroup a => AdditiveGroup (Dual s a) where
  neg (Dual primalValue' tangentValue') = Dual (neg primalValue') (neg tangentValue')

instance (Ring a) => MultiplicativeMonoid (Dual s a) where
  one = Dual one zero
  mul (Dual leftPrimal leftTangent) (Dual rightPrimal rightTangent) =
    Dual
      (mul leftPrimal rightPrimal)
      (add (mul leftPrimal rightTangent) (mul leftTangent rightPrimal))

instance (Ring a) => Ring (Dual s a)

instance (Field a) => Field (Dual s a) where
  tryInv (Dual primalValue' tangentValue') =
    case tryInv primalValue' of
      Nothing -> Nothing
      Just inversePrimal ->
        let inverseSquared = mul inversePrimal inversePrimal
         in Just (Dual inversePrimal (neg (mul tangentValue' inverseSquared)))

instance (Ring a) => Semiring (Dual s a)

instance (Metric a) => Metric (Dual s a) where
  type Magnitude (Dual s a) = Magnitude a
  magnitude = magnitude . primal

instance (Elementary a) => Elementary (Dual s a) where
  elementarySin (Dual primalValue' tangentValue') =
    Dual (elementarySin primalValue') (mul tangentValue' (elementaryCos primalValue'))
  elementaryCos (Dual primalValue' tangentValue') =
    Dual (elementaryCos primalValue') (neg (mul tangentValue' (elementarySin primalValue')))
  elementaryExp (Dual primalValue' tangentValue') =
    let result = elementaryExp primalValue'
     in Dual result (mul tangentValue' result)
  tryElementaryLog (Dual primalValue' tangentValue') =
    case (tryElementaryLog primalValue', tryDiv tangentValue' primalValue') of
      (Just logarithmValue, Just tangentScale) -> Just (Dual logarithmValue tangentScale)
      _ -> Nothing
  tryElementarySqrt (Dual primalValue' tangentValue') =
    case tryElementarySqrt primalValue' of
      Nothing -> Nothing
      Just root ->
        case tryDiv tangentValue' (add root root) of
          Just tangentScale -> Just (Dual root tangentScale)
          Nothing -> Nothing
  tryElementaryPow (Dual base baseTangent) (Dual exponentValue exponentTangent) =
    case (tryElementaryPow base exponentValue, tryElementaryLog base, tryDiv (mul exponentValue baseTangent) base) of
      (Just powered, Just logarithmValue, Just rationalTerm) ->
        let logarithmicTerm = mul exponentTangent logarithmValue
         in Just (Dual powered (mul powered (add logarithmicTerm rationalTerm)))
      _ -> Nothing

liftDual :: (AdditiveGroup a) => a -> Dual s a
liftDual value = Dual value zero

mapDual :: (a -> b) -> (a -> b) -> Dual s a -> Dual s b
mapDual mapPrimal mapTangent dualValue =
  Dual (mapPrimal (primal dualValue)) (mapTangent (tangent dualValue))

primalValue :: Dual s a -> a
primalValue = primal

tangentValue :: Dual s a -> a
tangentValue = tangent

diff :: (Field a) => (forall s. Dual s a -> Dual s a) -> a -> (a, a)
diff function value =
  let Dual primalResult tangentResult = function (Dual value one)
   in (primalResult, tangentResult)

derivative :: (Field a) => (forall s. Dual s a -> Dual s a) -> a -> a
derivative function value = snd (diff function value)

sinDual :: (Elementary a) => Dual s a -> Dual s a
sinDual = elementarySin

cosDual :: (Elementary a) => Dual s a -> Dual s a
cosDual = elementaryCos

expDual :: (Elementary a) => Dual s a -> Dual s a
expDual = elementaryExp

tryLogDual :: (Elementary a) => Dual s a -> Maybe (Dual s a)
tryLogDual = tryElementaryLog

trySqrtDual :: (Elementary a) => Dual s a -> Maybe (Dual s a)
trySqrtDual = tryElementarySqrt

tryPowDual :: (Elementary a) => Dual s a -> Dual s a -> Maybe (Dual s a)
tryPowDual = tryElementaryPow
