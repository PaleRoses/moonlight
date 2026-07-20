{-# LANGUAGE DerivingStrategies #-}

-- | Posets viewed as thin categories: a morphism exists exactly where the
-- order relation holds.
module Moonlight.Category.Pure.Poset
  ( PosetCat (..),
    PosetOb (..),
    PosetMor,
    PosetTwoMor (..),
    PosetCompositor (..),
    mkPosetMor,
    posetSource,
    posetTarget,
    OrdinalLower (..),
    OrdinalUpper (..),
    LowerPosetCat,
    UpperPosetCat,
    LowerMor,
    UpperMor,
    mkLowerMor,
    mkUpperMor,
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Galois (GaloisConnection (..), OrdinalGalois (..))
import Moonlight.Category.Pure.Thin
  ( ThinMorphism,
    identityThinMorphism,
    mkThinMorphismBy,
    thinMorphismSource,
    thinMorphismTarget,
  )

type PosetCat :: Type -> Type
data PosetCat a = PosetCat
  deriving stock (Eq, Show)

type PosetOb :: Type -> Type
newtype PosetOb a = PosetOb {unPosetOb :: a}
  deriving stock (Eq, Ord, Show)

type PosetMor :: Type -> Type
data PosetMor a = PosetMor a a
  deriving stock (Eq, Show)

type PosetTwoMor :: Type -> Type
data PosetTwoMor (a :: Type) = PosetTwoMor
  deriving stock (Eq, Show)

type PosetCompositor :: Type -> Type
data PosetCompositor (a :: Type) = PosetCompositor
  deriving stock (Eq, Show)

mkPosetMor :: Ord a => a -> a -> Maybe (PosetMor a)
mkPosetMor sourceValue targetValue =
  fromThinMorphism <$> mkThinMorphismBy (<=) sourceValue targetValue

posetSource :: PosetMor a -> a
posetSource (PosetMor sourceValue _) = sourceValue

posetTarget :: PosetMor a -> a
posetTarget (PosetMor _ targetValue) = targetValue

instance Ord a => Category (PosetCat a) where
  type Ob (PosetCat a) = PosetOb a
  type Mor (PosetCat a) = PosetMor a
  type TwoMor (PosetCat a) = PosetTwoMor a
  type Compositor (PosetCat a) = PosetCompositor a

  identity _ (PosetOb objectValue) =
    Right (fromThinMorphism (identityThinMorphism objectValue))

  compose _ leftMorphism rightMorphism
    | posetTarget rightMorphism /= posetSource leftMorphism = Left ()
    | otherwise =
        case mkPosetMor (posetSource rightMorphism) (posetTarget leftMorphism) of
          Just composedMorphism -> Right (composedMorphism, PosetCompositor)
          Nothing -> Left ()

  source _ = Right . PosetOb . posetSource
  target _ = Right . PosetOb . posetTarget

type OrdinalLower :: Type
newtype OrdinalLower = OrdinalLower {unOrdinalLower :: Int}
  deriving stock (Eq, Ord, Show)

type OrdinalUpper :: Type
newtype OrdinalUpper = OrdinalUpper {unOrdinalUpper :: Int}
  deriving stock (Eq, Ord, Show)

instance GaloisConnection OrdinalLower OrdinalUpper where
  alpha (OrdinalLower value) = OrdinalUpper (value * 2)
  gamma (OrdinalUpper value) = OrdinalLower (value `div` 2)

instance OrdinalGalois OrdinalLower OrdinalUpper where
  thresholds = map (\value -> (OrdinalLower value, OrdinalUpper (value * 2))) [0 .. 32]

type LowerPosetCat :: Type
type LowerPosetCat = PosetCat OrdinalLower

type UpperPosetCat :: Type
type UpperPosetCat = PosetCat OrdinalUpper

type LowerMor :: Type
type LowerMor = PosetMor OrdinalLower

type UpperMor :: Type
type UpperMor = PosetMor OrdinalUpper

mkLowerMor :: OrdinalLower -> OrdinalLower -> Maybe LowerMor
mkLowerMor = mkPosetMor

mkUpperMor :: OrdinalUpper -> OrdinalUpper -> Maybe UpperMor
mkUpperMor = mkPosetMor


fromThinMorphism :: ThinMorphism a -> PosetMor a
fromThinMorphism thinMorphism =
  PosetMor
    (thinMorphismSource thinMorphism)
    (thinMorphismTarget thinMorphism)
