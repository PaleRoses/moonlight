{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind (..),
    IncidenceCoefficient,
    mkIncidenceCoefficient,
    incidenceCoefficientValue,
    mkIncidenceRestriction,
    unitIncidenceRestriction,
    negativeUnitIncidenceRestriction,
    restrictionKindCoefficient,
    isIncidenceRestriction,
    isPortalRestriction,
    RestrictionArrow (..),
    composeRestrictionArrow,
    RestrictionId (..),
    Restriction (..),
    RestrictionParts (..),
    RestrictionPresentation,
    restrictionArrow,
    restrictApply,
    RestrictionCheck (..),
    checkRestriction,
  )
where

import Data.Kind (Type)
import Data.Maybe (isJust)
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    restrictStalk,
    stalkMismatches,
  )

type RestrictionKind :: Type
data RestrictionKind
  = IncidenceRestriction !IncidenceCoefficient
  | PortalRestriction
  deriving stock (Eq, Ord, Show)

type IncidenceCoefficient :: Type
newtype IncidenceCoefficient = UnsafeIncidenceCoefficient
  { incidenceCoefficientValue :: Int
  }
  deriving stock (Eq, Ord, Show)

mkIncidenceCoefficient :: Int -> Maybe IncidenceCoefficient
mkIncidenceCoefficient coefficient
  | coefficient == 0 = Nothing
  | otherwise = Just (UnsafeIncidenceCoefficient coefficient)

mkIncidenceRestriction :: Int -> Maybe RestrictionKind
mkIncidenceRestriction =
  fmap IncidenceRestriction . mkIncidenceCoefficient

unitIncidenceRestriction :: RestrictionKind
unitIncidenceRestriction =
  IncidenceRestriction (UnsafeIncidenceCoefficient 1)

negativeUnitIncidenceRestriction :: RestrictionKind
negativeUnitIncidenceRestriction =
  IncidenceRestriction (UnsafeIncidenceCoefficient (-1))

restrictionKindCoefficient :: RestrictionKind -> Maybe Int
restrictionKindCoefficient (IncidenceRestriction coefficient) = Just (incidenceCoefficientValue coefficient)
restrictionKindCoefficient PortalRestriction = Nothing

isIncidenceRestriction :: RestrictionKind -> Bool
isIncidenceRestriction = isJust . restrictionKindCoefficient

isPortalRestriction :: RestrictionKind -> Bool
isPortalRestriction = not . isIncidenceRestriction

type RestrictionArrow :: Type -> Type
data RestrictionArrow cell = RestrictionArrow
  { restrictFrom :: !cell,
    restrictTo :: !cell
  }
  deriving stock (Eq, Ord, Show, Read)

composeRestrictionArrow ::
  Eq cell =>
  RestrictionArrow cell ->
  RestrictionArrow cell ->
  Maybe (RestrictionArrow cell)
composeRestrictionArrow firstArrow secondArrow
  | restrictTo firstArrow == restrictFrom secondArrow =
      Just
        RestrictionArrow
          { restrictFrom = restrictFrom firstArrow,
            restrictTo = restrictTo secondArrow
          }
  | otherwise =
      Nothing

type RestrictionId :: Type
newtype RestrictionId = RestrictionId
  { unRestrictionId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type Restriction :: Type -> Type -> Type
data Restriction cell witness = Restriction
  { rId :: !RestrictionId,
    rKind :: !RestrictionKind,
    rSource :: !cell,
    rTarget :: !cell,
    rWitness :: !witness
  }
  deriving stock (Eq, Show)

type RestrictionParts :: Type -> Type -> Type
data RestrictionParts cell witness = RestrictionParts
  { partKind :: !RestrictionKind,
    partSource :: !cell,
    partTarget :: !cell,
    partWitness :: !witness
  }

type RestrictionPresentation :: Type -> Type -> Type -> Type
type RestrictionPresentation morphism cell witness = morphism -> RestrictionParts cell witness

restrictionArrow :: Restriction cell witness -> RestrictionArrow cell
restrictionArrow restriction =
  RestrictionArrow
    { restrictFrom = rSource restriction,
      restrictTo = rTarget restriction
    }

restrictApply ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Restriction cell witness ->
  stalk ->
  stalk
restrictApply algebra restriction =
  restrictStalk algebra (rWitness restriction)

type RestrictionCheck :: Type -> Type -> Type
data RestrictionCheck stalk mismatch = RestrictionCheck
  { restrictedStalk :: !stalk,
    restrictionMismatches :: ![mismatch]
  }
  deriving stock (Eq, Show)

checkRestriction ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Restriction cell witness ->
  stalk ->
  stalk ->
  RestrictionCheck stalk mismatch
checkRestriction algebra restriction sourceStalk targetStalk =
  let restricted = restrictApply algebra restriction sourceStalk
   in RestrictionCheck
        { restrictedStalk = restricted,
          restrictionMismatches = stalkMismatches algebra restricted targetStalk
        }
