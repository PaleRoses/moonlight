{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Tropical.Cost.Types
  ( TropicalTransition (..),
    TropicalWeightedTransition (..),
    TropicalCostModel (..),
    TropicalCostTable (..),
    TropicalClassChoice (..),
    TropicalCosectionPlan (..),
    TropicalCosectionFailure (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Moonlight.Cosheaf.Colimit
  ( CosheafColimit,
    CosheafColimitFailure,
  )
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey,
    CosectionRepKey,
    CosectionRepresentative,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportFailure,
  )
import Moonlight.Cosheaf.Tropical.Cost.MinPlus
  ( MinPlusWeight,
    TropicalCostParseFailure,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )

type TropicalTransition :: Type -> Type -> Type -> Type
data TropicalTransition obj mor value = TropicalTransition
  { tropicalTransitionMorphism :: !(CheckedMorphism obj mor),
    tropicalTransitionSource :: !(CosectionRepresentative obj value),
    tropicalTransitionTarget :: !(CosectionRepresentative obj value)
  }
  deriving stock (Eq, Ord, Show)

type TropicalWeightedTransition :: Type -> Type -> Type -> Type
data TropicalWeightedTransition obj mor value = TropicalWeightedTransition
  { twtTransition :: !(TropicalTransition obj mor value),
    twtSourceKey :: !CosectionRepKey,
    twtTargetKey :: !CosectionRepKey,
    twtWeight :: !MinPlusWeight
  }
  deriving stock (Eq, Ord, Show)

type TropicalCostModel :: Type -> Type -> Type
data TropicalCostModel site value = TropicalCostModel
  { tcmRepresentativeCost ::
      CosectionRepresentative (SiteObject site) value ->
      Either (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value) MinPlusWeight,
    tcmTransitionCost ::
      TropicalTransition (SiteObject site) (SiteMorphism site) value ->
      Either (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value) MinPlusWeight
  }

type TropicalCostTable :: Type -> Type -> Type
data TropicalCostTable site value = TropicalCostTable
  { tropicalCostTableColimitInternal :: !(CosheafColimit site value),
    tropicalCostTableRepresentativeCostsInternal :: !(IntMap MinPlusWeight),
    tropicalCostTableTransitionsInternal :: ![TropicalWeightedTransition (SiteObject site) (SiteMorphism site) value]
  }

deriving stock instance
  (Eq site, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (TropicalCostTable site value)

deriving stock instance
  (Show site, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (TropicalCostTable site value)

type TropicalClassChoice :: Type -> Type -> Type
data TropicalClassChoice obj value = TropicalClassChoice
  { tccClassKey :: !CosectionClassKey,
    tccRepresentativeKey :: !CosectionRepKey,
    tccRepresentative :: !(CosectionRepresentative obj value),
    tccCost :: !MinPlusWeight
  }
  deriving stock (Eq, Ord, Show)

type TropicalCosectionPlan :: Type -> Type -> Type
data TropicalCosectionPlan site value = TropicalCosectionPlan
  { tropicalCosectionPlanCostTableInternal :: !(TropicalCostTable site value),
    tropicalCosectionPlanClassChoicesInternal :: !(IntMap (TropicalClassChoice (SiteObject site) value))
  }

deriving stock instance
  (Eq site, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (TropicalCosectionPlan site value)

deriving stock instance
  (Show site, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (TropicalCosectionPlan site value)

type TropicalCosectionFailure :: Type -> Type -> Type -> Type
data TropicalCosectionFailure obj mor value
  = TropicalRepresentativeCostMissing !(CosectionRepresentative obj value)
  | TropicalTransitionCostMissing !(TropicalTransition obj mor value)
  | TropicalIllFormedCost !TropicalCostParseFailure
  | TropicalUnboundedCost !CosectionRepKey
  | TropicalIncompatibleBasis !Int !Int
  | TropicalEmptyColimitClass !CosectionClassKey
  | TropicalRepresentativeMissing !CosectionRepKey
  | TropicalColimitMalformed !(CosheafColimitFailure obj mor value)
  | TropicalSupportInvalid !(CosheafSupportFailure obj mor value)
  deriving stock (Eq, Show)
