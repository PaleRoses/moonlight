{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Linear.Types
  ( LocalBasisKey (..),
    LinearCostalk (..),
    linearCostalkDimension,
    LinearCorestriction (..),
    LinearCosheaf (..),
    LinearCosheafAlgebra (..),
    LinearCosheafFailure (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Moonlight.Core (DenseKey (..))
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
    CosheafSiteIndex,
    CosheafSiteIndexFailure,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
  )
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexCount,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )

-- | Dense coordinate inside a single linear costalk.
type LocalBasisKey :: Type
newtype LocalBasisKey = LocalBasisKey
  { unLocalBasisKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey LocalBasisKey where
  encodeDenseKey =
    unLocalBasisKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    LocalBasisKey
  {-# INLINE decodeDenseKey #-}

type LinearCostalk :: Type -> Type -> Type
data LinearCostalk obj basis = LinearCostalk
  { lcObjectKey :: !ObjectKey,
    lcObject :: !obj,
    lcBasis :: !(DenseIndex LocalBasisKey basis)
  }
  deriving stock (Eq, Show)

linearCostalkDimension :: LinearCostalk obj basis -> Int
linearCostalkDimension =
  denseIndexCount . lcBasis
{-# INLINE linearCostalkDimension #-}

type LinearCorestriction :: Type -> Type -> Type -> Type
data LinearCorestriction obj mor coeff = LinearCorestriction
  { lcrMorphismKey :: !CosheafMorphismKey,
    lcrMorphism :: !(CheckedMorphism obj mor),
    lcrSourceObjectKey :: !ObjectKey,
    lcrTargetObjectKey :: !ObjectKey,
    lcrMatrix :: !(BoundaryIncidence coeff)
  }
  deriving stock (Eq, Show)

type LinearCosheaf :: Type -> Type -> Type -> Type
data LinearCosheaf site basis coeff = LinearCosheaf
  { lcosSite :: !site,
    lcosSiteIndex :: !(CosheafSiteIndex site),
    lcosCostalks :: !(IntMap (LinearCostalk (SiteObject site) basis)),
    lcosCorestrictions :: !(IntMap (LinearCorestriction (SiteObject site) (SiteMorphism site) coeff))
  }

deriving stock instance
  (Eq site, Eq basis, Eq coeff, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (LinearCosheaf site basis coeff)

deriving stock instance
  (Show site, Show basis, Show coeff, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (LinearCosheaf site basis coeff)

type LinearCosheafAlgebra :: Type -> Type -> Type -> Type
data LinearCosheafAlgebra site coeff matrixFailure = LinearCosheafAlgebra
  { lcaCorestrictionMatrix ::
      CheckedMorphism (SiteObject site) (SiteMorphism site) ->
      Either matrixFailure (BoundaryIncidence coeff)
  }

type LinearCosheafFailure :: Type -> Type -> Type -> Type -> Type -> Type
data LinearCosheafFailure obj mor basis coeff matrixFailure
  = LinearCostalkMissing !obj
  | LinearCostalkUnknownObject !obj
  | LinearCostalkDuplicateBasis !obj !basis
  | LinearCosheafSiteIndexInvalid !(CosheafSiteIndexFailure obj mor)
  | LinearCosheafObjectKeyMissing !obj
  | LinearCorestrictionSourceCostalkMissing !(CheckedMorphism obj mor)
  | LinearCorestrictionTargetCostalkMissing !(CheckedMorphism obj mor)
  | LinearCorestrictionMatrixFailed !(CheckedMorphism obj mor) !matrixFailure
  | LinearCorestrictionShapeMismatch
      !(CheckedMorphism obj mor)
      !Int
      !Int
      !Int
      !Int
  | LinearCorestrictionIdentityMissing !(CheckedMorphism obj mor)
  | LinearCorestrictionIdentityMismatch
      !(CheckedMorphism obj mor)
      !(BoundaryIncidence coeff)
      !(BoundaryIncidence coeff)
  | LinearCorestrictionCompositionUndefined
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
  | LinearCorestrictionCompositeMissing !(CheckedMorphism obj mor)
  | LinearCorestrictionCompositionShapeFailed
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !BoundaryIncidenceShapeError
  | LinearCorestrictionCompositionMismatch
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(BoundaryIncidence coeff)
      !(BoundaryIncidence coeff)
  deriving stock (Eq, Show)
