module Moonlight.Geometry.Global.Acceleration
  ( ProxyKind (..),
    AccelerationCertificate (..),
    eligibleForSphereCarving,
    admissibleUnderSmoothParent,
  )
where

import Data.Kind (Type)
import Moonlight.Geometry.Section.Analysis (SpatialSupport)
import Moonlight.Geometry.Site.Semantics

type ProxyKind :: Type
data ProxyKind
  = ConservativeProxy
  | LipschitzProxy
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type AccelerationCertificate :: Type
data AccelerationCertificate = AccelerationCertificate
  { acSupport :: !SpatialSupport,
    acFarFieldConstant :: !(Certification FarFieldLowerBound),
    acProxyKind :: !(Maybe ProxyKind),
    acRegionEquivalentToSource :: !Bool
  }
  deriving stock (Eq, Show)

eligibleForSphereCarving :: DistanceCertificate -> Bool
eligibleForSphereCarving = supportsSphereCarving

admissibleUnderSmoothParent :: ProxyKind -> Bool
admissibleUnderSmoothParent = \case
  ConservativeProxy -> False
  LipschitzProxy -> True
