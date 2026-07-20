module Moonlight.Analysis.Dynamics.Inertia.Region.SheafStalk
  ( MassPropertiesMismatch (..),
    massPropertiesStalkOps,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Dynamics.Inertia.Region (MassProperties (..))
import Moonlight.Analysis.SheafRefinement.Tolerance (averageDouble, relClose, vecApproxEq)
import Moonlight.LinAlg.Geometry (Symmetric3, symmetric3Entries, zipSymmetric3With)
import Moonlight.LinAlg.Geometry (Vec3 (..), averageVec3)
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))

type MassPropertiesMismatch :: Type
data MassPropertiesMismatch
  = MassMismatch Double Double
  | CenterOfMassMismatch Vec3 Vec3
  | InertiaTensorMismatch (Symmetric3 Double) (Symmetric3 Double)
  deriving stock (Eq, Show)

massPropertiesStalkOps :: StalkAlgebra witness MassProperties MassPropertiesMismatch ()
massPropertiesStalkOps =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = massPropertiesMismatches,
      saMerge = \left right -> Right (mergeMassProperties left right),
      saRepair = const (Left ()),
      saNormalize = id
    }

massPropertiesMismatches :: MassProperties -> MassProperties -> [MassPropertiesMismatch]
massPropertiesMismatches left right =
  foldr
    (\mismatch -> maybe id (:) mismatch)
    []
    [ if relClose (massPropertiesMass left) (massPropertiesMass right)
        then Nothing
        else Just (MassMismatch (massPropertiesMass left) (massPropertiesMass right)),
      if vecApproxEq (massPropertiesCenterOfMass left) (massPropertiesCenterOfMass right)
        then Nothing
        else Just (CenterOfMassMismatch (massPropertiesCenterOfMass left) (massPropertiesCenterOfMass right)),
      if inertiaTensorApproxEq (massPropertiesInertiaTensor left) (massPropertiesInertiaTensor right)
        then Nothing
        else Just (InertiaTensorMismatch (massPropertiesInertiaTensor left) (massPropertiesInertiaTensor right))
    ]

mergeMassProperties :: MassProperties -> MassProperties -> MassProperties
mergeMassProperties left right =
  MassProperties
    { massPropertiesMass = averageDouble (massPropertiesMass left) (massPropertiesMass right),
      massPropertiesCenterOfMass = averageVec3 (massPropertiesCenterOfMass left) (massPropertiesCenterOfMass right),
      massPropertiesInertiaTensor =
        averageInertiaTensor
          (massPropertiesInertiaTensor left)
          (massPropertiesInertiaTensor right)
    }

inertiaTensorApproxEq :: Symmetric3 Double -> Symmetric3 Double -> Bool
inertiaTensorApproxEq leftTensor rightTensor =
  and (zipWith relClose (symmetric3Entries leftTensor) (symmetric3Entries rightTensor))

averageInertiaTensor :: Symmetric3 Double -> Symmetric3 Double -> Symmetric3 Double
averageInertiaTensor leftTensor rightTensor =
  zipSymmetric3With averageDouble leftTensor rightTensor
