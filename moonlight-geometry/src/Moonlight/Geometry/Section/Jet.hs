module Moonlight.Geometry.Section.Jet
  ( GradientEnvelope (..),
    HessianEnvelope (..),
    JetAnalysis (..),
  )
where

import Data.Kind (Type)
import Moonlight.Geometry.Site.Semantics
  ( Certification,
    CurvatureUpperBound,
    GradientLipschitzUpperBound,
  )
import Moonlight.LinAlg.Geometry (Vec3)

type GradientEnvelope :: Type
data GradientEnvelope = GradientEnvelope
  { geNormalEstimate :: !(Certification Vec3),
    geLipschitzGradient :: !(Certification GradientLipschitzUpperBound)
  }
  deriving stock (Eq, Show)

type HessianEnvelope :: Type
data HessianEnvelope = HessianEnvelope
  { heCurvatureBound :: !(Certification CurvatureUpperBound),
    heConvexityWitness :: !Bool
  }
  deriving stock (Eq, Show)

type JetAnalysis :: Type
data JetAnalysis = JetAnalysis
  { jaGradient :: !(Maybe GradientEnvelope),
    jaHessian :: !(Maybe HessianEnvelope)
  }
  deriving stock (Eq, Show)
