module Moonlight.Geometry.Global.Cost
  ( sdfBaseCost,
    constructorBaseCost,
    certificatePenalty,
    semanticsPenalty,
  )
where

import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Moonlight.Geometry.Site.Semantics
  ( Certification (..),
    DistanceCertificate (..),
    DistanceSemantics (..),
    TraceSafety (..),
  )
import Moonlight.Geometry.Site.Token (SDFTokenF (..))

sdfBaseCost :: CostAlgebra SDFTokenF Double
sdfBaseCost =
  CostAlgebra (\token -> constructorBaseCost token + foldr (+) 0.0 token)

constructorBaseCost :: SDFTokenF cost -> Double
constructorBaseCost = \case
  Prim _ -> 1.0
  SmoothUnion _ _ _ -> 1.5
  SmoothSubtract _ _ _ -> 2.0
  SmoothIntersect _ _ _ -> 2.0
  HardUnion _ _ -> 1.5
  HardSubtract _ _ -> 2.5
  HardIntersect _ _ -> 2.0
  Chamfer _ _ _ -> 2.0
  Round _ _ -> 1.0
  NoisePerturbation _ _ -> 3.0
  DomainWarp _ _ -> 3.5
  Twist _ _ -> 1.5
  Bend _ _ -> 1.5
  Onion _ _ -> 0.5
  Transform _ _ -> 0.8
  Scale _ _ -> 0.8
  Repeat _ _ -> 3.0
  SDFEmpty -> 0.0

certificatePenalty :: DistanceCertificate -> Double
certificatePenalty certificate =
  semanticsPenalty (dcSemantics certificate)
    + traceSafetyPenalty (dcTraceSafety certificate)
    + precisionPenalty (dcPrecisionLowerBound certificate)

semanticsPenalty :: DistanceSemantics -> Double
semanticsPenalty = \case
  ExactDist -> 0.0
  ConservativeDist -> 0.5
  PseudoField -> 6.0
  Occupancy -> 4.0

traceSafetyPenalty :: TraceSafety -> Double
traceSafetyPenalty = \case
  SphereTraceExact -> 0.0
  SphereTraceConservative _ -> 0.25
  RequiresCertifiedStepper -> 4.0
  UnsafeForSphereTracing -> 3.0

precisionPenalty :: Certification a -> Double
precisionPenalty = \case
  Unknown -> 2.0
  Certified _ -> 0.0
