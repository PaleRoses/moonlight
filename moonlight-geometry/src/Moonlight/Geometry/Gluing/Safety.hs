module Moonlight.Geometry.Gluing.Safety
  ( RewriteSemantics (..),
    RewriteLawfulness (..),
    lawfulnessForInterface,
    lawfulnessFor,
  )
where

import Data.Kind (Type)
import Moonlight.Geometry.Site.Semantics (InterfaceOperator (..))
import Moonlight.Geometry.Site.Token (SDFTokenF (..), tokenInterfaceOperator)

type RewriteSemantics :: Type
data RewriteSemantics
  = ZeroSetSemantics
  | MetricSemantics
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type RewriteLawfulness :: Type
data RewriteLawfulness
  = FullBooleanAlgebra
  | DegenerateOnly
  | Opaque
  deriving stock (Eq, Ord, Show, Enum, Bounded)

lawfulnessForInterface :: RewriteSemantics -> InterfaceOperator -> RewriteLawfulness
lawfulnessForInterface semantics interfaceOperator =
  case interfaceOperator of
    InterfaceHardUnion -> hardBooleanLawfulness semantics
    InterfaceHardSubtract -> hardBooleanLawfulness semantics
    InterfaceHardIntersect -> hardBooleanLawfulness semantics
    InterfaceSmoothUnion -> DegenerateOnly
    InterfaceSmoothSubtract -> DegenerateOnly
    InterfaceSmoothIntersect -> DegenerateOnly
    InterfaceChamfer -> DegenerateOnly
    InterfaceRound -> DegenerateOnly

lawfulnessFor :: RewriteSemantics -> SDFTokenF a -> RewriteLawfulness
lawfulnessFor semantics token =
  case tokenInterfaceOperator token of
    Just interfaceOperator -> lawfulnessForInterface semantics interfaceOperator
    Nothing ->
      case token of
        Prim _ -> Opaque
        _ -> DegenerateOnly

hardBooleanLawfulness :: RewriteSemantics -> RewriteLawfulness
hardBooleanLawfulness semantics =
  case semantics of
    ZeroSetSemantics -> FullBooleanAlgebra
    MetricSemantics -> DegenerateOnly
