{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Automata.Effect.LawNames
  ( LawName (..),
    lawName,
  )
where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..), constructorLawName)

type LawName :: Type
data LawName
  = DependentDBTAIsZygo
  | ProductDBTA
  | IntersectionAcceptance
  | UnionAcceptance
  | ComplementAcceptance
  | DenotationalUnionHomomorphism
  | DenotationalIntersectionHomomorphism
  | DenotationalComplementHomomorphism
  | TopDownFold
  | TopDownAnnotation
  | TopDownAnnotatedAttribute
  | TopDownStateProjection
  | TopDownAttributeProjection
  | RootAttributeProjection
  deriving stock (Eq, Ord, Show)

lawName :: LawName -> String
lawName = constructorLawName . show

instance IsLawName LawName where
  lawNameText = lawName
