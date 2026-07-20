module Moonlight.Sketch.Effect.LawNames
  ( SketchLawName (..),
    sketchLawName,
    CommonLawName (..),
    IsLawName (..),
    constructorLawNameWithOverrides,
  )
where

import Data.Kind (Type)
import Moonlight.Core (CommonLawName (..), IsLawName (..), constructorLawNameWithOverrides)

type SketchLawName :: Type
data SketchLawName
  = CommonLaw CommonLawName
  | NormalizeDeterministic
  | HashDeterministic
  | HashPostNormalization
  | HashCollisionResistance
  | SubtypeReflexive
  | SubtypeTransitive
  | SubtypeAntisymmetric
  | SubtypeVoidBottom
  | SubtypeUnknownTop
  | ResolveIdempotent
  | ResolvePreservesSemantics
  | ResolveCycleDetection
  | FormatMatchDeterministic
  | ValidateEmptyOnConformance
  | ValidateAccumulation
  | SchemaEqPostNormalization
  | LatticeBoundedJoinIdentity
  | LatticeBoundedMeetIdentity
  | EnvMergeAssociative
  | EnvMergeIdentity
  deriving stock (Eq, Ord, Show)

sketchLawName :: SketchLawName -> String
sketchLawName lawNameValue =
  case lawNameValue of
    CommonLaw commonLawName -> lawNameText commonLawName
    specificLawName -> constructorLawNameWithOverrides [] (show specificLawName)

instance IsLawName SketchLawName where
  lawNameText = sketchLawName
