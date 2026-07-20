module Moonlight.Analysis.Dynamics.Inertia.Region.PathBlueprint
  ( InertiaRegionPathBinding (..),
    InertiaRegionPathBlueprint (..),
    InertiaRegionPathBlueprintProgram (..),
    deriveInertiaRegionPathBlueprint,
    compileInertiaRegionPathBlueprint,
  )
where

import Data.Kind (Type)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Analysis.Dynamics.Inertia.Region.Kernel (RegionSubdivisionPath (..))
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery, cpqPrimaryPattern)
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph

type InertiaRegionPathBinding :: Type
data InertiaRegionPathBinding
  = RootInertiaRegionPath
  | WitnessInertiaRegionPath EGraph.PatternVar
  | StructuralInertiaRegionPath
  deriving stock (Eq, Show)

type InertiaRegionPathBlueprint :: Type
newtype InertiaRegionPathBlueprint = InertiaRegionPathBlueprint
  { irpbBindingByPath :: Map RegionSubdivisionPath InertiaRegionPathBinding
  }
  deriving stock (Eq, Show)

type InertiaRegionPathBlueprintProgram :: Type
data InertiaRegionPathBlueprintProgram
  = PrimaryPatternPathBlueprintProgram
  | StaticPathBlueprintProgram InertiaRegionPathBlueprint
  deriving stock (Eq, Show)

deriveInertiaRegionPathBlueprint ::
  Foldable f =>
  Pattern f ->
  InertiaRegionPathBlueprint
deriveInertiaRegionPathBlueprint primaryPattern =
  InertiaRegionPathBlueprint
    { irpbBindingByPath =
        Map.insert
          (RegionSubdivisionPath [])
          RootInertiaRegionPath
          (patternChildBindings (RegionSubdivisionPath []) primaryPattern)
    }

compileInertiaRegionPathBlueprint ::
  Foldable f =>
  InertiaRegionPathBlueprintProgram ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  InertiaRegionPathBlueprint
compileInertiaRegionPathBlueprint pathBlueprintProgram compiledQuery =
  case pathBlueprintProgram of
    PrimaryPatternPathBlueprintProgram ->
      deriveInertiaRegionPathBlueprint (cpqPrimaryPattern compiledQuery)
    StaticPathBlueprintProgram pathBlueprint ->
      pathBlueprint

patternChildBindings ::
  Foldable f =>
  RegionSubdivisionPath ->
  Pattern f ->
  Map RegionSubdivisionPath InertiaRegionPathBinding
patternChildBindings currentPath patternValue =
  case patternValue of
    PatternVar _ ->
      Map.empty
    PatternNode patternNode ->
      foldMap
        (\(childIndex, childPattern) ->
            pathBindingsAtPath
              (appendPatternChildIndex currentPath childIndex)
              childPattern
        )
        (zip [0 :: Int ..] (toList patternNode))

pathBindingsAtPath ::
  Foldable f =>
  RegionSubdivisionPath ->
  Pattern f ->
  Map RegionSubdivisionPath InertiaRegionPathBinding
pathBindingsAtPath currentPath patternValue =
  case patternValue of
    PatternVar patternVar ->
      Map.singleton currentPath (WitnessInertiaRegionPath patternVar)
    PatternNode patternNode ->
      Map.insert
        currentPath
        StructuralInertiaRegionPath
        ( foldMap
            (\(childIndex, childPattern) ->
                pathBindingsAtPath
                  (appendPatternChildIndex currentPath childIndex)
                  childPattern
            )
            (zip [0 :: Int ..] (toList patternNode))
        )

appendPatternChildIndex :: RegionSubdivisionPath -> Int -> RegionSubdivisionPath
appendPatternChildIndex (RegionSubdivisionPath currentIndices) childIndex =
  RegionSubdivisionPath (currentIndices <> [childIndex])
