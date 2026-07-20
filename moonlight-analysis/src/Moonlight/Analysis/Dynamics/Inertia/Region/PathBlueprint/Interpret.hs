module Moonlight.Analysis.Dynamics.Inertia.Region.PathBlueprint.Interpret
  ( InertiaRegionPathInterpreter (..),
    resolvePathBlueprintSite,
    relabelDecompositionWithPathBlueprint,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Dynamics.Inertia.Region.Kernel (RegionSubdivisionPath)
import Moonlight.Analysis.Dynamics.Inertia.Region.Cover (InertiaRegionDecomposition (..))
import Moonlight.Analysis.Dynamics.Inertia.Region.PathBlueprint
  ( InertiaRegionPathBinding (..),
    InertiaRegionPathBlueprint (..),
  )
import Moonlight.Core
  ( PatternVar
  )
import Moonlight.Core
  ( Substitution,
    lookupSubst
  )
import Moonlight.EGraph.Pure.Types (ClassId)
import Data.Map.Strict qualified as Map

type InertiaRegionPathInterpreter :: Type -> Type
data InertiaRegionPathInterpreter site = InertiaRegionPathInterpreter
  { irpiRootSite :: ClassId -> site,
    irpiWitnessSite :: PatternVar -> ClassId -> site,
    irpiStructuralSite :: RegionSubdivisionPath -> site
  }

resolvePathBlueprintSite ::
  InertiaRegionPathInterpreter site ->
  InertiaRegionPathBlueprint ->
  ClassId ->
  Substitution ->
  RegionSubdivisionPath ->
  Maybe site
resolvePathBlueprintSite pathInterpreter pathBlueprint rootClassId substitution decompositionPath =
  Map.lookup decompositionPath (irpbBindingByPath pathBlueprint)
    >>= interpretPathBinding
  where
    interpretPathBinding pathBinding =
      case pathBinding of
        RootInertiaRegionPath ->
          Just (irpiRootSite pathInterpreter rootClassId)
        WitnessInertiaRegionPath patternVar ->
          irpiWitnessSite pathInterpreter patternVar
            <$> lookupSubst patternVar substitution
        StructuralInertiaRegionPath ->
          Just (irpiStructuralSite pathInterpreter decompositionPath)

relabelDecompositionWithPathBlueprint ::
  InertiaRegionPathInterpreter site ->
  InertiaRegionPathBlueprint ->
  ClassId ->
  Substitution ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  Maybe (InertiaRegionDecomposition site)
relabelDecompositionWithPathBlueprint pathInterpreter pathBlueprint rootClassId substitution decomposition =
  resolvePathBlueprintSite pathInterpreter pathBlueprint rootClassId substitution (irdSite decomposition)
    >>= \relabeledSite ->
      fmap
        (\relabeledChildren ->
            InertiaRegionDecomposition
              { irdSite = relabeledSite,
                irdBoundingBox = irdBoundingBox decomposition,
                irdChildren = relabeledChildren
              }
        )
        ( traverse
            (relabelDecompositionWithPathBlueprint pathInterpreter pathBlueprint rootClassId substitution)
            (irdChildren decomposition)
        )
