module Moonlight.Analysis.InertiaRegionSheafRefinementImportManifest
  ( inertiaRegionSheafRefinementManifest,
    inertiaRelativeDirectory,
    packageMarker,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.SheafImportManifestSupport
  ( analysisDynamicsRelativeDirectory,
    analysisPackageMarker,
    mkAnalysisSheafManifest,
  )
import Moonlight.Pale.Test.Gluing.Discipline (SheafManifest)

packageMarker :: FilePath
packageMarker = analysisPackageMarker

inertiaRelativeDirectory :: FilePath
inertiaRelativeDirectory = analysisDynamicsRelativeDirectory "Inertia/Region"

inertiaRegionSheafRefinementManifest :: SheafManifest
inertiaRegionSheafRefinementManifest =
  mkAnalysisSheafManifest
    "Moonlight.Analysis.Dynamics.Inertia.Region.SheafRefinement"
    (Map.fromList
          [ ( "Moonlight.Analysis.Dynamics.Inertia.Region.SheafRefinement",
              Set.empty
            )
          ]
    )
