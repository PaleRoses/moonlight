module Moonlight.Analysis.IKFabrikSheafRefinementImportManifest
  ( ikFabrikSheafRefinementManifest,
    ikRelativeDirectory,
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

ikRelativeDirectory :: FilePath
ikRelativeDirectory = analysisDynamicsRelativeDirectory "IK/Fabrik"

ikFabrikSheafRefinementManifest :: SheafManifest
ikFabrikSheafRefinementManifest =
  mkAnalysisSheafManifest
    "Moonlight.Analysis.Dynamics.IK.Fabrik.SheafRefinement"
    (Map.fromList
          [ ( "Moonlight.Analysis.Dynamics.IK.Fabrik.SheafRefinement",
              Set.empty
            )
          ]
    )
