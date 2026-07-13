module Moonlight.Analysis.BiomechanicsSheafRefinementImportManifest
  ( biomechanicsSheafRefinementManifest,
    biomechanicsRelativeDirectory,
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

biomechanicsRelativeDirectory :: FilePath
biomechanicsRelativeDirectory = analysisDynamicsRelativeDirectory "Biomechanics"

biomechanicsSheafRefinementManifest :: SheafManifest
biomechanicsSheafRefinementManifest =
  mkAnalysisSheafManifest
    "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement"
    (Map.fromList
          [ ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement",
              Set.fromList
                [ "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Candidate",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Solve",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate"
                ]
            ),
            ("Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy", Set.empty),
            ("Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator", Set.empty),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
              Set.fromList ["Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy"]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk",
              Set.fromList ["Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core"]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate",
              Set.fromList ["Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core"]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score",
              Set.fromList
                [ "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk"
                ]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Solve",
              Set.fromList
                [ "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy"
                ]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton",
              Set.fromList
                [ "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate"
                ]
            ),
            ( "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Candidate",
              Set.fromList
                [ "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk",
                  "Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton"
                ]
            )
          ]
    )
