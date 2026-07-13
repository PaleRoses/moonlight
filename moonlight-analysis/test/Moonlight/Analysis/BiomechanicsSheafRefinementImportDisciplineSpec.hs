module Moonlight.Analysis.BiomechanicsSheafRefinementImportDisciplineSpec
  ( tests,
  )
where

import Moonlight.Analysis.BiomechanicsSheafRefinementImportManifest
  ( biomechanicsRelativeDirectory,
    biomechanicsSheafRefinementManifest,
    packageMarker,
  )
import Moonlight.Analysis.SheafImportDisciplineSupport (sheafImportDisciplineTests)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  sheafImportDisciplineTests
    "biomechanical sheaf refinement imports obey the layer DAG"
    packageMarker
    biomechanicsRelativeDirectory
    biomechanicsSheafRefinementManifest
