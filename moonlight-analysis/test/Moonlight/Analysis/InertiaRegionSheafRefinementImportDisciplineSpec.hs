module Moonlight.Analysis.InertiaRegionSheafRefinementImportDisciplineSpec
  ( tests,
  )
where

import Moonlight.Analysis.InertiaRegionSheafRefinementImportManifest
  ( inertiaRegionSheafRefinementManifest,
    inertiaRelativeDirectory,
    packageMarker,
  )
import Moonlight.Analysis.SheafImportDisciplineSupport (sheafImportDisciplineTests)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  sheafImportDisciplineTests
    "inertia region sheaf refinement imports obey the layer DAG"
    packageMarker
    inertiaRelativeDirectory
    inertiaRegionSheafRefinementManifest
