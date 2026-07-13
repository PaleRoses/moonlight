module Moonlight.Analysis.IKFabrikSheafRefinementImportDisciplineSpec
  ( tests,
  )
where

import Moonlight.Analysis.IKFabrikSheafRefinementImportManifest
  ( ikFabrikSheafRefinementManifest,
    ikRelativeDirectory,
    packageMarker,
  )
import Moonlight.Analysis.SheafImportDisciplineSupport (sheafImportDisciplineTests)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  sheafImportDisciplineTests
    "ik fabrik sheaf refinement imports obey the layer DAG"
    packageMarker
    ikRelativeDirectory
    ikFabrikSheafRefinementManifest
