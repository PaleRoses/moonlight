module Moonlight.Analysis.SheafImportDisciplineSupport
  ( sheafImportDisciplineTests,
  )
where

import Moonlight.Pale.Test.Gluing.Discipline
  ( SheafManifest,
    assertSheafDiscipline,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

sheafImportDisciplineTests :: String -> FilePath -> FilePath -> SheafManifest -> TestTree
sheafImportDisciplineTests testLabel packageMarker relativeDirectory sheafManifest =
  testGroup
    "import-discipline"
    [ testCase
        testLabel
        (assertSheafDiscipline packageMarker relativeDirectory sheafManifest)
    ]
