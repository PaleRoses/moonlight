module Moonlight.Analysis.SheafImportManifestSupport
  ( analysisDynamicsRelativeDirectory,
    analysisPackageMarker,
    mkAnalysisSheafManifest,
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Pale.Test.Gluing.Discipline (SheafManifest (..))

analysisPackageMarker :: FilePath
analysisPackageMarker = "foundation/moonlight-analysis/moonlight-analysis.cabal"

analysisDynamicsRelativeDirectory :: FilePath -> FilePath
analysisDynamicsRelativeDirectory relativeDirectory =
  "src/Moonlight/Analysis/Dynamics/" <> relativeDirectory

mkAnalysisSheafManifest :: String -> Map String (Set String) -> SheafManifest
mkAnalysisSheafManifest sheafModulePrefix sheafAllowedImports =
  SheafManifest
    { sheafModulePrefix = sheafModulePrefix,
      sheafAllowedImports = sheafAllowedImports
    }
