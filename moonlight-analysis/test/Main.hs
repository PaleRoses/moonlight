module Main (main) where

import qualified Moonlight.Analysis.ConvergenceSpec as ConvergenceSpec
import qualified Moonlight.Analysis.CPGSpec as CPGSpec
import qualified Moonlight.Analysis.BiomechanicsSheafRefinementSpec as BiomechanicsSheafRefinementSpec
import qualified Moonlight.Analysis.BiomechanicsSheafRefinementImportDisciplineSpec as BiomechanicsSheafRefinementImportDisciplineSpec
import qualified Moonlight.Analysis.IKFabrikSheafRefinementImportDisciplineSpec as IKFabrikSheafRefinementImportDisciplineSpec
import qualified Moonlight.Analysis.DualSpec as DualSpec
import qualified Moonlight.Analysis.InertiaSpec as InertiaSpec
import qualified Moonlight.Analysis.InertiaSheafRefinementSpec as InertiaSheafRefinementSpec
import qualified Moonlight.Analysis.InertiaRegionSheafRefinementImportDisciplineSpec as InertiaRegionSheafRefinementImportDisciplineSpec
import qualified Moonlight.Analysis.IKSheafRefinementSpec as IKSheafRefinementSpec
import qualified Moonlight.Analysis.LocomotionIKSpec as LocomotionIKSpec
import qualified Moonlight.Analysis.Mesh.GraphSpec as GraphSpec
import qualified Moonlight.Analysis.Mesh.ScalarSpec as ScalarSpec
import qualified Moonlight.Analysis.ModuleContractSpec as ModuleContractSpec
import qualified Moonlight.Analysis.ODESpec as ODESpec
import qualified Moonlight.Analysis.RootSpec as RootSpec
import qualified Moonlight.Analysis.SheafRefinementSpec as SheafRefinementSpec
import qualified Moonlight.Analysis.SheafArchitectureSpec as SheafArchitectureSpec
import qualified Moonlight.Analysis.SolverInfraSpec as SolverInfraSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-analysis"
        [ DualSpec.tests,
          ConvergenceSpec.tests,
          CPGSpec.tests,
          BiomechanicsSheafRefinementSpec.tests,
          BiomechanicsSheafRefinementImportDisciplineSpec.tests,
          InertiaSpec.tests,
          InertiaSheafRefinementSpec.tests,
          InertiaRegionSheafRefinementImportDisciplineSpec.tests,
          IKSheafRefinementSpec.tests,
          IKFabrikSheafRefinementImportDisciplineSpec.tests,
          LocomotionIKSpec.tests,
          GraphSpec.tests,
          ScalarSpec.tests,
          ModuleContractSpec.tests,
          SheafArchitectureSpec.tests,
          SheafRefinementSpec.tests,
          RootSpec.tests,
          SolverInfraSpec.tests,
          ODESpec.tests
        ]
    )
