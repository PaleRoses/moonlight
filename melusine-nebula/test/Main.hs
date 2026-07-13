module Main (main) where

import Melusine.Nebula.Spec.AuditSpec qualified as AuditSpec
import Melusine.Nebula.Spec.CertificateSpec qualified as CertificateSpec
import Melusine.Nebula.Spec.CaseLiftSpec qualified as CaseLiftSpec
import Melusine.Nebula.Spec.HarvestSpec qualified as HarvestSpec
import Melusine.Nebula.Spec.PipelineSpec qualified as PipelineSpec
import Melusine.Nebula.Spec.RealizeSpec qualified as RealizeSpec
import Melusine.Nebula.Spec.ReportJsonSpec qualified as ReportJsonSpec
import Melusine.Nebula.Spec.WriteBackSpec qualified as WriteBackSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "nebula"
      [ AuditSpec.spec,
        CertificateSpec.spec,
        CaseLiftSpec.spec,
        HarvestSpec.tests,
        PipelineSpec.spec,
        RealizeSpec.spec,
        ReportJsonSpec.spec,
        WriteBackSpec.spec
      ]
