module Moonlight.Sheaf.Obstruction.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Obstruction.AssignmentWitnessSpec qualified as AssignmentWitnessSpec
import Moonlight.Sheaf.Obstruction.CertificationSpec qualified as CertificationSpec
import Moonlight.Sheaf.Obstruction.Cohomological.AlgebraSpec qualified as CohomologicalAlgebraSpec
import Moonlight.Sheaf.Obstruction.Cohomological.EnvironmentSpec qualified as EnvironmentSpec
import Moonlight.Sheaf.Obstruction.Cohomological.LivePruningSpec qualified as LivePruningSpec
import Moonlight.Sheaf.Obstruction.Cohomological.MicrosupportSpec qualified as MicrosupportSpec
import Moonlight.Sheaf.Obstruction.Cohomological.SubstrateSpec qualified as CohomologicalSubstrateSpec
import Moonlight.Sheaf.Obstruction.ModalitySpec qualified as ObstructionModalitySpec
import Moonlight.Sheaf.Obstruction.PruningSpec qualified as ObstructionPruningSpec
import Moonlight.Sheaf.Obstruction.SectionSpec qualified as ObstructionSectionSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "obstruction"
    [ AssignmentWitnessSpec.tests,
      CertificationSpec.tests,
      CohomologicalAlgebraSpec.tests,
      CohomologicalSubstrateSpec.tests,
      EnvironmentSpec.environmentTests,
      LivePruningSpec.tests,
      MicrosupportSpec.tests,
      ObstructionModalitySpec.modalityTests,
      ObstructionSectionSpec.sectionTests,
      ObstructionPruningSpec.tests
    ]
