module Moonlight.Sheaf.Core.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Core.CertifiedSpec qualified as CertifiedSpec
import Moonlight.Sheaf.Core.ContainerSpec qualified as ContainerSpec
import Moonlight.Sheaf.Core.FootprintSpec qualified as FootprintSpec
import Moonlight.Sheaf.Core.LinearizeSpec qualified as LinearizeSpec
import Moonlight.Sheaf.Core.OperatorSpec qualified as OperatorSpec
import Moonlight.Sheaf.Core.PruningSpec qualified as PruningSpec
import Moonlight.Sheaf.Core.RestrictionIndexSpec qualified as RestrictionIndexSpec
import Moonlight.Sheaf.Core.RestrictionLaws qualified as RestrictionLaws
import Moonlight.Sheaf.Core.Section.Congruence.EquivalenceSpec qualified as SectionCongruenceEquivalenceSpec
import Moonlight.Sheaf.Core.Section.RepairSpec qualified as SectionRepairSpec
import Moonlight.Sheaf.Core.Section.Stalk.CongruenceSpec qualified as SectionStalkCongruenceSpec
import Moonlight.Sheaf.Core.StalkLawSpec qualified as StalkLawSpec
import Moonlight.Sheaf.Core.StalkSpec qualified as StalkSpec
import Moonlight.Sheaf.Core.VerdictSpec qualified as VerdictSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "core"
    [ ContainerSpec.tests,
      CertifiedSpec.tests,
      FootprintSpec.tests,
      LinearizeSpec.tests,
      OperatorSpec.tests,
      PruningSpec.tests,
      RestrictionIndexSpec.tests,
      RestrictionLaws.tests,
      SectionCongruenceEquivalenceSpec.tests,
      SectionStalkCongruenceSpec.tests,
      SectionRepairSpec.tests,
      StalkLawSpec.tests,
      StalkSpec.tests,
      VerdictSpec.tests
    ]
