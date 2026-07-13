module Moonlight.Cosheaf.Suite
  ( tests,
  )
where

import Moonlight.Cosheaf.Chain.CoreSpec qualified as CosheafChainSpec
import Moonlight.Cosheaf.Chain.GradedComplexSpec qualified as CosheafGradedComplexSpec
import Moonlight.Cosheaf.Chain.LinearSpec qualified as CosheafLinearChainSpec
import Moonlight.Cosheaf.Core.ComputedExampleSpec qualified as CosheafComputedExampleSpec
import Moonlight.Cosheaf.Core.FiniteSpec qualified as CosheafFiniteSpec
import Moonlight.Cosheaf.Core.LawSpec qualified as CosheafLawSpec
import Moonlight.Cosheaf.Core.LinearSpec qualified as CosheafLinearSpec
import Moonlight.Cosheaf.Core.SupportSpec qualified as CosheafSupportSpec
import Moonlight.Cosheaf.Homology.CoverSpec qualified as CosheafCoverHomologySpec
import Moonlight.Cosheaf.Homology.Examples.H1CyclicGroupSpec qualified as CosheafH1CyclicGroupSpec
import Moonlight.Cosheaf.Homology.Examples.H1TriangleSpec qualified as CosheafH1TriangleSpec
import Moonlight.Cosheaf.Homology.Examples.H2TetrahedronBoundarySpec qualified as CosheafH2TetrahedronBoundarySpec
import Moonlight.Cosheaf.Homology.LiftSpec qualified as CosheafHomologyLiftSpec
import Moonlight.Cosheaf.Homology.TropicalSpec qualified as CosheafTropicalHomologySpec
import Moonlight.Cosheaf.Surface.RootSpec qualified as CosheafSurfaceSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "cosheaf"
    [ CosheafFiniteSpec.tests,
      CosheafGradedComplexSpec.tests,
      CosheafLinearSpec.tests,
      CosheafLinearChainSpec.tests,
      CosheafSupportSpec.tests,
      CosheafLawSpec.tests,
      CosheafComputedExampleSpec.tests,
      CosheafChainSpec.tests,
      CosheafSurfaceSpec.tests,
      CosheafCoverHomologySpec.tests,
      CosheafTropicalHomologySpec.tests,
      CosheafHomologyLiftSpec.tests,
      CosheafH1TriangleSpec.tests,
      CosheafH1CyclicGroupSpec.tests,
      CosheafH2TetrahedronBoundarySpec.tests
    ]
