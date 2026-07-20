module Moonlight.Sheaf.Cochain.Suite
  ( stressOptions,
    tests,
  )
where

import Moonlight.Sheaf.Cochain.CoboundaryNilpotenceSpec qualified as CoboundaryNilpotenceSpec
import Moonlight.Sheaf.Cochain.CoboundarySpec qualified as CoboundarySpec
import Moonlight.Sheaf.Cochain.LaplacianSpec qualified as LaplacianSpec
import Moonlight.Sheaf.Cochain.SiteCohomologySpec qualified as SiteCohomologySpec
import Moonlight.Sheaf.Cochain.StressSpec qualified as StressSpec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Options (OptionDescription)

stressOptions :: [OptionDescription]
stressOptions =
  StressSpec.stressOptions

tests :: TestTree
tests =
  testGroup
    "cochain"
    [ CoboundaryNilpotenceSpec.tests,
      CoboundarySpec.tests,
      LaplacianSpec.tests,
      SiteCohomologySpec.tests,
      StressSpec.tests
    ]
