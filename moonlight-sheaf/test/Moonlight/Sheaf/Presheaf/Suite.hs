module Moonlight.Sheaf.Presheaf.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Presheaf.ClaimsSpec qualified as ClaimsSpec
import Moonlight.Sheaf.Presheaf.GluingSpec qualified as GluingSpec
import Moonlight.Sheaf.Presheaf.GoldenConstructionSpec qualified as GoldenConstructionSpec
import Moonlight.Sheaf.Presheaf.GoldenOperationSpec qualified as GoldenOperationSpec
import Moonlight.Sheaf.Presheaf.Image.AdjunctionSpec qualified as ImageAdjunctionSpec
import Moonlight.Sheaf.Presheaf.Image.ContextGaloisSpec qualified as ImageContextGaloisSpec
import Moonlight.Sheaf.Presheaf.Image.DirectSpec qualified as ImageDirectSpec
import Moonlight.Sheaf.Presheaf.MorphismCategorySpec qualified as MorphismCategorySpec
import Moonlight.Sheaf.Presheaf.PlusSpec qualified as PlusSpec
import Moonlight.Sheaf.Presheaf.PreparedCongruenceSheafificationSpec qualified as PreparedCongruenceSheafificationSpec
import Moonlight.Sheaf.Presheaf.SiteMapSpec qualified as SiteMapSpec
import Moonlight.Sheaf.Presheaf.StalkColimitSpec qualified as StalkColimitSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "presheaf"
    [ GluingSpec.tests,
      GoldenConstructionSpec.tests,
      ImageAdjunctionSpec.tests,
      ImageContextGaloisSpec.tests,
      ImageDirectSpec.tests,
      MorphismCategorySpec.compositionBoundaryTests,
      PreparedCongruenceSheafificationSpec.tests,
      GoldenOperationSpec.tests,
      PlusSpec.tests,
      SiteMapSpec.tests,
      StalkColimitSpec.tests,
      ClaimsSpec.tests
    ]
