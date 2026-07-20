module Main
  ( main,
  )
where

import Moonlight.Cosheaf.Suite qualified as CosheafSuite
import Moonlight.Sheaf.Cochain.Suite qualified as CochainSuite
import Moonlight.Sheaf.Core.Suite qualified as CoreSuite
import Moonlight.Sheaf.Descent.Suite qualified as DescentSuite
import Moonlight.Sheaf.Effect.Laws qualified as EffectLaws
import Moonlight.Sheaf.Obstruction.Suite qualified as ObstructionSuite
import Moonlight.Sheaf.Presheaf.Suite qualified as PresheafSuite
import Moonlight.Sheaf.Propagation.Suite qualified as PropagationSuite
import Moonlight.Sheaf.Query.Suite qualified as QuerySuite
import Moonlight.Sheaf.Runtime.Suite qualified as RuntimeSuite
import Moonlight.Sheaf.Site.Suite qualified as SiteSuite
import Moonlight.Sheaf.Surface.OwnerForgerySpec qualified as OwnerForgerySpec
import Test.Tasty
  ( defaultIngredients,
    defaultMainWithIngredients,
    includingOptions,
    testGroup,
  )

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions CochainSuite.stressOptions : defaultIngredients)
    ( testGroup
        "moonlight-sheaf"
        [ CoreSuite.tests,
          SiteSuite.tests,
          PresheafSuite.tests,
          CosheafSuite.tests,
          CochainSuite.tests,
          DescentSuite.tests,
          RuntimeSuite.tests,
          QuerySuite.tests,
          ObstructionSuite.tests,
          EffectLaws.tests,
          PropagationSuite.tests,
          OwnerForgerySpec.tests
        ]
    )
