module Moonlight.Sheaf.Surface.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Surface.ApiSurfaceSpec qualified as ApiSurfaceSpec
import Moonlight.Sheaf.Surface.ObstructionSurfaceSpec qualified as ObstructionSurfaceSpec
import Moonlight.Sheaf.Surface.PublicSpec qualified as PublicSpec
import Moonlight.Sheaf.Surface.SiteSurfaceSpec qualified as SiteSurfaceSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-sheaf-public"
    [ ApiSurfaceSpec.tests,
      SiteSurfaceSpec.tests,
      ObstructionSurfaceSpec.tests,
      PublicSpec.tests
    ]
