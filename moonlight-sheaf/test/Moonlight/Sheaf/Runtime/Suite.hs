module Moonlight.Sheaf.Runtime.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Runtime.InferenceSpec qualified as InferenceSpec
import Moonlight.Sheaf.Runtime.LocalFactLaws qualified as LocalFactLaws
import Moonlight.Sheaf.Runtime.SchemaSpec qualified as SchemaSpec
import Moonlight.Sheaf.Runtime.TwistSpec qualified as TwistSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "runtime"
    [ TwistSpec.tests,
      LocalFactLaws.tests,
      SchemaSpec.tests,
      InferenceSpec.tests
    ]
