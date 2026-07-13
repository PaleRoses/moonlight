module Main where

import Test.Tasty (defaultMain, testGroup)
import ClosedSupportSpec qualified
import ExceptionalPullbackSpec qualified
import ExceptionalPushforwardSpec qualified
import InjectiveComplexSpec qualified
import LabeledMatrixSpec qualified
import PosetCohomologySpec qualified
import PosetSpec qualified
import PresentationBuilderSpec qualified
import ResolutionSpec qualified
import SixFunctorSmokeSpec qualified
import SparseValidationSpec qualified
import TensorSpec qualified
import TriangulatedSpec qualified
import VerdierDualSpec qualified
import qualified Moonlight.Derived.Effect.Laws as DerivedLaws

main :: IO ()
main = defaultMain $ testGroup "moonlight-derived"
  [ PosetSpec.tests
  , LabeledMatrixSpec.tests
  , InjectiveComplexSpec.tests
  , SparseValidationSpec.tests
  , PosetCohomologySpec.tests
  , ResolutionSpec.tests
  , VerdierDualSpec.tests
  , ExceptionalPushforwardSpec.tests
  , ExceptionalPullbackSpec.tests
  , ClosedSupportSpec.tests
  , SixFunctorSmokeSpec.tests
  , TensorSpec.tests
  , PresentationBuilderSpec.tests
  , TriangulatedSpec.tests
  , DerivedLaws.tests
  ]
