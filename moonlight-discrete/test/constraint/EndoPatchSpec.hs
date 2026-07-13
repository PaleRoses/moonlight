module EndoPatchSpec
  ( tests,
  )
where

import Moonlight.Constraint
  ( CoFiniteTruth,
    EndoPatch,
    applyEndoPatch,
  )
import ConstraintArbitrary ()
import Moonlight.Constraint.Effect.Harness
  ( endoPatchActionComposition,
    endoPatchMonoidAssoc,
    endoPatchMonoidLeftId,
    endoPatchMonoidRightId,
  )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

endoPatchMonoidAssocLaw :: EndoPatch Int -> EndoPatch Int -> EndoPatch Int -> Bool
endoPatchMonoidAssocLaw first second third =
  endoPatchMonoidAssoc first second third

endoPatchMonoidLeftIdLaw :: EndoPatch Int -> Bool
endoPatchMonoidLeftIdLaw value =
  endoPatchMonoidLeftId value

endoPatchMonoidRightIdLaw :: EndoPatch Int -> Bool
endoPatchMonoidRightIdLaw value =
  endoPatchMonoidRightId value

endoPatchActionCompositionLaw :: EndoPatch Int -> EndoPatch Int -> CoFiniteTruth Int -> Bool
endoPatchActionCompositionLaw first second truth =
  endoPatchActionComposition first second truth

endoPatchActionIdentityLaw :: CoFiniteTruth Int -> Bool
endoPatchActionIdentityLaw truth =
  applyEndoPatch mempty truth == truth

tests :: TestTree
tests =
  testGroup
    "endopatch"
    [ QC.testProperty "monoid_assoc" endoPatchMonoidAssocLaw,
      QC.testProperty "monoid_left_id" endoPatchMonoidLeftIdLaw,
      QC.testProperty "monoid_right_id" endoPatchMonoidRightIdLaw,
      QC.testProperty "action_composition" endoPatchActionCompositionLaw,
      QC.testProperty "action_identity" endoPatchActionIdentityLaw
    ]
