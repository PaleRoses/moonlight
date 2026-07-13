module CoFiniteTruthSpec
  ( tests,
  )
where

import Moonlight.Constraint
  ( CoFiniteTruth,
  )
import ConstraintArbitrary ()
import Moonlight.Constraint.Effect.Harness
  ( coFiniteTruthAbsorptionJoin,
    coFiniteTruthAbsorptionMeet,
    coFiniteTruthComplementInvolution,
    coFiniteTruthNormalizationIdempotent,
  )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

coFiniteTruthNormalizationIdempotentLaw :: CoFiniteTruth Int -> Bool
coFiniteTruthNormalizationIdempotentLaw value =
  coFiniteTruthNormalizationIdempotent value

coFiniteTruthAbsorptionJoinLaw :: CoFiniteTruth Int -> CoFiniteTruth Int -> Bool
coFiniteTruthAbsorptionJoinLaw left right =
  coFiniteTruthAbsorptionJoin left right

coFiniteTruthAbsorptionMeetLaw :: CoFiniteTruth Int -> CoFiniteTruth Int -> Bool
coFiniteTruthAbsorptionMeetLaw left right =
  coFiniteTruthAbsorptionMeet left right

coFiniteTruthComplementInvolutionLaw :: CoFiniteTruth Int -> Bool
coFiniteTruthComplementInvolutionLaw value =
  coFiniteTruthComplementInvolution value

tests :: TestTree
tests =
  testGroup
    "cofinite-truth"
    [ QC.testProperty "normalization_idempotent" coFiniteTruthNormalizationIdempotentLaw,
      QC.testProperty "absorption_join" coFiniteTruthAbsorptionJoinLaw,
      QC.testProperty "absorption_meet" coFiniteTruthAbsorptionMeetLaw,
      QC.testProperty "complement_involution" coFiniteTruthComplementInvolutionLaw
    ]
