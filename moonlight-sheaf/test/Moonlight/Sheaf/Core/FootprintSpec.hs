module Moonlight.Sheaf.Core.FootprintSpec
  ( tests,
  )
where

import Moonlight.Sheaf.Footprint
  ( FootprintMeasure (..),
    FootprintMeasureBasis (..),
    FootprintMeasureExactness (..),
    FootprintMeasureUnit (..),
    exactRepresentedFootprintMeasure,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "footprint"
    [ testCase "exact represented measures account for full retention" testExactRepresentedShape
    ]

testExactRepresentedShape :: Assertion
testExactRepresentedShape =
  exactRepresentedFootprintMeasure CandidateSeedUnit RepresentedCandidateCarrier 12
    @?= FootprintMeasure
      { fmUnit = CandidateSeedUnit,
        fmExactness = FootprintExactRepresented,
        fmTotal = Just 12,
        fmRetained = Just 12,
        fmPruned = Nothing,
        fmBasis = RepresentedCandidateCarrier
      }
