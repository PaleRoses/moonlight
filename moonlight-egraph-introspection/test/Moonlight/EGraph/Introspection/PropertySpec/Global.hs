module Moonlight.EGraph.Introspection.PropertySpec.Global
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
import Moonlight.EGraph.Introspection.PropertySpec.Fixture

tests :: TestTree
tests =
  testGroup
    "global"
    [ testProperty "Morse reduction preserves Betti data" propMorseBettiInvariance,
      testProperty "Morse differential remains nilpotent after reduction" propMorseDifferentialNilpotence,
      testProperty "critical-cell counts dominate Betti numbers" propMorseInequality
    ]

propMorseBettiInvariance :: GeneratedRewriteSystem -> Property
propMorseBettiInvariance generatedRewriteSystem =
  withMorseReduction generatedRewriteSystem $ \reductionValue ->
    normalizeBettiVector (freeBettiVector (mrOriginalComplex reductionValue))
      == normalizeBettiVector (freeBettiVector (mrReducedComplex reductionValue))

propMorseDifferentialNilpotence :: GeneratedRewriteSystem -> Property
propMorseDifferentialNilpotence generatedRewriteSystem =
  withMorseReduction generatedRewriteSystem (chainComplexNilpotent . mrReducedComplex)

propMorseInequality :: GeneratedRewriteSystem -> Property
propMorseInequality generatedRewriteSystem =
  withMorseReduction generatedRewriteSystem morseInequalityHolds
