module Moonlight.EGraph.Introspection.NerveSpec.Fixture.Section
  ( assertModuleSupportTrace,
    complexCellCount,
    morseCriticalCountBoundHolds,
    collapsedWitnessIsComposed,
    obstructionRepresentativeClosed,
    third,
    obstructionSignatures,
    normalizeBettiVector,
    normalizedObservedEdgeCoverage,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.EGraph.Introspection.Analysis.Obstruction qualified as IntrospectionObstruction
import Moonlight.EGraph.Introspection.NerveSpec.Fixture.Site
import Moonlight.EGraph.Introspection.NerveSpec.FixturePrelude
import Moonlight.Control.Scheduling.Perturbation
  ( PerturbationSample,
    sampleObservedEdgeCoverage,
  )
import Moonlight.Sheaf.Twist.Report qualified as Twist
assertModuleSupportTrace ::
  String ->
  Twist.SupportSaturationReport
    result
    guideTrace
    (Twist.SupportTraceEntry (SupportBasis TwistScopeCtx) ruleId)
    host ->
  Assertion
assertModuleSupportTrace failurePrefix supportReport =
  case Twist.ssrTrace supportReport of
    supportTraceEntry : _ ->
      assertEqual
        (failurePrefix <> " should preserve the module support witness")
        (principalSupport ModuleTwistCtx)
        (Twist.steSupport supportTraceEntry)
    [] ->
      pure ()

complexCellCount :: FiniteChainComplex Int -> Int
complexCellCount complexValue =
  [0 .. maxDegreeValue]
    & fmap (sourceCardinality . incidenceMatrixAt complexValue . HomologicalDegree)
    & sum
  where
    HomologicalDegree maxDegreeValue = maxHomologicalDegree complexValue

morseCriticalCountBoundHolds :: MorseReduction f -> Bool
morseCriticalCountBoundHolds reductionValue =
  let criticalCounts =
        mrMorseComplex reductionValue
          & mcMatching
          & amCriticalCells
          & fmap (\BasisCellRef {cellDegree = HomologicalDegree degreeValue} -> (degreeValue, 1 :: Int))
          & Map.fromListWith (+)
      bettiCounts =
        freeBettiVector (mrOriginalComplex reductionValue)
          & zip [0 ..]
          & Map.fromList
      degreesToCheck =
        Set.toAscList (Map.keysSet criticalCounts `Set.union` Map.keysSet bettiCounts)
   in all
        (\degreeValue -> Map.findWithDefault 0 degreeValue criticalCounts >= Map.findWithDefault 0 degreeValue bettiCounts)
        degreesToCheck

collapsedWitnessIsComposed :: CompositionWitness f -> Bool
collapsedWitnessIsComposed witnessValue =
  case witnessValue of
    ComposedWitness _ -> True
    TerminalWitness -> False
    ObstructedWitness _ -> False

obstructionRepresentativeClosed ::
  FiniteChainComplex Int ->
  IntrospectionObstruction.ObstructionClass ArithF ->
  Bool
obstructionRepresentativeClosed chainComplexValue obstructionClass =
  let HomologicalDegree degreeValue = representativeDegree (IntrospectionObstruction.ocCocycleRepresentative obstructionClass)
      coboundaryValue =
        incidenceMatrixAt chainComplexValue (HomologicalDegree (degreeValue + 1))
          & transposeBoundaryIncidence
          & mapBoundaryCoefficients fromIntegral
      cochainVector =
        representativeTerms (IntrospectionObstruction.ocCocycleRepresentative obstructionClass)
          & fmap (\(coefficientValue, basisIndexValue) -> (basisIndexValue, coefficientValue))
          & Map.fromListWith (+)
          & Map.filter (/= 0)
   in boundaryIncidenceApply coboundaryValue cochainVector
        & Map.elems
        & all (== 0)

third :: (a, b, c) -> c
third (_, _, value) = value

obstructionSignatures ::
  [IntrospectionObstruction.ObstructionClass ArithF] ->
  [(RepresentativeChain Rational Int, [Int], [Rational])]
obstructionSignatures =
  fmap
    ( \obstructionClass ->
        ( IntrospectionObstruction.ocCocycleRepresentative obstructionClass,
          fmap grothendieckCellDimension (IntrospectionObstruction.ocSupportingCells obstructionClass),
          fmap snd (IntrospectionObstruction.oiCellEvaluations (IntrospectionObstruction.ocInterpretation obstructionClass))
        )
    )

normalizeBettiVector :: [Int] -> [Int]
normalizeBettiVector =
  dropWhileEnd (== 0)

normalizedObservedEdgeCoverage :: PerturbationSample scope key -> Maybe Double
normalizedObservedEdgeCoverage =
  sampleObservedEdgeCoverage
