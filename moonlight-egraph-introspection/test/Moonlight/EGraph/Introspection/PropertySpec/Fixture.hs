module Moonlight.EGraph.Introspection.PropertySpec.Fixture
  ( ArithF,
    GeneratedContextPair (..),
    GeneratedRewriteSystem (..),
    analysisDepth,
    generatedRewriteSystemProperty,
    generatedContextPairProperty,
    withMorseReduction,
    normalizeBettiVector,
    zeroDegreeBetti,
    chainComplexNilpotent,
    obstructionRepresentativeClosed,
    morseInequalityHolds,
  )
where

import Data.Function ((&))
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Analysis.Reduction
import Moonlight.EGraph.Introspection.Analysis.Obstruction qualified as IntrospectionObstruction
import Moonlight.EGraph.Introspection.Analysis.Resolution
import Moonlight.EGraph.Introspection.Core.Rewrite
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.EGraph.Introspection.Arbitrary
  ( ArithF,
    GeneratedContextPair (..),
    GeneratedRewriteSystem (..)
  )
import Moonlight.Sheaf.Site
  ( SiteComplexScaffold (..),
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    RepresentativeCocycle,
    amCriticalCells,
    boundaryCoefficient,
    boundaryEntries,
    boundaryIncidenceApply,
    composeBoundaryIncidence,
    freeBettiVector,
    incidenceMatrixAt,
    mapBoundaryCoefficients,
    maxHomologicalDegree,
    representativeDegree,
    representativeTerms,
    transposeBoundaryIncidence
  )
import Test.Tasty.QuickCheck (Property, counterexample, property)

analysisDepth :: Integer
analysisDepth = 2

generatedRewriteSystemProperty :: GeneratedRewriteSystem -> (RewriteSystem ArithF -> Either String Bool) -> Property
generatedRewriteSystemProperty generatedRewriteSystem predicate =
  counterexample
    ("generated rewrite system: " <> show generatedRewriteSystem)
    ( case predicate (grsSystem generatedRewriteSystem) of
        Left failureMessage ->
          counterexample failureMessage False
        Right propertyHolds ->
          property propertyHolds
    )

generatedContextPairProperty :: GeneratedContextPair -> (GeneratedContextPair -> Bool) -> Property
generatedContextPairProperty generatedContextPair predicate =
  counterexample
    ("generated contexts: " <> show generatedContextPair)
    (property (predicate generatedContextPair))

reduceNerveWithMorse ::
  RewriteSystem ArithF ->
  Natural ->
  Either
    HomologyFailure
    (MorseReduction (GrothendieckSite (RewriteSystem ArithF)) (GrothendieckCell (RewriteSystem ArithF)) (CompositionWitness (RewriteTag ArithF)))
reduceNerveWithMorse rewriteSystem depthValue = do
  resolutionValue <- buildResolutionBundle rewriteSystem depthValue
  morseValue <- raMorse (rbAnalysis resolutionValue)
  let analysisScaffold = rkScaffold (rbKernel resolutionValue)
      siteValue = scsSite analysisScaffold
  buildReduction
    ReductionScaffold
      { rsSite = siteValue,
        rsOriginalComplex = scsChainComplex analysisScaffold,
        rsMorseComplex = morseValue,
        rsBasisRefs = scsBasisRefs analysisScaffold,
        rsZeroCells = Map.findWithDefault [] 0 (scsCellsByDimension analysisScaffold),
        rsIncidentUpperCells =
          \cellValue ->
            grothendieckSiteFaceMorphisms siteValue
              & filter ((== cellValue) . grothendieckFaceMorphismTarget)
              & fmap grothendieckFaceMorphismSource
              & filter ((== 1) . grothendieckCellDimension),
        rsCellWeight = stalkWeight,
        rsUpperWitnessAtCell = rsWitness . grothendieckStalkFromCell
      }

stalkWeight :: GrothendieckCell (RewriteSystem ArithF) -> Double
stalkWeight cellValue =
  let stalkValue = grothendieckStalkFromCell cellValue
   in fromIntegral
        ( Set.size (rsBoundNames stalkValue)
            + Set.size (rsDeletedNames stalkValue)
            + Set.size (rsCreatedNames stalkValue)
            + if rsGuarded stalkValue then 1 else 0
        )

withMorseReduction ::
  GeneratedRewriteSystem ->
  (MorseReduction (GrothendieckSite (RewriteSystem ArithF)) (GrothendieckCell (RewriteSystem ArithF)) (CompositionWitness (RewriteTag ArithF)) -> Bool) ->
  Property
withMorseReduction generatedRewriteSystem predicate =
  generatedRewriteSystemProperty generatedRewriteSystem $ \rewriteSystem ->
    case reduceNerveWithMorse rewriteSystem (fromIntegral analysisDepth) of
      Left failure ->
        Left ("Morse reduction failed: " <> show failure)
      Right reductionValue ->
        Right (predicate reductionValue)

normalizeBettiVector :: [Int] -> [Int]
normalizeBettiVector =
  dropWhileEnd (== 0)

zeroDegreeBetti :: [Int] -> Int
zeroDegreeBetti bettiVector =
  maybe 0 id (listToMaybe bettiVector)

chainComplexNilpotent :: FiniteChainComplex Int -> Bool
chainComplexNilpotent finiteChainComplex =
  [1 .. max 0 (maxDegreeValue - 1)]
    & all
      ( \degreeValue ->
          composeBoundaryIncidence
            (incidenceMatrixAt finiteChainComplex (HomologicalDegree degreeValue))
            (incidenceMatrixAt finiteChainComplex (HomologicalDegree (degreeValue + 1)))
            & either (const False) (all ((== 0) . boundaryCoefficient) . boundaryEntries)
      )
  where
    HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteChainComplex

obstructionRepresentativeClosed ::
  FiniteChainComplex Int ->
  IntrospectionObstruction.ObstructionClass ArithF ->
  Bool
obstructionRepresentativeClosed chainComplexValue obstructionClass =
  let cocycleRepresentative = IntrospectionObstruction.ocCocycleRepresentative obstructionClass
   in IntrospectionObstruction.ocDegree obstructionClass == HomologicalDegree 1
        && all ((== 1) . grothendieckCellDimension) (IntrospectionObstruction.ocSupportingCells obstructionClass)
        && cocycleClosed chainComplexValue cocycleRepresentative

cocycleClosed :: FiniteChainComplex Int -> RepresentativeCocycle Rational Int -> Bool
cocycleClosed chainComplexValue cocycleRepresentative =
  let HomologicalDegree degreeValue = representativeDegree cocycleRepresentative
      coboundaryValue =
        incidenceMatrixAt chainComplexValue (HomologicalDegree (degreeValue + 1))
          & transposeBoundaryIncidence
          & mapBoundaryCoefficients fromIntegral
      cochainVector =
        representativeTerms cocycleRepresentative
          & fmap (\(coefficientValue, basisIndexValue) -> (basisIndexValue, coefficientValue))
          & Map.fromListWith (+)
          & Map.filter (/= 0)
   in boundaryIncidenceApply coboundaryValue cochainVector
        & Map.elems
        & all (== 0)

morseInequalityHolds :: MorseReduction site cell witness -> Bool
morseInequalityHolds reductionValue =
  let criticalCounts =
        amCriticalCells (mrMatching reductionValue)
          & fmap (\BasisCellRef {cellDegree = HomologicalDegree degreeValue} -> (degreeValue, 1 :: Int))
          & Map.fromListWith (+)
      bettiCounts =
        normalizeBettiVector (freeBettiVector (mrOriginalComplex reductionValue))
          & zip [0 ..]
          & Map.fromList
      degreesToCheck =
        Set.toAscList (Map.keysSet criticalCounts `Set.union` Map.keysSet bettiCounts)
   in all
        (\degreeValue -> Map.findWithDefault 0 degreeValue criticalCounts >= Map.findWithDefault 0 degreeValue bettiCounts)
        degreesToCheck
