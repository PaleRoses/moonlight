{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Reduction
  ( MorseReduction,
    mrSite,
    mrOriginalComplex,
    mrReducedComplex,
    mrPotential,
    mrSkeleton,
    mrCriticalNodes,
    mrCriticalCells,
    mrCollapsedPairs,
    mrCollapsedDerivations,
    mrRetainedCells,
    mrMatching,
    mrObstructions,
    mrMorseComplex,
    collapsedDerivations,
    criticalCells,
    nervePotential,
    reduceNerve,
    reduceNerveWithMorse,
    reducedComplex,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.Reduction qualified as Generic
import Moonlight.Analysis.Reduction
  ( mrSite,
    mrOriginalComplex,
    mrReducedComplex,
    mrPotential,
    mrSkeleton,
    mrCriticalNodes,
    mrCriticalCells,
    mrCollapsedPairs,
    mrCollapsedDerivations,
    mrRetainedCells,
    mrMatching,
    mrObstructions,
    mrMorseComplex,
  )
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.Sheaf.Site
  ( GrothendieckCell,
    GrothendieckSite,
    grothendieckCellDimension,
    grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    grothendieckSiteFaceMorphisms,
  )
import Moonlight.Sheaf.Site
  ( scsBasisRefs,
    scsCellsByDimension,
    scsChainComplex,
    scsSite,
  )
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionAnalysisAlg (..),
    ResolutionBundle (..),
    ResolutionKernel (..),
    RewriteSiteScaffold,
    buildResolutionBundle,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, RewriteTag)
import Moonlight.Sheaf.Site
  ( CompositionWitness,
    InterfaceStalk (..),
    grothendieckStalkFromCell,
  )
import Moonlight.Homology
  ( FiniteChainComplex,
    HomologyFailure,
    MorseComplex,
    ScalarPotentialField,
  )
import Numeric.Natural (Natural)

type MorseReduction :: (Type -> Type) -> Type
type MorseReduction f =
  Generic.MorseReduction
    (GrothendieckSite (RewriteSystem f))
    (GrothendieckCell (RewriteSystem f))
    (CompositionWitness (RewriteTag f))

reduceNerve ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (MorseReduction f)
reduceNerve = reduceNerveWithMorse

reduceNerveWithMorse ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (MorseReduction f)
reduceNerveWithMorse rewriteSystem depthValue = do
  resolutionValue <- buildResolutionBundle rewriteSystem depthValue
  morseValue <- raMorse (rbAnalysis resolutionValue)
  let analysisScaffold = rkScaffold (rbKernel resolutionValue)
  Generic.buildReduction
    (rewriteReductionScaffold analysisScaffold morseValue)

reducedComplex :: MorseReduction f -> FiniteChainComplex Int
reducedComplex = Generic.reducedComplex

criticalCells ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure [GrothendieckCell (RewriteSystem f)]
criticalCells rewriteSystem depthValue =
  Generic.mrCriticalCells <$> reduceNerveWithMorse rewriteSystem depthValue

collapsedDerivations ::
  MorseReduction f ->
  [(GrothendieckCell (RewriteSystem f), GrothendieckCell (RewriteSystem f), CompositionWitness (RewriteTag f))]
collapsedDerivations = Generic.collapsedDerivations

nervePotential ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure ScalarPotentialField
nervePotential rewriteSystem depthValue = do
  resolutionValue <- buildResolutionBundle rewriteSystem depthValue
  morseValue <- raMorse (rbAnalysis resolutionValue)
  let analysisScaffold = rkScaffold (rbKernel resolutionValue)
  Generic.potentialField
    (rewriteReductionScaffold analysisScaffold morseValue)

rewriteReductionScaffold ::
  (HasConstructorTag f, ZipMatch f) =>
  RewriteSiteScaffold f ->
  MorseComplex Int ->
  Generic.ReductionScaffold
    (GrothendieckSite (RewriteSystem f))
    (GrothendieckCell (RewriteSystem f))
    (CompositionWitness (RewriteTag f))
rewriteReductionScaffold analysisScaffold morseValue =
  let siteValue = scsSite analysisScaffold
   in Generic.ReductionScaffold
        { Generic.rsSite = siteValue,
          Generic.rsOriginalComplex = scsChainComplex analysisScaffold,
          Generic.rsMorseComplex = morseValue,
          Generic.rsBasisRefs = scsBasisRefs analysisScaffold,
          Generic.rsZeroCells = Map.findWithDefault [] 0 (scsCellsByDimension analysisScaffold),
          Generic.rsIncidentUpperCells = \zeroCell ->
            grothendieckSiteFaceMorphisms siteValue
              & filter ((== zeroCell) . grothendieckFaceMorphismTarget)
              & fmap grothendieckFaceMorphismSource
              & filter ((== 1) . grothendieckCellDimension),
          Generic.rsCellWeight = stalkWeight,
          Generic.rsUpperWitnessAtCell = rsWitness . grothendieckStalkFromCell
        }

stalkWeight :: (HasConstructorTag f, ZipMatch f) => GrothendieckCell (RewriteSystem f) -> Double
stalkWeight cellValue =
  let stalkValue = grothendieckStalkFromCell cellValue
   in fromIntegral
        ( Set.size (rsBoundNames stalkValue)
            + Set.size (rsDeletedNames stalkValue)
            + Set.size (rsCreatedNames stalkValue)
            + if rsGuarded stalkValue
              then 1
              else 0
        )
