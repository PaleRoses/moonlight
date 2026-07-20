{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Obstruction
  ( ConstantDerivedProfile (..),
    ObstructionClass,
    ObstructionInterpretation,
    ocDegree,
    ocCocycleRepresentative,
    ocSupportingCells,
    ocDerivedProfile,
    ocInterpretation,
    oiCellEvaluations,
    oiWitnessEvidence,
    oiObstructedCells,
    oiComposedCells,
    oiHarmonicLoops,
    oiHarmonicFailure,
    interpretObstructionRepresentative,
    nerveObstructions,
    nerveObstructionsAtDegree,
    nerveObstructionTower,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Analysis.Obstruction qualified as Generic
import Moonlight.Analysis.Obstruction
  ( ConstantDerivedProfile (..),
    ocDegree,
    ocCocycleRepresentative,
    ocSupportingCells,
    ocDerivedProfile,
    ocInterpretation,
    oiCellEvaluations,
    oiWitnessEvidence,
    oiObstructedCells,
    oiComposedCells,
    oiHarmonicLoops,
    oiHarmonicFailure,
  )
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.Derived.Morse (hypercohomologyDims)
import Moonlight.Sheaf.Site
  ( GrothendieckCell,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site
  ( SiteComplexScaffold,
    mkGrothendieckComplexScaffold,
    scsBasisRefs,
    scsChainComplex,
    scsSite,
  )
import Moonlight.Derived.Site (derivedFromFiniteChainComplex)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, RewriteTag)
import Moonlight.Sheaf.Site
  ( CompositionWitness (..),
    grothendieckStalkFromCell,
    InterfaceStalk (..),
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyFailure (..),
    RepresentativeCocycle,
    basisCellNodeId,
  )
import Numeric.Natural (Natural)

type ObstructionClass :: (Type -> Type) -> Type
type ObstructionClass f =
  Generic.ObstructionClass
    (GrothendieckCell (RewriteSystem f))
    (CompositionWitness (RewriteTag f))

type ObstructionInterpretation :: (Type -> Type) -> Type
type ObstructionInterpretation f =
  Generic.ObstructionInterpretation
    (GrothendieckCell (RewriteSystem f))
    (CompositionWitness (RewriteTag f))

nerveObstructions ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure [ObstructionClass f]
nerveObstructions rewriteSystem depthValue =
  nerveObstructionsAtDegree rewriteSystem depthValue (HomologicalDegree 1)

nerveObstructionsAtDegree ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  HomologicalDegree ->
  Either HomologyFailure [ObstructionClass f]
nerveObstructionsAtDegree rewriteSystem depthValue degreeValue = do
  obstructionContext <- buildObstructionContext rewriteSystem depthValue
  Generic.obstructionClassesAtDegree obstructionContext degreeValue

nerveObstructionTower ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  [HomologicalDegree] ->
  Either HomologyFailure [[ObstructionClass f]]
nerveObstructionTower rewriteSystem depthValue degreeValues = do
  obstructionContext <- buildObstructionContext rewriteSystem depthValue
  Generic.obstructionTower obstructionContext degreeValues

interpretObstructionRepresentative ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  RepresentativeCocycle Rational Int ->
  Either HomologyFailure (ObstructionClass f)
interpretObstructionRepresentative rewriteSystem depthValue cocycleRepresentative = do
  obstructionContext <- buildObstructionContext rewriteSystem depthValue
  Generic.interpretObstructionRepresentative obstructionContext cocycleRepresentative

buildObstructionContext ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (Generic.ObstructionContext (GrothendieckCell (RewriteSystem f)) (CompositionWitness (RewriteTag f)))
buildObstructionContext rewriteSystem depthValue = do
  analysisScaffold <- mkGrothendieckComplexScaffold (mkGrothendieckSite rewriteSystem depthValue)
  let finiteComplex = scsChainComplex analysisScaffold
      basisRefs = scsBasisRefs analysisScaffold
      basisNodeId = basisCellNodeId finiteComplex
  ambientDerived <-
    first (BackendFailure . show) (derivedFromFiniteChainComplex finiteComplex)
  ambientHypercohomology <-
    first (BackendFailure . show) (hypercohomologyDims ambientDerived)
  let (harmonicLoops, harmonicFailure) =
        Generic.harmonicEnrichmentFromComplex finiteComplex
  pure
    ( Generic.mkObstructionContext
        finiteComplex
        basisRefs
        basisNodeId
        (rsWitness . grothendieckStalkFromCell)
        rewriteWitnessClassifier
        ambientDerived
        ambientHypercohomology
        harmonicLoops
        harmonicFailure
    )

rewriteWitnessClassifier :: Generic.WitnessClassifier (CompositionWitness (RewriteTag f))
rewriteWitnessClassifier =
  Generic.WitnessClassifier
    { Generic.wcIsObstructed = \case
        ObstructedWitness _ -> True
        _ -> False,
      Generic.wcIsComposed = \case
        ComposedWitness _ -> True
        _ -> False
    }
