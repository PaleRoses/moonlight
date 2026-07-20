module Moonlight.EGraph.Introspection.Analysis.Descent
  ( DescentPage (..),
    computeDescentPage,
    descentCorrectedObstructions,
    phantomObstructionCount,
    descentCorrectedObstructionsFromTower,
    phantomObstructionCountFromTower,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.EGraph.Homology.Descent
  ( DescentPage (..),
    computeDescentPage,
    touchedCodomainKeys,
  )
import Moonlight.EGraph.Homology.Gerbe (isGerbeTrivial)
import Moonlight.EGraph.Homology.Representative (representativeKey)
import Moonlight.Sheaf.Site
  ( mkGrothendieckComplexScaffold,
    mkGrothendieckSite,
    scsChainComplex,
  )
import Moonlight.EGraph.Introspection.Analysis.Obstruction
  ( ObstructionClass,
    ocCocycleRepresentative,
    nerveObstructionTower,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem)
import Moonlight.Homology
  ( HomologicalDegree,
    HomologyFailure,
  )
import Numeric.Natural (Natural)

descentCorrectedObstructions :: DescentPage Rational -> [ObstructionClass f] -> [ObstructionClass f]
descentCorrectedObstructions descentPage obstructionValues =
  let phantomKeys = touchedCodomainKeys descentPage
   in filter
        ( \obstructionValue ->
            representativeKey (ocCocycleRepresentative obstructionValue)
              `Set.notMember` phantomKeys
        )
        obstructionValues

phantomObstructionCount :: DescentPage Rational -> [ObstructionClass f] -> Int
phantomObstructionCount descentPage obstructionValues =
  length obstructionValues - length (descentCorrectedObstructions descentPage obstructionValues)

descentCorrectedObstructionsFromTower ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  IntMap Int ->
  [HomologicalDegree] ->
  Either HomologyFailure [ObstructionClass f]
descentCorrectedObstructionsFromTower rewriteSystem depthValue automorphismCounts degreeValues = do
  analysisScaffold <- mkGrothendieckComplexScaffold (mkGrothendieckSite rewriteSystem depthValue)
  obstructionLayers <- nerveObstructionTower rewriteSystem depthValue degreeValues
  let obstructionValues = concat obstructionLayers
      finite = scsChainComplex analysisScaffold
  gerbeTrivial <- isGerbeTrivial finite automorphismCounts
  if gerbeTrivial
    then Right obstructionValues
    else do
      descentPage <- computeDescentPage finite automorphismCounts
      pure (descentCorrectedObstructions descentPage obstructionValues)

phantomObstructionCountFromTower ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  IntMap Int ->
  [HomologicalDegree] ->
  Either HomologyFailure Int
phantomObstructionCountFromTower rewriteSystem depthValue automorphismCounts degreeValues = do
  analysisScaffold <- mkGrothendieckComplexScaffold (mkGrothendieckSite rewriteSystem depthValue)
  obstructionLayers <- nerveObstructionTower rewriteSystem depthValue degreeValues
  let obstructionValues = concat obstructionLayers
      finite = scsChainComplex analysisScaffold
  gerbeTrivial <- isGerbeTrivial finite automorphismCounts
  if gerbeTrivial
    then Right 0
    else do
      descentPage <- computeDescentPage finite automorphismCounts
      pure (phantomObstructionCount descentPage obstructionValues)
