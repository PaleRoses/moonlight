{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Resolution.Descent
  ( DescentEnrichment (..),
    enrichWithDescent,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.EGraph.Introspection.Analysis.Descent (DescentPage (..), computeDescentPage)
import Moonlight.EGraph.Homology.Gerbe (isGerbeTrivial)
import Moonlight.Sheaf.Site (GrothendieckCell)
import Moonlight.Sheaf.Site (scsBasisRefs, scsChainComplex)
import Moonlight.EGraph.Homology.Representative (representativeKey)
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionAnalysisAlg (..),
    ResolutionBundle (..),
    ResolutionKernel (..),
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem)
import Moonlight.Sheaf.Section.Stalk.Groupoid
  ( InterfaceStalkGroupoid,
    maxInterfaceStalkAutomorphismCount,
  )
import Moonlight.Homology
  ( FormalMap (..),
    HomologicalDegree (..),
    HomologyFailure,
    RepresentativeCocycle,
    basisCellNodeId,
  )

type DescentEnrichment :: (Type -> Type) -> Type
data DescentEnrichment f = DescentEnrichment
  { deResolution :: ResolutionBundle f,
    deDescentPage :: Maybe (DescentPage Rational),
    dePhantomObstructionCount :: Int,
    deGroupoidProvider :: GrothendieckCell (RewriteSystem f) -> InterfaceStalkGroupoid
  }

enrichWithDescent ::
  ResolutionBundle f ->
  (GrothendieckCell (RewriteSystem f) -> InterfaceStalkGroupoid) ->
  Either HomologyFailure (DescentEnrichment f)
enrichWithDescent resolutionValue groupoidProvider =
  let finiteComplex = scsChainComplex (rkScaffold (rbKernel resolutionValue))
      automorphismCounts =
        automorphismCountsFromResolution resolutionValue groupoidProvider
   in do
        cocycleValues <- raRepresentativeCocycles (rbAnalysis resolutionValue) (HomologicalDegree 1)
        gerbeTrivial <- isGerbeTrivial finiteComplex automorphismCounts
        descentPage <-
          if gerbeTrivial
            then Right Nothing
            else Just <$> computeDescentPage finiteComplex automorphismCounts
        pure
          DescentEnrichment
            { deResolution = resolutionValue,
              deDescentPage = descentPage,
              dePhantomObstructionCount =
                maybe 0 (`phantomCountFromCocycles` cocycleValues) descentPage,
              deGroupoidProvider = groupoidProvider
            }

automorphismCountsFromResolution ::
  ResolutionBundle f ->
  (GrothendieckCell (RewriteSystem f) -> InterfaceStalkGroupoid) ->
  IntMap Int
automorphismCountsFromResolution resolutionValue groupoidProvider =
  let scaffold = rkScaffold (rbKernel resolutionValue)
      finiteComplex = scsChainComplex scaffold
   in scsBasisRefs scaffold
        & Map.toList
        & fmap
          ( \(cellValue, basisCellRef) ->
              ( basisCellNodeId finiteComplex basisCellRef,
                cellAutomorphismCount (groupoidProvider cellValue)
              )
          )
        & IntMap.fromList

cellAutomorphismCount :: InterfaceStalkGroupoid -> Int
cellAutomorphismCount =
  maxInterfaceStalkAutomorphismCount

phantomCountFromCocycles :: DescentPage Rational -> [RepresentativeCocycle Rational Int] -> Int
phantomCountFromCocycles descentPage cocycleRepresentatives =
  let phantomKeys = codomainKeysTouchedByD2 descentPage
   in cocycleRepresentatives
        & fmap representativeKey
        & filter (`Set.member` phantomKeys)
        & length

codomainKeysTouchedByD2 :: DescentPage Rational -> Set.Set String
codomainKeysTouchedByD2 descentPage =
  dpDifferentialD2 descentPage
    & Map.elems
    & foldr
      ( \formalMapValue accumulator ->
          let touchedRows =
                formalMatrix formalMapValue
                  & zip [0 :: Int ..]
                  & filter (any (/= 0) . snd)
                  & fmap fst
           in touchedRows
                & foldr
                  ( \rowIndexValue ->
                      case drop rowIndexValue (formalCodomainBasis formalMapValue) of
                        codomainRepresentative : _ ->
                          Set.insert (representativeKey codomainRepresentative)
                        [] ->
                          id
                  )
                  accumulator
      )
      Set.empty
