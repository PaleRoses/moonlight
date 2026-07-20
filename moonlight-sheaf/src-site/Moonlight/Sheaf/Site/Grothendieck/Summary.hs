module Moonlight.Sheaf.Site.Grothendieck.Summary
  ( GrothendieckNilpotentSystem (..),
    GrothendieckStructuralSummary (..),
    summarizeGrothendieckSystem,
  )
where

import Data.Kind (Constraint, Type)
import Data.Maybe (isJust, isNothing)
import Moonlight.Homology (HomologyFailure, freeBettiVector)
import Moonlight.Sheaf.Site.Context.Presentation (ContextPresentationSystem (..))
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckMor (..),
    GrothendieckSite,
    grothendieckCategory,
    grothendieckSiteCells,
    grothendieckSiteFaceMorphisms,
    grothendieckMorphisms,
    grothendieckObjects,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold,
    mkGrothendieckComplexScaffold,
    scsChainComplex,
    scsSite,
  )
import Moonlight.Sheaf.Site.System (AnalyzableSystem, LatticeAnalyzableSystem)
import Moonlight.Category.Simplicial (pi0Nerve)
import Numeric.Natural (Natural)
import Moonlight.Pale.Diagnostic.Global.Summary (GrothendieckStructuralSummary (..))
import Moonlight.Pale.Diagnostic.Site.Cohomology (CoboundaryNilpotenceEvidence)
import Moonlight.Pale.Diagnostic.Site.Homotopy (NerveHomotopyProfile (..))

type GrothendieckNilpotentSystem :: Type -> Constraint
class GrothendieckNilpotentSystem system where
  grothendieckCoboundaryNilpotenceEvidence :: system -> Natural -> CoboundaryNilpotenceEvidence

summarizeGrothendieckSystem ::
  ( GrothendieckNilpotentSystem system,
    ContextPresentationSystem system,
    LatticeAnalyzableSystem system
  ) =>
  system ->
  Natural ->
  Either HomologyFailure GrothendieckStructuralSummary
summarizeGrothendieckSystem systemValue depthValue = do
  let siteValue = mkGrothendieckSite systemValue depthValue
  analysisScaffold <- mkGrothendieckComplexScaffold siteValue
  let contextPresentationValue = systemContextPresentation systemValue
      morphisms = grothendieckMorphisms contextPresentationValue
   in pure
        GrothendieckStructuralSummary
          { gssHomotopyProfile = scaffoldHomotopyProfile systemValue analysisScaffold,
            gssCellCount = length (grothendieckSiteCells (scsSite analysisScaffold)),
            gssFaceCount = length (grothendieckSiteFaceMorphisms (scsSite analysisScaffold)),
            gssObjectCount = length (grothendieckObjects contextPresentationValue),
            gssMorphismCount = length morphisms,
            gssCrossContextMorphismCount = length (filter isCrossContext morphisms),
            gssVerticalMorphismCount = length (filter isVertical morphisms),
            gssDiagonalMorphismCount = length (filter isDiagonal morphisms),
            gssCoboundaryNilpotenceEvidence = grothendieckCoboundaryNilpotenceEvidence systemValue depthValue
          }

scaffoldHomotopyProfile ::
  (ContextPresentationSystem system, LatticeAnalyzableSystem system) =>
  system ->
  SiteComplexScaffold (GrothendieckSite system) (GrothendieckCell system) ->
  NerveHomotopyProfile
scaffoldHomotopyProfile systemValue analysisScaffold =
  NerveHomotopyProfile
    { nhpConnectedComponents = length (pi0Nerve (grothendieckCategory systemValue)),
      nhpBettiVector = freeBettiVector (scsChainComplex analysisScaffold)
    }

isCrossContext :: AnalyzableSystem system => GrothendieckMor system -> Bool
isCrossContext morphismValue =
  gmSourceContext morphismValue /= gmTargetContext morphismValue

isVertical :: AnalyzableSystem system => GrothendieckMor system -> Bool
isVertical morphismValue =
  isCrossContext morphismValue && isNothing (gmTargetMorphism morphismValue)

isDiagonal :: AnalyzableSystem system => GrothendieckMor system -> Bool
isDiagonal morphismValue =
  isCrossContext morphismValue && isJust (gmTargetMorphism morphismValue)
