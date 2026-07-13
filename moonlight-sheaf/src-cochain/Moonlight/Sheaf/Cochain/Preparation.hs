{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Cochain.Preparation
  ( SiteCochainPhase (..),
    RawSiteCochain (..),
    MorseReducedSiteCochain (..),
    SpectralReadySiteCochain (..),
    SiteCochainIteration (..),
    SiteCochainPreparationError (..),
    prepareGrothendieckCochainReduced,
    prepareGrothendieckCochainReducedWith,
    prepareGrothendieckCochainSpectralWith,
    prepareRawGrothendieckCochain,
    prepareRawNerveCochain,
    prepareMorseReducedSiteCochain,
    prepareNerveCochainReduced,
    prepareNerveCochainReducedWith,
    prepareNerveCochainSpectralWith,
    prepareSpectralReadySiteCochain,
    prepareSpectralReadySiteCochainWith,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Homology
  ( BasisCellRef,
    HomologyFailure,
    FilteredRefinedMorseComplex,
    RationalSpectralPage,
    computeRationalSpectralPages,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    frmcRefinedMorseComplex,
    rmcReducedComplex,
  )
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildGrothendieckCochainArtifact,
    buildNerveCochainArtifact,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold (..),
    SiteComplexScaffoldError,
    SiteMorseReduction (..),
    defaultSiteCellHeuristic,
    mkGrothendieckComplexScaffold,
    mkNerveComplexScaffold,
    reduceSiteComplexWith,
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckSite,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( NerveCell,
    NerveMorphism,
    NerveSite,
    NerveSiteAlgebra (..),
    NerveSource,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( InterfaceComposeError,
    InterfaceDomain,
    InterfaceMorphism,
    InterfaceObject,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Moonlight.Sheaf.Site.System (SystemMor, SystemOb, SystemTag)
import Moonlight.Pale.Diagnostic.Site.Cohomology (CoboundaryConstructionError)

type SiteCochainPhase :: Type
data SiteCochainPhase
  = RawSiteCochainPhase
  | MorseReducedSiteCochainPhase
  | SpectralReadySiteCochainPhase

type RawSiteCochain :: Type -> Type -> Type
data RawSiteCochain site cell = RawSiteCochain
  { rsciSiteComplex :: SiteComplexScaffold site cell,
    rsciCochainComplex :: GradedComplex cell Int
  }

type MorseReducedSiteCochain :: Type -> Type -> Type
data MorseReducedSiteCochain site cell = MorseReducedSiteCochain
  { mrscRaw :: SiteCochainIteration 'RawSiteCochainPhase site cell,
    mrscReduction :: SiteMorseReduction site cell
  }

type SpectralReadySiteCochain :: Type -> Type -> Type
data SpectralReadySiteCochain site cell = SpectralReadySiteCochain
  { srscSiteComplex :: SiteComplexScaffold site cell,
    srscOriginalFiltration :: BasisCellRef -> Int,
    srscReducedFiltration :: BasisCellRef -> Int,
    srscFilteredMorse :: FilteredRefinedMorseComplex Rational,
    srscSpectralPages :: [RationalSpectralPage]
  }

type SiteCochainIteration :: SiteCochainPhase -> Type -> Type -> Type
data SiteCochainIteration phase site cell where
  RawIteration ::
    RawSiteCochain site cell ->
    SiteCochainIteration 'RawSiteCochainPhase site cell
  MorseReducedIteration ::
    MorseReducedSiteCochain site cell ->
    SiteCochainIteration 'MorseReducedSiteCochainPhase site cell
  SpectralReadyIteration ::
    SpectralReadySiteCochain site cell ->
    SiteCochainIteration 'SpectralReadySiteCochainPhase site cell

type SiteCochainPreparationError :: Type
data SiteCochainPreparationError
  = SiteCochainRawConstructionFailed CoboundaryConstructionError
  | SiteCochainComplexScaffoldFailed HomologyFailure
  | SiteCochainReductionFailed SiteComplexScaffoldError
  | SiteCochainFilteredReductionFailed HomologyFailure
  | SiteCochainSpectralFailed HomologyFailure
  deriving stock (Eq, Show)

prepareRawNerveCochain ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveSite tag ->
  Either SiteCochainPreparationError (SiteCochainIteration 'RawSiteCochainPhase (NerveSite tag) (NerveCell tag))
prepareRawNerveCochain =
  prepareRawSiteCochain
    mkNerveComplexScaffold
    ( buildNerveCochainArtifact
        (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
        Right
    )

prepareNerveCochainReduced ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveSite tag ->
  Either SiteCochainPreparationError (SiteCochainIteration 'MorseReducedSiteCochainPhase (NerveSite tag) (NerveCell tag))
prepareNerveCochainReduced =
  prepareNerveCochainReducedWith (const defaultSiteCellHeuristic)

prepareNerveCochainReducedWith ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  (SiteComplexScaffold (NerveSite tag) (NerveCell tag) -> BasisCellRef -> Double) ->
  NerveSite tag ->
  Either SiteCochainPreparationError (SiteCochainIteration 'MorseReducedSiteCochainPhase (NerveSite tag) (NerveCell tag))
prepareNerveCochainReducedWith cellScore siteValue = do
  rawIteration <- prepareRawNerveCochain siteValue
  prepareMorseReducedSiteCochain (cellScore (rawIterationScaffold rawIteration)) rawIteration

prepareNerveCochainSpectralWith ::
  (SiteComplexScaffold (NerveSite tag) (NerveCell tag) -> BasisCellRef -> Double) ->
  (BasisCellRef -> Int) ->
  NerveSite tag ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase (NerveSite tag) (NerveCell tag))
prepareNerveCochainSpectralWith =
  prepareSiteCochainSpectralWith mkNerveComplexScaffold

prepareRawGrothendieckCochain ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckSite system ->
  Either SiteCochainPreparationError (SiteCochainIteration 'RawSiteCochainPhase (GrothendieckSite system) (GrothendieckCell system))
prepareRawGrothendieckCochain =
  prepareRawSiteCochain
    mkGrothendieckComplexScaffold
    ( buildGrothendieckCochainArtifact
        (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
        Right
    )

prepareGrothendieckCochainReduced ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckSite system ->
  Either SiteCochainPreparationError (SiteCochainIteration 'MorseReducedSiteCochainPhase (GrothendieckSite system) (GrothendieckCell system))
prepareGrothendieckCochainReduced =
  prepareGrothendieckCochainReducedWith (const defaultSiteCellHeuristic)

prepareGrothendieckCochainReducedWith ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  (SiteComplexScaffold (GrothendieckSite system) (GrothendieckCell system) -> BasisCellRef -> Double) ->
  GrothendieckSite system ->
  Either SiteCochainPreparationError (SiteCochainIteration 'MorseReducedSiteCochainPhase (GrothendieckSite system) (GrothendieckCell system))
prepareGrothendieckCochainReducedWith cellScore siteValue = do
  rawIteration <- prepareRawGrothendieckCochain siteValue
  prepareMorseReducedSiteCochain (cellScore (rawIterationScaffold rawIteration)) rawIteration

prepareGrothendieckCochainSpectralWith ::
  (SiteComplexScaffold (GrothendieckSite system) (GrothendieckCell system) -> BasisCellRef -> Double) ->
  (BasisCellRef -> Int) ->
  GrothendieckSite system ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase (GrothendieckSite system) (GrothendieckCell system))
prepareGrothendieckCochainSpectralWith =
  prepareSiteCochainSpectralWith mkGrothendieckComplexScaffold

prepareRawSiteCochain ::
  (site -> Either HomologyFailure (SiteComplexScaffold site cell)) ->
  (SiteCochainInput site cell -> Either CoboundaryConstructionError (GradedComplex cell Int)) ->
  site ->
  Either SiteCochainPreparationError (SiteCochainIteration 'RawSiteCochainPhase site cell)
prepareRawSiteCochain mkScaffold buildCochain siteValue = do
  siteComplexValue <-
    first SiteCochainComplexScaffoldFailed (mkScaffold siteValue)
  cochainComplexValue <-
    first
      SiteCochainRawConstructionFailed
      (buildCochain (ScaffoldedSite siteComplexValue))
  pure
    ( RawIteration
        RawSiteCochain
          { rsciSiteComplex = siteComplexValue,
            rsciCochainComplex = cochainComplexValue
          }
    )

prepareSiteCochainSpectralWith ::
  (site -> Either HomologyFailure (SiteComplexScaffold site cell)) ->
  (SiteComplexScaffold site cell -> BasisCellRef -> Double) ->
  (BasisCellRef -> Int) ->
  site ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase site cell)
prepareSiteCochainSpectralWith mkScaffold cellScore originalFiltration siteValue = do
  siteComplexValue <-
    first SiteCochainComplexScaffoldFailed (mkScaffold siteValue)
  prepareSpectralReadySiteCochainFromScaffoldWith
    (cellScore siteComplexValue)
    originalFiltration
    siteComplexValue

prepareMorseReducedSiteCochain ::
  (BasisCellRef -> Double) ->
  SiteCochainIteration 'RawSiteCochainPhase site cell ->
  Either SiteCochainPreparationError (SiteCochainIteration 'MorseReducedSiteCochainPhase site cell)
prepareMorseReducedSiteCochain cellScore rawIteration@(RawIteration rawValue) = do
  reductionValue <-
    first
      SiteCochainReductionFailed
      (reduceSiteComplexWith cellScore (rsciSiteComplex rawValue))
  pure
    ( MorseReducedIteration
        MorseReducedSiteCochain
          { mrscRaw = rawIteration,
            mrscReduction = reductionValue
          }
    )

prepareSpectralReadySiteCochain ::
  (BasisCellRef -> Int) ->
  SiteCochainIteration 'MorseReducedSiteCochainPhase site cell ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase site cell)
prepareSpectralReadySiteCochain =
  prepareSpectralReadySiteCochainWith defaultSiteCellHeuristic

prepareSpectralReadySiteCochainWith ::
  (BasisCellRef -> Double) ->
  (BasisCellRef -> Int) ->
  SiteCochainIteration 'MorseReducedSiteCochainPhase site cell ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase site cell)
prepareSpectralReadySiteCochainWith cellScore originalFiltration (MorseReducedIteration reducedValue) =
  prepareSpectralReadySiteCochainFromScaffoldWith
    cellScore
    originalFiltration
    (smrScaffold (mrscReduction reducedValue))

prepareSpectralReadySiteCochainFromScaffoldWith ::
  (BasisCellRef -> Double) ->
  (BasisCellRef -> Int) ->
  SiteComplexScaffold site cell ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase site cell)
prepareSpectralReadySiteCochainFromScaffoldWith cellScore originalFiltration siteComplexValue = do
  filteredMorseValue <-
    first
      SiteCochainFilteredReductionFailed
      ( filteredRefinedMorseComplex
          (scsChainComplex siteComplexValue)
          originalFiltration
          cellScore
      )
  prepareSpectralReadySiteCochainFromFiltered
    filteredMorseValue
    originalFiltration
    siteComplexValue

prepareSpectralReadySiteCochainFromFiltered ::
  FilteredRefinedMorseComplex Rational ->
  (BasisCellRef -> Int) ->
  SiteComplexScaffold site cell ->
  Either SiteCochainPreparationError (SiteCochainIteration 'SpectralReadySiteCochainPhase site cell)
prepareSpectralReadySiteCochainFromFiltered filteredMorseValue originalFiltration siteComplexValue =
  let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
      reducedFiltration = filteredReducedFiltration filteredMorseValue
   in do
        spectralPagesValue <-
          first
            SiteCochainSpectralFailed
            (computeRationalSpectralPages (rmcReducedComplex refinedMorseValue) reducedFiltration)
        pure
          ( SpectralReadyIteration
              SpectralReadySiteCochain
                { srscSiteComplex = siteComplexValue,
                  srscOriginalFiltration = originalFiltration,
                  srscReducedFiltration = reducedFiltration,
                  srscFilteredMorse = filteredMorseValue,
                  srscSpectralPages = spectralPagesValue
                }
          )

rawIterationScaffold ::
  SiteCochainIteration 'RawSiteCochainPhase site cell ->
  SiteComplexScaffold site cell
rawIterationScaffold (RawIteration rawValue) =
  rsciSiteComplex rawValue
