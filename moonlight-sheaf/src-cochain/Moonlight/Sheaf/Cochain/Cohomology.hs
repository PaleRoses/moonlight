{-# LANGUAGE ConstraintKinds #-}

module Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    cochainSupportWindow,
    buildNerveCochainArtifact,
    buildGrothendieckCochainArtifact,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree (..),
    transposeBoundaryIncidence,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundarySpec (..),
    buildCoboundaryComplex,
    buildRankOneCoboundaryComplex,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization (..),
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold (..),
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckFaceMorphism,
    GrothendieckSite,
    grothendieckCellDimension,
    grothendieckSiteCellsAtDimension,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceMorphism,
    NerveCell,
    NerveMorphism,
    NerveSite,
    NerveSiteAlgebra,
    NerveSource,
    nerveCellKey,
    nerveSiteBasis,
    nerveSiteCategory,
  )
import Moonlight.Sheaf.Site.Stalk.Restriction
  ( SiteRestrictionWitness,
    buildGrothendieckRestrictions,
    buildNerveRestrictions,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( InterfaceDomain (..),
    InterfaceMorphism,
    InterfaceObject,
    InterfaceStalk,
    grothendieckStalkFromCell,
    stalkFromCell,
  )
import Moonlight.Sheaf.Site.System
  ( SystemMor,
    SystemOb,
    SystemTag,
  )
import Moonlight.Sheaf.Site.Skeleton.Window
  ( SiteSkeletonWindow (..),
  )
import Numeric.Natural (Natural)
import Moonlight.Pale.Diagnostic.Site.Cohomology
  ( CoboundaryConstructionError (CoboundaryOperatorBuildError),
  )

data SiteCoboundaryRealization cell witness stalk
  = ExplicitSiteCoboundary (StalkLinearization stalk Int)
  | RankOneSiteCoboundary (Restriction cell witness -> stalk -> stalk -> Int)

data SiteCochainInput site cell
  = MaterializedSite site
  | ScaffoldedSite (SiteComplexScaffold site cell)

type NerveInterfaceCochain tag =
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  )

type GrothendieckInterfaceCochain system =
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  )

cochainSupportWindow :: Natural -> SiteSkeletonWindow
cochainSupportWindow maxCoboundarySourceDimension =
  SiteSkeletonWindow
    { sswCellDimensions = Set.fromDistinctAscList [0 .. maxCoboundarySourceDimension + 1],
      sswFaceSourceDimensions = Set.fromDistinctAscList [1 .. maxCoboundarySourceDimension + 1]
    }

buildNerveCochainArtifact ::
  NerveInterfaceCochain tag =>
  SiteCoboundaryRealization
    (NerveCell tag)
    (SiteRestrictionWitness (FaceMorphism tag) (InterfaceStalk tag))
    (InterfaceStalk tag) ->
  (GradedComplex (NerveCell tag) Int -> Either (SheafOperatorBuildError (NerveCell tag)) artifact) ->
  SiteCochainInput (NerveSite tag) (NerveCell tag) ->
  Either CoboundaryConstructionError artifact
buildNerveCochainArtifact realization buildArtifact =
  buildSiteCochainArtifactWithRealization
    buildArtifact
    buildNerveRestrictions
    (\siteValue -> stalkFromCell (nerveSiteCategory siteValue))
    realization
    mkSpec

buildGrothendieckCochainArtifact ::
  GrothendieckInterfaceCochain system =>
  SiteCoboundaryRealization
    (GrothendieckCell system)
    (SiteRestrictionWitness (GrothendieckFaceMorphism system) (InterfaceStalk (SystemTag system)))
    (InterfaceStalk (SystemTag system)) ->
  (GradedComplex (GrothendieckCell system) Int -> Either (SheafOperatorBuildError (GrothendieckCell system)) artifact) ->
  SiteCochainInput (GrothendieckSite system) (GrothendieckCell system) ->
  Either CoboundaryConstructionError artifact
buildGrothendieckCochainArtifact realization buildArtifact =
  buildSiteCochainArtifactWithRealization
    buildArtifact
    buildGrothendieckRestrictions
    (const grothendieckStalkFromCell)
    realization
    mkGrothendieckSpec

interfaceStalkCoboundaryBlock ::
  StalkLinearization stalk Int ->
  stalk ->
  stalk ->
  BoundaryIncidence Int
interfaceStalkCoboundaryBlock linearization faceStalk cofaceStalk =
  transposeBoundaryIncidence
    (slRestrictionIncidence linearization cofaceStalk faceStalk)

buildSiteCochainArtifactWithRealization ::
  (Ord cell, Show cell, Show registryError) =>
  (GradedComplex cell Int -> Either (SheafOperatorBuildError cell) artifact) ->
  (site -> Either registryError (RestrictionIndex cell witness)) ->
  (site -> cell -> stalk) ->
  SiteCoboundaryRealization cell witness stalk ->
  (Int -> site -> CoboundarySpec cell) ->
  SiteCochainInput site cell ->
  Either CoboundaryConstructionError artifact
buildSiteCochainArtifactWithRealization buildArtifact buildRestrictions lookupCellStalk realization specAt inputValue = do
  let (siteValue, specFor) =
        resolvedSiteCochainInput specAt inputValue
  restrictions <-
    first (CoboundaryOperatorBuildError . show) (buildRestrictions siteValue)
  first sheafOperatorToConstructionError
    ( buildSiteCochainComplexForRealization
        (lookupCellStalk siteValue)
        realization
        (specFor 0)
        (specFor 1)
        restrictions
        >>= buildArtifact
    )

resolvedSiteCochainInput ::
  Ord cell =>
  (Int -> site -> CoboundarySpec cell) ->
  SiteCochainInput site cell ->
  (site, Int -> CoboundarySpec cell)
resolvedSiteCochainInput specAt inputValue =
  case inputValue of
    MaterializedSite siteValue ->
      (siteValue, \dimensionValue -> specAt dimensionValue siteValue)
    ScaffoldedSite scaffoldValue ->
      let siteValue = scsSite scaffoldValue
       in
        ( siteValue,
          \dimensionValue -> scaffoldSpecAt scaffoldValue dimensionValue siteValue
        )

buildSiteCochainComplexForRealization ::
  Ord cell =>
  (cell -> stalk) ->
  SiteCoboundaryRealization cell witness stalk ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildSiteCochainComplexForRealization lookupCellStalk realization spec0 spec1 restrictions =
  case realization of
    ExplicitSiteCoboundary linearization ->
      buildCoboundaryComplex
        lookupCellStalk
        (slStalkDimension linearization)
        (interfaceStalkCoboundaryBlock linearization)
        spec0
        spec1
        restrictions
    RankOneSiteCoboundary scalarCoefficient ->
      buildRankOneCoboundaryComplex
        lookupCellStalk
        scalarCoefficient
        spec0
        spec1
        restrictions

mkSpec :: Int -> NerveSite f -> CoboundarySpec (NerveCell f)
mkSpec =
  mkDimensionSpec basisAtDimension

basisAtDimension :: Int -> NerveSite f -> SheafBasis (NerveCell f)
basisAtDimension =
  basisFromDimensionCells
    nerveCellDimensionInt
    (\_dimensionValue siteValue -> basisCells (nerveSiteBasis siteValue))

nerveCellDimensionInt :: NerveCell tag -> Int
nerveCellDimensionInt =
  fromIntegral . ckDimension . nerveCellKey

mkGrothendieckSpec ::
  Int ->
  GrothendieckSite system ->
  CoboundarySpec (GrothendieckCell system)
mkGrothendieckSpec =
  mkDimensionSpec grothendieckBasisAtDimension

scaffoldSpecAt ::
  Ord cell =>
  SiteComplexScaffold site cell ->
  Int ->
  site ->
  CoboundarySpec cell
scaffoldSpecAt scaffoldValue =
  mkDimensionSpec
    ( \dimensionValue _siteValue ->
        mkSheafBasis
          (Map.findWithDefault [] dimensionValue (scsCellsByDimension scaffoldValue))
    )

mkDimensionSpec ::
  (Int -> site -> SheafBasis cell) ->
  Int ->
  site ->
  CoboundarySpec cell
mkDimensionSpec basisAt sourceDimensionValue siteValue =
  CoboundarySpec
    { csDimension = HomologicalDegree sourceDimensionValue,
      csSourceBasis = basisAt sourceDimensionValue siteValue,
      csTargetBasis = basisAt (sourceDimensionValue + 1) siteValue
    }

grothendieckBasisAtDimension ::
  Int ->
  GrothendieckSite system ->
  SheafBasis (GrothendieckCell system)
grothendieckBasisAtDimension =
  basisFromDimensionCells
    grothendieckCellDimension
    (\dimensionValue siteValue -> grothendieckSiteCellsAtDimension siteValue (fromIntegral dimensionValue))

basisFromDimensionCells ::
  Ord cell =>
  (cell -> Int) ->
  (Int -> site -> [cell]) ->
  Int ->
  site ->
  SheafBasis cell
basisFromDimensionCells dimensionOf cellsAtDimension dimensionValue siteValue =
  mkSheafBasis
    (filter ((== dimensionValue) . dimensionOf) (cellsAtDimension dimensionValue siteValue))

sheafOperatorToConstructionError ::
  Show cell =>
  SheafOperatorBuildError cell ->
  CoboundaryConstructionError
sheafOperatorToConstructionError =
  CoboundaryOperatorBuildError . show
