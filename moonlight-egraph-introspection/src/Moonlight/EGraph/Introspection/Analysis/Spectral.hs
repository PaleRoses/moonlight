{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Spectral
  ( FixpointStrategy (..),
    GrothendieckTraversalDiagnostics (..),
    GrothendieckTraversalHotspot (..),
    ScalarShadow (..),
    GrothendieckConsistencyProfile (..),
    allScalarShadows,
    grothendieckConsistencyProfile,
    grothendieckConsistencyProfileWith,
    grothendieckTarskiLaplacian,
    tarskiRestrictionLaplacian,
    semiringRestrictionLaplacian,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.Sheaf.Site
  ( GrothendieckCell,
    GrothendieckFaceMorphism,
    GrothendieckSite,
    grothendieckSiteBasis,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site (scsSite)
import Moonlight.Sheaf.Site
  ( SiteRestrictionWitness,
    buildGrothendieckRestrictions,
    buildGrothendieckRestrictionsWithStalkCache,
    buildNerveRestrictions,
    siteRestrictionStalkAlgebra,
  )
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle (..),
    ResolutionKernel (..),
    buildResolutionBundle,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, RewriteTag)
import Moonlight.Sheaf.Site (NerveCell, NerveSite, nerveSiteBasis)
import Moonlight.Sheaf.Site
  ( CompositionWitness (TerminalWitness),
    InterfaceMismatch,
    InterfaceStalk (..),
    grothendieckStalkFromCell,
    interfaceStalkAlgebra,
    interfaceStalkSignature,
  )
import Moonlight.Homology
  ( HomologyFailure (BackendFailure),
  )
import Moonlight.Probability.Core (Prob, mkProb)
import Moonlight.Sheaf.Cochain.Laplacian
  ( LaplacianKind (SemiringLaplacian, TarskiLaplacian),
    SheafLaplacian,
    buildSemiringLaplacian,
    buildTarskiLaplacian,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionProgram (..),
    runRuntimeResolutionProgramInitial,
  )
import Moonlight.Sheaf.Section.Stalk (StalkBounds (..))
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
  )
import Moonlight.Sheaf.Section.Store.State
  ( emptyTotalSectionStoreWith,
    totalStalkAt,
    updateStalkAtChecked,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( TotalSectionStore,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionParts (..),
    rKind,
    rSource,
    rTarget,
    rWitness,
    restrictApply,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelObjects,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.ObjectIndex (SheafModelVersion (..))
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    restrictionEntries,
    restrictionsTo,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    mergeStalks,
    stalkMismatches,
  )
import Numeric.Natural (Natural)
import Moonlight.Pale.Diagnostic.Gluing.Algebra (statsByCell)
import Moonlight.Pale.Diagnostic.Section.Propagation (RestrictionOutcomeStat (..))

type ScalarShadow :: Type
data ScalarShadow
  = DepthShadow
  | CostShadow
  | ContextBudgetShadow
  | DirectionCardinalityShadow
  | GuardComplexityShadow
  deriving stock (Eq, Ord, Show)

type FixpointStrategy :: Type
data FixpointStrategy
  = TarskiIteration
  | DiagnosticTraversal
  | ChainingIteration
  | WidenedIteration
  deriving stock (Eq, Ord, Show)

type GrothendieckTraversalHotspot :: (Type -> Type) -> Type
data GrothendieckTraversalHotspot f = GrothendieckTraversalHotspot
  { gthCell :: GrothendieckCell (RewriteSystem f),
    gthMismatchCount :: Int
  }
  deriving stock (Eq, Show)

type GrothendieckTraversalDiagnostics :: (Type -> Type) -> Type
data GrothendieckTraversalDiagnostics f = GrothendieckTraversalDiagnostics
  { gtdIterationCount :: Int,
    gtdChangedCells :: [GrothendieckCell (RewriteSystem f)],
    gtdResidualCells :: [GrothendieckCell (RewriteSystem f)],
    gtdHotspots :: [GrothendieckTraversalHotspot f],
    gtdMismatchTopology :: [((GrothendieckCell (RewriteSystem f), GrothendieckCell (RewriteSystem f)), Int)]
  }
  deriving stock (Eq, Show)

type GrothendieckConsistencyProfile :: (Type -> Type) -> Type
data GrothendieckConsistencyProfile f = GrothendieckConsistencyProfile
  { gcpConverged :: Bool,
    gcpResidualEnergy :: Double,
    gcpMismatchCount :: Int,
    gcpMismatchedCellCount :: Int,
    gcpTotalCellCount :: Int,
    gcpConsistencyRatio :: Maybe Prob,
    gcpTraversalDiagnostics :: Maybe (GrothendieckTraversalDiagnostics f)
  }
  deriving stock (Eq, Show)

type RewriteRestrictionWitness f =
  SiteRestrictionWitness
    (GrothendieckFaceMorphism (RewriteSystem f))
    (InterfaceStalk (RewriteTag f))

type RewriteRestrictionIndex f =
  RestrictionIndex
    (GrothendieckCell (RewriteSystem f))
    (RewriteRestrictionWitness f)

type RewriteInterfaceAlgebra :: (Type -> Type) -> Constraint
type RewriteInterfaceAlgebra f =
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f))

rewriteRestrictionAlgebra ::
  RewriteInterfaceAlgebra f =>
  StalkAlgebra
    (RewriteRestrictionWitness f)
    (InterfaceStalk (RewriteTag f))
    InterfaceMismatch
    ()
rewriteRestrictionAlgebra =
  siteRestrictionStalkAlgebra interfaceStalkAlgebra

allScalarShadows :: [ScalarShadow]
allScalarShadows =
  [ DepthShadow,
    CostShadow,
    ContextBudgetShadow,
    DirectionCardinalityShadow,
    GuardComplexityShadow
  ]

liftLaplacianBuildError ::
  Show cell =>
  Either
    (SheafOperatorBuildError cell)
    (SheafLaplacian kind cell) ->
  Either HomologyFailure (SheafLaplacian kind cell)
liftLaplacianBuildError =
  first (BackendFailure . show)

tarskiRestrictionLaplacian ::
  (HasConstructorTag f, ZipMatch f, Show (Pattern f)) =>
  NerveSite (RewriteTag f) ->
  Either HomologyFailure (SheafLaplacian 'TarskiLaplacian (NerveCell (RewriteTag f)))
tarskiRestrictionLaplacian siteValue = do
  registry <- first (BackendFailure . show) (buildNerveRestrictions siteValue)
  withRestrictionModelFromRegistry (nerveSiteBasis siteValue) registry $ \model ->
    liftLaplacianBuildError
      (buildTarskiLaplacian model)

semiringRestrictionLaplacian ::
  (HasConstructorTag f, ZipMatch f, Show (Pattern f)) =>
  NerveSite (RewriteTag f) ->
  Either HomologyFailure (SheafLaplacian 'SemiringLaplacian (NerveCell (RewriteTag f)))
semiringRestrictionLaplacian siteValue = do
  registry <- first (BackendFailure . show) (buildNerveRestrictions siteValue)
  withRestrictionModelFromRegistry (nerveSiteBasis siteValue) registry $ \model ->
    liftLaplacianBuildError
      (buildSemiringLaplacian model)

grothendieckTarskiLaplacian ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either
    HomologyFailure
    (SheafLaplacian 'TarskiLaplacian (GrothendieckCell (RewriteSystem f)))
grothendieckTarskiLaplacian rewriteSystem depthValue = do
  let siteValue = mkGrothendieckSite rewriteSystem depthValue
  registry <- first (BackendFailure . show) (buildGrothendieckRestrictions siteValue)
  withRestrictionModelFromRegistry (grothendieckSiteBasis siteValue) registry $ \model ->
    liftLaplacianBuildError
      (buildTarskiLaplacian model)

presentStoredRestriction :: Restriction cell witness -> RestrictionParts cell witness
presentStoredRestriction restriction =
  RestrictionParts
    { partKind = rKind restriction,
      partSource = rSource restriction,
      partTarget = rTarget restriction,
      partWitness = rWitness restriction
    }

withRestrictionModelFromRegistry ::
  Ord cell =>
  SheafBasis cell ->
  RestrictionIndex cell witness ->
  (forall owner. SheafModel owner cell witness -> Either HomologyFailure result) ->
  Either HomologyFailure result
withRestrictionModelFromRegistry basis registry useModel =
  first (const (BackendFailure "restriction sheaf model construction failed"))
    ( withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells basis))
      presentStoredRestriction
      (restrictionEntries registry)
      useModel
    )
    >>= id

grothendieckConsistencyProfile ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (GrothendieckConsistencyProfile f)
grothendieckConsistencyProfile =
  grothendieckConsistencyProfileWith TarskiIteration

grothendieckConsistencyProfileWith ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  FixpointStrategy ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (GrothendieckConsistencyProfile f)
grothendieckConsistencyProfileWith strategy rewriteSystem depthValue = do
  resolutionValue <- buildResolutionBundle rewriteSystem depthValue
  let scaffold = rkScaffold (rbKernel resolutionValue)
      stalkCache = rkStalkCache (rbKernel resolutionValue)
      siteValue = scsSite scaffold
      basis = grothendieckSiteBasis siteValue
  registry <- first (BackendFailure . show) (buildGrothendieckRestrictionsWithStalkCache siteValue stalkCache)
  withRestrictionModelFromRegistry basis registry $ \model ->
    let canonicalStalkAt cellValue =
          Map.findWithDefault (grothendieckStalkFromCell cellValue) cellValue stalkCache
        stalkBounds = rewriteStalkBounds canonicalStalkAt
        initialSection =
          initialSectionFor strategy stalkBounds canonicalStalkAt model
        resolutionProgram =
          buildResolutionProgram strategy stalkBounds basis model registry
     in runRuntimeResolutionProgramInitial
          resolutionProgram
          siteValue
          initialSection
          & first (BackendFailure . show)
          & fmap
            ( \(_siteAfterRun, _resolvedSection, report) ->
                consistencyProfileFromReport strategy basis report
            )

buildResolutionProgram ::
  RewriteInterfaceAlgebra f =>
  FixpointStrategy ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  SheafBasis (GrothendieckCell (RewriteSystem f)) ->
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  RewriteRestrictionIndex f ->
  RuntimeResolutionProgram
    owner
    (GrothendieckSite (RewriteSystem f))
    (GrothendieckCell (RewriteSystem f))
    (InterfaceStalk (RewriteTag f))
    (GrothendieckResolutionReport f)
    String
buildResolutionProgram strategy stalkBounds basis model registry =
  RuntimeResolutionProgram
    { rrpInitialDirtyCells = Set.fromList (basisCells basis),
      rrpRunDirtyCells =
        \frontier siteValue sectionValue ->
          fmap
            ( \(resolvedSection, report) ->
                (siteValue, resolvedSection, report)
            )
            (runResolutionStep strategy stalkBounds model registry frontier sectionValue)
    }

type GrothendieckResolutionReport :: (Type -> Type) -> Type
data GrothendieckResolutionReport f = GrothendieckResolutionReport
  { grrSettled :: !Bool,
    grrIterationCount :: !Int,
    grrChangedCells :: !(Set.Set (GrothendieckCell (RewriteSystem f))),
    grrResidualEnergy :: !Double,
    grrRestrictionOutcomeStats :: ![RestrictionOutcomeStat (GrothendieckCell (RewriteSystem f)) InterfaceMismatch]
  }

type ProjectionAcceleration :: Type
data ProjectionAcceleration
  = ChainingAcceleration
  | WidenedAcceleration

initialSectionFor ::
  FixpointStrategy ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  (GrothendieckCell (RewriteSystem f) -> InterfaceStalk (RewriteTag f)) ->
  SheafModel owner (GrothendieckCell (RewriteSystem f)) witness ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f))
initialSectionFor strategy stalkBounds canonicalStalkAt model =
  emptyTotalSectionStoreWith
    model
    ( case strategy of
        ChainingIteration -> stalkBottomAt stalkBounds
        WidenedIteration -> stalkBottomAt stalkBounds
        TarskiIteration -> canonicalStalkAt
        DiagnosticTraversal -> canonicalStalkAt
    )

runResolutionStep ::
  RewriteInterfaceAlgebra f =>
  FixpointStrategy ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  RewriteRestrictionIndex f ->
  Set.Set (GrothendieckCell (RewriteSystem f)) ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  Either
    String
    ( TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)),
      GrothendieckResolutionReport f
    )
runResolutionStep strategy stalkBounds model registry frontier sectionValue = do
  (resolvedSection, changedCells, residualEnergy, iterationCount) <-
    case strategy of
      TarskiIteration ->
        Right (sectionValue, Set.empty, 0, 0)
      DiagnosticTraversal ->
        Right (sectionValue, Set.empty, 0, 0)
      ChainingIteration ->
        acceleratedResolutionStep ChainingAcceleration stalkBounds model registry frontier sectionValue
      WidenedIteration ->
        acceleratedResolutionStep WidenedAcceleration stalkBounds model registry frontier sectionValue
  let mismatchStats =
        restrictionStatsForSection model registry resolvedSection
  Right
    ( resolvedSection,
      GrothendieckResolutionReport
        { grrSettled = null mismatchStats,
          grrIterationCount = iterationCount,
          grrChangedCells = changedCells,
          grrResidualEnergy = residualEnergy,
          grrRestrictionOutcomeStats = mismatchStats
        }
    )

acceleratedResolutionStep ::
  RewriteInterfaceAlgebra f =>
  ProjectionAcceleration ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  RewriteRestrictionIndex f ->
  Set.Set (GrothendieckCell (RewriteSystem f)) ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  Either
    String
    ( TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)),
      Set.Set (GrothendieckCell (RewriteSystem f)),
      Double,
      Int
    )
acceleratedResolutionStep acceleration stalkBounds model registry frontier sectionValue =
  let updates =
        frontier
          & Set.toList
          & mapMaybe (projectionUpdateForCell acceleration stalkBounds model registry sectionValue)
   in if null updates
        then Right (sectionValue, Set.empty, 0, 1)
        else
          first show (applyProjectionUpdates model updates sectionValue)
            & fmap
              ( \updatedSection ->
                  ( updatedSection,
                    Set.fromList (fmap (\(cellValue, _, _) -> cellValue) updates),
                    sum (fmap (\(_, _, residualEnergy) -> residualEnergy) updates),
                    1
                    )
              )

projectionUpdateForCell ::
  RewriteInterfaceAlgebra f =>
  ProjectionAcceleration ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  RewriteRestrictionIndex f ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  GrothendieckCell (RewriteSystem f) ->
  Maybe (GrothendieckCell (RewriteSystem f), InterfaceStalk (RewriteTag f), Double)
projectionUpdateForCell acceleration stalkBounds model registry sectionValue targetCell = do
  currentStalk <- stalkMaybe model targetCell sectionValue
  let incomingEntries =
        restrictionsTo (sheafModelObjects model) targetCell registry
      candidateStalks =
        incomingEntries
          & mapMaybe
            (\entryValue -> fmap (restrictApply rewriteRestrictionAlgebra entryValue) (stalkMaybe model (rSource entryValue) sectionValue))
  projectedStalk <-
        case candidateStalks of
          [] -> Just (stalkTopAt stalkBounds targetCell)
          _ -> foldProjectionStalks (stalkBottomAt stalkBounds targetCell) candidateStalks
  nextStalk <-
    accelerateProjectedStalk acceleration stalkBounds targetCell currentStalk projectedStalk
  if interfaceStalkSignature nextStalk == interfaceStalkSignature currentStalk
    then Nothing
    else
      Just
        ( targetCell,
          nextStalk,
          fromIntegral (length (stalkMismatches interfaceStalkAlgebra currentStalk nextStalk))
        )


stalkMaybe ::
  Ord cell =>
  SheafModel owner cell witness ->
  cell ->
  TotalSectionStore owner cell stalk ->
  Maybe stalk
stalkMaybe model cell sectionValue =
  case totalStalkAt model cell sectionValue of
    Left _ ->
      Nothing
    Right stalkValue ->
      Just stalkValue

restrictionStatsForSection ::
  RewriteInterfaceAlgebra f =>
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  RewriteRestrictionIndex f ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  [RestrictionOutcomeStat (GrothendieckCell (RewriteSystem f)) InterfaceMismatch]
restrictionStatsForSection model registry sectionValue =
  restrictionEntries registry >>= restrictionStatsForEntry
  where
    restrictionStatsForEntry entryValue =
      case
        ( stalkMaybe model (rSource entryValue) sectionValue,
          stalkMaybe model (rTarget entryValue) sectionValue
        )
      of
        (Just sourceStalk, Just targetStalk) ->
          fmap
            ( \mismatchValue ->
                RestrictionOutcomeStat
                  { rosSourceCell = rSource entryValue,
                    rosTargetCell = rTarget entryValue,
                    rosMismatch = mismatchValue,
                    rosOccurrences = 1
                  }
            )
            ( stalkMismatches
                interfaceStalkAlgebra
                (restrictApply rewriteRestrictionAlgebra entryValue sourceStalk)
                targetStalk
            )
        _ ->
          []

foldProjectionStalks ::
  RewriteInterfaceAlgebra f =>
  InterfaceStalk (RewriteTag f) ->
  [InterfaceStalk (RewriteTag f)] ->
  Maybe (InterfaceStalk (RewriteTag f))
foldProjectionStalks initialStalk candidates =
  foldr
    ( \candidate continue accumulator ->
        case mergeStalks interfaceStalkAlgebra candidate accumulator of
          Left _ ->
            Nothing
          Right merged ->
            continue merged
    )
    Just
    candidates
    initialStalk

accelerateProjectedStalk ::
  RewriteInterfaceAlgebra f =>
  ProjectionAcceleration ->
  StalkBounds cell (InterfaceStalk (RewriteTag f)) ->
  cell ->
  InterfaceStalk (RewriteTag f) ->
  InterfaceStalk (RewriteTag f) ->
  Maybe (InterfaceStalk (RewriteTag f))
accelerateProjectedStalk acceleration stalkBounds cellValue currentStalk projectedStalk =
  case mergeStalks interfaceStalkAlgebra currentStalk projectedStalk of
    Left _ ->
      Nothing
    Right mergedStalk ->
      let chainedStalk =
            clampInterfaceStalk
              (stalkTopAt stalkBounds cellValue)
              mergedStalk
       in Just $
            case acceleration of
              ChainingAcceleration ->
                chainedStalk
              WidenedAcceleration ->
                clampInterfaceStalk
                  (stalkTopAt stalkBounds cellValue)
                  (widenStalkAt stalkBounds cellValue currentStalk chainedStalk)

applyProjectionUpdates ::
  SheafModel owner (GrothendieckCell (RewriteSystem f)) (RewriteRestrictionWitness f) ->
  [(GrothendieckCell (RewriteSystem f), InterfaceStalk (RewriteTag f), Double)] ->
  TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  Either
    String
    (TotalSectionStore owner (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)))
applyProjectionUpdates model updates sectionValue =
  foldr
    ( \(cellValue, stalkValue, _) continue updatedSection ->
        first show (updateStalkAtChecked model cellValue (const stalkValue) updatedSection)
          >>= continue
    )
    Right
    updates
    sectionValue

rewriteStalkBounds ::
  (GrothendieckCell (RewriteSystem f) -> InterfaceStalk (RewriteTag f)) ->
  StalkBounds (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f))
rewriteStalkBounds canonicalStalkAt =
  StalkBounds
    { stalkTopAt = canonicalStalkAt,
      stalkBottomAt = rewriteStalkBottom . canonicalStalkAt,
      widenStalkAt =
        \cellValue currentStalk nextStalk ->
          if interfaceStalkSignature currentStalk == interfaceStalkSignature nextStalk
            then nextStalk
            else canonicalStalkAt cellValue
    }

rewriteStalkBottom :: InterfaceStalk (RewriteTag f) -> InterfaceStalk (RewriteTag f)
rewriteStalkBottom topStalk =
  InterfaceStalk
    { rsBoundNames = Set.empty,
      rsDeletedNames = Set.empty,
      rsCreatedNames = Set.empty,
      rsGuarded = False,
      rsWitness = TerminalWitness,
      rsCellDimension = rsCellDimension topStalk
    }

clampInterfaceStalk :: InterfaceStalk (RewriteTag f) -> InterfaceStalk (RewriteTag f) -> InterfaceStalk (RewriteTag f)
clampInterfaceStalk topStalk candidateStalk =
  InterfaceStalk
    { rsBoundNames = Set.intersection (rsBoundNames topStalk) (rsBoundNames candidateStalk),
      rsDeletedNames = Set.intersection (rsDeletedNames topStalk) (rsDeletedNames candidateStalk),
      rsCreatedNames = Set.intersection (rsCreatedNames topStalk) (rsCreatedNames candidateStalk),
      rsGuarded = rsGuarded topStalk && rsGuarded candidateStalk,
      rsWitness = rsWitness candidateStalk,
      rsCellDimension = rsCellDimension topStalk
    }

consistencyProfileFromReport ::
  FixpointStrategy ->
  SheafBasis (GrothendieckCell (RewriteSystem f)) ->
  GrothendieckResolutionReport f ->
  GrothendieckConsistencyProfile f
consistencyProfileFromReport strategy basis report =
  let mismatchStats = grrRestrictionOutcomeStats report
      mismatchedCellCount = Map.size (statsByCell mismatchStats)
      totalCellCount = length (basisCells basis)
   in GrothendieckConsistencyProfile
        { gcpConverged = grrSettled report,
          gcpResidualEnergy = grrResidualEnergy report,
          gcpMismatchCount = sum (fmap rosOccurrences mismatchStats),
          gcpMismatchedCellCount = mismatchedCellCount,
          gcpTotalCellCount = totalCellCount,
          gcpConsistencyRatio = consistencyRatio totalCellCount mismatchedCellCount,
          gcpTraversalDiagnostics =
            case strategy of
              TarskiIteration ->
                Nothing
              DiagnosticTraversal ->
                Just (traversalDiagnosticsFromReport report)
              ChainingIteration ->
                Nothing
              WidenedIteration ->
                Nothing
        }

traversalDiagnosticsFromReport ::
  GrothendieckResolutionReport f ->
  GrothendieckTraversalDiagnostics f
traversalDiagnosticsFromReport report =
  let mismatchStats = grrRestrictionOutcomeStats report
      cellMismatchCounts = statsByCell mismatchStats
   in GrothendieckTraversalDiagnostics
        { gtdIterationCount = grrIterationCount report,
          gtdChangedCells = Set.toAscList (grrChangedCells report),
          gtdResidualCells = Map.keys cellMismatchCounts,
          gtdHotspots =
            cellMismatchCounts
              & Map.toList
              & fmap
                ( \(cellValue, mismatchCount) ->
                    GrothendieckTraversalHotspot
                      { gthCell = cellValue,
                        gthMismatchCount = mismatchCount
                      }
                ),
          gtdMismatchTopology =
            mismatchStats
              & fmap
                ( \statValue ->
                    ( (rosSourceCell statValue, rosTargetCell statValue),
                      rosOccurrences statValue
                    )
                )
        }

consistencyRatio :: Int -> Int -> Maybe Prob
consistencyRatio totalCellCount mismatchedCellCount =
  if totalCellCount <= 0 || mismatchedCellCount < 0 || mismatchedCellCount > totalCellCount
    then Nothing
    else either (const Nothing) Just (mkProb (fromIntegral (totalCellCount - mismatchedCellCount) / fromIntegral totalCellCount))
