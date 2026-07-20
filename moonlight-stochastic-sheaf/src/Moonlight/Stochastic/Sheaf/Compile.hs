{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Stochastic.Sheaf.Compile
  ( StochasticSite (..),
    StochasticViolation (..),
    StochasticArtifacts (..),
    compileStochasticSection,
    PossibilisticArtifacts (..),
    compilePossibilisticSection,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelBuildError (..),
    sheafModelRestrictions,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.ObjectIndex (SheafModelVersion (..))
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Store.State
  ( mkTotalSectionStore,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( SectionConstructionError (..),
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
  )
import Moonlight.Sheaf.Section.Morphism (RestrictionId)
import Moonlight.Sheaf.Section.Morphism (RestrictionParts (..), unitIncidenceRestriction)
import Moonlight.Stochastic.Sheaf.Algebra
  ( MarkovKernel,
    PossibilisticKernelWitness (..),
    StochasticKernelWitness (..),
  )
import Moonlight.Stochastic.Sheaf.Core
  ( PossibilisticSection,
    StochasticSection,
    possibilisticStalkFromStochastic,
    StochasticStalk,
  )
import Prelude

type StochasticSite :: Type -> Type -> Type
data StochasticSite cell a = StochasticSite
  { stochasticCells :: [cell],
    stochasticInitial :: Map cell (StochasticStalk a),
    stochasticKernels :: [(cell, cell, MarkovKernel a)]
  }


type StochasticViolation :: Type -> Type
data StochasticViolation cell
  = MissingInitialDistribution cell
  | UnknownInitialCell cell
  | KernelSourceOutsideBasis cell
  | KernelTargetOutsideBasis cell
  | KernelRestrictionUnknownId RestrictionId
  | KernelRestrictionDuplicateId RestrictionId
  | KernelRestrictionNonDenseId RestrictionId RestrictionId
  | KernelZeroIncidenceCoefficient cell cell
  deriving stock (Eq, Ord, Show)

type StochasticArtifacts :: Type -> Type -> Type -> Type
data StochasticArtifacts owner cell a = StochasticArtifacts
  { stochasticBasis :: SheafBasis cell,
    stochasticModel :: SheafModel owner cell (StochasticKernelWitness a),
    stochasticSection :: StochasticSection owner cell a,
    stochasticRestrictions :: RestrictionIndex cell (StochasticKernelWitness a)
  }

type role StochasticArtifacts nominal nominal nominal

type PossibilisticArtifacts :: Type -> Type -> Type -> Type
data PossibilisticArtifacts owner cell a = PossibilisticArtifacts
  { possibilisticBasis :: SheafBasis cell,
    possibilisticModel :: SheafModel owner cell (PossibilisticKernelWitness a),
    possibilisticSection :: PossibilisticSection owner cell a,
    possibilisticRestrictions :: RestrictionIndex cell (PossibilisticKernelWitness a)
  }

type role PossibilisticArtifacts nominal nominal nominal

compileStochasticSection ::
  Ord cell =>
  StochasticSite cell a ->
  (forall owner. StochasticArtifacts owner cell a -> result) ->
  Either [StochasticViolation cell] result
compileStochasticSection site useArtifacts =
  let basis = mkSheafBasis (stochasticCells site)
      siteCells = stochasticCells site
      basisSet = Set.fromList siteCells
      initialKeys = Map.keysSet (stochasticInitial site)
      missingViolations =
        fmap MissingInitialDistribution
          (filter (`Set.notMember` initialKeys) siteCells)
      extraViolations =
        fmap UnknownInitialCell
          (filter (`Set.notMember` basisSet) (Map.keys (stochasticInitial site)))
      kernelViolations =
        stochasticKernels site >>= \(sourceCell, targetCell, _) ->
          sourceViolation sourceCell <> targetViolation targetCell
      violations = missingViolations <> extraViolations <> kernelViolations
   in if null violations
        then
          withStochasticModel basis site $ \model ->
            case mkTotalSectionStore model (stochasticInitial site) of
              Left constructionError ->
                Left (sectionConstructionViolations constructionError)
              Right section ->
                Right
                  ( useArtifacts
                      StochasticArtifacts
                        { stochasticBasis = basis,
                          stochasticModel = model,
                          stochasticSection = section,
                          stochasticRestrictions = sheafModelRestrictions model
                        }
                  )
        else Left violations
  where
    sourceViolation sourceCell =
      if Set.member sourceCell (Set.fromList (stochasticCells site))
        then []
        else [KernelSourceOutsideBasis sourceCell]
    targetViolation targetCell =
      if Set.member targetCell (Set.fromList (stochasticCells site))
        then []
        else [KernelTargetOutsideBasis targetCell]

compilePossibilisticSection ::
  Ord cell =>
  StochasticSite cell a ->
  (forall owner. PossibilisticArtifacts owner cell a -> result) ->
  Either [StochasticViolation cell] result
compilePossibilisticSection site useArtifacts = do
  basis <- compileStochasticSection site stochasticBasis
  let
      possibilisticEntries =
        Map.map
          possibilisticStalkFromStochastic
          (stochasticInitial site)
  withPossibilisticModel basis site $ \model ->
    case mkTotalSectionStore model possibilisticEntries of
      Left constructionError ->
        Left (sectionConstructionViolations constructionError)
      Right section ->
        Right
          ( useArtifacts
              PossibilisticArtifacts
                { possibilisticBasis = basis,
                  possibilisticModel = model,
                  possibilisticSection = section,
                  possibilisticRestrictions = sheafModelRestrictions model
                }
          )

withStochasticModel ::
  Ord cell =>
  SheafBasis cell ->
  StochasticSite cell a ->
  (forall owner. SheafModel owner cell (StochasticKernelWitness a) -> Either [StochasticViolation cell] result) ->
  Either [StochasticViolation cell] result
withStochasticModel basis site useModel =
  case
    withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells basis))
      ( \(sourceCell, targetCell, kernel) ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = sourceCell,
              partTarget = targetCell,
              partWitness = StochasticKernelWitness kernel
            }
      )
      (stochasticKernels site)
      useModel
  of
    Left modelError -> Left [modelViolation modelError]
    Right outcome -> outcome

withPossibilisticModel ::
  Ord cell =>
  SheafBasis cell ->
  StochasticSite cell a ->
  (forall owner. SheafModel owner cell (PossibilisticKernelWitness a) -> Either [StochasticViolation cell] result) ->
  Either [StochasticViolation cell] result
withPossibilisticModel basis site useModel =
  case
    withPreparedSheafModel
      (SheafModelVersion 1)
      (mkObjectIndex (basisCells basis))
      ( \(sourceCell, targetCell, kernel) ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = sourceCell,
              partTarget = targetCell,
              partWitness = PossibilisticKernelWitness kernel
            }
      )
      (stochasticKernels site)
      useModel
  of
    Left modelError -> Left [modelViolation modelError]
    Right outcome -> outcome

sectionConstructionViolations ::
  SectionConstructionError cell ->
  [StochasticViolation cell]
sectionConstructionViolations constructionError =
  fmap MissingInitialDistribution (Set.toAscList (sceMissingCells constructionError))
    <> fmap UnknownInitialCell (Set.toAscList (sceExtraCells constructionError))

registryViolation ::
  RestrictionIndexError cell ->
  StochasticViolation cell
registryViolation registryError =
  case registryError of
    RestrictionUnknownSource cell ->
      KernelSourceOutsideBasis cell
    RestrictionUnknownTarget cell ->
      KernelTargetOutsideBasis cell
    RestrictionUnknownId restrictionId ->
      KernelRestrictionUnknownId restrictionId
    RestrictionDuplicateId restrictionId ->
      KernelRestrictionDuplicateId restrictionId
    RestrictionNonDenseId expectedId actualId ->
      KernelRestrictionNonDenseId expectedId actualId
    RestrictionZeroIncidenceCoefficient sourceCell targetCell ->
      KernelZeroIncidenceCoefficient sourceCell targetCell

modelViolation ::
  SheafModelBuildError cell ->
  StochasticViolation cell
modelViolation modelError =
  case modelError of
    SheafModelRestrictionBuildError registryError ->
      registryViolation registryError
