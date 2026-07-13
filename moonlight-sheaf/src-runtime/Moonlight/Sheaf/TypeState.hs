{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.TypeState
  ( SheafT (..),
    sheafSite,
    sheafBasis,
    sheafStalks,
    sheafRestrictions,
    sheafCoboundary,
    sheafConsistency,
    sheafResolutionReport,
    HasStalkAccess,
    HasRestrictionAccess,
    HasCoboundaryAccess,
    HasConsistencyAccess,
    HasResolutionReportAccess,
    SheafStep (..),
    composeSheafStep,
    totalSheafStep,
    SheafStepError (..),
    RestrictionBuilder,
    CoboundaryBuilder,
    ConsistencyBuilder,
    RuntimeResolutionProgram,
    RuntimeResolutionBuilder,
    ResolutionDriver (..),
    runtimeResolutionDriver,
    resolveBy,
    resolve,
    resolveWithSeedBy,
    resolveWithSeed,
    assignStep,
    consistencyStep,
    resolveStep,
    resolveStepWith,
    buildResolvedSheaf,
    buildResolvedSheafWith,
    adjustCellsAndPropagate,
    adjustCellsAndPropagateWith,
  )
where

import Control.Monad
  ( foldM,
    (>=>),
  )
import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Constraint,
    Type,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionBuilder,
    RuntimeResolutionProgram,
    runRuntimeResolutionProgram,
    runtimeResolutionInitialDirtyCells,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelRestrictions,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.Site.Phase
  ( SheafPhase (..),
  )

type SheafT ::
  SheafPhase ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type

data SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure where
  EmptySheafT ::
    site ->
    SheafBasis cell ->
    SheafT 'Empty site cell stalk restrictionWitness mismatch coboundaryState report failure

  StalkedSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore cell stalk ->
    SheafT 'Stalked site cell stalk restrictionWitness mismatch coboundaryState report failure

  ConsistentSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore cell stalk ->
    SheafModel cell restrictionWitness ->
    GradedComplex cell Int ->
    coboundaryState ->
    SheafT 'Consistent site cell stalk restrictionWitness mismatch coboundaryState report failure

  ResolvedSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore cell stalk ->
    SheafModel cell restrictionWitness ->
    GradedComplex cell Int ->
    report ->
    SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure

type SheafStep ::
  SheafPhase ->
  SheafPhase ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type

newtype SheafStep from to site cell stalk restrictionWitness mismatch coboundaryState report failure = SheafStep
  { runSheafStep ::
      SheafT from site cell stalk restrictionWitness mismatch coboundaryState report failure ->
      Either
        [SheafStepError cell failure]
        (SheafT to site cell stalk restrictionWitness mismatch coboundaryState report failure)
  }

type SheafStepError :: Type -> Type -> Type
data SheafStepError cell failure
  = RestrictionBuildError !failure
  | CoboundaryAssemblyError !failure
  | ResolutionError !failure
  | SectionUpdateFailed !(SectionUpdateError cell)
  deriving stock (Eq, Show)

type RestrictionBuilder :: Type -> Type -> Type -> Type -> Type
type RestrictionBuilder site cell restrictionWitness failure =
  site -> Either failure (SheafModel cell restrictionWitness)

type CoboundaryBuilder :: Type -> Type -> Type -> Type -> Type
type CoboundaryBuilder site cell restrictionWitness failure =
  site -> RestrictionIndex cell restrictionWitness -> Either failure (GradedComplex cell Int)

type ConsistencyBuilder :: Type -> Type -> Type -> Type -> Type -> Type -> Type
type ConsistencyBuilder site cell stalk mismatch restrictionWitness coboundaryState =
  site ->
  TotalSectionStore cell stalk ->
  SheafModel cell restrictionWitness ->
  GradedComplex cell Int ->
  coboundaryState

type DerivedAuthorityBuilder :: Type -> Type -> Type -> Type -> Type
type DerivedAuthorityBuilder site cell restrictionWitness failure =
  site ->
  Either
    [SheafStepError cell failure]
    (SheafModel cell restrictionWitness, GradedComplex cell Int)

type HasStalkAccess :: SheafPhase -> Constraint
class HasStalkAccess phase where
  stalkSectionAt ::
    SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    TotalSectionStore cell stalk

instance HasStalkAccess 'Stalked where
  stalkSectionAt sheafState =
    case sheafState of
      StalkedSheafT _ _ section -> section

instance HasStalkAccess 'Consistent where
  stalkSectionAt sheafState =
    case sheafState of
      ConsistentSheafT _ _ section _ _ _ -> section

instance HasStalkAccess 'Resolved where
  stalkSectionAt sheafState =
    case sheafState of
      ResolvedSheafT _ _ section _ _ _ -> section

type HasRestrictionAccess :: SheafPhase -> Constraint
class HasRestrictionAccess phase where
  restrictionIndexAt ::
    SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    RestrictionIndex cell restrictionWitness

instance HasRestrictionAccess 'Consistent where
  restrictionIndexAt sheafState =
    case sheafState of
      ConsistentSheafT _ _ _ model _ _ -> sheafModelRestrictions model

instance HasRestrictionAccess 'Resolved where
  restrictionIndexAt sheafState =
    case sheafState of
      ResolvedSheafT _ _ _ model _ _ -> sheafModelRestrictions model

type HasCoboundaryAccess :: SheafPhase -> Constraint
class HasCoboundaryAccess phase where
  coboundaryCacheAt ::
    SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    GradedComplex cell Int

instance HasCoboundaryAccess 'Consistent where
  coboundaryCacheAt sheafState =
    case sheafState of
      ConsistentSheafT _ _ _ _ cache _ -> cache

instance HasCoboundaryAccess 'Resolved where
  coboundaryCacheAt sheafState =
    case sheafState of
      ResolvedSheafT _ _ _ _ cache _ -> cache

type HasConsistencyAccess :: SheafPhase -> Constraint
class HasConsistencyAccess phase where
  consistencyStateAt ::
    SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    coboundaryState

instance HasConsistencyAccess 'Consistent where
  consistencyStateAt sheafState =
    case sheafState of
      ConsistentSheafT _ _ _ _ _ consistencyState -> consistencyState

type HasResolutionReportAccess :: SheafPhase -> Constraint
class HasResolutionReportAccess phase where
  resolutionReportAt ::
    SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    report

instance HasResolutionReportAccess 'Resolved where
  resolutionReportAt sheafState =
    case sheafState of
      ResolvedSheafT _ _ _ _ _ report ->
        report

sheafResolutionReport ::
  HasResolutionReportAccess phase =>
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  report
sheafResolutionReport =
  resolutionReportAt

sectionUpdateErrorToStepError :: SectionUpdateError cell -> SheafStepError cell failure
sectionUpdateErrorToStepError =
  SectionUpdateFailed

sheafSite ::
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  site
sheafSite sheafState =
  case sheafState of
    EmptySheafT site _ -> site
    StalkedSheafT site _ _ -> site
    ConsistentSheafT site _ _ _ _ _ -> site
    ResolvedSheafT site _ _ _ _ _ -> site

sheafBasis ::
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafBasis cell
sheafBasis sheafState =
  case sheafState of
    EmptySheafT _ basis -> basis
    StalkedSheafT _ basis _ -> basis
    ConsistentSheafT _ basis _ _ _ _ -> basis
    ResolvedSheafT _ basis _ _ _ _ -> basis

sheafStalks ::
  HasStalkAccess phase =>
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  TotalSectionStore cell stalk
sheafStalks =
  stalkSectionAt

sheafRestrictions ::
  HasRestrictionAccess phase =>
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  RestrictionIndex cell restrictionWitness
sheafRestrictions =
  restrictionIndexAt

sheafCoboundary ::
  HasCoboundaryAccess phase =>
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  GradedComplex cell Int
sheafCoboundary =
  coboundaryCacheAt

sheafConsistency ::
  HasConsistencyAccess phase =>
  SheafT phase site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  coboundaryState
sheafConsistency =
  consistencyStateAt

type ResolutionDriver :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ResolutionDriver site cell seed registry cache section report failure = ResolutionDriver
  { initializeResolutionSeed ::
      site ->
      registry ->
      cache ->
      section ->
      Either failure seed,
    runResolutionDriver ::
      seed ->
      site ->
      registry ->
      cache ->
      section ->
      Either failure (site, section, report)
  }

runtimeResolutionDriver ::
  RuntimeResolutionBuilder site cell stalk restrictionWitness report failure ->
  ResolutionDriver
    site
    cell
    (Set cell)
    (SheafModel cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore cell stalk)
    report
    failure
runtimeResolutionDriver buildProgram =
  ResolutionDriver
    { initializeResolutionSeed =
        \site model cache section ->
          Right
            ( runtimeResolutionInitialDirtyCells
                (buildProgram site model cache section)
            ),
      runResolutionDriver =
        \dirtyCells site model cache section ->
          runRuntimeResolutionProgram
            (buildProgram site model cache section)
            dirtyCells
            site
            section
    }

assignStep ::
  TotalSectionStore cell stalk ->
  SheafStep 'Empty 'Stalked site cell stalk restrictionWitness mismatch coboundaryState report failure
assignStep assignedSection =
  SheafStep $ \case
    EmptySheafT site basis ->
      Right (StalkedSheafT site basis assignedSection)

consistencyStep ::
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder site cell stalk mismatch restrictionWitness coboundaryState ->
  SheafStep 'Stalked 'Consistent site cell stalk restrictionWitness mismatch coboundaryState report failure
consistencyStep buildRestrictions buildCoboundary buildConsistency =
  SheafStep $ \case
    StalkedSheafT site basis section -> do
      (model, cache) <- rebuildDerivedAuthority buildRestrictions buildCoboundary site
      Right (ConsistentSheafT site basis section model cache (buildConsistency site section model cache))

rebuildDerivedAuthority ::
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  DerivedAuthorityBuilder site cell restrictionWitness failure
rebuildDerivedAuthority buildRestrictions buildCoboundary site = do
  model <- first (pure . RestrictionBuildError) (buildRestrictions site)
  cache <- first (pure . CoboundaryAssemblyError) (buildCoboundary site (sheafModelRestrictions model))
  Right (model, cache)

resolveBy ::
  (failure -> [stepError]) ->
  ResolutionDriver site cell seed registry cache section report failure ->
  site ->
  registry ->
  cache ->
  section ->
  Either [stepError] (site, section, report)
resolveBy promoteFailure resolutionDriver site registry cache section = do
  resolutionSeed <-
    first promoteFailure
      (initializeResolutionSeed resolutionDriver site registry cache section)
  resolveWithSeedBy
    promoteFailure
    resolutionDriver
    resolutionSeed
    site
    registry
    cache
    section

resolveWithSeedBy ::
  (failure -> [stepError]) ->
  ResolutionDriver site cell seed registry cache section report failure ->
  seed ->
  site ->
  registry ->
  cache ->
  section ->
  Either [stepError] (site, section, report)
resolveWithSeedBy promoteFailure resolutionDriver resolutionSeed site registry cache section =
  first promoteFailure
    (runResolutionDriver resolutionDriver resolutionSeed site registry cache section)

resolve ::
  ResolutionDriver site cell seed registry cache section report failure ->
  site ->
  registry ->
  cache ->
  section ->
  Either [SheafStepError cell failure] (site, section, report)
resolve =
  resolveBy (pure . ResolutionError)

resolveWithSeed ::
  ResolutionDriver site cell seed registry cache section report failure ->
  seed ->
  site ->
  registry ->
  cache ->
  section ->
  Either [SheafStepError cell failure] (site, section, report)
resolveWithSeed =
  resolveWithSeedBy (pure . ResolutionError)

resolveStep ::
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  RuntimeResolutionBuilder site cell stalk restrictionWitness report failure ->
  SheafStep 'Consistent 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure
resolveStep buildRestrictions buildCoboundary =
  resolveStepWith (rebuildDerivedAuthority buildRestrictions buildCoboundary)
    . runtimeResolutionDriver

resolveStepWith ::
  DerivedAuthorityBuilder site cell restrictionWitness failure ->
  ResolutionDriver
    site
    cell
    seed
    (SheafModel cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore cell stalk)
    report
    failure ->
  SheafStep 'Consistent 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure
resolveStepWith rebuildAuthority driver =
  SheafStep $ \case
    ConsistentSheafT site basis section model cache _consistencyState -> do
      (site1, resolvedSection, report) <- resolve driver site model cache section
      (model1, cache1) <- rebuildAuthority site1
      Right (ResolvedSheafT site1 basis resolvedSection model1 cache1 report)

buildResolvedSheaf ::
  TotalSectionStore cell stalk ->
  site ->
  SheafBasis cell ->
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder site cell stalk mismatch restrictionWitness coboundaryState ->
  RuntimeResolutionBuilder site cell stalk restrictionWitness report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure)
buildResolvedSheaf assignedSection site basis buildRestrictions buildCoboundary buildConsistency buildRuntime =
  buildResolvedSheafWith
    assignedSection
    site
    basis
    buildRestrictions
    buildCoboundary
    buildConsistency
    (runtimeResolutionDriver buildRuntime)

buildResolvedSheafWith ::
  TotalSectionStore cell stalk ->
  site ->
  SheafBasis cell ->
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder site cell stalk mismatch restrictionWitness coboundaryState ->
  ResolutionDriver
    site
    cell
    seed
    (SheafModel cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore cell stalk)
    report
    failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure)
buildResolvedSheafWith assignedSection site basis buildRestrictions buildCoboundary buildConsistency driver = do
  stalked <-
    runSheafStep
      (assignStep assignedSection)
      (EmptySheafT site basis)
  consistent <-
    runSheafStep
      (consistencyStep buildRestrictions buildCoboundary buildConsistency)
      stalked
  runSheafStep
    (resolveStepWith (rebuildDerivedAuthority buildRestrictions buildCoboundary) driver)
    consistent

adjustCellsAndPropagate ::
  Ord cell =>
  [(cell, stalk -> stalk)] ->
  RestrictionBuilder site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  RuntimeResolutionBuilder site cell stalk restrictionWitness report failure ->
  SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure)
adjustCellsAndPropagate adjustments buildRestrictions buildCoboundary =
  adjustCellsAndPropagateWith
    adjustments
    (rebuildDerivedAuthority buildRestrictions buildCoboundary)
    . runtimeResolutionDriver

adjustCellsAndPropagateWith ::
  Ord cell =>
  [(cell, stalk -> stalk)] ->
  DerivedAuthorityBuilder site cell restrictionWitness failure ->
  ResolutionDriver
    site
    cell
    (Set cell)
    (SheafModel cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore cell stalk)
    report
    failure ->
  SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved site cell stalk restrictionWitness mismatch coboundaryState report failure)
adjustCellsAndPropagateWith adjustments rebuildAuthority driver = \case
  ResolvedSheafT site basis section model cache _oldReport -> do
    updatedSection <-
      foldM
        ( \currentSection (cell, adjustStalk) ->
            first
              (pure . sectionUpdateErrorToStepError)
              (updateStalkAtChecked model cell adjustStalk currentSection)
        )
        section
        adjustments
    let touchedCells =
          Set.fromList (fmap fst adjustments)
    (site1, resolvedSection, report) <-
      resolveWithSeed driver touchedCells site model cache updatedSection
    (model1, cache1) <- rebuildAuthority site1
    Right (ResolvedSheafT site1 basis resolvedSection model1 cache1 report)

composeSheafStep ::
  SheafStep middle target site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafStep source middle site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafStep source target site cell stalk restrictionWitness mismatch coboundaryState report failure
composeSheafStep nextStep previousStep =
  SheafStep
    (runSheafStep previousStep >=> runSheafStep nextStep)

totalSheafStep ::
  ( SheafT source site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    SheafT target site cell stalk restrictionWitness mismatch coboundaryState report failure
  ) ->
  SheafStep source target site cell stalk restrictionWitness mismatch coboundaryState report failure
totalSheafStep transform =
  SheafStep (Right . transform)
