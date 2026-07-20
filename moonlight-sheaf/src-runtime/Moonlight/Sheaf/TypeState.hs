{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The type-state sheaf pipeline: 'SheafT' phases from bare site through
-- stalked to executable.
module Moonlight.Sheaf.TypeState
  ( SheafT,
    emptySheaf,
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
    SheafStep,
    runSheafStep,
    composeSheafStep,
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
  Type ->
  Type

data SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure where
  EmptySheafT ::
    site ->
    SheafBasis cell ->
    SheafT 'Empty owner site cell stalk restrictionWitness mismatch coboundaryState report failure

  StalkedSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore owner cell stalk ->
    SheafT 'Stalked owner site cell stalk restrictionWitness mismatch coboundaryState report failure

  ConsistentSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore owner cell stalk ->
    SheafModel owner cell restrictionWitness ->
    GradedComplex cell Int ->
    coboundaryState ->
    SheafT 'Consistent owner site cell stalk restrictionWitness mismatch coboundaryState report failure

  ResolvedSheafT ::
    site ->
    SheafBasis cell ->
    TotalSectionStore owner cell stalk ->
    SheafModel owner cell restrictionWitness ->
    GradedComplex cell Int ->
    report ->
    SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure

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
  Type ->
  Type

newtype SheafStep from to owner site cell stalk restrictionWitness mismatch coboundaryState report failure = SheafStep
  { runSheafStepInternal ::
      SheafT from owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
      Either
        [SheafStepError cell failure]
        (SheafT to owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
  }

runSheafStep ::
  SheafStep from to owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafT from owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT to owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
runSheafStep = runSheafStepInternal

emptySheaf ::
  site ->
  SheafBasis cell ->
  SheafT 'Empty owner site cell stalk restrictionWitness mismatch coboundaryState report failure
emptySheaf = EmptySheafT

type SheafStepError :: Type -> Type -> Type
data SheafStepError cell failure
  = RestrictionBuildError !failure
  | CoboundaryAssemblyError !failure
  | ResolutionError !failure
  | SectionUpdateFailed !(SectionUpdateError cell)
  deriving stock (Eq, Show)

type RestrictionBuilder :: Type -> Type -> Type -> Type -> Type -> Type
type RestrictionBuilder owner site cell restrictionWitness failure =
  site -> Either failure (SheafModel owner cell restrictionWitness)

type CoboundaryBuilder :: Type -> Type -> Type -> Type -> Type
type CoboundaryBuilder site cell restrictionWitness failure =
  site -> RestrictionIndex cell restrictionWitness -> Either failure (GradedComplex cell Int)

type ConsistencyBuilder :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
type ConsistencyBuilder owner site cell stalk mismatch restrictionWitness coboundaryState =
  site ->
  TotalSectionStore owner cell stalk ->
  SheafModel owner cell restrictionWitness ->
  GradedComplex cell Int ->
  coboundaryState

type DerivedAuthorityBuilder :: Type -> Type -> Type -> Type -> Type -> Type
type DerivedAuthorityBuilder owner site cell restrictionWitness failure =
  site ->
  Either
    [SheafStepError cell failure]
    (SheafModel owner cell restrictionWitness, GradedComplex cell Int)

type HasStalkAccess :: SheafPhase -> Constraint
class HasStalkAccess phase where
  stalkSectionAt ::
    SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    TotalSectionStore owner cell stalk

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
    SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
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
    SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
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
    SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    coboundaryState

instance HasConsistencyAccess 'Consistent where
  consistencyStateAt sheafState =
    case sheafState of
      ConsistentSheafT _ _ _ _ _ consistencyState -> consistencyState

type HasResolutionReportAccess :: SheafPhase -> Constraint
class HasResolutionReportAccess phase where
  resolutionReportAt ::
    SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
    report

instance HasResolutionReportAccess 'Resolved where
  resolutionReportAt sheafState =
    case sheafState of
      ResolvedSheafT _ _ _ _ _ report ->
        report

sheafResolutionReport ::
  HasResolutionReportAccess phase =>
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  report
sheafResolutionReport =
  resolutionReportAt

sectionUpdateErrorToStepError :: SectionUpdateError cell -> SheafStepError cell failure
sectionUpdateErrorToStepError =
  SectionUpdateFailed

sheafSite ::
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  site
sheafSite sheafState =
  case sheafState of
    EmptySheafT site _ -> site
    StalkedSheafT site _ _ -> site
    ConsistentSheafT site _ _ _ _ _ -> site
    ResolvedSheafT site _ _ _ _ _ -> site

sheafBasis ::
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafBasis cell
sheafBasis sheafState =
  case sheafState of
    EmptySheafT _ basis -> basis
    StalkedSheafT _ basis _ -> basis
    ConsistentSheafT _ basis _ _ _ _ -> basis
    ResolvedSheafT _ basis _ _ _ _ -> basis

sheafStalks ::
  HasStalkAccess phase =>
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  TotalSectionStore owner cell stalk
sheafStalks =
  stalkSectionAt

sheafRestrictions ::
  HasRestrictionAccess phase =>
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  RestrictionIndex cell restrictionWitness
sheafRestrictions =
  restrictionIndexAt

sheafCoboundary ::
  HasCoboundaryAccess phase =>
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  GradedComplex cell Int
sheafCoboundary =
  coboundaryCacheAt

sheafConsistency ::
  HasConsistencyAccess phase =>
  SheafT phase owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
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
      Either failure (section, report)
  }

runtimeResolutionDriver ::
  RuntimeResolutionBuilder owner site cell stalk restrictionWitness report failure ->
  ResolutionDriver
    site
    cell
    (Set cell)
    (SheafModel owner cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore owner cell stalk)
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
          fmap
            (\(_runtimeSite, resolvedSection, report) -> (resolvedSection, report))
            ( runRuntimeResolutionProgram
                (buildProgram site model cache section)
                dirtyCells
                site
                section
            )
    }

assignStep ::
  TotalSectionStore owner cell stalk ->
  SheafStep 'Empty 'Stalked owner site cell stalk restrictionWitness mismatch coboundaryState report failure
assignStep assignedSection =
  SheafStep $ \case
    EmptySheafT site basis ->
      Right (StalkedSheafT site basis assignedSection)

consistencyStep ::
  RestrictionBuilder owner site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder owner site cell stalk mismatch restrictionWitness coboundaryState ->
  SheafStep 'Stalked 'Consistent owner site cell stalk restrictionWitness mismatch coboundaryState report failure
consistencyStep buildRestrictions buildCoboundary buildConsistency =
  SheafStep $ \case
    StalkedSheafT site basis section -> do
      (model, cache) <- rebuildDerivedAuthority buildRestrictions buildCoboundary site
      Right (ConsistentSheafT site basis section model cache (buildConsistency site section model cache))

rebuildDerivedAuthority ::
  RestrictionBuilder owner site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  DerivedAuthorityBuilder owner site cell restrictionWitness failure
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
  Either [stepError] (section, report)
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
  Either [stepError] (section, report)
resolveWithSeedBy promoteFailure resolutionDriver resolutionSeed site registry cache section =
  first promoteFailure
    (runResolutionDriver resolutionDriver resolutionSeed site registry cache section)

resolve ::
  ResolutionDriver site cell seed registry cache section report failure ->
  site ->
  registry ->
  cache ->
  section ->
  Either [SheafStepError cell failure] (section, report)
resolve =
  resolveBy (pure . ResolutionError)

resolveWithSeed ::
  ResolutionDriver site cell seed registry cache section report failure ->
  seed ->
  site ->
  registry ->
  cache ->
  section ->
  Either [SheafStepError cell failure] (section, report)
resolveWithSeed =
  resolveWithSeedBy (pure . ResolutionError)

resolveStep ::
  RuntimeResolutionBuilder owner site cell stalk restrictionWitness report failure ->
  SheafStep 'Consistent 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure
resolveStep =
  resolveStepWith . runtimeResolutionDriver

resolveStepWith ::
  ResolutionDriver
    site
    cell
    seed
    (SheafModel owner cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore owner cell stalk)
    report
    failure ->
  SheafStep 'Consistent 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure
resolveStepWith driver =
  SheafStep $ \case
    ConsistentSheafT site basis section model cache _consistencyState -> do
      (resolvedSection, report) <- resolve driver site model cache section
      Right (ResolvedSheafT site basis resolvedSection model cache report)

buildResolvedSheaf ::
  TotalSectionStore owner cell stalk ->
  site ->
  SheafBasis cell ->
  RestrictionBuilder owner site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder owner site cell stalk mismatch restrictionWitness coboundaryState ->
  RuntimeResolutionBuilder owner site cell stalk restrictionWitness report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
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
  TotalSectionStore owner cell stalk ->
  site ->
  SheafBasis cell ->
  RestrictionBuilder owner site cell restrictionWitness failure ->
  CoboundaryBuilder site cell restrictionWitness failure ->
  ConsistencyBuilder owner site cell stalk mismatch restrictionWitness coboundaryState ->
  ResolutionDriver
    site
    cell
    seed
    (SheafModel owner cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore owner cell stalk)
    report
    failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
buildResolvedSheafWith assignedSection site basis buildRestrictions buildCoboundary buildConsistency driver = do
  stalked <-
    runSheafStep
      (assignStep assignedSection)
      (emptySheaf site basis)
  consistent <-
    runSheafStep
      (consistencyStep buildRestrictions buildCoboundary buildConsistency)
      stalked
  runSheafStep
    (resolveStepWith driver)
    consistent

adjustCellsAndPropagate ::
  Ord cell =>
  [(cell, stalk -> stalk)] ->
  RuntimeResolutionBuilder owner site cell stalk restrictionWitness report failure ->
  SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
adjustCellsAndPropagate adjustments =
  adjustCellsAndPropagateWith
    adjustments
    . runtimeResolutionDriver

adjustCellsAndPropagateWith ::
  Ord cell =>
  [(cell, stalk -> stalk)] ->
  ResolutionDriver
    site
    cell
    (Set cell)
    (SheafModel owner cell restrictionWitness)
    (GradedComplex cell Int)
    (TotalSectionStore owner cell stalk)
    report
    failure ->
  SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  Either
    [SheafStepError cell failure]
    (SheafT 'Resolved owner site cell stalk restrictionWitness mismatch coboundaryState report failure)
adjustCellsAndPropagateWith adjustments driver = \case
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
    (resolvedSection, report) <-
      resolveWithSeed driver touchedCells site model cache updatedSection
    Right (ResolvedSheafT site basis resolvedSection model cache report)

composeSheafStep ::
  SheafStep middle target owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafStep source middle owner site cell stalk restrictionWitness mismatch coboundaryState report failure ->
  SheafStep source target owner site cell stalk restrictionWitness mismatch coboundaryState report failure
composeSheafStep nextStep previousStep =
  SheafStep
    (runSheafStep previousStep >=> runSheafStep nextStep)
