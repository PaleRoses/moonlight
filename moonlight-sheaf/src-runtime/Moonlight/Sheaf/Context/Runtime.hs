{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Context.Runtime
  ( ContextRuntime (..),
    ContextRuntimeCacheIdentity (..),
    ContextPropagationState (..),
    ContextRefreshPrepared (..),
    ContextRefreshResult (..),
    ContextPropagationInvariantFailure (..),
    ContextSectionRepairDelta,
    emptyContextSectionRepairDelta,
    contextSectionRepairDelta,
    contextSectionRepairDeltaContexts,
    cachedContexts,
    freshSectionAt,
    defaultContextRowsCacheBudgetBytes,
    bootstrapContextSections,
    bootstrapContextSectionsResult,
    bootstrapContextSectionsWithBudget,
    bootstrapContextSectionsWithBudgetResult,
    repairContextSections,
    repairContextSectionsResult,
    repairContextSectionsWithBudget,
    repairContextSectionsWithBudgetResult,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Function
  ( (&),
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
    ContextRestrictionRegistry,
    crrEdges,
  )
import Moonlight.Sheaf.Context.Core
  ( ContextPropagationFailure (..),
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSiteError,
    contextObjectKeyFor,
    contextRestrictionRegistryForObjects,
  )
import Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionProgram,
    runRuntimeResolutionProgram,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelBuildError (..),
    prepareSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

type ContextRefreshPrepared :: Type -> Type -> Type -> Type
data ContextRefreshPrepared site ctx section = ContextRefreshPrepared
  { crpSite :: !site,
    crpContexts :: ![ctx],
    crpDirtyContexts :: !(Set ctx),
    crpSections :: !(Map ctx section),
    crpRestrictionModel :: !(SheafModel ctx (ContextRestrictionEdge ctx)),
    crpRuntimeCacheIdentity :: !ContextRuntimeCacheIdentity
  }

type ContextRefreshResult :: Type -> Type -> Type
data ContextRefreshResult site ctx = ContextRefreshResult
  { crrRefreshedSite :: !site,
    crrRefreshedDirtyContexts :: !(Set ctx)
  }

type ContextSectionBootstrap :: Type -> Type -> Type -> Type
data ContextSectionBootstrap site ctx section = ContextSectionBootstrap
  { csbSite :: !site,
    csbDirtyContexts :: !(Set ctx),
    csbInitialSections :: !(Map ctx section),
    csbRestrictionRegistry :: !(ContextRestrictionRegistry ctx)
  }

type ContextPropagationInvariantFailure :: Type -> Type
data ContextPropagationInvariantFailure ctx
  = ContextPropagationSheafModelFailed !(SheafModelBuildError ctx)
  | ContextPropagationRestrictionRegistryFailed !(PreparedContextSiteError ctx)
  | ContextPropagationInitialSectionFailed !(SectionStoreError ctx)
  | ContextPropagationResolvedSectionFailed !(SectionStoreError ctx)
  | ContextPropagationDirtyOverrideInvalidContexts !(Set ctx)
  deriving stock (Eq, Show)

type ContextRuntimeCacheIdentity :: Type
data ContextRuntimeCacheIdentity = ContextRuntimeCacheIdentity
  { crciBaseRevision :: !Int,
    crciContextRevision :: !Natural
  }
  deriving stock (Eq, Ord, Show)

type ContextPropagationState :: Type -> Type -> Type -> Type
data ContextPropagationState ctx report failure
  = ContextPropagationUnknown
  | ContextPropagationSettled !report
  | ContextPropagationFailed
      !(Maybe report)
      !(ContextPropagationFailure ctx (ContextPropagationInvariantFailure ctx) failure)
  deriving stock (Eq, Show)

type ContextSectionRepairFailure :: Type -> Type -> Type -> Type
data ContextSectionRepairFailure site ctx failure = ContextSectionRepairFailure
  { csrfSite :: !site,
    csrfDirtyContexts :: !(Set ctx),
    csrfFailure :: !(ContextPropagationFailure ctx (ContextPropagationInvariantFailure ctx) failure)
  }

type ContextSectionRepairDelta :: Type -> Type
newtype ContextSectionRepairDelta ctx = ContextSectionRepairDelta
  { csrDeltaContexts :: Set ctx
  }
  deriving stock (Eq, Show)

type ContextRuntime :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data ContextRuntime site ctx fresh section report failure = ContextRuntime
  { crPreparedSite :: site -> PreparedContextSite ctx,
    crCachedContexts :: site -> [ctx],
    crFreshSection :: ctx -> site -> fresh,
    crResolveFreshSection :: fresh -> section,
    crRuntimeCacheIdentity :: site -> ContextRuntimeCacheIdentity,
    crStoredRuntimeCacheIdentity :: site -> Maybe ContextRuntimeCacheIdentity,
    crSetStoredRuntimeCacheIdentity :: Maybe ContextRuntimeCacheIdentity -> site -> site,
    crPropagationState :: site -> ContextPropagationState ctx report failure,
    crDirtyContexts :: site -> Set ctx,
    crSetDirtyContexts :: Set ctx -> site -> site,
    crStoredSections :: site -> Map ctx section,
    crSetSections :: Map ctx section -> site -> site,
    crSetPropagationState ::
      ContextPropagationState ctx report failure ->
      site ->
      site,
    crCompileContextRefresh ::
      Natural ->
      ContextRefreshPrepared site ctx section ->
      RuntimeResolutionProgram site ctx section report failure
  }

cachedContexts ::
  ContextRuntime site ctx fresh section report failure ->
  site ->
  [ctx]
cachedContexts =
  crCachedContexts
{-# INLINE cachedContexts #-}

freshSectionAt ::
  ContextRuntime site ctx fresh section report failure ->
  ctx ->
  site ->
  section
freshSectionAt runtime contextValue site =
  crResolveFreshSection runtime (crFreshSection runtime contextValue site)
{-# INLINE freshSectionAt #-}

contextPropagationStateReport ::
  ContextPropagationState ctx report failure ->
  Maybe report
contextPropagationStateReport propagationState =
  case propagationState of
    ContextPropagationUnknown ->
      Nothing
    ContextPropagationSettled report ->
      Just report
    ContextPropagationFailed maybeReport _failure ->
      maybeReport
{-# INLINE contextPropagationStateReport #-}

defaultContextRowsCacheBudgetBytes :: Natural
defaultContextRowsCacheBudgetBytes =
  64 * 1024 * 1024
{-# INLINE defaultContextRowsCacheBudgetBytes #-}

emptyContextSectionRepairDelta :: ContextSectionRepairDelta ctx
emptyContextSectionRepairDelta =
  ContextSectionRepairDelta Set.empty
{-# INLINE emptyContextSectionRepairDelta #-}

contextSectionRepairDelta :: Set ctx -> ContextSectionRepairDelta ctx
contextSectionRepairDelta =
  ContextSectionRepairDelta
{-# INLINE contextSectionRepairDelta #-}

contextSectionRepairDeltaContexts :: ContextSectionRepairDelta ctx -> Set ctx
contextSectionRepairDeltaContexts =
  csrDeltaContexts
{-# INLINE contextSectionRepairDeltaContexts #-}

bootstrapContextSections ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  site ->
  site
bootstrapContextSections =
  bootstrapContextSectionsWithBudget defaultContextRowsCacheBudgetBytes

bootstrapContextSectionsResult ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  site ->
  ContextRefreshResult site ctx
bootstrapContextSectionsResult =
  bootstrapContextSectionsWithBudgetResult defaultContextRowsCacheBudgetBytes

bootstrapContextSectionsWithBudget ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  site ->
  site
bootstrapContextSectionsWithBudget cacheBudget runtime site0 =
  crrRefreshedSite (bootstrapContextSectionsWithBudgetResult cacheBudget runtime site0)

bootstrapContextSectionsWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  site ->
  ContextRefreshResult site ctx
bootstrapContextSectionsWithBudgetResult cacheBudget runtime =
  runContextSectionRepairWithBudgetResult cacheBudget runtime Nothing

repairContextSections ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  site
repairContextSections runtime delta site0 =
  crrRefreshedSite (repairContextSectionsResult runtime delta site0)

repairContextSectionsResult ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  ContextRefreshResult site ctx
repairContextSectionsResult =
  repairContextSectionsWithBudgetResult defaultContextRowsCacheBudgetBytes

repairContextSectionsWithBudget ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  site
repairContextSectionsWithBudget cacheBudget runtime delta site0 =
  crrRefreshedSite (repairContextSectionsWithBudgetResult cacheBudget runtime delta site0)

repairContextSectionsWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  ContextRefreshResult site ctx
repairContextSectionsWithBudgetResult cacheBudget runtime delta =
  runContextSectionRepairWithBudgetResult cacheBudget runtime (Just delta)

runContextSectionRepairWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  Maybe (ContextSectionRepairDelta ctx) ->
  site ->
  ContextRefreshResult site ctx
runContextSectionRepairWithBudgetResult cacheBudget runtime maybeRepairDelta site0 =
  either
    (contextSectionRepairFailureResult runtime)
    id
    (runContextSectionRepairWithBudgetAttempt cacheBudget runtime maybeRepairDelta site0)

runContextSectionRepairWithBudgetAttempt ::
  Ord ctx =>
  Natural ->
  ContextRuntime site ctx fresh section report failure ->
  Maybe (ContextSectionRepairDelta ctx) ->
  site ->
  Either (ContextSectionRepairFailure site ctx failure) (ContextRefreshResult site ctx)
runContextSectionRepairWithBudgetAttempt cacheBudget runtime maybeRepairDelta site0 = do
  ContextSectionBootstrap bootstrapSite dirtyContexts initialSections restrictionRegistryValue <-
    bootstrapSiteSections runtime (fmap contextSectionRepairDeltaContexts maybeRepairDelta) site0
  let contexts =
        Set.toAscList (Set.fromList (cachedContexts runtime bootstrapSite))
      currentIdentity =
        crRuntimeCacheIdentity runtime bootstrapSite
  restrictionModel <-
    first
      (invariantRepairFailure bootstrapSite dirtyContexts . ContextPropagationSheafModelFailed)
      (buildSiteSheafModel runtime bootstrapSite (crrEdges restrictionRegistryValue))
  initialSection <-
    first
      (invariantRepairFailure bootstrapSite dirtyContexts . ContextPropagationInitialSectionFailed)
      (sectionFromMap restrictionModel initialSections)
  let prepared =
        ContextRefreshPrepared
          { crpSite = bootstrapSite,
            crpContexts = contexts,
            crpDirtyContexts = dirtyContexts,
            crpSections = initialSections,
            crpRestrictionModel = restrictionModel,
            crpRuntimeCacheIdentity = currentIdentity
          }
      refreshProgram =
        crCompileContextRefresh runtime cacheBudget prepared
  (siteAfterRun, resolvedSection, report) <-
    first
      (runtimeRepairFailure bootstrapSite dirtyContexts)
      (runRuntimeResolutionProgram refreshProgram dirtyContexts bootstrapSite initialSection)
  resolvedEntries <-
    first
      (invariantRepairFailure siteAfterRun dirtyContexts . ContextPropagationResolvedSectionFailed)
      (totalSectionEntries restrictionModel resolvedSection)
  pure
    ( ContextRefreshResult
        ( siteAfterRun
            & crSetSections runtime resolvedEntries
            & crSetDirtyContexts runtime Set.empty
            & crSetStoredRuntimeCacheIdentity runtime (Just currentIdentity)
            & crSetPropagationState runtime (ContextPropagationSettled report)
        )
        dirtyContexts
    )

invariantRepairFailure ::
  site ->
  Set ctx ->
  ContextPropagationInvariantFailure ctx ->
  ContextSectionRepairFailure site ctx failure
invariantRepairFailure site dirtyContexts failureValue =
  ContextSectionRepairFailure
    site
    dirtyContexts
    (ContextPropagationInvariantViolation failureValue)

runtimeRepairFailure ::
  site ->
  Set ctx ->
  failure ->
  ContextSectionRepairFailure site ctx failure
runtimeRepairFailure site dirtyContexts failureValue =
  ContextSectionRepairFailure
    site
    dirtyContexts
    (ContextPropagationRuntimeFailure failureValue)

contextSectionRepairFailureResult ::
  ContextRuntime site ctx fresh section report failure ->
  ContextSectionRepairFailure site ctx failure ->
  ContextRefreshResult site ctx
contextSectionRepairFailureResult runtime failureValue =
  let previousReport =
        contextPropagationStateReport (crPropagationState runtime (csrfSite failureValue))
   in
  ContextRefreshResult
    ( crSetPropagationState
        runtime
        (ContextPropagationFailed previousReport (csrfFailure failureValue))
        (csrfSite failureValue)
    )
    (csrfDirtyContexts failureValue)

bootstrapSiteSections ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  Maybe (Set ctx) ->
  site ->
  Either (ContextSectionRepairFailure site ctx failure) (ContextSectionBootstrap site ctx section)
bootstrapSiteSections runtime dirtyOverride site0 =
  let preparedSite =
        crPreparedSite runtime site0
      contextIsPrepared contextValue =
        either (const False) (const True) (contextObjectKeyFor preparedSite contextValue)
      requestedOverrideContexts =
        fromMaybe Set.empty dirtyOverride
      invalidDirtyOverrideContexts =
        maybe
          Set.empty
          (Set.filter (not . contextIsPrepared))
          dirtyOverride
      contextSet =
        Set.union
          (Set.fromList (cachedContexts runtime site0))
          requestedOverrideContexts
      validContextSet =
        Set.filter contextIsPrepared contextSet
      storedSections =
        Map.restrictKeys (crStoredSections runtime site0) validContextSet
      storedContextSet =
        Map.keysSet storedSections
      missingStoredContexts =
        Set.difference validContextSet storedContextSet
      currentIdentity =
        crRuntimeCacheIdentity runtime site0
      identityStale =
        crStoredRuntimeCacheIdentity runtime site0 /= Just currentIdentity
      forcedDirtyContexts =
        if identityStale
          then validContextSet
          else missingStoredContexts
      requestedDirtyContexts =
        fromMaybe (crDirtyContexts runtime site0) dirtyOverride
      dirtyContexts =
        Set.intersection
          (Set.union forcedDirtyContexts requestedDirtyContexts)
          validContextSet
      refreshedDirtySections =
        Map.fromSet
          (\contextValue -> freshSectionAt runtime contextValue site0)
          dirtyContexts
      initialSections =
        Map.union refreshedDirtySections storedSections
      clearStaleIdentity =
        if identityStale
          then crSetStoredRuntimeCacheIdentity runtime Nothing
          else id
      siteWithBootstrapFailure =
        site0
          & crSetSections runtime initialSections
          & clearStaleIdentity
          & crSetDirtyContexts runtime dirtyContexts
   in do
        if Set.null invalidDirtyOverrideContexts
          then Right ()
          else
            Left
              ( invariantRepairFailure
                  siteWithBootstrapFailure
                  dirtyContexts
                  (ContextPropagationDirtyOverrideInvalidContexts invalidDirtyOverrideContexts)
              )
        restrictionRegistryValue <-
          first
            (invariantRepairFailure siteWithBootstrapFailure dirtyContexts . ContextPropagationRestrictionRegistryFailed)
            (contextRestrictionRegistryForObjects contextSet (crPreparedSite runtime site0))
        pure
          ContextSectionBootstrap
            { csbSite =
                site0
                  & crSetSections runtime initialSections
                  & clearStaleIdentity
                  & crSetDirtyContexts runtime dirtyContexts,
              csbDirtyContexts = dirtyContexts,
              csbInitialSections = initialSections,
              csbRestrictionRegistry = restrictionRegistryValue
            }

buildSiteSheafModel ::
  Ord ctx =>
  ContextRuntime site ctx fresh section report failure ->
  site ->
  [ContextRestrictionEdge ctx] ->
  Either (SheafModelBuildError ctx) (SheafModel ctx (ContextRestrictionEdge ctx))
buildSiteSheafModel runtime site restrictionEdgesValue =
  let objects =
        mkObjectIndex (cachedContexts runtime site)
      planFingerprint =
        crciBaseRevision (crRuntimeCacheIdentity runtime site)
   in prepareSheafModel
        (SheafModelVersion planFingerprint)
        objects
        ( \edge ->
            RestrictionParts
              { partKind = unitIncidenceRestriction,
                partSource = creSourceContext edge,
                partTarget = creTargetContext edge,
                partWitness = edge
              }
        )
        restrictionEdgesValue

sectionFromMap ::
  Ord ctx =>
  SheafModel ctx witness ->
  Map ctx section ->
  Either (SectionStoreError ctx) (TotalSectionStore ctx section)
sectionFromMap model sections =
  first SectionStoreConstructionFailed (mkTotalSectionStore model sections)
