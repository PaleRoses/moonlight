{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Context runtime: cached propagation state and prepared context refresh.
module Moonlight.Sheaf.Context.Runtime
  ( ContextRuntime (..),
    ContextRuntimeCacheIdentity (..),
    ContextPropagationState (..),
    ContextRefreshPrepared,
    crpSite,
    crpContexts,
    crpDirtyContexts,
    crpSections,
    crpRestrictionModel,
    crpRuntimeCacheIdentity,
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
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

type ContextRefreshPrepared :: Type -> Type -> Type -> Type -> Type -> Type
data ContextRefreshPrepared siteOwner modelOwner site ctx section = ContextRefreshPrepared
  { contextRefreshPreparedSite :: !site,
    contextRefreshPreparedContexts :: ![ctx],
    contextRefreshPreparedDirtyContexts :: !(Set ctx),
    contextRefreshPreparedSections :: !(Map ctx section),
    contextRefreshPreparedRestrictionModel :: !(SheafModel modelOwner ctx (ContextRestrictionEdge ctx)),
    contextRefreshPreparedRuntimeCacheIdentity :: !ContextRuntimeCacheIdentity
  }

type role ContextRefreshPrepared nominal nominal representational nominal representational

crpSite :: ContextRefreshPrepared siteOwner modelOwner site ctx section -> site
crpSite =
  contextRefreshPreparedSite
{-# INLINE crpSite #-}

crpContexts :: ContextRefreshPrepared siteOwner modelOwner site ctx section -> [ctx]
crpContexts =
  contextRefreshPreparedContexts
{-# INLINE crpContexts #-}

crpDirtyContexts :: ContextRefreshPrepared siteOwner modelOwner site ctx section -> Set ctx
crpDirtyContexts =
  contextRefreshPreparedDirtyContexts
{-# INLINE crpDirtyContexts #-}

crpSections :: ContextRefreshPrepared siteOwner modelOwner site ctx section -> Map ctx section
crpSections =
  contextRefreshPreparedSections
{-# INLINE crpSections #-}

crpRestrictionModel ::
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  SheafModel modelOwner ctx (ContextRestrictionEdge ctx)
crpRestrictionModel =
  contextRefreshPreparedRestrictionModel
{-# INLINE crpRestrictionModel #-}

crpRuntimeCacheIdentity ::
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  ContextRuntimeCacheIdentity
crpRuntimeCacheIdentity =
  contextRefreshPreparedRuntimeCacheIdentity
{-# INLINE crpRuntimeCacheIdentity #-}

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

type ContextRuntime :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ContextRuntime siteOwner site ctx fresh section report failure = ContextRuntime
  { crPreparedSite :: site -> PreparedContextSite siteOwner ctx,
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
      forall modelOwner.
      Natural ->
      ContextRefreshPrepared siteOwner modelOwner site ctx section ->
      RuntimeResolutionProgram modelOwner site ctx section report failure
  }

type role ContextRuntime nominal representational nominal representational representational representational representational

cachedContexts ::
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  [ctx]
cachedContexts =
  crCachedContexts
{-# INLINE cachedContexts #-}

freshSectionAt ::
  ContextRuntime siteOwner site ctx fresh section report failure ->
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
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  site
bootstrapContextSections =
  bootstrapContextSectionsWithBudget defaultContextRowsCacheBudgetBytes

bootstrapContextSectionsResult ::
  Ord ctx =>
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  ContextRefreshResult site ctx
bootstrapContextSectionsResult =
  bootstrapContextSectionsWithBudgetResult defaultContextRowsCacheBudgetBytes

bootstrapContextSectionsWithBudget ::
  Ord ctx =>
  Natural ->
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  site
bootstrapContextSectionsWithBudget cacheBudget runtime site0 =
  crrRefreshedSite (bootstrapContextSectionsWithBudgetResult cacheBudget runtime site0)

bootstrapContextSectionsWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  ContextRefreshResult site ctx
bootstrapContextSectionsWithBudgetResult cacheBudget runtime =
  runContextSectionRepairWithBudgetResult cacheBudget runtime Nothing

repairContextSections ::
  Ord ctx =>
  ContextRuntime siteOwner site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  site
repairContextSections runtime delta site0 =
  crrRefreshedSite (repairContextSectionsResult runtime delta site0)

repairContextSectionsResult ::
  Ord ctx =>
  ContextRuntime siteOwner site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  ContextRefreshResult site ctx
repairContextSectionsResult =
  repairContextSectionsWithBudgetResult defaultContextRowsCacheBudgetBytes

repairContextSectionsWithBudget ::
  Ord ctx =>
  Natural ->
  ContextRuntime siteOwner site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  site
repairContextSectionsWithBudget cacheBudget runtime delta site0 =
  crrRefreshedSite (repairContextSectionsWithBudgetResult cacheBudget runtime delta site0)

repairContextSectionsWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime siteOwner site ctx fresh section report failure ->
  ContextSectionRepairDelta ctx ->
  site ->
  ContextRefreshResult site ctx
repairContextSectionsWithBudgetResult cacheBudget runtime delta =
  runContextSectionRepairWithBudgetResult cacheBudget runtime (Just delta)

runContextSectionRepairWithBudgetResult ::
  Ord ctx =>
  Natural ->
  ContextRuntime siteOwner site ctx fresh section report failure ->
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
  ContextRuntime siteOwner site ctx fresh section report failure ->
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
  first
    (invariantRepairFailure bootstrapSite dirtyContexts . ContextPropagationSheafModelFailed)
    ( withSiteSheafModel runtime bootstrapSite (crrEdges restrictionRegistryValue) $ \restrictionModel -> do
        initialSection <-
          first
            (invariantRepairFailure bootstrapSite dirtyContexts . ContextPropagationInitialSectionFailed)
            (sectionFromMap restrictionModel initialSections)
        let prepared =
              ContextRefreshPrepared
                { contextRefreshPreparedSite = bootstrapSite,
                  contextRefreshPreparedContexts = contexts,
                  contextRefreshPreparedDirtyContexts = dirtyContexts,
                  contextRefreshPreparedSections = initialSections,
                  contextRefreshPreparedRestrictionModel = restrictionModel,
                  contextRefreshPreparedRuntimeCacheIdentity = currentIdentity
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
    )
    >>= id

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
  ContextRuntime siteOwner site ctx fresh section report failure ->
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
  ContextRuntime siteOwner site ctx fresh section report failure ->
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

withSiteSheafModel ::
  Ord ctx =>
  ContextRuntime siteOwner site ctx fresh section report failure ->
  site ->
  [ContextRestrictionEdge ctx] ->
  (forall owner. SheafModel owner ctx (ContextRestrictionEdge ctx) -> result) ->
  Either (SheafModelBuildError ctx) result
withSiteSheafModel runtime site restrictionEdgesValue useModel =
  let objects =
        mkObjectIndex (cachedContexts runtime site)
      planFingerprint =
        crciBaseRevision (crRuntimeCacheIdentity runtime site)
   in withPreparedSheafModel
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
        useModel

sectionFromMap ::
  Ord ctx =>
  SheafModel owner ctx witness ->
  Map ctx section ->
  Either (SectionStoreError ctx) (TotalSectionStore owner ctx section)
sectionFromMap model sections =
  first SectionStoreConstructionFailed (mkTotalSectionStore model sections)
