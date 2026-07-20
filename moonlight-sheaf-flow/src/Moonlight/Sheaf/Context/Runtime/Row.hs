{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Context.Runtime.Row
  ( RowRuntimeIdentity (..),
    RowRuntimeSelection (..),
    StoredRowRuntime (..),
    ContextRowRefreshSpec (..),
    compileContextRowRefresh,
    refreshDirtyContextRows,
    readVisibleContextSections,
    defaultRebuildResolvedContextSection,
  )
where

import Control.Monad
  ( foldM,
  )
import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
  )
import Control.Monad.Trans.State.Strict
  ( StateT,
    runStateT,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Differential.Context.RowsCache
  ( ContextRowsCache,
    ContextRowsRuntime,
    ContextRowsSourceSelectionError,
    dropContextRowsFor,
    getContextRows,
    resizeContextRowsCache,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime,
    RuntimeApplyError,
    RuntimeReadError,
    RuntimeSection (..),
  )
import Moonlight.Flow.Runtime.Visible
  ( visibleContext,
  )
import Moonlight.Flow.Patch
  ( Patch,
  )
import Moonlight.Sheaf.Context.Runtime
  ( ContextRefreshPrepared,
    crpContexts,
    crpDirtyContexts,
    crpRestrictionModel,
    crpSections,
  )
import Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionProgram,
  )
import Moonlight.Sheaf.Runtime.Compile.Row
  ( RowRuntimeIdentity (..),
    RowRuntimeResolutionSpec (..),
    RowRuntimeSelection (..),
    StoredRowRuntime (..),
    compileRowResolutionProgram,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

type ContextRowRefreshSpec ::
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
data ContextRowRefreshSpec siteOwner modelOwner site ctx rows section prop report failure =
  ContextRowRefreshSpec
    { crrsStoredRuntime ::
        site ->
        Maybe (StoredRowRuntime ctx prop),
      crrsSetStoredRuntime ::
        Maybe (StoredRowRuntime ctx prop) ->
        site ->
        site,
      crrsRuntimeIdentity ::
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        Set ctx ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        RowRuntimeIdentity,
      crrsRowsCache ::
        site ->
        ContextRowsCache ctx rows,
      crrsSetRowsCache ::
        ContextRowsCache ctx rows ->
        site ->
        site,
      crrsRowsRuntime ::
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        ContextRowsRuntime (Either failure) ctx rows,
      crrsRowsCacheSelectionFailure ::
        ContextRowsSourceSelectionError ctx ->
        failure,
      crrsBuildRuntime ::
        Natural ->
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        Set ctx ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        Either failure (Runtime ctx prop),
      crrsDirtyContextsToRelationalScope ::
        Set ctx ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        Either failure RelationalScope,
      crrsRelationalScopeToSite ::
        RelationalScope ->
        Set ctx ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        Either failure site,
      crrsRelationalScopeToPatch ::
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        RelationalScope ->
        Set ctx ->
        Map ctx rows ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        Either failure Patch,
      crrsVisibleSectionToSection ::
        ctx ->
        site ->
        TotalSectionStore modelOwner ctx section ->
        RelationalSection ctx Carrier prop ->
        Either failure section,
      crrsRebuildResolvedSection ::
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        TotalSectionStore modelOwner ctx section ->
        Map ctx section ->
        Either failure (TotalSectionStore modelOwner ctx section),
      crrsReportFromRuntime ::
        ContextRefreshPrepared siteOwner modelOwner site ctx section ->
        Set ctx ->
        RowRuntimeSelection ->
        site ->
        Runtime ctx prop ->
        TotalSectionStore modelOwner ctx section ->
        TotalSectionStore modelOwner ctx section ->
        Either failure report,
      crrsRuntimeApplyFailure ::
        RuntimeApplyError ctx prop ->
        failure,
      crrsRuntimeReadFailure ::
        RuntimeReadError ctx prop ->
        failure
    }

compileContextRowRefresh ::
  (Ord ctx, Ord prop) =>
  ContextRowRefreshSpec siteOwner modelOwner site ctx rows section prop report failure ->
  Natural ->
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  RuntimeResolutionProgram modelOwner site ctx section report failure
compileContextRowRefresh spec cacheBudget prepared =
  compileRowResolutionProgram
    RowRuntimeResolutionSpec
      { rrsInitialDirtyCells =
          crpDirtyContexts prepared,
        rrsStoredRuntime =
          crrsStoredRuntime spec,
        rrsSetStoredRuntime =
          crrsSetStoredRuntime spec,
        rrsRuntimeIdentity =
          \site dirtyContexts section0 ->
            crrsRuntimeIdentity spec prepared dirtyContexts site section0,
        rrsBuildRuntime =
          \site dirtyContexts section0 ->
            crrsBuildRuntime spec cacheBudget prepared dirtyContexts site section0,
        rrsDirtyCellsToRelationalScope =
          crrsDirtyContextsToRelationalScope spec,
        rrsRelationalScopeToSite =
          crrsRelationalScopeToSite spec,
        rrsRelationalScopeToPatch =
          \dirtyKeys dirtyContexts site0 section0 -> do
            (dirtyRows, site1) <-
              refreshDirtyContextRows spec cacheBudget prepared dirtyContexts site0 section0
            runtimePatch <-
              crrsRelationalScopeToPatch spec prepared dirtyKeys dirtyContexts dirtyRows site1 section0
            pure (site1, runtimePatch),

        -- Full-snapshot readback by design.
        --
        -- Dirty contexts select the refresh frontier and the runtime patch, but
        -- applying that patch may change the visible runtime section of a
        -- non-dirty prepared context. Reading every prepared context here keeps
        -- the rebuilt TotalSectionStore a complete post-runtime snapshot.
        --
        -- Do not replace this with dirty-only readback unless the caller proves:
        --
        --   visibleContext c runtimeAfter == visibleContext c runtimeBefore
        --
        -- for every prepared context c outside the read set.
        rrsReadResolvedEntries =
          \_dirtyContexts site runtime0 section0 ->
            readVisibleContextSections spec prepared site section0 runtime0,
        rrsRebuildResolvedSection =
          crrsRebuildResolvedSection spec prepared,
        rrsReportFromRuntime =
          \dirtyContexts selection site runtime section0 resolvedSection ->
            crrsReportFromRuntime spec prepared dirtyContexts selection site runtime section0 resolvedSection,
        rrsRuntimeApplyFailure =
          crrsRuntimeApplyFailure spec
      }

refreshDirtyContextRows ::
  Ord ctx =>
  ContextRowRefreshSpec siteOwner modelOwner site ctx rows section prop report failure ->
  Natural ->
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  Set ctx ->
  site ->
  TotalSectionStore modelOwner ctx section ->
  Either failure (Map ctx rows, site)
refreshDirtyContextRows spec cacheBudget prepared dirtyContexts site0 section0
  | Set.null dirtyContexts =
      Right (Map.empty, site0)
  | otherwise = do
      let rowsRuntime =
            crrsRowsRuntime spec prepared site0 section0
          (_refusedPinnedKeys, cache0) =
            dropContextRowsFor dirtyContexts $
              resizeContextRowsCache cacheBudget (crrsRowsCache spec site0)
      (dirtyRowsResult, cache1) <-
        runStateT
          (traverseDirtyRows rowsRuntime dirtyContexts)
          cache0
      dirtyRows <-
        first (crrsRowsCacheSelectionFailure spec) dirtyRowsResult
      pure (dirtyRows, crrsSetRowsCache spec cache1 site0)

traverseDirtyRows ::
  Ord ctx =>
  ContextRowsRuntime (Either failure) ctx rows ->
  Set ctx ->
  StateT
    (ContextRowsCache ctx rows)
    (Either failure)
    (Either (ContextRowsSourceSelectionError ctx) (Map ctx rows))
traverseDirtyRows rowsRuntime dirtyContexts =
  runExceptT
    ( Map.fromAscList
        <$> traverse
          (\contextValue -> (contextValue,) <$> ExceptT (getContextRows rowsRuntime contextValue))
          contexts
    )
  where
    contexts =
      Set.toAscList dirtyContexts

readVisibleContextSections ::
  (Ord ctx, Ord prop) =>
  ContextRowRefreshSpec siteOwner modelOwner site ctx rows section prop report failure ->
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  site ->
  TotalSectionStore modelOwner ctx section ->
  Runtime ctx prop ->
  Either
    failure
    (Map ctx section)
readVisibleContextSections spec prepared site section0 runtime0 =
  foldM readOne Map.empty (crpContexts prepared)
  where
    readOne !entries contextValue = do
      (_visibleRuntime, runtimeSection) <-
        first
          (crrsRuntimeReadFailure spec)
          (visibleContext contextValue runtime0)
      let !visibleSection =
            unRuntimeSection runtimeSection
      sectionValue <-
        crrsVisibleSectionToSection spec contextValue site section0 visibleSection
      pure (Map.insert contextValue sectionValue entries)

defaultRebuildResolvedContextSection ::
  Ord ctx =>
  (SectionConstructionError ctx -> failure) ->
  ContextRefreshPrepared siteOwner modelOwner site ctx section ->
  TotalSectionStore modelOwner ctx section ->
  Map ctx section ->
  Either failure (TotalSectionStore modelOwner ctx section)
defaultRebuildResolvedContextSection mapConstructionFailure prepared _section0 resolvedEntries =
  first mapConstructionFailure $
    mkTotalSectionStore
      (crpRestrictionModel prepared)
      (Map.union resolvedEntries (crpSections prepared))
