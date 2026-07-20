{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Runtime.Compile.Row
  ( RowRuntimeIdentity (..),
    RowRuntimeSelection (..),
    StoredRowRuntime (..),
    RowRuntimeResolutionSpec (..),
    dirtyCellsToPatch,
    selectRuntime,
    observeResolvedSection,
    runPersistentRowResolution,
    compileRowResolutionProgram,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Runtime.Apply
  ( applyPatch,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime,
    RuntimeApplyError,
  )
import Moonlight.Flow.Patch
  ( Patch,
  )
import Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionProgram (..),
  )
import Moonlight.Sheaf.Section.Store.Types

data RowRuntimeIdentity = RowRuntimeIdentity
  { rriGeneratedSiteFingerprint :: !Int,
    rriContextLatticeFingerprint :: !Int,
    rriPlanFingerprint :: !Int,
    rriQuotientEpochFingerprint :: !Int,
    rriLiveEpochFingerprint :: !Int,
    rriRuntimeFingerprint :: !Int,
    rriRoutingFingerprint :: !Int,
    rriVisibleCachePolicyFingerprint :: !Int
  }
  deriving stock (Eq, Ord, Show)

type RowRuntimeSelection :: Type
data RowRuntimeSelection
  = RowRuntimeReused
  | RowRuntimeRebuilt
  deriving stock (Eq, Ord, Show, Read)

data StoredRowRuntime ctx prop = StoredRowRuntime
  { srrIdentity :: !RowRuntimeIdentity,
    srrRuntime :: !(Runtime ctx prop)
  }

type RowRuntimeResolutionSpec ::
  Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure =
  RowRuntimeResolutionSpec
    { rrsInitialDirtyCells :: !(Set cell),
      rrsStoredRuntime ::
        site ->
        Maybe (StoredRowRuntime ctx prop),
      rrsSetStoredRuntime ::
        Maybe (StoredRowRuntime ctx prop) ->
        site ->
        site,
      rrsRuntimeIdentity ::
        site ->
        Set cell ->
        TotalSectionStore owner cell stalk ->
        RowRuntimeIdentity,
      rrsBuildRuntime ::
        site ->
        Set cell ->
        TotalSectionStore owner cell stalk ->
        Either failure (Runtime ctx prop),
      rrsDirtyCellsToRelationalScope ::
        Set cell ->
        site ->
        TotalSectionStore owner cell stalk ->
        Either failure RelationalScope,
      rrsRelationalScopeToSite ::
        RelationalScope ->
        Set cell ->
        site ->
        TotalSectionStore owner cell stalk ->
        Either failure site,
      rrsRelationalScopeToPatch ::
        RelationalScope ->
        Set cell ->
        site ->
        TotalSectionStore owner cell stalk ->
        Either failure (site, Patch),
      rrsReadResolvedEntries ::
        Set cell ->
        site ->
        Runtime ctx prop ->
        TotalSectionStore owner cell stalk ->
        Either
          failure
          (Map cell stalk),
      rrsRebuildResolvedSection ::
        TotalSectionStore owner cell stalk ->
        Map cell stalk ->
        Either failure (TotalSectionStore owner cell stalk),
      rrsReportFromRuntime ::
        Set cell ->
        RowRuntimeSelection ->
        site ->
        Runtime ctx prop ->
        TotalSectionStore owner cell stalk ->
        TotalSectionStore owner cell stalk ->
        Either failure report,
      rrsRuntimeApplyFailure ::
        RuntimeApplyError ctx prop ->
        failure
    }

dirtyCellsToPatch ::
  RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure ->
  Set cell ->
  site ->
  TotalSectionStore owner cell stalk ->
  Either failure (site, Patch)
dirtyCellsToPatch spec dirtyCells site0 section0 = do
  dirtyKeys <-
    rrsDirtyCellsToRelationalScope spec dirtyCells site0 section0

  site1 <-
    rrsRelationalScopeToSite spec dirtyKeys dirtyCells site0 section0

  rrsRelationalScopeToPatch spec dirtyKeys dirtyCells site1 section0

selectRuntime ::
  RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure ->
  Set cell ->
  site ->
  TotalSectionStore owner cell stalk ->
  Either failure (RowRuntimeSelection, Runtime ctx prop)
selectRuntime spec dirtyCells site section0 =
  case rrsStoredRuntime spec site of
    Just storedRuntime
      | srrIdentity storedRuntime == runtimeIdentity ->
          Right (RowRuntimeReused, srrRuntime storedRuntime)
    _ -> do
      runtime <-
        rrsBuildRuntime spec site dirtyCells section0
      Right (RowRuntimeRebuilt, runtime)
  where
    runtimeIdentity =
      rrsRuntimeIdentity spec site dirtyCells section0

observeResolvedSection ::
  RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure ->
  Set cell ->
  RowRuntimeSelection ->
  site ->
  Runtime ctx prop ->
  TotalSectionStore owner cell stalk ->
  Either
    failure
    (TotalSectionStore owner cell stalk, report)
observeResolvedSection spec dirtyCells selection site runtime0 section0 = do
  resolvedEntries <-
    rrsReadResolvedEntries spec dirtyCells site runtime0 section0

  resolvedSection <-
    rrsRebuildResolvedSection spec section0 resolvedEntries

  report <-
    rrsReportFromRuntime spec dirtyCells selection site runtime0 section0 resolvedSection

  pure (resolvedSection, report)

runPersistentRowResolution ::
  RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure ->
  Set cell ->
  site ->
  TotalSectionStore owner cell stalk ->
  Either failure (site, TotalSectionStore owner cell stalk, report)
runPersistentRowResolution spec dirtyCells site0 section0 = do
  (site1, runtimePatch) <-
    dirtyCellsToPatch spec dirtyCells site0 section0

  (!selection, !runtime0) <-
    selectRuntime spec dirtyCells site1 section0

  runtime1 <-
    first
      (rrsRuntimeApplyFailure spec)
      (applyPatch runtimePatch runtime0)

  (resolvedSection, report) <-
    observeResolvedSection spec dirtyCells selection site1 runtime1 section0

  let site2 =
        rrsSetStoredRuntime
          spec
          ( Just
              StoredRowRuntime
                { srrIdentity = rrsRuntimeIdentity spec site1 dirtyCells section0,
                  srrRuntime = runtime1
                }
          )
          site1

  pure (site2, resolvedSection, report)

compileRowResolutionProgram ::
  RowRuntimeResolutionSpec owner site ctx prop cell stalk report failure ->
  RuntimeResolutionProgram owner site cell stalk report failure
compileRowResolutionProgram spec =
  RuntimeResolutionProgram
    { rrpInitialDirtyCells = rrsInitialDirtyCells spec,
      rrpRunDirtyCells = runPersistentRowResolution spec
    }
