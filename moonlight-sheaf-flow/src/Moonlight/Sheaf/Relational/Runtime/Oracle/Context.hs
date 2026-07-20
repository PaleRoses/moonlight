{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Relational.Runtime.Oracle.Context
  ( ContextOraclePort (..),
    ContextOracleMismatch (..),
    projectContextOracle,
    compareContextOracleAgainstRuntime,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime,
    RuntimeReadError,
    RuntimeSection (..),
  )
import Moonlight.Flow.Runtime.Visible
  ( visibleContext,
  )
import Moonlight.Sheaf.Context.Runtime
  ( ContextRuntime (..),
    ContextRefreshResult (..),
    cachedContexts,
    contextSectionRepairDelta,
    freshSectionAt,
    repairContextSectionsResult,
  )

data ContextOraclePort siteOwner site ctx fresh section report failure prop = ContextOraclePort
  { coaRuntime :: !(ContextRuntime siteOwner site ctx fresh section report failure),
    coaProjectSection ::
      ctx ->
      section ->
      RelationalSection ctx Carrier prop
  }

data ContextOracleMismatch ctx carrier prop
  = ContextOracleSectionMismatch
      !ctx
      !(RelationalSection ctx carrier prop)
      !(RelationalSection ctx carrier prop)
  | ContextOracleRuntimeReadFailed
      !(RuntimeReadError ctx prop)
  deriving stock (Eq, Show)

projectContextOracle ::
  Ord ctx =>
  ContextOraclePort siteOwner site ctx fresh section report failure prop ->
  site ->
  Map ctx (RelationalSection ctx Carrier prop)
projectContextOracle adapter site0 =
  projectContextOracleForContexts
    adapter
    (Set.fromList (cachedContexts runtime site0))
    site0
  where
    runtime =
      coaRuntime adapter

projectContextOracleForContexts ::
  Ord ctx =>
  ContextOraclePort siteOwner site ctx fresh section report failure prop ->
  Set ctx ->
  site ->
  Map ctx (RelationalSection ctx Carrier prop)
projectContextOracleForContexts adapter contexts site =
  let runtime =
        coaRuntime adapter
      stored =
        crStoredSections runtime site
      sectionAt contextValue =
        Map.findWithDefault
          (freshSectionAt runtime contextValue site)
          contextValue
          stored
   in Map.fromSet
        (\contextValue ->
           coaProjectSection adapter contextValue (sectionAt contextValue)
        )
        contexts

compareContextOracleAgainstRuntime ::
  (Ord ctx, Ord prop) =>
  ContextOraclePort siteOwner site ctx fresh section report failure prop ->
  Runtime ctx prop ->
  site ->
  Either (ContextOracleMismatch ctx Carrier prop) ()
compareContextOracleAgainstRuntime adapter runtime site =
  let contextRuntime =
        coaRuntime adapter
      ContextRefreshResult site1 dirtyContexts =
        repairContextSectionsResult
          contextRuntime
          (contextSectionRepairDelta (crDirtyContexts contextRuntime site))
          site
      expected =
        projectContextOracleForContexts adapter dirtyContexts site1
   in Map.foldlWithKey'
        compareOne
        (Right ())
        expected
  where
    compareOne eitherUnit contextValue expectedSection = do
      eitherUnit
      (_visibleRuntime, runtimeSection) <-
        first
          ContextOracleRuntimeReadFailed
          (visibleContext contextValue runtime)
      let actualSection =
            unRuntimeSection runtimeSection
      if expectedSection == actualSection
        then Right ()
        else
          Left
            ( ContextOracleSectionMismatch
                contextValue
                expectedSection
                actualSection
            )
