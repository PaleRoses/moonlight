{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Context.Core
  ( ContextLattice (..),
    contextRefinesTo,
    ClassSiteSupport,
    ContextRestrictionMismatch (..),
    mismatchAtKey,
    ContextResolutionStatus (..),
    ContextPropagationReport (..),
    settledPropagationReport,
    ContextPropagationFailure (..),
    contextPropagationChangedContexts,
    contextPropagationSettled,
    contextPropagationObstructionCount,
    AnalysisRestrictionMismatch (..),
    SectionMismatch (..),
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice (..),
    ContextLatticeLookupError,
    SupportBasis,
    leqContext
  )

contextRefinesTo :: Ord ctx => ContextLattice ctx -> ctx -> ctx -> Either (ContextLatticeLookupError ctx) Bool
contextRefinesTo latticeValue sourceContext targetContext =
  leqContext latticeValue targetContext sourceContext

type ClassSiteSupport :: Type -> Type
type ClassSiteSupport ctx = SupportBasis ctx

type ContextRestrictionMismatch :: Type -> Type
data ContextRestrictionMismatch classId = ContextRestrictionMismatch
  { crmClassKey :: Int,
    crmExpectedRepresentative :: Maybe classId,
    crmActualRepresentative :: Maybe classId
  }
  deriving stock (Eq, Ord, Show)

mismatchAtKey :: Eq classId => IntMap classId -> IntMap classId -> Int -> [ContextRestrictionMismatch classId]
mismatchAtKey leftEntries rightEntries key =
  let expectedRepresentative = IntMap.lookup key leftEntries
      actualRepresentative = IntMap.lookup key rightEntries
   in [ ContextRestrictionMismatch
          { crmClassKey = key,
            crmExpectedRepresentative = expectedRepresentative,
            crmActualRepresentative = actualRepresentative
          }
      | expectedRepresentative /= actualRepresentative
      ]

type ContextResolutionStatus :: Type
data ContextResolutionStatus
  = ContextResolutionSettled
  | ContextResolutionObstructed !Int
  deriving stock (Eq, Ord, Show)

type ContextPropagationReport :: Type -> Type
data ContextPropagationReport ctx = ContextPropagationReport
  { cprChangedContexts :: !(Set ctx),
    cprStatus :: !ContextResolutionStatus
  }
  deriving stock (Eq, Show)

settledPropagationReport ::
  Set ctx ->
  ContextPropagationReport ctx
settledPropagationReport changedContexts =
  ContextPropagationReport
    { cprChangedContexts = changedContexts,
      cprStatus = ContextResolutionSettled
    }
{-# INLINE settledPropagationReport #-}

type ContextPropagationFailure :: Type -> Type -> Type -> Type
data ContextPropagationFailure ctx invariant failure
  = ContextPropagationInvariantViolation !invariant
  | ContextPropagationRuntimeFailure !failure
  deriving stock (Eq, Ord, Show)

contextPropagationChangedContexts :: ContextPropagationReport ctx -> [ctx]
contextPropagationChangedContexts =
  Set.toAscList . cprChangedContexts
{-# INLINE contextPropagationChangedContexts #-}

contextPropagationSettled :: ContextPropagationReport ctx -> Bool
contextPropagationSettled report =
  case cprStatus report of
    ContextResolutionSettled ->
      True
    ContextResolutionObstructed _ ->
      False
{-# INLINE contextPropagationSettled #-}

contextPropagationObstructionCount :: ContextPropagationReport ctx -> Int
contextPropagationObstructionCount report =
  case cprStatus report of
    ContextResolutionSettled ->
      0
    ContextResolutionObstructed obstructionCount ->
      obstructionCount
{-# INLINE contextPropagationObstructionCount #-}

type AnalysisRestrictionMismatch :: Type -> Type
data AnalysisRestrictionMismatch a = AnalysisRestrictionMismatch
  { armClassKey :: Int,
    armExpectedAnalysis :: Maybe a,
    armActualAnalysis :: Maybe a
  }
  deriving stock (Eq, Ord, Show)

type SectionMismatch :: Type -> Type -> Type
data SectionMismatch classId a
  = OnlyClass !(ContextRestrictionMismatch classId)
  | OnlyAnalysis !(AnalysisRestrictionMismatch a)
  | BothMismatch !(ContextRestrictionMismatch classId) !(AnalysisRestrictionMismatch a)
  deriving stock (Eq, Ord, Show)
