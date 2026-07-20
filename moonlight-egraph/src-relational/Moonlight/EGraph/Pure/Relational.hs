{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Relational
  ( QueryPlan, EGraphPreparedBase,
    buildPreparedBase, patchPreparedBaseWith, preparedBaseRowBlocks,
    egraphRowsByAtomFromDatabaseRows,
    egraphRowsByAtomFromPhysicalRows,
    egraphRowsByAtomFromStructuralStore,
    structuralRowsForOperator,
    structuralRowsForResultKeys,
    atomizeCompiledPatternQuery, compiledPatternQueryFingerprint,
    PatternAtomizeObstruction (..), quotientPatchFromRowDeltas,
    EGraphRelationalMatchObstruction (..), RegionalAssignmentObstruction (..),
    EGraphPreparedMatchState, emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateDirty,
    markEGraphPreparedMatchStateAnnotatedDirty,
    refreshEGraphPreparedMatchStateAnnotatedRevisions,
    resetEGraphPreparedMatchState,
    preparedPlanTemplate,
    preparedPlanCacheSize,
    PreparedQueryKey,
    compiledPatternQueryKey,
    wcojPreparedRegionalDeltaMatchCompiledWithRoots,
    wcojPreparedRegionalDeltaMatchCompiledWithRootFilter,
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots,
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter,
    wcojPreparedDeltaMatchCompiledWithRoots,
    wcojPreparedDeltaMatchCompiledWithRootFilter,
    PreparedBaseMatchMemo, emptyPreparedBaseMatchMemo,
    preparedBaseMatchMemoResultCount,
    wcojPreparedSharedBaseDeltaMatchCompiledWithRoots,
    wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter,
    wcojPreparedMatchCompiledWithRoots,
    wcojPreparedMatchCompiledWithRootFilter,
    wcojMatchCompiledWithRoots, wcojMatchCompiledWithRootFilter,
  )
where

import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)
import Moonlight.Core
  ( Language,
    Operator (..),
    Pattern (..),
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Core (QuotientEpoch)
import Moonlight.Core
  ( PatternFreeJoinPlan (..),
    QueryBinding (..),
    QueryTerm (..),
    compilePatternsFreeJoinPlan,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    AnnotatedRow (..),
    absorbedRowsAtKey,
    annotatedRepresentativeKeyAt,
    annotatedRowsAtKey,
    annotatedVariantRowsForTag,
  )
import Moonlight.Sheaf.Context.Site (ContextObjectKey)
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionEmpty,
    regionJoin,
    regionMeet,
    regionTop,
    regionVoid,
  )
import Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter (..),
    canonicalRootKeys,
    rootClassAllowed,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    EGraphRevision,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphEditDeltaNull,
    eGraphPendingDelta,
    eGraphRevision,
    eGraphRevisionValue,
    eGraphStore,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..),
    atomPatchFromRowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
    rowDeltaNull
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    RowTupleKey,
    tupleKeyFromRepKeys,
    tupleKeyIndex,
    tupleKeyToRepKeys,
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    plainRowPatchFromList,
  )
import Moonlight.EGraph.Pure.Relational.Source
  ( dirtyEGraphRowsByAtomFromStructuralStore,
    egraphRowsByAtomFromDatabaseRows,
    egraphRowsByAtomFromPhysicalRows,
    egraphRowsByAtomFromStructuralStore,
    physicalRowsByTagFromTagRows,
    projectPhysicalAtomSpecRow,
    rowsByResultForAtomSpec,
    structuralRowsByTag,
    structuralRowsByTagForCanonicalResultKeys,
    structuralRowsForOperator,
    structuralRowsForResultKeys,
  )
import Moonlight.EGraph.Pure.Relational.Direct
  ( DirectPatternShape (..),
    classifyCompiledPatternQuery,
    directPatternDeltaMatches,
    directPatternMatches,
  )
import Moonlight.Flow.Internal.Digest (stableHashString64)
import Moonlight.Flow.Plan.Compile.Atomize
  ( PatternAtomizeHost (..),
    PatternAtomizeObstruction (..),
  )
import Moonlight.Flow.Plan.Compile.Atomize qualified as RelAtomize
import Moonlight.Flow.Storage.Relation
  ( atomRowsFromTupleKeys,
    materializeAtomRow,
  )
import Moonlight.Flow.Storage.Plan qualified as StoragePlan
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBlockIdentity (..),
    RowState (Canonical),
  )
import Moonlight.Differential.Row.Block qualified as Row
import Moonlight.Flow.Model.Scope
  ( relationalScopeFromSets,
  )
import Moonlight.Flow.Execution.Direct qualified as RelRuntime
import Moonlight.Saturation.Context.Match.Types.Plan qualified as SaturationMatch
import Moonlight.Rewrite.System
  ( CompiledGuard,
    compiledGuardCanonicalNodeWordsWith,
  )
import Moonlight.Core
  ( Substitution,
    emptySubstitution,
    insertSubst,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqCondition,
    cpqQuery,
    patternQueryPatterns,
  )

type QueryPlan :: Type -> (Type -> Type) -> Type
type QueryPlan capability f =
  RelPlan.QueryPlan
    (CompiledPatternQuery (CompiledGuard capability f) f)
    (ClassId, Substitution)
    (CompiledGuard capability f)
    (f ())
    (ENode f)
    ClassId

type EGraphPreparedBase :: Type -> (Type -> Type) -> Type
data EGraphPreparedBase capability f = EGraphPreparedBase
  { epbPlan :: !(QueryPlan capability f),
    epbRowsByResult :: !(IntMap (IntMap [RowTupleKey]))
  }

instance RelPlan.QueryOutput (ClassId, Substitution) ClassId where
  type OutputVar (ClassId, Substitution) ClassId = EGraph.PatternVar

  data OutputRecipe (ClassId, Substitution) ClassId =
    EGraphOutputRecipe !(Vector.Vector EGraph.PatternVar)

  mkOutputRecipe =
    EGraphOutputRecipe . Vector.fromList

  projectOutputRecipe (EGraphOutputRecipe patternVars) rootClass bindingValues =
    if Vector.length patternVars /= Vector.length bindingValues
      then
        Left
          ( RelPlan.OutputBindingArityMismatch
              (Vector.length patternVars)
              (Vector.length bindingValues)
          )
      else
        Right
          ( rootClass,
            Vector.foldl'
              (\subst (patternVar, classIdValue) -> insertSubst patternVar classIdValue subst)
              emptySubstitution
              (Vector.zip patternVars bindingValues)
          )

instance SaturationMatch.PresheafCarrier (RowBlock 'Canonical) where
  emptySection =
    Row.emptyRowBlock

  fromRows =
    id

  sectionRows =
    id

  sectionSchema =
    Row.rowBlockLayout

  restrictSection =
    Row.restrictRows

  sectionSupport =
    Row.rowBlockSupport

instance
  RelPlan.QueryOutput output key =>
  SaturationMatch.MatchPlan (RelPlan.QueryPlan compiled output guard tag tuple key)
  where
  type PlanSection (RelPlan.QueryPlan compiled output guard tag tuple key) = RowBlock 'Canonical
  type PlanOutput (RelPlan.QueryPlan compiled output guard tag tuple key) = output
  type PlanAtom (RelPlan.QueryPlan compiled output guard tag tuple key) = RelPlan.AtomSpec tag tuple key
  type PlanObstruction (RelPlan.QueryPlan compiled output guard tag tuple key) = RelRuntime.RuntimeQueryPlanObstruction

  planId =
    RelPlan.qpId

  setPlanId queryId plan =
    plan {RelPlan.qpId = queryId}

  planFingerprint =
    SaturationMatch.QueryFingerprint . RelPlan.qpFingerprint

  planAtoms =
    RelPlan.qpAtoms

  runJoinRows =
    RelRuntime.evalPlanRows

  existsPinnedRow =
    RelRuntime.evalPinnedRow

  planSupportRows =
    RelRuntime.evalPlanSupportRows

  outputProjectRow =
    RelRuntime.evalPlanOutputAt

  atomSpecId =
    RelPlan.queryAtomAsAtomId . RelPlan.asQueryAtomId

  atomColumns =
    RelPlan.asColumns

egraphPatternAtomizeHost ::
  (Language f, Show (f ()), Show capability) =>
  PatternAtomizeHost
    (CompiledPatternQuery (CompiledGuard capability f) f)
    (Pattern f)
    EGraph.PatternVar
    (CompiledGuard capability f)
    (f ())
    (ENode f)
    ClassId
    (ClassId, Substitution)
egraphPatternAtomizeHost =
  PatternAtomizeHost
    { pahQueryPatterns = patternQueryPatterns . cpqQuery,
      pahQueryResidualGuard = cpqCondition,
      pahResidualWords = compiledGuardCanonicalNodeWordsWith (stableHashString64 . show) (stableHashString64 . show),
      pahPatternVar = \case
        PatternVar patternVar -> Just patternVar
        PatternNode _ -> Nothing,
      pahPatternNode = \case
        PatternVar _ -> Nothing
        PatternNode patternNode -> Just (void patternNode, Foldable.toList patternNode),
      pahPatternVarKey = EGraph.patternVarKey,
      pahTagDigest = stableHashString64 . show
    }

compiledPatternQueryFingerprint ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  Either PatternAtomizeObstruction Int
compiledPatternQueryFingerprint =
  RelAtomize.compiledPatternQueryFingerprintWith egraphPatternAtomizeHost

atomizeCompiledPatternQuery ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  Either PatternAtomizeObstruction (QueryPlan capability f)
atomizeCompiledPatternQuery =
  RelAtomize.atomizePatternQueryWith egraphPatternAtomizeHost

buildPreparedBase ::
  Language f =>
  QueryPlan capability f ->
  EGraph f a ->
  EGraphPreparedBase capability f
buildPreparedBase plan graph =
  EGraphPreparedBase
    { epbPlan = plan,
      epbRowsByResult =
        egraphRowsByAtomFromStructuralStore plan (canonicalizeClassId graph) (eGraphStore graph)
    }

patchPreparedBaseWith ::
  Language f =>
  EGraph f a ->
  IntSet ->
  EGraphPreparedBase capability f ->
  (EGraphPreparedBase capability f, IntMap RowDelta)
patchPreparedBaseWith graph dirtyResults preparedBase =
  ( preparedBase {epbRowsByResult = patchedRowsByResult},
    atomDeltas
  )
  where
    dirtyRowsByAtom =
      dirtyEGraphRowsByAtomFromStructuralStore
        (epbPlan preparedBase)
        (canonicalizeClassId graph)
        (eGraphStore graph)
        dirtyResults

    effectiveDirtyResults =
      rowProjectionDirtyResultKeys dirtyResults dirtyRowsByAtom

    patchedRowsByResult =
      IntMap.mapWithKey patchAtomRows (epbRowsByResult preparedBase)

    patchAtomRows atomKey oldRows =
      cursorRowsByResult
        (dirtyRowsByResultForAtom dirtyRowsByAtom effectiveDirtyResults atomKey)
        oldRows
        effectiveDirtyResults

    atomDeltas =
      IntMap.mapMaybeWithKey
        ( \atomKey oldRows ->
            let rowDelta =
                  rowDeltaForDirtyResults
                    oldRows
                    (dirtyRowsByResultForAtom dirtyRowsByAtom effectiveDirtyResults atomKey)
                    effectiveDirtyResults
             in if rowDeltaNull rowDelta
                  then Nothing
                  else Just rowDelta
        )
        (epbRowsByResult preparedBase)

preparedBaseRowBlocks ::
  Int ->
  EGraphPreparedBase capability f ->
  Either Row.RowBuildError (IntMap (RowBlock 'Canonical))
preparedBaseRowBlocks baseRevision preparedBase =
  IntMap.fromList
    <$> traverse
      atomRows
      (Vector.toList (RelPlan.qpAtoms (epbPlan preparedBase)))
  where
    atomRows atomSpec =
      let atomKey =
            RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
       in fmap ((,) atomKey) $
            atomRowsFromTupleKeys
              (preparedBaseRowIdentity baseRevision (epbPlan preparedBase) atomSpec)
              (RelPlan.asColumns atomSpec)
              (flattenRowsByResult (IntMap.findWithDefault IntMap.empty atomKey (epbRowsByResult preparedBase)))

preparedBaseRowIdentity ::
  Int ->
  QueryPlan capability f ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  RowBlockIdentity
preparedBaseRowIdentity baseRevision queryPlan atomSpec =
  RowBlockIdentity
    { rowBlockBaseRevision = baseRevision,
      rowBlockOverlayEpoch = 0,
      rowBlockPlanFingerprint = RelPlan.qpFingerprint queryPlan,
      rowBlockEntityKey = RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec),
      rowBlockGeneration = 0
    }

flattenRowsByResult :: IntMap [RowTupleKey] -> [RowTupleKey]
flattenRowsByResult =
  IntMap.foldr (<>) []
{-# INLINE flattenRowsByResult #-}

dirtyRowsByResultForAtom ::
  IntMap (IntMap [RowTupleKey]) ->
  IntSet ->
  Int ->
  IntMap [RowTupleKey]
dirtyRowsByResultForAtom dirtyRowsByAtom dirtyResults atomKey =
  IntMap.filterWithKey
    (\resultKey _ -> IntSet.member resultKey dirtyResults)
    (IntMap.findWithDefault IntMap.empty atomKey dirtyRowsByAtom)
{-# INLINE dirtyRowsByResultForAtom #-}

cursorRowsByResult ::
  IntMap [RowTupleKey] ->
  IntMap [RowTupleKey] ->
  IntSet ->
  IntMap [RowTupleKey]
cursorRowsByResult dirtyRows oldRows dirtyResults =
  IntMap.union dirtyRows (IntMap.withoutKeys oldRows dirtyResults)
{-# INLINE cursorRowsByResult #-}

rowDeltaForDirtyResults ::
  IntMap [RowTupleKey] ->
  IntMap [RowTupleKey] ->
  IntSet ->
  RowDelta
rowDeltaForDirtyResults oldRows dirtyRows dirtyResults =
  plainRowPatchFromList
    ( IntSet.foldr
        dirtyResultEntries
        []
        dirtyResults
    )
  where
    dirtyResultEntries resultKey entries =
      weightedRows (-1) (IntMap.lookup resultKey oldRows)
        <> weightedRows 1 (IntMap.lookup resultKey dirtyRows)
        <> entries

    weightedRows :: Integer -> Maybe [RowTupleKey] -> [(RowTupleKey, MultiplicityChange)]
    weightedRows weight =
      maybe [] (fmap (\row -> (row, MultiplicityChange (fromIntegral weight))))
{-# INLINE rowDeltaForDirtyResults #-}

rowProjectionDirtyResultKeys :: Foldable rows => IntSet -> rows (IntMap [RowTupleKey]) -> IntSet
rowProjectionDirtyResultKeys =
  Foldable.foldl'
    (\resultKeys rowsByResult -> IntSet.union resultKeys (IntMap.keysSet rowsByResult))
{-# INLINE rowProjectionDirtyResultKeys #-}

rowsByResultFromRowBlock :: RowBlock 'Canonical -> IntMap [RowTupleKey]
rowsByResultFromRowBlock rows =
  Row.foldRowBlock insertRow IntMap.empty rows
  where
    insertRow rowsByResult rowDesc =
      let row =
            materializeAtomRow rows rowDesc
       in case tupleKeyIndex row 0 of
            Just (RepKey resultKey) ->
              IntMap.insertWith (<>) resultKey [row] rowsByResult
            Nothing ->
              rowsByResult
{-# INLINE rowsByResultFromRowBlock #-}

quotientPatchFromRowDeltas ::
  QuotientEpoch ->
  QuotientEpoch ->
  IntSet ->
  IntSet ->
  IntMap RowDelta ->
  QuotientPatch
quotientPatchFromRowDeltas epochBefore epochAfter dirtyKeys dirtyTopo rowDeltas =
  let patchScope =
        relationalScopeFromSets dirtyKeys dirtyTopo IntSet.empty dirtyKeys dirtyKeys
      patchEvents =
        fmap
          atomPatchFromRowDelta
          (IntMap.filter (not . rowDeltaNull) rowDeltas)
   in QuotientPatch
        { qpEpoch =
            EpochTransition
              { etBefore = epochBefore,
                etAfter = epochAfter
              },
          qpScope = patchScope,
          qpAtomScopeByAtom = patchScope <$ patchEvents,
          qpEvents = patchEvents
        }

type RegionalAssignmentObstruction :: Type
data RegionalAssignmentObstruction
  = RegionalAssignmentArityMismatch !Int !Int
  | RegionalAssignmentSlotMissing !RelPlan.SlotId
  deriving stock (Eq, Show)

data EGraphRelationalMatchObstruction
  = EGraphRelationalAtomizeObstruction !PatternAtomizeObstruction
  | EGraphRelationalRuntimeQueryObstruction !RelRuntime.RuntimeQueryPlanObstruction
  | EGraphRelationalDirtySnapshot
  | EGraphRelationalRegionalAssignmentObstruction !RegionalAssignmentObstruction
  deriving stock (Eq, Show)

type PreparedMatchDelta :: Type
newtype PreparedMatchDelta = PreparedMatchDelta
  { pmdDirtyKeys :: IntSet
  }

type PreparedAnnotatedSnapshotRevision :: Type
data PreparedAnnotatedSnapshotRevision = PreparedAnnotatedSnapshotRevision
  { pasrBaseRevision :: !EGraphRevision,
    pasrContextRevision :: !Natural,
    pasrRootClassFilter :: !RootClassFilter
  }

type PreparedQueryKey :: (Type -> Type) -> Type
type PreparedQueryKey f = NonEmpty (Pattern f)

type PreparedPlanKey :: (Type -> Type) -> Type
data PreparedPlanKey f = PreparedPlanKey
  { ppkQueryKey :: !(PreparedQueryKey f),
    ppkMemoFingerprint :: {-# UNPACK #-} !Int
  }

deriving stock instance (forall a. Ord a => Ord (f a)) => Eq (PreparedPlanKey f)

deriving stock instance (forall a. Ord a => Ord (f a)) => Ord (PreparedPlanKey f)

type PreparedPatternPlans :: Type -> (Type -> Type) -> Type
data PreparedPatternPlans capability f = PreparedPatternPlans
  { pppPlanKey :: !(PreparedPlanKey f),
    pppQueryPlan :: !(QueryPlan capability f),
    pppDecomp :: !RelPlan.DecompPlan,
    pppBindingPlan :: !(PatternFreeJoinPlan f ClassId),
    pppDirectShape :: !(DirectPatternShape f),
    pppCompiledStoragePlan :: !StoragePlan.CompiledStoragePlan
  }

preparedPatternQueryKey :: PreparedPatternPlans capability f -> PreparedQueryKey f
preparedPatternQueryKey =
  ppkQueryKey . pppPlanKey
{-# INLINE preparedPatternQueryKey #-}

type PreparedSharedTagRows :: (Type -> Type) -> Type
data PreparedSharedTagRows f = PreparedSharedTagRows
  { pstrRevision :: !EGraphRevision,
    pstrTags :: !(Set.Set (f ())),
    pstrRowsByTag :: !(Map.Map (f ()) (IntMap [RowTupleKey]))
  }

type PreparedAlphaProjectionKey :: (Type -> Type) -> Type
data PreparedAlphaProjectionKey f = PreparedAlphaProjectionKey !(f ()) !RelPlan.StalkRecipe

deriving stock instance Eq (f ()) => Eq (PreparedAlphaProjectionKey f)

deriving stock instance Ord (f ()) => Ord (PreparedAlphaProjectionKey f)

type PreparedAlphaBlockKey :: (Type -> Type) -> Type
data PreparedAlphaBlockKey f = PreparedAlphaBlockKey !(PreparedAlphaProjectionKey f) !(Vector.Vector RelPlan.SlotId)

deriving stock instance Eq (f ()) => Eq (PreparedAlphaBlockKey f)

deriving stock instance Ord (f ()) => Ord (PreparedAlphaBlockKey f)

type PreparedAlphaCache :: (Type -> Type) -> Type
data PreparedAlphaCache f = PreparedAlphaCache
  { pacRevision :: !EGraphRevision,
    pacRowsByKey :: !(Map.Map (PreparedAlphaProjectionKey f) (IntMap [RowTupleKey])),
    pacBlocksByKey :: !(Map.Map (PreparedAlphaBlockKey f) (RowBlock 'Canonical)),
    pacSources :: !(Map.Map (PreparedQueryKey f) [RelRuntime.DenseArrangement])
  }

type RegionalOutput :: Type
type RegionalOutput = (ClassId, Substitution)

type PreparedRegionalSection :: Type -> Type
data PreparedRegionalSection owner = PreparedRegionalSection
  { prsRowsByAtom :: !(IntMap (Map.Map RowTupleKey (ContextRegion owner))),
    prsAssignments :: !(Map.Map RowTupleKey RegionalOutput),
    prsAssignmentsByOutput :: !(Map.Map RegionalOutput (Set.Set RowTupleKey)),
    prsAssignmentsByAtomRow :: !(IntMap (Map.Map RowTupleKey (Set.Set RowTupleKey)))
  }

type role PreparedRegionalSection nominal

type PreparedSectionBuild :: Type -> Type -> (Type -> Type) -> Type -> Type
data PreparedSectionBuild owner capability f section = PreparedSectionBuild
  { psbState :: !(EGraphPreparedMatchState owner capability f),
    psbSections :: !(IntMap section)
  }

type role PreparedSectionBuild nominal nominal nominal representational

type EGraphPreparedMatchState :: Type -> Type -> (Type -> Type) -> Type
data EGraphPreparedMatchState owner capability f = EGraphPreparedMatchState
  { epmsPendingDeltas :: !(Map.Map (PreparedQueryKey f) PreparedMatchDelta),
    epmsPendingAnnotatedDeltas :: !(Map.Map (PreparedQueryKey f) PreparedMatchDelta),
    epmsSnapshotRevisions :: !(Map.Map (PreparedQueryKey f) EGraphRevision),
    epmsAnnotatedSnapshotRevisions :: !(Map.Map (PreparedQueryKey f) PreparedAnnotatedSnapshotRevision),
    epmsPlanCache :: !(Map.Map (PreparedPlanKey f) (PreparedPatternPlans capability f)),
    epmsRegionalSections :: !(Map.Map (PreparedPlanKey f) (PreparedRegionalSection owner)),
    epmsSharedTagRows :: !(Maybe (PreparedSharedTagRows f)),
    epmsAlphaCache :: !(Maybe (PreparedAlphaCache f))
  }

type role EGraphPreparedMatchState nominal nominal nominal

data PreparedMatchScope
  = PreparedNoMatches
  | PreparedFullMatches !RootClassFilter
  | PreparedDeltaMatches !RootClassFilter !IntSet

data PreparedAnnotatedMatchScope
  = PreparedAnnotatedNoMatches
  | PreparedAnnotatedFullMatches !RootClassFilter
  | PreparedAnnotatedDeltaMatches !RootClassFilter !IntSet

emptyEGraphPreparedMatchState :: EGraphPreparedMatchState owner capability f
emptyEGraphPreparedMatchState =
  EGraphPreparedMatchState
    { epmsPendingDeltas = Map.empty,
      epmsPendingAnnotatedDeltas = Map.empty,
      epmsSnapshotRevisions = Map.empty,
      epmsAnnotatedSnapshotRevisions = Map.empty,
      epmsPlanCache = Map.empty,
      epmsRegionalSections = Map.empty,
      epmsSharedTagRows = Nothing,
      epmsAlphaCache = Nothing
    }

resetEGraphPreparedMatchState :: EGraphPreparedMatchState owner capability f -> EGraphPreparedMatchState owner capability f
resetEGraphPreparedMatchState _ =
  emptyEGraphPreparedMatchState

-- | A fresh prepared state carrying only the compiled query plans. Plans are
-- functions of the query alone, never of the graph, so a template survives
-- any invalidation that discards row caches and snapshots.
preparedPlanTemplate :: EGraphPreparedMatchState owner capability f -> EGraphPreparedMatchState owner capability f
preparedPlanTemplate state =
  emptyEGraphPreparedMatchState {epmsPlanCache = epmsPlanCache state}

preparedPlanCacheSize :: EGraphPreparedMatchState owner capability f -> Int
preparedPlanCacheSize =
  Map.size . epmsPlanCache

markEGraphPreparedMatchStateDirty ::
  Language f =>
  IntSet ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
markEGraphPreparedMatchStateDirty dirtyKeys state
  | IntSet.null dirtyKeys =
      state
  | otherwise =
      state
        { epmsPendingDeltas =
            dirtyPreparedDeltasForKeys
              dirtyKeys
              (preparedKnownQueryKeys state)
              (epmsPendingDeltas state)
        }

refreshEGraphPreparedMatchStateAnnotatedRevisions ::
  Natural ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
refreshEGraphPreparedMatchStateAnnotatedRevisions contextRevision state =
  state
    { epmsAnnotatedSnapshotRevisions =
        fmap
          (\snapshot -> snapshot {pasrContextRevision = contextRevision})
          (epmsAnnotatedSnapshotRevisions state)
    }

markEGraphPreparedMatchStateAnnotatedDirty ::
  Language f =>
  IntSet ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
markEGraphPreparedMatchStateAnnotatedDirty dirtyKeys state
  | IntSet.null dirtyKeys =
      state
  | otherwise =
      state
        { epmsPendingAnnotatedDeltas =
            dirtyPreparedDeltasForKeys
              dirtyKeys
              (preparedKnownAnnotatedQueryKeys state)
              (epmsPendingAnnotatedDeltas state)
        }

preparedKnownQueryKeys ::
  Language f =>
  EGraphPreparedMatchState owner capability f ->
  Set.Set (PreparedQueryKey f)
preparedKnownQueryKeys state =
  foldMap (Set.singleton . preparedPatternQueryKey) (Map.elems (epmsPlanCache state))
    <> Map.keysSet (epmsPendingDeltas state)
    <> Map.keysSet (epmsSnapshotRevisions state)
{-# INLINE preparedKnownQueryKeys #-}

preparedKnownAnnotatedQueryKeys ::
  Language f =>
  EGraphPreparedMatchState owner capability f ->
  Set.Set (PreparedQueryKey f)
preparedKnownAnnotatedQueryKeys state =
  foldMap (Set.singleton . preparedPatternQueryKey) (Map.elems (epmsPlanCache state))
    <> Map.keysSet (epmsPendingAnnotatedDeltas state)
    <> Map.keysSet (epmsAnnotatedSnapshotRevisions state)
{-# INLINE preparedKnownAnnotatedQueryKeys #-}

dirtyPreparedDeltasForKeys ::
  Language f =>
  IntSet ->
  Set.Set (PreparedQueryKey f) ->
  Map.Map (PreparedQueryKey f) PreparedMatchDelta ->
  Map.Map (PreparedQueryKey f) PreparedMatchDelta
dirtyPreparedDeltasForKeys dirtyKeys queryKeys pendingDeltas =
  Foldable.foldr
    (Map.alter (Just . recordPreparedMatchDelta dirtyKeys))
    pendingDeltas
    queryKeys
{-# INLINE dirtyPreparedDeltasForKeys #-}

recordPreparedMatchDelta ::
  IntSet ->
  Maybe PreparedMatchDelta ->
  PreparedMatchDelta
recordPreparedMatchDelta dirtyKeys pendingDelta =
  PreparedMatchDelta
    { pmdDirtyKeys = maybe dirtyKeys ((dirtyKeys <>) . pmdDirtyKeys) pendingDelta
    }
{-# INLINE recordPreparedMatchDelta #-}

wcojPreparedMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability) =>
  RootClassFilter ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedMatchCompiledWithRootFilter rootClassFilter compiledQuery graph state = do
  (preparedPlans, plannedState) <-
    preparedPatternPlans compiledQuery state
  (matchedState, matches) <-
    flowPreparedMatchPreparedPatternPlans rootClassFilter graph preparedPlans plannedState
  pure (preparedMatchStateAfterRun graph (compiledPatternQueryKey compiledQuery) matchedState, matches)

wcojPreparedDeltaMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability) =>
  RootClassFilter ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedDeltaMatchCompiledWithRootFilter rootClassFilter compiledQuery graph state =
  case matchScope of
    PreparedNoMatches ->
      Right (preparedMatchStateAfterRun graph queryKey state, [])
    PreparedFullMatches scopedRootFilter ->
      wcojPreparedMatchCompiledWithRootFilter scopedRootFilter compiledQuery graph state
    PreparedDeltaMatches scopedRootFilter dirtyResults -> do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery state
      (matchedState, matches) <-
        flowPreparedDeltaMatchPreparedPatternPlans scopedRootFilter dirtyResults graph preparedPlans plannedState
      pure (preparedMatchStateAfterRun graph queryKey matchedState, matches)
  where
    queryKey =
      compiledPatternQueryKey compiledQuery

    matchScope =
      preparedScope graph rootClassFilter queryKey state

wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability, Ord (f ())) =>
  RootClassFilter ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  Natural ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter rootClassFilter buckets contextKey contextRevision compiledQuery graph state =
  case matchScope of
    PreparedAnnotatedNoMatches ->
      Right (preparedAnnotatedMatchStateAfterRun graph contextRevision rootClassFilter queryKey state, [])
    PreparedAnnotatedFullMatches scopedRootFilter -> do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery state
      (matchedState, matches) <-
        flowPreparedAnnotatedContextDeltaMatches scopedRootFilter buckets contextKey Nothing graph preparedPlans plannedState
      pure (preparedAnnotatedMatchStateAfterRun graph contextRevision scopedRootFilter queryKey matchedState, matches)
    PreparedAnnotatedDeltaMatches scopedRootFilter dirtyResults -> do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery state
      (matchedState, matches) <-
        flowPreparedAnnotatedContextDeltaMatches scopedRootFilter buckets contextKey (Just dirtyResults) graph preparedPlans plannedState
      pure (preparedAnnotatedMatchStateAfterRun graph contextRevision scopedRootFilter queryKey matchedState, matches)
  where
    queryKey =
      compiledPatternQueryKey compiledQuery

    matchScope =
      preparedAnnotatedScope graph contextRevision rootClassFilter queryKey state
{-# INLINE wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter #-}

-- | Match one compiled query once over the authoritative regional structural
-- section. Dense prepared sources supply the join provenance; the region
-- algebra interprets it into the exact contexts where an assignment exists.
wcojPreparedRegionalDeltaMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability, Ord (f ())) =>
  RootClassFilter ->
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  Natural ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution, (ContextRegion owner))])
wcojPreparedRegionalDeltaMatchCompiledWithRootFilter rootClassFilter regionTable buckets contextRevision compiledQuery graph state =
  case preparedAnnotatedScope graph contextRevision rootClassFilter queryKey state of
    PreparedAnnotatedNoMatches ->
      Right (preparedAnnotatedMatchStateAfterRun graph contextRevision rootClassFilter queryKey state, [])
    PreparedAnnotatedFullMatches scopedRootFilter ->
      runRegional scopedRootFilter
    PreparedAnnotatedDeltaMatches scopedRootFilter dirtyResults ->
      runRegionalDelta scopedRootFilter dirtyResults
  where
    queryKey =
      compiledPatternQueryKey compiledQuery

    runRegional scopedRootFilter = do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery state
      (matchedState, matches) <-
        flowPreparedRegionalMatches
          scopedRootFilter
          regionTable
          buckets
          graph
          preparedPlans
          plannedState
      pure
        ( preparedAnnotatedMatchStateAfterRun
            graph
            contextRevision
            scopedRootFilter
            queryKey
            matchedState,
          matches
        )

    runRegionalDelta scopedRootFilter dirtyResults = do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery state
      (matchedState, matches) <-
        flowPreparedRegionalDeltaMatches
          scopedRootFilter
          dirtyResults
          regionTable
          buckets
          graph
          preparedPlans
          plannedState
      pure
        ( preparedAnnotatedMatchStateAfterRun
            graph
            contextRevision
            scopedRootFilter
            queryKey
            matchedState,
          matches
        )
{-# INLINE wcojPreparedRegionalDeltaMatchCompiledWithRootFilter #-}

wcojPreparedRegionalDeltaMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability, Ord (f ())) =>
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  Natural ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution, (ContextRegion owner))])
wcojPreparedRegionalDeltaMatchCompiledWithRoots =
  wcojPreparedRegionalDeltaMatchCompiledWithRootFilter AllRootClasses
{-# INLINE wcojPreparedRegionalDeltaMatchCompiledWithRoots #-}

wcojPreparedDeltaMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedDeltaMatchCompiledWithRoots =
  wcojPreparedDeltaMatchCompiledWithRootFilter AllRootClasses

wcojPreparedSharedBaseDeltaMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  PreparedBaseMatchMemo f ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (PreparedBaseMatchMemo f, EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedSharedBaseDeltaMatchCompiledWithRoots =
  wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter AllRootClasses

wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability, Ord (f ())) =>
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  Natural ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots =
  wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter AllRootClasses
{-# INLINE wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots #-}

wcojPreparedMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedMatchCompiledWithRoots =
  wcojPreparedMatchCompiledWithRootFilter AllRootClasses

preparedScope ::
  Language f =>
  EGraph f a ->
  RootClassFilter ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  PreparedMatchScope
preparedScope graph rootClassFilter queryKey state =
  case Map.lookup queryKey (epmsPendingDeltas state) of
    Just pendingDelta ->
      PreparedDeltaMatches
        rootClassFilter
        (pmdDirtyKeys pendingDelta)
    Nothing
      | preparedSnapshotCurrent graph queryKey state ->
          PreparedNoMatches
      | otherwise ->
          PreparedFullMatches rootClassFilter
{-# INLINE preparedScope #-}

preparedSnapshotCurrent ::
  Language f =>
  EGraph f a ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  Bool
preparedSnapshotCurrent graph queryKey state =
  Map.lookup queryKey (epmsSnapshotRevisions state) == Just (eGraphRevision graph)
{-# INLINE preparedSnapshotCurrent #-}

preparedAnnotatedScope ::
  Language f =>
  EGraph f a ->
  Natural ->
  RootClassFilter ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  PreparedAnnotatedMatchScope
preparedAnnotatedScope graph contextRevision rootClassFilter queryKey state =
  case Map.lookup queryKey (epmsPendingAnnotatedDeltas state) of
    Just pendingDelta ->
      PreparedAnnotatedDeltaMatches
        rootClassFilter
        (pmdDirtyKeys pendingDelta)
    Nothing
      | preparedAnnotatedSnapshotCurrent graph contextRevision rootClassFilter queryKey state ->
          PreparedAnnotatedNoMatches
      | otherwise ->
          PreparedAnnotatedFullMatches rootClassFilter
{-# INLINE preparedAnnotatedScope #-}

preparedAnnotatedSnapshotCurrent ::
  Language f =>
  EGraph f a ->
  Natural ->
  RootClassFilter ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  Bool
preparedAnnotatedSnapshotCurrent graph contextRevision rootClassFilter queryKey state =
  case Map.lookup queryKey (epmsAnnotatedSnapshotRevisions state) of
    Nothing ->
      False
    Just snapshot ->
      pasrBaseRevision snapshot == eGraphRevision graph
        && pasrContextRevision snapshot == contextRevision
        && sameRootClassFilter (pasrRootClassFilter snapshot) rootClassFilter
{-# INLINE preparedAnnotatedSnapshotCurrent #-}

sameRootClassFilter :: RootClassFilter -> RootClassFilter -> Bool
sameRootClassFilter leftFilter rightFilter =
  case (leftFilter, rightFilter) of
    (AllRootClasses, AllRootClasses) ->
      True
    (RestrictedRootClasses leftRootKeys, RestrictedRootClasses rightRootKeys) ->
      leftRootKeys == rightRootKeys
    _ ->
      False
{-# INLINE sameRootClassFilter #-}

compiledPatternQueryKey ::
  CompiledPatternQuery (CompiledGuard capability f) f ->
  PreparedQueryKey f
compiledPatternQueryKey compiledQuery =
  patternQueryPatterns (cpqQuery compiledQuery)
{-# INLINE compiledPatternQueryKey #-}

preparedPlanKeyForCompiledQuery ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  Either EGraphRelationalMatchObstruction (PreparedPlanKey f)
preparedPlanKeyForCompiledQuery compiledQuery =
  fmap
    ( \memoFingerprint ->
        PreparedPlanKey
          { ppkQueryKey = compiledPatternQueryKey compiledQuery,
            ppkMemoFingerprint = memoFingerprint
          }
    )
    ( first EGraphRelationalAtomizeObstruction
        (compiledPatternQueryFingerprint compiledQuery)
    )
{-# INLINE preparedPlanKeyForCompiledQuery #-}

compilePreparedPatternPlans ::
  (Language f, Show (f ()), Show capability) =>
  PreparedPlanKey f ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  Either EGraphRelationalMatchObstruction (PreparedPatternPlans capability f)
compilePreparedPatternPlans planKey compiledQuery = do
  queryPlan <-
    first EGraphRelationalAtomizeObstruction $
      atomizeCompiledPatternQuery compiledQuery
  compiledStoragePlan <-
    first
      (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanStoragePlanObstruction)
      (StoragePlan.compileStoragePlan (StoragePlan.storagePlanFromQueryPlan queryPlan))
  pure
    PreparedPatternPlans
      { pppPlanKey = planKey,
        pppQueryPlan = queryPlan,
        pppDecomp = RelRuntime.evalPlanPreparedDecomp queryPlan,
        pppBindingPlan = compilePatternsFreeJoinPlan (ppkQueryKey planKey),
        pppDirectShape = classifyCompiledPatternQuery compiledQuery,
        pppCompiledStoragePlan = compiledStoragePlan
      }
{-# INLINE compilePreparedPatternPlans #-}

preparedPatternPlans ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (PreparedPatternPlans capability f, EGraphPreparedMatchState owner capability f)
preparedPatternPlans compiledQuery state = do
  planKey <-
    preparedPlanKeyForCompiledQuery compiledQuery
  case Map.lookup planKey (epmsPlanCache state) of
    Just cachedPlan ->
      Right (cachedPlan, state)
    Nothing -> do
      preparedPlans <-
        compilePreparedPatternPlans planKey compiledQuery
      Right
        ( preparedPlans,
          state {epmsPlanCache = Map.insert planKey preparedPlans (epmsPlanCache state)}
        )
{-# INLINE preparedPatternPlans #-}

wcojMatchCompiledWithRoots ::
  (Language f, Show (f ()), Show capability) =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
wcojMatchCompiledWithRoots =
  wcojMatchCompiledWithRootFilter AllRootClasses

wcojMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability) =>
  RootClassFilter ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
wcojMatchCompiledWithRootFilter rootClassFilter compiledQuery graph = do
  planKey <-
    preparedPlanKeyForCompiledQuery compiledQuery
  preparedPlans <-
    compilePreparedPatternPlans planKey compiledQuery
  flowMatchPreparedPatternPlans
    rootClassFilter
    graph
    preparedPlans

preparedMatchStateAfterRun ::
  Language f =>
  EGraph f a ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
preparedMatchStateAfterRun graph queryKey state =
  state
    { epmsPendingDeltas = Map.delete queryKey (epmsPendingDeltas state),
      epmsSnapshotRevisions = Map.insert queryKey (eGraphRevision graph) (epmsSnapshotRevisions state)
    }
{-# INLINE preparedMatchStateAfterRun #-}

preparedAnnotatedMatchStateAfterRun ::
  Language f =>
  EGraph f a ->
  Natural ->
  RootClassFilter ->
  PreparedQueryKey f ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
preparedAnnotatedMatchStateAfterRun graph contextRevision rootClassFilter queryKey state =
  state
    { epmsPendingAnnotatedDeltas = Map.delete queryKey (epmsPendingAnnotatedDeltas state),
      epmsAnnotatedSnapshotRevisions =
        Map.insert
          queryKey
          PreparedAnnotatedSnapshotRevision
            { pasrBaseRevision = eGraphRevision graph,
              pasrContextRevision = contextRevision,
              pasrRootClassFilter = rootClassFilter
            }
          (epmsAnnotatedSnapshotRevisions state)
    }
{-# INLINE preparedAnnotatedMatchStateAfterRun #-}

-- | Cross-site memo for the base match component. Both organs are pure
-- functions of the graph at a revision: the shared tag-row arrangement, and
-- full match results keyed by query fingerprint (patterns AND residual guard)
-- plus root filter. Site states borrow the arrangement instead of each
-- rebuilding it; a full run recorded by one site serves every other site at
-- the same revision.
type PreparedBaseMatchMemo :: (Type -> Type) -> Type
data PreparedBaseMatchMemo f = PreparedBaseMatchMemo
  { pbmmSharedTagRows :: !(Maybe (PreparedSharedTagRows f)),
    pbmmFullResults :: !(Map.Map (PreparedPlanKey f, Maybe IntSet) (EGraphRevision, [(ClassId, Substitution)]))
  }

emptyPreparedBaseMatchMemo :: PreparedBaseMatchMemo f
emptyPreparedBaseMatchMemo =
  PreparedBaseMatchMemo
    { pbmmSharedTagRows = Nothing,
      pbmmFullResults = Map.empty
    }

preparedBaseMatchMemoResultCount :: PreparedBaseMatchMemo f -> Int
preparedBaseMatchMemoResultCount =
  Map.size . pbmmFullResults

adoptPreparedSharedTagRows ::
  EGraph f a ->
  PreparedBaseMatchMemo f ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
adoptPreparedSharedTagRows graph memo state =
  case pbmmSharedTagRows memo of
    Just donated
      | pstrRevision donated == eGraphRevision graph,
        maybe True ((/= eGraphRevision graph) . pstrRevision) (epmsSharedTagRows state) ->
          state
            { epmsSharedTagRows = Just donated,
              epmsAlphaCache = Nothing
            }
    _ ->
      state
{-# INLINE adoptPreparedSharedTagRows #-}

harvestPreparedSharedTagRows ::
  Language f =>
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  PreparedBaseMatchMemo f ->
  PreparedBaseMatchMemo f
harvestPreparedSharedTagRows graph state memo =
  case epmsSharedTagRows state of
    Just grown
      | pstrRevision grown == eGraphRevision graph ->
          case pbmmSharedTagRows memo of
            Just held
              | pstrRevision held == pstrRevision grown ->
                  if pstrTags grown `Set.isSubsetOf` pstrTags held
                    then memo
                    else
                      memo
                        { pbmmSharedTagRows =
                            Just
                              PreparedSharedTagRows
                                { pstrRevision = pstrRevision grown,
                                  pstrTags = Set.union (pstrTags grown) (pstrTags held),
                                  pstrRowsByTag = Map.union (pstrRowsByTag grown) (pstrRowsByTag held)
                                }
                        }
            _ ->
              memo {pbmmSharedTagRows = Just grown}
    _ ->
      memo
{-# INLINE harvestPreparedSharedTagRows #-}

-- | The delta-protocol entry point threaded through a cross-site memo. The
-- memo consults and records only while the graph is edit-quiescent, and a
-- memo hit still performs the site state's after-run bookkeeping so the
-- delta-consumption protocol observes an indistinguishable history.
wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter ::
  (Language f, Show (f ()), Show capability) =>
  RootClassFilter ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  PreparedBaseMatchMemo f ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (PreparedBaseMatchMemo f, EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter rootClassFilter compiledQuery graph memo state =
  case preparedScope graph rootClassFilter queryKey seededState of
    PreparedNoMatches ->
      Right (memo, preparedMatchStateAfterRun graph queryKey seededState, [])
    PreparedDeltaMatches scopedRootFilter dirtyResults -> do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery seededState
      (matchedState, matches) <-
        flowPreparedDeltaMatchPreparedPatternPlans scopedRootFilter dirtyResults graph preparedPlans plannedState
      let finishedState =
            preparedMatchStateAfterRun graph queryKey matchedState
      Right (harvestPreparedSharedTagRows graph finishedState memo, finishedState, matches)
    PreparedFullMatches scopedRootFilter -> do
      (preparedPlans, plannedState) <-
        preparedPatternPlans compiledQuery seededState
      let maybeMemoKey =
            if eGraphEditDeltaNull (eGraphPendingDelta graph)
              then Just (pppPlanKey preparedPlans)
              else Nothing
      case maybeMemoKey of
        Just fullKey
          | Just (heldRevision, heldMatches) <- Map.lookup (fullKey, filterKey) (pbmmFullResults memo),
            heldRevision == eGraphRevision graph ->
              Right (memo, preparedMatchStateAfterRun graph queryKey plannedState, heldMatches)
        _ -> do
          (matchedState, matches) <-
            flowPreparedMatchPreparedPatternPlans scopedRootFilter graph preparedPlans plannedState
          let finishedState =
                preparedMatchStateAfterRun graph queryKey matchedState
          let harvestedMemo =
                harvestPreparedSharedTagRows graph finishedState memo
              recordedMemo =
                case maybeMemoKey of
                  Just fullKey ->
                    harvestedMemo
                      { pbmmFullResults =
                          Map.insert
                            (fullKey, filterKey)
                            (eGraphRevision graph, matches)
                            (Map.filter ((== eGraphRevision graph) . fst) (pbmmFullResults harvestedMemo))
                      }
                  Nothing ->
                    harvestedMemo
          Right (recordedMemo, finishedState, matches)
  where
    queryKey =
      compiledPatternQueryKey compiledQuery

    seededState =
      adoptPreparedSharedTagRows graph memo state

    filterKey =
      case rootClassFilter of
        AllRootClasses ->
          Nothing
        RestrictedRootClasses rootKeys ->
          Just rootKeys

flowPreparedMatchPreparedPatternPlans ::
  Language f =>
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedMatchPreparedPatternPlans rootClassFilter graph preparedPlans state =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain (pppQueryPlan preparedPlans) of
        RelPlan.RootDomainQueryPlan ->
          Right (state, rootDomainMatches rootClassFilter graph (pppBindingPlan preparedPlans))
        RelPlan.StructuralQueryPlan ->
          case pppDirectShape preparedPlans of
            DirectRelationalJoin ->
              flowPreparedStructuralMatches
                rootClassFilter
                graph
                preparedPlans
                state
            directShape ->
              Right (state, directPatternMatches rootClassFilter graph directShape)
    else
      Left EGraphRelationalDirtySnapshot
{-# INLINE flowPreparedMatchPreparedPatternPlans #-}

flowPreparedDeltaMatchPreparedPatternPlans ::
  Language f =>
  RootClassFilter ->
  IntSet ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedDeltaMatchPreparedPatternPlans rootClassFilter dirtyResults graph preparedPlans state =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain (pppQueryPlan preparedPlans) of
        RelPlan.RootDomainQueryPlan ->
          Right (state, dirtyRootDomainMatches rootClassFilter graph (pppBindingPlan preparedPlans) dirtyResults)
        RelPlan.StructuralQueryPlan ->
          case pppDirectShape preparedPlans of
            DirectRelationalJoin ->
              flowPreparedStructuralDeltaMatches
                rootClassFilter
                dirtyResults
                graph
                preparedPlans
                state
            directShape ->
              Right (state, directPatternDeltaMatches rootClassFilter dirtyResults graph directShape)
    else
      Left EGraphRelationalDirtySnapshot
{-# INLINE flowPreparedDeltaMatchPreparedPatternPlans #-}

flowMatchPreparedPatternPlans ::
  Language f =>
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
flowMatchPreparedPatternPlans rootClassFilter graph preparedPlans =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain (pppQueryPlan preparedPlans) of
        RelPlan.RootDomainQueryPlan ->
          Right (rootDomainMatches rootClassFilter graph (pppBindingPlan preparedPlans))
        RelPlan.StructuralQueryPlan ->
          case pppDirectShape preparedPlans of
            DirectRelationalJoin ->
              flowStructuralMatches
                rootClassFilter
                graph
                preparedPlans
            directShape ->
              Right (directPatternMatches rootClassFilter graph directShape)
    else
      Left EGraphRelationalDirtySnapshot
{-# INLINE flowMatchPreparedPatternPlans #-}

flowStructuralMatches ::
  Language f =>
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
flowStructuralMatches rootClassFilter graph preparedPlans = do
  sections <-
    freshFlowQuerySections graph (pppQueryPlan preparedPlans)
  first EGraphRelationalRuntimeQueryObstruction $
    RelRuntime.evalPlanOutputsWithCompiledStoragePlanAndRootSelection
      (pppQueryPlan preparedPlans)
      (pppCompiledStoragePlan preparedPlans)
      (flowRootSelection rootClassFilter graph)
      sections
{-# INLINE flowStructuralMatches #-}

flowPreparedStructuralMatches ::
  Language f =>
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedStructuralMatches rootClassFilter graph preparedPlans state = do
  (sectionsState, sections) <-
    flowQuerySections graph (pppQueryPlan preparedPlans) state
  (sourcesState, sources) <-
    preparedSourcesForQuery graph preparedPlans sections sectionsState
  matches <-
    first EGraphRelationalRuntimeQueryObstruction $
      RelRuntime.evalPlanOutputsFromPreparedSources
        (pppQueryPlan preparedPlans)
        (pppDecomp preparedPlans)
        (flowRootSelection rootClassFilter graph)
        sources
  pure (sourcesState, matches)
{-# INLINE flowPreparedStructuralMatches #-}

preparedSourcesForQuery ::
  Language f =>
  EGraph f a ->
  PreparedPatternPlans capability f ->
  IntMap (RowBlock 'Canonical) ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [RelRuntime.DenseArrangement])
preparedSourcesForQuery graph preparedPlans sections state =
  case Map.lookup (preparedPatternQueryKey preparedPlans) (pacSources alphaCache) of
    Just sources ->
      Right (state {epmsAlphaCache = Just alphaCache}, sources)
    Nothing -> do
      sources <-
        first EGraphRelationalRuntimeQueryObstruction $
          RelRuntime.evalPlanPreparedSources (pppQueryPlan preparedPlans) sections
      let nextAlphaCache =
            alphaCache
              { pacSources = Map.insert (preparedPatternQueryKey preparedPlans) sources (pacSources alphaCache)
              }
      Right (state {epmsAlphaCache = Just nextAlphaCache}, sources)
  where
    alphaCache =
      preparedAlphaCacheForGraph graph state
{-# INLINE preparedSourcesForQuery #-}

flowPreparedStructuralDeltaMatches ::
  Language f =>
  RootClassFilter ->
  IntSet ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedStructuralDeltaMatches rootClassFilter dirtyResults graph preparedPlans state = do
  (dirtyRowsState, dirtyRowsByAtom) <-
    flowRowsForDirtyResults dirtyResults graph (pppQueryPlan preparedPlans) state
  dirtySections <-
    traverse
      (flowDirtySectionForAtom graph (pppQueryPlan preparedPlans))
      (IntMap.toAscList dirtyRowsByAtom)
  let selectedDirtySections =
        mapMaybe id dirtySections
  if null selectedDirtySections
    then pure (dirtyRowsState, [])
    else do
      (sectionsState, sections) <-
        flowQuerySections graph (pppQueryPlan preparedPlans) dirtyRowsState
      (sourcesState, baseSources) <-
        preparedSourcesForQuery graph preparedPlans sections sectionsState
      matches <-
        fmap
          (Set.toAscList . Set.fromList . foldMap id)
          ( traverse
              (flowStructuralMatchesWithSourceOverride rootClassFilter graph preparedPlans baseSources)
              selectedDirtySections
          )
      pure (sourcesState, matches)
{-# INLINE flowPreparedStructuralDeltaMatches #-}

flowPreparedAnnotatedContextDeltaMatches ::
  (Language f, Ord (f ())) =>
  RootClassFilter ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  Maybe IntSet ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedAnnotatedContextDeltaMatches rootClassFilter buckets contextKey externalDirtyFrontier graph preparedPlans state =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain (pppQueryPlan preparedPlans) of
        RelPlan.RootDomainQueryPlan ->
          pure (state, [])
        RelPlan.StructuralQueryPlan ->
          flowPreparedAnnotatedStructuralDeltaMatches rootClassFilter buckets contextKey externalDirtyFrontier graph preparedPlans state
    else
      Left EGraphRelationalDirtySnapshot
{-# INLINE flowPreparedAnnotatedContextDeltaMatches #-}

flowPreparedRegionalMatches ::
  (Language f, Ord (f ())) =>
  RootClassFilter ->
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution, (ContextRegion owner))])
flowPreparedRegionalMatches rootClassFilter regionTable buckets graph preparedPlans state =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain queryPlan of
        RelPlan.RootDomainQueryPlan ->
          Right
            ( state,
              [ (rootClass, substitutionValue, regionTop regionTable)
                | (rootClass, substitutionValue) <-
                    rootDomainMatches rootClassFilter graph (pppBindingPlan preparedPlans)
              ]
            )
        RelPlan.StructuralQueryPlan -> do
          let currentRows =
                regionalRowsByAtom regionTable buckets graph queryPlan
          sections <-
            regionalRowBlocks graph queryPlan currentRows
          sources <-
            first EGraphRelationalRuntimeQueryObstruction $
              RelRuntime.evalPlanPreparedSources queryPlan sections
          assignmentRows <-
            first EGraphRelationalRuntimeQueryObstruction $
              RelRuntime.evalPlanAssignmentsFromPreparedSources
                queryPlan
                (pppDecomp preparedPlans)
                RelRuntime.RuntimeAllRoots
                sources
          assignments <-
            projectRegionalAssignments queryPlan (Set.fromList assignmentRows)
          regionalSection <-
            preparedRegionalSection queryPlan currentRows assignments
          matches <-
            regionalMatchesForOutputs
              rootClassFilter
              graph
              regionTable
              queryPlan
              regionalSection
              (Set.fromList (Map.elems assignments))
          Right
            ( recordPreparedRegionalSection
                preparedPlans
                regionalSection
                state,
              matches
            )
    else
      Left EGraphRelationalDirtySnapshot
  where
    queryPlan =
      pppQueryPlan preparedPlans
{-# INLINE flowPreparedRegionalMatches #-}

flowPreparedRegionalDeltaMatches ::
  (Language f, Ord (f ())) =>
  RootClassFilter ->
  IntSet ->
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either
    EGraphRelationalMatchObstruction
    (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution, (ContextRegion owner))])
flowPreparedRegionalDeltaMatches rootClassFilter dirtyResults regionTable buckets graph preparedPlans state =
  if eGraphEditDeltaNull (eGraphPendingDelta graph)
    then
      case RelPlan.qpDomain queryPlan of
        RelPlan.RootDomainQueryPlan ->
          Right
            ( state,
              [ (rootClass, substitutionValue, regionTop regionTable)
                | (rootClass, substitutionValue) <-
                    dirtyRootDomainMatches
                      rootClassFilter
                      graph
                      (pppBindingPlan preparedPlans)
                      dirtyResults
              ]
            )
        RelPlan.StructuralQueryPlan ->
          case Map.lookup planKey (epmsRegionalSections state) of
            Nothing ->
              flowPreparedRegionalMatches
                rootClassFilter
                regionTable
                buckets
                graph
                preparedPlans
                state
            Just priorSection -> do
              let currentRows =
                    regionalRowsByAtom regionTable buckets graph queryPlan
                  priorRows =
                    prsRowsByAtom priorSection
                  changedRowKeys =
                    regionalChangedRowKeys priorRows currentRows
                  dirtyCurrentRows =
                    regionalCurrentRowsForKeys currentRows changedRowKeys
                  affectedAssignments =
                    regionalAssignmentsForRows priorSection changedRowKeys
                  retainedAssignments =
                    Map.withoutKeys
                      (prsAssignments priorSection)
                      affectedAssignments
              sections <-
                regionalRowBlocks graph queryPlan currentRows
              deltaAssignments <-
                regionalDeltaAssignments
                  queryPlan
                  (pppDecomp preparedPlans)
                  sections
                  dirtyCurrentRows
              projectedDeltaAssignments <-
                projectRegionalAssignments queryPlan deltaAssignments
              let nextAssignments =
                    Map.union projectedDeltaAssignments retainedAssignments
                  touchedOutputs =
                    Set.fromList
                      ( Map.elems projectedDeltaAssignments
                          <> mapMaybe
                            (`Map.lookup` prsAssignments priorSection)
                            (Set.toAscList affectedAssignments)
                      )
              nextSection <-
                preparedRegionalSection queryPlan currentRows nextAssignments
              matches <-
                regionalMatchesForOutputs
                  rootClassFilter
                  graph
                  regionTable
                  queryPlan
                  nextSection
                  touchedOutputs
              Right
                ( recordPreparedRegionalSection
                    preparedPlans
                    nextSection
                    state,
                  matches
                )
    else
      Left EGraphRelationalDirtySnapshot
  where
    queryPlan =
      pppQueryPlan preparedPlans

    planKey =
      pppPlanKey preparedPlans
{-# INLINE flowPreparedRegionalDeltaMatches #-}

regionalRowsByAtom ::
  (Language f, Ord (f ())) =>
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  QueryPlan capability f ->
  IntMap (Map.Map RowTupleKey (ContextRegion owner))
regionalRowsByAtom regionTable buckets graph queryPlan =
  IntMap.fromList
    [ ( atomKey,
        Map.fromListWith
          regionJoin
          [ (projectedRow, rowRegion)
            | (physicalRow, rowRegion) <-
                Map.toAscList
                  (Map.findWithDefault Map.empty (RelPlan.asTag atomSpec) rowsByTag),
              Just projectedRow <- [projectPhysicalAtomSpecRow atomSpec physicalRow]
          ]
      )
      | atomSpec <- Vector.toList (RelPlan.qpAtoms queryPlan),
        let atomKey = RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
    ]
  where
    rowsByTag =
      Map.fromList
        [ (tag, regionalPhysicalRowsForTag regionTable buckets graph tag)
          | tag <- Set.toAscList (queryPlanTags queryPlan)
        ]
{-# INLINE regionalRowsByAtom #-}

regionalPhysicalRowsForTag ::
  (Language f, Ord (f ())) =>
  (RegionTable owner) ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  f () ->
  Map.Map RowTupleKey (ContextRegion owner)
regionalPhysicalRowsForTag regionTable buckets graph tag =
  Map.fromListWith regionJoin (baseEntries <> variantEntries)
  where
    baseEntries =
      [ (regionalPhysicalRow graph rootKey childKeys, regionTop regionTable)
        | (rootKey, childKeys) <- structuralRowsForOperator (eGraphStore graph) (Operator tag)
      ]

    variantEntries =
      [ (regionalPhysicalRow graph (arRootKey row) (arChildKeys row), arRegion row)
        | row <- annotatedVariantRowsForTag tag buckets,
          not (regionEmpty (arRegion row))
      ]
{-# INLINE regionalPhysicalRowsForTag #-}

regionalPhysicalRow ::
  EGraph f a ->
  Int ->
  [Int] ->
  RowTupleKey
regionalPhysicalRow graph rootKey childKeys =
  tupleKeyFromRepKeys
    ( fmap
        (RepKey . classIdKey . canonicalizeClassId graph . ClassId)
        (rootKey : childKeys)
    )
{-# INLINE regionalPhysicalRow #-}

regionalRowBlocks ::
  EGraph f a ->
  QueryPlan capability f ->
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  Either EGraphRelationalMatchObstruction (IntMap (RowBlock 'Canonical))
regionalRowBlocks graph queryPlan rowsByAtom =
  fmap IntMap.fromList $
    traverse
      (\atomSpec ->
          let atomKey =
                RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
           in fmap ((,) atomKey) $
                first
                  (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
                  ( atomRowsFromTupleKeys
                      (preparedBaseRowIdentity (eGraphRevisionValue (eGraphRevision graph)) queryPlan atomSpec)
                      (RelPlan.asColumns atomSpec)
                      (Map.keys (IntMap.findWithDefault Map.empty atomKey rowsByAtom))
                  )
      )
      (Vector.toList (RelPlan.qpAtoms queryPlan))
{-# INLINE regionalRowBlocks #-}

regionalChangedRowKeys ::
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  IntMap (Set.Set RowTupleKey)
regionalChangedRowKeys priorRows currentRows =
  IntMap.mapMaybeWithKey changedAtAtom (IntMap.union priorRows currentRows)
  where
    changedAtAtom atomKey _ =
      let priorAtomRows =
            IntMap.findWithDefault Map.empty atomKey priorRows
          currentAtomRows =
            IntMap.findWithDefault Map.empty atomKey currentRows
          candidateRows =
            Set.union (Map.keysSet priorAtomRows) (Map.keysSet currentAtomRows)
          changedRows =
            Set.filter
              (\rowValue -> Map.lookup rowValue priorAtomRows /= Map.lookup rowValue currentAtomRows)
              candidateRows
       in if Set.null changedRows then Nothing else Just changedRows
{-# INLINE regionalChangedRowKeys #-}

regionalCurrentRowsForKeys ::
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  IntMap (Set.Set RowTupleKey) ->
  IntMap (Set.Set RowTupleKey)
regionalCurrentRowsForKeys currentRows =
  IntMap.mapMaybeWithKey
    (\atomKey changedRows ->
        let currentKeys =
              Map.keysSet (IntMap.findWithDefault Map.empty atomKey currentRows)
            dirtyRows =
              Set.intersection changedRows currentKeys
         in if Set.null dirtyRows then Nothing else Just dirtyRows
    )
{-# INLINE regionalCurrentRowsForKeys #-}

regionalAssignmentsForRows ::
  PreparedRegionalSection owner ->
  IntMap (Set.Set RowTupleKey) ->
  Set.Set RowTupleKey
regionalAssignmentsForRows section =
  IntMap.foldlWithKey'
    (\assignments atomKey changedRows ->
        let assignmentsByRow =
              IntMap.findWithDefault Map.empty atomKey (prsAssignmentsByAtomRow section)
         in Set.union
              assignments
              ( foldMap
                  (\rowValue -> Map.findWithDefault Set.empty rowValue assignmentsByRow)
                  changedRows
              )
    )
    Set.empty
{-# INLINE regionalAssignmentsForRows #-}

regionalDeltaAssignments ::
  QueryPlan capability f ->
  RelPlan.DecompPlan ->
  IntMap (RowBlock 'Canonical) ->
  IntMap (Set.Set RowTupleKey) ->
  Either EGraphRelationalMatchObstruction (Set.Set RowTupleKey)
regionalDeltaAssignments queryPlan decomp sections dirtyRowsByAtom
  | IntMap.null dirtyRowsByAtom =
      Right Set.empty
  | otherwise = do
      sources <-
        first EGraphRelationalRuntimeQueryObstruction $
          RelRuntime.evalPlanPreparedSources queryPlan sections
      assignments <-
        first EGraphRelationalRuntimeQueryObstruction $
          RelRuntime.evalPlanDeltaAssignmentsFromPreparedSources
            queryPlan
            decomp
            RelRuntime.RuntimeAllRoots
            (RelRuntime.evalPlanPreparedSourcesWithDirtyRows queryPlan dirtyRowsByAtom sources)
      pure (Set.fromList assignments)
{-# INLINE regionalDeltaAssignments #-}

projectRegionalAssignments ::
  QueryPlan capability f ->
  Set.Set RowTupleKey ->
  Either EGraphRelationalMatchObstruction (Map.Map RowTupleKey RegionalOutput)
projectRegionalAssignments queryPlan =
  Foldable.foldlM projectAssignment Map.empty
  where
    projectAssignment assignments assignment = do
      maybeOutput <-
        first
          (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanOutputProjectionObstruction)
          (RelPlan.projectQueryPlanOutput queryPlan assignment)
      pure
        ( maybe
            assignments
            (\output -> Map.insert assignment output assignments)
            maybeOutput
        )
{-# INLINE projectRegionalAssignments #-}

preparedRegionalSection ::
  QueryPlan capability f ->
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  Map.Map RowTupleKey RegionalOutput ->
  Either EGraphRelationalMatchObstruction (PreparedRegionalSection owner)
preparedRegionalSection queryPlan rowsByAtom assignments =
  Foldable.foldlM
    insertAssignment
    PreparedRegionalSection
      { prsRowsByAtom = rowsByAtom,
        prsAssignments = assignments,
        prsAssignmentsByOutput = Map.empty,
        prsAssignmentsByAtomRow = IntMap.empty
      }
    (Map.toAscList assignments)
  where
    insertAssignment section (assignment, output) = do
      atomRows <-
        regionalAssignmentRows queryPlan assignment
      pure
        section
          { prsAssignmentsByOutput =
              Map.insertWith
                Set.union
                output
                (Set.singleton assignment)
                (prsAssignmentsByOutput section),
            prsAssignmentsByAtomRow =
              IntMap.unionWith
                (Map.unionWith Set.union)
                ( IntMap.map
                    (\rowValue -> Map.singleton rowValue (Set.singleton assignment))
                    atomRows
                )
                (prsAssignmentsByAtomRow section)
          }
{-# INLINE preparedRegionalSection #-}

recordPreparedRegionalSection ::
  Language f =>
  PreparedPatternPlans capability f ->
  PreparedRegionalSection owner ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
recordPreparedRegionalSection preparedPlans section state =
  state
    { epmsRegionalSections =
        Map.insert
          (pppPlanKey preparedPlans)
          section
          (epmsRegionalSections state)
    }
{-# INLINE recordPreparedRegionalSection #-}

regionalAssignmentRows ::
  QueryPlan capability f ->
  RowTupleKey ->
  Either EGraphRelationalMatchObstruction (IntMap RowTupleKey)
regionalAssignmentRows queryPlan assignment = do
  environment <-
    regionalAssignmentEnvironment
      (Vector.toList (RelPlan.qpFullSchema queryPlan))
      assignment
  fmap IntMap.fromList $
    traverse
      (\atomSpec -> do
          rowValues <-
            traverse
              (\slotId ->
                  maybe
                    ( Left
                        ( EGraphRelationalRegionalAssignmentObstruction
                            (RegionalAssignmentSlotMissing slotId)
                        )
                    )
                    Right
                    (IntMap.lookup (RelPlan.slotIdKey slotId) environment)
              )
              (Vector.toList (RelPlan.asColumns atomSpec))
          pure
            ( RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec),
              tupleKeyFromRepKeys rowValues
            )
      )
      (Vector.toList (RelPlan.qpAtoms queryPlan))
{-# INLINE regionalAssignmentRows #-}

regionalAssignmentEnvironment ::
  [RelPlan.SlotId] ->
  RowTupleKey ->
  Either EGraphRelationalMatchObstruction (IntMap RepKey)
regionalAssignmentEnvironment schema assignment =
  let values =
        tupleKeyToRepKeys assignment
   in if length schema == length values
        then
          Right
            ( IntMap.fromList
                [ (RelPlan.slotIdKey slotId, value)
                  | (slotId, value) <- zip schema values
                ]
            )
        else
          Left
            ( EGraphRelationalRegionalAssignmentObstruction
                (RegionalAssignmentArityMismatch (length schema) (length values))
            )
{-# INLINE regionalAssignmentEnvironment #-}

regionalMatchesForOutputs ::
  RootClassFilter ->
  EGraph f a ->
  (RegionTable owner) ->
  QueryPlan capability f ->
  PreparedRegionalSection owner ->
  Set.Set RegionalOutput ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution, (ContextRegion owner))]
regionalMatchesForOutputs rootClassFilter graph regionTable queryPlan section =
  fmap (mapMaybe id) . traverse matchForOutput . Set.toAscList
  where
    matchForOutput output@(rootClass, substitutionValue)
      | not (rootClassAllowed rootClassFilter graph rootClass) =
          Right Nothing
      | otherwise = do
          regionValue <-
            fmap (Foldable.foldl' regionJoin regionVoid) $
              traverse
                (regionalAssignmentRegion regionTable queryPlan (prsRowsByAtom section))
                (Set.toAscList (Map.findWithDefault Set.empty output (prsAssignmentsByOutput section)))
          pure
            ( if regionEmpty regionValue
                then Nothing
                else Just (rootClass, substitutionValue, regionValue)
            )
{-# INLINE regionalMatchesForOutputs #-}

regionalAssignmentRegion ::
  (RegionTable owner) ->
  QueryPlan capability f ->
  IntMap (Map.Map RowTupleKey (ContextRegion owner)) ->
  RowTupleKey ->
  Either EGraphRelationalMatchObstruction (ContextRegion owner)
regionalAssignmentRegion regionTable queryPlan rowsByAtom assignment = do
  atomRows <-
    regionalAssignmentRows queryPlan assignment
  pure
    ( IntMap.foldlWithKey'
        (\currentRegion atomKey rowValue ->
            regionMeet
              currentRegion
              (Map.findWithDefault regionVoid rowValue (IntMap.findWithDefault Map.empty atomKey rowsByAtom))
        )
        (regionTop regionTable)
        atomRows
    )
{-# INLINE regionalAssignmentRegion #-}

flowPreparedAnnotatedStructuralDeltaMatches ::
  (Language f, Ord (f ())) =>
  RootClassFilter ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  Maybe IntSet ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, [(ClassId, Substitution)])
flowPreparedAnnotatedStructuralDeltaMatches rootClassFilter buckets contextKey externalDirtyFrontier graph preparedPlans state =
  do
    dirtySections <-
      annotatedDirtySections graph buckets contextKey dirtyFrontier queryPlan
    let selectedDirtySections =
          mapMaybe id dirtySections
        dirtyAtomKeys =
          IntSet.fromList (fmap fst selectedDirtySections)
    if null selectedDirtySections
      then pure (state, [])
      else do
        (sectionsState, baseSections) <-
          flowQuerySectionsExcept
            graph
            queryPlan
            (fullSectionAtomKeys queryPlan dirtyAtomKeys)
            state
        sections <-
          annotatedRuntimeSections graph buckets contextKey queryPlan baseSections
        matches <-
          fmap
            (Set.toAscList . Set.fromList . foldMap id)
            ( traverse
                (flowStructuralMatchesWithRuntimeSections rootClassFilter graph preparedPlans sections)
                selectedDirtySections
            )
        pure (sectionsState, matches)
  where
    queryPlan =
      pppQueryPlan preparedPlans

    dirtyFrontier =
      maybe
        id
        IntSet.intersection
        externalDirtyFrontier
        (annotatedDirtyFrontier graph buckets contextKey queryPlan)
{-# INLINE flowPreparedAnnotatedStructuralDeltaMatches #-}

annotatedDirtyFrontier ::
  Ord (f ()) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  QueryPlan capability f ->
  IntSet
annotatedDirtyFrontier graph buckets contextKey queryPlan =
  IntSet.fromList
    [ keyValue
      | tag <- Set.toAscList (queryPlanTags queryPlan),
        (rootKey, _) <- absorbedRowsAtKey tag contextKey buckets <> annotatedRowsAtKey tag contextKey buckets,
        keyValue <- [rootKey, baseCanonicalKey rootKey, annotatedCanonicalKey rootKey]
    ]
  where
    baseCanonicalKey =
      classIdKey . canonicalizeClassId graph . ClassId

    annotatedCanonicalKey =
      annotatedRepresentativeKeyAt contextKey buckets . baseCanonicalKey
{-# INLINE annotatedDirtyFrontier #-}

annotatedDirtySections ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  IntSet ->
  QueryPlan capability f ->
  Either EGraphRelationalMatchObstruction [Maybe (Int, RowBlock 'Canonical)]
annotatedDirtySections graph buckets contextKey dirtyFrontier queryPlan =
  traverse
    dirtySectionForAtom
    (Vector.toList (RelPlan.qpAtoms queryPlan))
  where
    dirtyRowsByAtom =
      annotatedDirtyRowsByAtom graph buckets contextKey dirtyFrontier queryPlan

    dirtySectionForAtom atomSpec =
      let atomKey =
            RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
          rowsByResult =
            IntMap.findWithDefault IntMap.empty atomKey dirtyRowsByAtom
       in if IntMap.null rowsByResult
            then Right Nothing
            else (\rows -> Just (atomKey, rows)) <$> annotatedRowBlockForAtom graph queryPlan atomSpec rowsByResult
{-# INLINE annotatedDirtySections #-}

annotatedRuntimeSections ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  QueryPlan capability f ->
  IntMap (RowBlock 'Canonical) ->
  Either EGraphRelationalMatchObstruction (IntMap RelRuntime.RuntimeSection)
annotatedRuntimeSections graph buckets contextKey queryPlan baseSections =
  IntMap.traverseWithKey runtimeSectionForAtom baseSections
  where
    absorbedRowsByAtom =
      annotatedAbsorbedRowsByAtom graph buckets contextKey queryPlan

    variantRowsByAtom =
      annotatedVariantRowsByAtom graph buckets contextKey queryPlan

    runtimeSectionForAtom atomKey baseRows =
      case queryPlanAtomSpec atomKey queryPlan of
        Nothing ->
          Right (RelRuntime.wholeRuntimeSection baseRows)
        Just atomSpec ->
          annotatedRuntimeSectionForAtom
            graph
            queryPlan
            atomSpec
            baseRows
            (IntMap.findWithDefault IntMap.empty atomKey absorbedRowsByAtom)
            (IntMap.findWithDefault IntMap.empty atomKey variantRowsByAtom)
{-# INLINE annotatedRuntimeSections #-}

annotatedRuntimeSectionForAtom ::
  EGraph f a ->
  QueryPlan capability f ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  RowBlock 'Canonical ->
  IntMap [RowTupleKey] ->
  IntMap [RowTupleKey] ->
  Either EGraphRelationalMatchObstruction RelRuntime.RuntimeSection
annotatedRuntimeSectionForAtom graph queryPlan atomSpec baseRows absorbedRows variantRows
  | IntMap.null absorbedRows && IntMap.null variantRows =
      Right (RelRuntime.wholeRuntimeSection baseRows)
  | otherwise = do
      maskRows <-
        if IntMap.null absorbedRows
          then Right (Row.emptyRowBlock (Row.rowBlockIdentity baseRows) (Row.rowBlockLayout baseRows))
          else annotatedRowBlockForAtom graph queryPlan atomSpec absorbedRows
      extraRows <-
        if IntMap.null variantRows
          then Right Nothing
          else Just <$> annotatedRowBlockForAtom graph queryPlan atomSpec variantRows
      Right (RelRuntime.composedRuntimeSection baseRows maskRows extraRows)
{-# INLINE annotatedRuntimeSectionForAtom #-}

annotatedRowBlockForAtom ::
  EGraph f a ->
  QueryPlan capability f ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  IntMap [RowTupleKey] ->
  Either EGraphRelationalMatchObstruction (RowBlock 'Canonical)
annotatedRowBlockForAtom graph queryPlan atomSpec rowsByResult =
  first
    (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
    ( atomRowsFromTupleKeys
        (preparedBaseRowIdentity (eGraphRevisionValue (eGraphRevision graph)) queryPlan atomSpec)
        (RelPlan.asColumns atomSpec)
        (flattenRowsByResult rowsByResult)
    )
{-# INLINE annotatedRowBlockForAtom #-}

annotatedDirtyRowsByAtom ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  IntSet ->
  QueryPlan capability f ->
  IntMap (IntMap [RowTupleKey])
annotatedDirtyRowsByAtom graph buckets contextKey dirtyFrontier queryPlan =
  egraphRowsByAtomFromPhysicalRows
    queryPlan
    (annotatedCanonicalizeClass graph buckets contextKey)
    (annotatedDirtyRowsForOperator graph buckets contextKey dirtyFrontier)
{-# INLINE annotatedDirtyRowsByAtom #-}

annotatedVariantRowsByAtom ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  QueryPlan capability f ->
  IntMap (IntMap [RowTupleKey])
annotatedVariantRowsByAtom graph buckets contextKey queryPlan =
  egraphRowsByAtomFromPhysicalRows
    queryPlan
    (annotatedCanonicalizeClass graph buckets contextKey)
    (\(Operator tag) -> annotatedRowsAtKey tag contextKey buckets)
{-# INLINE annotatedVariantRowsByAtom #-}

annotatedAbsorbedRowsByAtom ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  QueryPlan capability f ->
  IntMap (IntMap [RowTupleKey])
annotatedAbsorbedRowsByAtom graph buckets contextKey queryPlan =
  egraphRowsByAtomFromPhysicalRows
    queryPlan
    (canonicalizeClassId graph)
    (\(Operator tag) -> absorbedRowsAtKey tag contextKey buckets)
{-# INLINE annotatedAbsorbedRowsByAtom #-}

annotatedDirtyRowsForOperator ::
  (Language f, Ord (f ())) =>
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  IntSet ->
  Operator f ->
  [(Int, [Int])]
annotatedDirtyRowsForOperator graph buckets contextKey dirtyFrontier (Operator tag) =
  Set.toAscList $
    Set.union
      (Set.difference baseRows absorbedRows)
      variantRows
  where
    baseRows =
      Set.fromList (structuralRowsForResultKeys dirtyFrontier (eGraphStore graph) (Operator tag))

    absorbedRows =
      Set.fromList (absorbedRowsAtKey tag contextKey buckets)

    variantRows =
      Set.fromList (annotatedRowsAtKey tag contextKey buckets)
{-# INLINE annotatedDirtyRowsForOperator #-}

annotatedCanonicalizeClass ::
  EGraph f a ->
  AnnotatedDeltaBuckets owner f ->
  (ContextObjectKey owner) ->
  ClassId ->
  ClassId
annotatedCanonicalizeClass graph buckets contextKey classId =
  ClassId
    ( annotatedRepresentativeKeyAt
        contextKey
        buckets
        (classIdKey (canonicalizeClassId graph classId))
    )
{-# INLINE annotatedCanonicalizeClass #-}

preparedQuerySections ::
  Language f =>
  Int ->
  EGraph f a ->
  QueryPlan capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either Row.RowBuildError (EGraphPreparedMatchState owner capability f, IntMap (RowBlock 'Canonical))
preparedQuerySections baseRevision graph queryPlan state =
  fmap
    (\sectionBuild -> (psbState sectionBuild, psbSections sectionBuild))
    ( Foldable.foldlM
        (insertPreparedAtomSection baseRevision graph queryPlan)
        PreparedSectionBuild
          { psbState = ensurePreparedSharedTagRows graph state,
            psbSections = IntMap.empty
          }
        (Vector.toList (RelPlan.qpAtoms queryPlan))
    )
{-# INLINE preparedQuerySections #-}

preparedQuerySectionsForAtomKeys ::
  Language f =>
  IntSet ->
  Int ->
  EGraph f a ->
  QueryPlan capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either Row.RowBuildError (EGraphPreparedMatchState owner capability f, IntMap (RowBlock 'Canonical))
preparedQuerySectionsForAtomKeys atomKeys baseRevision graph queryPlan state =
  fmap
    (\sectionBuild -> (psbState sectionBuild, psbSections sectionBuild))
    ( Foldable.foldlM
        (insertPreparedAtomSection baseRevision graph queryPlan)
        PreparedSectionBuild
          { psbState = ensurePreparedSharedTagRows graph state,
            psbSections = IntMap.empty
          }
        (filter (atomKeySelected atomKeys) (Vector.toList (RelPlan.qpAtoms queryPlan)))
    )
{-# INLINE preparedQuerySectionsForAtomKeys #-}

atomKeySelected :: IntSet -> RelPlan.AtomSpec tag tuple key -> Bool
atomKeySelected atomKeys atomSpec =
  IntSet.member (RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)) atomKeys
{-# INLINE atomKeySelected #-}

insertPreparedAtomSection ::
  Language f =>
  Int ->
  EGraph f a ->
  QueryPlan capability f ->
  PreparedSectionBuild owner capability f (RowBlock 'Canonical) ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  Either Row.RowBuildError (PreparedSectionBuild owner capability f (RowBlock 'Canonical))
insertPreparedAtomSection baseRevision graph queryPlan sectionBuild atomSpec = do
  (rowsState, rows) <-
    preparedAlphaBlockForAtom baseRevision graph queryPlan atomSpec (psbState sectionBuild)
  pure
    PreparedSectionBuild
      { psbState = rowsState,
        psbSections =
          IntMap.insert
            (RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec))
            rows
            (psbSections sectionBuild)
      }
{-# INLINE insertPreparedAtomSection #-}

preparedAlphaBlockForAtom ::
  Language f =>
  Int ->
  EGraph f a ->
  QueryPlan capability f ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  EGraphPreparedMatchState owner capability f ->
  Either Row.RowBuildError (EGraphPreparedMatchState owner capability f, RowBlock 'Canonical)
preparedAlphaBlockForAtom baseRevision graph queryPlan atomSpec state = do
  let (rowsState, rowsByResult) =
        preparedAlphaRowsForAtom graph atomSpec state
      alphaCache =
        preparedAlphaCacheForGraph graph rowsState
      blockKey =
        preparedAlphaBlockKey atomSpec
      planIdentity =
        preparedBaseRowIdentity baseRevision queryPlan atomSpec
  case Map.lookup blockKey (pacBlocksByKey alphaCache) of
    Just cachedRows ->
      Right (rowsState {epmsAlphaCache = Just alphaCache}, Row.reidentifyRows planIdentity cachedRows)
    Nothing -> do
      rows <-
        atomRowsFromTupleKeys
          planIdentity
          (RelPlan.asColumns atomSpec)
          (flattenRowsByResult rowsByResult)
      let nextAlphaCache =
            alphaCache
              { pacBlocksByKey = Map.insert blockKey rows (pacBlocksByKey alphaCache)
              }
      pure (rowsState {epmsAlphaCache = Just nextAlphaCache}, rows)
{-# INLINE preparedAlphaBlockForAtom #-}

preparedAlphaRowsForAtom ::
  Language f =>
  EGraph f a ->
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  EGraphPreparedMatchState owner capability f ->
  (EGraphPreparedMatchState owner capability f, IntMap [RowTupleKey])
preparedAlphaRowsForAtom graph atomSpec state =
  case Map.lookup projectionKey (pacRowsByKey alphaCache) of
    Just rowsByResult ->
      (stateWithShared {epmsAlphaCache = Just alphaCache}, rowsByResult)
    Nothing ->
      let rowsByResult =
            rowsByResultForAtomSpec
              atomSpec
              (Map.findWithDefault IntMap.empty (RelPlan.asTag atomSpec) (preparedSharedRowsByTag graph stateWithShared))
          nextAlphaCache =
            alphaCache
              { pacRowsByKey = Map.insert projectionKey rowsByResult (pacRowsByKey alphaCache)
              }
       in (stateWithShared {epmsAlphaCache = Just nextAlphaCache}, rowsByResult)
  where
    stateWithShared =
      ensurePreparedSharedTagRows graph state

    alphaCache =
      preparedAlphaCacheForGraph graph stateWithShared

    projectionKey =
      preparedAlphaProjectionKey atomSpec
{-# INLINE preparedAlphaRowsForAtom #-}

preparedAlphaProjectionKey ::
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  PreparedAlphaProjectionKey f
preparedAlphaProjectionKey atomSpec =
  PreparedAlphaProjectionKey (RelPlan.asTag atomSpec) (RelPlan.asStalkRecipe atomSpec)
{-# INLINE preparedAlphaProjectionKey #-}

preparedAlphaBlockKey ::
  RelPlan.AtomSpec (f ()) (ENode f) ClassId ->
  PreparedAlphaBlockKey f
preparedAlphaBlockKey atomSpec =
  PreparedAlphaBlockKey (preparedAlphaProjectionKey atomSpec) (RelPlan.asColumns atomSpec)
{-# INLINE preparedAlphaBlockKey #-}

preparedSharedRowsByTag ::
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  Map.Map (f ()) (IntMap [RowTupleKey])
preparedSharedRowsByTag graph state =
  case epmsSharedTagRows state of
    Just sharedRows
      | pstrRevision sharedRows == eGraphRevision graph ->
          pstrRowsByTag sharedRows
    _ ->
      Map.empty
{-# INLINE preparedSharedRowsByTag #-}

ensurePreparedSharedTagRows ::
  Language f =>
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
ensurePreparedSharedTagRows graph state =
  case epmsSharedTagRows state of
    Just sharedRows
      | pstrRevision sharedRows == revision && wantedTags `Set.isSubsetOf` pstrTags sharedRows ->
          state
      | not (IntSet.null sharedDirtyKeys) ->
          patchPreparedSharedTagRows graph wantedTags sharedDirtyKeys sharedRows state
    _ ->
      buildPreparedSharedTagRows graph wantedTags state
  where
    revision =
      eGraphRevision graph

    wantedTags =
      preparedCachedPlanTags state

    sharedDirtyKeys =
      foldMap pmdDirtyKeys (Map.elems (epmsPendingDeltas state))
{-# INLINE ensurePreparedSharedTagRows #-}

buildPreparedSharedTagRows ::
  Language f =>
  EGraph f a ->
  Set.Set (f ()) ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
buildPreparedSharedTagRows graph wantedTags state =
  state
    { epmsSharedTagRows =
        Just
          PreparedSharedTagRows
            { pstrRevision = eGraphRevision graph,
              pstrTags = wantedTags,
              pstrRowsByTag =
                physicalRowsByTagFromTagRows
                  (canonicalizeClassId graph)
                  (structuralRowsByTag wantedTags (eGraphStore graph))
            },
      epmsAlphaCache = Nothing
    }
{-# INLINE buildPreparedSharedTagRows #-}

patchPreparedSharedTagRows ::
  Language f =>
  EGraph f a ->
  Set.Set (f ()) ->
  IntSet ->
  PreparedSharedTagRows f ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
patchPreparedSharedTagRows graph wantedTags dirtyKeys sharedRows state =
  state
    { epmsSharedTagRows =
        Just
          PreparedSharedTagRows
            { pstrRevision = eGraphRevision graph,
              pstrTags = wantedTags,
              pstrRowsByTag = patchedRowsByTag
            },
      epmsAlphaCache =
        patchPreparedAlphaCache (eGraphRevision graph) dirtyAlphaTags effectiveDirtyResults <$> epmsAlphaCache state
    }
  where
    canonicalize =
      canonicalizeClassId graph

    dirtyAndCanonicalKeys =
      dirtyKeys <> canonicalRootKeys graph dirtyKeys

    dirtyAlphaTags =
      Set.union missingWantedTags (Map.keysSet dirtyRowsByTag)

    existingWantedTags =
      Set.intersection wantedTags (pstrTags sharedRows)

    missingWantedTags =
      Set.difference wantedTags (pstrTags sharedRows)

    dirtyRowsByTag =
      physicalRowsByTagFromTagRows
        canonicalize
        (structuralRowsByTagForCanonicalResultKeys canonicalize dirtyAndCanonicalKeys existingWantedTags (eGraphStore graph))

    missingRowsByTag =
      physicalRowsByTagFromTagRows
        canonicalize
        (structuralRowsByTag missingWantedTags (eGraphStore graph))

    effectiveDirtyResults =
      rowProjectionDirtyResultKeys dirtyAndCanonicalKeys dirtyRowsByTag

    patchedRowsByTag =
      Map.union
        missingRowsByTag
        ( Map.mapWithKey
            ( \tag oldRows ->
                cursorRowsByResult
                  (Map.findWithDefault IntMap.empty tag dirtyRowsByTag)
                  oldRows
                  effectiveDirtyResults
            )
            (Map.restrictKeys (pstrRowsByTag sharedRows) existingWantedTags)
        )
{-# INLINE patchPreparedSharedTagRows #-}

patchPreparedAlphaCache ::
  Ord (f ()) =>
  EGraphRevision ->
  Set.Set (f ()) ->
  IntSet ->
  PreparedAlphaCache f ->
  PreparedAlphaCache f
patchPreparedAlphaCache revision dirtyTags dirtyResults alphaCache =
  PreparedAlphaCache
    { pacRevision = revision,
      pacRowsByKey =
        Map.filterWithKey
          (\key rows -> projectionKeyClean dirtyTags key && rowsByResultClean dirtyResults rows)
          (pacRowsByKey alphaCache),
      pacBlocksByKey =
        Map.filterWithKey
          (\key rows -> blockKeyClean dirtyTags key && rowBlockClean dirtyResults rows)
          (pacBlocksByKey alphaCache),
      pacSources = Map.empty
    }
{-# INLINE patchPreparedAlphaCache #-}

projectionKeyClean :: Ord (f ()) => Set.Set (f ()) -> PreparedAlphaProjectionKey f -> Bool
projectionKeyClean dirtyTags (PreparedAlphaProjectionKey tag _) =
  not (Set.member tag dirtyTags)
{-# INLINE projectionKeyClean #-}

blockKeyClean :: Ord (f ()) => Set.Set (f ()) -> PreparedAlphaBlockKey f -> Bool
blockKeyClean dirtyTags (PreparedAlphaBlockKey projectionKey _) =
  projectionKeyClean dirtyTags projectionKey
{-# INLINE blockKeyClean #-}

rowsByResultClean :: IntSet -> IntMap [RowTupleKey] -> Bool
rowsByResultClean dirtyResults =
  IntSet.null . IntSet.intersection dirtyResults . IntMap.keysSet
{-# INLINE rowsByResultClean #-}

rowBlockClean :: IntSet -> RowBlock 'Canonical -> Bool
rowBlockClean dirtyResults =
  rowsByResultClean dirtyResults . rowsByResultFromRowBlock
{-# INLINE rowBlockClean #-}

preparedCachedPlanTags ::
  Language f =>
  EGraphPreparedMatchState owner capability f ->
  Set.Set (f ())
preparedCachedPlanTags =
  foldMap (queryPlanTags . pppQueryPlan) . Map.elems . epmsPlanCache
{-# INLINE preparedCachedPlanTags #-}

queryPlanTags ::
  Ord tag =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Set.Set tag
queryPlanTags =
  Set.fromList . fmap RelPlan.asTag . Vector.toList . RelPlan.qpAtoms
{-# INLINE queryPlanTags #-}

preparedAlphaCacheForGraph ::
  EGraph f a ->
  EGraphPreparedMatchState owner capability f ->
  PreparedAlphaCache f
preparedAlphaCacheForGraph graph state =
  case epmsAlphaCache state of
    Just alphaCache
      | pacRevision alphaCache == eGraphRevision graph ->
          alphaCache
    _ ->
      emptyPreparedAlphaCache (eGraphRevision graph)
{-# INLINE preparedAlphaCacheForGraph #-}

emptyPreparedAlphaCache :: EGraphRevision -> PreparedAlphaCache f
emptyPreparedAlphaCache revision =
  PreparedAlphaCache
    { pacRevision = revision,
      pacRowsByKey = Map.empty,
      pacBlocksByKey = Map.empty,
      pacSources = Map.empty
    }
{-# INLINE emptyPreparedAlphaCache #-}

flowStructuralMatchesWithSourceOverride ::
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  [RelRuntime.DenseArrangement] ->
  (Int, RowBlock 'Canonical) ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
flowStructuralMatchesWithSourceOverride rootClassFilter graph preparedPlans baseSources (atomKey, dirtyRows) =
  first EGraphRelationalRuntimeQueryObstruction $
    RelRuntime.evalPlanOutputsFromPreparedSources
      (pppQueryPlan preparedPlans)
      (pppDecomp preparedPlans)
      (flowRootSelection rootClassFilter graph)
      ( RelRuntime.evalPlanPreparedSourceOverride
          (pppQueryPlan preparedPlans)
          atomKey
          dirtyRows
          baseSources
      )
{-# INLINE flowStructuralMatchesWithSourceOverride #-}

flowStructuralMatchesWithRuntimeSections ::
  RootClassFilter ->
  EGraph f a ->
  PreparedPatternPlans capability f ->
  IntMap RelRuntime.RuntimeSection ->
  (Int, RowBlock 'Canonical) ->
  Either EGraphRelationalMatchObstruction [(ClassId, Substitution)]
flowStructuralMatchesWithRuntimeSections rootClassFilter graph preparedPlans sections (atomKey, dirtyRows) =
  first EGraphRelationalRuntimeQueryObstruction $
    RelRuntime.evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections
      (pppQueryPlan preparedPlans)
      (pppCompiledStoragePlan preparedPlans)
      (flowRootSelection rootClassFilter graph)
      (IntMap.insert atomKey (RelRuntime.wholeRuntimeSection dirtyRows) sections)
{-# INLINE flowStructuralMatchesWithRuntimeSections #-}

flowQuerySections ::
  Language f =>
  EGraph f a ->
  QueryPlan capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, IntMap (RowBlock 'Canonical))
flowQuerySections graph queryPlan state =
  first
    (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
    (preparedQuerySections (eGraphRevisionValue (eGraphRevision graph)) graph queryPlan state)
{-# INLINE flowQuerySections #-}

flowQuerySectionsExcept ::
  Language f =>
  EGraph f a ->
  QueryPlan capability f ->
  IntSet ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, IntMap (RowBlock 'Canonical))
flowQuerySectionsExcept graph queryPlan atomKeys state =
  first
    (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
    (preparedQuerySectionsForAtomKeys atomKeys (eGraphRevisionValue (eGraphRevision graph)) graph queryPlan state)
{-# INLINE flowQuerySectionsExcept #-}

fullSectionAtomKeys ::
  QueryPlan capability f ->
  IntSet ->
  IntSet
fullSectionAtomKeys queryPlan dirtyAtomKeys =
  if IntSet.size dirtyAtomKeys <= 1
    then IntSet.difference queryAtomKeys dirtyAtomKeys
    else queryAtomKeys
  where
    queryAtomKeys =
      IntSet.fromList
        (fmap (RelPlan.queryAtomKey . RelPlan.asQueryAtomId) (Vector.toList (RelPlan.qpAtoms queryPlan)))
{-# INLINE fullSectionAtomKeys #-}

freshFlowQuerySections ::
  Language f =>
  EGraph f a ->
  QueryPlan capability f ->
  Either EGraphRelationalMatchObstruction (IntMap (RowBlock 'Canonical))
freshFlowQuerySections graph queryPlan =
  first
    (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
    (preparedBaseRowBlocks (eGraphRevisionValue (eGraphRevision graph)) (buildPreparedBase queryPlan graph))
{-# INLINE freshFlowQuerySections #-}

flowRowsForDirtyResults ::
  Language f =>
  IntSet ->
  EGraph f a ->
  QueryPlan capability f ->
  EGraphPreparedMatchState owner capability f ->
  Either EGraphRelationalMatchObstruction (EGraphPreparedMatchState owner capability f, IntMap (IntMap [RowTupleKey]))
flowRowsForDirtyResults dirtyResults graph queryPlan state =
  Right (dirtyState, dirtyRowsByAtom)
  where
    PreparedSectionBuild dirtyState dirtyRowsByAtom =
      Foldable.foldl'
        insertDirtyRows
        PreparedSectionBuild
          { psbState = ensurePreparedSharedTagRows graph state,
            psbSections = IntMap.empty
          }
        (Vector.toList (RelPlan.qpAtoms queryPlan))

    insertDirtyRows dirtyBuild atomSpec =
      let (rowsState, rowsByResult) =
            preparedAlphaRowsForAtom graph atomSpec (psbState dirtyBuild)
          atomKey =
            RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
          dirtyRowsByResult =
            IntMap.restrictKeys rowsByResult dirtyResults
       in PreparedSectionBuild
            { psbState = rowsState,
              psbSections =
                if IntMap.null dirtyRowsByResult
                  then psbSections dirtyBuild
                  else IntMap.insert atomKey dirtyRowsByResult (psbSections dirtyBuild)
            }
{-# INLINE flowRowsForDirtyResults #-}

flowDirtySectionForAtom ::
  EGraph f a ->
  QueryPlan capability f ->
  (Int, IntMap [RowTupleKey]) ->
  Either EGraphRelationalMatchObstruction (Maybe (Int, RowBlock 'Canonical))
flowDirtySectionForAtom graph queryPlan (atomKey, rowsByResult) =
  traverse
    ( \atomSpec ->
        fmap ((,) atomKey) $
          first
            (EGraphRelationalRuntimeQueryObstruction . RelRuntime.RuntimeQueryPlanRowBuildObstruction)
            ( atomRowsFromTupleKeys
                (preparedBaseRowIdentity (eGraphRevisionValue (eGraphRevision graph)) queryPlan atomSpec)
                (RelPlan.asColumns atomSpec)
                (flattenRowsByResult rowsByResult)
            )
    )
    (queryPlanAtomSpec atomKey queryPlan)
{-# INLINE flowDirtySectionForAtom #-}

queryPlanAtomSpec ::
  Int ->
  QueryPlan capability f ->
  Maybe (RelPlan.AtomSpec (f ()) (ENode f) ClassId)
queryPlanAtomSpec atomKey queryPlan =
  case filter ((== atomKey) . RelPlan.queryAtomKey . RelPlan.asQueryAtomId) (Vector.toList (RelPlan.qpAtoms queryPlan)) of
    atomSpec : _ ->
      Just atomSpec
    [] ->
      Nothing
{-# INLINE queryPlanAtomSpec #-}

flowRootSelection ::
  RootClassFilter ->
  EGraph f a ->
  RelRuntime.RuntimeRootSelection
flowRootSelection rootClassFilter graph =
  case rootClassFilter of
    AllRootClasses ->
      RelRuntime.RuntimeAllRoots
    RestrictedRootClasses rootKeys ->
      RelRuntime.RuntimeRootKeys (canonicalRootKeys graph rootKeys)
{-# INLINE flowRootSelection #-}

deltaMatchBindings ::
  RootClassFilter ->
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  [QueryBinding ClassId] ->
  [(ClassId, Substitution)]
deltaMatchBindings rootClassFilter graph compiledPlan =
  mapMaybe (bindingMatch rootClassFilter graph compiledPlan)
    . Set.toAscList
    . Set.fromList
{-# INLINE deltaMatchBindings #-}

dirtyRootDomainMatches ::
  RootClassFilter ->
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  IntSet ->
  [(ClassId, Substitution)]
dirtyRootDomainMatches rootClassFilter graph compiledPlan dirtyResults =
  deltaMatchBindings
    rootClassFilter
    graph
    compiledPlan
    (dirtyRootDomainBindings rootClassFilter graph compiledPlan dirtyResults)
{-# INLINE dirtyRootDomainMatches #-}

dirtyRootDomainBindings ::
  RootClassFilter ->
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  IntSet ->
  [QueryBinding ClassId]
dirtyRootDomainBindings rootClassFilter graph compiledPlan dirtyResults =
  fmap
    (rootDomainBinding compiledPlan . ClassId)
    ( IntSet.toAscList
        (IntSet.intersection (rootDomainKeys rootClassFilter graph) (canonicalRootKeys graph dirtyResults))
    )
{-# INLINE dirtyRootDomainBindings #-}

rootDomainMatches ::
  RootClassFilter ->
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  [(ClassId, Substitution)]
rootDomainMatches rootClassFilter graph compiledPlan =
  fmap rootMatch (IntSet.toAscList (rootDomainKeys rootClassFilter graph))
  where
    rootMatch rootKey =
      let rootClass = ClassId rootKey
       in (rootClass, rootDomainSubstitution rootClass compiledPlan)
{-# INLINE rootDomainMatches #-}

rootDomainBinding :: PatternFreeJoinPlan f ClassId -> ClassId -> QueryBinding ClassId
rootDomainBinding compiledPlan rootClass =
  QueryBinding (Map.fromList rootAssignments)
  where
    rootAssignments =
      fmap
        (\queryVar -> (queryVar, rootClass))
        (Map.elems (patternFreeJoinVariables compiledPlan))
{-# INLINE rootDomainBinding #-}

rootDomainSubstitution ::
  ClassId ->
  PatternFreeJoinPlan f ClassId ->
  Substitution
rootDomainSubstitution rootClass =
  Map.foldrWithKey
    (\patternVar _queryVar substitution -> insertSubst patternVar rootClass substitution)
    emptySubstitution
    . patternFreeJoinVariables
{-# INLINE rootDomainSubstitution #-}

bindingMatch ::
  RootClassFilter ->
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  QueryBinding ClassId ->
  Maybe (ClassId, Substitution)
bindingMatch rootClassFilter graph compiledPlan binding =
  bindingRoot compiledPlan binding
    >>= canonicalRootMatch
  where
    canonicalRootMatch rootClass =
      let canonicalRoot =
            canonicalizeClassId graph rootClass
       in if rootClassAllowed rootClassFilter graph canonicalRoot
            then fmap ((,) canonicalRoot) (bindingSubstitution graph compiledPlan binding)
            else Nothing
{-# INLINE bindingMatch #-}

bindingRoot ::
  PatternFreeJoinPlan f ClassId ->
  QueryBinding ClassId ->
  Maybe ClassId
bindingRoot compiledPlan binding =
  case patternFreeJoinRoots compiledPlan of
    rootTerm :| _ ->
      resolveQueryTerm binding rootTerm
{-# INLINE bindingRoot #-}

bindingSubstitution ::
  EGraph f a ->
  PatternFreeJoinPlan f ClassId ->
  QueryBinding ClassId ->
  Maybe Substitution
bindingSubstitution graph compiledPlan binding =
  Map.foldrWithKey
    insertPatternBinding
    (Just emptySubstitution)
    (patternFreeJoinVariables compiledPlan)
  where
    insertPatternBinding patternVar queryVar maybeSubstitution =
      maybeSubstitution >>= \substitution ->
        fmap
          (\classIdValue -> insertSubst patternVar (canonicalizeClassId graph classIdValue) substitution)
          (Map.lookup queryVar (queryBindingAssignments binding))
{-# INLINE bindingSubstitution #-}

resolveQueryTerm :: QueryBinding key -> QueryTerm key -> Maybe key
resolveQueryTerm binding term =
  case term of
    QueryBound key ->
      Just key
    QueryVariable queryVar ->
      Map.lookup queryVar (queryBindingAssignments binding)
{-# INLINE resolveQueryTerm #-}

rootDomainKeys ::
  RootClassFilter ->
  EGraph f a ->
  IntSet
rootDomainKeys rootClassFilter graph =
  let graphKeys =
        IntMap.keysSet (eGraphAnalysis graph)
   in case rootClassFilter of
        AllRootClasses ->
          graphKeys
        RestrictedRootClasses rootKeys ->
          IntSet.intersection (canonicalRootKeys graph rootKeys) graphKeys
