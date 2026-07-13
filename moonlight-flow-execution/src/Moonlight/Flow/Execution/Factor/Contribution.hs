{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Factor.Contribution
  ( FactorSourceCell (..),
    FactorContribution (..),
    FactorContributionIndex,
    FactorContributionChange (..),
    FactorSupportPatchStats (..),
    emptyFactorContributionIndex,
    insertFactorContribution,
    advanceFactorContributionIndex,
    factorContributionIndexSupportKeysForSourceCells,
    factorContributionIndexValueAt,
    factorContributionIndexProvRoots,
    factorContributionSupportPatchStats,
    remapFactorContributionIndexProvIds,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Operator.SupportedView
  ( Contribution (..),
    SupportedRow,
    SupportedView,
    ViewChange (..),
    buildSupportedView,
    emptySupportedView,
    supportedRowContributions,
    supportedViewAdvance,
    supportedViewKeysForCells,
    supportedViewRows,
    supportedViewValueAt,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
  )
import Moonlight.Flow.Execution.Factor.Contribution.Identity
  ( ProvAccum (..),
    provAccumOfValue,
    renderProvAccum,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvIdRemap,
    remapProvVal,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvId (..),
    ProvVal (..),
    ProvenanceObstruction,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
  )

data FactorSourceCell = FactorSourceCell
  { fscSourceId :: {-# UNPACK #-} !Int,
    fscKey :: !AssignmentTupleKey
  }
  deriving stock (Eq, Ord, Show)

-- | One factor-output witness contribution.  This is the maintained aggregate
-- authority for factor payloads and source-cell locality; there is no separate
-- support-index sidecar.
data FactorContribution = FactorContribution
  { fctValue :: !ProvVal,
    fctSupportCells :: !(Set FactorSourceCell)
  }
  deriving stock (Eq, Ord, Show)

-- | The support-counted factor view seated on the differential organ: a pure
-- presence-monoid @SupportedView@ owns invalidation and support edges, and the
-- arena-rendered @ProvVal@ per key is an emit-side projection cache.
data FactorContributionIndex = FactorContributionIndex
  { fciView :: !(SupportedView FactorSourceCell AssignmentTupleKey ProvAccum),
    fciRendered :: !(Map AssignmentTupleKey ProvVal)
  }
  deriving stock (Eq, Show)

data FactorContributionChange = FactorContributionChange
  { fccTouchedKeys :: !(Set AssignmentTupleKey),
    fccInsertedSupportEdges :: !(Map AssignmentTupleKey (Set FactorSourceCell)),
    fccDeletedSupportEdges :: !(Map AssignmentTupleKey (Set FactorSourceCell)),
    fccRepairTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Show)

data FactorSupportPatchStats = FactorSupportPatchStats
  { fspsCellsVisited :: {-# UNPACK #-} !Int,
    fspsEdgesPreserved :: {-# UNPACK #-} !Int,
    fspsEdgesInserted :: {-# UNPACK #-} !Int,
    fspsEdgesDeleted :: {-# UNPACK #-} !Int,
    fspsOutputKeysDeleted :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup FactorContributionChange where
  left <> right =
    FactorContributionChange
      { fccTouchedKeys =
          Set.union (fccTouchedKeys left) (fccTouchedKeys right),
        fccInsertedSupportEdges =
          Map.unionWith Set.union (fccInsertedSupportEdges left) (fccInsertedSupportEdges right),
        fccDeletedSupportEdges =
          Map.unionWith Set.union (fccDeletedSupportEdges left) (fccDeletedSupportEdges right),
        fccRepairTelemetry =
          fccRepairTelemetry left <> fccRepairTelemetry right
      }
  {-# INLINE (<>) #-}

instance Monoid FactorContributionChange where
  mempty =
    FactorContributionChange
      { fccTouchedKeys = Set.empty,
        fccInsertedSupportEdges = Map.empty,
        fccDeletedSupportEdges = Map.empty,
        fccRepairTelemetry = emptyRepairTelemetry
      }
  {-# INLINE mempty #-}

emptyFactorContributionIndex :: FactorContributionIndex
emptyFactorContributionIndex =
  FactorContributionIndex
    { fciView = emptySupportedView,
      fciRendered = Map.empty
    }
{-# INLINE emptyFactorContributionIndex #-}

emptyFactorSupportPatchStats :: FactorSupportPatchStats
emptyFactorSupportPatchStats =
  FactorSupportPatchStats
    { fspsCellsVisited = 0,
      fspsEdgesPreserved = 0,
      fspsEdgesInserted = 0,
      fspsEdgesDeleted = 0,
      fspsOutputKeysDeleted = 0
    }
{-# INLINE emptyFactorSupportPatchStats #-}

toOrganContribution :: FactorContribution -> Contribution FactorSourceCell ProvAccum
toOrganContribution contribution =
  Contribution
    (provAccumOfValue (fctValue contribution))
    (fctSupportCells contribution)
{-# INLINE toOrganContribution #-}

insertFactorContribution ::
  RepairTelemetryConfig ->
  AssignmentTupleKey ->
  FactorContribution ->
  ProvArena ->
  FactorContributionIndex ->
  (ProvArena, FactorContributionIndex, FactorContributionChange)
insertFactorContribution config key contribution arena0 index0 =
  advanceFactorContributionIndex config Set.empty (Map.singleton key [contribution]) arena0 index0
{-# INLINE insertFactorContribution #-}

advanceFactorContributionIndex ::
  RepairTelemetryConfig ->
  Set FactorSourceCell ->
  Map AssignmentTupleKey [FactorContribution] ->
  ProvArena ->
  FactorContributionIndex ->
  (ProvArena, FactorContributionIndex, FactorContributionChange)
advanceFactorContributionIndex config dirtyCells fresh arena0 index0 =
  let !oldView =
        fciView index0
      !freshOrgan =
        fmap (fmap toOrganContribution) fresh
      (!newView, !changes) =
        supportedViewAdvance dirtyCells freshOrgan oldView
      (!arena1, !rendered1, !telemetry) =
        Map.foldlWithKey'
          (renderChange newView)
          (arena0, fciRendered index0, emptyRepairTelemetry)
          changes
      !workKeys =
        Set.union (supportedViewKeysForCells dirtyCells oldView) (Map.keysSet fresh)
      (!insertedEdges, !deletedEdges) =
        supportEdgeTransitions oldView newView workKeys
      !touched =
        Set.unions
          [ Map.keysSet changes,
            Map.keysSet insertedEdges,
            Map.keysSet deletedEdges
          ]
      !change =
        FactorContributionChange
          { fccTouchedKeys = touched,
            fccInsertedSupportEdges = insertedEdges,
            fccDeletedSupportEdges = deletedEdges,
            fccRepairTelemetry = telemetry
          }
      !index1 =
        FactorContributionIndex
          { fciView = newView,
            fciRendered = rendered1
          }
   in (arena1, index1, change)
  where
    renderChange newView (!arena, !rendered, !tel) key change =
      case change of
        ViewRemoved _ ->
          (arena, Map.delete key rendered, tel)
        _ ->
          let (!arena', !renderedValue, !tel') =
                renderProvAccum config arena (supportedViewValueAt key newView)
           in (arena', Map.insert key renderedValue rendered, tel <> tel')

supportEdgeTransitions ::
  SupportedView FactorSourceCell AssignmentTupleKey ProvAccum ->
  SupportedView FactorSourceCell AssignmentTupleKey ProvAccum ->
  Set AssignmentTupleKey ->
  ( Map AssignmentTupleKey (Set FactorSourceCell),
    Map AssignmentTupleKey (Set FactorSourceCell)
  )
supportEdgeTransitions oldView newView workKeys =
  Set.foldl' step (Map.empty, Map.empty) workKeys
  where
    step (!inserted, !deleted) key =
      let !oldCells =
            supportCellsAtView key oldView
          !newCells =
            supportCellsAtView key newView
          !addedCells =
            Set.difference newCells oldCells
          !removedCells =
            Set.difference oldCells newCells
          !inserted1 =
            if Set.null addedCells then inserted else Map.insert key addedCells inserted
          !deleted1 =
            if Set.null removedCells then deleted else Map.insert key removedCells deleted
       in (inserted1, deleted1)

supportCellsAtView ::
  AssignmentTupleKey ->
  SupportedView FactorSourceCell AssignmentTupleKey ProvAccum ->
  Set FactorSourceCell
supportCellsAtView key view =
  maybe
    Set.empty
    supportCellsOfRow
    (Map.lookup key (supportedViewRows view))
{-# INLINE supportCellsAtView #-}

supportCellsOfRow :: SupportedRow FactorSourceCell ProvAccum -> Set FactorSourceCell
supportCellsOfRow =
  foldMap contributionSupport . Map.keys . supportedRowContributions
{-# INLINE supportCellsOfRow #-}

factorContributionIndexSupportKeysForSourceCells ::
  Set FactorSourceCell ->
  FactorContributionIndex ->
  Set AssignmentTupleKey
factorContributionIndexSupportKeysForSourceCells cells indexValue =
  supportedViewKeysForCells cells (fciView indexValue)

factorContributionIndexValueAt ::
  AssignmentTupleKey ->
  FactorContributionIndex ->
  ProvVal
factorContributionIndexValueAt key indexValue =
  Map.findWithDefault PVZero key (fciRendered indexValue)
{-# INLINE factorContributionIndexValueAt #-}

factorContributionIndexProvRoots :: FactorContributionIndex -> [ProvVal]
factorContributionIndexProvRoots indexValue =
  Map.elems (fciRendered indexValue) <> foldMap rowRoots rows
  where
    rows =
      Map.elems (supportedViewRows (fciView indexValue))
    rowRoots row =
      fmap (invertProvAccum . contributionValue) (Map.keys (supportedRowContributions row))

factorContributionSupportPatchStats ::
  FactorContributionIndex ->
  FactorContributionChange ->
  FactorSupportPatchStats
factorContributionSupportPatchStats indexValue change =
  Set.foldl' measureOutput emptyFactorSupportPatchStats outputKeys
  where
    outputKeys =
      Set.union
        (Map.keysSet (fccInsertedSupportEdges change))
        (Map.keysSet (fccDeletedSupportEdges change))

    measureOutput stats key =
      let !rawDeleted =
            Map.findWithDefault Set.empty key (fccDeletedSupportEdges change)
          !rawInserted =
            Map.findWithDefault Set.empty key (fccInsertedSupportEdges change)
          !deletedCells =
            Set.difference rawDeleted rawInserted
          !insertedCells =
            Set.difference rawInserted rawDeleted
          !outputDeleted =
            not (Set.null deletedCells) && Set.null (supportCellsAtView key (fciView indexValue))
       in stats
            { fspsCellsVisited =
                fspsCellsVisited stats + Set.size deletedCells + Set.size insertedCells,
              fspsEdgesInserted =
                fspsEdgesInserted stats + Set.size insertedCells,
              fspsEdgesDeleted =
                fspsEdgesDeleted stats + Set.size deletedCells,
              fspsOutputKeysDeleted =
                fspsOutputKeysDeleted stats + if outputDeleted then 1 else 0
            }

remapFactorContributionIndexProvIds ::
  ProvIdRemap ->
  FactorContributionIndex ->
  Either ProvenanceObstruction FactorContributionIndex
remapFactorContributionIndexProvIds remap indexValue = do
  rendered <- traverse (remapProvVal remap) (fciRendered indexValue)
  contributionLists <- traverse remapRow (supportedViewRows (fciView indexValue))
  pure
    FactorContributionIndex
      { fciView = buildSupportedView contributionLists,
        fciRendered = rendered
      }
  where
    remapRow row =
      fmap concat (traverse remapEntry (Map.toList (supportedRowContributions row)))

    remapEntry (contribution, count) = do
      value <- remapProvVal remap (invertProvAccum (contributionValue contribution))
      let !organ =
            Contribution (provAccumOfValue value) (contributionSupport contribution)
      pure (replicate count organ)

invertProvAccum :: ProvAccum -> ProvVal
invertProvAccum accum =
  case Set.lookupMin (paObstructions accum) of
    Just obstruction ->
      PVObstructed obstruction
    Nothing ->
      case IntSet.minView (paValueRefs accum) of
        Just (rawId, _) ->
          PVRef (ProvId rawId)
        Nothing ->
          if paHasUnit accum then PVOne else PVZero
{-# INLINE invertProvAccum #-}
