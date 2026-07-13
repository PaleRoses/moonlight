{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Execution.Observe.Provenance.Arena
  ( nodeAt,
    internProv,
    internProvWithTelemetry,
    rebuildCons,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvEntry (..),
    ProvGen (..),
    ProvId (..),
    ProvNode,
    ProvenanceObstruction (..),
    paCons,
    paNext,
    paNodes,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
    recordProvInternInsert,
    recordProvInternLookup,
    repairTelemetryDetailed,
  )

nodeAt :: ProvArena -> ProvId -> Either ProvenanceObstruction ProvNode
nodeAt arena pid =
  case IntMap.lookup (unProvId pid) (paNodes arena) of
    Just entry -> Right (peNode entry)
    Nothing -> Left (DanglingProvId pid)

internProv :: ProvNode -> ProvArena -> (ProvArena, ProvId)
internProv node arena =
  case Map.lookup node (paCons arena) of
    Just pid -> (arena, pid)
    Nothing ->
      let pid = ProvId (paNext arena)
          entry =
            ProvEntry
              { peNode = node,
                peGen = GenNursery,
                peSurvivals = 0
              }
       in ( arena
              { paNext = paNext arena + 1,
                paNodes = IntMap.insert (unProvId pid) entry (paNodes arena),
                paCons = Map.insert node pid (paCons arena)
              },
            pid
          )

internProvWithTelemetry ::
  RepairTelemetryConfig ->
  ProvNode ->
  ProvArena ->
  (ProvArena, ProvId, RepairTelemetry)
internProvWithTelemetry config node arena
  | not (repairTelemetryDetailed config) =
      let (!arena1, !pid) =
            internProv node arena
       in (arena1, pid, emptyRepairTelemetry)
  | otherwise =
      case Map.lookup node (paCons arena) of
        Just pid ->
          (arena, pid, recordProvInternLookup config emptyRepairTelemetry)
        Nothing ->
          let pid = ProvId (paNext arena)
              entry =
                ProvEntry
                  { peNode = node,
                    peGen = GenNursery,
                    peSurvivals = 0
                  }
           in ( arena
                  { paNext = paNext arena + 1,
                    paNodes = IntMap.insert (unProvId pid) entry (paNodes arena),
                    paCons = Map.insert node pid (paCons arena)
                  },
                pid,
                recordProvInternInsert config $
                  recordProvInternLookup config emptyRepairTelemetry
              )
{-# INLINE internProvWithTelemetry #-}

rebuildCons :: IntMap ProvEntry -> Map ProvNode ProvId
rebuildCons =
  Map.fromList
    . fmap (\(key, entry) -> (peNode entry, ProvId key))
    . IntMap.toAscList
