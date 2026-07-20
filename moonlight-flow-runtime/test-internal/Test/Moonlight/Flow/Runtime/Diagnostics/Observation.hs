{-# LANGUAGE DerivingStrategies #-}
module Test.Moonlight.Flow.Runtime.Diagnostics.Observation
  ( RuntimeObservation (..),
    observeRuntimeWithEvidenceView,
  )
where
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( listToMaybe,
    mapMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalGlobalSection (..),
    RelationalSection (..),
  )
import Moonlight.Flow.Carrier.Fact
  ( carrierLiveEvidenceAt,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierBoundaryLatestTraceNow,
    visibleGlobalAcrossStores,
  )
import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CohomologicalFailure,
    PropagationFailure,
    RestrictionFailure,
  )
import Moonlight.Flow.Carrier.Diagnostics.Obstruction
  ( CarrierEvidenceView,
    cohomologicalFailuresNow,
    propagationFailuresNow,
    restrictionFailuresNow,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseStats,
    planReuseStats,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsPlanReuse,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    runtimeShardRegistry,
  )
data RuntimeObservation ctx prop carrier boundary evidence = RuntimeObservation
  { roVisibleGlobal :: !(RelationalGlobalSection ctx carrier prop),
    roVisibleByContext :: !(Map ctx (RelationalSection ctx carrier prop)),
    roBoundaryLatestTraceByCarrier :: !(Map (CarrierAddr ctx carrier prop) boundary),
    roEvidenceLiveSeedByCarrier :: !(Map (CarrierAddr ctx carrier prop) [evidence]),
    roRestrictionFailures :: ![RestrictionFailure ctx carrier prop boundary],
    roPropagationFailures :: ![PropagationFailure ctx carrier prop],
    roCohomologicalFailures :: ![CohomologicalFailure ctx carrier prop boundary],
    roPlanReuseStats :: !PlanReuseStats
  }
  deriving stock (Eq, Show)

observeRuntimeWithEvidenceView ::
  (Ord ctx, Ord prop) =>
  CarrierEvidenceView ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeObservation ctx prop Carrier boundary evidence
observeRuntimeWithEvidenceView evidenceView runtime =
  RuntimeObservation
    { roVisibleGlobal = visibleGlobal,
      roVisibleByContext = visibleContexts,
      roBoundaryLatestTraceByCarrier = carrierBoundariesLatestTrace indexes visibleCarriers,
      roEvidenceLiveSeedByCarrier = carrierEvidenceLiveSeed indexes visibleCarriers,
      roRestrictionFailures = restrictionFailures indexes visibleContextSet,
      roPropagationFailures = propagationFailures indexes visibleContextSet,
      roCohomologicalFailures = cohomologicalFailures indexes visibleContextSet,
      roPlanReuseStats = planReuseStats (rsPlanReuse (rdrState runtime))
    }
  where
    indexes =
      rsrIndexOps (runtimeShardRegistry (rdrState runtime))
    visibleGlobal =
      visibleGlobalAcrossStores indexes
    visibleContexts =
      rgsContexts visibleGlobal
    visibleContextSet =
      Map.keysSet visibleContexts
    visibleCarriers =
      Set.unions
        [ Map.keysSet (rsCarriers sectionValue)
        | sectionValue <- Map.elems visibleContexts
        ]
    restrictionFailures =
      failuresByContext restrictionFailuresNow evidenceView
    propagationFailures =
      failuresByContext propagationFailuresNow evidenceView
    cohomologicalFailures =
      failuresByContext cohomologicalFailuresNow evidenceView
{-# INLINE observeRuntimeWithEvidenceView #-}

carrierBoundariesLatestTrace ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierStore ctx carrier prop boundary evidence) ->
  Set (CarrierAddr ctx carrier prop) ->
  Map (CarrierAddr ctx carrier prop) boundary
carrierBoundariesLatestTrace indexes =
  Map.fromAscList . mapMaybe (carrierBoundaryLatestTraceAcrossIndexes indexes) . Set.toAscList
{-# INLINE carrierBoundariesLatestTrace #-}
carrierBoundaryLatestTraceAcrossIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierStore ctx carrier prop boundary evidence) ->
  CarrierAddr ctx carrier prop ->
  Maybe (CarrierAddr ctx carrier prop, boundary)
carrierBoundaryLatestTraceAcrossIndexes indexes addr =
  fmap
    (\boundary -> (addr, boundary))
    (listToMaybe (mapMaybe (carrierBoundaryLatestTraceNow addr) (IntMap.elems indexes)))
{-# INLINE carrierBoundaryLatestTraceAcrossIndexes #-}
carrierEvidenceLiveSeed ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierStore ctx carrier prop boundary evidence) ->
  Set (CarrierAddr ctx carrier prop) ->
  Map (CarrierAddr ctx carrier prop) [evidence]
carrierEvidenceLiveSeed indexes =
  Map.fromAscList
    . mapMaybe carrierEvidenceAcrossIndexes
    . Set.toAscList
  where
    carrierEvidenceAcrossIndexes addr =
      let evidenceValues =
            concatMap (carrierLiveEvidenceAt addr) (IntMap.elems indexes)
       in case evidenceValues of
            [] ->
              Nothing
            _ ->
              Just (addr, evidenceValues)
{-# INLINE carrierEvidenceLiveSeed #-}
failuresByContext ::
  (ctx -> CarrierEvidenceView ctx carrier prop boundary evidence -> CarrierStore ctx carrier prop boundary evidence -> [failure]) ->
  CarrierEvidenceView ctx carrier prop boundary evidence ->
  IntMap (CarrierStore ctx carrier prop boundary evidence) ->
  Set ctx ->
  [failure]
failuresByContext projectFailures evidenceView indexes contexts =
  [ failure
  | contextValue <- Set.toAscList contexts,
    indexState <- IntMap.elems indexes,
    failure <- projectFailures contextValue evidenceView indexState
  ]
{-# INLINE failuresByContext #-}
