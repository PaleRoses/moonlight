{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Moonlight.Flow.Trace.EngineClosureFixture
  ( ClosureRuntime,
    ClosureFixture (..),
    ClosureFixtureError (..),
    closureContexts,
    expectedPinnedTraceIds,
    expectedPendingTimes,
    closureFixture,
  )
where

import Data.Bifunctor (first)
import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkSlotId,
  )
import Moonlight.Differential.Frontier
  ( emptyRuntimeFrontier,
    frontierAdvanceVisibleMin,
    frontierWithPendingCounts,
    frontierWithTraceRetention,
    traceRetention,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( FrontierStamp, frontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryAtomCarrier,
    queryBagCarrier,
    queryRootCarrier,
    querySeparatorCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    carrierAddr,
    restrictKey,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreError,
    commitCarrierDelta,
    emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CohomologicalFailure (..),
    PropagationFailure (..),
    RestrictionFailure (..),
  )
import Moonlight.Flow.Carrier.Diagnostics.Obstruction
  ( CarrierEvidenceView (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (..),
    RelationalOrigin (..),
    emptyDerivationRoute,
    originConsRestriction,
    originMerge,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  (
    plainRowPatchFromList,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
    FactorNode (..),
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Runtime.Topology
  ( RuntimeTopologyError,
    updateRuntimeGeneratedSite,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    runtimeShardRegistry,
    setRuntimeShardRegistry,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedContextShape (..),
    GeneratedQueryBinding (..),
    GeneratedRoutingSource (..),
    GeneratedSiteState (..),
    Shard (..),
    emptyGeneratedSiteState,
    generatedContextShapeDigest,
    refreshGeneratedSiteDigest,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( emptyRuntimeInitialData,
    runtimeContextSchema,
    runtimeSchema,
  )
import Moonlight.Flow.Runtime.Types
  ( RuntimeCreateError,
  )
import Moonlight.Flow.Runtime.Types
  ( RuntimeCreateOptions (..),
    defaultRuntimeCreateOptions,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeSpec (..),
  )
import Moonlight.Flow.Runtime.Kernel.Create
  ( createRelDiffRuntimeWithBackend,
  )
import Moonlight.Flow.Runtime.Backend
  ( sheafBackend,
  )
import Test.Moonlight.Flow.Oracle.Runtime.Program
  ( TestRuntime,
  )
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )

type Ctx = Int

type Prop = Int

type Evidence = ()

type ClosureRuntime = TestRuntime

data ClosureFixture = ClosureFixture
  { cfRichRuntime :: !ClosureRuntime,
    cfCompactingFrontier :: !(RelDiffFrontier Ctx RelationalPhase),
    cfPinnedFrontier :: !(RelDiffFrontier Ctx RelationalPhase),
    cfPendingFrontier :: !(RelDiffFrontier Ctx RelationalPhase),
    cfEvidenceView :: !(CarrierEvidenceView Ctx Carrier Prop RuntimeBoundary Evidence)
  }

data ClosureFixtureError
  = ClosureBoundaryError !RuntimeBoundaryError
  | ClosureRuntimeCreateError !(RuntimeCreateError Ctx Prop)
  | ClosureRuntimeTopologyError !(RuntimeTopologyError Ctx Prop)
  | ClosureCarrierStoreError !(CarrierStoreError Ctx Carrier Prop RuntimeBoundary Evidence)
  | ClosureContextLatticeError !(ContextLatticeCompileError Ctx)
  deriving stock (Show)

data ClosureTraceSpec = ClosureTraceSpec
  { ctsStamp :: !Word64,
    ctsAddr :: !(CarrierAddr Ctx Carrier Prop),
    ctsRow :: !RowTupleKey,
    ctsOrigin :: !(RelationalOrigin Ctx Carrier Prop)
  }

closureFixture :: Either ClosureFixtureError ClosureFixture
closureFixture = do
  boundary <- first ClosureBoundaryError boundaryValue
  indexState <- indexedTrace boundary
  runtime <- runtimeFromIndex indexState
  let evidenceView = fixtureEvidenceView boundary
  pure
    ClosureFixture
      { cfRichRuntime = runtimeWithObservationReports evidenceView indexState runtime,
        cfCompactingFrontier = frontierWithVisibleMin IntSet.empty Set.empty,
        cfPinnedFrontier = frontierWithVisibleMin expectedPinnedTraceIds Set.empty,
        cfPendingFrontier = frontierWithVisibleMin IntSet.empty expectedPendingTimes,
        cfEvidenceView = evidenceView
      }

indexedTrace :: RuntimeBoundary -> Either ClosureFixtureError (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence)
indexedTrace boundary = do
  lattice <- contextLattice
  foldM (insertTrace lattice) emptyCarrierStore (fmap (traceDelta boundary) closureTraceSpecs)
  where
    insertTrace ::
      ContextLattice Ctx ->
      CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence ->
      RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence ->
      Either ClosureFixtureError (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence)
    insertTrace lattice indexState delta =
      first ClosureCarrierStoreError (commitCarrierDelta lattice delta indexState)

traceDelta :: RuntimeBoundary -> ClosureTraceSpec -> RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence
traceDelta boundary ClosureTraceSpec {ctsStamp, ctsAddr, ctsRow, ctsOrigin} =
  RelationalCarrierDelta
    { deAddr = ctsAddr,
      deTime = eventTime (caContext ctsAddr) ctsStamp,
      deSupport = principalSupport (caContext ctsAddr),
      deBoundary = boundary,
      deEvidence = (),
      deRows = (plainRowPatchFromList [(ctsRow, MultiplicityChange 1)]),
      deOrigin = ctsOrigin,
      deScope =
        mempty
          { rsDeps = DepsDelta (IntSet.singleton (fromIntegral ctsStamp)),
            rsTopo = TopoDelta (IntSet.singleton (fromIntegral ctsStamp + 20))
          },
      dePayload = ()
    }

closureTraceSpecs :: [ClosureTraceSpec]
closureTraceSpecs =
  [ ClosureTraceSpec 0 atomAddr (rowKey 7) atomOrigin,
    ClosureTraceSpec 1 bagAddr (rowKey 8) (factorOrigin (FactorNodeBag bagOne)),
    ClosureTraceSpec 2 separatorAddr (rowKey 9) (factorOrigin (FactorNodeSeparator bagOne bagTwo)),
    ClosureTraceSpec 3 rootAddr (rowKey 10) (factorOrigin FactorNodeRoot),
    ClosureTraceSpec 4 restrictedAddr (rowKey 11) (originConsRestriction restrictionKey (factorOrigin FactorNodeRoot)),
    ClosureTraceSpec 5 amalgamatedAddr (rowKey 12) (originMerge OriginAmalgamated (atomOrigin :| [factorOrigin FactorNodeRoot]))
  ]

runtimeFromIndex ::
  CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence ->
  Either ClosureFixtureError ClosureRuntime
runtimeFromIndex indexState = do
  lattice <- contextLattice
  runtime <-
    first ClosureRuntimeCreateError $
      createRelDiffRuntimeWithBackend
        (sheafBackend lattice)
        closureRuntimeSpec
        defaultRuntimeCreateOptions {rcoVisibleCacheBudgetBytes = 4096}
  first ClosureRuntimeTopologyError $
    unsafeSetRuntimeGeneratedSite
      closureGeneratedSite
      ( unsafeSetRuntimeNextFrontierStamp (frontierStamp 128)
          . unsafeSetRuntimeIndexStores (IntMap.singleton 0 indexState)
          $ runtime
      )
{-# INLINE runtimeFromIndex #-}

closureRuntimeSpec :: RuntimeSpec Ctx Prop
closureRuntimeSpec =
  RuntimeSpec
    { rsSchema =
        runtimeSchema
          [ (contextValue, runtimeContextSchema [] [propKey])
          | contextValue <- closureContexts
          ],
      rsPlans = [],
      rsInitialData = emptyRuntimeInitialData
    }
{-# INLINE closureRuntimeSpec #-}

runtimeWithObservationReports ::
  CarrierEvidenceView Ctx Carrier Prop RuntimeBoundary Evidence ->
  CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence ->
  ClosureRuntime ->
  ClosureRuntime
runtimeWithObservationReports _evidenceView _indexState runtime =
  runtime
{-# INLINE runtimeWithObservationReports #-}

unsafeSetRuntimeGeneratedSite ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either (RuntimeTopologyError ctx prop) (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
unsafeSetRuntimeGeneratedSite site runtime = do
  topology <-
    updateRuntimeGeneratedSite site (Core.rsTopology state0)
  pure
    runtime
      { rdrState =
          Core.mapRuntimeTopologySection (const topology) state0
      }
  where
    state0 =
      rdrState runtime
{-# INLINE unsafeSetRuntimeGeneratedSite #-}

unsafeSetRuntimeIndexStores ::
  IntMap.IntMap (CarrierStore ctx Carrier prop boundary evidence) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unsafeSetRuntimeIndexStores stores runtime =
  runtime
    { rdrState =
        setRuntimeShardRegistry
          ( (runtimeShardRegistry state0)
              { rsrIndexOps = stores
              }
          )
          state0
    }
  where
    state0 =
      rdrState runtime
{-# INLINE unsafeSetRuntimeIndexStores #-}

unsafeSetRuntimeNextFrontierStamp ::
  FrontierStamp ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unsafeSetRuntimeNextFrontierStamp stamp runtime =
  runtime
    { rdrState =
        Core.mapRuntimeClockState
          ( \clockState ->
              clockState {Core.rcsNextFrontierStamp = stamp}
          )
          (rdrState runtime)
    }
{-# INLINE unsafeSetRuntimeNextFrontierStamp #-}

fixtureEvidenceView :: RuntimeBoundary -> CarrierEvidenceView Ctx Carrier Prop RuntimeBoundary Evidence
fixtureEvidenceView boundary =
  CarrierEvidenceView
    { cevRestrictionFailures =
        \entry ->
          [ RestrictionFailure
              { rfSourceCarrier = csseAddr entry,
                rfTargetCarrier = restrictedAddr,
                rfFailedBoundary = boundary,
                rfDowngradeReason = "fixture restriction trace"
              }
          ],
      cevPropagationFailures =
        \entry ->
          [ PropagationFailure
              { pfCarrier = csseAddr entry,
                pfReason = "fixture propagation trace"
              }
          ],
      cevCohomologicalFailures =
        \entry ->
          [ CohomologicalFailure
              { cfCarrier = csseAddr entry,
                cfBoundary = boundary,
                cfReason = "fixture cohomological trace"
              }
          ]
    }


closureGeneratedSite :: GeneratedSiteState Ctx Prop
closureGeneratedSite =
  refreshGeneratedSiteDigest
    emptyGeneratedSiteState
      { gssContexts =
          Map.fromList
            [ (contextValue, closureGeneratedContextShape contextValue)
            | contextValue <- closureContexts
            ],
        gssRouteSource =
          GeneratedRoutingSource
            { grsAtomSubscribers = IntMap.empty,
              grsCarrierTouches = Map.empty,
              grsRestrictShardsByCarrier = shardByCarrier,
              grsIndexShardsByCarrier = shardByCarrier
            }
      }
  where
    shardByCarrier =
      Map.fromSet (const (Shard 0)) routedCarriers
{-# INLINE closureGeneratedSite #-}

closureGeneratedContextShape :: Ctx -> GeneratedContextShape Prop
closureGeneratedContextShape contextValue =
  refreshGeneratedContextShape
    GeneratedContextShape
      { gcsShapeDigest = runtimeShapeEmptyDigest,
        gcsQueryBindings = queryBindings,
        gcsIndexShardsByProp = Map.singleton propKey (Shard 0)
      }
  where
    queryBindings =
      if contextValue == 0
        then
          Map.singleton
            queryId
            GeneratedQueryBinding
              { gqbProp = propKey,
                gqbProjectShard = Shard 0
              }
        else Map.empty
{-# INLINE closureGeneratedContextShape #-}

refreshGeneratedContextShape ::
  GeneratedContextShape Prop ->
  GeneratedContextShape Prop
refreshGeneratedContextShape shape =
  shape {gcsShapeDigest = generatedContextShapeDigest shape}
{-# INLINE refreshGeneratedContextShape #-}

runtimeShapeEmptyDigest :: StableDigest128
runtimeShapeEmptyDigest =
  StableDigest128 0 0
{-# INLINE runtimeShapeEmptyDigest #-}

routedCarriers :: Set (CarrierAddr Ctx Carrier Prop)
routedCarriers =
  Set.fromList (fmap ctsAddr closureTraceSpecs)

frontierWithVisibleMin :: IntSet -> Set (RelationalCarrierTime Ctx) -> RelDiffFrontier Ctx RelationalPhase
frontierWithVisibleMin pinnedTraceIds pendingTimes =
  frontierWithPendingCounts
    (Map.fromSet (const 1) pendingTimes)
    ( frontierWithTraceRetention
        (Just (traceRetention pinnedTraceIds IntSet.empty IntSet.empty))
        (foldr frontierAdvanceVisibleMin emptyRuntimeFrontier visibleTimes)
    )
  where
    visibleTimes =
      zipWith eventTime closureContexts [20 ..]

expectedPinnedTraceIds :: IntSet
expectedPinnedTraceIds =
  IntSet.singleton 0

expectedPendingTimes :: Set (RelationalCarrierTime Ctx)
expectedPendingTimes =
  Set.singleton (eventTime 0 0)

closureContexts :: [Ctx]
closureContexts =
  [0 .. 5]

contextLattice :: Either ClosureFixtureError (ContextLattice Ctx)
contextLattice =
  first ClosureContextLatticeError $
    compileContextLattice
      (Set.fromList closureLatticeContexts)
      (contextOrderDecl 0 16 closureLatticeEdges)
  where
    closureLatticeContexts =
      [0 .. 16]

    closureLatticeEdges =
      zip [1 .. 16] [0 .. 15]

boundaryValue :: Either RuntimeBoundaryError RuntimeBoundary
boundaryValue =
  mkRuntimeBoundary [mkSlotId 0] IntSet.empty IntMap.empty

eventTime :: Ctx -> Word64 -> RelationalCarrierTime Ctx
eventTime contextValue stamp =
  mkRelationalCarrierTime contextValue initialQuotientEpoch initialLiveEpoch PhaseProject (frontierStamp (fromIntegral stamp))

atomOrigin :: RelationalOrigin Ctx Carrier Prop
atomOrigin =
  RelationalOrigin {roEvent = OriginAtom queryId atomId, roRoute = emptyDerivationRoute}

factorOrigin :: FactorNode -> RelationalOrigin Ctx Carrier Prop
factorOrigin node =
  RelationalOrigin {roEvent = OriginFactor queryId node, roRoute = emptyDerivationRoute}

queryId :: QueryId
queryId =
  mkQueryId 0

atomId :: AtomId
atomId =
  mkAtomId 7

propKey :: PropositionKey Prop
propKey =
  PropositionKey 0

bagOne :: BagId
bagOne =
  BagId 1

bagTwo :: BagId
bagTwo =
  BagId 2

atomAddr, bagAddr, separatorAddr, rootAddr, restrictedAddr, amalgamatedAddr :: CarrierAddr Ctx Carrier Prop
atomAddr = carrierAddr 0 propKey (queryAtomCarrier queryId atomId)
bagAddr = carrierAddr 1 propKey (queryBagCarrier queryId bagOne)
separatorAddr = carrierAddr 2 propKey (querySeparatorCarrier queryId bagOne bagTwo)
rootAddr = carrierAddr 3 propKey (queryRootCarrier queryId)
restrictedAddr = carrierAddr 4 propKey (queryRootCarrier (mkQueryId 1))
amalgamatedAddr = carrierAddr 5 propKey (queryRootCarrier (mkQueryId 2))

restrictionKey :: RestrictKey Ctx Carrier Prop
restrictionKey =
  restrictKey rootAddr restrictedAddr

rowKey :: Int -> RowTupleKey
rowKey value =
  tupleKeyFromRepKeys [RepKey value]
