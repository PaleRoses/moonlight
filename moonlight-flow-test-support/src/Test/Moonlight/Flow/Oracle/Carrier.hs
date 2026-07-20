module Test.Moonlight.Flow.Oracle.Carrier
  ( oracleFinalMultiplicity,
    oracleLiveEvidence,
    oracleCarrierAddr,
    oracleCarrierBoundary,
    oracleCarrierDelta,
    oracleCarrierRow,
    oracleCarrierTime,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkSlotId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryAtomCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Test.Moonlight.Flow.Gen.Carrier (CarrierWorkload (..))
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( principalSupport
  )

oracleFinalMultiplicity :: CarrierWorkload -> Int
oracleFinalMultiplicity workload =
  cwInsertCount workload - cwRetractCount workload

oracleLiveEvidence :: CarrierWorkload -> [String]
oracleLiveEvidence workload =
  drop (cwRetractCount workload) (fmap seedLabel [0 .. cwInsertCount workload - 1])

seedLabel :: Int -> String
seedLabel ix =
  "seed-" <> show ix

oracleCarrierTime :: Word64 -> RelationalCarrierTime Int
oracleCarrierTime seqValue =
  mkRelationalCarrierTime
    0
    initialQuotientEpoch
    initialLiveEpoch
    PhaseProject
    (frontierStamp (fromIntegral seqValue))
{-# INLINE oracleCarrierTime #-}

oracleCarrierRow :: [RepKey] -> RowTupleKey
oracleCarrierRow =
  tupleKeyFromRepKeys
{-# INLINE oracleCarrierRow #-}

oracleCarrierAddr :: Int -> CarrierAddr Int Carrier Int
oracleCarrierAddr contextValue =
  carrierAddr contextValue (PropositionKey 0) (queryAtomCarrier (mkQueryId 0) (mkAtomId 1))
{-# INLINE oracleCarrierAddr #-}

oracleCarrierBoundary :: Either RuntimeBoundaryError RuntimeBoundary
oracleCarrierBoundary =
  mkRuntimeBoundary [mkSlotId 0] IntSet.empty IntMap.empty
{-# INLINE oracleCarrierBoundary #-}

oracleCarrierDelta ::
  RuntimeBoundary ->
  CarrierAddr Int Carrier Int ->
  RowDelta ->
  RelationalCarrierDelta Int Carrier Int RuntimeBoundary ()
oracleCarrierDelta boundary addr rows =
  RelationalCarrierDelta
    { deAddr = addr,
      deTime = oracleCarrierTime 0,
      deSupport = principalSupport (caContext addr),
      deBoundary = boundary,
      deEvidence = (),
      deRows = rows,
      deOrigin = RelationalOrigin {roEvent = OriginLocal (mkQueryId 0), roRoute = emptyDerivationRoute},
      deScope =
        mempty
          { rsDeps = DepsDelta IntSet.empty,
            rsTopo = TopoDelta IntSet.empty
          },
      dePayload = ()
    }
{-# INLINE oracleCarrierDelta #-}
