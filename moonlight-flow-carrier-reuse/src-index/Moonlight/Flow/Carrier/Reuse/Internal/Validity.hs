{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidity (..),
    ReuseValidityRequest (..),
    viewSignatureDigest,
    reuseValidityFromRegistration,
    reuseValidityFromDelta,
    reuseValidityRequestFromTime,
    reuseTemporalViewMatchesRequest,
    reuseExactValidityMatchesRequest,
  )
where
import Moonlight.Flow.Model.Scope
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core (LiveEpoch, QuotientEpoch)
import Moonlight.Differential.Time (FrontierStamp)
import Moonlight.Flow.Carrier.Core.Delta (RelationalCarrierDeltaP (..), RelationalCarrierDelta)
import Moonlight.Flow.Carrier.Core.Time (RelationalCarrierTime, relationalTimeFrontierStamp, relationalTimeLiveEpoch, relationalTimeQuotientEpoch)

import Moonlight.Flow.Internal.Digest (wordOfInt)
import Moonlight.Flow.Plan.Shape.Encode (digestIntSet)
import Moonlight.Flow.Model.Schema.Digest (StableDigest128, stableDigest128)
import Moonlight.Flow.Plan.Residual (ResidualShape)
import Moonlight.Flow.Storage.Relation
  ( relationEpochDigestWords,
  )
import Moonlight.Flow.Storage.View
  ( ViewSignature (..),
  )

data ReuseValidity = ReuseValidity
  { rvQuotientEpoch :: !QuotientEpoch,
    rvLiveEpoch :: !LiveEpoch,
    rvFrontierStamp :: !FrontierStamp,
    rvViewDigest :: !(Maybe StableDigest128),
    rvResidualShape :: !ResidualShape,
    rvDependencyDigest :: !StableDigest128,
    rvTopoDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

data ReuseValidityRequest = ReuseValidityRequest
  { rvrQuotientEpoch :: !QuotientEpoch,
    rvrLiveEpoch :: !LiveEpoch,
    rvrFrontierStamp :: !FrontierStamp,
    rvrViewDigest :: !(Maybe StableDigest128),
    rvrResidualShape :: !ResidualShape
  }
  deriving stock (Eq, Ord, Show)

viewSignatureDigest :: ViewSignature -> StableDigest128
viewSignatureDigest signature =
  stableDigest128
    ( [0x76696577, vsOverrideHash signature]
        <> foldMap atomEpochWords (IntMap.toAscList (vsAtomEpochs signature))
    )
  where
    atomEpochWords (atomKey, epochValue) =
      wordOfInt atomKey : relationEpochDigestWords epochValue
{-# INLINE viewSignatureDigest #-}

reuseValidityFromDelta ::
  Maybe StableDigest128 ->
  ResidualShape ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  ReuseValidity
reuseValidityFromDelta maybeViewDigest residualShape deltaValue =
  reuseValidityFromRegistration
    maybeViewDigest
    residualShape
    (deTime deltaValue)
    (deScope deltaValue)
{-# INLINE reuseValidityFromDelta #-}

reuseValidityFromRegistration ::
  Maybe StableDigest128 ->
  ResidualShape ->
  RelationalCarrierTime ctx ->
  RelationalScope ->
  ReuseValidity
reuseValidityFromRegistration maybeViewDigest residualShape timeValue scopeValue =
  ReuseValidity
    { rvQuotientEpoch = relationalTimeQuotientEpoch timeValue,
      rvLiveEpoch = relationalTimeLiveEpoch timeValue,
      rvFrontierStamp = relationalTimeFrontierStamp timeValue,
      rvViewDigest = maybeViewDigest,
      rvResidualShape = residualShape,
      rvDependencyDigest = digestIntSet (scopeDeps scopeValue),
      rvTopoDigest = digestIntSet (scopeTopo scopeValue)
    }
{-# INLINE reuseValidityFromRegistration #-}

reuseValidityRequestFromTime ::
  Maybe StableDigest128 ->
  ResidualShape ->
  RelationalCarrierTime ctx ->
  ReuseValidityRequest
reuseValidityRequestFromTime maybeViewDigest residualShape timeValue =
  ReuseValidityRequest
    { rvrQuotientEpoch = relationalTimeQuotientEpoch timeValue,
      rvrLiveEpoch = relationalTimeLiveEpoch timeValue,
      rvrFrontierStamp = relationalTimeFrontierStamp timeValue,
      rvrViewDigest = maybeViewDigest,
      rvrResidualShape = residualShape
    }
{-# INLINE reuseValidityRequestFromTime #-}

reuseTemporalViewMatchesRequest ::
  ReuseValidityRequest ->
  ReuseValidity ->
  Bool
reuseTemporalViewMatchesRequest request validity =
  case (rvrViewDigest request, rvViewDigest validity) of
    (Just requestedDigest, Just registeredDigest) ->
      requestedDigest == registeredDigest
    _ ->
      rvrQuotientEpoch request == rvQuotientEpoch validity
        && rvrLiveEpoch request == rvLiveEpoch validity
        && rvrFrontierStamp request == rvFrontierStamp validity
        && rvrViewDigest request == rvViewDigest validity
{-# INLINE reuseTemporalViewMatchesRequest #-}

reuseExactValidityMatchesRequest ::
  ReuseValidityRequest ->
  ReuseValidity ->
  Bool
reuseExactValidityMatchesRequest request validity =
  reuseTemporalViewMatchesRequest request validity
    && rvrResidualShape request == rvResidualShape validity
{-# INLINE reuseExactValidityMatchesRequest #-}
