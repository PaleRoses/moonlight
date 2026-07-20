{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.Subscription
  ( QueryAtomSubscription (..),
    AtomSubscriptionError (..),
    buildAtomSubscribers,
  )
where
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( firstDuplicate,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryAtomId,
    SourceAtomId,
    queryAtomAsAtomId,
    sourceAtomKey,
  )
data QueryAtomSubscription = QueryAtomSubscription
  { qasSourceAtomId :: !SourceAtomId,
    qasQueryId :: !QueryId,
    qasQueryAtomId :: !QueryAtomId
  }
  deriving stock (Eq, Ord, Show, Read)
data AtomSubscriptionError
  = DuplicateAtomSubscription !QueryAtomSubscription
  deriving stock (Eq, Ord, Show, Read)
buildAtomSubscribers ::
  [QueryAtomSubscription] ->
  Either AtomSubscriptionError (IntMap [(QueryId, AtomId)])
buildAtomSubscribers subscriptions =
  case firstDuplicate subscriptions of
    Just duplicateSubscription ->
      Left (DuplicateAtomSubscription duplicateSubscription)
    Nothing ->
      Right
        ( IntMap.fromListWith
            (<>)
            [ ( sourceAtomKey (qasSourceAtomId subscription),
                [(qasQueryId subscription, queryAtomAsAtomId (qasQueryAtomId subscription))]
              )
            | subscription <- reverse subscriptions
            ]
        )
