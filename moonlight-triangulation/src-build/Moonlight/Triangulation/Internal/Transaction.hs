{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | The hidden publication boundary shared by persistent build-side edits.
--
-- The rank-two action can observe one mutable section, but neither the section
-- nor any site witness can escape it.  This is deliberately below the public
-- Session surface: a caller may compose public verbs, while an owner that has
-- just derived private evidence can interpret it without making that evidence
-- forgeable.
module Moonlight.Triangulation.Internal.Transaction
  ( runTransaction
  , runUnmeasuredTransaction
  ) where

import Control.Monad.ST (ST, runST)
import Moonlight.Triangulation.Dcel (numVertices)
import Moonlight.Triangulation.Internal.Capacity (ensureCapacity)
import Moonlight.Triangulation.Internal.Mutable
  ( MutableDcel
  , freezeTriangulation
  , halfEdgeCapacity
  , thawTriangulation
  , thawTriangulationDense
  )
import Moonlight.Triangulation.Internal.OperationState
  ( OperationState
  , freezeBuildStats
  , newOperationState
  )
import Moonlight.Triangulation.Internal.Paged (TransactionShape (..))
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types (BuildError, BuildStats)

-- | Reserve, thaw, interpret, and publish one transaction.
--
-- The physical section is selected by the operation that knows its edit
-- volume.  Refusal short-circuits before freezing, so a partially rewritten
-- mutable mesh cannot escape as a published triangulation.
runTransaction
  :: (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, BuildStats)
runTransaction = runTransactionWithReceipt freezeBuildStats
{-# INLINE runTransaction #-}

-- | Publish a transaction whose caller observes no instrumentation. Avoiding
-- the statistics fold matters for singleton constraint verbs: their public
-- result has no statistics field, so reading every counter would be dead work.
runUnmeasuredTransaction
  :: (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face)
runUnmeasuredTransaction mapBuildFailure shape triangulation additional action =
  fmap
    (\(result, frozen, ()) -> (result, frozen))
    ( runTransactionWithReceipt
        (const (pure ()))
        mapBuildFailure
        shape
        triangulation
        additional
        action
    )
{-# INLINE runUnmeasuredTransaction #-}

runTransactionWithReceipt
  :: (forall s. OperationState s -> ST s receipt)
  -> (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, receipt)
runTransactionWithReceipt freezeReceipt mapBuildFailure shape triangulation additional action = do
  let !currentCapacity = numVertices triangulation
      !requestedAdditional = max 0 additional
      !capacity
        | requestedAdditional > maxBound - currentCapacity = maxBound
        | otherwise = currentCapacity + requestedAdditional
  case ensureCapacity capacity of
    Left failure -> Left (mapBuildFailure failure)
    Right () -> pure ()
  runST $ do
    mutable <-
      case shape of
        DenseTransaction -> thawTriangulationDense capacity triangulation
        LocalTransaction -> thawTriangulation capacity triangulation
    operation <- newOperationState (halfEdgeCapacity mutable)
    outcome <- action mutable operation
    case outcome of
      Left refusal -> pure (Left refusal)
      Right value -> do
        frozenOutcome <- freezeTriangulation mutable
        case frozenOutcome of
          Left obstruction -> pure (Left (mapBuildFailure obstruction))
          Right frozen -> do
            receipt <- freezeReceipt operation
            pure (Right (value, frozen, receipt))
{-# INLINE runTransactionWithReceipt #-}
