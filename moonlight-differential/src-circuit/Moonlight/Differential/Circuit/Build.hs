{-# LANGUAGE GADTs #-}

-- | Circuit construction: the builder monad holds the mint monopoly on
-- handles, the fixpoint combinator delimits feedback scopes, and the seal
-- compiles every node to its kernel behind the region-typed existential.
module Moonlight.Differential.Circuit.Build
  ( CircuitBuilder,
    SealedCircuit (..),
    buildCircuit,
    withSealedCircuit,
    inputNode,
    mapNode,
    filterNode,
    flatMapNode,
    concatNodes,
    negateNode,
    differenceNodes,
    indexByNode,
    deindexNode,
    joinNodes,
    countByNode,
    aggregateNode,
    distinctNode,
    fixpointNode,
    foreignNode,
    foreignNode2,
  )
where

import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Proxy
  ( Proxy (..),
  )
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Core
  ( AdditiveGroup,
  )
import Moonlight.Differential.Algebra.ZSet
  ( ZSet,
  )
import Moonlight.Differential.Circuit.Carrier
  ( Circuit,
    CircuitDraft (..),
    ClosedScope (..),
    DraftNode (..),
    draftNodeParents,
    emptyCircuitDraft,
  )
import Moonlight.Differential.Circuit.Foreign
  ( ForeignKernel,
    ForeignKernel2,
  )
import Moonlight.Differential.Circuit.Handle
  ( IndexedNode (..),
    InputPort (..),
    Node (..),
  )
import Moonlight.Differential.Circuit.Kernel
  ( sealKernels,
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitBuildError (..),
    nodeId,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget,
  )

type CircuitBuilder :: Type -> Type -> Type -> Type -> Type
newtype CircuitBuilder s fault weight a = CircuitBuilder
  { runCircuitBuilder ::
      CircuitDraft s fault weight ->
      (a, CircuitDraft s fault weight)
  }

instance Functor (CircuitBuilder s fault weight) where
  fmap transform (CircuitBuilder run) =
    CircuitBuilder
      ( \draft ->
          case run draft of
            (result, advanced) ->
              (transform result, advanced)
      )

instance Applicative (CircuitBuilder s fault weight) where
  pure result =
    CircuitBuilder (\draft -> (result, draft))

  CircuitBuilder runTransform <*> CircuitBuilder runArgument =
    CircuitBuilder
      ( \draft ->
          case runTransform draft of
            (transform, afterTransform) ->
              case runArgument afterTransform of
                (argument, afterArgument) ->
                  (transform argument, afterArgument)
      )

instance Monad (CircuitBuilder s fault weight) where
  CircuitBuilder run >>= continue =
    CircuitBuilder
      ( \draft ->
          case run draft of
            (result, advanced) ->
              runCircuitBuilder (continue result) advanced
      )

mintNode :: DraftNode s fault weight -> CircuitBuilder s fault weight Int
mintNode node =
  CircuitBuilder
    ( \draft ->
        ( draftNextId draft,
          draft
            { draftNextId = draftNextId draft + 1,
              draftNodes = IntMap.insert (draftNextId draft) node (draftNodes draft)
            }
        )
    )

peekNextId :: CircuitBuilder s fault weight Int
peekNextId =
  CircuitBuilder (\draft -> (draftNextId draft, draft))

recordScope :: ClosedScope -> CircuitBuilder s fault weight ()
recordScope scope =
  CircuitBuilder
    (\draft -> ((), draft {draftClosedScopes = scope : draftClosedScopes draft}))

inputNode ::
  forall value s fault weight.
  Ord value =>
  CircuitBuilder s fault weight (InputPort s value, Node s value)
inputNode = do
  key <- mintNode (DraftInput (Proxy :: Proxy value))
  pure (InputPort key, Node key)

mapNode ::
  Ord b =>
  (a -> b) ->
  Node s a ->
  CircuitBuilder s fault weight (Node s b)
mapNode transform parent =
  Node <$> mintNode (DraftMap transform parent)

filterNode ::
  Ord a =>
  (a -> Bool) ->
  Node s a ->
  CircuitBuilder s fault weight (Node s a)
filterNode keep parent =
  Node <$> mintNode (DraftFilter keep parent)

flatMapNode ::
  Ord b =>
  (a -> [b]) ->
  Node s a ->
  CircuitBuilder s fault weight (Node s b)
flatMapNode transform parent =
  Node <$> mintNode (DraftFlatMap transform parent)

concatNodes ::
  Ord a =>
  Node s a ->
  Node s a ->
  CircuitBuilder s fault weight (Node s a)
concatNodes left right =
  Node <$> mintNode (DraftConcat left right)

negateNode ::
  Ord a =>
  Node s a ->
  CircuitBuilder s fault weight (Node s a)
negateNode parent =
  Node <$> mintNode (DraftNegate parent)

differenceNodes ::
  Ord a =>
  Node s a ->
  Node s a ->
  CircuitBuilder s fault weight (Node s a)
differenceNodes left right =
  concatNodes left =<< negateNode right

indexByNode ::
  (Ord key, Ord a) =>
  (a -> key) ->
  Node s a ->
  CircuitBuilder s fault weight (IndexedNode s key a)
indexByNode keyOf parent =
  IndexedNode <$> mintNode (DraftIndexBy keyOf parent)

deindexNode ::
  Ord b =>
  (key -> a -> b) ->
  IndexedNode s key a ->
  CircuitBuilder s fault weight (Node s b)
deindexNode project parent =
  Node <$> mintNode (DraftDeindex project parent)

joinNodes ::
  (Ord key, Ord a, Ord b) =>
  IndexedNode s key a ->
  IndexedNode s key b ->
  CircuitBuilder s fault weight (Node s (key, a, b))
joinNodes left right =
  Node <$> mintNode (DraftJoin left right)

countByNode ::
  Ord key =>
  IndexedNode s key a ->
  CircuitBuilder s fault weight (Node s key)
countByNode parent =
  Node <$> mintNode (DraftCountBy parent)

aggregateNode ::
  (Ord key, Ord a, Ord reduced) =>
  (ZSet a weight -> reduced) ->
  IndexedNode s key a ->
  CircuitBuilder s fault weight (Node s (key, reduced))
aggregateNode reducer parent =
  Node <$> mintNode (DraftAggregate reducer parent)

distinctNode ::
  Ord a =>
  Node s a ->
  CircuitBuilder s fault weight (Node s a)
distinctNode parent =
  Node <$> mintNode (DraftDistinct parent)

fixpointNode ::
  forall a s fault weight.
  Ord a =>
  SemiNaiveBudget ->
  Node s a ->
  (Node s a -> CircuitBuilder s fault weight (Node s a)) ->
  CircuitBuilder s fault weight (Node s a)
fixpointNode budget seed body = do
  feedbackKey <- mintNode (DraftFeedback (Proxy :: Proxy a))
  result <- body (Node feedbackKey)
  bodyEnd <- peekNextId
  fixpointKey <-
    mintNode (DraftFixpoint budget seed (Node feedbackKey :: Node s a) result)
  recordScope
    ClosedScope
      { closedScopeFixpointId = fixpointKey,
        closedScopeFeedbackId = feedbackKey,
        closedScopeResultId = nodeId result,
        closedScopeSpan =
          IntSet.fromDistinctAscList [feedbackKey + 1 .. bodyEnd - 1]
      }
  pure (Node fixpointKey)

foreignNode ::
  Ord b =>
  ForeignKernel fault weight a b ->
  Node s a ->
  CircuitBuilder s fault weight (Node s b)
foreignNode kernel parent =
  Node <$> mintNode (DraftForeign kernel parent)

foreignNode2 ::
  Ord c =>
  ForeignKernel2 fault weight a b c ->
  Node s a ->
  Node s b ->
  CircuitBuilder s fault weight (Node s c)
foreignNode2 kernel left right =
  Node <$> mintNode (DraftForeign2 kernel left right)

type SealedCircuit :: Type -> Type -> (Type -> Type) -> Type
data SealedCircuit fault weight ports = forall s.
  SealedCircuit
    !(Circuit s fault weight)
    !(ports s)

withSealedCircuit ::
  SealedCircuit fault weight ports ->
  (forall s. Circuit s fault weight -> ports s -> r) ->
  r
withSealedCircuit (SealedCircuit circuit ports) consume =
  consume circuit ports

data SealScope

buildCircuit ::
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  (forall s. CircuitBuilder s fault weight (ports s)) ->
  Either CircuitBuildError (SealedCircuit fault weight ports)
buildCircuit builder =
  case runCircuitBuilder (builder @SealScope) emptyCircuitDraft of
    (ports, draft) -> do
      validateParents (draftNodes draft)
      validateScopes (draftNodes draft) (draftClosedScopes draft)
      circuit <- sealKernels (draftNodes draft) (draftClosedScopes draft)
      pure
        ( SealedCircuit
            circuit
            ports
        )

validateParents ::
  IntMap (DraftNode s fault weight) ->
  Either CircuitBuildError ()
validateParents nodes =
  traverse_ validateNode (IntMap.toAscList nodes)
  where
    validateNode (selfId, node) =
      traverse_ (validateParent selfId) (draftNodeParents node)

    validateParent selfId parentId
      | IntMap.member parentId nodes =
          Right ()
      | otherwise =
          Left
            CircuitBuildMissingParent
              { missingParentConsumerId = selfId,
                missingParentId = parentId
              }

validateScopes ::
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  Either CircuitBuildError ()
validateScopes nodes =
  traverse_ validateScope
  where
    validateScope scope =
      traverse_ (checkNode scope) (IntMap.toAscList nodes)

    checkNode scope (offenderId, node)
      | offenderId == closedScopeFixpointId scope =
          Right ()
      | IntSet.member offenderId (closedScopeSpan scope) =
          Right ()
      | otherwise =
          traverse_ (checkParent scope offenderId) (draftNodeParents node)

    checkParent scope offenderId parentId
      | parentId == closedScopeFeedbackId scope =
          Left
            CircuitFeedbackEscapesScope
              { escapeFixpointId = closedScopeFixpointId scope,
                escapeOffendingId = offenderId
              }
      | IntSet.member parentId (closedScopeSpan scope) =
          Left
            CircuitFixpointBodyEscapesScope
              { escapeFixpointId = closedScopeFixpointId scope,
                escapeReferencedId = parentId,
                escapeOffendingId = offenderId
              }
      | otherwise =
          Right ()
