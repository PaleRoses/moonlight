{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Moonlight.Flow.Runtime.Topology.Site.Types
  ( Shard (..),
    TouchKey (..),
    CarrierFamily,
    ContextClassId (..),
    ContextClass (..),
    ContextEGraph (..),
    ContextMergeError (..),
    emptyContextEGraph,
    insertContextClass,
    mergeContextClasses,
    canonicalContextOf,
    GeneratedQueryBinding (..),
    GeneratedContextShape (..),
    GeneratedRoutingSource (..),
    GeneratedRoutePatch (..),
    CarrierMoves (..),
    CarrierMoveError (..),
    emptyGeneratedRoutingSource,
    emptyGeneratedRoutePatch,
    generatedRoutingSourceCarriers,
    emptyCarrierMoves,
    normalizeCarrierMoves,
    removeContextCarrierMoves,
    carrierMovesTarget,
    carrierMovesRetargetPairs,
    carrierMovesTargetSet,
    carrierMovesTargetMapWith,
    MorphismKey (..),
    GeneratedMorphism (..),
    GeneratedCover (..),
    ContextMergePlan (..),
    GeneratedSiteState (..),
    GeneratedSiteValidationError (..),
    GeneratedSitePatchError (..),
    emptyGeneratedSiteState,
    generatedContextShapeDigest,
    generatedSiteDigest,
    refreshGeneratedSiteDigest,
    validateGeneratedContextShape,
    validateGeneratedContextShapeWithPrograms,
    validateGeneratedContextRouting,
  )
where
import Control.Monad
  ( foldM,
    unless,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    DerivedCarrierId (..),
    QueryCarrierNode (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( TouchKey (..),
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierFamilyTargetContext,
    carrierFamilyMembers,
    carrierFamilyProp,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
newtype ContextClassId = ContextClassId Int
  deriving stock (Eq, Ord, Show, Read)
data ContextClass ctx = ContextClass
  { ccRepresentative :: !ctx,
    ccMembers :: !(Set ctx),
    ccDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)
data ContextEGraph ctx = ContextEGraph
  { cegParentByContext :: !(Map ctx ctx),
    cegClasses :: !(Map ctx (ContextClass ctx)),
    cegDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)
data ContextMergeError ctx
  = ContextMergeMissingLeft !ctx
  | ContextMergeMissingRight !ctx
  | ContextMergeCanonicalNotInMergedClass !ctx !ctx !ctx
  deriving stock (Eq, Ord, Show)
emptyContextEGraph :: ContextEGraph ctx
emptyContextEGraph =
  ContextEGraph
    { cegParentByContext = Map.empty,
      cegClasses = Map.empty,
      cegDigest = stableDigest128 [0x636567, 0]
    }
{-# INLINE emptyContextEGraph #-}
insertContextClass ::
  Ord ctx =>
  ctx ->
  ContextEGraph ctx ->
  ContextEGraph ctx
insertContextClass contextValue graph
  | Map.member contextValue (cegParentByContext graph) =
      graph
  | otherwise =
      refreshContextEGraphDigest
        graph
          { cegParentByContext =
              Map.insert contextValue contextValue (cegParentByContext graph),
            cegClasses =
              Map.insert
                contextValue
                (singletonContextClass contextValue)
                (cegClasses graph)
          }
{-# INLINE insertContextClass #-}
singletonContextClass ::
  Ord ctx =>
  ctx ->
  ContextClass ctx
singletonContextClass contextValue =
  let members =
        Set.singleton contextValue
   in ContextClass
        { ccRepresentative = contextValue,
          ccMembers = members,
          ccDigest = contextClassDigest contextValue members
        }
{-# INLINE singletonContextClass #-}
mergeContextClasses ::
  Ord ctx =>
  ctx ->
  ctx ->
  ctx ->
  ContextEGraph ctx ->
  Either (ContextMergeError ctx) (ContextEGraph ctx)
mergeContextClasses oldA oldB canonical graph0 = do
  classA <-
    case contextClassOf oldA graph0 of
      Nothing -> Left (ContextMergeMissingLeft oldA)
      Just value -> Right value
  classB <-
    case contextClassOf oldB graph0 of
      Nothing -> Left (ContextMergeMissingRight oldB)
      Just value -> Right value
  let members =
        Set.insert canonical $
          Set.union (ccMembers classA) (ccMembers classB)
  unless (Set.member canonical members) $
    Left (ContextMergeCanonicalNotInMergedClass oldA oldB canonical)
  let classValue =
        ContextClass
          { ccRepresentative = canonical,
            ccMembers = members,
            ccDigest = contextClassDigest canonical members
          }
      deleteOld =
        Map.delete (ccRepresentative classA)
          . Map.delete (ccRepresentative classB)
      parentByContext =
        Set.foldl'
          (\acc member -> Map.insert member canonical acc)
          (cegParentByContext graph0)
          members
  pure $
    refreshContextEGraphDigest
      graph0
        { cegParentByContext = parentByContext,
          cegClasses = Map.insert canonical classValue (deleteOld (cegClasses graph0))
        }
{-# INLINE mergeContextClasses #-}
contextClassOf ::
  Ord ctx =>
  ctx ->
  ContextEGraph ctx ->
  Maybe (ContextClass ctx)
contextClassOf contextValue graph =
  Map.lookup (canonicalContextOf contextValue graph) (cegClasses graph)
{-# INLINE contextClassOf #-}
canonicalContextOf ::
  Ord ctx =>
  ctx ->
  ContextEGraph ctx ->
  ctx
canonicalContextOf contextValue graph =
  go Set.empty contextValue
  where
    go !seen !current
      | Set.member current seen =
          current
      | otherwise =
          case Map.lookup current (cegParentByContext graph) of
            Just parent
              | parent /= current ->
                  go (Set.insert current seen) parent
            _ ->
              current
{-# INLINE canonicalContextOf #-}
refreshContextEGraphDigest ::
  Ord ctx =>
  ContextEGraph ctx ->
  ContextEGraph ctx
refreshContextEGraphDigest graph =
  graph {cegDigest = contextEGraphDigest graph}
{-# INLINE refreshContextEGraphDigest #-}
contextClassDigest ::
  Ord ctx =>
  ctx ->
  Set ctx ->
  StableDigest128
contextClassDigest representative members =
  let ranks =
        ranksOf (Set.toAscList members)
      repRank =
        Map.findWithDefault maxBound representative ranks
   in stableDigest128
        ( [0x63636c617373, wordOfInt repRank, wordOfInt (Set.size members)]
            <> fmap wordOfInt [0 .. Set.size members - 1]
        )
{-# INLINE contextClassDigest #-}
contextEGraphDigest ::
  Ord ctx =>
  ContextEGraph ctx ->
  StableDigest128
contextEGraphDigest graph =
  let contexts =
        Set.toAscList $
          Map.keysSet (cegParentByContext graph)
            <> foldMap ccMembers (Map.elems (cegClasses graph))
      ranks =
        ranksOf contexts
      classWords classValue =
        [0x01, wordOfInt (rankOf (ccRepresentative classValue) ranks)]
          <> stableDigestWords (ccDigest classValue)
          <> fmap wordOfInt [0 .. Set.size (ccMembers classValue) - 1]
   in stableDigest128
        ( [0x636567, wordOfInt (Map.size (cegClasses graph))]
            <> foldMap classWords (Map.elems (cegClasses graph))
        )
{-# INLINE contextEGraphDigest #-}
data GeneratedQueryBinding prop = GeneratedQueryBinding
  { gqbProp :: !(PropositionKey prop),
    gqbProjectShard :: !Shard
  }
  deriving stock (Eq, Show)
data GeneratedContextShape prop = GeneratedContextShape
  { gcsShapeDigest :: !StableDigest128,
    gcsQueryBindings :: !(Map QueryId (GeneratedQueryBinding prop)),
    gcsIndexShardsByProp :: !(Map (PropositionKey prop) Shard)
  }
  deriving stock (Eq, Show)
data GeneratedRoutingSource ctx prop = GeneratedRoutingSource
  { grsAtomSubscribers :: !(IntMap.IntMap [(QueryId, AtomId)]),
    grsCarrierTouches :: !(Map TouchKey (Set (CarrierAddr ctx Carrier prop))),
    grsRestrictShardsByCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard),
    grsIndexShardsByCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard)
  }
  deriving stock (Eq, Show)
emptyGeneratedRoutingSource :: GeneratedRoutingSource ctx prop
emptyGeneratedRoutingSource =
  GeneratedRoutingSource
    { grsAtomSubscribers = IntMap.empty,
      grsCarrierTouches = Map.empty,
      grsRestrictShardsByCarrier = Map.empty,
      grsIndexShardsByCarrier = Map.empty
    }
{-# INLINE emptyGeneratedRoutingSource #-}
data GeneratedRoutePatch ctx prop = GeneratedRoutePatch
  { grpCarrierTouches :: !(Map TouchKey (Set (CarrierAddr ctx Carrier prop))),
    grpRestrictShardsByCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard),
    grpIndexShardsByCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard)
  }
  deriving stock (Eq, Show)
emptyGeneratedRoutePatch :: GeneratedRoutePatch ctx prop
emptyGeneratedRoutePatch =
  GeneratedRoutePatch
    { grpCarrierTouches = Map.empty,
      grpRestrictShardsByCarrier = Map.empty,
      grpIndexShardsByCarrier = Map.empty
    }
{-# INLINE emptyGeneratedRoutePatch #-}
generatedRoutingSourceCarriers ::
  (Ord ctx, Ord prop) =>
  GeneratedRoutingSource ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
generatedRoutingSourceCarriers source =
  Set.unions
    [ Set.unions (Map.elems (grsCarrierTouches source)),
      Map.keysSet (grsRestrictShardsByCarrier source),
      Map.keysSet (grsIndexShardsByCarrier source)
    ]
{-# INLINE generatedRoutingSourceCarriers #-}
data CarrierMoves addr = CarrierMoves
  { cmRetarget :: !(Map addr addr),
    cmEvict :: !(Set addr)
  }
  deriving stock (Eq, Show)
data CarrierMoveError addr
  = CarrierMoveCycle !(Set addr)
  deriving stock (Eq, Show)
emptyCarrierMoves :: CarrierMoves addr
emptyCarrierMoves =
  CarrierMoves
    { cmRetarget = Map.empty,
      cmEvict = Set.empty
    }
{-# INLINE emptyCarrierMoves #-}
normalizeCarrierMoves ::
  forall addr.
  Ord addr =>
  CarrierMoves addr ->
  Either (CarrierMoveError addr) (CarrierMoves addr)
normalizeCarrierMoves moves0 = do
  (_memo, retarget1, evict1) <-
    foldM
      normalizeOne
      (Map.empty, Map.empty, cmEvict moves0)
      (Map.keys (cmRetarget moves0))
  pure
    CarrierMoves
      { cmRetarget = retarget1,
        cmEvict = evict1
      }
  where
    normalizeOne ::
      (Map addr (Maybe addr), Map addr addr, Set addr) ->
      addr ->
      Either
        (CarrierMoveError addr)
        (Map addr (Maybe addr), Map addr addr, Set addr)
    normalizeOne (!memo0, !retargetAcc, !evictAcc) source
      | Set.member source evictAcc =
          Right (memo0, retargetAcc, evictAcc)
      | otherwise = do
          (memo1, maybeTarget) <-
            chase evictAcc Set.empty memo0 source
          case maybeTarget of
            Nothing ->
              Right (memo1, retargetAcc, Set.insert source evictAcc)
            Just target
              | target == source ->
                  Right (memo1, retargetAcc, evictAcc)
              | otherwise ->
                  Right
                    ( memo1,
                      Map.insert source target retargetAcc,
                      evictAcc
                    )
    chase ::
      Set addr ->
      Set addr ->
      Map addr (Maybe addr) ->
      addr ->
      Either (CarrierMoveError addr) (Map addr (Maybe addr), Maybe addr)
    chase evict seen memo addr
      | Set.member addr evict =
          Right (Map.insert addr Nothing memo, Nothing)
      | Set.member addr seen =
          Left (CarrierMoveCycle (Set.insert addr seen))
      | otherwise =
          case Map.lookup addr memo of
            Just cached ->
              Right (memo, cached)
            Nothing ->
              case Map.lookup addr (cmRetarget moves0) of
                Nothing ->
                  Right (Map.insert addr (Just addr) memo, Just addr)
                Just next
                  | next == addr ->
                      Right (Map.insert addr (Just addr) memo, Just addr)
                  | otherwise -> do
                      (memo1, target) <-
                        chase evict (Set.insert addr seen) memo next
                      Right (Map.insert addr target memo1, target)
{-# INLINE normalizeCarrierMoves #-}
carrierMovesTarget ::
  Ord addr =>
  CarrierMoves addr ->
  addr ->
  Maybe addr
carrierMovesTarget moves addr
  | Set.member addr (cmEvict moves) =
      Nothing
  | otherwise =
      Just (Map.findWithDefault addr addr (cmRetarget moves))
{-# INLINE carrierMovesTarget #-}
carrierMovesRetargetPairs ::
  CarrierMoves addr ->
  [(addr, addr)]
carrierMovesRetargetPairs =
  Map.toAscList . cmRetarget
{-# INLINE carrierMovesRetargetPairs #-}
carrierMovesTargetSet ::
  Ord addr =>
  CarrierMoves addr ->
  Set addr ->
  Set addr
carrierMovesTargetSet moves =
  Set.foldl'
    ( \acc addr ->
        case carrierMovesTarget moves addr of
          Nothing ->
            acc
          Just target ->
            Set.insert target acc
    )
    Set.empty
{-# INLINE carrierMovesTargetSet #-}
carrierMovesTargetMapWith ::
  (Ord addr, Eq value) =>
  (addr -> value -> value -> err) ->
  CarrierMoves addr ->
  Map addr value ->
  Either err (Map addr value)
carrierMovesTargetMapWith mkCollision moves =
  Map.foldlWithKey'
    ( \result addr value -> do
        acc <- result
        case carrierMovesTarget moves addr of
          Nothing ->
            Right acc
          Just target ->
            case Map.lookup target acc of
              Nothing ->
                Right (Map.insert target value acc)
              Just oldValue
                | oldValue == value ->
                    Right acc
                | otherwise ->
                    Left (mkCollision target oldValue value)
    )
    (Right Map.empty)
{-# INLINE carrierMovesTargetMapWith #-}
removeContextCarrierMoves ::
  (Ord ctx, Ord prop) =>
  ctx ->
  Set (CarrierAddr ctx Carrier prop) ->
  GeneratedRoutingSource ctx prop ->
  CarrierMoves (CarrierAddr ctx Carrier prop)
removeContextCarrierMoves contextValue explicitEvictions source =
  emptyCarrierMoves
    { cmEvict =
        explicitEvictions
          <> Set.filter
            ((== contextValue) . caContext)
            (generatedRoutingSourceCarriers source)
    }
{-# INLINE removeContextCarrierMoves #-}
data MorphismKey ctx = MorphismKey
  { mkSourceContext :: !ctx,
    mkTargetContext :: !ctx,
    mkMorphismDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)
data GeneratedMorphism ctx prop = GeneratedMorphism
  { gmKey :: !(MorphismKey ctx),
    gmRestrictionEdges :: !(Set (RestrictKey ctx Carrier prop))
  }
  deriving stock (Eq, Show)
data GeneratedCover ctx prop = GeneratedCover
  { gcDirtyTopo :: !IntSet
  }
  deriving stock (Eq, Show)
data ContextMergePlan ctx prop = ContextMergePlan
  { cmpOldA :: !ctx,
    cmpOldB :: !ctx,
    cmpCanonical :: !ctx,
    cmpDirtyTopo :: !IntSet,
    cmpCarrierMoves :: !(CarrierMoves (CarrierAddr ctx Carrier prop))
  }
  deriving stock (Eq, Show)
data GeneratedSiteState ctx prop = GeneratedSiteState
  { gssContexts :: !(Map ctx (GeneratedContextShape prop)),
    gssContextClasses :: !(ContextEGraph ctx),
    gssMorphisms :: !(Map (MorphismKey ctx) (GeneratedMorphism ctx prop)),
    gssCovers :: !(Map (CarrierFamily ctx Carrier prop) (GeneratedCover ctx prop)),
    gssRouteSource :: !(GeneratedRoutingSource ctx prop),
    gssPlanObjects :: !(Map StableDigest128 ctx),
    gssDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)
data GeneratedSiteValidationError ctx prop
  = GeneratedContextMissingFactorProgram !ctx !QueryId
  | GeneratedContextBindingPropUnrouted !ctx !QueryId !(PropositionKey prop)
  | GeneratedContextShapeDigestMismatch !ctx !StableDigest128 !StableDigest128
  | GeneratedContextRouteCollision !ctx !QueryId !ctx
  | GeneratedContextRouteShardCollision !ctx !(PropositionKey prop) !Shard !Shard
  deriving stock (Eq, Show)
data GeneratedSitePatchError ctx prop
  = GeneratedSiteContextAlreadyExists !ctx
  | GeneratedSiteContextMissing !ctx
  | GeneratedSiteContextShapeInvalid ![GeneratedSiteValidationError ctx prop]
  | GeneratedSiteMorphismEndpointMissing !(MorphismKey ctx)
  | GeneratedSiteMorphismConflict
      !(MorphismKey ctx)
      !(GeneratedMorphism ctx prop)
      !(GeneratedMorphism ctx prop)
  | GeneratedSiteMorphismRouteShardCollision
      !(CarrierAddr ctx Carrier prop)
      !Shard
      !Shard
  | GeneratedSiteCoverAlreadyExists !(CarrierFamily ctx Carrier prop)
  | GeneratedSiteMergeFailed !(ContextMergeError ctx)
  | GeneratedSiteMergeQueryCollision !QueryId
  | GeneratedSiteMergePropShardCollision !(PropositionKey prop) !Shard !Shard
  | GeneratedSiteCarrierMoveInvalid !(CarrierMoveError (CarrierAddr ctx Carrier prop))
  | GeneratedSiteCarrierMoveShardCollision !(CarrierAddr ctx Carrier prop) !Shard !Shard
  deriving stock (Eq, Show)
emptyGeneratedSiteState ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop
emptyGeneratedSiteState =
  refreshGeneratedSiteDigest
    GeneratedSiteState
      { gssContexts = Map.empty,
        gssContextClasses = emptyContextEGraph,
        gssMorphisms = Map.empty,
        gssCovers = Map.empty,
        gssRouteSource = emptyGeneratedRoutingSource,
        gssPlanObjects = Map.empty,
        gssDigest = StableDigest128 0 0
      }
{-# INLINE emptyGeneratedSiteState #-}
validateGeneratedContextShape ::
  Ord prop =>
  ctx ->
  GeneratedContextShape prop ->
  Either [GeneratedSiteValidationError ctx prop] ()
validateGeneratedContextShape contextValue shape =
  finishErrors (generatedContextShapeErrors contextValue shape)
{-# INLINE validateGeneratedContextShape #-}

validateGeneratedContextShapeWithPrograms ::
  Ord prop =>
  Map QueryId program ->
  ctx ->
  GeneratedContextShape prop ->
  Either [GeneratedSiteValidationError ctx prop] ()
validateGeneratedContextShapeWithPrograms factorPrograms contextValue shape =
  finishErrors
    ( missingProgramErrors
        <> generatedContextShapeErrors contextValue shape
    )
  where
    bindings =
      Map.toAscList (gcsQueryBindings shape)
    missingProgramErrors =
      [ GeneratedContextMissingFactorProgram contextValue queryId
      | (queryId, _binding) <- bindings,
        not (Map.member queryId factorPrograms)
      ]
{-# INLINE validateGeneratedContextShapeWithPrograms #-}

generatedContextShapeErrors ::
  Ord prop =>
  ctx ->
  GeneratedContextShape prop ->
  [GeneratedSiteValidationError ctx prop]
generatedContextShapeErrors contextValue shape =
  propRouteErrors <> digestErrors
  where
    bindings =
      Map.toAscList (gcsQueryBindings shape)
    propRouteErrors =
      [ GeneratedContextBindingPropUnrouted contextValue queryId (gqbProp binding)
      | (queryId, binding) <- bindings,
        not (Map.member (gqbProp binding) (gcsIndexShardsByProp shape))
      ]
    expectedDigest =
      generatedContextShapeDigest shape
    digestErrors =
      [ GeneratedContextShapeDigestMismatch contextValue expectedDigest (gcsShapeDigest shape)
      | expectedDigest /= gcsShapeDigest shape
      ]
{-# INLINE generatedContextShapeErrors #-}
validateGeneratedContextRouting ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  ctx ->
  GeneratedContextShape prop ->
  Either [GeneratedSiteValidationError ctx prop] ()
validateGeneratedContextRouting site contextValue shape =
  finishErrors (queryCollisions <> propCollisions)
  where
    existingContextByQuery =
      Map.fromList
        [ (queryId, existingContext)
        | (existingContext, existingShape) <- Map.toAscList (gssContexts site),
          queryId <- Map.keys (gcsQueryBindings existingShape)
        ]
    existingIndexByContextProp =
      Map.fromList
        [ ((existingContext, propKey), shard)
        | (existingContext, existingShape) <- Map.toAscList (gssContexts site),
          (propKey, shard) <- Map.toAscList (gcsIndexShardsByProp existingShape)
        ]
    queryCollisions =
      [ GeneratedContextRouteCollision contextValue queryId routedContext
      | queryId <- Map.keys (gcsQueryBindings shape),
        Just routedContext <- [Map.lookup queryId existingContextByQuery],
        routedContext /= contextValue
      ]
    propCollisions =
      [ GeneratedContextRouteShardCollision contextValue propKey oldShard newShard
      | (propKey, newShard) <- Map.toAscList (gcsIndexShardsByProp shape),
        Just oldShard <- [Map.lookup (contextValue, propKey) existingIndexByContextProp],
        oldShard /= newShard
      ]
{-# INLINE validateGeneratedContextRouting #-}
generatedContextShapeDigest ::
  Ord prop =>
  GeneratedContextShape prop ->
  StableDigest128
generatedContextShapeDigest shape =
  let propRanks =
        ranksOf . Set.toAscList $
          Map.keysSet (gcsIndexShardsByProp shape)
            <> Set.fromList
              [ gqbProp binding
              | binding <- Map.elems (gcsQueryBindings shape)
              ]
      bindingWords (queryId, binding) =
        [ 0x01,
          wordOfInt (queryIdKey queryId),
          wordOfInt (rankOf (gqbProp binding) propRanks),
          wordOfInt (shardKey (gqbProjectShard binding))
        ]
      indexWords (propKey, shard) =
        [0x02, wordOfInt (rankOf propKey propRanks), wordOfInt (shardKey shard)]
   in stableDigest128
        ( [0x67737368617065, wordOfInt (Map.size (gcsQueryBindings shape))]
            <> foldMap bindingWords (Map.toAscList (gcsQueryBindings shape))
            <> foldMap indexWords (Map.toAscList (gcsIndexShardsByProp shape))
        )
{-# INLINE generatedContextShapeDigest #-}
generatedSiteDigest ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  StableDigest128
generatedSiteDigest site =
  let routingSource =
        gssRouteSource site
      routingCarriers =
        generatedRoutingSourceCarriers routingSource
      morphismCarriers =
        generatedMorphismCarriers (gssMorphisms site)
      contextRanks =
        ranksOf . Set.toAscList $
          Map.keysSet (gssContexts site)
            <> foldMap ccMembers (Map.elems (cegClasses (gssContextClasses site)))
            <> Set.fromList (Map.elems (gssPlanObjects site))
            <> Set.fromList [caContext addr | addr <- Set.toAscList routingCarriers]
            <> Set.fromList [caContext addr | addr <- Set.toAscList morphismCarriers]
      propRanks =
        generatedSitePropRanks site
      contextWords (contextValue, shape) =
        [0x10, wordOfInt (rankOf contextValue contextRanks)]
          <> stableDigestWords (gcsShapeDigest shape)
      classWords =
        stableDigestWords (cegDigest (gssContextClasses site))
      morphismWords (morphismKey, morphism) =
        morphismKeyWords contextRanks morphismKey
          <> [0x21, wordOfInt (Set.size (gmRestrictionEdges morphism))]
          <> foldMap
            (restrictKeyWords contextRanks propRanks)
            (Set.toAscList (gmRestrictionEdges morphism))
      coverWords (family, coverValue) =
        carrierFamilyWords contextRanks propRanks family
          <> intSetWords (gcDirtyTopo coverValue)
      planObjectWords (planDigest, contextValue) =
        [0x40, wordOfInt (rankOf contextValue contextRanks)]
          <> stableDigestWords planDigest
   in stableDigest128
        ( [0x677373697465]
            <> classWords
            <> foldMap contextWords (Map.toAscList (gssContexts site))
            <> foldMap morphismWords (Map.toAscList (gssMorphisms site))
            <> foldMap coverWords (Map.toAscList (gssCovers site))
            <> generatedRoutingSourceWords contextRanks propRanks routingSource
            <> foldMap planObjectWords (Map.toAscList (gssPlanObjects site))
        )
{-# INLINE generatedSiteDigest #-}
refreshGeneratedSiteDigest ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  GeneratedSiteState ctx prop
refreshGeneratedSiteDigest site =
  site {gssDigest = generatedSiteDigest site}
{-# INLINE refreshGeneratedSiteDigest #-}
generatedSitePropRanks ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  Map (PropositionKey prop) Int
generatedSitePropRanks site =
  ranksOf . Set.toAscList $
    foldMap (Map.keysSet . gcsIndexShardsByProp) (Map.elems (gssContexts site))
      <> Set.fromList
        [ gqbProp binding
        | shape <- Map.elems (gssContexts site),
          binding <- Map.elems (gcsQueryBindings shape)
        ]
      <> Set.fromList
        [ carrierFamilyProp family
        | family <- Map.keys (gssCovers site)
        ]
      <> Set.fromList
        [ caProp addr
        | addr <- Set.toAscList (generatedRoutingSourceCarriers (gssRouteSource site))
        ]
      <> Set.fromList
        [ caProp addr
        | addr <- Set.toAscList (generatedMorphismCarriers (gssMorphisms site))
        ]
{-# INLINE generatedSitePropRanks #-}
generatedMorphismCarriers ::
  (Ord ctx, Ord prop) =>
  Map (MorphismKey ctx) (GeneratedMorphism ctx prop) ->
  Set (CarrierAddr ctx Carrier prop)
generatedMorphismCarriers morphisms =
  Set.unions
    [ Set.fromList
        [ rkSource restrictKey,
          rkTarget restrictKey
        ]
    | morphism <- Map.elems morphisms,
      restrictKey <- Set.toAscList (gmRestrictionEdges morphism)
    ]
{-# INLINE generatedMorphismCarriers #-}
generatedRoutingSourceWords ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  Map ctx Int ->
  Map (PropositionKey prop) Int ->
  GeneratedRoutingSource ctx prop ->
  [Word64]
generatedRoutingSourceWords contextRanks propRanks source =
  [0x80, wordOfInt (IntMap.size (grsAtomSubscribers source))]
    <> foldMap atomSubscriberEntryWords (IntMap.toAscList (grsAtomSubscribers source))
    <> [0x81, wordOfInt (Map.size (grsCarrierTouches source))]
    <> foldMap touchEntryWords (Map.toAscList (grsCarrierTouches source))
    <> [0x82, wordOfInt (Map.size (grsRestrictShardsByCarrier source))]
    <> foldMap (carrierShardEntryWords 0x83) (Map.toAscList (grsRestrictShardsByCarrier source))
    <> [0x84, wordOfInt (Map.size (grsIndexShardsByCarrier source))]
    <> foldMap (carrierShardEntryWords 0x85) (Map.toAscList (grsIndexShardsByCarrier source))
  where
    atomSubscriberEntryWords :: (Int, [(QueryId, AtomId)]) -> [Word64]
    atomSubscriberEntryWords (atomKey, subscribers) =
      [0x87, wordOfInt atomKey, wordOfInt (length subscribers)]
        <> foldMap subscriberWords subscribers
    subscriberWords :: (QueryId, AtomId) -> [Word64]
    subscriberWords (queryId, atomId) =
      [0x88, wordOfInt (queryIdKey queryId), wordOfInt (atomIdKey atomId)]
    touchEntryWords :: (TouchKey, Set (CarrierAddr ctx Carrier prop)) -> [Word64]
    touchEntryWords (touchKey, addrs) =
      [0x89]
        <> touchKeyWords touchKey
        <> [wordOfInt (Set.size addrs)]
        <> foldMap
          (carrierAddrWords contextRanks propRanks)
          (Set.toAscList addrs)
    carrierShardEntryWords :: Word64 -> (CarrierAddr ctx Carrier prop, Shard) -> [Word64]
    carrierShardEntryWords tag (addr, shard) =
      [tag]
        <> carrierAddrWords contextRanks propRanks addr
        <> [wordOfInt (shardKey shard)]
{-# INLINE generatedRoutingSourceWords #-}
touchKeyWords :: TouchKey -> [Word64]
touchKeyWords touchKey =
  case touchKey of
    TouchAtom atomKey ->
      [0x01, wordOfInt atomKey]
    TouchDep depKey ->
      [0x02, wordOfInt depKey]
    TouchTopo topoKey ->
      [0x03, wordOfInt topoKey]
{-# INLINE touchKeyWords #-}
morphismKeyWords ::
  Ord ctx =>
  Map ctx Int ->
  MorphismKey ctx ->
  [Word64]
morphismKeyWords contextRanks key =
  [ 0x20,
    wordOfInt (rankOf (mkSourceContext key) contextRanks),
    wordOfInt (rankOf (mkTargetContext key) contextRanks)
  ]
    <> stableDigestWords (mkMorphismDigest key)
{-# INLINE morphismKeyWords #-}
restrictKeyWords ::
  (Ord ctx, Ord prop) =>
  Map ctx Int ->
  Map (PropositionKey prop) Int ->
  RestrictKey ctx Carrier prop ->
  [Word64]
restrictKeyWords contextRanks propRanks key =
  [0x24]
    <> carrierAddrWords contextRanks propRanks (rkSource key)
    <> carrierAddrWords contextRanks propRanks (rkTarget key)
{-# INLINE restrictKeyWords #-}
carrierFamilyWords ::
  (Ord ctx, Ord prop) =>
  Map ctx Int ->
  Map (PropositionKey prop) Int ->
  CarrierFamily ctx Carrier prop ->
  [Word64]
carrierFamilyWords contextRanks propRanks family =
  [ 0x30,
    wordOfInt (rankOf (carrierFamilyTargetContext family) contextRanks),
    wordOfInt (rankOf (carrierFamilyProp family) propRanks),
    wordOfInt (Set.size (carrierFamilyMembers family))
  ]
    <> foldMap
      (carrierAddrWords contextRanks propRanks)
      (Set.toAscList (carrierFamilyMembers family))
{-# INLINE carrierFamilyWords #-}
carrierAddrWords ::
  (Ord ctx, Ord prop) =>
  Map ctx Int ->
  Map (PropositionKey prop) Int ->
  CarrierAddr ctx Carrier prop ->
  [Word64]
carrierAddrWords contextRanks propRanks addr =
  [ 0x50,
    wordOfInt (rankOf (caContext addr) contextRanks),
    wordOfInt (rankOf (caProp addr) propRanks)
  ]
    <> carrierWords (caCarrier addr)
{-# INLINE carrierAddrWords #-}
carrierWords :: Carrier -> [Word64]
carrierWords carrier =
  case carrier of
    QueryCarrier queryId (QueryAtom atomId) ->
      [0x51, wordOfInt (queryIdKey queryId), wordOfInt (atomIdKey atomId)]
    QueryCarrier queryId (QueryFactor factorNode) ->
      [0x52, wordOfInt (queryIdKey queryId)] <> factorNodeWords factorNode
    DerivedCarrier derived ->
      [0x53]
        <> stableDigestWords (unSubsumptionWitnessDigest (dciWitness derived))
        <> stableDigestWords (dciShape derived)
{-# INLINE carrierWords #-}
factorNodeWords :: FactorNode -> [Word64]
factorNodeWords node =
  case node of
    FactorNodeRoot ->
      [0x60]
    FactorNodeBag (BagId bagKey) ->
      [0x61, wordOfInt bagKey]
    FactorNodeBagBelief (BagId bagKey) ->
      [0x63, wordOfInt bagKey]
    FactorNodeSeparator (BagId childKey) (BagId parentKey) ->
      [0x62, wordOfInt childKey, wordOfInt parentKey]
{-# INLINE factorNodeWords #-}
intSetWords :: IntSet -> [Word64]
intSetWords values =
  [0x70, wordOfInt (IntSet.size values)]
    <> fmap wordOfInt (IntSet.toAscList values)
{-# INLINE intSetWords #-}
ranksOf :: Ord value => [value] -> Map value Int
ranksOf values =
  Map.fromList (zip values [0 :: Int ..])
{-# INLINE ranksOf #-}
rankOf :: Ord value => value -> Map value Int -> Int
rankOf value =
  Map.findWithDefault maxBound value
{-# INLINE rankOf #-}
finishErrors :: [err] -> Either [err] ()
finishErrors errors =
  case errors of
    [] -> Right ()
    _ -> Left errors
{-# INLINE finishErrors #-}
