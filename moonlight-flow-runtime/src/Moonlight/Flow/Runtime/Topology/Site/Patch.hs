{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSitePatch (..),
    SiteEffects (..),
    GeneratedSiteTransition (..),
    emptySiteEffects,
    emptyGeneratedSiteTransition,
    applyGeneratedSitePatchState,
  )
where
import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( catMaybes,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope (..),
    TopoDelta (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Differential.Carrier.Topology
  ( carrierFamilyTargetContext,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
data GeneratedSitePatch ctx prop
  = AddContext
      !ctx
      !(GeneratedContextShape prop)
  | InstallMorphism
      !(GeneratedMorphism ctx prop)
      !(GeneratedRoutePatch ctx prop)
  | AddCover !(CarrierFamily ctx Carrier prop) !(GeneratedCover ctx prop)
  | MergeContexts !(ContextMergePlan ctx prop)
  | RemoveObsoleteContext !ctx !(Set (CarrierAddr ctx Carrier prop))
data SiteEffects ctx prop = SiteEffects
  { seRelationalScope :: !RelationalScope,
    seCarrierMoves :: !(CarrierMoves (CarrierAddr ctx Carrier prop)),
    seDropContexts :: !(Set ctx)
  }
emptySiteEffects :: SiteEffects ctx prop
emptySiteEffects =
  SiteEffects
    { seRelationalScope = mempty,
      seCarrierMoves = emptyCarrierMoves,
      seDropContexts = Set.empty
    }
{-# INLINE emptySiteEffects #-}
data GeneratedSiteTransition ctx prop = GeneratedSiteTransition
  { gstBefore :: !(GeneratedSiteState ctx prop),
    gstAfter :: !(GeneratedSiteState ctx prop),
    gstEffects :: !(SiteEffects ctx prop)
  }
emptyGeneratedSiteTransition ::
  GeneratedSiteState ctx prop ->
  GeneratedSiteState ctx prop ->
  GeneratedSiteTransition ctx prop
emptyGeneratedSiteTransition before after =
  GeneratedSiteTransition
    { gstBefore = before,
      gstAfter = after,
      gstEffects = emptySiteEffects
    }
{-# INLINE emptyGeneratedSiteTransition #-}
applyGeneratedSitePatchState ::
  (Ord ctx, Ord prop) =>
  GeneratedSitePatch ctx prop ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyGeneratedSitePatchState patch site =
  case patch of
    AddContext contextValue shape ->
      applyAddContextState contextValue shape site
    InstallMorphism morphism routePatch ->
      applyInstallMorphismState morphism routePatch site
    AddCover family generatedCover ->
      applyAddCoverState family generatedCover site
    MergeContexts mergePlan ->
      applyMergeContextsState mergePlan site
    RemoveObsoleteContext contextValue carriers ->
      applyRemoveObsoleteContextState contextValue carriers site
{-# INLINE applyGeneratedSitePatchState #-}
applyAddContextState ::
  (Ord ctx, Ord prop) =>
  ctx ->
  GeneratedContextShape prop ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyAddContextState contextValue shape site0 = do
  case Map.lookup contextValue (gssContexts site0) of
    Just _ ->
      Left (GeneratedSiteContextAlreadyExists contextValue)
    Nothing ->
      Right ()
  case validateGeneratedContextShape contextValue shape of
    Left errors ->
      Left (GeneratedSiteContextShapeInvalid errors)
    Right () ->
      Right ()
  case validateGeneratedContextRouting site0 contextValue shape of
    Left errors ->
      Left (GeneratedSiteContextShapeInvalid errors)
    Right () ->
      Right ()
  let site1 =
        refreshGeneratedSiteDigest
          site0
            { gssContexts =
                Map.insert contextValue shape (gssContexts site0),
              gssContextClasses =
                insertContextClass contextValue (gssContextClasses site0)
            }
  pure (emptyGeneratedSiteTransition site0 site1)
{-# INLINE applyAddContextState #-}
applyInstallMorphismState ::
  (Ord ctx, Ord prop) =>
  GeneratedMorphism ctx prop ->
  GeneratedRoutePatch ctx prop ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyInstallMorphismState morphism routePatch site0 = do
  let key =
        gmKey morphism
  unlessContextPresent (mkSourceContext key) key site0
  unlessContextPresent (mkTargetContext key) key site0
  mergedMorphism <-
    case Map.lookup key (gssMorphisms site0) of
      Nothing ->
        Right morphism
      Just existing ->
        mergeInstalledMorphism key existing morphism
  routeSource1 <-
    insertGeneratedRoutePatch routePatch (gssRouteSource site0)
  let site1 =
        refreshGeneratedSiteDigest
          site0
            { gssMorphisms =
                Map.insert key mergedMorphism (gssMorphisms site0),
              gssRouteSource =
                routeSource1
            }
  pure (emptyGeneratedSiteTransition site0 site1)
{-# INLINE applyInstallMorphismState #-}
mergeInstalledMorphism ::
  (Ord ctx, Ord prop) =>
  MorphismKey ctx ->
  GeneratedMorphism ctx prop ->
  GeneratedMorphism ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedMorphism ctx prop)
mergeInstalledMorphism key existing incoming
  | gmKey existing /= key =
      Left (GeneratedSiteMorphismConflict key existing incoming)
  | gmKey incoming /= key =
      Left (GeneratedSiteMorphismConflict key existing incoming)
  | gmKey existing /= gmKey incoming =
      Left (GeneratedSiteMorphismConflict key existing incoming)
  | otherwise =
      Right (mergeGeneratedMorphism existing incoming)
{-# INLINE mergeInstalledMorphism #-}
insertGeneratedRoutePatch ::
  (Ord ctx, Ord prop) =>
  GeneratedRoutePatch ctx prop ->
  GeneratedRoutingSource ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedRoutingSource ctx prop)
insertGeneratedRoutePatch patch source = do
  restrictRoutes <-
    insertShardRoutes
      (grsRestrictShardsByCarrier source)
      (grpRestrictShardsByCarrier patch)
  indexRoutes <-
    insertShardRoutes
      (grsIndexShardsByCarrier source)
      (grpIndexShardsByCarrier patch)
  pure
    source
      { grsCarrierTouches =
          Map.unionWith Set.union
            (grsCarrierTouches source)
            (grpCarrierTouches patch),
        grsRestrictShardsByCarrier = restrictRoutes,
        grsIndexShardsByCarrier = indexRoutes
      }
{-# INLINE insertGeneratedRoutePatch #-}
insertShardRoutes ::
  (Ord ctx, Ord prop) =>
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Either
    (GeneratedSitePatchError ctx prop)
    (Map (CarrierAddr ctx Carrier prop) Shard)
insertShardRoutes =
  Map.foldlWithKey' insertRoute . Right
  where
    insertRoute ::
      (Ord ctx, Ord prop) =>
      Either
        (GeneratedSitePatchError ctx prop)
        (Map (CarrierAddr ctx Carrier prop) Shard) ->
      CarrierAddr ctx Carrier prop ->
      Shard ->
      Either
        (GeneratedSitePatchError ctx prop)
        (Map (CarrierAddr ctx Carrier prop) Shard)
    insertRoute eitherAcc addr shard = do
      acc <- eitherAcc
      case Map.lookup addr acc of
        Nothing ->
          Right (Map.insert addr shard acc)
        Just existing
          | existing == shard ->
              Right acc
          | otherwise ->
              Left (GeneratedSiteMorphismRouteShardCollision addr existing shard)
{-# INLINE insertShardRoutes #-}
unlessContextPresent ::
  Ord ctx =>
  ctx ->
  MorphismKey ctx ->
  GeneratedSiteState ctx prop ->
  Either (GeneratedSitePatchError ctx prop) ()
unlessContextPresent contextValue key site =
  if Map.member contextValue (gssContexts site)
    then Right ()
    else Left (GeneratedSiteMorphismEndpointMissing key)
{-# INLINE unlessContextPresent #-}
applyAddCoverState ::
  (Ord ctx, Ord prop) =>
  CarrierFamily ctx Carrier prop ->
  GeneratedCover ctx prop ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyAddCoverState family generatedCover site0 = do
  case Map.lookup family (gssCovers site0) of
    Just _ ->
      Left (GeneratedSiteCoverAlreadyExists family)
    Nothing ->
      Right ()
  let site1 =
        refreshGeneratedSiteDigest
          site0
            { gssCovers =
                Map.insert family generatedCover (gssCovers site0)
            }
  pure
    ( (emptyGeneratedSiteTransition site0 site1)
        { gstEffects =
            emptySiteEffects
              { seRelationalScope =
                  mempty {rsTopo = TopoDelta (gcDirtyTopo generatedCover)}
              }
        }
    )
{-# INLINE applyAddCoverState #-}
applyMergeContextsState ::
  (Ord ctx, Ord prop) =>
  ContextMergePlan ctx prop ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyMergeContextsState mergePlan site0 = do
  classes1 <-
    first GeneratedSiteMergeFailed $
      mergeContextClasses
        (cmpOldA mergePlan)
        (cmpOldB mergePlan)
        (cmpCanonical mergePlan)
        (gssContextClasses site0)
  carrierMoves <-
    first GeneratedSiteCarrierMoveInvalid $
      normalizeCarrierMoves (cmpCarrierMoves mergePlan)
  routeSource1 <-
    applyCarrierMovesRoutingSource
      carrierMoves
      (gssRouteSource site0)
  mergedShape <-
    mergeContextShapes
      (cmpCanonical mergePlan)
      (catMaybes
         [ Map.lookup (cmpOldA mergePlan) (gssContexts site0),
           Map.lookup (cmpOldB mergePlan) (gssContexts site0),
           Map.lookup (cmpCanonical mergePlan) (gssContexts site0)
         ])
  let site1 =
        refreshGeneratedSiteDigest
          site0
            { gssContexts =
                Map.insert
                  (cmpCanonical mergePlan)
                  mergedShape
                  ( Map.delete (cmpOldA mergePlan)
                      (Map.delete (cmpOldB mergePlan) (gssContexts site0))
                  ),
              gssContextClasses = classes1,
              gssMorphisms =
                retargetGeneratedMorphisms
                  mergePlan
                  carrierMoves
                  (gssMorphisms site0),
              gssCovers = dropOldContextCovers mergePlan (gssCovers site0),
              gssRouteSource = routeSource1,
              gssPlanObjects =
                fmap
                  (canonicalizeMergeOwner mergePlan)
                  (gssPlanObjects site0)
            }
  pure
    ( (emptyGeneratedSiteTransition site0 site1)
        { gstEffects =
            emptySiteEffects
              { seRelationalScope =
                  mempty {rsTopo = TopoDelta (cmpDirtyTopo mergePlan)},
                seCarrierMoves = carrierMoves,
                seDropContexts = Set.fromList [cmpOldA mergePlan, cmpOldB mergePlan]
              }
        }
    )
{-# INLINE applyMergeContextsState #-}
applyRemoveObsoleteContextState ::
  (Ord ctx, Ord prop) =>
  ctx ->
  Set (CarrierAddr ctx Carrier prop) ->
  GeneratedSiteState ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedSiteTransition ctx prop)
applyRemoveObsoleteContextState contextValue explicitCarriers site0 = do
  case Map.lookup contextValue (gssContexts site0) of
    Nothing ->
      Left (GeneratedSiteContextMissing contextValue)
    Just _ ->
      Right ()
  let carrierMoves0 =
        removeContextCarrierMoves
          contextValue
          explicitCarriers
          (gssRouteSource site0)
  carrierMoves <-
    first GeneratedSiteCarrierMoveInvalid $
      normalizeCarrierMoves carrierMoves0
  routeSource1 <-
    applyCarrierMovesRoutingSource
      carrierMoves
      (gssRouteSource site0)
  let site1 =
        refreshGeneratedSiteDigest
          site0
            { gssContexts =
                Map.delete contextValue (gssContexts site0),
              gssRouteSource =
                routeSource1,
              gssMorphisms =
                dropContextMorphisms
                  contextValue
                  (gssMorphisms site0),
              gssCovers =
                Map.filterWithKey
                  (\family _cover -> carrierFamilyTargetContext family /= contextValue)
                  (gssCovers site0),
              gssPlanObjects =
                Map.filter (/= contextValue) (gssPlanObjects site0)
            }
  pure
    ( (emptyGeneratedSiteTransition site0 site1)
        { gstEffects =
            emptySiteEffects
              { seCarrierMoves = carrierMoves,
                seDropContexts = Set.singleton contextValue
              }
        }
    )
{-# INLINE applyRemoveObsoleteContextState #-}

mergeContextShapes ::
  forall ctx prop.
  Ord prop =>
  ctx ->
  [GeneratedContextShape prop] ->
  Either (GeneratedSitePatchError ctx prop) (GeneratedContextShape prop)
mergeContextShapes canonical shapes =
  case shapes of
    [] ->
      Left (GeneratedSiteContextMissing canonical)
    _ -> do
      bindings <-
        foldM insertShapeBindings Map.empty shapes
      propRoutes <-
        foldM insertShapePropRoutes Map.empty shapes
      let shape0 =
            GeneratedContextShape
              { gcsShapeDigest = StableDigest128 0 0,
                gcsQueryBindings = bindings,
                gcsIndexShardsByProp = propRoutes
              }
      pure shape0 {gcsShapeDigest = generatedContextShapeDigest shape0}
  where
    insertShapeBindings ::
      Map QueryId (GeneratedQueryBinding prop) ->
      GeneratedContextShape prop ->
      Either
        (GeneratedSitePatchError ctx prop)
        (Map QueryId (GeneratedQueryBinding prop))
    insertShapeBindings acc shape =
      foldM insertBinding acc (Map.toAscList (gcsQueryBindings shape))
    insertBinding ::
      Map QueryId (GeneratedQueryBinding prop) ->
      (QueryId, GeneratedQueryBinding prop) ->
      Either
        (GeneratedSitePatchError ctx prop)
        (Map QueryId (GeneratedQueryBinding prop))
    insertBinding acc (queryId, binding) =
      case Map.lookup queryId acc of
        Nothing ->
          Right (Map.insert queryId binding acc)
        Just existing
          | existing == binding ->
              Right acc
          | otherwise ->
              Left (GeneratedSiteMergeQueryCollision queryId)
    insertShapePropRoutes ::
      Map (PropositionKey prop) Shard ->
      GeneratedContextShape prop ->
      Either (GeneratedSitePatchError ctx prop) (Map (PropositionKey prop) Shard)
    insertShapePropRoutes acc shape =
      foldM insertPropRoute acc (Map.toAscList (gcsIndexShardsByProp shape))
    insertPropRoute ::
      Map (PropositionKey prop) Shard ->
      (PropositionKey prop, Shard) ->
      Either (GeneratedSitePatchError ctx prop) (Map (PropositionKey prop) Shard)
    insertPropRoute acc (propKey, shard) =
      case Map.lookup propKey acc of
        Nothing ->
          Right (Map.insert propKey shard acc)
        Just existing
          | existing == shard ->
              Right acc
          | otherwise ->
              Left (GeneratedSiteMergePropShardCollision propKey existing shard)
{-# INLINE mergeContextShapes #-}
canonicalizeMergeOwner ::
  Eq ctx =>
  ContextMergePlan ctx prop ->
  ctx ->
  ctx
canonicalizeMergeOwner mergePlan contextValue =
  if contextValue == cmpOldA mergePlan || contextValue == cmpOldB mergePlan
    then cmpCanonical mergePlan
    else contextValue
{-# INLINE canonicalizeMergeOwner #-}
applyCarrierMovesRoutingSource ::
  (Ord ctx, Ord prop) =>
  CarrierMoves (CarrierAddr ctx Carrier prop) ->
  GeneratedRoutingSource ctx prop ->
  Either
    (GeneratedSitePatchError ctx prop)
    (GeneratedRoutingSource ctx prop)
applyCarrierMovesRoutingSource moves source = do
  restrictShards <-
    carrierMovesTargetMapWith
      GeneratedSiteCarrierMoveShardCollision
      moves
      (grsRestrictShardsByCarrier source)
  indexShards <-
    carrierMovesTargetMapWith
      GeneratedSiteCarrierMoveShardCollision
      moves
      (grsIndexShardsByCarrier source)
  pure
    source
      { grsCarrierTouches =
          fmap
            (carrierMovesTargetSet moves)
            (grsCarrierTouches source),
        grsRestrictShardsByCarrier = restrictShards,
        grsIndexShardsByCarrier = indexShards
      }
{-# INLINE applyCarrierMovesRoutingSource #-}
dropOldContextCovers ::
  Eq ctx =>
  ContextMergePlan ctx prop ->
  Map (CarrierFamily ctx Carrier prop) (GeneratedCover ctx prop) ->
  Map (CarrierFamily ctx Carrier prop) (GeneratedCover ctx prop)
dropOldContextCovers mergePlan =
  Map.filterWithKey
    (\family _cover -> carrierFamilyTargetContext family /= cmpOldA mergePlan && carrierFamilyTargetContext family /= cmpOldB mergePlan)
{-# INLINE dropOldContextCovers #-}
dropContextMorphisms ::
  forall ctx prop.
  Eq ctx =>
  ctx ->
  Map (MorphismKey ctx) (GeneratedMorphism ctx prop) ->
  Map (MorphismKey ctx) (GeneratedMorphism ctx prop)
dropContextMorphisms contextValue =
  Map.mapMaybe liveMorphism
    . Map.filterWithKey
      (\key _morphism -> not (morphismKeyTouchesContext contextValue key))
  where
    liveMorphism ::
      GeneratedMorphism ctx prop ->
      Maybe (GeneratedMorphism ctx prop)
    liveMorphism morphism =
      Just
        morphism
          { gmRestrictionEdges =
              Set.filter
                (not . restrictKeyTouchesContext contextValue)
                (gmRestrictionEdges morphism)
          }
{-# INLINE dropContextMorphisms #-}
morphismKeyTouchesContext ::
  Eq ctx =>
  ctx ->
  MorphismKey ctx ->
  Bool
morphismKeyTouchesContext contextValue key =
  mkSourceContext key == contextValue
    || mkTargetContext key == contextValue
{-# INLINE morphismKeyTouchesContext #-}
restrictKeyTouchesContext ::
  Eq ctx =>
  ctx ->
  RestrictKey ctx Carrier prop ->
  Bool
restrictKeyTouchesContext contextValue key =
  caContext (rkSource key) == contextValue
    || caContext (rkTarget key) == contextValue
{-# INLINE restrictKeyTouchesContext #-}
retargetGeneratedMorphisms ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  ContextMergePlan ctx prop ->
  CarrierMoves (CarrierAddr ctx Carrier prop) ->
  Map (MorphismKey ctx) (GeneratedMorphism ctx prop) ->
  Map (MorphismKey ctx) (GeneratedMorphism ctx prop)
retargetGeneratedMorphisms mergePlan moves =
  Map.foldl'
    insertRetargeted
    Map.empty
  where
    insertRetargeted ::
      Map (MorphismKey ctx) (GeneratedMorphism ctx prop) ->
      GeneratedMorphism ctx prop ->
      Map (MorphismKey ctx) (GeneratedMorphism ctx prop)
    insertRetargeted acc morphism =
      case retargetGeneratedMorphism mergePlan moves morphism of
        Nothing ->
          acc
        Just morphism' ->
          Map.insertWith mergeGeneratedMorphism (gmKey morphism') morphism' acc
{-# INLINE retargetGeneratedMorphisms #-}
retargetGeneratedMorphism ::
  (Ord ctx, Ord prop) =>
  ContextMergePlan ctx prop ->
  CarrierMoves (CarrierAddr ctx Carrier prop) ->
  GeneratedMorphism ctx prop ->
  Maybe (GeneratedMorphism ctx prop)
retargetGeneratedMorphism mergePlan moves morphism =
  Just
    morphism
      { gmKey = key',
        gmRestrictionEdges =
          Set.fromList
            [ edge'
            | edge <- Set.toAscList (gmRestrictionEdges morphism),
              Just edge' <- [retargetRestrictKey moves edge]
            ]
      }
  where
    key =
      gmKey morphism
    key' =
      key
        { mkSourceContext = canonicalizeMergeOwner mergePlan (mkSourceContext key),
          mkTargetContext = canonicalizeMergeOwner mergePlan (mkTargetContext key)
        }
{-# INLINE retargetGeneratedMorphism #-}
mergeGeneratedMorphism ::
  (Ord ctx, Ord prop) =>
  GeneratedMorphism ctx prop ->
  GeneratedMorphism ctx prop ->
  GeneratedMorphism ctx prop
mergeGeneratedMorphism left right =
  left
    { gmRestrictionEdges = Set.union (gmRestrictionEdges left) (gmRestrictionEdges right)
    }
{-# INLINE mergeGeneratedMorphism #-}
retargetRestrictKey ::
  (Ord ctx, Ord prop) =>
  CarrierMoves (CarrierAddr ctx Carrier prop) ->
  RestrictKey ctx Carrier prop ->
  Maybe (RestrictKey ctx Carrier prop)
retargetRestrictKey moves edge = do
  source' <-
    carrierMovesTarget moves (rkSource edge)
  target' <-
    carrierMovesTarget moves (rkTarget edge)
  pure
    edge
      { rkSource = source',
        rkTarget = target'
      }
{-# INLINE retargetRestrictKey #-}
