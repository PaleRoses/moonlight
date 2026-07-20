{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
    ContextRestrictionRegistry,
    ContextRestrictionRegistryError (..),
    crrContexts,
    crrEdges,
    emptyRestrictionRegistry,
    contextRestrictionCount,
    contextRestrictionPairs,
    mkContextRestrictionRegistry,
    contextRestrictionSources,
    contextRestrictionTargets,
  )
where

import Data.Foldable
  ( traverse_,
  )
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Index.Arrow
  ( ArrowIndex,
    arrowIndexCount,
    arrowIndexEntries,
    arrowIndexVertices,
    arrowsFrom,
    arrowsTo,
    buildArrowIndex,
    emptyArrowIndex,
  )

type ContextRestrictionEdge :: Type -> Type
data ContextRestrictionEdge ctx = ContextRestrictionEdge
  { creSourceContext :: !ctx,
    creTargetContext :: !ctx
  }
  deriving stock (Eq, Ord, Show, Read)

type ContextRestrictionRegistry :: Type -> Type
newtype ContextRestrictionRegistry ctx = ContextRestrictionRegistry
  { crrIndex :: ArrowIndex ctx (ContextRestrictionEdge ctx)
  }
  deriving stock (Eq, Show)

type ContextRestrictionRegistryError :: Type -> Type
newtype ContextRestrictionRegistryError ctx
  = ContextRestrictionEdgeEndpointUnknown (ContextRestrictionEdge ctx)
  deriving stock (Eq, Ord, Show)

mkContextRestrictionRegistry ::
  Ord ctx =>
  Set ctx ->
  [ContextRestrictionEdge ctx] ->
  Either (ContextRestrictionRegistryError ctx) (ContextRestrictionRegistry ctx)
mkContextRestrictionRegistry contexts edges = do
  traverse_ validateEdge edges
  pure
    ContextRestrictionRegistry
      { crrIndex =
          buildArrowIndex
            contexts
            creSourceContext
            creTargetContext
            edges
      }
  where
    validateEdge edge
      | Set.member (creSourceContext edge) contexts
          && Set.member (creTargetContext edge) contexts =
          Right ()
      | otherwise =
          Left (ContextRestrictionEdgeEndpointUnknown edge)
{-# INLINE mkContextRestrictionRegistry #-}

emptyRestrictionRegistry :: ContextRestrictionRegistry ctx
emptyRestrictionRegistry =
  ContextRestrictionRegistry
    { crrIndex = emptyArrowIndex Set.empty
    }
{-# INLINE emptyRestrictionRegistry #-}

crrContexts ::
  ContextRestrictionRegistry ctx ->
  Set ctx
crrContexts =
  arrowIndexVertices . crrIndex
{-# INLINE crrContexts #-}

crrEdges ::
  ContextRestrictionRegistry ctx ->
  [ContextRestrictionEdge ctx]
crrEdges =
  arrowIndexEntries . crrIndex
{-# INLINE crrEdges #-}

contextRestrictionCount :: ContextRestrictionRegistry ctx -> Int
contextRestrictionCount =
  arrowIndexCount . crrIndex
{-# INLINE contextRestrictionCount #-}

contextRestrictionPairs :: ContextRestrictionRegistry ctx -> [(ctx, ctx)]
contextRestrictionPairs =
  fmap
    ( \restrictionEdge ->
        (creSourceContext restrictionEdge, creTargetContext restrictionEdge)
    )
    . crrEdges
{-# INLINE contextRestrictionPairs #-}

contextRestrictionSources ::
  Ord ctx =>
  ctx ->
  ContextRestrictionRegistry ctx ->
  [ctx]
contextRestrictionSources contextValue =
  fmap creSourceContext . arrowsTo contextValue . crrIndex
{-# INLINE contextRestrictionSources #-}

contextRestrictionTargets ::
  Ord ctx =>
  ctx ->
  ContextRestrictionRegistry ctx ->
  [ctx]
contextRestrictionTargets contextValue =
  fmap creTargetContext . arrowsFrom contextValue . crrIndex
{-# INLINE contextRestrictionTargets #-}
