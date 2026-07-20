{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Topology.Lowering.GeneratedSite
  ( lowerGeneratedSiteTransition,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    amalgamateCarrierFamilyDataflowOp,
    fullRepairFactorBatchDataflowOp,
    restrictCarrierDataflowOp,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason (FullRepairContextInstalled),
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRouting (..),
    RuntimeRepairRoute (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSitePatch (..),
    GeneratedSiteTransition (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedContextShape (..),
    GeneratedMorphism (..),
    GeneratedQueryBinding (..),
    GeneratedSiteState (..),
  )

lowerGeneratedSiteTransition ::
  (Ord ctx, Ord prop) =>
  RuntimeRepairRouting ->
  GeneratedSitePatch ctx prop ->
  GeneratedSiteTransition ctx prop ->
  [RuntimeDataflowOp ctx prop boundary evidence]
lowerGeneratedSiteTransition repairRouting patch transition =
  case patch of
    AddContext contextValue shape ->
      [ fullRepairFactorBatchDataflowOp
          contextValue
          (gqbProp binding)
          (rrtRepairKey route)
          (rrtRepresentativeQueryId route)
          FullRepairContextInstalled
      | (queryId, binding) <- Map.toAscList (gcsQueryBindings shape),
        Just route <- [rrRepairRouteOfQuery repairRouting queryId]
      ]
    InstallMorphism morphism _routePatch ->
      installedMorphismDataflowOps transition morphism
    AddCover family _generatedCover ->
      [amalgamateCarrierFamilyDataflowOp family]
    MergeContexts {} ->
      []
    RemoveObsoleteContext {} ->
      []
{-# INLINE lowerGeneratedSiteTransition #-}

installedMorphismDataflowOps ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteTransition ctx prop ->
  GeneratedMorphism ctx prop ->
  [RuntimeDataflowOp ctx prop boundary evidence]
installedMorphismDataflowOps transition incoming =
  fmap restrictCarrierDataflowOp (Set.toAscList newRestrictions)
  where
    key =
      gmKey incoming

    before =
      Map.findWithDefault
        (emptyIncomingMorphism incoming)
        key
        (gssMorphisms (gstBefore transition))

    after =
      Map.findWithDefault
        (emptyIncomingMorphism incoming)
        key
        (gssMorphisms (gstAfter transition))

    newRestrictions =
      Set.difference
        (gmRestrictionEdges after)
        (gmRestrictionEdges before)
{-# INLINE installedMorphismDataflowOps #-}

emptyIncomingMorphism :: GeneratedMorphism ctx prop -> GeneratedMorphism ctx prop
emptyIncomingMorphism incoming =
  incoming
    { gmRestrictionEdges = Set.empty
    }
{-# INLINE emptyIncomingMorphism #-}
