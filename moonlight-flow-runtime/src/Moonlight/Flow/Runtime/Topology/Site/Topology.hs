module Moonlight.Flow.Runtime.Topology.Site.Topology
  ( compileGeneratedCarrierTopology,
  )
where
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( rkSource,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedMorphism (..),
    GeneratedRoutingSource (..),
    GeneratedSiteState (..),
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    emptyCarrierTopology,
    insertCarrierEdge,
    insertCarrierTouch,
    insertCarrierFamily,
  )
compileGeneratedCarrierTopology ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  CarrierTopology ctx Carrier prop
compileGeneratedCarrierTopology site =
  graphWithCovers
  where
    source =
      gssRouteSource site
    graph0 =
      Map.foldlWithKey'
        (\graph touchKey addrs ->
          Set.foldl'
            (\graph' addr -> insertCarrierTouch touchKey addr graph')
            graph
            addrs
        )
        emptyCarrierTopology
        (grsCarrierTouches source)
    graphWithMorphisms =
      List.foldl'
        ( \graph morphism ->
            insertMorphismGraph morphism graph
        )
        graph0
        (Map.elems (gssMorphisms site))
    graphWithCovers =
      List.foldl'
        ( \graph family ->
            insertCarrierFamily family graph
        )
        graphWithMorphisms
        (Map.keys (gssCovers site))
{-# INLINE compileGeneratedCarrierTopology #-}
insertMorphismGraph ::
  (Ord ctx, Ord prop) =>
  GeneratedMorphism ctx prop ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
insertMorphismGraph morphism graph0 =
  Set.foldl'
    (\graph edge -> insertCarrierEdge (rkSource edge) (EdgeRestriction edge) graph)
    graph0
    (gmRestrictionEdges morphism)
{-# INLINE insertMorphismGraph #-}
