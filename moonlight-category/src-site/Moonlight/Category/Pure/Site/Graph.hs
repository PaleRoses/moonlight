-- | Import-graph queries over a site manifest: edges, reachable closure, and
-- import-cycle detection. Reachability and cycle reporting both run on the
-- shared dense closure kernel
-- ("Moonlight.Category.Pure.Finite.DenseReachability").
module Moonlight.Category.Pure.Site.Graph
  ( siteImportEdges,
    siteReachable,
    reachableClosure,
    importCycles,
  )
where

import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))
import Moonlight.Category.Pure.Finite.DenseReachability
  ( denseClosureCycleComponents,
    denseClosureReachabilityRows,
    denseReachabilityWithCycles,
    objectIndexOf,
    objectSetFromBits,
    relationBitRows,
    relationUniverse,
  )

siteImportEdges :: Ord obj => SiteManifest obj -> Set (obj, obj)
siteImportEdges manifest =
  siteImports manifest
    & Map.toList
    >>= ( \(targetObj, sources) ->
            Set.toList sources
              & fmap (\sourceObj -> (targetObj, sourceObj))
        )
    & Set.fromList

siteReachable :: Ord obj => SiteManifest obj -> obj -> Set obj
siteReachable manifest start =
  Map.findWithDefault Set.empty start (reachableClosure (siteImports manifest))

reachableClosure :: Ord obj => Map obj (Set obj) -> Map obj (Set obj)
reachableClosure adjacency =
  let objectVector = Vector.fromList (Set.toAscList (relationUniverse adjacency))
      objectIndex = objectIndexOf objectVector
      closureRows =
        denseClosureReachabilityRows
          (denseReachabilityWithCycles (relationBitRows objectIndex objectVector adjacency))
      reachableSet objectValue =
        maybe
          Set.empty
          (objectSetFromBits objectVector)
          (Map.lookup objectValue objectIndex >>= (closureRows Vector.!?))
   in Map.mapWithKey (\objectValue _ -> reachableSet objectValue) adjacency

importCycles :: Ord obj => SiteManifest obj -> [NonEmpty obj]
importCycles manifest =
  let objectVector = Vector.fromList (Set.toAscList (siteObjects manifest))
      objectIndex = objectIndexOf objectVector
      closure =
        denseReachabilityWithCycles
          (relationBitRows objectIndex objectVector (siteImports manifest))
   in cycleComponentsFromIndices objectVector (denseClosureCycleComponents closure)

cycleComponentsFromIndices :: Ord obj => Vector.Vector obj -> [NonEmpty Int] -> [NonEmpty obj]
cycleComponentsFromIndices objectVector components =
  components
    >>= indexComponentObjects objectVector
    & List.sortOn NonEmpty.head

indexComponentObjects :: Ord obj => Vector.Vector obj -> NonEmpty Int -> [NonEmpty obj]
indexComponentObjects objectVector component =
  component
    & NonEmpty.toList
    & mapMaybe (objectVector Vector.!?)
    & List.sort
    & NonEmpty.nonEmpty
    & maybe [] pure
