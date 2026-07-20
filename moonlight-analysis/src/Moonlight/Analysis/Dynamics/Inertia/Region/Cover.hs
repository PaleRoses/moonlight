module Moonlight.Analysis.Dynamics.Inertia.Region.Cover
  ( RegionProvenance (..),
    InertiaRegionCell (..),
    InertiaRegionDecomposition (..),
    InertiaRegionCoverBlueprint (..),
    buildCoverBlueprint,
    coverBlueprintFromDecomposition,
    coverChildrenByParent,
    restrictCoverBlueprint,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.Dynamics.Inertia.Region.Kernel
  ( AABB,
    InertiaRegionCell (..),
    InertiaRegionDecomposition (..),
    RegionProvenance (..),
    aabbDimensions,
    aabbMax,
    aabbMin,
  )
import Moonlight.LinAlg.Geometry (Vec3 (..))

type InertiaRegionCoverBlueprint :: Type -> Type
data InertiaRegionCoverBlueprint site = InertiaRegionCoverBlueprint
  { ircbCellsBySite :: Map site (InertiaRegionCell site),
    ircbCoverPairs :: [(site, site)]
  }
  deriving stock (Eq, Show)

buildCoverBlueprint ::
  Ord site =>
  [site] ->
  Map site (InertiaRegionCell site) ->
  InertiaRegionCoverBlueprint site
buildCoverBlueprint alignedSites cellsBySite =
  let alignedSiteSet = Set.fromList alignedSites
      alignedCells = Map.restrictKeys cellsBySite alignedSiteSet
   in InertiaRegionCoverBlueprint
        { ircbCellsBySite = alignedCells,
          ircbCoverPairs = explicitCoverPairs alignedSites alignedCells
        }

coverBlueprintFromDecomposition ::
  Ord site =>
  InertiaRegionDecomposition site ->
  InertiaRegionCoverBlueprint site
coverBlueprintFromDecomposition decomposition =
  let regionEntries = decompositionEntries Nothing decomposition
   in buildCoverBlueprint
        (fmap fst regionEntries)
        (Map.fromList regionEntries)

coverChildrenByParent ::
  Ord site =>
  InertiaRegionCoverBlueprint site ->
  Map site [site]
coverChildrenByParent = Map.fromListWith (<>) . fmap (fmap pure) . ircbCoverPairs

restrictCoverBlueprint ::
  Ord site =>
  [site] ->
  InertiaRegionCoverBlueprint site ->
  InertiaRegionCoverBlueprint site
restrictCoverBlueprint alignedSites coverBlueprint =
  let alignedSiteSet = Set.fromList alignedSites
   in InertiaRegionCoverBlueprint
        { ircbCellsBySite = Map.restrictKeys (ircbCellsBySite coverBlueprint) alignedSiteSet,
          ircbCoverPairs =
            filter
              (\(parentSite, childSite) -> Set.member parentSite alignedSiteSet && Set.member childSite alignedSiteSet)
              (ircbCoverPairs coverBlueprint)
        }

decompositionEntries ::
  Maybe site ->
  InertiaRegionDecomposition site ->
  [(site, InertiaRegionCell site)]
decompositionEntries maybeParentSite decomposition =
  let currentSite = irdSite decomposition
      currentEntry =
        ( currentSite,
          InertiaRegionCell
            { ircBoundingBox = irdBoundingBox decomposition,
              ircProvenance = maybe RootRegion RefinedFrom maybeParentSite
            }
        )
   in currentEntry : (irdChildren decomposition >>= decompositionEntries (Just currentSite))

explicitCoverPairs ::
  Ord site =>
  [site] ->
  Map site (InertiaRegionCell site) ->
  [(site, site)]
explicitCoverPairs alignedSites cellsBySite =
  alignedSites
    >>= \childSite ->
      case Map.lookup childSite cellsBySite >>= parentSiteOf of
        Just parentSite
          | Map.member parentSite cellsBySite
              && parentContainsChild cellsBySite parentSite childSite ->
              [(parentSite, childSite)]
        _ ->
          []

parentSiteOf :: InertiaRegionCell site -> Maybe site
parentSiteOf regionCell =
  case ircProvenance regionCell of
    RootRegion -> Nothing
    RefinedFrom parentSite -> Just parentSite

parentContainsChild ::
  Ord site =>
  Map site (InertiaRegionCell site) ->
  site ->
  site ->
  Bool
parentContainsChild cellsBySite parentSite childSite =
  case (Map.lookup parentSite cellsBySite, Map.lookup childSite cellsBySite) of
    (Just parentCell, Just childCell) ->
      strictlyContainsBoundingBox (ircBoundingBox parentCell) (ircBoundingBox childCell)
    _ ->
      False

strictlyContainsBoundingBox :: AABB -> AABB -> Bool
strictlyContainsBoundingBox parentBox childBox =
  let Vec3 parentMinX parentMinY parentMinZ = aabbMin parentBox
      Vec3 parentMaxX parentMaxY parentMaxZ = aabbMax parentBox
      Vec3 childMinX childMinY childMinZ = aabbMin childBox
      Vec3 childMaxX childMaxY childMaxZ = aabbMax childBox
   in parentMinX <= childMinX
        && parentMinY <= childMinY
        && parentMinZ <= childMinZ
        && parentMaxX >= childMaxX
        && parentMaxY >= childMaxY
        && parentMaxZ >= childMaxZ
        && boundingBoxVolume parentBox > boundingBoxVolume childBox + 1.0e-12

boundingBoxVolume :: AABB -> Double
boundingBoxVolume boundingBox =
  let Vec3 width height depth = aabbDimensions boundingBox
   in width * height * depth
