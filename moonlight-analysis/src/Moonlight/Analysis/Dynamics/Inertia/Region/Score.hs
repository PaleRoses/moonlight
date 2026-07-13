module Moonlight.Analysis.Dynamics.Inertia.Region.Score
  ( InertiaRegionScore (..),
    InertiaRegionScorePolicy (..),
    defaultInertiaRegionScorePolicy,
    scoreInertiaRegionSection,
    interpretInertiaRegionScore,
    inertiaRegionScoreValue,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Analysis.Dynamics.Inertia.Region.Cover
  ( InertiaRegionCoverBlueprint (..),
    coverChildrenByParent,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.Kernel
  ( MassProperties (..),
    composeMassProperties,
  )
import Moonlight.EGraph.Fuzzy.Rank (totalOf, weightedComponent)
import Moonlight.LinAlg.Geometry (Symmetric3, symmetric3Entries)
import Moonlight.LinAlg.Geometry (magnitudeVec3, subVec3)

type InertiaRegionScore :: Type
data InertiaRegionScore = InertiaRegionScore
  { irsAnchorSupport :: Int,
    irsCoverPairCount :: Int,
    irsStructuralSiteCount :: Int,
    irsCompositionResidual :: Double
  }
  deriving stock (Eq, Show)

type InertiaRegionScorePolicy :: Type
data InertiaRegionScorePolicy = InertiaRegionScorePolicy
  { irspCompositionResidualWeight :: Double,
    irspCoverPairWeight :: Double,
    irspStructuralSiteWeight :: Double,
    irspMinimumSupportScale :: Int
  }
  deriving stock (Eq, Show)

instance Semigroup InertiaRegionScore where
  leftScore <> rightScore =
    InertiaRegionScore
      { irsAnchorSupport = irsAnchorSupport leftScore + irsAnchorSupport rightScore,
        irsCoverPairCount = irsCoverPairCount leftScore + irsCoverPairCount rightScore,
        irsStructuralSiteCount =
          irsStructuralSiteCount leftScore + irsStructuralSiteCount rightScore,
        irsCompositionResidual =
          irsCompositionResidual leftScore + irsCompositionResidual rightScore
      }

instance Monoid InertiaRegionScore where
  mempty =
    InertiaRegionScore
      { irsAnchorSupport = 0,
        irsCoverPairCount = 0,
        irsStructuralSiteCount = 0,
        irsCompositionResidual = 0.0
      }

defaultInertiaRegionScorePolicy :: InertiaRegionScorePolicy
defaultInertiaRegionScorePolicy =
  InertiaRegionScorePolicy
    { irspCompositionResidualWeight = 1.0,
      irspCoverPairWeight = 1.0,
      irspStructuralSiteWeight = 1.0,
      irspMinimumSupportScale = 1
    }

scoreInertiaRegionSection ::
  Ord site =>
  (site -> Bool) ->
  Map site MassProperties ->
  Map site MassProperties ->
  InertiaRegionCoverBlueprint site ->
  InertiaRegionScore
scoreInertiaRegionSection isStructuralSite anchorMassPropertiesBySite valueBySite coverBlueprint =
  mconcat
    [ anchorSupportScore (Map.size anchorMassPropertiesBySite),
      coverPairScore (length (ircbCoverPairs coverBlueprint)),
      structuralSiteScore
        (length (filter isStructuralSite (Map.keys (ircbCellsBySite coverBlueprint)))),
      foldMap compositionResidualAtSite (Map.keys childSitesByParent)
    ]
  where
    childSitesByParent = coverChildrenByParent coverBlueprint

    compositionResidualAtSite parentSite =
      case Map.lookup parentSite childSitesByParent of
        Nothing ->
          mempty
        Just childSites ->
          case
            ( Map.lookup parentSite valueBySite,
              composeMassProperties =<< traverse (`Map.lookup` valueBySite) childSites
            )
            of
            (Just parentMassProperties, Just composedChildMassProperties) ->
              compositionResidualScore
                (massPropertiesResidual parentMassProperties composedChildMassProperties)
            _ ->
              mempty

inertiaRegionScoreValue :: InertiaRegionScore -> Double
inertiaRegionScoreValue =
  interpretInertiaRegionScore defaultInertiaRegionScorePolicy

interpretInertiaRegionScore :: InertiaRegionScorePolicy -> InertiaRegionScore -> Double
interpretInertiaRegionScore scorePolicy inertiaRegionScore =
  totalOf
    [ weightedComponent
        (irspCompositionResidualWeight scorePolicy)
        (irsCompositionResidual inertiaRegionScore),
      weightedComponent
        (irspCoverPairWeight scorePolicy)
        (fromIntegral (irsCoverPairCount inertiaRegionScore) / supportScale),
      weightedComponent
        (irspStructuralSiteWeight scorePolicy)
        (fromIntegral (irsStructuralSiteCount inertiaRegionScore) / supportScale)
    ]
  where
    supportScale =
      fromIntegral
        ( max
            (irspMinimumSupportScale scorePolicy)
            (irsAnchorSupport inertiaRegionScore)
        )

massPropertiesResidual :: MassProperties -> MassProperties -> Double
massPropertiesResidual left right =
  abs (massPropertiesMass left - massPropertiesMass right)
    + magnitudeVec3 (subVec3 (massPropertiesCenterOfMass left) (massPropertiesCenterOfMass right))
    + inertiaTensorResidual (massPropertiesInertiaTensor left) (massPropertiesInertiaTensor right)

inertiaTensorResidual :: Symmetric3 Double -> Symmetric3 Double -> Double
inertiaTensorResidual leftTensor rightTensor =
  foldr
    (\(leftValue, rightValue) totalResidual ->
        totalResidual + abs (leftValue - rightValue)
    )
    0.0
    (zip (symmetric3Entries leftTensor) (symmetric3Entries rightTensor))

anchorSupportScore :: Int -> InertiaRegionScore
anchorSupportScore anchorSupport =
  mempty
    { irsAnchorSupport = anchorSupport
    }

coverPairScore :: Int -> InertiaRegionScore
coverPairScore coverPairCount =
  mempty
    { irsCoverPairCount = coverPairCount
    }

structuralSiteScore :: Int -> InertiaRegionScore
structuralSiteScore structuralSiteCount =
  mempty
    { irsStructuralSiteCount = structuralSiteCount
    }

compositionResidualScore :: Double -> InertiaRegionScore
compositionResidualScore compositionResidual =
  mempty
    { irsCompositionResidual = compositionResidual
    }
