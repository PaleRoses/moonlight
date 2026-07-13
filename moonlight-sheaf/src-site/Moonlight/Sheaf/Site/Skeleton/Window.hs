{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Site.Skeleton.Window
  ( SiteSkeletonWindow (..),
    siteWindowDepth,
  )
where

import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Numeric.Natural (Natural)

data SiteSkeletonWindow = SiteSkeletonWindow
  { sswCellDimensions :: !(Set.Set Natural),
    sswFaceSourceDimensions :: !(Set.Set Natural)
  }
  deriving stock (Eq, Ord, Show)

siteWindowDepth :: SiteSkeletonWindow -> Natural
siteWindowDepth windowValue =
  fromMaybe 0 (Set.lookupMax (Set.union (sswCellDimensions windowValue) (sswFaceSourceDimensions windowValue)))
