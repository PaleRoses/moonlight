{-# LANGUAGE DataKinds #-}

-- | Canonical publication of an exact site section through the existing bulk
-- loader. Join, finite-set algebra, and constrained union share this boundary;
-- none owns a private point/payload carrier or a second constructor.
module Moonlight.Triangulation.Internal.Join.Rebuild
  ( rebuildCanonicalSiteSet
  ) where

import qualified Data.Vector as V
import Moonlight.Triangulation.BulkLoad
  ( DuplicatePayloadPolicy (KeepFirstPayload)
  , delaunayFromCoordinates
  )
import Moonlight.Triangulation.Internal.Canonical (canonicalize)
import Moonlight.Triangulation.Internal.Join.SiteSet (SiteSet, siteSetAssocs)
import Moonlight.Triangulation.Internal.Representation
  ( BuildResult (buildTriangulation)
  , Triangulation
  )
import Moonlight.Triangulation.Internal.Types
  ( BuildError
  , ConstraintMode (Unconstrained)
  , unitElementDefaults
  )

rebuildCanonicalSiteSet
  :: SiteSet annotation
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
rebuildCanonicalSiteSet sites = do
  built <-
    delaunayFromCoordinates
      unitElementDefaults
      (V.fromList (fmap fst associations))
      (V.fromList (fmap snd associations))
      KeepFirstPayload
  canonicalize (buildTriangulation built)
 where
  associations = siteSetAssocs sites
