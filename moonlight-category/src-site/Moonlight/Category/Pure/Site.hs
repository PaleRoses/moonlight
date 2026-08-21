
-- | The site and path presentation layer (re-export): site manifests, path and thin
-- path categories, quotients, validation, and compilation to a finite category.
module Moonlight.Category.Pure.Site
  ( SiteManifest (..),
    SiteViolation (..),
    SiteFinCatError (..),
    ThinSiteKernel,
    ThinSitePresentation (..),
    SitePathCategory,
    SitePathObject,
    SitePathMorphism,
    PathThinCat,
    PathThinObject,
    PathThinMorphism,
    SitePathQuotient,
    SitePathQuotientError (..),
    mkSiteManifest,
    validateSiteManifest,
    thinSiteKernel,
    thinSitePresentation,
    thinPresentationToFinCat,
    sitePathCategory,
    sitePathManifest,
    mkSitePathObject,
    mkSitePathMorphism,
    sitePathMorphismsBetween,
    pathThinCat,
    mkPathThinObject,
    mkPathThinMorphism,
    quotientPathThinObject,
    quotientPathThinMorphism,
    pathThinCodomainObject,
    pathThinCodomainMorphism,
    sitePathQuotient,
    quotientMapObject,
    quotientMapMorphism,
    siteImportsAsFinCat,
    siteImportEdges,
    siteReachable,
  )
where

import Moonlight.Category.Pure.Site.Category as X
import Moonlight.Category.Pure.Site.Compile as X
  ( ThinSiteKernel,
    ThinSitePresentation (..),
    siteImportsAsFinCat,
    thinPresentationToFinCat,
    thinSiteKernel,
    thinSitePresentation,
  )
import Moonlight.Category.Pure.Site.Core as X
import Moonlight.Category.Pure.Site.Graph as X (siteImportEdges, siteReachable)
import Moonlight.Category.Pure.Site.Manifest as X
import Moonlight.Category.Pure.Site.Quotient as X
