{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.EGraph.Introspection.Analysis.Cohomology
  ( CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
    rewriteGrothendieckCoboundaryNilpotenceEvidence,
  )
where

import Moonlight.Analysis.Cohomology
  ( CoboundaryNilpotenceEvidence (..),
    coboundaryNilpotenceEvidenceFromResult,
    evidenceNilpotent,
  )
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, rsContexts)
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildGrothendieckCochainArtifact,
  )
import Moonlight.Sheaf.Site (mkGrothendieckSite)
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Numeric.Natural (Natural)

rewriteGrothendieckCoboundaryNilpotenceEvidence ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  CoboundaryNilpotenceEvidence
rewriteGrothendieckCoboundaryNilpotenceEvidence rewriteSystem depthValue =
  coboundaryNilpotenceEvidenceFromResult
    (length (rsContexts rewriteSystem))
    ( buildGrothendieckCochainArtifact
        (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
        Right
        (MaterializedSite (mkGrothendieckSite rewriteSystem depthValue))
    )
