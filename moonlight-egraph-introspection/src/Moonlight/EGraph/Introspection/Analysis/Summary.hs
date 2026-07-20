module Moonlight.EGraph.Introspection.Analysis.Summary
  ( StructuralSummary (..),
    summarizeRewriteSystem,
  )
where

import Data.Bifunctor (first)
import Moonlight.Analysis.Summary (StructuralSummaryModel (..), summarizeStructure)
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildNerveCochainArtifact,
  )
import Moonlight.EGraph.Introspection.Analysis.Homotopy (nerveHomotopyProfile)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, mkRewriteNerveSite)
import Moonlight.Sheaf.Site (nerveSiteBasis, siteFaceMorphisms)
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Moonlight.Homology (HomologyFailure (..))
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Numeric.Natural (Natural)
import Moonlight.Pale.Diagnostic.Global.Summary (StructuralSummary (..))

summarizeRewriteSystem ::
  ( HasConstructorTag f,
    ZipMatch f,
    Ord (Pattern f),
    Show (Pattern f)
  ) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure StructuralSummary
summarizeRewriteSystem rewriteSystem depthValue =
  let siteValue = mkRewriteNerveSite rewriteSystem depthValue
   in summarizeStructure
        StructuralSummaryModel
          { ssmCellCount = \siteVal ->
              length (basisCells (nerveSiteBasis siteVal)),
            ssmRestrictionCount = \siteVal ->
              length (siteFaceMorphisms siteVal),
            ssmCochainComplex = \siteVal ->
              first
                (BackendFailure . show)
                ( buildNerveCochainArtifact
                    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
                    Right
                    (MaterializedSite siteVal)
                ),
            ssmHomotopyProfile =
              nerveHomotopyProfile rewriteSystem
          }
        siteValue
