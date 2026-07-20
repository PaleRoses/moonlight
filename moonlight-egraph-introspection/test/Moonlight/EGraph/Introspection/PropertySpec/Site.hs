module Moonlight.EGraph.Introspection.PropertySpec.Site
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
import Moonlight.EGraph.Introspection.PropertySpec.Fixture
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildGrothendieckCochainArtifact,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization (interfaceStalkBasisLinearization)
import Moonlight.EGraph.Introspection.Analysis.Homotopy (nerveHomotopyProfile)
import Moonlight.Category.Simplicial (pi0Nerve)

tests :: TestTree
tests =
  testGroup
    "site"
    [ testProperty "Grothendieck coboundary remains nilpotent" propCoboundaryNilpotence,
      testProperty "nerve connected components agree with categorical pi0" propPi0MatchesProfileComponents
    ]

propCoboundaryNilpotence :: GeneratedRewriteSystem -> Property
propCoboundaryNilpotence generatedRewriteSystem =
  generatedRewriteSystemProperty generatedRewriteSystem $ \rewriteSystem ->
    case explicitGrothendieckCochain (mkGrothendieckSite rewriteSystem (fromIntegral analysisDepth)) of
      Left shapeError ->
        Left ("Grothendieck coboundary materialization failed: " <> show shapeError)
      Right coboundaryCacheValue ->
        Right (checkCoboundaryNilpotence coboundaryCacheValue)

explicitGrothendieckCochain siteValue =
  buildGrothendieckCochainArtifact
    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
    Right
    (MaterializedSite siteValue)

propPi0MatchesProfileComponents :: GeneratedRewriteSystem -> Property
propPi0MatchesProfileComponents generatedRewriteSystem =
  generatedRewriteSystemProperty generatedRewriteSystem $ \rewriteSystem ->
    let siteValue = mkRewriteNerveSite rewriteSystem (fromIntegral analysisDepth)
     in case nerveHomotopyProfile rewriteSystem siteValue of
          Left failure ->
            Left ("nerve homotopy failed: " <> show failure)
          Right homotopyProfileValue ->
            Right
              (nhpConnectedComponents homotopyProfileValue == length (pi0Nerve (rsCategory rewriteSystem)))
