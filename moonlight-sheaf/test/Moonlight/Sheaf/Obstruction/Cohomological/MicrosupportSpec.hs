module Moonlight.Sheaf.Obstruction.Cohomological.MicrosupportSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.LivePruning
  ( nonCriticalNodesFromLiveMicrosupport,
    recomputeLiveMicrosupport,
    staticComponentIndex,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Microsupport
  ( computeMicrosupportEnrichment,
    computeNerveMicrosupportEnrichment,
    nerveSiteToPoset,
  )
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildNerveCochainArtifact,
  )
import Moonlight.Pale.Diagnostic.Site.Cohomology (CoboundaryConstructionError)
import Moonlight.Sheaf.Operator.GradedComplex (GradedComplex)
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    NerveCell,
    NerveSite,
    nerveCellKey,
    siteCellsAtDimension,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization (interfaceStalkBasisLinearization)
import Moonlight.Sheaf.TestFixture.Site (SampleSiteTag, nodeCellKey, sampleNerveSite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "microsupport"
    [ testCase "generic microsupport agrees with the nerve adapter on a non-egraph fixture" testMicrosupportAdapterAgreement,
      testCase "live microsupport recomputation closes seeded cells downward on the local site" testLiveMicrosupportRecomputation
    ]

testMicrosupportAdapterAgreement :: Assertion
testMicrosupportAdapterAgreement =
  case (nerveSiteToPoset sampleNerveSite, explicitNerveCochain sampleNerveSite) of
    (Left failure, _) ->
      assertFailure ("expected the generic site to admit a poset presentation, received " <> show failure)
    (_, Left shapeError) ->
      assertFailure ("expected generic coboundary materialization to succeed, received " <> show shapeError)
    (Right posetValue, Right cacheValue) ->
      let genericResult =
            computeMicrosupportEnrichment
              (FinObjectId . ckOrdinal . nerveCellKey)
              posetValue
              (nodeCellKey sampleNerveSite)
              cacheValue
          nerveResult =
            computeNerveMicrosupportEnrichment
              (nodeCellKey sampleNerveSite)
              sampleNerveSite
              cacheValue
       in assertEqual
            "expected the generic microsupport kernel and the nerve adapter to agree"
            genericResult
            nerveResult

explicitNerveCochain ::
  NerveSite SampleSiteTag ->
  Either CoboundaryConstructionError (GradedComplex (NerveCell SampleSiteTag) Int)
explicitNerveCochain siteValue =
  buildNerveCochainArtifact
    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
    Right
    (MaterializedSite siteValue)

testLiveMicrosupportRecomputation :: Assertion
testLiveMicrosupportRecomputation =
  case siteCellsAtDimension sampleNerveSite 0 of
    [] ->
      assertFailure "expected the generic nerve fixture to contain a root cell"
    rootCell : _ ->
      let seededCellKeys =
            Set.singleton (nerveCellKey rootCell)
          liveMicrosupport =
            recomputeLiveMicrosupport
              (nodeCellKey sampleNerveSite)
              Just
              (staticComponentIndex sampleNerveSite)
              seededCellKeys
          nonCriticalNodes =
            nonCriticalNodesFromLiveMicrosupport liveMicrosupport
       in do
            assertBool
              "expected noncritical nodes to remain inside the seeded support"
              (nonCriticalNodes `Set.isSubsetOf` seededCellKeys)
