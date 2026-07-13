module Moonlight.EGraph.Introspection.NerveSpec.Site.Core
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.NerveSpec.Site.Prelude
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Moonlight.EGraph.Introspection.Analysis.Homotopy
  ( nerveHomotopyProfile,
    grothendieckHomotopyProfile,
  )

tests :: TestTree
tests =
  testGroup
    "core"
    [ testCase "site basis contains nontrivial simplices" testSiteBasis,
      testCase "restriction registry yields nilpotent coboundary" testCoboundary,
      testCase "Grothendieck restriction registry yields nilpotent coboundary" testGrothendieckCoboundary,
      testCase "homotopy profile agrees with disconnected rewrite data" testHomotopy,
      testCase "runtime polynomial presentation reuses system interfaces" testPolynomial,
      testCase "Grothendieck site materializes basis and face data" testGrothendieckSite,
      testCase "single-context Grothendieck homotopy uses the categorical nerve" testGrothendieckHomotopyAgreement,
      testCase "face restrictions reindex the visible 2-simplex boundary" testFaceRestriction,
      testCase "summary reports component and restriction counts" testSummary
    ]

testSiteBasis :: Assertion
testSiteBasis =
  assertBool
    "expected 0-, 1-, and 2-dimensional cells"
    ( not (null (siteCellsAtDimension reversibleSite 0))
        && not (null (siteCellsAtDimension reversibleSite 1))
        && not (null (siteCellsAtDimension reversibleSite 2))
    )

testCoboundary :: Assertion
testCoboundary =
  case explicitNerveCochain reversibleSite of
    Left shapeError ->
      assertFailure ("expected coboundary materialization to succeed, received " <> show shapeError)
    Right coboundaryCacheValue ->
      assertBool
        "expected coboundary nilpotence"
        (checkCoboundaryNilpotence coboundaryCacheValue)

testGrothendieckCoboundary :: Assertion
testGrothendieckCoboundary =
  case
    ( explicitGrothendieckCochain (mkGrothendieckSite reversibleSystem 2),
      absoluteDiagnostics reversibleSystem 2,
      multiContextSystemResult
    ) of
    (Left shapeError, _, _) ->
      assertFailure ("expected Grothendieck coboundary materialization to succeed, received " <> show shapeError)
    (_, Left failure, _) ->
      assertFailure (show failure)
    (_, _, Left failure) ->
      assertFailure (show failure)
    (Right grothendieckCache, Right absoluteValue, Right rewriteSystem) ->
      let nilpotenceValue = checkCoboundaryNilpotence grothendieckCache
       in do
        assertEqual
          "expected the single-context Grothendieck coboundary to agree with the flat rewrite-site coboundary"
          (either (const False) checkCoboundaryNilpotence (explicitNerveCochain reversibleSite))
          nilpotenceValue
        assertEqual
          "expected single-context absolute diagnostics to retain verified single-context nilpotence evidence"
          (if nilpotenceValue then SingleContextNilpotent else SingleContextNonNilpotent)
          (gssCoboundaryNilpotenceEvidence (adGrothendieckSummary absoluteValue))
        assertEqual
          "expected the multi-context law harness to discharge multi-context nilpotence directly"
          MultiContextNilpotent
          (grothendieckCoboundaryNilpotenceEvidence rewriteSystem 2)

explicitNerveCochain siteValue =
  buildNerveCochainArtifact
    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
    Right
    (MaterializedSite siteValue)

explicitGrothendieckCochain siteValue =
  buildGrothendieckCochainArtifact
    (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
    Right
    (MaterializedSite siteValue)

testHomotopy :: Assertion
testHomotopy =
  let expectedComponents = length (pi0Nerve (rsCategory disjointSystem))
   in case nerveHomotopyProfile disjointSystem (mkRewriteNerveSite disjointSystem 1) of
        Left failure ->
          assertFailure (show failure)
        Right profile -> do
          assertEqual "expected connected components to agree with pi0" expectedComponents (nhpConnectedComponents profile)
          case nhpBettiVector profile of
            betti0 : _ ->
              assertEqual "expected Betti-0 to agree with the component count" expectedComponents betti0
            [] ->
              assertFailure "expected a non-empty Betti vector"

testPolynomial :: Assertion
testPolynomial =
  let morphismPresentation = systemMorphismPolynomialPresentation reversibleSystem
      objectPresentation = systemObjectPolynomialPresentation reversibleSystem
      morphismPositionCount = length (ppPositions morphismPresentation)
      objectPositionCount = length (ppPositions objectPresentation)
      morphismInterfaceMatches =
        case ppPositions morphismPresentation of
          SystemMorphismPosition contextValue morphismValue : _ ->
            ppDirections morphismPresentation (SystemMorphismPosition contextValue morphismValue)
              == morphismInterface reversibleSystem morphismValue
          [] ->
            False
      nonEmptyOutgoingDirections =
        [ outgoingMorphisms
        | positionValue <- ppPositions objectPresentation
        , let outgoingMorphisms = ppDirections objectPresentation positionValue
        , not (null outgoingMorphisms)
        ]
   in do
        assertEqual
          "expected one morphism position per rule"
          (length (systemMorphisms reversibleSystem))
          morphismPositionCount
        assertEqual
          "expected one object position per rewrite object"
          (length (systemObjects reversibleSystem))
          objectPositionCount
        assertBool
          "expected morphism directions to reuse morphismInterface"
          morphismInterfaceMatches
        assertBool
          "expected at least one object to expose outgoing morphisms"
          (not (null nonEmptyOutgoingDirections))

testGrothendieckSite :: Assertion
testGrothendieckSite =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let grothendieckSite = mkGrothendieckSite rewriteSystem 2
       in do
            assertBool
              "expected the Grothendieck site to materialize non-empty cells"
              (not (null (grothendieckSiteCells grothendieckSite)))
            assertBool
              "expected the Grothendieck site to materialize face morphisms"
              (not (null (grothendieckSiteFaceMorphisms grothendieckSite)))
            assertEqual
              "expected Grothendieck 0-cells to match 0-simplices"
              (length (simplicesAtDimension (grothendieckSiteSourceNerve grothendieckSite) 0))
              (length (grothendieckSiteCellsAtDimension grothendieckSite 0))

testGrothendieckHomotopyAgreement :: Assertion
testGrothendieckHomotopyAgreement =
  case (nerveHomotopyProfile reversibleSystem reversibleSite, grothendieckHomotopyProfile reversibleSystem 2) of
    (Left failure, _) ->
      assertFailure (show failure)
    (_, Left failure) ->
      assertFailure (show failure)
    (Right flatProfile, Right grothendieckProfile) ->
      do
        assertEqual
          "expected the categorical Grothendieck nerve to preserve connected components"
          (nhpConnectedComponents flatProfile)
          (nhpConnectedComponents grothendieckProfile)
        assertEqual
          "expected the categorical Grothendieck nerve to retain the full depth-2 face witness"
          [1, 0, 1]
          (nhpBettiVector grothendieckProfile)

testFaceRestriction :: Assertion
testFaceRestriction =
  case siteCellsAtDimension reversibleSite 2 of
    [] ->
      assertFailure "expected a non-empty 2-simplex layer"
    sourceCell : _ ->
      let categoryValue = nerveSiteCategory reversibleSite
          boundaryFaces = filter ((== sourceCell) . faceMorphismSource) (siteFaceMorphisms reversibleSite)
       in do
            assertEqual
              "expected the visible normalized faces to preserve every 2-simplex face witness"
              [LeadingFace, InnerFace 0, TrailingFace]
              (fmap faceMorphismKind boundaryFaces)
            case traverse (targetStalkForFace categoryValue) boundaryFaces of
              Left projectionFailure ->
                assertFailure ("expected target stalk projection to succeed, received " <> show projectionFailure)
              Right restrictedStalks ->
                assertEqual
                  "expected targetStalkForFace to reconstruct the target stalk signature"
                  (fmap (interfaceStalkSignature . stalkFromCell categoryValue . faceMorphismTarget) boundaryFaces)
                  (fmap interfaceStalkSignature restrictedStalks)

testSummary :: Assertion
testSummary =
  case summarizeRewriteSystem reversibleSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right summaryValue -> do
      assertEqual "expected a single connected component" 1 (ssConnectedComponents summaryValue)
      assertBool "expected at least one restriction" (ssRestrictionCount summaryValue > 0)
      assertBool "expected nilpotent coboundary" (ssCoboundaryNilpotent summaryValue)
