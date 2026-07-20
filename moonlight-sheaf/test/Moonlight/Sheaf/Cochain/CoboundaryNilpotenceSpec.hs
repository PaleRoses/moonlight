{-# LANGUAGE TypeApplications #-}

module Moonlight.Sheaf.Cochain.CoboundaryNilpotenceSpec
  ( tests,
  )
where

import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category
  ( FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    mkFinCat,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( buildCoboundaryComplex,
  )
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildNerveCochainArtifact,
  )
import Moonlight.Sheaf.Cochain.Laplacian
  ( buildHodgeLaplacian0,
    buildHodgeLaplacian1,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( NerveSite,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    mkNerveSite,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.TestFixture.Assertions (assertRight)
import Moonlight.Sheaf.TestFixture.Site (SampleSiteTag)
import Moonlight.Sheaf.TestFixture.Triangle
  ( TriangleCell,
    triangleCoboundarySpec0,
    triangleCoboundarySpec1,
    triangleRestrictionIndex,
    triangleUnitCoboundaryBlock,
    triangleUnitStalkDimension,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

newtype TriangleStalk = TriangleStalk ()
  deriving stock (Eq, Show)

stalkAt :: TriangleCell -> TriangleStalk
stalkAt _ = TriangleStalk ()

testUnitTwoSimplexIsNilpotent :: Assertion
testUnitTwoSimplexIsNilpotent = do
  restrictions <-
    assertRight
      "triangle restriction index"
      triangleRestrictionIndex
  case buildCoboundaryComplex
    stalkAt
    triangleUnitStalkDimension
    triangleUnitCoboundaryBlock
    triangleCoboundarySpec0
    triangleCoboundarySpec1
    restrictions of
    Right _ -> pure ()
    Left err ->
      assertFailure
        ( "expected unit 2-simplex to satisfy d^2 = 0 through the nerve-style coboundary pipeline, but got: "
            <> show err
        )

mkIdempotentLoopSite :: Either String (NerveSite SampleSiteTag)
mkIdempotentLoopSite =
  case
    mkFinCat
      (Set.singleton (FinObjectId 0))
      (Map.singleton (FinObjectId 0, FinObjectId 0) [FinGeneratorMorphismId (FinGeneratorId 10)])
      (Map.singleton (FinGeneratorMorphismId (FinGeneratorId 10), FinGeneratorMorphismId (FinGeneratorId 10)) (FinGeneratorMorphismId (FinGeneratorId 10))) of
    Left err ->
      Left (show err)
    Right categoryValue ->
      Right (mkNerveSite @SampleSiteTag categoryValue 2)

testNerveCoboundaryPreservesDuplicateFaceWitnesses :: Assertion
testNerveCoboundaryPreservesDuplicateFaceWitnesses =
  case mkIdempotentLoopSite of
    Left err ->
      assertFailure ("expected idempotent loop category to build, but got: " <> err)
    Right siteValue -> do
      let duplicateOrientations =
            Map.fromListWith (<>)
              ( fmap
                  (\faceMorphism -> ((faceMorphismSource faceMorphism, faceMorphismTarget faceMorphism), [faceMorphismOrientation faceMorphism]))
                  (siteFaceMorphisms siteValue)
              )
              & Map.elems
              & fmap sort
      assertBool
        "expected an idempotent loop nerve with repeated source-target faces"
        ([-1, 1, 1] `elem` duplicateOrientations)
      case
        ( buildNerveCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) Right (MaterializedSite siteValue),
          buildNerveCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) buildHodgeLaplacian0 (MaterializedSite siteValue),
          buildNerveCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) buildHodgeLaplacian1 (MaterializedSite siteValue)
        ) of
        (Right _, Right _, Right _) ->
          pure ()
        (Left coboundaryError, _, _) ->
          assertFailure
            ( "expected duplicate-face nerve site to preserve witness-local orientations across coboundary and Laplacian assembly, but got "
                <> show coboundaryError
            )
        (_, Left laplacian0Error, _) ->
          assertFailure
            ( "expected duplicate-face nerve site to preserve witness-local orientations across coboundary and Laplacian assembly, but Hodge 0 failed with "
                <> show laplacian0Error
            )
        (_, _, Left laplacian1Error) ->
          assertFailure
            ( "expected duplicate-face nerve site to preserve witness-local orientations across coboundary and Laplacian assembly, but Hodge 1 failed with "
                <> show laplacian1Error
            )

tests :: TestTree
tests =
  testGroup
    "coboundary-nilpotence"
    [ testCase
        "standard 2-simplex with face-index orientation and unit stalks satisfies d^2 = 0"
        testUnitTwoSimplexIsNilpotent,
      testCase
        "nerve coboundary preserves duplicate face witnesses instead of collapsing them by cell pair"
        testNerveCoboundaryPreservesDuplicateFaceWitnesses
    ]
