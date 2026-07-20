module Moonlight.Analysis.InertiaSpec
  ( tests,
  )
where

import Moonlight.Analysis
  ( AABB,
    MassProperties (..),
    PointMass (..),
    Vec3 (..),
    centerOfMass,
    compileRegionDecompositionBoundaryAware,
    composeMassProperties,
    computeRegionInertia,
    computeRegionInertiaBoundaryAware,
    coverBlueprintFromDecomposition,
    defaultVoxelGrid,
    defaultBoundaryRefinement,
    InertiaRegionCoverBlueprint (..),
    InertiaRegionDecomposition (..),
    inertiaTensorAboutCenterOfMass,
    inertiaTensorAboutOrigin,
    massPropertiesFromPointMasses,
    mkAabb,
    mkRefinementDepth,
    principalInertia,
    RegionSubdivisionPath (..),
    translatePointMasses,
    uniformVoxelGrid,
    voxelGridDimensions,
  )
import Moonlight.LinAlg
  ( DiagonalizedSymmetric3 (..),
    Symmetric3 (..),
    symmetric3Entries,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

closeTo :: Double -> Double -> Double -> Bool
closeTo tolerance expected actual = abs (expected - actual) <= tolerance

closeTensor :: Double -> Symmetric3 Double -> Symmetric3 Double -> Bool
closeTensor tolerance expected actual =
  and (zipWith (closeTo tolerance) (symmetric3Entries expected) (symmetric3Entries actual))

closeVec3 :: Double -> Vec3 -> Vec3 -> Bool
closeVec3 tolerance expected actual =
  closeTo tolerance (vecX expected) (vecX actual)
    && closeTo tolerance (vecY expected) (vecY actual)
    && closeTo tolerance (vecZ expected) (vecZ actual)

absoluteError :: Double -> Double -> Double
absoluteError expected actual = abs (expected - actual)

sphereSdf :: Double -> Vec3 -> Double
sphereSdf radiusValue (Vec3 xValue yValue zValue) =
  sqrt (xValue * xValue + yValue * yValue + zValue * zValue) - radiusValue

boxIndicator :: Vec3 -> Vec3 -> Double
boxIndicator (Vec3 halfX halfY halfZ) (Vec3 xValue yValue zValue) =
  maximum [abs xValue - halfX, abs yValue - halfY, abs zValue - halfZ]

tests :: TestTree
tests =
  testGroup
    "inertia"
    [ testCase "single point mass yields the expected inertia tensor" $
        let tensorValue =
              inertiaTensorAboutOrigin
                [PointMass 1.0 (Vec3 0.0 1.0 0.0)]
         in assertBool
              "unit mass at (0,1,0) should produce diag(1,0,1)"
              ( closeTensor
                  1.0e-12
                  Symmetric3
                    { sym3XX = 1.0,
                      sym3XY = 0.0,
                      sym3XZ = 0.0,
                      sym3YY = 0.0,
                      sym3YZ = 0.0,
                      sym3ZZ = 1.0
                    }
                  tensorValue
              ),
      testCase "center-of-mass inertia is translation invariant" $
        let pointMasses =
              [ PointMass 1.0 (Vec3 1.0 0.0 0.0),
                PointMass 2.0 (Vec3 0.0 2.0 0.0),
                PointMass 1.5 (Vec3 0.0 0.0 3.0)
              ]
            shiftedMasses =
              translatePointMasses
                (Vec3 10.0 (-4.0) 7.5)
                pointMasses
         in case (inertiaTensorAboutCenterOfMass pointMasses, inertiaTensorAboutCenterOfMass shiftedMasses) of
              (Just referenceTensor, Just shiftedTensor) ->
                assertBool
                  "parallel translations should preserve the inertia tensor about the center of mass"
                  (closeTensor 1.0e-9 referenceTensor shiftedTensor)
              _ ->
                assertFailure "expected nonzero total mass",
      testCase "principalInertia reuses symmetric eigen decomposition for principal moments" $
        let tensorValue =
              inertiaTensorAboutOrigin
                [PointMass 1.0 (Vec3 0.0 1.0 0.0)]
         in case principalInertia tensorValue of
              Left err ->
                assertFailure ("unexpected principal inertia failure: " <> show err)
              Right DiagonalizedSymmetric3 {diag3XX = firstMoment, diag3YY = secondMoment, diag3ZZ = thirdMoment} ->
                assertBool
                  "principal moments should match the diagonal tensor spectrum"
                  ( closeTo 1.0e-9 1.0 firstMoment
                      && closeTo 1.0e-9 1.0 secondMoment
                      && closeTo 1.0e-9 0.0 thirdMoment
                  ),
      testCase "centerOfMass averages positions by mass" $
        case
          centerOfMass
            [ PointMass 1.0 (Vec3 0.0 0.0 0.0),
              PointMass 3.0 (Vec3 4.0 0.0 0.0)
            ]
          of
          Just (Vec3 centerX centerY centerZ) ->
            assertBool
              "weighted center should sit at x = 3"
              ( closeTo 1.0e-12 3.0 centerX
                  && closeTo 1.0e-12 0.0 centerY
                  && closeTo 1.0e-12 0.0 centerZ
              )
          Nothing ->
            assertFailure "expected a non-empty center of mass",
      testCase "default voxel grid is the intended 8x8x8 lattice" $
        voxelGridDimensions defaultVoxelGrid @?= (8, 8, 8),
      testCase "computeRegionInertia approximates a unit sphere" $
        case (uniformVoxelGrid 32, mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0)) of
          (Just gridValue, Just boundingBoxValue) ->
            case computeRegionInertia gridValue boundingBoxValue (sphereSdf 1.0) of
              Just
                MassProperties
                  { massPropertiesMass = massValue,
                    massPropertiesCenterOfMass = centerValue,
                    massPropertiesInertiaTensor = tensorValue
                  } ->
                    let expectedMass = (4.0 / 3.0) * pi
                        expectedMoment = (2.0 / 5.0) * expectedMass
                     in assertBool
                          "voxelized sphere should approximate isotropic analytic mass properties"
                          ( closeTo 2.5e-1 expectedMass massValue
                              && closeVec3 7.0e-2 (Vec3 0.0 0.0 0.0) centerValue
                              && closeTo 2.0e-1 expectedMoment (sym3XX tensorValue)
                              && closeTo 2.0e-1 expectedMoment (sym3YY tensorValue)
                              && closeTo 2.0e-1 expectedMoment (sym3ZZ tensorValue)
                              && closeTo 1.0e-1 0.0 (sym3XY tensorValue)
                              && closeTo 1.0e-1 0.0 (sym3XZ tensorValue)
                              && closeTo 1.0e-1 0.0 (sym3YZ tensorValue)
                          )
              Nothing ->
                assertFailure "expected non-empty sphere mass properties"
          _ ->
            assertFailure "expected valid sphere sampling inputs",
      testCase "computeRegionInertia approximates a centered cube" $
        case (uniformVoxelGrid 24, mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0)) of
          (Just gridValue, Just boundingBoxValue) ->
            case computeRegionInertia gridValue boundingBoxValue (boxIndicator (Vec3 1.0 1.0 1.0)) of
              Just
                MassProperties
                  { massPropertiesMass = massValue,
                    massPropertiesCenterOfMass = centerValue,
                    massPropertiesInertiaTensor = tensorValue
                  } ->
                    let expectedMass = 8.0
                        expectedMoment = 16.0 / 3.0
                     in assertBool
                          "voxelized cube should approximate analytic cuboid mass properties"
                          ( closeTo 1.0e-9 expectedMass massValue
                              && closeVec3 1.0e-9 (Vec3 0.0 0.0 0.0) centerValue
                              && closeTo 1.2e-1 expectedMoment (sym3XX tensorValue)
                              && closeTo 1.2e-1 expectedMoment (sym3YY tensorValue)
                              && closeTo 1.2e-1 expectedMoment (sym3ZZ tensorValue)
                          )
              Nothing ->
                assertFailure "expected non-empty cube mass properties"
          _ ->
            assertFailure "expected valid cube sampling inputs",
      testCase "boundary-aware refinement improves coarse cube boundary mass and inertia" $
        case mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0) of
          Just boundingBoxValue ->
            case
              ( computeRegionInertia defaultVoxelGrid boundingBoxValue (boxIndicator (Vec3 0.9 0.9 0.9)),
                computeRegionInertiaBoundaryAware defaultVoxelGrid defaultBoundaryRefinement boundingBoxValue (boxIndicator (Vec3 0.9 0.9 0.9))
              )
              of
              (Just coarseProperties, Just refinedProperties) ->
                let expectedMass = 1.8 ** 3
                    expectedMoment = expectedMass * (1.8 * 1.8 + 1.8 * 1.8) / 12.0
                 in assertBool
                      "boundary-aware integration should beat midpoint occupancy on a subcell cube"
                      ( absoluteError expectedMass (massPropertiesMass refinedProperties)
                          < absoluteError expectedMass (massPropertiesMass coarseProperties)
                          && absoluteError expectedMoment (sym3XX (massPropertiesInertiaTensor refinedProperties))
                          < absoluteError expectedMoment (sym3XX (massPropertiesInertiaTensor coarseProperties))
                      )
              _ ->
                assertFailure "expected both coarse and refined cube mass properties"
          Nothing ->
            assertFailure "expected valid cube boundary inputs",
      testCase "boundary-aware refinement improves coarse sphere mass" $
        case (mkRefinementDepth 2, mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0)) of
          (Just refinementDepthValue, Just boundingBoxValue) ->
            case
              ( computeRegionInertia defaultVoxelGrid boundingBoxValue (sphereSdf 1.0),
                computeRegionInertiaBoundaryAware defaultVoxelGrid refinementDepthValue boundingBoxValue (sphereSdf 1.0)
              )
              of
              (Just coarseProperties, Just refinedProperties) ->
                let expectedMass = (4.0 / 3.0) * pi
                 in assertBool
                      "boundary-aware sphere mass should be closer to the analytic volume than midpoint occupancy"
                      ( absoluteError expectedMass (massPropertiesMass refinedProperties)
                          < absoluteError expectedMass (massPropertiesMass coarseProperties)
                      )
              _ ->
                assertFailure "expected both coarse and refined sphere mass properties"
          _ ->
            assertFailure "expected valid sphere boundary inputs",
      testCase "boundary-aware compilation emits a region decomposition tree" $
        case (mkRefinementDepth 1, mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0)) of
          (Just refinementDepthValue, Just boundingBoxValue) ->
            case
              compileRegionDecompositionBoundaryAware
                refinementDepthValue
                boundingBoxValue
                (boxIndicator (Vec3 0.9 0.9 0.9))
                (\path _ -> path)
              of
              Just decomposition ->
                let coverBlueprint = coverBlueprintFromDecomposition decomposition
                 in assertBool
                      "boundary-aware decomposition should retain the root and its eight refined children"
                      ( case decomposition of
                          InertiaRegionDecomposition
                            { irdSite = RegionSubdivisionPath [],
                              irdChildren = childDecompositions
                            } ->
                              length childDecompositions == 8
                                && length (ircbCoverPairs coverBlueprint) == 8
                          _ ->
                            False
                      )
              Nothing ->
                assertFailure "expected a non-empty decomposition for a partially occupied region"
          _ ->
            assertFailure "expected valid decomposition inputs",
      testCase "boundary-aware compilation rejects fully empty regions" $
        case mkRefinementDepth 1 of
          Just refinementDepthValue ->
            case
              compileRegionDecompositionBoundaryAware
                refinementDepthValue
                farAwayBoundingBox
                (boxIndicator (Vec3 0.25 0.25 0.25))
                (\path _ -> path)
              of
              Nothing ->
                pure ()
              Just decomposition ->
                assertFailure ("expected no decomposition, got " <> show decomposition)
          Nothing ->
            assertFailure "expected valid decomposition refinement depth",
      testCase "composeMassProperties applies the parallel-axis theorem" $
        case
          ( massPropertiesFromPointMasses [PointMass 1.0 (Vec3 (-1.0) 0.0 0.0)],
            massPropertiesFromPointMasses [PointMass 1.0 (Vec3 1.0 0.0 0.0)]
          )
          of
          (Just leftProperties, Just rightProperties) ->
            case composeMassProperties [leftProperties, rightProperties] of
              Just
                MassProperties
                  { massPropertiesMass = massValue,
                    massPropertiesCenterOfMass = centerValue,
                    massPropertiesInertiaTensor = tensorValue
                  } ->
                    assertBool
                      "two unit point masses at ±1 should yield diag(0,2,2) about the combined center"
                      ( closeTo 1.0e-12 2.0 massValue
                          && closeVec3 1.0e-12 (Vec3 0.0 0.0 0.0) centerValue
                          && closeTensor
                            1.0e-12
                            Symmetric3
                              { sym3XX = 0.0,
                                sym3XY = 0.0,
                                sym3XZ = 0.0,
                                sym3YY = 2.0,
                                sym3YZ = 0.0,
                                sym3ZZ = 2.0
                              }
                            tensorValue
                      )
              Nothing ->
                assertFailure "expected composed point-mass properties"
          _ ->
            assertFailure "expected one-point mass properties for each branch"
    ]

farAwayBoundingBox :: AABB
farAwayBoundingBox =
  case mkAabb (Vec3 2.0 2.0 2.0) (Vec3 3.0 3.0 3.0) of
    Just boundingBoxValue ->
      boundingBoxValue
    Nothing ->
      error "invalid far-away bounding box fixture"
