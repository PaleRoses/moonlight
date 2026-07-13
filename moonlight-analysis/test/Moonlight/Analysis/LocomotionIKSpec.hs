module Moonlight.Analysis.LocomotionIKSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Analysis
  ( ContactType (..),
    FootPlacementSpec (..),
    IKChain,
    SurfaceHit (..),
    TerrainOracle (..),
    Vec3 (..),
    defaultFootPlacementSpec,
    endEffector,
    mkIKChain,
    sampleConeCandidates,
    searchFoothold,
    solveFabrik,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

closeTo :: Double -> Double -> Double -> Bool
closeTo tolerance expected actual = abs (expected - actual) <= tolerance

tests :: TestTree
tests =
  testGroup
    "locomotion-ik"
    [ testCase "tier one raycast foothold wins when directly available" testRaycastFoothold,
      testCase "tier two cone search finds a nearby ledge" testConeSearchFoothold,
      testCase "cone candidate enumeration is deterministic and eight-wide" testConeCandidateShape,
      testCase "fabrik reaches a reachable target" testFabrikReachable,
      testCase "fabrik saturates to max reach for unreachable targets" testFabrikUnreachable
    ]

flatOracle :: TerrainOracle
flatOracle =
  TerrainOracle
    { terrainRaycastDown = \(Vec3 xValue _ zValue) -> Just (SurfaceHit (Vec3 xValue 0.0 zValue) (Vec3 0.0 1.0 0.0)),
      terrainSignedDistance = \(Vec3 _ yValue _) -> yValue,
      terrainSurfaceNormal = const (Vec3 0.0 1.0 0.0)
    }

ledgeOracle :: TerrainOracle
ledgeOracle =
  TerrainOracle
    { terrainRaycastDown = const Nothing,
      terrainSignedDistance = \(Vec3 xValue yValue zValue) -> distanceToPoint (Vec3 0.5 0.1339745962155614 0.0) (Vec3 xValue yValue zValue),
      terrainSurfaceNormal = const (Vec3 0.0 1.0 0.0)
    }

reachableChain :: IKChain
reachableChain =
  mkIKChain
    ( Vec3 0.0 0.0 0.0 :|
      [ Vec3 1.0 0.0 0.0,
        Vec3 2.0 0.0 0.0
      ]
    )

testRaycastFoothold :: IO ()
testRaycastFoothold =
  case searchFoothold (defaultFootPlacementSpec FootContact) flatOracle (Vec3 0.0 1.0 0.0) of
    Just hitValue ->
      assertBool
        "raycast hit should be returned unchanged"
        (surfaceHitPosition hitValue == Vec3 0.0 0.0 0.0)
    Nothing ->
      assertBool "expected a foothold" False

testConeSearchFoothold :: IO ()
testConeSearchFoothold =
  case searchFoothold (defaultFootPlacementSpec FootContact) ledgeOracle (Vec3 0.0 1.0 0.0) of
    Just hitValue ->
      let Vec3 xValue yValue zValue = surfaceHitPosition hitValue
       in assertBool
            "cone search should recover a nearby ledge candidate"
            (closeTo 1.0e-9 0.5 xValue && closeTo 1.0e-9 0.1339745962155614 yValue && closeTo 1.0e-9 0.0 zValue)
    Nothing ->
      assertBool "expected a cone-search foothold" False

testConeCandidateShape :: IO ()
testConeCandidateShape =
  let spec = (defaultFootPlacementSpec ClawContact) {footPlacementMaxSearchRadius = 2.0}
      candidates = sampleConeCandidates spec (Vec3 0.0 1.0 0.0)
   in case candidates of
        firstCandidate : _ ->
          let Vec3 xValue yValue zValue = firstCandidate
           in assertBool
                "cone sampler should emit the canonical eight candidates"
                ( length candidates == 8
                    && closeTo 1.0e-12 1.0 xValue
                    && closeTo 1.0e-12 (-0.7320508075688774) yValue
                    && closeTo 1.0e-12 0.0 zValue
                )
        [] ->
          assertBool "cone sampler should not be empty" False

testFabrikReachable :: IO ()
testFabrikReachable =
  let solvedChain = solveFabrik 32 1.0e-4 (Vec3 1.5 1.0 0.0) reachableChain
      Vec3 xValue yValue zValue = endEffector solvedChain
   in assertBool
        "reachable targets should be solved within tolerance"
        (closeTo 1.0e-2 1.5 xValue && closeTo 1.0e-2 1.0 yValue && closeTo 1.0e-2 0.0 zValue)

testFabrikUnreachable :: IO ()
testFabrikUnreachable =
  let solvedChain = solveFabrik 32 1.0e-4 (Vec3 3.0 4.0 0.0) reachableChain
      Vec3 xValue yValue zValue = endEffector solvedChain
   in assertBool
        "unreachable targets should clamp to total reach"
        (closeTo 1.0e-2 1.2 xValue && closeTo 1.0e-2 1.6 yValue && closeTo 1.0e-2 0.0 zValue)

distanceToPoint :: Vec3 -> Vec3 -> Double
distanceToPoint leftValue rightValue =
  sqrt
    ( (vecX leftValue - vecX rightValue) ^ (2 :: Int)
        + (vecY leftValue - vecY rightValue) ^ (2 :: Int)
        + (vecZ leftValue - vecZ rightValue) ^ (2 :: Int)
    )
