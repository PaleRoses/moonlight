{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module Moonlight.Analysis.InertiaSheafRefinementSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Analysis
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compilePatternQuery,
    singlePatternQuery,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    combineCompiledGuards,
    compileGuard,
  )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Core (Substitution (..))
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    emptyEGraph,
  )
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Core
  ( ContinuousBinding (..),
    ContinuousSubstitution (..),
    FuzzyMatch (..),
    FuzzyRank (..),
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount,
    analysisSpec,
  )
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Refiner (CompiledSeedMatcher)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "inertia-sheaf-refinement"
    [ testCase "minimum anchor count rejects negative values" $
        mkMinimumAnchorCount (-1) @?= Nothing,
      testCase "score policy scalarizes composable score components through an explicit interpreter" $ do
        let baseScore =
              InertiaRegionScore
                { irsAnchorSupport = 4,
                  irsCoverPairCount = 4,
                  irsStructuralSiteCount = 1,
                  irsCompositionResidual = 0.5
                }
            customPolicy =
              defaultInertiaRegionScorePolicy
                { irspCompositionResidualWeight = 2.0,
                  irspCoverPairWeight = 0.5,
                  irspStructuralSiteWeight = 4.0,
                  irspMinimumSupportScale = 2
                }
        assertApproxEqual
          "expected default score alias to match the default interpreter"
          (interpretInertiaRegionScore defaultInertiaRegionScorePolicy baseScore)
          (inertiaRegionScoreValue baseScore)
        assertApproxEqual
          "expected custom score policy to rescale components"
          2.5
          (interpretInertiaRegionScore customPolicy baseScore),
      testCase "custom score policy threads through the inertia refiner" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        customEnergy <-
          expectSingleRank
            "expected one custom-policy refined match"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner compiledQuery customScorePolicyRefiner) compiledQuery emptyGraph)
        assertApproxEqual "expected custom-policy refiner energy" 2.0 customEnergy,
      testCase "compiled-query refinement reuses composed root mass properties and preserves exact witnesses" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        case refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner compiledQuery refiner) compiledQuery emptyGraph of
          [matchValue] -> do
            fmRootClass matchValue @?= ClassId 8
            fmDiscreteSubstitution matchValue
              @?= Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)])
            fmContinuousSubstitution matchValue @?= expectedContinuousSubstitution
            fmDetail matchValue
              @?= InertiaRegionRefinementDetail
                { irdAnchorCount = 3,
                  irdSiteCount = 3,
                  irdCoverPairCount = 2
                }
            assertApproxEqual "expected flat refinement rank" (2.0 / 3.0) (unFuzzyRank (fmRank matchValue))
          refinedMatches ->
            assertFailure ("expected one refined match, got " <> show refinedMatches),
      testCase "geometry-aware cover construction rejects candidates without containment" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner compiledQuery geometrylessRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "restriction checking rejects composition-inconsistent anchored mass properties" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner compiledQuery inconsistentRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "witness relabeling rejects decompositions with unmapped witness paths" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner compiledQuery unmatchedPathRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "witness relabeling rejects decompositions with extra descendants under witness leaf paths" $ do
        compiledQuery <- expectCompiledQuery nestedPattern
        refineSheafCompiledWithMatcher (seedMatcher nestedSeedBackend) (prepareRefiner compiledQuery witnessLeafDescendantRefiner) compiledQuery emptyGraph
          @?= [],
      testCase "pattern-derived structural sites support deeper decompositions" $ do
        compiledQuery <- expectCompiledQuery nestedPattern
        case refineSheafCompiledWithMatcher (seedMatcher nestedSeedBackend) (prepareRefiner compiledQuery nestedRefiner) compiledQuery emptyGraph of
          [matchValue] -> do
            fmRootClass matchValue @?= ClassId 8
            fmDiscreteSubstitution matchValue
              @?= Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81), (2, ClassId 82)])
            fmContinuousSubstitution matchValue @?= expectedNestedContinuousSubstitution
            fmDetail matchValue
              @?= InertiaRegionRefinementDetail
                { irdAnchorCount = 4,
                  irdSiteCount = 5,
                  irdCoverPairCount = 4
                }
            assertApproxEqual "expected nested refinement rank" 1.25 (unFuzzyRank (fmRank matchValue))
          refinedMatches ->
            assertFailure ("expected one nested refined match, got " <> show refinedMatches),
      testCase "domain score penalizes deeper structural covers more than flat covers" $ do
        flatQuery <- expectCompiledQuery samplePattern
        nestedQuery <- expectCompiledQuery nestedPattern
        flatEnergy <-
          expectSingleRank
            "expected one flat refined match"
            (refineSheafCompiledWithMatcher (seedMatcher seedBackend) (prepareRefiner flatQuery refiner) flatQuery emptyGraph)
        nestedEnergy <-
          expectSingleRank
            "expected one nested refined match"
            (refineSheafCompiledWithMatcher (seedMatcher nestedSeedBackend) (prepareRefiner nestedQuery nestedRefiner) nestedQuery emptyGraph)
        assertBool "expected nested refinement energy to exceed flat refinement energy" (nestedEnergy > flatEnergy)
    ]

emptyGraph :: EGraph ArithF NodeCount
emptyGraph = emptyEGraph analysisSpec

expectCompiledQuery :: Pattern ArithF -> IO (CompiledPatternQuery (CompiledGuard () ArithF) ArithF)
expectCompiledQuery patternValue =
  case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
    Left unboundPatternVars ->
      assertFailure
        ("expected compiled query to validate, got unbound vars " <> show unboundPatternVars)
    Right queryValue ->
      pure queryValue

expectSingleRank ::
  String ->
  [FuzzyMatch InertiaRegionSite MassProperties InertiaRegionRefinementDetail InertiaRegionScore Double] ->
  IO Double
expectSingleRank failureMessage refinedMatches =
  case refinedMatches of
    [matchValue] ->
      pure (unFuzzyRank (fmRank matchValue))
    _ ->
      assertFailure (failureMessage <> ", got " <> show refinedMatches)

assertApproxEqual :: String -> Double -> Double -> IO ()
assertApproxEqual failureMessage expectedValue observedValue =
  assertBool
    failureMessage
    (abs (expectedValue - observedValue) <= 1.0e-9)

prepareRefiner ::
  CompiledPatternQuery (CompiledGuard () ArithF) ArithF ->
  SheafRefiner InertiaRegionRefinementModel ->
  SheafRefiner InertiaRegionRefinementModel
prepareRefiner compiledQuery (SheafRefiner model) =
  SheafRefiner (prepareInertiaRegionModel compiledQuery model)

samplePattern :: Pattern ArithF
samplePattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

nestedPattern :: Pattern ArithF
nestedPattern =
  PatternNode
    ( Add
        (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
        (PatternVar (EGraph.mkPatternVar 2))
    )

type SeedBackend :: Type
data SeedBackend = SeedBackend
  { compiledSeeds :: [(ClassId, Substitution)]
  }

seedMatcher :: SeedBackend -> CompiledSeedMatcher ArithF
seedMatcher backend _ _ =
  compiledSeeds backend

seedBackend :: SeedBackend
seedBackend =
  SeedBackend
    { compiledSeeds = sampleSeeds
    }

nestedSeedBackend :: SeedBackend
nestedSeedBackend =
  SeedBackend
    { compiledSeeds = nestedSampleSeeds
    }

sampleSeeds :: [(ClassId, Substitution)]
sampleSeeds =
  [(ClassId 8, Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)]))]

nestedSampleSeeds :: [(ClassId, Substitution)]
nestedSampleSeeds =
  [(ClassId 8, Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81), (2, ClassId 82)]))]

refiner :: SheafRefiner InertiaRegionRefinementModel
refiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupConsistentMassProperties
    containedRegionDecomposition

geometrylessRefiner :: SheafRefiner InertiaRegionRefinementModel
geometrylessRefiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupConsistentMassProperties
    geometrylessRegionDecomposition

inconsistentRefiner :: SheafRefiner InertiaRegionRefinementModel
inconsistentRefiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupInconsistentMassProperties
    containedRegionDecomposition

unmatchedPathRefiner :: SheafRefiner InertiaRegionRefinementModel
unmatchedPathRefiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupConsistentMassProperties
    unmatchedPathRegionDecomposition

witnessLeafDescendantRefiner :: SheafRefiner InertiaRegionRefinementModel
witnessLeafDescendantRefiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupNestedMassProperties
    witnessLeafDescendantRegionDecomposition

nestedRefiner :: SheafRefiner InertiaRegionRefinementModel
nestedRefiner =
  inertiaRegionRefinerFromPatternDecomposition
    minimumAnchorCount
    lookupNestedMassProperties
    nestedRegionDecomposition

customScorePolicyRefiner :: SheafRefiner InertiaRegionRefinementModel
customScorePolicyRefiner =
  inertiaRegionRefinerFromPatternDecompositionWithScorePolicy
    minimumAnchorCount
    coverWeightedScorePolicy
    lookupConsistentMassProperties
    containedRegionDecomposition

minimumAnchorCount :: MinimumAnchorCount
minimumAnchorCount =
  case mkMinimumAnchorCount 3 of
    Just minimumValue ->
      minimumValue
    Nothing ->
      defaultMinimumAnchorCount

coverWeightedScorePolicy :: InertiaRegionScorePolicy
coverWeightedScorePolicy =
  defaultInertiaRegionScorePolicy
    { irspCompositionResidualWeight = 1.0,
      irspCoverPairWeight = 3.0,
      irspStructuralSiteWeight = 0.0,
      irspMinimumSupportScale = 1
    }

lookupConsistentMassProperties :: ClassId -> Maybe MassProperties
lookupConsistentMassProperties classId
  | classId == ClassId 8 =
      consistentRootMassProperties
  | classId == ClassId 80 =
      Just childLeftMassProperties
  | classId == ClassId 81 =
      Just childRightMassProperties
  | otherwise =
      Nothing

lookupInconsistentMassProperties :: ClassId -> Maybe MassProperties
lookupInconsistentMassProperties classId
  | classId == ClassId 8 =
      Just childLeftMassProperties
  | classId == ClassId 80 =
      Just childLeftMassProperties
  | classId == ClassId 81 =
      Just childRightMassProperties
  | otherwise =
      Nothing

lookupNestedMassProperties :: ClassId -> Maybe MassProperties
lookupNestedMassProperties classId
  | classId == ClassId 8 =
      nestedRootMassProperties
  | classId == ClassId 80 =
      Just childLeftMassProperties
  | classId == ClassId 81 =
      Just childMiddleMassProperties
  | classId == ClassId 82 =
      Just childRightMassProperties
  | otherwise =
      Nothing

containedRegionDecomposition :: InertiaRegionDecomposition RegionSubdivisionPath
containedRegionDecomposition =
  InertiaRegionDecomposition
    { irdSite = RegionSubdivisionPath [],
      irdBoundingBox = rootBoundingBox,
      irdChildren =
        [ InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [0],
              irdBoundingBox = leftBoundingBox,
              irdChildren = []
            },
          InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [1],
              irdBoundingBox = rightBoundingBox,
              irdChildren = []
            }
        ]
    }

geometrylessRegionDecomposition :: InertiaRegionDecomposition RegionSubdivisionPath
geometrylessRegionDecomposition =
  InertiaRegionDecomposition
    { irdSite = RegionSubdivisionPath [],
      irdBoundingBox = isolatedRootBoundingBox,
      irdChildren =
        [ InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [0],
              irdBoundingBox = isolatedLeftBoundingBox,
              irdChildren = []
            },
          InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [1],
              irdBoundingBox = isolatedRightBoundingBox,
              irdChildren = []
            }
        ]
    }

unmatchedPathRegionDecomposition :: InertiaRegionDecomposition RegionSubdivisionPath
unmatchedPathRegionDecomposition =
  InertiaRegionDecomposition
    { irdSite = RegionSubdivisionPath [],
      irdBoundingBox = rootBoundingBox,
      irdChildren =
        [ InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [0],
              irdBoundingBox = leftBoundingBox,
              irdChildren = []
            },
          InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [2],
              irdBoundingBox = rightBoundingBox,
              irdChildren = []
            }
        ]
    }

witnessLeafDescendantRegionDecomposition :: InertiaRegionDecomposition RegionSubdivisionPath
witnessLeafDescendantRegionDecomposition =
  InertiaRegionDecomposition
    { irdSite = RegionSubdivisionPath [],
      irdBoundingBox = rootBoundingBox,
      irdChildren =
        [ InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [0],
              irdBoundingBox = leftAggregateBoundingBox,
              irdChildren =
                [ InertiaRegionDecomposition
                    { irdSite = RegionSubdivisionPath [0, 0],
                      irdBoundingBox = leftLeafBoundingBox,
                      irdChildren =
                        [ InertiaRegionDecomposition
                            { irdSite = RegionSubdivisionPath [0, 0, 0],
                              irdBoundingBox = leftLeafDescendantBoundingBox,
                              irdChildren = []
                            }
                        ]
                    },
                  InertiaRegionDecomposition
                    { irdSite = RegionSubdivisionPath [0, 1],
                      irdBoundingBox = middleBoundingBox,
                      irdChildren = []
                    }
                ]
            },
          InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [1],
              irdBoundingBox = rightBoundingBox,
              irdChildren = []
            }
        ]
    }

nestedRegionDecomposition :: InertiaRegionDecomposition RegionSubdivisionPath
nestedRegionDecomposition =
  InertiaRegionDecomposition
    { irdSite = RegionSubdivisionPath [],
      irdBoundingBox = rootBoundingBox,
      irdChildren =
        [ InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [0],
              irdBoundingBox = leftAggregateBoundingBox,
              irdChildren =
                [ InertiaRegionDecomposition
                    { irdSite = RegionSubdivisionPath [0, 0],
                      irdBoundingBox = leftLeafBoundingBox,
                      irdChildren = []
                    },
                  InertiaRegionDecomposition
                    { irdSite = RegionSubdivisionPath [0, 1],
                      irdBoundingBox = middleBoundingBox,
                      irdChildren = []
                    }
                ]
            },
          InertiaRegionDecomposition
            { irdSite = RegionSubdivisionPath [1],
              irdBoundingBox = rightBoundingBox,
              irdChildren = []
            }
        ]
    }

consistentRootMassProperties :: Maybe MassProperties
consistentRootMassProperties =
  composeMassProperties [childLeftMassProperties, childRightMassProperties]

childLeftMassProperties :: MassProperties
childLeftMassProperties =
  MassProperties
    { massPropertiesMass = 2.0,
      massPropertiesCenterOfMass = Vec3 0.0 0.0 0.0,
      massPropertiesInertiaTensor = mempty
    }

childRightMassProperties :: MassProperties
childRightMassProperties =
  MassProperties
    { massPropertiesMass = 3.0,
      massPropertiesCenterOfMass = Vec3 2.0 0.0 0.0,
      massPropertiesInertiaTensor = mempty
    }

childMiddleMassProperties :: MassProperties
childMiddleMassProperties =
  MassProperties
    { massPropertiesMass = 1.5,
      massPropertiesCenterOfMass = Vec3 1.0 0.0 0.0,
      massPropertiesInertiaTensor = mempty
    }

nestedRootMassProperties :: Maybe MassProperties
nestedRootMassProperties =
  composeMassProperties
    [ childLeftMassProperties,
      childMiddleMassProperties,
      childRightMassProperties
    ]

rootBoundingBox :: AABB
rootBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 4.0 1.0 1.0)

leftBoundingBox :: AABB
leftBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 2.0 1.0 1.0)

rightBoundingBox :: AABB
rightBoundingBox =
  expectBoundingBox (Vec3 2.0 0.0 0.0) (Vec3 4.0 1.0 1.0)

leftAggregateBoundingBox :: AABB
leftAggregateBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 2.0 1.0 1.0)

leftLeafBoundingBox :: AABB
leftLeafBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 1.0 1.0 1.0)

leftLeafDescendantBoundingBox :: AABB
leftLeafDescendantBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 0.5 1.0 1.0)

middleBoundingBox :: AABB
middleBoundingBox =
  expectBoundingBox (Vec3 1.0 0.0 0.0) (Vec3 2.0 1.0 1.0)

isolatedRootBoundingBox :: AABB
isolatedRootBoundingBox =
  expectBoundingBox (Vec3 0.0 0.0 0.0) (Vec3 1.0 1.0 1.0)

isolatedLeftBoundingBox :: AABB
isolatedLeftBoundingBox =
  expectBoundingBox (Vec3 2.0 0.0 0.0) (Vec3 3.0 1.0 1.0)

isolatedRightBoundingBox :: AABB
isolatedRightBoundingBox =
  expectBoundingBox (Vec3 4.0 0.0 0.0) (Vec3 5.0 1.0 1.0)

expectBoundingBox :: Vec3 -> Vec3 -> AABB
expectBoundingBox minimumCorner maximumCorner =
  case mkAabb minimumCorner maximumCorner of
    Just boundingBox -> boundingBox
    Nothing -> error "invalid bounding box fixture"

expectedContinuousSubstitution :: ContinuousSubstitution InertiaRegionSite MassProperties
expectedContinuousSubstitution =
  ContinuousSubstitution
    ( IntMap.fromList
        [ ( 0,
            ContinuousBinding
              { cbClassId = ClassId 80,
                cbSite = WitnessInertiaRegionSite (EGraph.mkPatternVar 0) (ClassId 80),
                cbPayload = childLeftMassProperties,
                cbResidual = 0.0
              }
          ),
          ( 1,
            ContinuousBinding
              { cbClassId = ClassId 81,
                cbSite = WitnessInertiaRegionSite (EGraph.mkPatternVar 1) (ClassId 81),
                cbPayload = childRightMassProperties,
                cbResidual = 0.0
              }
          )
        ]
    )

expectedNestedContinuousSubstitution :: ContinuousSubstitution InertiaRegionSite MassProperties
expectedNestedContinuousSubstitution =
  ContinuousSubstitution
    ( IntMap.fromList
        [ ( 0,
            ContinuousBinding
              { cbClassId = ClassId 80,
                cbSite = WitnessInertiaRegionSite (EGraph.mkPatternVar 0) (ClassId 80),
                cbPayload = childLeftMassProperties,
                cbResidual = 0.0
              }
          ),
          ( 1,
            ContinuousBinding
              { cbClassId = ClassId 81,
                cbSite = WitnessInertiaRegionSite (EGraph.mkPatternVar 1) (ClassId 81),
                cbPayload = childMiddleMassProperties,
                cbResidual = 0.0
              }
          ),
          ( 2,
            ContinuousBinding
              { cbClassId = ClassId 82,
                cbSite = WitnessInertiaRegionSite (EGraph.mkPatternVar 2) (ClassId 82),
                cbPayload = childRightMassProperties,
                cbResidual = 0.0
              }
          )
        ]
    )
