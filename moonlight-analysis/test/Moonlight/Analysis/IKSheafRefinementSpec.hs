{-# LANGUAGE PackageImports #-}
module Moonlight.Analysis.IKSheafRefinementSpec
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
    "ik-sheaf-refinement"
    [ testCase "joint-count refinement rejects negative values" $
        mkMinimumJointCount (-1) @?= Nothing,
      testCase "compiled-query refinement solves a simple fabrik chain while preserving the exact witness" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        case refineSheafCompiledWithMatcher (seedMatcher seedBackend) refiner compiledQuery emptyGraph of
          [matchValue] -> do
            fmRootClass matchValue @?= ClassId 8
            fmDiscreteSubstitution matchValue
              @?= Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)])
            fmContinuousSubstitution matchValue @?= expectedContinuousSubstitution
            fmDetail matchValue
              @?= IKChainRefinementDetail
                { ikdJointCount = 2,
                  ikdEndEffector = Vec3 0.0 1.0 0.0
                }
            assertBool "expected fabrik rank to reflect joint displacement" (unFuzzyRank (fmRank matchValue) > 0.0)
          refinedMatches ->
            assertFailure ("expected one IK refined match, got " <> show refinedMatches),
      testCase "refinement rejects chains with missing joint anchors" $ do
        compiledQuery <- expectCompiledQuery samplePattern
        refineSheafCompiledWithMatcher (seedMatcher seedBackend) incompleteRefiner compiledQuery emptyGraph
          @?= []
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

samplePattern :: Pattern ArithF
samplePattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

type SeedBackend :: Type
data SeedBackend = SeedBackend
  { compiledSeeds :: [(ClassId, Substitution)]
  }

seedMatcher :: SeedBackend -> CompiledSeedMatcher ArithF
seedMatcher backend _ _ = compiledSeeds backend

seedBackend :: SeedBackend
seedBackend =
  SeedBackend
    { compiledSeeds =
        [ (ClassId 8, Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)]))
        ]
    }

refiner :: SheafRefiner IKChainRefinementModel
refiner =
  ikChainRefiner minimumJointCount roundLimit fabrikTolerance (Vec3 0.0 1.0 0.0) lookupJointPosition

incompleteRefiner :: SheafRefiner IKChainRefinementModel
incompleteRefiner =
  ikChainRefiner minimumJointCount roundLimit fabrikTolerance (Vec3 0.0 1.0 0.0) lookupIncompleteJointPosition

minimumJointCount :: MinimumJointCount
minimumJointCount =
  case mkMinimumJointCount 2 of
    Just value -> value
    Nothing -> error "invalid minimum joint count fixture"

roundLimit :: FabrikRoundLimit
roundLimit =
  case mkFabrikRoundLimit 8 of
    Just value -> value
    Nothing -> error "invalid fabrik round limit fixture"

fabrikTolerance :: FabrikTolerance
fabrikTolerance =
  case mkFabrikTolerance 1.0e-9 of
    Just value -> value
    Nothing -> error "invalid fabrik tolerance fixture"

lookupJointPosition :: ClassId -> Maybe Vec3
lookupJointPosition classId
  | classId == ClassId 80 = Just (Vec3 0.0 0.0 0.0)
  | classId == ClassId 81 = Just (Vec3 1.0 0.0 0.0)
  | otherwise = Nothing

lookupIncompleteJointPosition :: ClassId -> Maybe Vec3
lookupIncompleteJointPosition classId
  | classId == ClassId 80 = Just (Vec3 0.0 0.0 0.0)
  | otherwise = Nothing

expectedContinuousSubstitution :: ContinuousSubstitution IKJointSite Vec3
expectedContinuousSubstitution =
  ContinuousSubstitution
    ( IntMap.fromList
        [ ( 0,
            ContinuousBinding
              { cbClassId = ClassId 80,
                cbSite = IKJointSite 0 (ClassId 80),
                cbPayload = Vec3 0.0 0.0 0.0,
                cbResidual = 0.0
              }
          ),
          ( 1,
            ContinuousBinding
              { cbClassId = ClassId 81,
                cbSite = IKJointSite 1 (ClassId 81),
                cbPayload = Vec3 0.0 1.0 0.0,
                cbResidual = 0.0
              }
          )
        ]
    )
