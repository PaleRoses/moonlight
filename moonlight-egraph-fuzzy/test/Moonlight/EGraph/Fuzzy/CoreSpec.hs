{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module Moonlight.EGraph.Fuzzy.CoreSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Core (ClassId(..))
import Moonlight.Rewrite.Algebra (CompiledPatternQuery, compilePatternQuery, singlePatternQuery)
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Core (Substitution (..))
import Moonlight.Rewrite.System
  ( CompiledGuard,
    combineCompiledGuards,
    compileGuard,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraph)
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Core
  ( ContinuousBinding (..),
    ContinuousSubstitution (..),
    FuzzyMatch (..),
    FuzzyRank (..),
    RefinementCandidate (..),
    RefinementSolution (..),
    RefinementSolve (..),
    assembleFuzzyMatch,
    decodeContinuousSubstitution,
  )
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Refiner
  ( RefinementModel (..),
    RefinementRanking (..),
    CompiledSeedMatcher,
    refineCompiledWithMatcher,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    analysisSpec,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "core"
    [ testCase "decodeContinuousSubstitution preserves exact bindings and omits missing payloads" $
        decodeContinuousSubstitution refinementCandidate valueBySite 0.125
          @?= expectedContinuousSubstitution,
      testCase "assembleFuzzyMatch preserves the exact discrete substitution" $
        assembleFuzzyMatch refinementCandidate refinementSolve
          @?= expectedFuzzyMatch,
      testCase "refineCompiledWithMatcher dispatches through compiled seed enumeration" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          let graph = emptyEGraph analysisSpec
          refineCompiledWithMatcher (seedMatcher seedBackend) SeedRefiner compiledQuery graph
            @?= expectedRefinedMatches
    ]

refinementCandidate :: RefinementCandidate String () ()
refinementCandidate =
  RefinementCandidate
    { rcRootClass = ClassId 99,
      rcDiscreteSubstitution =
        Substitution
          ( IntMap.fromList
              [ (0, ClassId 10),
                (1, ClassId 11),
                (2, ClassId 12)
              ]
          ),
      rcVarSites =
        IntMap.fromList
          [ (0, "left"),
            (1, "right"),
            (2, "missing-value"),
            (3, "missing-class")
          ],
      rcSites = ["left", "right", "missing-payload", "missing-class"],
      rcAnchors = Map.empty,
      rcEvidence = ()
    }

valueBySite :: Map.Map String String
valueBySite =
  Map.fromList
    [ ("left", "alpha"),
      ("right", "beta")
    ]

refinementSolve :: RefinementSolve String String String Double Double
refinementSolve =
  RefinementSolve
    { rsValueBySite = valueBySite,
      rsResidual = 0.125,
      rsScore = 7.5,
      rsRank = FuzzyRank 7.5,
      rsDetail = "solve-detail"
    }

expectedContinuousSubstitution :: ContinuousSubstitution String String
expectedContinuousSubstitution =
  ContinuousSubstitution
    ( IntMap.fromList
        [ ( 0,
            ContinuousBinding
              { cbClassId = ClassId 10,
                cbSite = "left",
                cbPayload = "alpha",
                cbResidual = 0.125
              }
          ),
          ( 1,
            ContinuousBinding
              { cbClassId = ClassId 11,
                cbSite = "right",
                cbPayload = "beta",
                cbResidual = 0.125
              }
          )
        ]
    )

expectedFuzzyMatch :: FuzzyMatch String String String Double Double
expectedFuzzyMatch =
  FuzzyMatch
    { fmRootClass = ClassId 99,
      fmDiscreteSubstitution = rcDiscreteSubstitution refinementCandidate,
      fmContinuousSubstitution = expectedContinuousSubstitution,
      fmScore = 7.5,
      fmRank = FuzzyRank 7.5,
      fmDetail = "solve-detail"
    }

type SeedBackend :: Type
data SeedBackend = SeedBackend
  { sbCompiledSeeds :: [(ClassId, Substitution)]
  }

type SeedRefiner :: Type
data SeedRefiner = SeedRefiner

seedMatcher :: SeedBackend -> CompiledSeedMatcher ArithF
seedMatcher backend _ _ =
  sbCompiledSeeds backend

instance RefinementRanking SeedRefiner where
  type RefinementScore SeedRefiner = Double
  type RefinementRank SeedRefiner = Double

  rankRefinementScore _ scoreValue = FuzzyRank scoreValue

  compareRefinementRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

instance RefinementModel SeedRefiner where
  type ModelSite SeedRefiner = String
  type ModelAnchor SeedRefiner = ()
  type ModelEvidence SeedRefiner = ()
  type ModelValue SeedRefiner = String
  type ModelDetail SeedRefiner = Int
  type ModelBlueprint SeedRefiner = ()

  compileRefinementBlueprint _ _ = ()

  enumerateRefinementCandidates _ _ _ _ seedMatches =
    fmap
      (\(rootClassId, discreteSubstitution) ->
         RefinementCandidate
           { rcRootClass = rootClassId,
             rcDiscreteSubstitution = discreteSubstitution,
             rcVarSites = IntMap.fromList [(0, "left"), (1, "missing-value")],
             rcSites = ["left", "missing-payload"],
             rcAnchors = Map.empty,
             rcEvidence = ()
           }
      )
      seedMatches

  acceptRefinementCandidate _ _ _ = True

  solveRefinementCandidate _ _ candidate =
    Just
      RefinementSolution
        { rslValueBySite = Map.fromList [("left", show (rcRootClass candidate))],
          rslResidual = 0.25,
          rslDetail = IntMap.size (let Substitution bindings = rcDiscreteSubstitution candidate in bindings)
        }

  scoreRefinementSolution _ _ candidate _ =
    case rcRootClass candidate of
      ClassId 7 -> 4.0
      ClassId 8 -> 3.0
      _ -> 3.0

seedToFuzzyMatch :: (ClassId, Substitution) -> FuzzyMatch String String Int Double Double
seedToFuzzyMatch (rootClassId, discreteSubstitution) =
  let
    scoreValue =
      case rootClassId of
        ClassId 7 -> 4.0
        ClassId 8 -> 3.0
        _ -> 3.0
   in
  assembleFuzzyMatch
    RefinementCandidate
      { rcRootClass = rootClassId,
        rcDiscreteSubstitution = discreteSubstitution,
        rcVarSites = IntMap.fromList [(0, "left"), (1, "missing-value")],
        rcSites = ["left", "missing-payload"],
        rcAnchors = Map.empty,
        rcEvidence = ()
      }
    RefinementSolve
      { rsValueBySite = Map.fromList [("left", show rootClassId)],
        rsResidual = 0.25,
        rsScore = scoreValue,
        rsRank = FuzzyRank scoreValue,
        rsDetail = IntMap.size (let Substitution bindings = discreteSubstitution in bindings)
      }

seedBackend :: SeedBackend
seedBackend =
  SeedBackend
    { sbCompiledSeeds =
        [ (ClassId 7, Substitution (IntMap.fromList [(0, ClassId 70)])),
          (ClassId 8, Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)]))
        ]
    }

expectedRefinedMatches :: [FuzzyMatch String String Int Double Double]
expectedRefinedMatches =
  fmap seedToFuzzyMatch (reverse (sbCompiledSeeds seedBackend))

samplePattern :: Pattern ArithF
samplePattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0)))

expectCompiledQuery :: Pattern ArithF -> IO (CompiledPatternQuery (CompiledGuard () ArithF) ArithF)
expectCompiledQuery patternValue =
  case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
    Left unboundPatternVars ->
      assertFailure ("expected compiled query to validate, got unbound vars " <> show unboundPatternVars)
    Right compiledQuery ->
      pure compiledQuery
