{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Analysis.SheafRefinementSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Analysis.SheafRefinement
  ( SheafRefinementModel (..),
    SheafEnergy (..),
    SheafSolve (..),
    SheafRefiner (..),
    refineSheafCompiledWithMatcher,
  )
import Moonlight.EGraph.Fuzzy.Core
  ( ContinuousBinding (..),
    ContinuousSubstitution (..),
    FuzzyMatch (..),
    FuzzyRank (..),
    RefinementCandidate (..),
  )
import Moonlight.EGraph.Fuzzy.Refiner (CompiledSeedMatcher, MatchRefiner (..))
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
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount,
    analysisSpec,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "sheaf-refinement"
    [ testCase "candidate acceptance filters rejected sheaf candidates" $
        do
          refinedMatches <- refineDirectly
          fmap fmRootClass refinedMatches @?= [ClassId 8],
      testCase "solve results assemble an exact-preserving fuzzy match" $
        do
          refinedMatches <- refineDirectly
          refinedMatches @?= [expectedAcceptedMatch],
      testCase "compiled-query helper dispatches through exact seed enumeration" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          refineSheafCompiledWithMatcher (seedMatcher seedBackend) (SheafRefiner DomainModel) compiledQuery emptyGraph
            @?= [expectedAcceptedMatch],
      testCase "all-rejected seeds produce empty fuzzy match list" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          refineMatches (SheafRefiner DomainModel) compiledQuery emptyGraph allRejectedSeeds
            @?= [],
      testCase "accepted candidates with solve failure produce empty fuzzy match list" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          refineMatches (SheafRefiner UnsolvableModel) compiledQuery emptyGraph sampleSeeds
            @?= [],
      testCase "multiple accepted candidates are ordered by compareSheafRanks" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          let matches = refineMatches (SheafRefiner RankedModel) compiledQuery emptyGraph rankedSeeds
          fmap fmRootClass matches @?= [ClassId 10, ClassId 20, ClassId 30],
      testCase "reverse rank comparator inverts result sequence" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          let matches = refineMatches (SheafRefiner ReverseRankedModel) compiledQuery emptyGraph rankedSeeds
          fmap fmRootClass matches @?= [ClassId 30, ClassId 20, ClassId 10],
      testCase "blueprint compilation governs candidate acceptance" $
        do
          compiledQuery <- expectCompiledQuery samplePattern
          let matches = refineMatches (SheafRefiner (BlueprintFilterModel "b")) compiledQuery emptyGraph blueprintSeeds
          fmap fmRootClass matches @?= [ClassId 42]
    ]

refineDirectly :: IO [FuzzyMatch String String String Double Double]
refineDirectly =
  fmap
    (\compiledQuery -> refineMatches (SheafRefiner DomainModel) compiledQuery emptyGraph sampleSeeds)
    (expectCompiledQuery samplePattern)

emptyGraph :: EGraph ArithF NodeCount
emptyGraph = emptyEGraph analysisSpec

expectCompiledQuery :: Pattern ArithF -> IO (CompiledPatternQuery (CompiledGuard () ArithF) ArithF)
expectCompiledQuery patternValue =
  case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
    Left unboundPatternVars ->
      assertFailure
        ("expected compiled query to validate, got unbound vars " <> show unboundPatternVars)
    Right compiledQuery ->
      pure compiledQuery

samplePattern :: Pattern ArithF
samplePattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0)))

type DomainBlueprint :: Type
data DomainBlueprint = DomainBlueprint
  { allowedEvidence :: String
  }
  deriving stock (Eq, Show)

type DomainModel :: Type
data DomainModel = DomainModel

type SeedBackend :: Type
data SeedBackend = SeedBackend
  { compiledSeeds :: [(ClassId, Substitution)]
  }

seedMatcher :: SeedBackend -> CompiledSeedMatcher ArithF
seedMatcher backend _ _ =
  compiledSeeds backend

instance SheafRefinementModel DomainModel where
  type SheafSite DomainModel = String
  type SheafAnchor DomainModel = ()
  type SheafEvidence DomainModel = String
  type SheafValue DomainModel = String
  type SheafDetail DomainModel = String
  type SheafBlueprint DomainModel = DomainBlueprint
  type SheafScore DomainModel = Double
  type SheafRank DomainModel = Double
  type SheafSeed DomainModel = (ClassId, Substitution)

  compileSheafBlueprint _ =
    DomainBlueprint {allowedEvidence = "accept"}

  enumerateSheafCandidates _ _ =
    fmap seedToCandidate

  acceptSheafCandidate _ blueprint candidate =
    rcEvidence candidate == allowedEvidence blueprint

  solveSheafCandidate _ _ candidate =
    case rcEvidence candidate of
      "accept" ->
        Just acceptedSolve
      _ ->
        Nothing

  interpretSheafSolve _ _ _ _ = SheafEnergy 2.5

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

seedToCandidate :: (ClassId, Substitution) -> RefinementCandidate String () String
seedToCandidate (rootClassId, discreteSubstitution) =
  RefinementCandidate
    { rcRootClass = rootClassId,
      rcDiscreteSubstitution = discreteSubstitution,
      rcVarSites = IntMap.fromList [(0, "left"), (1, "missing-value")],
      rcSites = ["left", "missing-payload"],
      rcAnchors = Map.empty,
      rcEvidence =
        if rootClassId == ClassId 8
          then "accept"
          else "reject"
    }

acceptedSolve :: SheafSolve String String String
acceptedSolve =
  SheafSolve
    { ssValueBySite = Map.fromList [("left", "value-left")],
      ssResidual = 0.5,
      ssDetail = "accept"
    }

rejectedSeed :: (ClassId, Substitution)
rejectedSeed =
  (ClassId 7, Substitution (IntMap.fromList [(0, ClassId 70)]))

acceptedSeed :: (ClassId, Substitution)
acceptedSeed =
  (ClassId 8, Substitution (IntMap.fromList [(0, ClassId 80), (1, ClassId 81)]))

sampleSeeds :: [(ClassId, Substitution)]
sampleSeeds =
  [rejectedSeed, acceptedSeed]

seedBackend :: SeedBackend
seedBackend =
  SeedBackend
    { compiledSeeds = sampleSeeds
    }

expectedAcceptedMatch :: FuzzyMatch String String String Double Double
expectedAcceptedMatch =
  FuzzyMatch
    { fmRootClass = ClassId 8,
      fmDiscreteSubstitution = snd acceptedSeed,
      fmContinuousSubstitution =
        ContinuousSubstitution
          ( IntMap.fromList
              [ ( 0,
                  ContinuousBinding
                    { cbClassId = ClassId 80,
                      cbSite = "left",
                      cbPayload = "value-left",
                      cbResidual = 0.5
                    }
                )
              ]
          ),
      fmScore = 2.5,
      fmRank = FuzzyRank 2.5,
      fmDetail = "accept"
    }

allRejectedSeeds :: [(ClassId, Substitution)]
allRejectedSeeds =
  [ (ClassId 5, Substitution (IntMap.fromList [(0, ClassId 50)])),
    (ClassId 6, Substitution (IntMap.fromList [(0, ClassId 60)]))
  ]

type UnsolvableModel :: Type
data UnsolvableModel = UnsolvableModel

instance SheafRefinementModel UnsolvableModel where
  type SheafSite UnsolvableModel = String
  type SheafAnchor UnsolvableModel = ()
  type SheafEvidence UnsolvableModel = ()
  type SheafValue UnsolvableModel = String
  type SheafDetail UnsolvableModel = String
  type SheafBlueprint UnsolvableModel = ()
  type SheafScore UnsolvableModel = Double
  type SheafRank UnsolvableModel = Double
  type SheafSeed UnsolvableModel = (ClassId, Substitution)

  compileSheafBlueprint _ = ()

  enumerateSheafCandidates _ _ =
    fmap unsolvableCandidate

  acceptSheafCandidate _ _ _ = True

  solveSheafCandidate _ _ _ = Nothing

  interpretSheafSolve _ _ _ _ = SheafEnergy 0.0

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

unsolvableCandidate :: (ClassId, Substitution) -> RefinementCandidate String () ()
unsolvableCandidate (rootClassId, discreteSubstitution) =
  RefinementCandidate
    { rcRootClass = rootClassId,
      rcDiscreteSubstitution = discreteSubstitution,
      rcVarSites = IntMap.fromList [(0, "site")],
      rcSites = ["site"],
      rcAnchors = Map.empty,
      rcEvidence = ()
    }

type RankedModel :: Type
data RankedModel = RankedModel

instance SheafRefinementModel RankedModel where
  type SheafSite RankedModel = String
  type SheafAnchor RankedModel = ()
  type SheafEvidence RankedModel = Double
  type SheafValue RankedModel = String
  type SheafDetail RankedModel = String
  type SheafBlueprint RankedModel = ()
  type SheafScore RankedModel = Double
  type SheafRank RankedModel = Double
  type SheafSeed RankedModel = (ClassId, Substitution)

  compileSheafBlueprint _ = ()

  enumerateSheafCandidates _ _ =
    fmap rankedCandidate

  acceptSheafCandidate _ _ _ = True

  solveSheafCandidate _ _ _ =
    Just
      SheafSolve
        { ssValueBySite = Map.fromList [("site", "value")],
          ssResidual = 0.0,
          ssDetail = "detail"
        }

  interpretSheafSolve _ _ candidate _ =
    SheafEnergy (rcEvidence candidate)

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

type ReverseRankedModel :: Type
data ReverseRankedModel = ReverseRankedModel

instance SheafRefinementModel ReverseRankedModel where
  type SheafSite ReverseRankedModel = String
  type SheafAnchor ReverseRankedModel = ()
  type SheafEvidence ReverseRankedModel = Double
  type SheafValue ReverseRankedModel = String
  type SheafDetail ReverseRankedModel = String
  type SheafBlueprint ReverseRankedModel = ()
  type SheafScore ReverseRankedModel = Double
  type SheafRank ReverseRankedModel = Double
  type SheafSeed ReverseRankedModel = (ClassId, Substitution)

  compileSheafBlueprint _ = ()

  enumerateSheafCandidates _ _ =
    fmap rankedCandidate

  acceptSheafCandidate _ _ _ = True

  solveSheafCandidate _ _ _ =
    Just
      SheafSolve
        { ssValueBySite = Map.fromList [("site", "value")],
          ssResidual = 0.0,
          ssDetail = "detail"
        }

  interpretSheafSolve _ _ candidate _ =
    SheafEnergy (rcEvidence candidate)

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare rightRank leftRank

rankedCandidate :: (ClassId, Substitution) -> RefinementCandidate String () Double
rankedCandidate (rootClassId, discreteSubstitution) =
  let ClassId classIdValue = rootClassId
   in RefinementCandidate
        { rcRootClass = rootClassId,
          rcDiscreteSubstitution = discreteSubstitution,
          rcVarSites = IntMap.fromList [(0, "site")],
          rcSites = ["site"],
          rcAnchors = Map.empty,
          rcEvidence = fromIntegral classIdValue
        }

rankedSeeds :: [(ClassId, Substitution)]
rankedSeeds =
  [ (ClassId 30, Substitution (IntMap.fromList [(0, ClassId 300)])),
    (ClassId 10, Substitution (IntMap.fromList [(0, ClassId 100)])),
    (ClassId 20, Substitution (IntMap.fromList [(0, ClassId 200)]))
  ]

type BlueprintFilterModel :: Type
newtype BlueprintFilterModel = BlueprintFilterModel String

instance SheafRefinementModel BlueprintFilterModel where
  type SheafSite BlueprintFilterModel = String
  type SheafAnchor BlueprintFilterModel = ()
  type SheafEvidence BlueprintFilterModel = String
  type SheafValue BlueprintFilterModel = String
  type SheafDetail BlueprintFilterModel = String
  type SheafBlueprint BlueprintFilterModel = String
  type SheafScore BlueprintFilterModel = Double
  type SheafRank BlueprintFilterModel = Double
  type SheafSeed BlueprintFilterModel = (ClassId, Substitution)

  compileSheafBlueprint (BlueprintFilterModel tag) = tag

  enumerateSheafCandidates _ _ =
    fmap blueprintCandidate

  acceptSheafCandidate _ blueprint candidate =
    rcEvidence candidate == blueprint

  solveSheafCandidate _ _ _ =
    Just
      SheafSolve
        { ssValueBySite = Map.fromList [("site", "value")],
          ssResidual = 0.0,
          ssDetail = "detail"
        }

  interpretSheafSolve _ _ _ _ = SheafEnergy 1.0

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

blueprintCandidate :: (ClassId, Substitution) -> RefinementCandidate String () String
blueprintCandidate (rootClassId, discreteSubstitution) =
  RefinementCandidate
    { rcRootClass = rootClassId,
      rcDiscreteSubstitution = discreteSubstitution,
      rcVarSites = IntMap.fromList [(0, "site")],
      rcSites = ["site"],
      rcAnchors = Map.empty,
      rcEvidence =
        case rootClassId of
          ClassId 41 -> "a"
          ClassId 42 -> "b"
          _ -> "c"
    }

blueprintSeeds :: [(ClassId, Substitution)]
blueprintSeeds =
  [ (ClassId 41, Substitution (IntMap.fromList [(0, ClassId 410)])),
    (ClassId 42, Substitution (IntMap.fromList [(0, ClassId 420)])),
    (ClassId 43, Substitution (IntMap.fromList [(0, ClassId 430)]))
  ]
