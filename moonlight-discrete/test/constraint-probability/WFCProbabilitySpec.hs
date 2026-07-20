module WFCProbabilitySpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Constraint
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    DomainPolicy (..),
    SlotId (..),
    WFCError,
    WFCSearchResult (..),
    WFCRule (..),
    WFCTopology (..),
    compileWFCProblem,
    domainFromList,
    wfcProblemDomains,
  )
import Moonlight.Constraint.Pure.WFC.Probability
  ( CandidateSelectionStrategy (..),
    ProbabilisticWFCPolicyProblem (..),
    ProbabilisticWFCProblem (..),
    WFCProbabilityOptions (..),
    compileProbabilisticWFCPolicyProblem,
    compileProbabilisticWFCProblem,
    defaultWFCProbabilityOptions,
    pwfcProblemDomains,
    selectNextEntropySlot,
    solveProbabilisticWFCPolicyWith,
    solveProbabilisticWFCWith,
  )
import Moonlight.Probability (categoricalLookup, mkCategorical, probValue)
import Moonlight.Pale.Test.Site.Assertion (expectRight, expectSome)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

type Tile :: Type
data Tile
  = Empty
  | Hall
  | Shrine
  deriving stock (Eq, Ord, Show)

alphaSlot :: SlotId String
alphaSlot = SlotId "alpha"

betaSlot :: SlotId String
betaSlot = SlotId "beta"

equalityRule :: SlotId String -> SlotId String -> AdjacencyRule String Tile
equalityRule source target =
  AdjacencyRule
    { adjacencyRuleSource = source,
      adjacencyRuleTarget = target,
      adjacencyRuleCompatible = (==)
    }

tests :: TestTree
tests =
  testGroup
    "wfc-probability"
    [ testCase "compile uses categorical support" $ do
        priorDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 3.0), (Shrine, 1.0)]))
        let compiled =
              compileProbabilisticWFCProblem
                ProbabilisticWFCProblem
                  { pwfcProblemDomains = Map.fromList [(alphaSlot, priorDistribution)],
                    pwfcProblemAdjacencyRules = []
                  }
        Map.lookup alphaSlot (wfcProblemDomains compiled)
          @?= Just (domainFromList [Hall, Shrine]),
      testCase "entropy heuristic prefers lower entropy slot" $ do
        alphaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 9.0), (Shrine, 1.0)]))
        betaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 1.0), (Shrine, 1.0)]))
        let problem =
              ProbabilisticWFCProblem
                { pwfcProblemDomains =
                    Map.fromList
                      [ (alphaSlot, alphaDistribution),
                        (betaSlot, betaDistribution)
                      ],
                  pwfcProblemAdjacencyRules = []
                }
            compiled = compileWFCProblem (compileProbabilisticWFCProblem problem)
        selectNextEntropySlot problem compiled @?= Just alphaSlot,
      testCase "weighted descending chooses highest-priority branch first" $ do
        priorDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 9.0), (Shrine, 1.0)]))
        solved <-
          expectRight
            ( solveProbabilisticWFCWith
                defaultWFCProbabilityOptions
                  { wfcProbabilityCandidateSelection = WeightedDescending
                  }
                ProbabilisticWFCProblem
                  { pwfcProblemDomains = Map.fromList [(alphaSlot, priorDistribution)],
                    pwfcProblemAdjacencyRules = []
                  }
            )
        solved @?= WFCSolved (Map.fromList [(alphaSlot, Hall)]),
      testCase "weighted sampling seed influences candidate choice" $ do
        alphaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 1.0), (Shrine, 3.0)]))
        betaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 1.0), (Shrine, 3.0)]))
        let problem =
              ProbabilisticWFCProblem
                { pwfcProblemDomains =
                    Map.fromList
                      [ (alphaSlot, alphaDistribution),
                        (betaSlot, betaDistribution)
                      ],
                  pwfcProblemAdjacencyRules = [equalityRule alphaSlot betaSlot]
                }
            solveAtSeed seed =
              solveProbabilisticWFCWith
                defaultWFCProbabilityOptions
                  { wfcProbabilitySeed = seed,
                    wfcProbabilityCandidateSelection = WeightedSampling
                  }
                problem
            seededResults = fmap solveAtSeed [0 .. 31]
            expectedResults :: [Either (WFCError String) (WFCSearchResult String Tile)]
            expectedResults =
              [ Right (WFCSolved (Map.fromList [(alphaSlot, Hall), (betaSlot, Hall)])),
                Right (WFCSolved (Map.fromList [(alphaSlot, Shrine), (betaSlot, Shrine)]))
              ]
        assertBool
          "seed range must exercise both supported weighted branches and no other result"
          ( all (`elem` seededResults) expectedResults
              && all (`elem` expectedResults) seededResults
          ),
      testCase "policy compile preserves relative weights after domain filtering" $ do
        alphaDistribution <- expectRight (mkCategorical (Map.fromList [(Empty, 1.0), (Hall, 3.0), (Shrine, 9.0)]))
        betaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 1.0), (Shrine, 1.0)]))
        compiled <-
          expectSome "expected compiled probabilistic policy problem"
            ( compileProbabilisticWFCPolicyProblem
                ProbabilisticWFCPolicyProblem
                  { pwfcPolicyDomains =
                      Map.fromList
                        [ (alphaSlot, alphaDistribution),
                          (betaSlot, betaDistribution)
                        ],
                    pwfcPolicyTopology = WFCTopology {wfcTopologyAdjacency = Map.empty},
                    pwfcPolicyRules =
                      [ DomainPolicyRule
                          (DomainPolicy (\caseSlot tile -> caseSlot /= alphaSlot || tile /= Shrine))
                      ]
                  }
            )
        case Map.lookup (SlotId (CompiledBaseSlot "alpha")) (pwfcProblemDomains compiled) of
          Nothing ->
            assertFailure "compiled alpha distribution is missing"
          Just compiledDistribution -> do
            fmap probValue (categoricalLookup (CompiledBaseValue Empty) compiledDistribution) @?= Just 0.25
            fmap probValue (categoricalLookup (CompiledBaseValue Hall) compiledDistribution) @?= Just 0.75
            categoricalLookup (CompiledBaseValue Shrine) compiledDistribution @?= Nothing,
      testCase "policy solve projects weighted assignments back to base slots" $ do
        alphaDistribution <- expectRight (mkCategorical (Map.fromList [(Empty, 1.0), (Hall, 4.0)]))
        betaDistribution <- expectRight (mkCategorical (Map.fromList [(Hall, 1.0)]))
        solved <-
          expectRight
            ( solveProbabilisticWFCPolicyWith
                defaultWFCProbabilityOptions
                ProbabilisticWFCPolicyProblem
                  { pwfcPolicyDomains =
                      Map.fromList
                        [ (alphaSlot, alphaDistribution),
                          (betaSlot, betaDistribution)
                        ],
                    pwfcPolicyTopology =
                      WFCTopology
                        { wfcTopologyAdjacency = Map.fromList [(alphaSlot, [betaSlot])]
                        },
                    pwfcPolicyRules =
                      [ AdjacencyPolicyRule
                          (AdjacencyPolicy (\_ _ left right -> left == right))
                      ]
                  }
            )
        solved @?= WFCSolved (Map.fromList [(alphaSlot, Hall), (betaSlot, Hall)])
    ]
