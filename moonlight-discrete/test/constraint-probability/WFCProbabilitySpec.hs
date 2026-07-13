module WFCProbabilitySpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Constraint
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    DomainPolicy (..),
    SlotId (..),
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
import Moonlight.Probability (categoricalSupport, mkCategorical)
import Moonlight.Pale.Test.Site.Assertion (expectRight, expectSome)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

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
      testCase "weighted sampling is deterministic for a fixed seed" $ do
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
            options =
              defaultWFCProbabilityOptions
                { wfcProbabilitySeed = 11,
                  wfcProbabilityCandidateSelection = WeightedSampling
                }
            leftResult = solveProbabilisticWFCWith options problem
            rightResult = solveProbabilisticWFCWith options problem
        leftResult @?= rightResult,
      testCase "policy compile preserves weighted support after domain filtering" $ do
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
        fmap categoricalSupport (Map.lookup (SlotId (CompiledBaseSlot "alpha")) (pwfcProblemDomains compiled))
          @?= Just (Set.fromList [CompiledBaseValue Empty, CompiledBaseValue Hall]),
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
