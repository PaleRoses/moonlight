module WFCSpec
  ( tests,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Constraint
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    Arc (..),
    BacktrackLimit (..),
    BinaryConstraint (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    ConstraintSatisfactionProblem (..),
    DomainPolicy (..),
    EdgeF,
    PresencePolicy (..),
    SlotId (..),
    WFCRule (..),
    WFCOptions (..),
    WFCPolicyProblem (..),
    WFCProblem (..),
    WFCSearchResult (..),
    WFCTopology (..),
    automatonAdjacencyPolicy,
    automatonAdjacencyRule,
    compileWFCPolicyProblem,
    compileWFCProblem,
    defaultWFCOptions,
    domainFromList,
    domainSingleton,
    edgeAutomaton,
    selectNextSlot,
    solveWFCPolicyWith,
    solveWFCWith,
    transitionCompatible,
  )
import Moonlight.Pale.Test.LawSuite
  ( lawSuiteGroup,
    quickCheckLaw,
  )
import Moonlight.Automata.Pure.Core (TopDownTA)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

type Tile :: Type
data Tile
  = Empty
  | Hall
  | Shrine
  deriving stock (Eq, Ord, Show)

instance QC.Arbitrary Tile where
  arbitrary =
    QC.elements [Empty, Hall, Shrine]

alphaSlot :: SlotId String
alphaSlot = SlotId "alpha"

betaSlot :: SlotId String
betaSlot = SlotId "beta"

gammaSlot :: SlotId String
gammaSlot = SlotId "gamma"

equalityRule :: SlotId String -> SlotId String -> AdjacencyRule String Tile
equalityRule source target =
  AdjacencyRule
    { adjacencyRuleSource = source,
      adjacencyRuleTarget = target,
      adjacencyRuleCompatible = (==)
    }

betaConstrainsGamma :: SlotId String -> SlotId String -> AdjacencyRule String Tile
betaConstrainsGamma source target =
  AdjacencyRule
    { adjacencyRuleSource = source,
      adjacencyRuleTarget = target,
      adjacencyRuleCompatible =
        \beta gamma ->
          case beta of
            Empty -> gamma == Empty
            Hall -> gamma == Empty || gamma == Hall
            Shrine -> gamma == Empty || gamma == Hall || gamma == Shrine
    }

inequalityRule :: SlotId String -> SlotId String -> AdjacencyRule String Tile
inequalityRule source target =
  AdjacencyRule
    { adjacencyRuleSource = source,
      adjacencyRuleTarget = target,
      adjacencyRuleCompatible = (/=)
    }

backtrackingProblem :: WFCProblem String Tile
backtrackingProblem =
  WFCProblem
    { wfcProblemDomains =
        Map.fromList
          [ (alphaSlot, domainFromList [Empty, Hall]),
            (betaSlot, domainFromList [Empty, Hall]),
            (gammaSlot, domainFromList [Empty, Hall]),
            (SlotId "delta", domainFromList [Empty, Hall])
          ],
      wfcProblemAdjacencyRules =
        [ equalityRule alphaSlot betaSlot,
          betaConstrainsGamma betaSlot gammaSlot,
          equalityRule betaSlot (SlotId "delta"),
          inequalityRule gammaSlot (SlotId "delta")
        ]
    }

policyProblem :: WFCPolicyProblem String Tile
policyProblem =
  WFCPolicyProblem
    { wfcPolicyDomains =
        Map.fromList
          [ (alphaSlot, domainFromList [Empty, Hall, Shrine]),
            (betaSlot, domainFromList [Hall, Shrine])
          ],
      wfcPolicyTopology =
        WFCTopology
          { wfcTopologyAdjacency =
              Map.fromList
                [ (alphaSlot, [betaSlot]),
                  (betaSlot, [alphaSlot])
                ]
          },
      wfcPolicyRules =
        [ DomainPolicyRule
            (DomainPolicy
               ( \slotId tile ->
                   if slotId == alphaSlot
                     then tile /= Shrine
                     else True
               )),
          AdjacencyPolicyRule
            (AdjacencyPolicy (\_ _ left right -> left == right))
        ]
    }

policySolveProblem :: WFCPolicyProblem String Tile
policySolveProblem =
  WFCPolicyProblem
    { wfcPolicyDomains =
        Map.fromList
          [ (alphaSlot, domainFromList [Empty, Hall]),
            (betaSlot, domainSingleton Hall)
          ],
      wfcPolicyTopology =
        WFCTopology
          { wfcTopologyAdjacency =
              Map.fromList
                [ (alphaSlot, [betaSlot])
                ]
          },
      wfcPolicyRules =
        [ AdjacencyPolicyRule
            (AdjacencyPolicy (\_ _ left right -> left == right))
        ]
    }

presencePolicyProblem :: WFCPolicyProblem String Tile
presencePolicyProblem =
  WFCPolicyProblem
    { wfcPolicyDomains =
        Map.fromList
          [ (alphaSlot, domainSingleton Empty),
            (betaSlot, domainFromList [Empty, Shrine]),
            (gammaSlot, domainSingleton Empty)
          ],
      wfcPolicyTopology =
        WFCTopology
          { wfcTopologyAdjacency = Map.empty
          },
      wfcPolicyRules =
        [ PresencePolicyRule
            (PresencePolicy [alphaSlot, betaSlot, gammaSlot] (== Shrine))
        ]
    }

presenceWitnessAssignment :: CompiledPolicyValue String Tile
presenceWitnessAssignment =
  CompiledPresenceWitnessValue
    ( Map.fromList
        [ (alphaSlot, Empty),
          (betaSlot, Shrine),
          (gammaSlot, Empty)
        ]
    )

type Phase :: Type
data Phase
  = PhaseA
  | PhaseB
  deriving stock (Eq, Ord, Show)

phaseOfTile :: SlotId String -> Tile -> Phase
phaseOfTile _ tile =
  case tile of
    Empty -> PhaseA
    Hall -> PhaseB
    Shrine -> PhaseA

checkerboardAutomaton :: TopDownTA EdgeF Phase
checkerboardAutomaton =
  edgeAutomaton
    ( \phaseValue ->
        case phaseValue of
          PhaseA -> PhaseB
          PhaseB -> PhaseA
    )

automatonAdjacencyPolicyMatchesTransitionProperty :: Tile -> Tile -> Bool
automatonAdjacencyPolicyMatchesTransitionProperty sourceValue targetValue =
  let expectedCompatibility =
        transitionCompatible
          checkerboardAutomaton
          (phaseOfTile alphaSlot sourceValue)
          (phaseOfTile betaSlot targetValue)
   in applyAdjacencyPolicy
        (automatonAdjacencyPolicy checkerboardAutomaton phaseOfTile)
        alphaSlot
        betaSlot
        sourceValue
        targetValue
        == expectedCompatibility

automatonAdjacencyRuleMatchesTransitionProperty :: Tile -> Tile -> Bool
automatonAdjacencyRuleMatchesTransitionProperty sourceValue targetValue =
  let expectedCompatibility =
        transitionCompatible
          checkerboardAutomaton
          (phaseOfTile alphaSlot sourceValue)
          (phaseOfTile betaSlot targetValue)
      derivedRule =
        automatonAdjacencyRule
          alphaSlot
          betaSlot
          checkerboardAutomaton
          phaseOfTile
   in adjacencyRuleCompatible derivedRule sourceValue targetValue == expectedCompatibility

tests :: TestTree
tests =
  testGroup
    "wfc"
    [ testCase "compile_translates_domains_and_arcs" $
        let compiled =
              compileWFCProblem
                WFCProblem
                  { wfcProblemDomains =
                      Map.fromList
                        [ (alphaSlot, domainFromList [Empty, Hall]),
                          (betaSlot, domainSingleton Hall),
                          (gammaSlot, domainSingleton Shrine)
                        ],
                    wfcProblemAdjacencyRules =
                      [ equalityRule alphaSlot betaSlot,
                        equalityRule betaSlot gammaSlot
                      ]
                  }
         in do
              Map.keys (cspDomains compiled) @?= [alphaSlot, betaSlot, gammaSlot]
              fmap binaryConstraintArc (cspConstraints compiled)
                @?= [Arc alphaSlot betaSlot, Arc betaSlot gammaSlot],
      testCase "selects_mrv_with_slot_id_tiebreak" $
        let compiled =
              compileWFCProblem
                WFCProblem
                  { wfcProblemDomains =
                      Map.fromList
                        [ (betaSlot, domainFromList [Hall, Shrine]),
                          (alphaSlot, domainFromList [Hall, Shrine]),
                          (gammaSlot, domainFromList [Empty, Hall, Shrine])
                        ],
                    wfcProblemAdjacencyRules = []
                  }
         in selectNextSlot compiled @?= Just alphaSlot,
      testCase "solve_backtracks_deterministically" $ do
        case solveWFCWith defaultWFCOptions backtrackingProblem of
          Left err ->
            assertFailure ("unexpected WFC failure: " <> show err)
          Right result ->
            result
              @?= WFCSolved
                ( Map.fromList
                    [ (alphaSlot, Hall),
                      (betaSlot, Hall),
                      (gammaSlot, Empty),
                      (SlotId "delta", Hall)
                    ]
                ),
      testCase "solve_respects_backtrack_bound" $ do
        let options =
              WFCOptions
                { wfcBacktrackLimit = BacktrackLimit 0
                }
        case solveWFCWith options backtrackingProblem of
          Left err ->
            assertFailure ("unexpected WFC failure: " <> show err)
          Right result ->
            result @?= WFCBacktrackLimitReached,
      testCase "compile_policy_problem_filters_domains_and_derives_rules" $ do
        let compiled = compileWFCPolicyProblem policyProblem
        Map.lookup (SlotId (CompiledBaseSlot "alpha")) (wfcProblemDomains compiled)
          @?= Just (domainFromList [CompiledBaseValue Empty, CompiledBaseValue Hall])
        Map.lookup (SlotId (CompiledBaseSlot "beta")) (wfcProblemDomains compiled)
          @?= Just (domainFromList [CompiledBaseValue Hall, CompiledBaseValue Shrine])
        fmap
          (\rule -> Arc (adjacencyRuleSource rule) (adjacencyRuleTarget rule))
          (wfcProblemAdjacencyRules compiled)
          @?=
            [ Arc (SlotId (CompiledBaseSlot "alpha")) (SlotId (CompiledBaseSlot "beta")),
              Arc (SlotId (CompiledBaseSlot "beta")) (SlotId (CompiledBaseSlot "alpha"))
            ],
      testCase "solve_policy_problem_via_compiled_surface" $ do
        case solveWFCPolicyWith defaultWFCOptions policySolveProblem of
          Left err ->
            assertFailure ("unexpected policy WFC failure: " <> show err)
          Right result ->
            result
              @?= WFCSolved
                ( Map.fromList
                    [ (alphaSlot, Hall),
                      (betaSlot, Hall)
                    ]
                ),
      testCase "compile_global_presence_policy_to_witness_constraints" $ do
        let compiled = compileWFCPolicyProblem presencePolicyProblem
        Map.lookup (SlotId (CompiledPresenceWitnessSlot 0)) (wfcProblemDomains compiled)
          @?= Just (domainSingleton presenceWitnessAssignment)
        fmap
          (\rule -> Arc (adjacencyRuleSource rule) (adjacencyRuleTarget rule))
          (wfcProblemAdjacencyRules compiled)
          @?=
            [ Arc (SlotId (CompiledPresenceWitnessSlot 0)) (SlotId (CompiledBaseSlot "alpha")),
              Arc (SlotId (CompiledPresenceWitnessSlot 0)) (SlotId (CompiledBaseSlot "beta")),
              Arc (SlotId (CompiledPresenceWitnessSlot 0)) (SlotId (CompiledBaseSlot "gamma"))
            ],
      testCase "solve_global_presence_policy_via_projection" $ do
        case solveWFCPolicyWith defaultWFCOptions presencePolicyProblem of
          Left err ->
            assertFailure ("unexpected global policy WFC failure: " <> show err)
          Right result ->
            result
              @?= WFCSolved
                ( Map.fromList
                    [ (alphaSlot, Empty),
                      (betaSlot, Shrine),
                      (gammaSlot, Empty)
                    ]
                ),
      testCase "edge automata encode deterministic adjacency transitions" $ do
        transitionCompatible checkerboardAutomaton PhaseA PhaseB @?= True
        transitionCompatible checkerboardAutomaton PhaseB PhaseA @?= True
        transitionCompatible checkerboardAutomaton PhaseA PhaseA @?= False,
      testCase "automaton adjacency policy compiles through WFC" $
        let compiled =
              compileWFCPolicyProblem
                WFCPolicyProblem
                  { wfcPolicyDomains =
                      Map.fromList
                        [ (alphaSlot, domainFromList [Empty, Hall]),
                          (betaSlot, domainFromList [Empty, Hall])
                        ],
                    wfcPolicyTopology =
                      WFCTopology
                        { wfcTopologyAdjacency =
                            Map.fromList
                              [ (alphaSlot, [betaSlot])
                              ]
                        },
                    wfcPolicyRules =
                      [ AdjacencyPolicyRule
                          (automatonAdjacencyPolicy checkerboardAutomaton phaseOfTile)
                      ]
                  }
         in do
              fmap
                (\rule -> adjacencyRuleCompatible rule (CompiledBaseValue Empty) (CompiledBaseValue Hall))
                (wfcProblemAdjacencyRules compiled)
                @?= [True]
              fmap
                (\rule -> adjacencyRuleCompatible rule (CompiledBaseValue Hall) (CompiledBaseValue Hall))
                (wfcProblemAdjacencyRules compiled)
                @?= [False],
      testCase "automaton adjacency rule rejects incompatible target states" $
        let rule =
              automatonAdjacencyRule
                alphaSlot
                betaSlot
                checkerboardAutomaton
                phaseOfTile
         in do
              adjacencyRuleCompatible rule Empty Hall @?= True
              adjacencyRuleCompatible rule Hall Hall @?= False,
      lawSuiteGroup
        "laws"
        [ quickCheckLaw "automaton_adjacency_policy_matches_transition" automatonAdjacencyPolicyMatchesTransitionProperty,
          quickCheckLaw "automaton_adjacency_rule_matches_transition" automatonAdjacencyRuleMatchesTransitionProperty
        ]
    ]
