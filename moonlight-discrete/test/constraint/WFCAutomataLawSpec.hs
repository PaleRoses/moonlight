module WFCAutomataLawSpec
  ( tests,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Core (IsLawName (..))
import Moonlight.Constraint
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    CompiledPolicyValue (..),
    EdgeF,
    SlotId (..),
    WFCPolicyProblem (..),
    WFCRule (..),
    WFCTopology (..),
    automatonAdjacencyPolicy,
    automatonAdjacencyRule,
    compileWFCPolicyProblem,
    domainFromList,
    edgeAutomaton,
    transitionCompatible,
    wfcProblemAdjacencyRules,
  )
import Moonlight.Automata.Pure.Core (TopDownTA)
import Moonlight.Pale.Test.LawSuite
  ( QuickCheckLawBundle,
    lawSuiteGroup,
    quickCheckLawBundle,
    quickCheckLawBundleGroup,
    quickCheckLawDefinition,
  )
import Test.Tasty (TestTree, localOption)
import qualified Test.Tasty.QuickCheck as QC

type Tile :: Type
data Tile
  = Empty
  | Hall
  | Shrine
  deriving stock (Eq, Ord, Show)

type Phase :: Type
data Phase
  = PhaseA
  | PhaseB
  deriving stock (Eq, Ord, Show)

type WFCAutomataLawName :: Type
data WFCAutomataLawName
  = PolicyMatchesTransitionCompatibility
  | RuleMatchesTransitionCompatibility
  | CompiledPolicyMatchesTransitionCompatibility
  deriving stock (Eq, Ord, Show)

instance IsLawName WFCAutomataLawName where
  lawNameText lawName =
    case lawName of
      PolicyMatchesTransitionCompatibility -> "policy_matches_transition_compatibility"
      RuleMatchesTransitionCompatibility -> "rule_matches_transition_compatibility"
      CompiledPolicyMatchesTransitionCompatibility -> "compiled_policy_matches_transition_compatibility"

instance QC.Arbitrary Tile where
  arbitrary = QC.elements [Empty, Hall, Shrine]

alphaSlot :: SlotId String
alphaSlot = SlotId "alpha"

betaSlot :: SlotId String
betaSlot = SlotId "beta"

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

policyMatchesTransitionCompatibilityLaw :: Tile -> Tile -> Bool
policyMatchesTransitionCompatibilityLaw sourceValue targetValue =
  applyAdjacencyPolicy
    (automatonAdjacencyPolicy checkerboardAutomaton phaseOfTile)
    alphaSlot
    betaSlot
    sourceValue
    targetValue
    == transitionCompatible
      checkerboardAutomaton
      (phaseOfTile alphaSlot sourceValue)
      (phaseOfTile betaSlot targetValue)

ruleMatchesTransitionCompatibilityLaw :: Tile -> Tile -> Bool
ruleMatchesTransitionCompatibilityLaw sourceValue targetValue =
  adjacencyRuleCompatible
    (automatonAdjacencyRule alphaSlot betaSlot checkerboardAutomaton phaseOfTile)
    sourceValue
    targetValue
    == transitionCompatible
      checkerboardAutomaton
      (phaseOfTile alphaSlot sourceValue)
      (phaseOfTile betaSlot targetValue)

compiledPolicyMatchesTransitionCompatibilityLaw :: Tile -> Tile -> Bool
compiledPolicyMatchesTransitionCompatibilityLaw sourceValue targetValue =
  case wfcProblemAdjacencyRules compiledProblem of
    [compiledRule] ->
      adjacencyRuleCompatible compiledRule (CompiledBaseValue sourceValue) (CompiledBaseValue targetValue)
        == transitionCompatible
          checkerboardAutomaton
          (phaseOfTile alphaSlot sourceValue)
          (phaseOfTile betaSlot targetValue)
    _ -> False
  where
    compiledProblem =
      compileWFCPolicyProblem
        WFCPolicyProblem
          { wfcPolicyDomains =
              Map.fromList
                [ (alphaSlot, domainFromList [Empty, Hall, Shrine]),
                  (betaSlot, domainFromList [Empty, Hall, Shrine])
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

wfcAutomataLawBundle :: QuickCheckLawBundle String WFCAutomataLawName
wfcAutomataLawBundle =
  quickCheckLawBundle
    "wfc-automata-laws"
    [ quickCheckLawDefinition PolicyMatchesTransitionCompatibility policyMatchesTransitionCompatibilityLaw,
      quickCheckLawDefinition RuleMatchesTransitionCompatibility ruleMatchesTransitionCompatibilityLaw,
      quickCheckLawDefinition CompiledPolicyMatchesTransitionCompatibility compiledPolicyMatchesTransitionCompatibilityLaw
    ]

tests :: TestTree
tests =
  localOption
    (QC.QuickCheckTests 100)
    (lawSuiteGroup "wfc-automata-laws" [quickCheckLawBundleGroup "constraint" id [wfcAutomataLawBundle]])
