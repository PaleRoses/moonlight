module ConstraintLaws
  ( tests,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import ConstraintArbitrary ()
import Moonlight.Constraint.Effect.Harness
  ( booleanComplement,
    booleanExcludedMiddle,
    cnfPreservesSatisfiability,
    coFiniteTruthAbsorptionJoin,
    coFiniteTruthAbsorptionMeet,
    coFiniteTruthComplementInvolution,
    deMorganAnd,
    deMorganOr,
    doubleNegationElimination,
    dpllDecisionProcedure,
    dpllSoundness,
    endoPatchActionComposition,
    endoPatchActionIdentity,
    endoPatchCanonicalAssignments,
    endoPatchMonoidAssoc,
    endoPatchMonoidLeftId,
    endoPatchMonoidRightId,
    evaluateHomomorphismAnd,
    evaluateHomomorphismNot,
    evaluateHomomorphismOr,
    implicationSoundness,
    latticeAbsorptionJoin,
    latticeAbsorptionMeet,
    latticeDistributivityAndOverOr,
    latticeDistributivityOrOverAnd,
    normalizeIdempotent,
    normalizeSemanticPreservation,
  )
import Moonlight.Constraint.Effect.LawNames (CommonLawName (..), ConstraintLawName (..))
import Moonlight.Pale.Test.LawSuite
  ( QuickCheckLawBundle,
    lawSuiteGroup,
    quickCheckLawBundle,
    quickCheckLawDefinition,
    quickCheckLawBundleGroup,
    suffixedQuickCheckLawDefinition,
  )
import Test.Tasty (TestTree, localOption)
import qualified Test.Tasty.QuickCheck as QC

type Atom :: Type
data Atom = Alpha | Beta | Gamma
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type Resolver :: Type
newtype Resolver = Resolver (Map.Map Atom Bool)
  deriving stock (Show)

instance QC.Arbitrary Atom where
  arbitrary = QC.elements [Alpha, Beta, Gamma]

instance QC.Arbitrary Resolver where
  arbitrary = Resolver . Map.fromList <$> QC.arbitrary

resolverFn :: Resolver -> Atom -> Bool
resolverFn (Resolver assignments) atom =
  Map.findWithDefault False atom assignments

effectLawBundle :: QuickCheckLawBundle String ConstraintLawName
effectLawBundle =
  quickCheckLawBundle
    "effect-laws"
    [ quickCheckLawDefinition (CommonLaw NormalizeIdempotent) (normalizeIdempotent @Atom),
      quickCheckLawDefinition NormalizeSemanticPreservation (normalizeSemanticPreservation @Atom),
      quickCheckLawDefinition DeMorganAnd (deMorganAnd @Atom),
      quickCheckLawDefinition DeMorganOr (deMorganOr @Atom),
      quickCheckLawDefinition DoubleNegationElimination (doubleNegationElimination @Atom),
      quickCheckLawDefinition (CommonLaw LatticeAbsorptionJoin) (latticeAbsorptionJoin @Atom),
      quickCheckLawDefinition (CommonLaw LatticeAbsorptionMeet) (latticeAbsorptionMeet @Atom),
      quickCheckLawDefinition DistributivityAndOverOr (latticeDistributivityAndOverOr @Atom),
      quickCheckLawDefinition DistributivityOrOverAnd (latticeDistributivityOrOverAnd @Atom),
      quickCheckLawDefinition BooleanComplement (booleanComplement @Atom),
      quickCheckLawDefinition BooleanExcludedMiddle (booleanExcludedMiddle @Atom),
      quickCheckLawDefinition DPLLSoundness (dpllSoundness @Atom),
      quickCheckLawDefinition DPLLDecisionProcedure (dpllDecisionProcedure @Atom),
      quickCheckLawDefinition CNFPreservesSatisfiability (cnfPreservesSatisfiability @Atom),
      suffixedQuickCheckLawDefinition EvaluateHomomorphism "and"
        (\resolver left right -> evaluateHomomorphismAnd @Atom (resolverFn resolver) left right),
      suffixedQuickCheckLawDefinition EvaluateHomomorphism "or"
        (\resolver left right -> evaluateHomomorphismOr @Atom (resolverFn resolver) left right),
      suffixedQuickCheckLawDefinition EvaluateHomomorphism "not"
        (\resolver expression -> evaluateHomomorphismNot @Atom (resolverFn resolver) expression),
      quickCheckLawDefinition ImplicationSoundness
        (\resolver premise conclusion -> implicationSoundness @Atom (resolverFn resolver) premise conclusion),
      quickCheckLawDefinition CoFiniteTruthLatticeAbsorptionJoin
        (coFiniteTruthAbsorptionJoin @Atom),
      quickCheckLawDefinition CoFiniteTruthLatticeAbsorptionMeet
        (coFiniteTruthAbsorptionMeet @Atom),
      quickCheckLawDefinition CoFiniteTruthComplementInvolution
        (coFiniteTruthComplementInvolution @Atom),
      quickCheckLawDefinition EndoPatchMonoidAssoc (endoPatchMonoidAssoc @Atom),
      quickCheckLawDefinition EndoPatchMonoidLeftId (endoPatchMonoidLeftId @Atom),
      quickCheckLawDefinition EndoPatchMonoidRightId (endoPatchMonoidRightId @Atom),
      quickCheckLawDefinition EndoPatchActionComposition (endoPatchActionComposition @Atom),
      quickCheckLawDefinition EndoPatchActionIdentity (endoPatchActionIdentity @Atom),
      quickCheckLawDefinition EndoPatchCanonicalAssignments (endoPatchCanonicalAssignments @Atom)
    ]

tests :: TestTree
tests =
  localOption
    (QC.QuickCheckTests 100)
    (lawSuiteGroup "constraint-effect-laws" [quickCheckLawBundleGroup "constraint" id [effectLawBundle]])
