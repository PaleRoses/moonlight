module Moonlight.Constraint.Effect.Harness
  ( normalizeIdempotent,
    normalizeSemanticPreservation,
    deMorganAnd,
    deMorganOr,
    doubleNegationElimination,
    booleanComplement,
    booleanExcludedMiddle,
    latticeAbsorptionJoin,
    latticeAbsorptionMeet,
    latticeDistributivityAndOverOr,
    latticeDistributivityOrOverAnd,
    dpllSoundness,
    dpllDecisionProcedure,
    cnfPreservesSatisfiability,
    evaluateHomomorphismAnd,
    evaluateHomomorphismOr,
    evaluateHomomorphismNot,
    implicationSoundness,
    coFiniteTruthNormalizationIdempotent,
    coFiniteTruthAbsorptionJoin,
    coFiniteTruthAbsorptionMeet,
    coFiniteTruthComplementInvolution,
    endoPatchNormalizationIdempotent,
    endoPatchMonoidAssoc,
    endoPatchMonoidLeftId,
    endoPatchMonoidRightId,
    endoPatchActionComposition,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Algebra
  ( BooleanAlgebra (..),
    BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    MeetSemilattice (..),
  )
import Moonlight.Constraint.Pure.CNF (toCNF)
import Moonlight.Constraint.Pure.CoFiniteTruth
  ( CoFiniteTruth,
    EndoPatch,
    applyEndoPatch,
    normalizeCoFiniteTruth,
    normalizeEndoPatch,
  )
import Moonlight.Constraint.Pure.DPLL (dpll)
import Moonlight.Constraint.Pure.Evaluate (atoms, equivalent, evaluate, implies)
import Moonlight.Constraint.Pure.Normalize (normalize)
import Moonlight.Constraint.Pure.Types
  ( CNF,
    ConstraintExpr (..),
    Literal,
    literalPolarity,
    literalVariable,
  )

normalizeIdempotent :: Ord a => ConstraintExpr a -> Bool
normalizeIdempotent expression =
  normalize (normalize expression) == normalize expression

normalizeSemanticPreservation :: Ord a => ConstraintExpr a -> Bool
normalizeSemanticPreservation expression =
  equivalent (normalize expression) expression

deMorganAnd :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
deMorganAnd left right =
  normalize (Not (And [left, right]))
    == normalize (Or [Not left, Not right])

deMorganOr :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
deMorganOr left right =
  normalize (Not (Or [left, right]))
    == normalize (And [Not left, Not right])

doubleNegationElimination :: Ord a => ConstraintExpr a -> Bool
doubleNegationElimination expression =
  normalize (Not (Not expression)) == normalize expression

booleanComplement :: Ord a => ConstraintExpr a -> Bool
booleanComplement expression =
  equivalent (meet expression (complement expression)) bottom

booleanExcludedMiddle :: Ord a => ConstraintExpr a -> Bool
booleanExcludedMiddle expression =
  equivalent (join expression (complement expression)) top

latticeAbsorptionJoin :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
latticeAbsorptionJoin left right =
  equivalent (join left (meet left right)) left

latticeAbsorptionMeet :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
latticeAbsorptionMeet left right =
  equivalent (meet left (join left right)) left

latticeDistributivityAndOverOr :: Ord a => ConstraintExpr a -> ConstraintExpr a -> ConstraintExpr a -> Bool
latticeDistributivityAndOverOr first second third =
  equivalent
    (meet first (join second third))
    (join (meet first second) (meet first third))

latticeDistributivityOrOverAnd :: Ord a => ConstraintExpr a -> ConstraintExpr a -> ConstraintExpr a -> Bool
latticeDistributivityOrOverAnd first second third =
  equivalent
    (join first (meet second third))
    (meet (join first second) (join first third))

allAssignments :: Ord a => Set.Set a -> [Map.Map a Bool]
allAssignments variables =
  case Set.minView variables of
    Nothing -> [Map.empty]
    Just (variable, rest) ->
      allAssignments rest
        >>= \assignment ->
          [ Map.insert variable False assignment,
            Map.insert variable True assignment
          ]

satisfiableByEnumeration :: Ord a => ConstraintExpr a -> Bool
satisfiableByEnumeration expression =
  any
    (\assignment -> evaluate (\variable -> Map.findWithDefault False variable assignment) expression)
    (allAssignments (atoms expression))

satisfiableCNFByEnumeration :: Ord a => CNF a -> Bool
satisfiableCNFByEnumeration clauses =
  let variables = foldMap (Set.map literalVariable) clauses
      evalLiteral :: Ord a => Map.Map a Bool -> Literal a -> Bool
      evalLiteral assignment literal =
        Map.findWithDefault False (literalVariable literal) assignment
          == literalPolarity literal
      evalClause :: Ord a => Map.Map a Bool -> Set.Set (Literal a) -> Bool
      evalClause assignment clause =
        any (evalLiteral assignment) (Set.toList clause)
      evalCNF assignment =
        all (evalClause assignment) clauses
   in any evalCNF (allAssignments variables)

dpllSoundness :: Ord a => ConstraintExpr a -> Bool
dpllSoundness expression =
  case dpll (toCNF expression) of
    Nothing -> True
    Just assignment ->
      evaluate
        (\variable -> Map.findWithDefault False variable assignment)
        expression

dpllDecisionProcedure :: Ord a => ConstraintExpr a -> Bool
dpllDecisionProcedure expression =
  let atomBound = 10
      withinBound = Set.size (atoms expression) <= atomBound
   in if withinBound
        then
          let satBySearch = maybe False (const True) (dpll (toCNF expression))
           in satBySearch == satisfiableByEnumeration expression
        else True

cnfPreservesSatisfiability :: Ord a => ConstraintExpr a -> Bool
cnfPreservesSatisfiability expression =
  let atomBound = 10
      withinBound = Set.size (atoms expression) <= atomBound
   in if withinBound
        then satisfiableByEnumeration expression == satisfiableCNFByEnumeration (toCNF expression)
        else True

evaluateHomomorphismAnd :: (a -> Bool) -> ConstraintExpr a -> ConstraintExpr a -> Bool
evaluateHomomorphismAnd resolver left right =
  evaluate resolver (And [left, right])
    == (evaluate resolver left && evaluate resolver right)

evaluateHomomorphismOr :: (a -> Bool) -> ConstraintExpr a -> ConstraintExpr a -> Bool
evaluateHomomorphismOr resolver left right =
  evaluate resolver (Or [left, right])
    == (evaluate resolver left || evaluate resolver right)

evaluateHomomorphismNot :: (a -> Bool) -> ConstraintExpr a -> Bool
evaluateHomomorphismNot resolver expression =
  evaluate resolver (Not expression)
    == not (evaluate resolver expression)

implicationSoundness :: Ord a => (a -> Bool) -> ConstraintExpr a -> ConstraintExpr a -> Bool
implicationSoundness resolver premise conclusion =
  if implies premise conclusion && evaluate resolver premise
    then evaluate resolver conclusion
    else True

coFiniteTruthNormalizationIdempotent :: Ord k => CoFiniteTruth k -> Bool
coFiniteTruthNormalizationIdempotent value =
  normalizeCoFiniteTruth (normalizeCoFiniteTruth value)
    == normalizeCoFiniteTruth value

coFiniteTruthAbsorptionJoin :: Ord k => CoFiniteTruth k -> CoFiniteTruth k -> Bool
coFiniteTruthAbsorptionJoin left right =
  join left (meet left right) == left

coFiniteTruthAbsorptionMeet :: Ord k => CoFiniteTruth k -> CoFiniteTruth k -> Bool
coFiniteTruthAbsorptionMeet left right =
  meet left (join left right) == left

coFiniteTruthComplementInvolution :: Ord k => CoFiniteTruth k -> Bool
coFiniteTruthComplementInvolution value =
  complement (complement value) == value

endoPatchNormalizationIdempotent :: Ord k => EndoPatch k -> Bool
endoPatchNormalizationIdempotent patch =
  normalizeEndoPatch (normalizeEndoPatch patch)
    == normalizeEndoPatch patch

endoPatchMonoidAssoc :: Ord k => EndoPatch k -> EndoPatch k -> EndoPatch k -> Bool
endoPatchMonoidAssoc first second third =
  (<>) first ((<>) second third)
    == (<>) ((<>) first second) third

endoPatchMonoidLeftId :: Ord k => EndoPatch k -> Bool
endoPatchMonoidLeftId value =
  (<>) mempty value == value

endoPatchMonoidRightId :: Ord k => EndoPatch k -> Bool
endoPatchMonoidRightId value =
  (<>) value mempty == value

endoPatchActionComposition :: Ord k => EndoPatch k -> EndoPatch k -> CoFiniteTruth k -> Bool
endoPatchActionComposition first second truthValue =
  applyEndoPatch second (applyEndoPatch first truthValue)
    == applyEndoPatch ((<>) first second) truthValue
