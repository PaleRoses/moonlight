module Moonlight.Constraint.Pure.DPLL
  ( dpll,
    unitPropagate,
    pureLiteralEliminate,
    chooseLiteral,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint.Pure.Types
  ( CNF,
    Literal (..),
    literalPolarity,
    literalVariable,
    negateLiteral,
  )

dpll :: Ord a => CNF a -> Maybe (Map.Map a Bool)
dpll clauses =
  case clauses of
    [] -> Just Map.empty
    _
      | any Set.null clauses -> Nothing
      | otherwise ->
          let (clausesAfterUnit, assignmentAfterUnit) = unitPropagate clauses Map.empty
           in if any Set.null clausesAfterUnit
                then Nothing
                else
                  let (clausesAfterPure, assignmentAfterPure) =
                        pureLiteralEliminate clausesAfterUnit assignmentAfterUnit
                   in if any Set.null clausesAfterPure
                        then Nothing
                        else
                          case clausesAfterPure of
                            [] -> Just assignmentAfterPure
                            _ ->
                              case chooseLiteral clausesAfterPure of
                                Nothing -> Just assignmentAfterPure
                                Just literal ->
                                  let variable = literalVariable literal
                                      tryLiteral candidate =
                                        fmap
                                          (Map.union (Map.singleton variable (literalPolarity candidate)))
                                          (dpll (assignLiteral candidate clausesAfterPure))
                                   in case tryLiteral (Pos variable) of
                                        Just branchAssignment ->
                                          Just (Map.union branchAssignment assignmentAfterPure)
                                        Nothing ->
                                          fmap
                                            (`Map.union` assignmentAfterPure)
                                            (tryLiteral (Neg variable))

unitPropagate :: Ord a => CNF a -> Map.Map a Bool -> (CNF a, Map.Map a Bool)
unitPropagate clauses assignment =
  case findUnitClause clauses of
    Nothing -> (clauses, assignment)
    Just literal ->
      let reducedClauses = assignLiteral literal clauses
          variable = literalVariable literal
          polarity = literalPolarity literal
          updatedAssignment = Map.insert variable polarity assignment
       in unitPropagate reducedClauses updatedAssignment

findUnitClause :: CNF a -> Maybe (Literal a)
findUnitClause clauses =
  case filter ((== 1) . Set.size) clauses of
    [] -> Nothing
    unitClause : _ -> Set.lookupMin unitClause

pureLiteralEliminate :: Ord a => CNF a -> Map.Map a Bool -> (CNF a, Map.Map a Bool)
pureLiteralEliminate clauses assignment =
  let allLiterals = foldMap id clauses
      pureLiterals = Set.filter (isPureLiteral allLiterals) allLiterals
   in if Set.null pureLiterals
        then (clauses, assignment)
        else
          let reducedClauses =
                filter (\clause -> Set.null (Set.intersection clause pureLiterals)) clauses
              assignmentAfterPure =
                Set.foldr
                  (\literal acc -> Map.insert (literalVariable literal) (literalPolarity literal) acc)
                  assignment
                  pureLiterals
           in pureLiteralEliminate reducedClauses assignmentAfterPure

isPureLiteral :: Ord a => Set.Set (Literal a) -> Literal a -> Bool
isPureLiteral allLiterals literal =
  not (Set.member (negateLiteral literal) allLiterals)

chooseLiteral :: CNF a -> Maybe (Literal a)
chooseLiteral clauses =
  case clauses of
    [] -> Nothing
    firstClause : _ -> Set.lookupMin firstClause

assignLiteral :: Ord a => Literal a -> CNF a -> CNF a
assignLiteral literal clauses =
  let negated = negateLiteral literal
      keepUnsatisfied clause = not (Set.member literal clause)
      stripNegated = Set.delete negated
   in map stripNegated (filter keepUnsatisfied clauses)
