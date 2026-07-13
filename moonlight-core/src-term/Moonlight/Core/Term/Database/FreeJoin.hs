{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.FreeJoin where

import Control.Monad (foldM)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Arrangement
import Moonlight.Core.Term.Database.Atom
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

freeJoin ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
freeJoin plan database =
  case freeJoinStrategy plan of
    FreeJoinEmptyConjunction ->
      Right ([QueryBinding Map.empty], database)
    FreeJoinExactAtomProbe ->
      exactAtomFreeJoin plan database
    FreeJoinGenericIntersection ->
      genericFreeJoin plan database
{-# INLINE freeJoin #-}

freeJoinStrategy :: FreeJoinPlan f key -> FreeJoinStrategy
freeJoinStrategy plan =
  case freeJoinAtoms plan of
    [] ->
      FreeJoinEmptyConjunction
    [atom]
      | all termLiteralBound (atomChildren atom) ->
          FreeJoinExactAtomProbe
    _ ->
      FreeJoinGenericIntersection
{-# INLINE freeJoinStrategy #-}

termLiteralBound :: QueryTerm key -> Bool
termLiteralBound term =
  case term of
    QueryBound _key ->
      True
    QueryVariable _variable ->
      False
{-# INLINE termLiteralBound #-}

exactAtomFreeJoin ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
exactAtomFreeJoin plan database =
  case freeJoinAtoms plan of
    [atom] -> do
      let emptyBinding :: QueryBinding key
          emptyBinding =
            QueryBinding Map.empty
      (candidateRows, arrangedDatabase) <-
        atomCandidateRows atom emptyBinding database
      pure (mapMaybe (atomBindRow atom emptyBinding . snd) candidateRows, arrangedDatabase)
    _ ->
      genericFreeJoin plan database
{-# INLINE exactAtomFreeJoin #-}

genericFreeJoin ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
genericFreeJoin plan database = do
  (joinedBindings, joinedDatabase) <-
    bindVariables
      plan
      (orderedVariables database plan)
      [QueryBinding Map.empty]
      database
  filterBindings plan joinedBindings joinedDatabase
{-# INLINE genericFreeJoin #-}

strictJoinTraverse ::
  (Database f key -> input -> Either err (Database f key, output)) ->
  Database f key ->
  [input] ->
  Either err (Database f key, [output])
strictJoinTraverse step database inputs =
  fmap
    (\(finalDatabase, reversedOutputs) -> (finalDatabase, reverse reversedOutputs))
    (foldM collect (database, []) inputs)
  where
    collect (!currentDatabase, !reversedOutputs) input = do
      (!nextDatabase, output) <- step currentDatabase input
      pure (nextDatabase, output : reversedOutputs)
{-# INLINE strictJoinTraverse #-}

bindVariables ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  [QueryVar] ->
  [QueryBinding key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
bindVariables plan variables bindings database =
  foldM bindVariable (bindings, database) variables
  where
    bindVariable (!currentBindings, !currentDatabase) variable = do
      (nextDatabase, extendedBindings) <-
        strictJoinTraverse
          (extendVariable plan variable)
          currentDatabase
          currentBindings
      let !nextBindings = concat extendedBindings
      pure (nextBindings, nextDatabase)
{-# INLINE bindVariables #-}

extendVariable ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  QueryVar ->
  Database f key ->
  QueryBinding key ->
  Either ArrangementValidationError (Database f key, [QueryBinding key])
extendVariable plan variable database binding = do
  (candidateKeys, candidateDatabase) <-
    variableCandidates plan variable binding database
  pure $
    case Map.lookup variable (queryBindingAssignments binding) of
      Just boundValue ->
        (database, [binding | IntSet.member (encodeDenseKey boundValue) candidateKeys])
      Nothing ->
        (candidateDatabase, mapMaybe (\key -> bindVariableValue variable key binding) (IntSet.toAscList candidateKeys))
{-# INLINE extendVariable #-}

variableCandidates ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  QueryVar ->
  QueryBinding key ->
  Database f key ->
  Either ArrangementValidationError (IntSet, Database f key)
variableCandidates plan variable binding database = do
  (arrangedDatabase, candidateSets) <-
    strictJoinTraverse
      (atomVariableCandidates variable binding)
      database
      (atomsForVariable variable plan)
  pure (intersectCandidateSets candidateSets, arrangedDatabase)
{-# INLINE variableCandidates #-}

atomVariableCandidates ::
  (DenseKey key, Language f) =>
  QueryVar ->
  QueryBinding key ->
  Database f key ->
  QueryAtom f key ->
  Either ArrangementValidationError (Database f key, IntSet)
atomVariableCandidates variable binding database atom = do
  (candidateRows, arrangedDatabase) <-
    atomCandidateRows atom binding database
  pure
    ( arrangedDatabase,
      IntSet.fromList
        [ encodeDenseKey variableValue
          | (_rowId, row) <- candidateRows,
            Just nextBinding <- [atomBindRow atom binding row],
            Just variableValue <- [Map.lookup variable (queryBindingAssignments nextBinding)]
        ]
    )
{-# INLINE atomVariableCandidates #-}

intersectCandidateSets :: [IntSet] -> IntSet
intersectCandidateSets candidateSets =
  case candidateSets of
    [] ->
      IntSet.empty
    firstSet : restSets ->
      foldl' IntSet.intersection firstSet restSets
{-# INLINE intersectCandidateSets #-}

filterBindings ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  [QueryBinding key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
filterBindings plan bindings database = do
  (checkedDatabase, checkedBindings) <-
    strictJoinTraverse (filterBinding plan) database bindings
  pure (catMaybes checkedBindings, checkedDatabase)
{-# INLINE filterBindings #-}

filterBinding ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  Database f key ->
  QueryBinding key ->
  Either ArrangementValidationError (Database f key, Maybe (QueryBinding key))
filterBinding plan database binding = do
  (checkedDatabase, satisfied) <-
    strictJoinTraverse
      (atomSatisfied binding)
      database
      (freeJoinAtoms plan)
  pure (checkedDatabase, if and satisfied then Just binding else Nothing)
{-# INLINE filterBinding #-}

atomSatisfied ::
  (DenseKey key, Language f) =>
  QueryBinding key ->
  Database f key ->
  QueryAtom f key ->
  Either ArrangementValidationError (Database f key, Bool)
atomSatisfied binding database atom = do
  (candidateRows, arrangedDatabase) <-
    atomCandidateRows atom binding database
  pure (arrangedDatabase, any (isJust . atomBindRow atom binding . snd) candidateRows)
{-# INLINE atomSatisfied #-}

orderedVariables ::
  Language f =>
  Database f key ->
  FreeJoinPlan f key ->
  [QueryVar]
orderedVariables database plan =
  List.sortOn (variableCost database plan) (Set.toList (planVariables plan))
{-# INLINE orderedVariables #-}

planVariables :: FreeJoinPlan f key -> Set.Set QueryVar
planVariables =
  foldMap atomVariables . freeJoinAtoms
{-# INLINE planVariables #-}

atomVariables :: QueryAtom f key -> Set.Set QueryVar
atomVariables =
  foldMap termVariables . atomTerms
{-# INLINE atomVariables #-}

termVariables :: QueryTerm key -> Set.Set QueryVar
termVariables term =
  case term of
    QueryBound _ ->
      Set.empty
    QueryVariable variable ->
      Set.singleton variable
{-# INLINE termVariables #-}

atomsForVariable :: QueryVar -> FreeJoinPlan f key -> [QueryAtom f key]
atomsForVariable variable =
  filter (atomMentionsVariable variable) . freeJoinAtoms
{-# INLINE atomsForVariable #-}

atomMentionsVariable :: QueryVar -> QueryAtom f key -> Bool
atomMentionsVariable variable =
  Set.member variable . atomVariables
{-# INLINE atomMentionsVariable #-}

variableCost ::
  Language f =>
  Database f key ->
  FreeJoinPlan f key ->
  QueryVar ->
  (Int, Int, QueryVar)
variableCost database plan variable =
  ( minimumCandidateDistinct variableDistincts,
    negate (length variableDistincts),
    variable
  )
  where
    variableDistincts =
      [ distinct
        | atom <- atomsForVariable variable plan,
          column <- atomVariableColumns variable atom,
          Just distinct <- [atomColumnDistinct database atom column]
      ]
{-# INLINE variableCost #-}

minimumCandidateDistinct :: [Int] -> Int
minimumCandidateDistinct values =
  case values of
    [] ->
      maxBound
    firstValue : restValues ->
      foldl' min firstValue restValues
{-# INLINE minimumCandidateDistinct #-}

atomVariableColumns :: QueryVar -> QueryAtom f key -> [Column]
atomVariableColumns variable atom =
  [ column
    | (column, QueryVariable columnVariable) <- zip (atomColumns atom) (atomTerms atom),
      columnVariable == variable
  ]
{-# INLINE atomVariableColumns #-}

atomColumnDistinct :: Language f => Database f key -> QueryAtom f key -> Column -> Maybe Int
atomColumnDistinct database atom column =
  Map.lookup (atomOperator atom) (operatorTables database)
    >>= \table -> Just (distinctColumnValueCount table column)
{-# INLINE atomColumnDistinct #-}

atomCandidateRows ::
  (DenseKey key, Language f) =>
  QueryAtom f key ->
  QueryBinding key ->
  Database f key ->
  Either ArrangementValidationError ([(RowId, DatabaseRow)], Database f key)
atomCandidateRows atom binding database =
  case Map.lookup (atomOperator atom) (operatorTables database) of
    Just table
      | Just rowIds <- atomIndexedCandidateRows atom binding table ->
          Right (rowsForIds table rowIds, database)
    _ -> do
      (arrangementKey, arrangementPrefix) <-
        atomArrangementPrefix atom binding
      arrangementRowsForPrefix
        (atomOperator atom)
        arrangementKey
        arrangementPrefix
        database
{-# INLINE atomCandidateRows #-}

atomIndexedCandidateRows ::
  DenseKey key =>
  QueryAtom f key ->
  QueryBinding key ->
  OperatorTable f ->
  Maybe RowIdSet
atomIndexedCandidateRows atom binding table =
  chooseIndexedCandidateRows exactRows resultRows
  where
    exactRows =
      fmap
        (\childKeys -> lookupExactIndex childKeys (derivedExactIndex table))
        (traverse (termBoundValue binding) (atomChildren atom))

    resultRows =
      fmap
        (\resultKey -> lookupResultIndex resultKey (derivedResultIndex table))
        (termBoundValue binding (atomResult atom))
{-# INLINE atomIndexedCandidateRows #-}

chooseIndexedCandidateRows :: Maybe RowIdSet -> Maybe RowIdSet -> Maybe RowIdSet
chooseIndexedCandidateRows exactRows resultRows =
  case (exactRows, resultRows) of
    (Just leftRows, Just rightRows)
      | rowIdSetSize leftRows <= rowIdSetSize rightRows ->
          Just leftRows
      | otherwise ->
          Just rightRows
    (Just rows, Nothing) ->
      Just rows
    (Nothing, Just rows) ->
      Just rows
    (Nothing, Nothing) ->
      Nothing
{-# INLINE chooseIndexedCandidateRows #-}

atomArrangementPrefix ::
  (DenseKey key, Foldable f) =>
  QueryAtom f key ->
  QueryBinding key ->
  Either ArrangementValidationError (ArrangementKey, ArrangementPrefix)
atomArrangementPrefix atom binding = do
  arrangementKey <-
    arrangementKeyForOperator
      (atomOperator atom)
      (fmap fst boundColumns <> unboundColumns)
  arrangementPrefix <-
    arrangementPrefixForKey arrangementKey (fmap snd boundColumns)
  pure (arrangementKey, arrangementPrefix)
  where
    columnTerms =
      zip (atomColumns atom) (atomTerms atom)

    boundColumns =
      mapMaybe (boundColumn binding) columnTerms

    unboundColumns =
      fmap fst (filter (isNothing . termBoundValue binding . snd) columnTerms)
{-# INLINE atomArrangementPrefix #-}
