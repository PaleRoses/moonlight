module Moonlight.Core.Term.Database.Atom where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Term.Database.Types
import Prelude

boundColumn :: DenseKey key => QueryBinding key -> (Column, QueryTerm key) -> Maybe (Column, Int)
boundColumn binding (column, term) =
  fmap (\value -> (column, value)) (termBoundValue binding term)
{-# INLINE boundColumn #-}

termBoundValue :: DenseKey key => QueryBinding key -> QueryTerm key -> Maybe Int
termBoundValue binding term =
  case term of
    QueryBound key ->
      Just (encodeDenseKey key)
    QueryVariable variable ->
      fmap encodeDenseKey (Map.lookup variable (queryBindingAssignments binding))
{-# INLINE termBoundValue #-}

atomArity :: QueryAtom f key -> Int
atomArity atom =
  length (atomChildren atom)
{-# INLINE atomArity #-}

atomColumns :: QueryAtom f key -> [Column]
atomColumns atom =
  ResultColumn : fmap ChildColumn [0 .. atomArity atom - 1]
{-# INLINE atomColumns #-}

atomTerms :: QueryAtom f key -> [QueryTerm key]
atomTerms atom =
  atomResult atom : atomChildren atom
{-# INLINE atomTerms #-}

atomBindRow ::
  DenseKey key =>
  QueryAtom f key ->
  QueryBinding key ->
  DatabaseRow ->
  Maybe (QueryBinding key)
atomBindRow atom binding row =
  atomRowValues atom row
    >>= foldM bindTermValue binding . zip (atomTerms atom)
{-# INLINE atomBindRow #-}

atomRowValues :: QueryAtom f key -> DatabaseRow -> Maybe [Int]
atomRowValues atom row
  | atomArity atom == length (rowChildren row) =
      Just (rowResult row : rowChildren row)
  | otherwise =
      Nothing
{-# INLINE atomRowValues #-}

bindTermValue :: DenseKey key => QueryBinding key -> (QueryTerm key, Int) -> Maybe (QueryBinding key)
bindTermValue binding (term, encodedValue) =
  case term of
    QueryBound key
      | encodeDenseKey key == encodedValue ->
          Just binding
      | otherwise ->
          Nothing
    QueryVariable variable ->
      bindVariableValue variable encodedValue binding
{-# INLINE bindTermValue #-}

bindVariableValue :: DenseKey key => QueryVar -> Int -> QueryBinding key -> Maybe (QueryBinding key)
bindVariableValue variable encodedValue binding =
  case Map.lookup variable (queryBindingAssignments binding) of
    Nothing ->
      Just
        binding
          { queryBindingAssignments =
              Map.insert variable (decodeDenseKey encodedValue) (queryBindingAssignments binding)
          }
    Just existingValue
      | encodeDenseKey existingValue == encodedValue ->
          Just binding
      | otherwise ->
          Nothing
{-# INLINE bindVariableValue #-}
