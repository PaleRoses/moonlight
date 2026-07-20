{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
    assignmentUpperBound,
    budgetAccepts,
    guardEnumerationBudget,
    enumerateMapAssignments,
    enumerateDenseAssignments,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Moonlight.Core (DenseKey (..))

newtype FiniteEnumerationBudget = FiniteEnumerationBudget
  { febMaxCandidateAssignmentsPerObject :: Maybe Natural
  }
  deriving stock (Eq, Show)

assignmentUpperBound :: Foldable values => [(key, values value)] -> Natural
assignmentUpperBound =
  product . fmap (fromIntegral . length . snd)
{-# INLINE assignmentUpperBound #-}

budgetAccepts :: FiniteEnumerationBudget -> Natural -> Bool
budgetAccepts budget candidateCount =
  maybe True (candidateCount <=) (febMaxCandidateAssignmentsPerObject budget)
{-# INLINE budgetAccepts #-}

guardEnumerationBudget :: FiniteEnumerationBudget -> Natural -> failure -> Either failure ()
guardEnumerationBudget budget candidateCount failure =
  if budgetAccepts budget candidateCount then Right () else Left failure
{-# INLINE guardEnumerationBudget #-}

enumerateMapAssignments :: Ord key => [(key, [value])] -> [Map key value]
enumerateMapAssignments =
  foldr extend [Map.empty]
  where
    extend :: Ord key => (key, [value]) -> [Map key value] -> [Map key value]
    extend (key, values) assignments =
      Map.insert key <$> values <*> assignments
{-# INLINEABLE enumerateMapAssignments #-}

enumerateDenseAssignments :: DenseKey key => [(key, [value])] -> [IntMap value]
enumerateDenseAssignments =
  foldr extend [IntMap.empty]
  where
    extend :: DenseKey key => (key, [value]) -> [IntMap value] -> [IntMap value]
    extend (key, values) assignments =
      IntMap.insert (encodeDenseKey key) <$> values <*> assignments
{-# INLINEABLE enumerateDenseAssignments #-}
