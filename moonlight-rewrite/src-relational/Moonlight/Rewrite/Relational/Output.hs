{-# LANGUAGE GHC2024 #-}

-- | Relational query output model for rewrite matches.
-- Owns root/binding projection and pinned match keys used for existence
-- checks.
-- Contracts: output arity must match the requested variables, and a
-- 'MatchKey' pins one atom row by atom id plus tuple key.
module Moonlight.Rewrite.Relational.Output
  ( RelationalRewriteMatch (..),
    relationalRewriteMatchOutputVars,
    MatchKey,
    matchKeyFromInts,
    matchKeyAtomId,
    matchKeyPinnedRow,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Core
  ( DenseKey,
  )
import Moonlight.Core
  ( AtomId,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
  )
import Moonlight.Flow.Plan.Query.Core
  ( OutputProjectionObstruction (..),
    QueryOutput (..),
  )

type RelationalRewriteMatch :: Type -> Type -> Type
data RelationalRewriteMatch var key = RelationalRewriteMatch
  { rrmRoot :: !key,
    rrmBindings :: !(Map var key)
  }
  deriving stock (Eq, Ord, Show, Read)

type MatchKey :: Type
data MatchKey = MatchKey
  { mkAtomId :: !AtomId,
    mkPinnedRow :: !RowTupleKey
  }
  deriving stock (Eq, Ord, Show)

matchKeyFromInts :: AtomId -> [Int] -> MatchKey
matchKeyFromInts atomId row =
  MatchKey
    { mkAtomId = atomId,
      mkPinnedRow = tupleKeyFromInts row
    }

matchKeyAtomId :: MatchKey -> AtomId
matchKeyAtomId =
  mkAtomId

matchKeyPinnedRow :: MatchKey -> RowTupleKey
matchKeyPinnedRow =
  mkPinnedRow

instance (Ord var, DenseKey key) => QueryOutput (RelationalRewriteMatch var key) key where
  type OutputVar (RelationalRewriteMatch var key) key = var

  data OutputRecipe (RelationalRewriteMatch var key) key
    = RelationalRewriteMatchOutputRecipe ![var]

  mkOutputRecipe =
    RelationalRewriteMatchOutputRecipe

  projectOutputRecipe (RelationalRewriteMatchOutputRecipe vars) rootKey outputValues
    | expectedArity == actualArity =
        Right
          RelationalRewriteMatch
            { rrmRoot = rootKey,
              rrmBindings =
                Map.fromList
                  (zip vars (Vector.toList outputValues))
            }
    | otherwise =
        Left
          (OutputBindingArityMismatch expectedArity actualArity)
    where
      expectedArity =
        length vars

      actualArity =
        Vector.length outputValues

relationalRewriteMatchOutputVars ::
  OutputRecipe (RelationalRewriteMatch var key) key ->
  [var]
relationalRewriteMatchOutputVars recipe =
  case recipe of
    RelationalRewriteMatchOutputRecipe vars ->
      vars
{-# INLINE relationalRewriteMatchOutputVars #-}
