{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Moonlight.Flow.Carrier.Core.Coverage
  ( ObstructionToken (..),
    ObstructionTokenSet (..),
    obstructionTokenSet,
    CoverageFact (..),
    obstructedCoverage,
    coverageFactExact,
    joinCoverageFact,
    joinCoverageFactList,
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Algebra
  ( JoinSemilattice (..),
  )

type ObstructionToken :: Type
newtype ObstructionToken = ObstructionToken
  { unObstructionToken :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type ObstructionTokenSet :: Type
newtype ObstructionTokenSet = ObstructionTokenSet
  { unObstructionTokenSet :: Set ObstructionToken
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Semigroup, Monoid)

obstructionTokenSet :: [ObstructionToken] -> ObstructionTokenSet
obstructionTokenSet =
  ObstructionTokenSet . Set.fromList

type CoverageFact :: Type
data CoverageFact
  = ExactLocal
  | ExactRestricted
  | ExactAmalgamated
  | LowerBound
  | Obstructed !ObstructionTokenSet
  deriving stock (Eq, Ord, Show, Read)

obstructedCoverage :: ObstructionToken -> CoverageFact
obstructedCoverage =
  Obstructed . obstructionTokenSet . (: [])

coverageFactExact :: CoverageFact -> Bool
coverageFactExact coverage =
  case coverage of
    ExactLocal -> True
    ExactRestricted -> True
    ExactAmalgamated -> True
    LowerBound -> False
    Obstructed {} -> False
{-# INLINE coverageFactExact #-}

instance JoinSemilattice ObstructionTokenSet where
  join =
    (<>)

instance JoinSemilattice CoverageFact where
  join =
    joinCoverageFact

joinCoverageFact :: CoverageFact -> CoverageFact -> CoverageFact
joinCoverageFact left right =
  case (left, right) of
    (Obstructed leftTokens, Obstructed rightTokens) ->
      Obstructed (leftTokens <> rightTokens)
    (Obstructed tokens, _) ->
      Obstructed tokens
    (_, Obstructed tokens) ->
      Obstructed tokens
    (LowerBound, _) ->
      LowerBound
    (_, LowerBound) ->
      LowerBound
    (ExactAmalgamated, _) ->
      ExactAmalgamated
    (_, ExactAmalgamated) ->
      ExactAmalgamated
    (ExactRestricted, _) ->
      ExactRestricted
    (_, ExactRestricted) ->
      ExactRestricted
    (ExactLocal, ExactLocal) ->
      ExactLocal

joinCoverageFactList :: [CoverageFact] -> Maybe CoverageFact
joinCoverageFactList coverages =
  case coverages of
    [] ->
      Nothing
    firstCoverage : remainingCoverages ->
      Just (foldr joinCoverageFact firstCoverage remainingCoverages)
