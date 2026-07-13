{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.EGraph.Fuzzy.Refiner
  ( RefinementRanking (..),
    RefinementModel (..),
    MatchRefiner (..),
    CompiledSeedMatcher,
    refineModelMatches,
    sortRefinedMatches,
    refineCompiledWithMatcher,
  )
where

import Data.Kind (Constraint, Type)
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Moonlight.Core (Language)
import Moonlight.EGraph.Fuzzy.Core
  ( FuzzyMatch,
    RefinementCandidate,
    RefinementSolution,
    assembleFuzzyMatch,
    buildRefinementSolve,
  )
import Moonlight.EGraph.Fuzzy.Core qualified as FuzzyCore
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery)
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph)

type RefinementRanking :: Type -> Constraint
class RefinementRanking refiner where
  type RefinementScore refiner
  type RefinementRank refiner

  rankRefinementScore ::
    refiner ->
    RefinementScore refiner ->
    FuzzyCore.FuzzyRank (RefinementRank refiner)

  compareRefinementRanks ::
    refiner ->
    FuzzyCore.FuzzyRank (RefinementRank refiner) ->
    FuzzyCore.FuzzyRank (RefinementRank refiner) ->
    Ordering

type RefinementModel :: Type -> Constraint
class RefinementRanking refiner => RefinementModel refiner where
  type ModelSite refiner
  type ModelAnchor refiner
  type ModelEvidence refiner
  type ModelValue refiner
  type ModelDetail refiner
  type ModelBlueprint refiner

  compileRefinementBlueprint ::
    Language f =>
    refiner ->
    CompiledPatternQuery (CompiledGuard capability f) f ->
    ModelBlueprint refiner

  enumerateRefinementCandidates ::
    Language f =>
    refiner ->
    ModelBlueprint refiner ->
    CompiledPatternQuery (CompiledGuard capability f) f ->
    EGraph f a ->
    [(ClassId, Substitution)] ->
    [RefinementCandidate (ModelSite refiner) (ModelAnchor refiner) (ModelEvidence refiner)]

  acceptRefinementCandidate ::
    refiner ->
    ModelBlueprint refiner ->
    RefinementCandidate (ModelSite refiner) (ModelAnchor refiner) (ModelEvidence refiner) ->
    Bool

  solveRefinementCandidate ::
    refiner ->
    ModelBlueprint refiner ->
    RefinementCandidate (ModelSite refiner) (ModelAnchor refiner) (ModelEvidence refiner) ->
    Maybe (RefinementSolution (ModelSite refiner) (ModelValue refiner) (ModelDetail refiner))

  scoreRefinementSolution ::
    refiner ->
    ModelBlueprint refiner ->
    RefinementCandidate (ModelSite refiner) (ModelAnchor refiner) (ModelEvidence refiner) ->
    RefinementSolution (ModelSite refiner) (ModelValue refiner) (ModelDetail refiner) ->
    RefinementScore refiner

type MatchRefiner :: Type -> (Type -> Type) -> Constraint
class RefinementRanking refiner => MatchRefiner refiner f where
  type RefinementSite refiner
  type RefinementValue refiner
  type RefinementDetail refiner

  refineMatches ::
    Language f =>
    refiner ->
    CompiledPatternQuery (CompiledGuard capability f) f ->
    EGraph f a ->
    [(ClassId, Substitution)] ->
    [ FuzzyMatch
        (RefinementSite refiner)
        (RefinementValue refiner)
        (RefinementDetail refiner)
        (RefinementScore refiner)
        (RefinementRank refiner)
    ]

instance
  (RefinementModel refiner, Ord (ModelSite refiner)) =>
  MatchRefiner refiner f
  where
  type RefinementSite refiner = ModelSite refiner
  type RefinementValue refiner = ModelValue refiner
  type RefinementDetail refiner = ModelDetail refiner

  refineMatches = refineModelMatches

type CompiledSeedMatcher :: (Type -> Type) -> Type
type CompiledSeedMatcher f =
  forall capability a.
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [(ClassId, Substitution)]

refineModelMatches ::
  (RefinementModel refiner, Language f, Ord (ModelSite refiner)) =>
  refiner ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [(ClassId, Substitution)] ->
  [ FuzzyMatch
      (ModelSite refiner)
      (ModelValue refiner)
      (ModelDetail refiner)
      (RefinementScore refiner)
      (RefinementRank refiner)
  ]
refineModelMatches refiner compiledQuery graph seedMatches =
  let blueprint = compileRefinementBlueprint refiner compiledQuery
      candidates = enumerateRefinementCandidates refiner blueprint compiledQuery graph seedMatches
   in sortRefinedMatches refiner (candidates >>= refineCandidate blueprint)
  where
    refineCandidate blueprint candidate
      | acceptRefinementCandidate refiner blueprint candidate =
          maybeToList
            ( assembleFuzzyMatch candidate
                <$> refinementSolve blueprint candidate
            )
      | otherwise =
          []

    refinementSolve blueprint candidate =
      fmap
        ( \solution ->
            let scoreValue = scoreRefinementSolution refiner blueprint candidate solution
                rankValue = rankRefinementScore refiner scoreValue
             in buildRefinementSolve solution scoreValue rankValue
        )
        (solveRefinementCandidate refiner blueprint candidate)

refineCompiledWithMatcher ::
  (Language f, MatchRefiner refiner f) =>
  CompiledSeedMatcher f ->
  refiner ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [ FuzzyMatch
      (RefinementSite refiner)
      (RefinementValue refiner)
      (RefinementDetail refiner)
      (RefinementScore refiner)
      (RefinementRank refiner)
  ]
refineCompiledWithMatcher compiledSeedMatcher refiner compiledQuery graph =
  sortRefinedMatches
    refiner
    ( refineMatches
        refiner
        compiledQuery
        graph
        (compiledSeedMatcher compiledQuery graph)
    )

sortRefinedMatches ::
  RefinementRanking refiner =>
  refiner ->
  [ FuzzyMatch
      site
      payload
      detail
      (RefinementScore refiner)
      (RefinementRank refiner)
  ] ->
  [ FuzzyMatch
      site
      payload
      detail
      (RefinementScore refiner)
      (RefinementRank refiner)
  ]
sortRefinedMatches refiner =
  sortBy compareByRank
  where
    compareByRank leftMatch rightMatch =
      compareRefinementRanks
        refiner
        (FuzzyCore.fmRank leftMatch)
        (FuzzyCore.fmRank rightMatch)
