{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Analysis.SheafRefinement
  ( SheafEnergy (..),
    SheafSolve (..),
    SheafRefinementModel (..),
    SheafRefiner (..),
    refineSheafSection,
    refineSheafCompiledWithMatcher,
  )
where

import Data.List (sortBy)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Moonlight.EGraph.Fuzzy.Core
  ( FuzzyMatch,
    FuzzyRank,
    RefinementCandidate,
    RefinementSolution (..),
    assembleFuzzyMatch,
    buildRefinementSolve,
  )
import qualified Moonlight.EGraph.Fuzzy.Core as FuzzyCore
import Moonlight.EGraph.Fuzzy.Refiner
  ( CompiledSeedMatcher,
    RefinementModel (..),
    RefinementRanking (..),
  )
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery)
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph)

type SheafEnergy :: Type -> Type
newtype SheafEnergy score = SheafEnergy
  { unSheafEnergy :: score
  }
  deriving stock (Eq, Ord, Show)

type SheafSolve :: Type -> Type -> Type -> Type
data SheafSolve site payload detail = SheafSolve
  { ssValueBySite :: Map site payload,
    ssResidual :: Double,
    ssDetail :: detail
  }
  deriving stock (Eq, Show)

type SheafRefinementModel :: Type -> Constraint
class SheafRefinementModel model where
  type SheafSite model
  type SheafAnchor model
  type SheafEvidence model
  type SheafValue model
  type SheafDetail model
  type SheafBlueprint model
  type SheafScore model
  type SheafRank model
  type SheafSeed model

  compileSheafBlueprint ::
    model ->
    SheafBlueprint model

  enumerateSheafCandidates ::
    model ->
    SheafBlueprint model ->
    [SheafSeed model] ->
    [RefinementCandidate (SheafSite model) (SheafAnchor model) (SheafEvidence model)]

  acceptSheafCandidate ::
    model ->
    SheafBlueprint model ->
    RefinementCandidate (SheafSite model) (SheafAnchor model) (SheafEvidence model) ->
    Bool

  solveSheafCandidate ::
    model ->
    SheafBlueprint model ->
    RefinementCandidate (SheafSite model) (SheafAnchor model) (SheafEvidence model) ->
    Maybe (SheafSolve (SheafSite model) (SheafValue model) (SheafDetail model))

  interpretSheafSolve ::
    model ->
    SheafBlueprint model ->
    RefinementCandidate (SheafSite model) (SheafAnchor model) (SheafEvidence model) ->
    SheafSolve (SheafSite model) (SheafValue model) (SheafDetail model) ->
    SheafEnergy (SheafScore model)

  rankSheafEnergy ::
    model ->
    SheafEnergy (SheafScore model) ->
    FuzzyRank (SheafRank model)

  compareSheafRanks ::
    model ->
    FuzzyRank (SheafRank model) ->
    FuzzyRank (SheafRank model) ->
    Ordering

type SheafRefiner :: Type -> Type
newtype SheafRefiner model = SheafRefiner
  { sheafRefinerModel :: model
  }

instance
  SheafRefinementModel model =>
  RefinementRanking (SheafRefiner model)
  where
  type RefinementScore (SheafRefiner model) = SheafScore model
  type RefinementRank (SheafRefiner model) = SheafRank model

  rankRefinementScore SheafRefiner {..} scoreValue =
    rankSheafEnergy sheafRefinerModel (SheafEnergy scoreValue)

  compareRefinementRanks SheafRefiner {..} =
    compareSheafRanks sheafRefinerModel

instance
  (SheafRefinementModel model, SheafSeed model ~ (ClassId, Substitution)) =>
  RefinementModel (SheafRefiner model)
  where
  type ModelSite (SheafRefiner model) = SheafSite model
  type ModelAnchor (SheafRefiner model) = SheafAnchor model
  type ModelEvidence (SheafRefiner model) = SheafEvidence model
  type ModelValue (SheafRefiner model) = SheafValue model
  type ModelDetail (SheafRefiner model) = SheafDetail model
  type ModelBlueprint (SheafRefiner model) = SheafBlueprint model

  compileRefinementBlueprint SheafRefiner {..} _ =
    compileSheafBlueprint sheafRefinerModel

  enumerateRefinementCandidates SheafRefiner {..} blueprint _ _ seedMatches =
    enumerateSheafCandidates sheafRefinerModel blueprint seedMatches

  acceptRefinementCandidate SheafRefiner {..} =
    acceptSheafCandidate sheafRefinerModel

  solveRefinementCandidate SheafRefiner {..} blueprint candidate =
    sheafSolveToRefinementSolution
      <$> solveSheafCandidate sheafRefinerModel blueprint candidate

  scoreRefinementSolution SheafRefiner {..} blueprint candidate solution =
    let sheafSolve = refinementSolutionToSheafSolve solution
        sheafEnergy =
          interpretSheafSolve sheafRefinerModel blueprint candidate sheafSolve
     in unSheafEnergy sheafEnergy

sheafSolveToRefinementSolution ::
  SheafSolve site payload detail ->
  RefinementSolution site payload detail
sheafSolveToRefinementSolution sheafSolve =
  RefinementSolution
    { rslValueBySite = ssValueBySite sheafSolve,
      rslResidual = ssResidual sheafSolve,
      rslDetail = ssDetail sheafSolve
    }

refinementSolutionToSheafSolve ::
  RefinementSolution site payload detail ->
  SheafSolve site payload detail
refinementSolutionToSheafSolve solution =
  SheafSolve
    { ssValueBySite = rslValueBySite solution,
      ssResidual = rslResidual solution,
      ssDetail = rslDetail solution
    }

refineSheafSection ::
  ( SheafRefinementModel model,
    Ord (SheafSite model)
  ) =>
  model ->
  [SheafSeed model] ->
  [FuzzyMatch (SheafSite model) (SheafValue model) (SheafDetail model) (SheafScore model) (SheafRank model)]
refineSheafSection model seeds =
  let blueprint = compileSheafBlueprint model
      candidates = enumerateSheafCandidates model blueprint seeds
   in sortBy (compareSheafMatchRanks model) (candidates >>= refineCandidate blueprint)
  where
    refineCandidate blueprint candidate
      | acceptSheafCandidate model blueprint candidate =
          maybeToList
            ( assembleFuzzyMatch candidate
                <$> refinementSolve blueprint candidate
            )
      | otherwise =
          []

    refinementSolve blueprint candidate =
      fmap
        ( \sheafSolve ->
            let solution = sheafSolveToRefinementSolution sheafSolve
                scoreValue =
                  unSheafEnergy
                    (interpretSheafSolve model blueprint candidate sheafSolve)
                rankValue = rankSheafEnergy model (SheafEnergy scoreValue)
             in buildRefinementSolve solution scoreValue rankValue
        )
        (solveSheafCandidate model blueprint candidate)

    compareSheafMatchRanks ::
      SheafRefinementModel modelValue =>
      modelValue ->
      FuzzyMatch siteLeft payloadLeft detailLeft scoreLeft (SheafRank modelValue) ->
      FuzzyMatch siteRight payloadRight detailRight scoreRight (SheafRank modelValue) ->
      Ordering
    compareSheafMatchRanks modelValue leftMatch rightMatch =
      compareSheafRanks
        modelValue
        (FuzzyCore.fmRank leftMatch)
        (FuzzyCore.fmRank rightMatch)

refineSheafCompiledWithMatcher ::
  ( SheafRefinementModel model,
    SheafSeed model ~ (ClassId, Substitution),
    Ord (SheafSite model)
  ) =>
  CompiledSeedMatcher f ->
  SheafRefiner model ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [FuzzyMatch (SheafSite model) (SheafValue model) (SheafDetail model) (SheafScore model) (SheafRank model)]
refineSheafCompiledWithMatcher seedMatcher (SheafRefiner model) compiledQuery graph =
  refineSheafSection model (seedMatcher compiledQuery graph)
