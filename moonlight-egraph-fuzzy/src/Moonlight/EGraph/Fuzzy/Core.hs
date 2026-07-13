module Moonlight.EGraph.Fuzzy.Core
  ( ContinuousBinding (..),
    ContinuousSubstitution (..),
    FuzzyRank (..),
    FuzzyMatch (..),
    RefinementCandidate (..),
    RefinementSolution (..),
    RefinementSolve (..),
    buildRefinementSolve,
    refinementSolveSolution,
    decodeContinuousSubstitution,
    assembleFuzzyMatch,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( Substitution,
    lookupSubst
  )
import Moonlight.EGraph.Pure.Types (ClassId)

type ContinuousBinding :: Type -> Type -> Type
data ContinuousBinding site payload = ContinuousBinding
  { cbClassId :: ClassId,
    cbSite :: site,
    cbPayload :: payload,
    cbResidual :: Double
  }
  deriving stock (Eq, Show)

type ContinuousSubstitution :: Type -> Type -> Type
newtype ContinuousSubstitution site payload = ContinuousSubstitution
  { unContinuousSubstitution :: IntMap (ContinuousBinding site payload)
  }
  deriving stock (Eq, Show)

type FuzzyRank :: Type -> Type
newtype FuzzyRank rank = FuzzyRank
  { unFuzzyRank :: rank
  }
  deriving stock (Eq, Show)

type FuzzyMatch :: Type -> Type -> Type -> Type -> Type -> Type
data FuzzyMatch site payload detail score rank = FuzzyMatch
  { fmRootClass :: ClassId,
    fmDiscreteSubstitution :: Substitution,
    fmContinuousSubstitution :: ContinuousSubstitution site payload,
    fmScore :: score,
    fmRank :: FuzzyRank rank,
    fmDetail :: detail
  }
  deriving stock (Eq, Show)

type RefinementCandidate :: Type -> Type -> Type -> Type
data RefinementCandidate site anchor evidence = RefinementCandidate
  { rcRootClass :: ClassId,
    rcDiscreteSubstitution :: Substitution,
    rcVarSites :: IntMap site,
    rcSites :: [site],
    rcAnchors :: Map site anchor,
    rcEvidence :: evidence
  }
  deriving stock (Eq, Show)

type RefinementSolution :: Type -> Type -> Type -> Type
data RefinementSolution site payload detail = RefinementSolution
  { rslValueBySite :: Map site payload,
    rslResidual :: Double,
    rslDetail :: detail
  }
  deriving stock (Eq, Show)

type RefinementSolve :: Type -> Type -> Type -> Type -> Type -> Type
data RefinementSolve site payload detail score rank = RefinementSolve
  { rsValueBySite :: Map site payload,
    rsResidual :: Double,
    rsScore :: score,
    rsRank :: FuzzyRank rank,
    rsDetail :: detail
  }
  deriving stock (Eq, Show)

buildRefinementSolve ::
  RefinementSolution site payload detail ->
  score ->
  FuzzyRank rank ->
  RefinementSolve site payload detail score rank
buildRefinementSolve solution scoreValue rankValue =
  RefinementSolve
    { rsValueBySite = rslValueBySite solution,
      rsResidual = rslResidual solution,
      rsScore = scoreValue,
      rsRank = rankValue,
      rsDetail = rslDetail solution
    }

refinementSolveSolution ::
  RefinementSolve site payload detail score rank ->
  RefinementSolution site payload detail
refinementSolveSolution solve =
  RefinementSolution
    { rslValueBySite = rsValueBySite solve,
      rslResidual = rsResidual solve,
      rslDetail = rsDetail solve
    }

decodeContinuousSubstitution ::
  Ord site =>
  RefinementCandidate site anchor evidence ->
  Map site payload ->
  Double ->
  ContinuousSubstitution site payload
decodeContinuousSubstitution candidate valueBySite residualValue =
  ContinuousSubstitution
    ( IntMap.mapMaybeWithKey
        ( \patternKey site ->
            lookupSubst (EGraph.mkPatternVar patternKey) (rcDiscreteSubstitution candidate)
              >>= \classId ->
                Map.lookup site valueBySite
                  >>= \payload ->
                    Just
                      ContinuousBinding
                        { cbClassId = classId,
                          cbSite = site,
                          cbPayload = payload,
                          cbResidual = residualValue
                        }
        )
        (rcVarSites candidate)
    )

assembleFuzzyMatch ::
  Ord site =>
  RefinementCandidate site anchor evidence ->
  RefinementSolve site payload detail score rank ->
  FuzzyMatch site payload detail score rank
assembleFuzzyMatch candidate solve =
  FuzzyMatch
    { fmRootClass = rcRootClass candidate,
      fmDiscreteSubstitution = rcDiscreteSubstitution candidate,
      fmContinuousSubstitution =
        decodeContinuousSubstitution candidate (rsValueBySite solve) (rsResidual solve),
      fmScore = rsScore solve,
      fmRank = rsRank solve,
      fmDetail = rsDetail solve
    }
