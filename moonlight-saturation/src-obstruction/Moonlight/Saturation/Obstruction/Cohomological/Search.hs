{-# LANGUAGE TupleSections #-}

module Moonlight.Saturation.Obstruction.Cohomological.Search
  ( FeasibleFamily (..),
    FeasibleFamilySearch (..),
    chooseMinimumFeasibleFamily,
    provisionalFamily,
  )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)

type FeasibleFamily :: Type -> Type -> Type -> Type -> Type
data FeasibleFamily context section report cost = FeasibleFamily
  { ffsChosenSections :: !(Map context section),
    ffsReport :: !report,
    ffsCost :: !cost
  }
  deriving stock (Eq, Ord, Show, Read)

type FeasibleFamilySearch :: Type -> Type -> Type -> Type -> Type
data FeasibleFamilySearch context section report cost = FeasibleFamilySearch
  { ffSearchEvaluateFamily :: !(Map context section -> (report, cost)),
    ffSearchReportSatisfied :: !(report -> Bool),
    ffSearchLowerBound :: !(Maybe (Map context section -> cost)),
    ffSearchFixedSections :: !(Map context section),
    ffSearchCandidateSections :: !(Map context [section])
  }

chooseMinimumFeasibleFamily ::
  (Ord context, Ord cost) =>
  FeasibleFamilySearch context section report cost ->
  Maybe (FeasibleFamily context section report cost)
chooseMinimumFeasibleFamily search =
  go orderedContexts Map.empty Nothing
  where
    orderedContexts =
      sortOn
        (length . snd)
        (Map.toList (Map.difference (ffSearchCandidateSections search) (ffSearchFixedSections search)))

    resolvedSections =
      Map.union (ffSearchFixedSections search)

    lowerBoundOf chosenSections =
      fmap
        (\lowerBound -> lowerBound (resolvedSections chosenSections))
        (ffSearchLowerBound search)

    admissiblyPruned chosenSections bestSoFar =
      case (lowerBoundOf chosenSections, bestSoFar) of
        (Just bound, Just bestFamily) ->
          bound >= ffsCost bestFamily
        _ ->
          False

    orderedCandidateBranches contextValue candidates chosenSections =
      case ffSearchLowerBound search of
        Nothing ->
          map (, Nothing) candidates
        Just lowerBound ->
          sortOn
            snd
            [ ( candidateSection,
                Just
                  ( lowerBound
                      ( resolvedSections
                          (Map.insert contextValue candidateSection chosenSections)
                      )
                  )
              )
            | candidateSection <- candidates
            ]

    go [] chosenSections bestSoFar =
      let familySections =
            resolvedSections chosenSections
          (reportValue, costValue) =
            ffSearchEvaluateFamily search familySections
          candidateFamily =
            FeasibleFamily
              { ffsChosenSections = familySections,
                ffsReport = reportValue,
                ffsCost = costValue
              }
       in if ffSearchReportSatisfied search reportValue
            then betterFeasibleFamily bestSoFar candidateFamily
            else bestSoFar
    go ((contextValue, candidates) : remainingContexts) chosenSections bestSoFar
      | admissiblyPruned chosenSections bestSoFar =
          bestSoFar
      | otherwise =
          foldl'
            ( \currentBest (candidateSection, maybeCandidateBound) ->
                let nextChosenSections =
                      Map.insert contextValue candidateSection chosenSections
                 in case (maybeCandidateBound, currentBest) of
                      (Just candidateBound, Just bestFamily)
                        | candidateBound >= ffsCost bestFamily ->
                            currentBest
                      _ ->
                        go remainingContexts nextChosenSections currentBest
            )
            bestSoFar
            (orderedCandidateBranches contextValue candidates chosenSections)

betterFeasibleFamily ::
  Ord cost =>
  Maybe (FeasibleFamily context section report cost) ->
  FeasibleFamily context section report cost ->
  Maybe (FeasibleFamily context section report cost)
betterFeasibleFamily Nothing candidateFamily =
  Just candidateFamily
betterFeasibleFamily (Just bestFamily) candidateFamily
  | ffsCost candidateFamily < ffsCost bestFamily =
      Just candidateFamily
  | otherwise =
      Just bestFamily

provisionalFamily ::
  Ord context =>
  (context -> section) ->
  [context] ->
  Map context section ->
  Map context [section] ->
  Map context section
provisionalFamily emptySection contexts fixedSections candidateSections =
  Map.union fixedSections $
    Map.fromList
      [ ( contextValue,
          fromMaybe
            (emptySection contextValue)
            (Map.lookup contextValue candidateSections >>= listToMaybe)
        )
      | contextValue <- contexts
      ]
