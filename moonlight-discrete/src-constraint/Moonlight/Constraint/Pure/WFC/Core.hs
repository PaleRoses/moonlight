module Moonlight.Constraint.Pure.WFC.Core
  ( solveWFC,
    solveWFCWith,
    solveWFCPolicy,
    solveWFCPolicyWith,
  )
where

import Moonlight.Constraint.Pure.WFC.Algebra
  ( propagateCSP,
    selectNextSlot,
    slotCandidates,
  )
import Moonlight.Constraint.Pure.WFC.Compile
  ( compileWFCPolicyProblem,
    compileWFCProblem,
    projectCompiledPolicyError,
    projectCompiledPolicySearchResult,
  )
import Moonlight.Constraint.Pure.WFC.Search
  ( SearchContext (..),
    searchWithContext,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( BacktrackLimit (..),
    WFCError,
    WFCOptions (..),
    WFCPolicyProblem,
    WFCProblem,
    WFCSearchResult (..),
    defaultWFCOptions,
  )

solveWFC ::
  (Ord slot, Ord value) =>
  WFCProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveWFC =
  solveWFCWith defaultWFCOptions

solveWFCPolicy ::
  (Ord slot, Ord value) =>
  WFCPolicyProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveWFCPolicy =
  solveWFCPolicyWith defaultWFCOptions

solveWFCWith ::
  (Ord slot, Ord value) =>
  WFCOptions ->
  WFCProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveWFCWith options problem = do
  initialPropagation <- propagateCSP (compileWFCProblem problem)
  case initialPropagation of
    Nothing -> pure WFCUnsatisfiable
    Just propagatedProblem -> do
      let BacktrackLimit remainingBacktracks = wfcBacktrackLimit options
      (_, _, result) <-
        searchWithContext
          deterministicSearchContext
          remainingBacktracks
          ()
          propagatedProblem
      pure result

solveWFCPolicyWith ::
  (Ord slot, Ord value) =>
  WFCOptions ->
  WFCPolicyProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveWFCPolicyWith options problem = do
  compiledResult <-
    case solveWFCWith options (compileWFCPolicyProblem problem) of
      Left err -> Left (projectCompiledPolicyError err)
      Right result -> Right result
  projectCompiledPolicySearchResult compiledResult

deterministicSearchContext :: Ord slot => SearchContext () slot value
deterministicSearchContext =
  SearchContext
    { searchSelectSlot =
        \() problem -> selectNextSlot problem,
      searchCandidates =
        \() problem slotId ->
          fmap (\candidates -> (candidates, ())) (slotCandidates problem slotId)
    }
