-- | Descent outcome vocabulary: reports and their collection.
module Moonlight.Sheaf.Descent.Core
  ( DescentOutcome (..),
    DescentReport (..),
    emptyDescentReport,
    collectDescentReport,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
    searchVerdictObstructions,
    searchVerdictRefusals,
  )

type DescentOutcome :: Type
data DescentOutcome
  = DescentSatisfied
  | DescentObstructed
  | DescentUndecided
  deriving stock (Eq, Ord, Show)

type DescentReport :: Type -> Type -> Type -> Type
data DescentReport ctx refusal obstruction = DescentReport
  { drContextCount :: !Int,
    drObstructionCount :: !Int,
    drOutcome :: !DescentOutcome,
    drSatisfied :: !Bool,
    drRefusals :: ![refusal],
    drObstructions :: ![obstruction]
  }
  deriving stock (Eq, Show)

emptyDescentReport :: DescentReport ctx refusal obstruction
emptyDescentReport =
  DescentReport
    { drContextCount = 0,
      drObstructionCount = 0,
      drOutcome = DescentSatisfied,
      drSatisfied = True,
      drRefusals = [],
      drObstructions = []
    }

collectDescentReport ::
  [ctx] ->
  (ctx -> Bool) ->
  (ctx -> SearchVerdict refusal obstruction) ->
  DescentReport ctx refusal obstruction
collectDescentReport contexts shouldCheck verdictAt =
  let searchVerdict =
        foldMap
          verdictAt
          (filter shouldCheck contexts)
      obstructions =
        searchVerdictObstructions searchVerdict
      outcome =
        descentOutcome searchVerdict
   in DescentReport
        { drContextCount = length contexts,
          drObstructionCount = length obstructions,
          drOutcome = outcome,
          drSatisfied = outcome == DescentSatisfied,
          drRefusals = searchVerdictRefusals searchVerdict,
          drObstructions = obstructions
        }

descentOutcome :: SearchVerdict refusal obstruction -> DescentOutcome
descentOutcome searchVerdict =
  case searchVerdict of
    SearchAccepted ->
      DescentSatisfied
    SearchRejected _ ->
      DescentObstructed
    SearchUndecided _ _ ->
      DescentUndecided
