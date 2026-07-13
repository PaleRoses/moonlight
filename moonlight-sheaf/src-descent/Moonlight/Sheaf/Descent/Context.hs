{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Descent.Context
  ( QuotientDescentObstruction (..),
    PreparedContextDescentScaffold (..),
    DescentReport (..),
    prepareContextDescentScaffold,
    descentAtWithScaffold,
    descentAt,
    fullDescentCheck,
  )
where

import Data.Kind (Type)
import Data.Void (Void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite,
    contextEnumerableContexts,
    contextPreparedSite,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    preparedJoinClosureOver,
  )
import Moonlight.Sheaf.Descent.Context.Support qualified as SupportDescent
import Moonlight.Sheaf.Descent.Core
  ( DescentOutcome (..),
    DescentReport (..),
    collectDescentReport,
  )
import Moonlight.Sheaf.Descent.Quotient
  ( PreparedCoverPlan,
    QuotientDescentObstruction (..),
  )
import Moonlight.Sheaf.Descent.Quotient qualified as SheafDescent
import Moonlight.Sheaf.Verdict
  ( SearchVerdict,
    decidedSearchVerdict,
    rejectedFromList,
  )

type PreparedContextDescentScaffold :: Type -> Type -> Type
data PreparedContextDescentScaffold ctx classId = PreparedContextDescentScaffold
  { pcdsSite :: !(PreparedContextSite ctx),
    pcdsMaterializedContexts :: ![ctx],
    pcdsJoinObstructions :: ![QuotientDescentObstruction ctx classId],
    pcdsCoverPlans :: !(Map ctx (PreparedCoverPlan ctx classId))
  }

descentAt ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  store ->
  SearchVerdict Void (QuotientDescentObstruction ctx classId)
descentAt contextValue store =
  let scaffold = prepareContextDescentScaffold store
   in decidedSearchVerdict (rejectedFromList (pcdsJoinObstructions scaffold))
        <> descentAtWithScaffold store scaffold contextValue

fullDescentCheck ::
  ContextAlgebraSite store ctx classId analysis =>
  store ->
  DescentReport ctx Void (QuotientDescentObstruction ctx classId)
fullDescentCheck store =
  let scaffold = prepareContextDescentScaffold store
      joinObstructions = pcdsJoinObstructions scaffold
      report =
        collectDescentReport
          (pcdsMaterializedContexts scaffold)
          (const True)
          (descentAtWithScaffold store scaffold)
      obstructions =
        joinObstructions <> drObstructions report
      outcome =
        outcomeWithJoinObstructions obstructions (drOutcome report)
   in report
        { drObstructionCount = length obstructions,
          drOutcome = outcome,
          drSatisfied = outcome == DescentSatisfied,
          drObstructions = obstructions
        }

-- | Pure descent at a context: the section-monotonicity sweep, the pinned
-- hypothesis under which meet-form descent surjectivity is a theorem
-- (docs/ELECTION-FREE-DESCENT.md). Election-free and linear; the
-- compatible-family search this replaces checked a consequence of the same
-- hypothesis.
descentAtWithScaffold ::
  ContextAlgebraSite store ctx classId analysis =>
  store ->
  PreparedContextDescentScaffold ctx classId ->
  ctx ->
  SearchVerdict Void (QuotientDescentObstruction ctx classId)
descentAtWithScaffold store scaffold contextValue =
  case Map.lookup contextValue (pcdsCoverPlans scaffold) of
    Nothing ->
      SupportDescent.supportFiberObstructions
        (pcdsSite scaffold)
        contextValue
        store
    Just coverPlan ->
      SupportDescent.supportFiberObstructionsWithPreparedCoverPlan
        contextValue
        coverPlan
        store

outcomeWithJoinObstructions :: [obstruction] -> DescentOutcome -> DescentOutcome
outcomeWithJoinObstructions obstructions outcome =
  case outcome of
    DescentUndecided ->
      DescentUndecided
    _ | null obstructions ->
      DescentSatisfied
    _ ->
      DescentObstructed

prepareContextDescentScaffold ::
  ContextAlgebraSite store ctx classId analysis =>
  store ->
  PreparedContextDescentScaffold ctx classId
prepareContextDescentScaffold store =
  let site = contextPreparedSite store
      base = contextEnumerableContexts store
      (joinClosure, joinFailures) =
        preparedJoinClosureOver site base
      joinObstructions =
        [ DescentJoinLookupObstruction left right lookupError
        | (left, right, lookupError) <- joinFailures
        ]
   in PreparedContextDescentScaffold
        { pcdsSite = site,
          pcdsMaterializedContexts = joinClosure,
          pcdsJoinObstructions = joinObstructions,
          pcdsCoverPlans =
            Map.fromList
              [ (contextValue, SheafDescent.preparedCoverPlanAt site contextValue)
              | contextValue <- joinClosure
              ]
        }
