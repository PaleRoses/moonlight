{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Descent.Context
  ( QuotientDescentObstruction (..),
    PreparedContextDescentScaffold,
    pcdsSite,
    pcdsMaterializedContexts,
    pcdsJoinObstructions,
    pcdsCoverPlans,
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
    ContextSiteOwner,
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

type PreparedContextDescentScaffold :: Type -> Type -> Type -> Type
data PreparedContextDescentScaffold owner ctx classId = PreparedContextDescentScaffold
  { preparedContextDescentSiteInternal :: !(PreparedContextSite owner ctx),
    preparedContextDescentMaterializedContextsInternal :: ![ctx],
    preparedContextDescentJoinObstructionsInternal :: ![QuotientDescentObstruction ctx classId],
    preparedContextDescentCoverPlansInternal :: !(Map ctx (PreparedCoverPlan owner ctx classId))
  }

type role PreparedContextDescentScaffold nominal nominal representational

pcdsSite :: PreparedContextDescentScaffold owner ctx classId -> PreparedContextSite owner ctx
pcdsSite = preparedContextDescentSiteInternal

pcdsMaterializedContexts :: PreparedContextDescentScaffold owner ctx classId -> [ctx]
pcdsMaterializedContexts = preparedContextDescentMaterializedContextsInternal

pcdsJoinObstructions :: PreparedContextDescentScaffold owner ctx classId -> [QuotientDescentObstruction ctx classId]
pcdsJoinObstructions = preparedContextDescentJoinObstructionsInternal

pcdsCoverPlans :: PreparedContextDescentScaffold owner ctx classId -> Map ctx (PreparedCoverPlan owner ctx classId)
pcdsCoverPlans = preparedContextDescentCoverPlansInternal

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
  PreparedContextDescentScaffold (ContextSiteOwner store) ctx classId ->
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
  PreparedContextDescentScaffold (ContextSiteOwner store) ctx classId
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
        { preparedContextDescentSiteInternal = site,
          preparedContextDescentMaterializedContextsInternal = joinClosure,
          preparedContextDescentJoinObstructionsInternal = joinObstructions,
          preparedContextDescentCoverPlansInternal =
            Map.fromList
              [ (contextValue, SheafDescent.preparedCoverPlanAt site contextValue)
              | contextValue <- joinClosure
              ]
        }
