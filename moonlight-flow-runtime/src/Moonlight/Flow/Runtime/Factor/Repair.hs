{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Repair
  ( repairFactorBatch,
  )
where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid
  ( Endo (..),
    appEndo,
  )
import Data.Ord
  ( comparing,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Commit
  ( ExactFactorRepairCommit (..),
    commitExactFactorRepairResults,
    commitFactorReuseAction,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Engine
  ( selectFactorReuseAction,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Exact
  ( ExactFactorRepairPrepared (..),
    ExactFactorRepairResult (..),
    exactFactorRepairPreparedShareable,
    prepareExactFactorRepair,
    runPreparedExactFactorRepair,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairBatchRequest,
    FactorRepairRequest (..),
    factorRepairBatchRequests,
  )
import Moonlight.Flow.Runtime.Factor.Reuse
  ( FactorRepairReport,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorRepairSubscribers,
    lookupFactorProgramByKey,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )

data FactorRepairMember ctx prop = FactorRepairMember
  { frmRequest :: !(FactorRepairRequest ctx prop),
    frmProgram :: !FactorProgram
  }

repairFactorBatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  FactorRepairBatchRequest ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( [FactorRepairReport ctx prop boundary evidence],
      RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
repairFactorBatch eventTime batch runtime0 = do
  members <-
    traverse
      (`lookupRepairMember` runtime0)
      (factorRepairBatchRequests batch)
  repairMembersLoop
    eventTime
    []
    mempty
    members
    runtime0

lookupRepairMember ::
  FactorRepairRequest ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorRepairMember ctx prop)
lookupRepairMember request runtime =
  case lookupFactorProgramByKey (frrRepairKey request) runtime of
    Nothing ->
      Left (RuntimeMissingFactorProgram queryId)
    Just program ->
      Right
        FactorRepairMember
          { frmRequest = request,
            frmProgram = program
          }
  where
    queryId =
      frrQueryId request

repairMembersLoop ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  [FactorRepairReport ctx prop boundary evidence] ->
  CarrierCommitTrace ctx prop ->
  [FactorRepairMember ctx prop] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( [FactorRepairReport ctx prop boundary evidence],
      RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
repairMembersLoop _eventTime reports commitTrace [] runtime =
  Right (reports, runtime, commitTrace)
repairMembersLoop eventTime reports0 commitTrace0 pending0 runtime0 = do
  (runtime1, reports1, commitTrace1, exactPending) <-
    planAndCommitReuse
      eventTime
      reports0
      commitTrace0
      pending0
      runtime0
  if null exactPending
    then
      Right (reports1, runtime1, commitTrace1)
    else do
      prepared <-
        traverse
          (prepareExactMember runtime1)
          exactPending
      case largestExactGroup (groupExactRepairs prepared) of
        Nothing ->
          Right (reports1, runtime1, commitTrace1)
        Just group -> do
          (runtime2, exactCommits) <-
            runExactGroup eventTime group runtime1
          (exactReports, runtime3, exactTrace) <-
            commitExactFactorRepairResults
              eventTime
              exactCommits
              runtime2
          let !repairedQueries =
                Set.fromList
                  [ frrQueryId (efrcRequest commit)
                  | commit <- exactCommits
                  ]
              !stillPending =
                filter
                  ( \member ->
                      not (Set.member (memberQueryId member) repairedQueries)
                  )
                  exactPending
          repairMembersLoop
            eventTime
            (reports1 <> exactReports)
            (commitTrace1 <> exactTrace)
            stillPending
            runtime3

planAndCommitReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  [FactorRepairReport ctx prop boundary evidence] ->
  CarrierCommitTrace ctx prop ->
  [FactorRepairMember ctx prop] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [FactorRepairReport ctx prop boundary evidence],
      CarrierCommitTrace ctx prop,
      [FactorRepairMember ctx prop]
    )
planAndCommitReuse eventTime reports0 trace0 members runtime0 = do
  (runtime1, reports, trace1, exactPending) <-
    Foldable.foldlM
      (commitReuseOrKeepExact eventTime)
      (runtime0, mempty, trace0, mempty)
      members
  pure
    ( runtime1,
      reports0 <> appEndo reports [],
      trace1,
      appEndo exactPending []
    )

commitReuseOrKeepExact ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
    Endo [FactorRepairReport ctx prop boundary evidence],
    CarrierCommitTrace ctx prop,
    Endo [FactorRepairMember ctx prop]
  ) ->
  FactorRepairMember ctx prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      Endo [FactorRepairReport ctx prop boundary evidence],
      CarrierCommitTrace ctx prop,
      Endo [FactorRepairMember ctx prop]
    )
commitReuseOrKeepExact eventTime (runtime0, reports, commitTrace0, exactPending) member = do
  (runtime1, maybeReuse) <-
    selectFactorReuseAction
      eventTime
      (frmRequest member)
      (frmProgram member)
      runtime0
  case maybeReuse of
    Just reuseAction -> do
      (report, runtime2, commitTrace) <-
        commitFactorReuseAction
          (frmRequest member)
          (frmProgram member)
          reuseAction
          runtime1
      pure
        ( runtime2,
          reports <> Endo (report :),
          commitTrace0 <> commitTrace,
          exactPending
        )
    Nothing ->
      pure
        ( runtime1,
          reports,
          commitTrace0,
          exactPending <> Endo (member :)
        )

prepareExactMember ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  FactorRepairMember ctx prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (ExactFactorRepairPrepared ctx prop boundary evidence joinState joinErr)
prepareExactMember runtime member =
  prepareExactFactorRepair
    (reAtomCarrierEmitSpec (rdrEnv runtime))
    (reFactorCarrierEmitSpec (rdrEnv runtime))
    (frmRequest member)
    (frmProgram member)
    runtime

groupExactRepairs ::
  [ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr] ->
  [NonEmpty (ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr)]
groupExactRepairs =
  Foldable.foldl'
    insertExactRepairGroup
    []

insertExactRepairGroup ::
  [NonEmpty (ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr)] ->
  ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr ->
  [NonEmpty (ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr)]
insertExactRepairGroup [] prepared =
  [prepared :| []]
insertExactRepairGroup (group@(representative :| rest) : groups) prepared
  | exactFactorRepairPreparedShareable representative prepared =
      (representative :| (prepared : rest)) : groups
  | otherwise =
      group : insertExactRepairGroup groups prepared

largestExactGroup ::
  [NonEmpty value] ->
  Maybe (NonEmpty value)
largestExactGroup =
  fmap (Foldable.maximumBy (comparing NonEmpty.length)) . NonEmpty.nonEmpty

runExactGroup ::
  (boundary ~ RuntimeBoundary) =>
  RelationalCarrierTime ctx ->
  NonEmpty (ExactFactorRepairPrepared ctx prop boundary evidence joinState joinErr) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr]
    )
runExactGroup eventTime (representative :| _) runtime0 = do
  representativeResult <-
    runPreparedExactFactorRepair
      (reFactorCarrierEmitSpec (rdrEnv runtime0))
      eventTime
      representative
  let !representativeRequest =
        efrpRequest representative
      !subscriberCount =
        length (factorRepairSubscribers (frrRepairKey representativeRequest) runtime0)
      !commits =
        [ ExactFactorRepairCommit
            { efrcRequest = representativeRequest,
              efrcSubscriberCount = subscriberCount,
              efrcResult = representativeResult
            }
        ]
  pure (efrrRuntime representativeResult, commits)

memberQueryId ::
  FactorRepairMember ctx prop ->
  QueryId
memberQueryId =
  frrQueryId . frmRequest
