-- | Rewrite and transition summaries derived from a saturation trace.
module Moonlight.Pale.Diagnostic.Derived.Rewrite
  ( RewriteOutcomeSummary (..),
    RewriteTransitionSummary (..),
    summarizeSaturationTrace,
    summarizeRewriteTransitions,
  )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Moonlight.Pale.Diagnostic.Section.Rewrite
  ( RewriteOutcomeStat (..),
    RewriteTransitionStat (..),
    RuleTrace (..),
  )
import Moonlight.Pale.Diagnostic.Section.Saturation
  ( SaturationIterationTrace (..),
    SaturationTrace (..),
  )
import Prelude
  ( Eq ((==)),
    Int,
    Ord,
    Show,
    drop,
    filter,
    fmap,
    foldr,
    length,
    zip,
    (+),
    (.),
    (>),
    (>>=),
  )

type RewriteOutcomeSummary :: Type -> Type
data RewriteOutcomeSummary ruleId = RewriteOutcomeSummary
  { rosIterations :: Int,
    rosTotalMatched :: Int,
    rosTotalFiltered :: Int,
    rosTotalScheduled :: Int,
    rosRuleStats :: [RewriteOutcomeStat ruleId]
  }
  deriving stock (Eq, Show)

type RewriteTransitionSummary :: Type -> Type
data RewriteTransitionSummary ruleId = RewriteTransitionSummary
  { rtrsTotalTransitions :: Int,
    rtrsTransitions :: [RewriteTransitionStat ruleId],
    rtrsSelfCycles :: [RewriteTransitionStat ruleId]
  }
  deriving stock (Eq, Show)

summarizeSaturationTrace :: Ord ruleId => SaturationTrace ruleId -> RewriteOutcomeSummary ruleId
summarizeSaturationTrace saturationTrace =
  let ruleStats =
        sortOn
          (Down . rosScheduledCount)
          (Map.elems (foldr accumulateRuleTrace Map.empty (stIterations saturationTrace >>= sitRuleTraces)))
      totalMatched = foldr ((+) . rosMatchedCount) 0 ruleStats
      totalFiltered = foldr ((+) . rosFilteredCount) 0 ruleStats
      totalScheduled = foldr ((+) . rosScheduledCount) 0 ruleStats
   in RewriteOutcomeSummary
        { rosIterations = length (stIterations saturationTrace),
          rosTotalMatched = totalMatched,
          rosTotalFiltered = totalFiltered,
          rosTotalScheduled = totalScheduled,
          rosRuleStats = ruleStats
        }

summarizeRewriteTransitions :: Ord ruleId => SaturationTrace ruleId -> RewriteTransitionSummary ruleId
summarizeRewriteTransitions saturationTrace =
  let scheduledRules =
        stIterations saturationTrace
          >>= fmap rtRuleId
            . filter ((> 0) . rtScheduledCount)
            . sitRuleTraces
      transitionPairs =
        case scheduledRules of
          [] -> []
          [ruleId] -> [(ruleId, ruleId)]
          _ -> zip scheduledRules (drop 1 scheduledRules)
      transitionCounts =
        foldr
          (\(fromRule, toRule) -> Map.insertWith (+) (fromRule, toRule) 1)
          Map.empty
          transitionPairs
      transitions =
        sortOn
          (Down . rtsCount)
          ( fmap
              (\((fromRule, toRule), transitionCount) ->
                 RewriteTransitionStat
                   { rtsFromRule = fromRule,
                     rtsToRule = toRule,
                     rtsCount = transitionCount
                   }
              )
              (Map.toList transitionCounts)
          )
      selfCycles =
        filter (\transition -> rtsFromRule transition == rtsToRule transition) transitions
   in RewriteTransitionSummary
        { rtrsTotalTransitions = foldr ((+) . rtsCount) 0 transitions,
          rtrsTransitions = transitions,
          rtrsSelfCycles = selfCycles
        }

accumulateRuleTrace :: Ord ruleId => RuleTrace ruleId -> Map.Map ruleId (RewriteOutcomeStat ruleId) -> Map.Map ruleId (RewriteOutcomeStat ruleId)
accumulateRuleTrace ruleTrace =
  Map.insertWith combineStats (rtRuleId ruleTrace) (statFromTrace ruleTrace)

combineStats :: RewriteOutcomeStat ruleId -> RewriteOutcomeStat ruleId -> RewriteOutcomeStat ruleId
combineStats leftStat rightStat =
  RewriteOutcomeStat
    { rosRuleId = rosRuleId leftStat,
      rosMatchedCount = rosMatchedCount leftStat + rosMatchedCount rightStat,
      rosFilteredCount = rosFilteredCount leftStat + rosFilteredCount rightStat,
      rosScheduledCount = rosScheduledCount leftStat + rosScheduledCount rightStat,
      rosBannedCount = rosBannedCount leftStat + rosBannedCount rightStat
    }

statFromTrace :: RuleTrace ruleId -> RewriteOutcomeStat ruleId
statFromTrace ruleTrace =
  RewriteOutcomeStat
    { rosRuleId = rtRuleId ruleTrace,
      rosMatchedCount = rtMatchedCount ruleTrace,
      rosFilteredCount = rtFilteredCount ruleTrace,
      rosScheduledCount = rtScheduledCount ruleTrace,
      rosBannedCount = if rtSkippedByScheduler ruleTrace then 1 else 0
    }
