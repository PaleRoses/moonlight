-- | Per-rule rewrite traces and outcome\/transition statistics.
module Moonlight.Pale.Diagnostic.Section.Rewrite
  ( RuleTrace (..),
    RewriteOutcomeStat (..),
    RewriteTransitionStat (..),
  )
where

import Data.Kind (Type)
import Prelude (Bool, Eq, Int, Maybe, Show)

type RuleTrace :: Type -> Type
data RuleTrace ruleId = RuleTrace
  { rtRuleId :: ruleId,
    rtMatchedCount :: Int,
    rtFilteredCount :: Int,
    rtScheduledCount :: Int,
    rtSkippedByScheduler :: Bool,
    rtBannedUntil :: Maybe Int
  }
  deriving stock (Eq, Show)

type RewriteOutcomeStat :: Type -> Type
data RewriteOutcomeStat ruleId = RewriteOutcomeStat
  { rosRuleId :: ruleId,
    rosMatchedCount :: Int,
    rosFilteredCount :: Int,
    rosScheduledCount :: Int,
    rosBannedCount :: Int
  }
  deriving stock (Eq, Show)

type RewriteTransitionStat :: Type -> Type
data RewriteTransitionStat ruleId = RewriteTransitionStat
  { rtsFromRule :: ruleId,
    rtsToRule :: ruleId,
    rtsCount :: Int
  }
  deriving stock (Eq, Show)
