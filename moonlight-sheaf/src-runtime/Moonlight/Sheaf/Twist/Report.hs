{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Twist.Report
  ( SupportTraceEntry (..),
    SupportSaturationReport (..),
  )
where

import Data.Kind (Type)

type SupportTraceEntry :: Type -> Type -> Type
data SupportTraceEntry support ruleId = SupportTraceEntry
  { steRound :: !Int,
    steRuleId :: !ruleId,
    steSupport :: !support,
    steMatchedCount :: !Int,
    steScheduledCount :: !Int,
    steSuppressedCount :: !Int,
    steSuppressedByCooldown :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type SupportSaturationReport :: Type -> Type -> Type -> Type -> Type
data SupportSaturationReport result guideTrace traceEntry host = SupportSaturationReport
  { ssrResult :: !result,
    ssrIterations :: !Int,
    ssrMatchesApplied :: !Int,
    ssrTrace :: ![traceEntry],
    ssrGuideTrace :: ![guideTrace],
    ssrProofGraph :: !host
  }
  deriving stock (Eq, Show)
