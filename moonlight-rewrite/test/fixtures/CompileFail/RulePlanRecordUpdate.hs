module RulePlanRecordUpdate where

import Moonlight.Rewrite.Runtime (RulePlan, rpId)

forgeRulePlan :: RulePlan guard f -> RulePlan guard f
forgeRulePlan rulePlan =
  rulePlan {rpId = rpId rulePlan}
