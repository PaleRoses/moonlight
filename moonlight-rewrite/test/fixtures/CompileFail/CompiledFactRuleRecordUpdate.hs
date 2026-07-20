module CompiledFactRuleRecordUpdate where

import Moonlight.Rewrite.System (CompiledFactRule, cfrId)

forgeCompiledFactRule :: CompiledFactRule capability f -> CompiledFactRule capability f
forgeCompiledFactRule ruleValue =
  ruleValue {cfrId = cfrId ruleValue}
