module CheckedRawRewriteRuleRecordUpdate where

import Moonlight.Rewrite.System (CheckedRawRewriteRule, chrId)

forgeCheckedRawRewriteRule :: CheckedRawRewriteRule guard f -> CheckedRawRewriteRule guard f
forgeCheckedRawRewriteRule ruleValue =
  ruleValue {chrId = chrId ruleValue}
