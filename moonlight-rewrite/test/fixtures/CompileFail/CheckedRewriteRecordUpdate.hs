module CheckedRewriteRecordUpdate where

import Moonlight.Rewrite.System (CheckedRewrite, checkedRewriteId)

forgeCheckedRewrite :: CheckedRewrite capability f -> CheckedRewrite capability f
forgeCheckedRewrite rewriteValue =
  rewriteValue {checkedRewriteId = checkedRewriteId rewriteValue}
