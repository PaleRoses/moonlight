module PatternRewriteRecordUpdate where

import Moonlight.Rewrite.Algebra (PatternRewrite, prOrigin)

forgePatternRewrite :: PatternRewrite atom dec f -> PatternRewrite atom dec f
forgePatternRewrite rewriteValue =
  rewriteValue {prOrigin = prOrigin rewriteValue}
