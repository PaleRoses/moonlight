module PBPORuleRecordUpdate where

import Moonlight.Rewrite.Algebra (PBPORule, pbpoRuleMeta)

forgePBPORule :: PBPORule category meta -> PBPORule category meta
forgePBPORule ruleValue =
  ruleValue {pbpoRuleMeta = pbpoRuleMeta ruleValue}
