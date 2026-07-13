{-# LANGUAGE GHC2024 #-}

-- | Rewrite-origin attribution for checked and composed system rules.
-- Owns the 'RuleOrigin' atom and the rendering/folding of kernel
-- 'RewriteOrigin' trees back to rule ids and names.
-- Contract: composite origins preserve every atomic rule id, while identity
-- remains the neutral rendered origin.
module Moonlight.Rewrite.System.Origin
  ( RuleOrigin (..),
    ruleOriginId,
    ruleOriginName,
    rewriteOriginRuleIds,
    renderRewriteOrigin,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( RewriteRuleId,
  )
import Moonlight.Rewrite.Algebra
  ( RewriteOrigin (..),
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
    ruleNameString,
  )

data RuleOrigin = RuleOrigin
  { roRuleId :: !RewriteRuleId,
    roRuleName :: !RuleName
  }
  deriving stock (Eq, Ord, Show)

ruleOriginId :: RuleOrigin -> RewriteRuleId
ruleOriginId =
  roRuleId

ruleOriginName :: RuleOrigin -> RuleName
ruleOriginName =
  roRuleName

rewriteOriginRuleIds :: RewriteOrigin RuleOrigin -> Set RewriteRuleId
rewriteOriginRuleIds =
  foldMap (Set.singleton . roRuleId)

renderRewriteOrigin :: RewriteOrigin RuleOrigin -> String
renderRewriteOrigin =
  go
  where
    go RewriteIdentity =
      "id"

    go (RewriteAtomic origin) =
      ruleNameString (roRuleName origin)

    go (RewriteComposite leftOrigin rightOrigin) =
      go leftOrigin <> ";" <> go rightOrigin
