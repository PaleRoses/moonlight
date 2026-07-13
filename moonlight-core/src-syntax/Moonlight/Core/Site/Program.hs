-- | Context-indexed rule programs, splitting rules into a base and per-context
-- refinements.
module Moonlight.Core.Site.Program
  ( SiteIndex (..),
    MatchActivationIndex (..),
    SupportIndexedRule (..),
    SiteProgram (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Prelude (Eq, Ord, Show)

type SiteIndex :: Type -> Type -> Type
data SiteIndex c rule = SiteIndex
  { siBase :: ![rule],
    siContexts :: !(Map c [rule])
  }

type MatchActivationIndex :: Type -> Type -> Type
data MatchActivationIndex c ruleId = MatchActivationIndex
  { maiBase :: !(Set ruleId),
    maiContexts :: !(Map c (Set ruleId))
  }

type SupportIndexedRule :: Type -> Type -> Type
data SupportIndexedRule support rule = SupportIndexedRule
  { sirSupport :: !support,
    sirRule :: !rule
  }
  deriving stock (Eq, Ord, Show)

type SiteProgram :: Type -> Type -> Type -> Type -> Type -> Type
data SiteProgram c rewriteRule factRule ruleId support = SiteProgram
  { spFactRules :: !(SiteIndex c factRule),
    spRewriteRules :: !(SiteIndex c rewriteRule),
    spSupportedFactRules :: ![SupportIndexedRule support factRule],
    spSupportedRewriteRules :: !(Map ruleId (SupportIndexedRule support rewriteRule)),
    spRewriteActivation :: !(MatchActivationIndex c ruleId),
    spBaseRewriteSupport :: !(Map ruleId support)
  }
