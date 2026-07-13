-- | Public facade for the rewrite runtime stratum.
--
-- The runtime derives executable plans from the algebraic rewrite model.  It
-- owns no term anti-unification or automata substrate; those kernels belong to
-- @moonlight-core@.
module Moonlight.Rewrite.Runtime
  ( module Moonlight.Rewrite.Runtime.Capabilities,
    module Moonlight.Rewrite.Runtime.Condition,
    module Moonlight.Rewrite.Runtime.Exec,
    module Moonlight.Rewrite.Runtime.PostMatch,
    module Moonlight.Rewrite.Runtime.Rhs,
    module Moonlight.Rewrite.Runtime.RulePlan,
  )
where

import Moonlight.Rewrite.Runtime.Capabilities
import Moonlight.Rewrite.Runtime.Condition
import Moonlight.Rewrite.Runtime.Exec
import Moonlight.Rewrite.Runtime.PostMatch
import Moonlight.Rewrite.Runtime.Rhs
import Moonlight.Rewrite.Runtime.RulePlan
