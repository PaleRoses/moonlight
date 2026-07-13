-- | Public RHS-instantiation surface for rule plans.
-- It owns the hidden split between static templates and post-match RHS patterns,
-- exposing only the canonical spec constructor consumed by execution.
module Moonlight.Rewrite.Runtime.Rhs
  ( RhsInstantiationSpec,
    rhsInstantiationSpec,
  )
where

import Moonlight.Rewrite.Runtime.Rhs.Internal
  ( RhsInstantiationSpec,
    rhsInstantiationSpec,
  )
