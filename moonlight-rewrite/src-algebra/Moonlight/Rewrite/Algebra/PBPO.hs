-- | Public PBPO algebra surface.
-- It owns no independent semantics; it glues the rule, step, and derivation
-- strata without hiding their separate well-formedness and adhesive-step duties.
module Moonlight.Rewrite.Algebra.PBPO
  ( module Moonlight.Rewrite.Algebra.PBPO.Rule,
    module Moonlight.Rewrite.Algebra.PBPO.Step,
    module Moonlight.Rewrite.Algebra.PBPO.Derive,
  )
where

import Moonlight.Rewrite.Algebra.PBPO.Derive
import Moonlight.Rewrite.Algebra.PBPO.Rule
import Moonlight.Rewrite.Algebra.PBPO.Step
