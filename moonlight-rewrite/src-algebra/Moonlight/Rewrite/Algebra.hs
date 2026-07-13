-- | Public facade for the algebra stratum.
-- This module owns no behavior; it exposes the deliberately supported
-- categorical, simplicial, PBPO, and kernel surfaces.
module Moonlight.Rewrite.Algebra
  ( module Moonlight.Rewrite.Algebra.Category.Core,
    module Moonlight.Rewrite.Algebra.Category.Finite,
    module Moonlight.Rewrite.Algebra.Nerve.Core,
    module Moonlight.Rewrite.Algebra.Nerve.Simplex,
    module Moonlight.Rewrite.Algebra.PBPO,
    module Moonlight.Rewrite.Algebra.Term.Category,
    module Moonlight.Rewrite.Kernel.Query,
    module Moonlight.Rewrite.Kernel.Condition,
    module Moonlight.Rewrite.Kernel.Compose,
    module Moonlight.Rewrite.Kernel.Rewrite,
    module Moonlight.Rewrite.Kernel.Decoration,
    module Moonlight.Rewrite.Kernel.Decoration.Product,
    module Moonlight.Rewrite.Kernel.SpanModel,
    module Moonlight.Rewrite.Kernel.Subst,
    module Moonlight.Rewrite.Kernel.Unify,
  )
where

import Moonlight.Rewrite.Algebra.Category.Core
import Moonlight.Rewrite.Algebra.Category.Finite
import Moonlight.Rewrite.Algebra.Nerve.Core
import Moonlight.Rewrite.Algebra.Nerve.Simplex
import Moonlight.Rewrite.Algebra.PBPO
import Moonlight.Rewrite.Algebra.Term.Category
import Moonlight.Rewrite.Kernel.Compose
import Moonlight.Rewrite.Kernel.Condition
import Moonlight.Rewrite.Kernel.Decoration
import Moonlight.Rewrite.Kernel.Decoration.Product
import Moonlight.Rewrite.Kernel.Query
import Moonlight.Rewrite.Kernel.Rewrite
import Moonlight.Rewrite.Kernel.SpanModel
import Moonlight.Rewrite.Kernel.Subst
import Moonlight.Rewrite.Kernel.Unify
