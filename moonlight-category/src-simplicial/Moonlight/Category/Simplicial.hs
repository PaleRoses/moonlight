{-| The public simplicial-category surface.

This facade exposes the definitional simplicial mathematics owned by the
@simplicial@ sublibrary without making downstream consumers name its
@Pure.Simplicial@ implementation paths.  The effectful property harness remains
in @moonlight-category:laws@.
-}
module Moonlight.Category.Simplicial
  ( module CategoricalSimplex,
    module Delta,
    module Homotopy,
    module Kan,
    module Nerve,
    module Ordinal,
    module Presheaf,
    module Set,
    module Spaces,
    module TypeLevel,
    module Validation,
  )
where

import Moonlight.Category.Pure.Simplicial.CategoricalSimplex as CategoricalSimplex
import Moonlight.Category.Pure.Simplicial.Delta as Delta
import Moonlight.Category.Pure.Simplicial.Homotopy as Homotopy
import Moonlight.Category.Pure.Simplicial.Kan as Kan
import Moonlight.Category.Pure.Simplicial.Nerve as Nerve
import Moonlight.Category.Pure.Simplicial.Ordinal as Ordinal
import Moonlight.Category.Pure.Simplicial.Presheaf as Presheaf
import Moonlight.Category.Pure.Simplicial.Set as Set
import Moonlight.Category.Pure.Simplicial.Spaces as Spaces
import Moonlight.Category.Pure.Simplicial.TypeLevel as TypeLevel
import Moonlight.Category.Pure.Simplicial.Validation as Validation
