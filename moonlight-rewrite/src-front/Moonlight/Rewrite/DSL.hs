-- | Public facade for typed rewrite authoring and compilation.
--
-- This module owns no lowering behavior.  It curates the DSL declarations,
-- elaboration result types, compiler entry points, and signature derivation
-- supplied by their defining modules.
module Moonlight.Rewrite.DSL
  ( module Moonlight.Rewrite.DSL.Compile,
    module Moonlight.Rewrite.DSL.Error,
    module Moonlight.Rewrite.DSL.Program,
    module Moonlight.Rewrite.DSL.Rule,
    module Moonlight.Rewrite.DSL.Signature,
    module Moonlight.Rewrite.DSL.Term,
    module Moonlight.Rewrite.Signature.TH,
  )
where

import Moonlight.Rewrite.DSL.Compile
import Moonlight.Rewrite.DSL.Error
import Moonlight.Rewrite.DSL.Program
import Moonlight.Rewrite.DSL.Rule
import Moonlight.Rewrite.DSL.Signature
import Moonlight.Rewrite.DSL.Term
import Moonlight.Rewrite.Signature.TH
