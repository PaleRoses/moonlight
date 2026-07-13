-- | Public facade for contextual rewrite proofs.
-- It is a pure re-export of the proof registry, contextual support, and
-- independent proof-boundary owners.
module Moonlight.Rewrite.ProofContext
  ( module Moonlight.Rewrite.ContextualSupport,
    module Moonlight.Rewrite.Proof,
    module Moonlight.Rewrite.Proof.Boundary,
  )
where

import Moonlight.Rewrite.ContextualSupport
import Moonlight.Rewrite.Proof
import Moonlight.Rewrite.Proof.Boundary
