{-# LANGUAGE GHC2024 #-}

-- | Front-stratum compilation entry point from DSL programs to rule plans.
-- Owns the orchestration that elaborates a source program into a
-- 'CanonicalProgram' and derives a 'RulePlanSet'.
-- Contracts: source errors are preserved as 'ProgramError', and plans are
-- derived from the checked system rather than authored independently.
module Moonlight.Rewrite.DSL.Compile
  ( CanonicalProgram,
    canonicalSourceProgram,
    canonicalCheckedSystem,
    canonicalSupportIndex,
    compileProgramRuleSet,
  )
where

import Data.Bifunctor (first)
import Moonlight.Core (ZipMatch)
import Moonlight.Rewrite.DSL.Elaborate
  ( CanonicalProgram,
    canonicalCheckedSystem,
    canonicalSourceProgram,
    canonicalSupportIndex,
    elaborateProgram,
  )
import Moonlight.Rewrite.DSL.Error
  ( ProgramError (..),
  )
import Moonlight.Rewrite.DSL.Program
  ( Program,
  )
import Moonlight.Rewrite.DSL.Rule
  ( RewriteGuardAtom (..),
  )
import Moonlight.Rewrite.DSL.Signature
  ( Node,
    NodeTag,
    RewriteSignature,
  )
import Moonlight.Rewrite.System
  ( RulePlanSet,
    planRuleSet,
  )

compileProgramRuleSet ::
  (RewriteSignature sig, ZipMatch (Node sig), RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  Program sig atom ->
  Either (ProgramError sig) (CanonicalProgram sig atom, RulePlanSet (GuardCapabilityKey atom) (Node sig))
compileProgramRuleSet sourceProgram = do
  canonicalProgram <-
    elaborateProgram sourceProgram

  rulePlanSet <-
    first ProgramRulePlanError
      (planRuleSet (canonicalCheckedSystem canonicalProgram))

  pure (canonicalProgram, rulePlanSet)
