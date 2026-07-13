{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Twist.Program
  ( SupportExecutionProgram (..),
    supportExecutionProgram,
    supportExecutionProgramWithFacts,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedRuleBook,
  )

type SupportExecutionProgram :: Type -> Type -> Type -> Type -> Type
data SupportExecutionProgram context rewrite rawFact proof = SupportExecutionProgram
  { sepRuleBook :: !(SupportedRuleBook context rewrite),
    sepFacts :: !(SupportedFactBook context rawFact),
    sepProofCarrier :: !proof
  }

instance
  (Ord context, Semigroup proof) =>
  Semigroup (SupportExecutionProgram context rewrite rawFact proof)
  where
  leftProgram <> rightProgram =
    SupportExecutionProgram
      { sepRuleBook = sepRuleBook leftProgram <> sepRuleBook rightProgram,
        sepFacts = sepFacts leftProgram <> sepFacts rightProgram,
        sepProofCarrier = sepProofCarrier leftProgram <> sepProofCarrier rightProgram
      }

instance
  (Ord context, Monoid proof) =>
  Monoid (SupportExecutionProgram context rewrite rawFact proof)
  where
  mempty =
    SupportExecutionProgram
      { sepRuleBook = mempty,
        sepFacts = mempty,
        sepProofCarrier = mempty
      }

supportExecutionProgram ::
  Ord context =>
  SupportedRuleBook context rewrite ->
  proof ->
  SupportExecutionProgram context rewrite rawFact proof
supportExecutionProgram ruleBookValue =
  supportExecutionProgramWithFacts ruleBookValue mempty

supportExecutionProgramWithFacts ::
  SupportedRuleBook context rewrite ->
  SupportedFactBook context rawFact ->
  proof ->
  SupportExecutionProgram context rewrite rawFact proof
supportExecutionProgramWithFacts ruleBookValue factBookValue proofValue =
  SupportExecutionProgram
    { sepRuleBook = ruleBookValue,
      sepFacts = factBookValue,
      sepProofCarrier = proofValue
    }
