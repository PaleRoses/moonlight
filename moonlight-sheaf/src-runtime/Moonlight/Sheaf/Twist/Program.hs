{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
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

type SupportExecutionProgram :: Type -> Type -> Type -> Type -> Type -> Type
data SupportExecutionProgram owner context rewrite rawFact proof = SupportExecutionProgram
  { sepRuleBook :: !(SupportedRuleBook owner context rewrite),
    sepFacts :: !(SupportedFactBook owner context rawFact),
    sepProofCarrier :: !proof
  }

type role SupportExecutionProgram nominal nominal representational representational representational

instance
  Semigroup proof =>
  Semigroup (SupportExecutionProgram owner context rewrite rawFact proof)
  where
  leftProgram <> rightProgram =
    SupportExecutionProgram
      { sepRuleBook = sepRuleBook leftProgram <> sepRuleBook rightProgram,
        sepFacts = sepFacts leftProgram <> sepFacts rightProgram,
        sepProofCarrier = sepProofCarrier leftProgram <> sepProofCarrier rightProgram
      }

instance
  Monoid proof =>
  Monoid (SupportExecutionProgram owner context rewrite rawFact proof)
  where
  mempty =
    SupportExecutionProgram
      { sepRuleBook = mempty,
        sepFacts = mempty,
        sepProofCarrier = mempty
      }

supportExecutionProgram ::
  SupportedRuleBook owner context rewrite ->
  proof ->
  SupportExecutionProgram owner context rewrite rawFact proof
supportExecutionProgram ruleBookValue =
  supportExecutionProgramWithFacts ruleBookValue mempty

supportExecutionProgramWithFacts ::
  SupportedRuleBook owner context rewrite ->
  SupportedFactBook owner context rawFact ->
  proof ->
  SupportExecutionProgram owner context rewrite rawFact proof
supportExecutionProgramWithFacts ruleBookValue factBookValue proofValue =
  SupportExecutionProgram
    { sepRuleBook = ruleBookValue,
      sepFacts = factBookValue,
      sepProofCarrier = proofValue
    }
