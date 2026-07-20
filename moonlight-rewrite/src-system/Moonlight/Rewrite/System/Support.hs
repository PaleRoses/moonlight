{-# LANGUAGE GHC2024 #-}

-- | Rule-support index for base and named-context execution.
-- Owns the known-rule universe and its base/context support partitions.
-- Contracts: construction rejects support entries outside the known set, while
-- the convenience constructors make all supplied rules local to one stratum.
module Moonlight.Rewrite.System.Support
  ( RuleSupportIndex,
    RuleSupportIndexError (..),
    mkRuleSupportIndex,
    baseRuleSupportIndex,
    contextRuleSupportIndex,
    baseSupportRuleNames,
    contextSupportRuleNames,
    contextSupportEntries,
    baseSupportsRule,
    contextSupportsRule,
  )
where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )

type RuleSupportIndex :: Type -> Type
data RuleSupportIndex context = RuleSupportIndex
  { rsiKnownRules :: !(Set RuleName),
    rsiBaseSupport :: !(Set RuleName),
    rsiContextSupport :: !(Map context (Set RuleName))
  }
  deriving stock (Eq, Show)

type RuleSupportIndexError :: Type -> Type
data RuleSupportIndexError context
  = RuleSupportUnknownBaseRules !(Set RuleName)
  | RuleSupportUnknownContextRules !context !(Set RuleName)
  deriving stock (Eq, Show)

mkRuleSupportIndex ::
  Set RuleName ->
  Set RuleName ->
  Map context (Set RuleName) ->
  Either (RuleSupportIndexError context) (RuleSupportIndex context)
mkRuleSupportIndex knownRules baseSupport contextSupport = do
  validateBaseSupport knownRules baseSupport
  validateContextSupport knownRules (Map.toAscList contextSupport)
  Right
    RuleSupportIndex
      { rsiKnownRules = knownRules,
        rsiBaseSupport = baseSupport,
        rsiContextSupport = contextSupport
      }

baseRuleSupportIndex :: Set RuleName -> RuleSupportIndex context
baseRuleSupportIndex ruleNames =
  RuleSupportIndex
    { rsiKnownRules = ruleNames,
      rsiBaseSupport = ruleNames,
      rsiContextSupport = Map.empty
    }

contextRuleSupportIndex :: context -> Set RuleName -> RuleSupportIndex context
contextRuleSupportIndex contextName ruleNames =
  RuleSupportIndex
    { rsiKnownRules = ruleNames,
      rsiBaseSupport = Set.empty,
      rsiContextSupport = Map.singleton contextName ruleNames
    }

baseSupportRuleNames :: RuleSupportIndex context -> Set RuleName
baseSupportRuleNames =
  rsiBaseSupport

contextSupportRuleNames ::
  Ord context =>
  context ->
  RuleSupportIndex context ->
  Set RuleName
contextSupportRuleNames contextId index =
  Map.findWithDefault Set.empty contextId (rsiContextSupport index)

contextSupportEntries :: RuleSupportIndex context -> [(context, Set RuleName)]
contextSupportEntries =
  Map.toAscList . rsiContextSupport

baseSupportsRule :: RuleName -> RuleSupportIndex context -> Bool
baseSupportsRule ruleNameValue =
  Set.member ruleNameValue . rsiBaseSupport

contextSupportsRule ::
  Ord context =>
  context ->
  RuleName ->
  RuleSupportIndex context ->
  Bool
contextSupportsRule contextId ruleNameValue index =
  Set.member ruleNameValue (contextSupportRuleNames contextId index)

validateBaseSupport ::
  Set RuleName ->
  Set RuleName ->
  Either (RuleSupportIndexError context) ()
validateBaseSupport knownRules baseSupport =
  let unknown =
        Set.difference baseSupport knownRules
   in if Set.null unknown
        then Right ()
        else Left (RuleSupportUnknownBaseRules unknown)

validateContextSupport ::
  Set RuleName ->
  [(context, Set RuleName)] ->
  Either (RuleSupportIndexError context) ()
validateContextSupport knownRules =
  traverse_ validateContext
  where
    validateContext (contextId, supportedRules) =
      let unknown =
            Set.difference supportedRules knownRules
       in if Set.null unknown
            then Right ()
            else Left (RuleSupportUnknownContextRules contextId unknown)
