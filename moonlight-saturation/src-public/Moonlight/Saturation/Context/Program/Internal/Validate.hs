{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Internal.Validate
  ( validateProgram,
    validateSourceProgram,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (RewriteRuleId)
import Moonlight.Core (accumByKey)
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Saturation.Context.Error
  ( ProgramRelation (..),
    ProgramViolation (..),
    RuleKind (..),
    SaturationProgramSite (..),
    SaturationSupportError (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (..),
  )
import Moonlight.Saturation.Substrate

validateProgram ::
  forall u.
  (RewriteSystem u, FactSystem u) =>
  Program 'CompiledProgramStage u ->
  Either (SaturationSupportError u) ()
validateProgram compiledProgram =
  validateProgramViolations
    ( programViolationsWithIds
        (rewriteRuleId @u)
        (factRuleId @u)
        compiledProgram
    )
{-# INLINE validateProgram #-}

validateSourceProgram ::
  forall u.
  (RewriteSystem u, FactSystem u) =>
  Program 'SourceProgramStage u ->
  Either (SaturationSupportError u) ()
validateSourceProgram sourceProgram =
  validateProgramViolations
    ( programViolationsWithIds
        (rewriteRuleSourceId @u)
        (factSourceId @u)
        sourceProgram
    )
{-# INLINE validateSourceProgram #-}

validateProgramViolations ::
  [ProgramViolation (SatContext u)] ->
  Either (SaturationSupportError u) ()
validateProgramViolations violations =
  case NonEmpty.nonEmpty violations of
    Nothing ->
      Right ()
    Just nonEmptyViolations ->
      Left (SaturationSupportError nonEmptyViolations)
{-# INLINE validateProgramViolations #-}

programViolationsWithIds ::
  (rewrite -> RewriteRuleId) ->
  (fact -> RewriteRuleId) ->
  SiteProgram context rewrite fact RewriteRuleId support ->
  [ProgramViolation context]
programViolationsWithIds rewriteRuleIdOf factRuleIdOf siteProgram =
  let rewriteRules =
        spRewriteRules siteProgram
      factRules =
        spFactRules siteProgram
      activation =
        spRewriteActivation siteProgram
      baseRewriteScan =
        ruleIdOccurrences rewriteRuleIdOf (siBase rewriteRules)
      baseRewriteIds =
        Map.keysSet baseRewriteScan
      duplicateContextRewriteIds =
        duplicateRuleIdsByContext rewriteRuleIdOf (siContexts rewriteRules)
      baseFactScan =
        ruleIdOccurrences factRuleIdOf (siBase factRules)
      duplicateContextFactIds =
        duplicateRuleIdsByContext factRuleIdOf (siContexts factRules)
      unknownContextActivated =
        mapMaybeSet
          (`Set.difference` baseRewriteIds)
          (maiContexts activation)
      unknownBaseSupport =
        Set.difference
          (Map.keysSet (spBaseRewriteSupport siteProgram))
          baseRewriteIds
   in violationAt
        BaseProgramSite
        RewriteRuleKind
        DuplicateRuleId
        (duplicateRuleIds baseRewriteScan)
        <> contextViolations
          RewriteRuleKind
          DuplicateRuleId
          duplicateContextRewriteIds
        <> violationAt
          BaseProgramSite
          FactRuleKind
          DuplicateRuleId
          (duplicateRuleIds baseFactScan)
        <> contextViolations
          FactRuleKind
          DuplicateRuleId
          duplicateContextFactIds
        <> violationAt
          BaseProgramSite
          RewriteRuleKind
          UnknownActivatedRule
          (Set.difference (maiBase activation) baseRewriteIds)
        <> contextViolations
          RewriteRuleKind
          UnknownActivatedRule
          unknownContextActivated
        <> violationAt
          BaseProgramSite
          RewriteRuleKind
          UnknownSupportRule
          unknownBaseSupport
{-# INLINE programViolationsWithIds #-}

ruleIdOccurrences ::
  (value -> RewriteRuleId) ->
  [value] ->
  Map RewriteRuleId (Sum Int)
ruleIdOccurrences ruleIdOf =
  accumByKey ruleIdOf (const (Sum 1))

duplicateRuleIds ::
  Map RewriteRuleId (Sum Int) ->
  Set RewriteRuleId
duplicateRuleIds =
  Map.keysSet . Map.filter (> Sum 1)

duplicateRuleIdsByContext ::
  (value -> RewriteRuleId) ->
  Map context [value] ->
  Map context (Set RewriteRuleId)
duplicateRuleIdsByContext ruleIdOf =
  mapMaybeSet (duplicateRuleIds . ruleIdOccurrences ruleIdOf)

mapMaybeSet ::
  (value -> Set key) ->
  Map context value ->
  Map context (Set key)
mapMaybeSet transform =
  Map.filter (not . Set.null) . fmap transform

violationAt ::
  SaturationProgramSite context ->
  RuleKind ->
  ProgramRelation ->
  Set RewriteRuleId ->
  [ProgramViolation context]
violationAt site ruleKind relation ruleIds =
  [ ProgramViolation
      { pvSite = site,
        pvRuleKind = ruleKind,
        pvRelation = relation,
        pvRuleIds = ruleIds
      }
  | not (Set.null ruleIds)
  ]

contextViolations ::
  RuleKind ->
  ProgramRelation ->
  Map context (Set RewriteRuleId) ->
  [ProgramViolation context]
contextViolations ruleKind relation violationsByContext =
  [ ProgramViolation
      { pvSite = ContextProgramSite contextValue,
        pvRuleKind = ruleKind,
        pvRelation = relation,
        pvRuleIds = ruleIds
      }
  | (contextValue, ruleIds) <- Map.toAscList violationsByContext
  ]
