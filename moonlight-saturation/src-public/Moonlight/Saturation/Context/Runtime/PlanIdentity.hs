{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.PlanIdentity
  ( compiledContextQueries,
    runtimePlanIdentity,
    stampRuntimePlanIdentity,
    ensureRuntimeResumeCompatible,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
    SupportIndexedRule (..),
  )
import Moonlight.Saturation.Context.Error
  ( RuntimeResumeError (..),
    SaturationRunError (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    Program,
    ProgramStage (CompiledProgramStage),
    planMatchingStrategy,
    planProgram,
    planSchedulerConfig,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore (..),
    RuntimePlanIdentity (..),
    RuntimePlanFingerprint (..),
    RuntimeRuleIdentity (..),
    RuntimeState (..),
  )
import Moonlight.Saturation.Substrate

compiledContextQueries ::
  forall u.
  RebuildSystem u =>
  Program 'CompiledProgramStage u ->
  [SatQuery u]
compiledContextQueries siteProgram =
  foldMap
    (fmap (factRuleQuery @u))
    (siContexts (spFactRules siteProgram))
    <> fmap
      (factRuleQuery @u . sirRule)
      (spSupportedFactRules siteProgram)
    <> foldMap
      (fmap (rewriteRuleQuery @u))
      (siContexts (spRewriteRules siteProgram))
    <> fmap
      (rewriteRuleQuery @u . sirRule)
      (Map.elems (spSupportedRewriteRules siteProgram))
{-# INLINE compiledContextQueries #-}

runtimePlanIdentity ::
  forall u carrier schedulerGroup.
  RebuildSystem u =>
  Plan u carrier schedulerGroup ->
  Either (SatObstruction u) (RuntimePlanIdentity u schedulerGroup)
runtimePlanIdentity plan =
  RuntimePlanIdentity
    <$> runtimePlanFingerprint @u plan
{-# INLINE runtimePlanIdentity #-}

runtimePlanFingerprint ::
  forall u carrier schedulerGroup.
  RebuildSystem u =>
  Plan u carrier schedulerGroup ->
  Either (SatObstruction u) (RuntimePlanFingerprint u schedulerGroup)
runtimePlanFingerprint plan =
  let siteProgram =
        planProgram plan
   in RuntimePlanFingerprint
        (planMatchingStrategy plan)
        (planSchedulerConfig plan)
        <$> traverse (compiledFactRuleIdentity @u) (siBase (spFactRules siteProgram))
        <*> traverseSiteIndex (compiledFactRuleIdentity @u) (siContexts (spFactRules siteProgram))
        <*> traverse (identifySupportIndexedRule (compiledFactRuleIdentity @u)) (spSupportedFactRules siteProgram)
        <*> traverse (compiledRewriteRuleIdentity @u) (siBase (spRewriteRules siteProgram))
        <*> traverseSiteIndex (compiledRewriteRuleIdentity @u) (siContexts (spRewriteRules siteProgram))
        <*> traverse (identifySupportIndexedRule (compiledRewriteRuleIdentity @u)) (spSupportedRewriteRules siteProgram)
        <*> pure (spRewriteActivation siteProgram)
        <*> pure (spBaseRewriteSupport siteProgram)
{-# INLINE runtimePlanFingerprint #-}

compiledFactRuleIdentity ::
  forall u.
  FactSystem u =>
  SatFactRule u ->
  Either (SatObstruction u) (RuntimeRuleIdentity (SatFactRuleIdentity u))
compiledFactRuleIdentity rule =
  RuntimeRuleIdentity (factRuleId @u rule)
    <$> queryFingerprint @u (factRuleQuery @u rule)
    <*> factRuleIdentity @u rule
{-# INLINE compiledFactRuleIdentity #-}

compiledRewriteRuleIdentity ::
  forall u.
  RewriteSystem u =>
  SatRule u ->
  Either (SatObstruction u) (RuntimeRuleIdentity (SatRewriteRuleIdentity u))
compiledRewriteRuleIdentity rule =
  RuntimeRuleIdentity (rewriteRuleId @u rule)
    <$> queryFingerprint @u (rewriteRuleQuery @u rule)
    <*> rewriteRuleIdentity @u rule
{-# INLINE compiledRewriteRuleIdentity #-}

traverseSiteIndex ::
  Applicative f =>
  (rule -> f identity) ->
  Map.Map context [rule] ->
  f (Map.Map context [identity])
traverseSiteIndex identify =
  traverse (traverse identify)
{-# INLINE traverseSiteIndex #-}

identifySupportIndexedRule ::
  Applicative f =>
  (rule -> f identity) ->
  SupportIndexedRule support rule ->
  f (SupportIndexedRule support identity)
identifySupportIndexedRule identify indexedRule =
  (\identity -> indexedRule {sirRule = identity})
    <$> identify (sirRule indexedRule)
{-# INLINE identifySupportIndexedRule #-}

stampRuntimePlanIdentity ::
  RuntimePlanIdentity u schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
stampRuntimePlanIdentity planIdentity state =
  state
    { rsCore =
        (rsCore state)
          { rcPlanIdentity = Just planIdentity
          }
    }
{-# INLINE stampRuntimePlanIdentity #-}

ensureRuntimeResumeCompatible ::
  forall u carrier schedulerGroup.
  ( RebuildSystem u,
    Eq (SatMatchStrategy u),
    Eq (SatContext u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq schedulerGroup
  ) =>
  Plan u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  Either (SaturationRunError u) ()
ensureRuntimeResumeCompatible plan state = do
  expectedIdentity <-
    first SaturationRunSectionObstructed $
      runtimePlanIdentity @u plan

  case rcPlanIdentity (rsCore state) of
    Nothing ->
      Left
        ( SaturationRunResumeIncompatible
            RuntimeResumeMissingPlanIdentity
        )

    Just actualIdentity
      | not (sameRuntimePlanIdentity actualIdentity expectedIdentity) ->
          Left
            ( SaturationRunResumeIncompatible
                RuntimeResumePlanChanged
            )

      | otherwise ->
          Right ()
{-# INLINE ensureRuntimeResumeCompatible #-}

sameRuntimePlanIdentity ::
  ( Eq (SatMatchStrategy u),
    Eq (SatContext u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq schedulerGroup
  ) =>
  RuntimePlanIdentity u schedulerGroup ->
  RuntimePlanIdentity u schedulerGroup ->
  Bool
sameRuntimePlanIdentity leftIdentity rightIdentity =
  sameRuntimePlanFingerprint
    (rpiPlanFingerprint leftIdentity)
    (rpiPlanFingerprint rightIdentity)
{-# INLINE sameRuntimePlanIdentity #-}

sameRuntimePlanFingerprint ::
  ( Eq (SatMatchStrategy u),
    Eq (SatContext u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq schedulerGroup
  ) =>
  RuntimePlanFingerprint u schedulerGroup ->
  RuntimePlanFingerprint u schedulerGroup ->
  Bool
sameRuntimePlanFingerprint leftFingerprint rightFingerprint =
  rpfMatchingStrategy leftFingerprint == rpfMatchingStrategy rightFingerprint
    && rpfSchedulerConfig leftFingerprint == rpfSchedulerConfig rightFingerprint
    && rpfBaseFactRules leftFingerprint == rpfBaseFactRules rightFingerprint
    && rpfContextFactRules leftFingerprint == rpfContextFactRules rightFingerprint
    && rpfSupportedFactRules leftFingerprint == rpfSupportedFactRules rightFingerprint
    && rpfBaseRewriteRules leftFingerprint == rpfBaseRewriteRules rightFingerprint
    && rpfContextRewriteRules leftFingerprint == rpfContextRewriteRules rightFingerprint
    && rpfSupportedRewriteRules leftFingerprint == rpfSupportedRewriteRules rightFingerprint
    && sameMatchActivationIndex
      (rpfRewriteActivation leftFingerprint)
      (rpfRewriteActivation rightFingerprint)
    && rpfBaseRewriteSupport leftFingerprint == rpfBaseRewriteSupport rightFingerprint
{-# INLINE sameRuntimePlanFingerprint #-}

sameMatchActivationIndex ::
  (Eq context, Eq ruleId) =>
  MatchActivationIndex context ruleId ->
  MatchActivationIndex context ruleId ->
  Bool
sameMatchActivationIndex leftIndex rightIndex =
  maiBase leftIndex == maiBase rightIndex
    && maiContexts leftIndex == maiContexts rightIndex
{-# INLINE sameMatchActivationIndex #-}
