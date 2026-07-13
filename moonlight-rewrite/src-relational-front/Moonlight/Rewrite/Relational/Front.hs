{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | High-level relational rewrite engine for compiled DSL programs.
-- Owns compile/prepare/match/apply/saturate orchestration plus base/context
-- host state and application-condition caches.
-- Contracts: the host is canonical, matches carry host revisions, stale
-- applies revalidate by canonical evidence, and conditions recheck pre-apply.
module Moonlight.Rewrite.Relational.Front
  ( Host,
    ClassId,
    classIdKey,
    HostTerm (..),
    HostProgramResult (..),
    HostRebuildResult (..),
    rebuildHostBarrier,
    emptyHost,
    hostFromTerm,
    hostFromTerms,
    hostFromNodes,
    hostFromNodeClasses,
    hostCanonicalClass,
    hostClassCount,
    hostNodeClasses,
    hostClassWitness,
    hostLookupTermClass,
    hostRevision,
    hostSections,
    hostSectionsFromClasses,
    runHostRewriteProgram,

    MatchVar,
    matchVarOrdinal,
    matchVarName,
    matchVarSort,
    RewriteTarget (..),
    MatchQuery (..),
    Match,
    matchTarget,
    matchRule,
    matchRoot,
    matchBindings,
    matchRevision,
    matchSubstitution,

    Rules,
    Engine,
    compile,
    prepare,
    engineHost,
    replaceHost,
    setContext,
    removeContext,

    ApplyConfig (..),
    defaultApplyConfig,
    ApplyRejection (..),
    ApplyStatus (..),
    ApplyResult (..),
    apply,

    SaturationConfig (..),
    defaultSaturationConfig,
    SaturationRound (..),
    SaturationResult (..),
    match,
    saturate,

    Cost (..),
    ExtractRoundLimit (..),
    ExtractConfig (..),
    defaultExtractConfig,
    Extracted,
    extractedTerm,
    extractedClass,
    extractedCost,
    SomeExtracted (..),
    ExtractError (..),
    extract,
    extractWith,
    extractSome,
    extractSomeWith,

    HostBuildError (..),
    prettyHostBuildError,
    RelationalSaturationPlanError (..),
    RelationalSaturationObstruction (..),
    relationalSaturationResumeError,
    RelationalProgramError (..),
    prettyRelationalProgramError,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Proxy
  ( Proxy (..),
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    Pattern (..),
    PatternVar,
    ZipMatch,
    classIdKey,
    patternVarKey,
  )
import Moonlight.EGraph.Pure.Delta
  ( eGraphEditDeltaNull,
  )
import Moonlight.Core
  ( DenseKey (..),
  )
import Moonlight.Core.EGraph.Program (eGraphProgramChanged)
import Moonlight.Flow.Plan.Compile.Atomize
  ( PatternAtomizeHost (..),
  )
import Moonlight.Rewrite.Algebra
  ( cpqCondition,
    cpqQuery,
    compiledPatternQueryVariablesWith,
    patternQueryPatterns,
  )
import Moonlight.Rewrite.Runtime
  ( aceDecision,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
    compileExecutableRewriteMatch,
  )
import Moonlight.Rewrite.Runtime (RulePlan (..))
import Moonlight.Rewrite.DSL
  ( CanonicalProgram,
    canonicalRuleVariables,
    canonicalSupportIndex,
    compileProgramRuleSet,
    ruleVariableMap,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
    Program,
  )
import Moonlight.Rewrite.DSL
  ( RewriteGuardAtom (..),
  )
import Moonlight.Rewrite.DSL
  ( Node (..),
    NodeTag,
    RewriteSignature (..),
  )
import Moonlight.Rewrite.DSL
  ( someTypedVarName,
    someTypedVarSort,
  )
import Moonlight.Rewrite.Relational
  ( compileRelationalRulePlan,
  )
import Moonlight.Rewrite.Relational.Front.Error
  ( HostBuildError (..),
    RelationalProgramError (..),
    RelationalSaturationObstruction (..),
    RelationalSaturationPlanError (..),
    prettyHostBuildError,
    prettyRelationalProgramError,
    relationalSaturationResumeError,
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    HostProgramResult (..),
    HostRebuildResult (..),
    HostTerm (..),
    emptyHost,
    hostBackend,
    hostCanonicalClass,
    hostClassCount,
    hostClassWitness,
    hostFromNodeClasses,
    hostFromNodes,
    hostFromTerm,
    hostFromTerms,
    hostLookupTermClass,
    hostNodeClasses,
    hostRevision,
    hostSections,
    hostSectionsFromClasses,
    rebuildHostBarrier,
    runHostRewriteProgram,
  )
import Moonlight.Rewrite.Relational.Front.Extraction
  ( Cost (..),
    ExtractConfig (..),
    ExtractError (..),
    ExtractRoundLimit (..),
    Extracted,
    SomeExtracted (..),
    defaultExtractConfig,
    extract,
    extractSome,
    extractSomeWith,
    extractWith,
    extractedClass,
    extractedCost,
    extractedTerm,
  )
import Moonlight.Rewrite.Relational.Front.Internal.GuardDigest
  ( compiledGuardCanonicalWords,
    patternNode,
  )
import Moonlight.Rewrite.Relational.Front.ApplicationCondition
  ( RelationalApplicationConditionCache,
    compileRelationalApplicationConditionPlansForRule,
    emptyRelationalApplicationConditionCache,
    runRelationalApplicationConditionEvaluatorCached,
  )
import Moonlight.Rewrite.Relational.Front.Saturation
  ( saturateBase,
    saturateContext,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Types
  ( ApplyConfig (..),
    ApplyRejection (..),
    ApplyResult (..),
    ApplyStatus (..),
    Engine (..),
    Match,
    MatchQuery (..),
    MatchVar (..),
    PreparedCache,
    RawMatch,
    RelationalCompiledRule (..),
    RewriteTarget (..),
    Rules (..),
    SaturationConfig (..),
    SaturationResult (..),
    SaturationRound (..),
    defaultApplyConfig,
    defaultSaturationConfig,
    matchBindings,
    matchRoot,
    matchRule,
    matchRevision,
    matchSubstitution,
    matchTarget,
    matchVarName,
    matchVarOrdinal,
    matchVarSort,
    rulesRelationalPlanSet,
    tagRawMatch,
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunConfig (..),
    RewriteRunError (..),
    RewriteRunResult (..),
    RewriteRestriction (..),
    advanceRelationalSystemHost,
    defaultRewriteRunConfig,
    prepareRelationalSystem,
    runMatchRule,
    runMatchRuleWithContextHost,
  )
import Moonlight.Rewrite.System
  ( RulePlanSet,
    rulePlanNames,
    rulePlans,
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    compiledGuardVariables,
  )

compile ::
  (RewriteSignature sig, ZipMatch (Node sig), RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  Program sig atom ->
  Either (RelationalProgramError sig) (Rules sig atom)
compile sourceProgram = do
  (canonicalProgramValue, rulePlanSet) <-
    first RelationalProgramSourceError
      (compileProgramRuleSet sourceProgram)

  relationalRules <-
    compileRelationalRules canonicalProgramValue rulePlanSet

  Right
    Rules
      { rulesCanonicalProgram = canonicalProgramValue,
        rulesRelationalRules = relationalRules
      }

prepare ::
  Rules sig atom ->
  Host sig ->
  Engine sig atom
prepare rulesValue host =
  Engine
    { engRules = rulesValue,
      engHost = host,
      engPrepared = prepareCache host rulesValue,
      engContexts = Map.empty,
      engApplicationConditionCaches = Map.empty
    }

prepareCache ::
  Host sig ->
  Rules sig atom ->
  PreparedCache sig atom
prepareCache host rulesValue =
  prepareRelationalSystem
    (hostBackend host)
    (canonicalSupportIndex (rulesCanonicalProgram rulesValue))
    (rulesRelationalPlanSet rulesValue)

engineHost :: Engine sig atom -> Host sig
engineHost =
  engHost

replaceHost ::
  Host sig ->
  Engine sig atom ->
  Engine sig atom
replaceHost host =
  clearConditionCache RewriteBase . replaceTargetHost RewriteBase host

setContext ::
  ContextName ->
  Host sig ->
  Engine sig atom ->
  Engine sig atom
setContext contextNameValue host =
  clearConditionCache target . replaceTargetHost target host
  where
    target =
      RewriteContext contextNameValue

removeContext ::
  ContextName ->
  Engine sig atom ->
  Engine sig atom
removeContext contextNameValue engineValue =
  engineValue
    { engContexts = Map.delete contextNameValue (engContexts engineValue),
      engApplicationConditionCaches =
        Map.delete
          (RewriteContext contextNameValue)
          (engApplicationConditionCaches engineValue)
    }

match ::
  Ord (NodeTag sig) =>
  MatchQuery ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, [Match])
match query engineValue =
  case matchQueryTarget query of
    RewriteBase ->
      matchBase query engineValue

    RewriteContext contextNameValue ->
      matchContext contextNameValue query engineValue

matchBase ::
  forall sig atom.
  Ord (NodeTag sig) =>
  MatchQuery ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, [Match])
matchBase query engineValue = do
  (prepared', rawMatches) <-
    runPreparedMatch @sig @atom (runConfigForQuery query) (matchQueryRule query) (engPrepared engineValue)

  Right
    ( engineValue {engPrepared = prepared'},
      tagMatchesForQuery query (hostRevision (engHost engineValue)) rawMatches
    )

matchContext ::
  Ord (NodeTag sig) =>
  ContextName ->
  MatchQuery ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, [Match])
matchContext contextNameValue query engineValue = do
  contextHost <-
    requireContextHost contextNameValue engineValue

  (prepared', rawMatches) <-
    first RelationalProgramRunError
      ( runMatchRuleWithContextHost
          (runConfigForQuery query)
          contextNameValue
          (hostBackend contextHost)
          (matchQueryRule query)
          (engPrepared engineValue)
      )

  Right
    ( engineValue {engPrepared = prepared'},
      tagMatchesForQuery query (hostRevision contextHost) (rrrValue rawMatches)
    )

tagMatchesForQuery :: MatchQuery -> Int -> [RawMatch] -> [Match]
tagMatchesForQuery query revision =
  fmap (tagRawMatch (matchQueryTarget query) (matchQueryRule query) revision)

runPreparedMatch ::
  forall sig atom.
  Ord (NodeTag sig) =>
  RewriteRunConfig ContextName () ->
  RuleName ->
  PreparedCache sig atom ->
  Either (RelationalProgramError sig) (PreparedCache sig atom, [RawMatch])
runPreparedMatch config ruleNameValue prepared =
  fmap
    (\(prepared', result) -> (prepared', rrrValue result))
    (first RelationalProgramRunError (runMatchRule config ruleNameValue prepared))

runConfigForQuery :: MatchQuery -> RewriteRunConfig ContextName ()
runConfigForQuery query =
  case matchQueryRoot query of
    Nothing ->
      defaultRewriteRunConfig

    Just rootClass ->
      defaultRewriteRunConfig
        { rrcRestriction =
            RewriteRootFrontier (IntSet.singleton (encodeDenseKey rootClass))
        }

apply ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  ApplyConfig sig ->
  Match ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, ApplyResult)
apply config matchValue engineValue = do
  (engineAfterValidation, validation) <-
    validateMatchForApply matchValue engineValue

  case validation of
    Left rejectedStatus ->
      Right
        ( engineAfterValidation,
          ApplyResult
            { applyResultTarget = matchTarget matchValue,
              applyResultRule = matchRule matchValue,
              applyResultRoot = matchRoot matchValue,
              applyResultStatus = rejectedStatus
            }
        )

    Right validatedMatch ->
      applyValidated config validatedMatch engineAfterValidation

validateMatchForApply ::
  Ord (NodeTag sig) =>
  Match ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, Either ApplyStatus Match)
validateMatchForApply matchValue engineValue = do
  host <-
    requireTargetHost (matchTarget matchValue) engineValue

  let currentRevision =
        hostRevision host

      staleStatus =
        ApplyRejected (RejectedStaleMatch (matchRevision matchValue) currentRevision)

  if matchRevision matchValue == currentRevision
    then Right (engineValue, Right matchValue)
    else
      case canonicalizeMatchForHost host matchValue of
        Nothing ->
          Right (engineValue, Left staleStatus)

        Just canonicalMatch -> do
          (engine', currentMatches) <-
            match
              MatchQuery
                { matchQueryTarget = matchTarget canonicalMatch,
                  matchQueryRule = matchRule canonicalMatch,
                  matchQueryRoot = Just (matchRoot canonicalMatch)
                }
              engineValue

          Right
            ( engine',
              if any (sameMatchEvidence canonicalMatch) currentMatches
                then Right canonicalMatch {matchRevision = currentRevision}
                else Left staleStatus
            )

canonicalizeMatchForHost ::
  Host sig ->
  Match ->
  Maybe Match
canonicalizeMatchForHost host matchValue = do
  canonicalRoot <-
    hostCanonicalClass host (matchRoot matchValue)

  canonicalBindings <-
    traverse (hostCanonicalClass host) (matchBindings matchValue)

  Just
    matchValue
      { matchRoot = canonicalRoot,
        matchBindings = canonicalBindings
      }

sameMatchEvidence :: Match -> Match -> Bool
sameMatchEvidence leftMatch rightMatch =
  matchTarget leftMatch == matchTarget rightMatch
    && matchRule leftMatch == matchRule rightMatch
    && matchRoot leftMatch == matchRoot rightMatch
    && matchBindings leftMatch == matchBindings rightMatch

applyValidated ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  ApplyConfig sig ->
  Match ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, ApplyResult)
applyValidated config matchValue engineValue = do
  host <-
    requireTargetHost (matchTarget matchValue) engineValue

  relationalRule <-
    requireRelationalRule (matchRule matchValue) (engRules engineValue)

  let executableMatch =
        ExecutableRewriteMatch
          { ermRule = rcrRulePlan relationalRule,
            ermRootClass = matchRoot matchValue,
            ermGuardEvidence = Nothing :: Maybe (),
            ermGuideEvidence = Nothing :: Maybe (),
            ermSubstitution = matchSubstitution matchValue
          }
      target =
        matchTarget matchValue
      initialCache =
        conditionCacheFor target engineValue

  (conditionCache', conditionAccepted) <-
    recheckApplicationCondition
      initialCache
      host
      relationalRule
      executableMatch

  let engineWithConditionCache =
        setConditionCache target conditionCache' engineValue

  if not conditionAccepted
    then
      Right
        ( engineWithConditionCache,
          ApplyResult
            { applyResultTarget = target,
              applyResultRule = matchRule matchValue,
              applyResultRoot = matchRoot matchValue,
              applyResultStatus = ApplyRejected RejectedApplicationCondition
            }
        )
    else do
      rewriteProgram <-
        first RelationalProgramRewriteApplicationError
          ( compileExecutableRewriteMatch
              (acResolveBindingPattern config)
              (acBinderSubstAlgebra config)
              executableMatch
          )

      HostProgramResult hostAfterProgram executedRewrite applicationEffect programDelta programDirtyResults <-
        first RelationalProgramRewriteApplicationError
          (runHostRewriteProgram rewriteProgram host)

      (host', dirtyResults) <-
        if eGraphEditDeltaNull programDelta
          then Right (hostAfterProgram, programDirtyResults)
          else do
            HostRebuildResult rebuiltHost _rebuildDelta rebuildDirtyResults <-
              first
                RelationalProgramRewriteApplicationError
                (rebuildHostBarrier hostAfterProgram)
            Right (rebuiltHost, programDirtyResults <> rebuildDirtyResults)

      let changed =
            eGraphProgramChanged applicationEffect
          engine' =
            if changed
              then commitTargetHost target host' dirtyResults engineWithConditionCache
              else advanceTargetHost target host' dirtyResults engineWithConditionCache

      Right
        ( engine',
          ApplyResult
            { applyResultTarget = target,
              applyResultRule = matchRule matchValue,
              applyResultRoot = matchRoot matchValue,
              applyResultStatus = ApplyExecuted executedRewrite changed
            }
        )

saturate ::
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RewriteTarget ->
  SaturationConfig sig ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Engine sig atom, SaturationResult sig)
saturate target config engineValue =
  case target of
    RewriteBase -> do
      result <-
        saturateBase
          config
          (engRules engineValue)
          (engHost engineValue)

      Right
        ( clearConditionCache RewriteBase (replaceTargetHost RewriteBase (saturationHost result) engineValue),
          result
        )

    RewriteContext contextNameValue -> do
      contextHost <-
        requireContextHost contextNameValue engineValue

      result <-
        saturateContext
          config
          contextNameValue
          (engRules engineValue)
          (engHost engineValue)
          contextHost

      let contextTarget =
            RewriteContext contextNameValue

      Right
        ( clearConditionCache contextTarget (replaceTargetHost contextTarget (saturationHost result) engineValue),
          result
        )

requireRelationalRule ::
  RuleName ->
  Rules sig atom ->
  Either (RelationalProgramError sig) (RelationalCompiledRule sig atom)
requireRelationalRule ruleNameValue rulesValue =
  case Map.lookup ruleNameValue (rulesRelationalRules rulesValue) of
    Just relationalRule ->
      Right relationalRule

    Nothing ->
      Left (RelationalProgramRunError (RewriteRuleNotFound ruleNameValue))

requireContextHost ::
  ContextName ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Host sig)
requireContextHost contextNameValue engineValue =
  case Map.lookup contextNameValue (engContexts engineValue) of
    Just host ->
      Right host

    Nothing ->
      Left (RelationalProgramContextMissing contextNameValue)

requireTargetHost ::
  RewriteTarget ->
  Engine sig atom ->
  Either (RelationalProgramError sig) (Host sig)
requireTargetHost target engineValue =
  case target of
    RewriteBase ->
      Right (engHost engineValue)

    RewriteContext contextNameValue ->
      requireContextHost contextNameValue engineValue

replaceTargetHost ::
  RewriteTarget ->
  Host sig ->
  Engine sig atom ->
  Engine sig atom
replaceTargetHost target host engineValue =
  case target of
    RewriteBase ->
      engineValue
        { engHost = host,
          engPrepared = prepareCache host (engRules engineValue)
        }

    RewriteContext contextNameValue ->
      engineValue
        { engContexts =
            Map.insert contextNameValue host (engContexts engineValue)
        }

advanceTargetHost ::
  Ord (NodeTag sig) =>
  RewriteTarget ->
  Host sig ->
  IntSet.IntSet ->
  Engine sig atom ->
  Engine sig atom
advanceTargetHost target host dirtyResults engineValue =
  case target of
    RewriteBase ->
      engineValue
        { engHost = host,
          engPrepared =
            advanceRelationalSystemHost
              (hostBackend host)
              dirtyResults
              (engPrepared engineValue)
        }

    RewriteContext contextNameValue ->
      engineValue
        { engContexts =
            Map.insert contextNameValue host (engContexts engineValue)
        }

commitTargetHost ::
  Ord (NodeTag sig) =>
  RewriteTarget ->
  Host sig ->
  IntSet.IntSet ->
  Engine sig atom ->
  Engine sig atom
commitTargetHost target host dirtyResults =
  clearConditionCache target
    . advanceTargetHost target host dirtyResults

conditionCacheFor ::
  RewriteTarget ->
  Engine sig atom ->
  RelationalApplicationConditionCache (GuardCapabilityKey atom) sig
conditionCacheFor target engineValue =
  Map.findWithDefault
    emptyRelationalApplicationConditionCache
    target
    (engApplicationConditionCaches engineValue)

setConditionCache ::
  RewriteTarget ->
  RelationalApplicationConditionCache (GuardCapabilityKey atom) sig ->
  Engine sig atom ->
  Engine sig atom
setConditionCache target cache engineValue =
  engineValue
    { engApplicationConditionCaches =
        Map.insert target cache (engApplicationConditionCaches engineValue)
    }

clearConditionCache ::
  RewriteTarget ->
  Engine sig atom ->
  Engine sig atom
clearConditionCache target engineValue =
  engineValue
    { engApplicationConditionCaches =
        Map.delete target (engApplicationConditionCaches engineValue)
    }

recheckApplicationCondition ::
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RelationalApplicationConditionCache (GuardCapabilityKey atom) sig ->
  Host sig ->
  RelationalCompiledRule sig atom ->
  ExecutableRewriteMatch (CompiledGuard (GuardCapabilityKey atom) (Node sig)) () () (Node sig) ->
  Either (RelationalProgramError sig) (RelationalApplicationConditionCache (GuardCapabilityKey atom) sig, Bool)
recheckApplicationCondition conditionCache host relationalRule executableMatch =
  case rpApplicationCondition (ermRule executableMatch) of
    Nothing ->
      Right (conditionCache, True)

    Just applicationCondition -> do
      (conditionCache', evidence) <-
        runRelationalApplicationConditionEvaluatorCached
          (rcrApplicationConditionPlans relationalRule)
          conditionCache
          host
          (ermRootClass executableMatch)
          (ermSubstitution executableMatch)
          applicationCondition

      Right (conditionCache', aceDecision evidence)

compileRelationalRules ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  CanonicalProgram sig atom ->
  RulePlanSet (GuardCapabilityKey atom) (Node sig) ->
  Either
    (RelationalProgramError sig)
    (Map RuleName (RelationalCompiledRule sig atom))
compileRelationalRules canonicalProgramValue rulePlanSet =
  Map.fromList
    <$> traverse
      ( \(ruleNameValue, rulePlan) ->
          fmap
            (\compiledRule -> (ruleNameValue, compiledRule))
            (compileRelationalRule canonicalProgramValue ruleNameValue rulePlan)
      )
      (zip (rulePlanNames rulePlanSet) (rulePlans rulePlanSet))

compileRelationalRule ::
  forall sig atom.
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  CanonicalProgram sig atom ->
  RuleName ->
  RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig) ->
  Either
    (RelationalProgramError sig)
    (RelationalCompiledRule sig atom)
compileRelationalRule canonicalProgramValue ruleNameValue rulePlan = do
  matchVariables <-
    matchVariablesForRule canonicalProgramValue ruleNameValue rulePlan
  matchPlan <-
    first RelationalProgramCompileError
      ( compileRelationalRulePlan
          (canonicalPatternAtomizeHost @sig @atom matchVariables)
          ruleNameValue
          rulePlan
      )
  applicationConditionPlans <-
    compileRelationalApplicationConditionPlansForRule
      (guardCapabilityDigest (Proxy @atom))
      rulePlan
  Right
    RelationalCompiledRule
      { rcrRulePlan = rulePlan,
        rcrMatchPlan = matchPlan,
        rcrApplicationConditionPlans = applicationConditionPlans
      }

canonicalPatternAtomizeHost ::
  forall sig atom.
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (NodeTag sig)) =>
  Map PatternVar MatchVar ->
  PatternAtomizeHost
    (RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig))
    (Pattern (Node sig))
    MatchVar
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    (NodeTag sig)
    (Node sig ClassId)
    ClassId
    RawMatch
canonicalPatternAtomizeHost matchVariables =
  PatternAtomizeHost
    { pahQueryPatterns = patternQueryPatterns . cpqQuery . rpQuery,
      pahQueryResidualGuard = cpqCondition . rpQuery,
      pahResidualWords = compiledGuardCanonicalWords (guardCapabilityDigest (Proxy @atom)),
      pahPatternVar = matchPatternVariable matchVariables,
      pahPatternNode = patternNode,
      pahPatternVarKey = matchVarOrdinal,
      pahTagDigest = nodeTagDigest (Proxy @sig)
    }

matchPatternVariable ::
  Map PatternVar MatchVar ->
  Pattern (Node sig) ->
  Maybe MatchVar
matchPatternVariable matchVariables =
  \case
    PatternVar patternVariable ->
      Map.lookup patternVariable matchVariables

    PatternNode _ ->
      Nothing

matchVariablesForRule ::
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  CanonicalProgram sig atom ->
  RuleName ->
  RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig) ->
  Either (RelationalProgramError sig) (Map PatternVar MatchVar)
matchVariablesForRule canonicalProgramValue ruleNameValue rulePlan =
  case Map.lookup ruleNameValue (canonicalRuleVariables canonicalProgramValue) of
    Nothing
      | Set.null queryVariables ->
          Right Map.empty

      | otherwise ->
          Left (RelationalProgramMatchVariablesMissing ruleNameValue (Set.toAscList queryVariables))

    Just ruleVariables ->
      let typedVariables =
            ruleVariableMap ruleVariables
          missingVariables =
            Set.filter (`Map.notMember` typedVariables) queryVariables
       in if Set.null missingVariables
            then
              Right
                ( Map.restrictKeys
                    (Map.mapWithKey matchVarFromTypedVariable typedVariables)
                    queryVariables
                )
            else
              Left (RelationalProgramMatchVariablesMissing ruleNameValue (Set.toAscList missingVariables))
  where
    queryVariables =
      compiledPatternQueryVariablesWith compiledGuardVariables (rpQuery rulePlan)

    matchVarFromTypedVariable patternVariable typedVariable =
      MatchVar
        (patternVarKey patternVariable)
        (someTypedVarName typedVariable)
        (Just (someTypedVarSort typedVariable))
