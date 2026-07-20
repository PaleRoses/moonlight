{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Rewrite.Relational.Front.ApplicationCondition
  ( ApplicationConditionCompiledExtension,
    ApplicationConditionRelationalPlan,
    RelationalApplicationConditionPlans (..),
    emptyRelationalApplicationConditionPlans,
    compileRelationalApplicationConditionPlans,
    compileRelationalApplicationConditionPlansForRule,
    RelationalApplicationConditionCache,
    emptyRelationalApplicationConditionCache,
    recheckRelationalApplicationCondition,
    runRelationalApplicationConditionEvaluatorCached,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core
  ( ClassId,
    DenseKey (..),
    Pattern,
    PatternVar,
    Substitution,
    lookupSubst,
    patternVarKey,
    slotIdKey,
  )
import Moonlight.Flow.Plan.Compile.Atomize
  ( PatternAtomizeHost (..),
  )
import Moonlight.Rewrite.Algebra
  ( CompiledApplicationCondition,
    CompiledPatternExtension,
    PatternExtensionScope (..),
    compiledApplicationConditionExtensions,
    compiledPatternQueryVariablesWith,
    cpqCondition,
    cpqQuery,
    cpeAnchorVars,
    cpePath,
    cpeQuery,
    cpeScope,
    patternQueryPatterns,
  )
import Moonlight.Rewrite.Runtime
  ( ApplicationConditionAnchor (..),
    ApplicationConditionEvidence,
    ExecutableRewriteMatch (..),
    RewriteApplicationError (..),
    RulePlan,
    aceDecision,
    evaluateCompiledApplicationConditionWithState,
    rpApplicationCondition,
  )
import Moonlight.Flow.Plan.Query.Core
  ( qpOutputRecipe,
    qpOutputSlots,
    qpRootSlot,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
    Node (..),
    NodeTag,
    RewriteSignature (..),
  )
import Moonlight.Rewrite.Relational
  ( RelationalPlanSet (..),
    RelationalPreparedSystem,
    RelationalRewriteMatch (..),
    RewritePreparedOp (..),
    RewriteRestriction (..),
    RewriteRunConfig (..),
    RewriteRunResult (..),
    RewritePlan,
    compileRelationalRulePlan,
    defaultRewriteRunConfig,
    prepareRelationalSystem,
    relationalRewriteMatchOutputVars,
    runRewrite,
  )
import Moonlight.Rewrite.Relational.Front.Error
  ( RelationalProgramError (..),
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    hostBackend,
    hostRevision,
  )
import Moonlight.Rewrite.Relational.Front.Internal.GuardDigest
  ( compiledGuardCanonicalWords,
    patternNode,
    patternVar,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RuleName,
    RuleSupportIndex,
    baseRuleSupportIndex,
    compiledGuardVariables,
    mkRuleName,
  )

newtype ApplicationConditionMatchVar = ApplicationConditionMatchVar
  { applicationConditionMatchVarOrdinal :: Int
  }
  deriving stock (Eq, Ord)

type ApplicationConditionMatch =
  RelationalRewriteMatch ApplicationConditionMatchVar ClassId

type ApplicationConditionCompiledExtension capability sig =
  CompiledPatternExtension (CompiledGuard capability (Node sig)) (Node sig)

type ApplicationConditionRelationalPlan capability sig =
  RewritePlan
    (ApplicationConditionCompiledExtension capability sig)
    ApplicationConditionMatchVar
    ClassId
    (CompiledGuard capability (Node sig))
    (NodeTag sig)
    (Node sig ClassId)

type ApplicationConditionPrepared capability sig projection =
  RelationalPreparedSystem
    ContextName
    projection
    (ApplicationConditionCompiledExtension capability sig)
    ApplicationConditionMatchVar
    ClassId
    (CompiledGuard capability (Node sig))
    (NodeTag sig)
    (Node sig ClassId)

data ApplicationConditionDependencyBinding = ApplicationConditionDependencyBinding
  { acdbVariable :: !PatternVar,
    acdbClass :: !(Maybe ClassId)
  }
  deriving stock (Eq, Ord)

data RelationalApplicationConditionCacheKey capability sig = RelationalApplicationConditionCacheKey
  { raccRoot :: !(Maybe ClassId),
    raccDependencyBindings :: ![ApplicationConditionDependencyBinding],
    raccExtension :: !(ApplicationConditionCompiledExtension capability sig)
  }

deriving stock instance
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  Eq (RelationalApplicationConditionCacheKey capability sig)

deriving stock instance
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  Ord (RelationalApplicationConditionCacheKey capability sig)

data RelationalApplicationConditionCache capability sig = RelationalApplicationConditionCache
  { racRevision :: {-# UNPACK #-} !Int,
    racTruthValues :: !(Map (RelationalApplicationConditionCacheKey capability sig) Bool),
    racPreparedExtensions :: !(Map (ApplicationConditionCompiledExtension capability sig) (ApplicationConditionPrepared capability sig ()))
  }

emptyRelationalApplicationConditionCache :: RelationalApplicationConditionCache capability sig
emptyRelationalApplicationConditionCache =
  RelationalApplicationConditionCache
    { racRevision = minBound,
      racTruthValues = Map.empty,
      racPreparedExtensions = Map.empty
    }

newtype RelationalApplicationConditionPlans capability sig = RelationalApplicationConditionPlans
  { racpPlans :: Map (ApplicationConditionCompiledExtension capability sig) (ApplicationConditionRelationalPlan capability sig)
  }

emptyRelationalApplicationConditionPlans :: RelationalApplicationConditionPlans capability sig
emptyRelationalApplicationConditionPlans =
  RelationalApplicationConditionPlans Map.empty

compileRelationalApplicationConditionPlansForRule ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  (capability -> Word64) ->
  RulePlan (CompiledGuard capability (Node sig)) (Node sig) ->
  Either (RelationalProgramError sig) (RelationalApplicationConditionPlans capability sig)
compileRelationalApplicationConditionPlansForRule capabilityDigest rulePlan =
  maybe
    (Right emptyRelationalApplicationConditionPlans)
    (compileRelationalApplicationConditionPlans capabilityDigest)
    (rpApplicationCondition rulePlan)

compileRelationalApplicationConditionPlans ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  (capability -> Word64) ->
  CompiledApplicationCondition (CompiledGuard capability (Node sig)) (Node sig) ->
  Either (RelationalProgramError sig) (RelationalApplicationConditionPlans capability sig)
compileRelationalApplicationConditionPlans capabilityDigest condition = do
  extensionRuleName <-
    applicationConditionExtensionRuleName

  plans <-
    traverse
      ( \extension ->
          fmap
            ((,) extension)
            (compileApplicationConditionExtensionPlan capabilityDigest extensionRuleName extension)
      )
      (compiledApplicationConditionExtensions condition)

  Right (RelationalApplicationConditionPlans (Map.fromList plans))

recheckRelationalApplicationCondition ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RelationalApplicationConditionPlans capability sig ->
  RelationalApplicationConditionCache capability sig ->
  Host sig ->
  ExecutableRewriteMatch
    (CompiledGuard capability (Node sig))
    guardEvidence
    guideEvidence
    (Node sig) ->
  Either
    (RelationalProgramError sig)
    (RelationalApplicationConditionCache capability sig, Bool)
recheckRelationalApplicationCondition plans conditionCache host executableMatch =
  case rpApplicationCondition (ermRule executableMatch) of
    Nothing ->
      Right (conditionCache, True)

    Just applicationCondition -> do
      (conditionCache', evidence) <-
        runRelationalApplicationConditionEvaluatorCached
          plans
          conditionCache
          host
          (ermRootClass executableMatch)
          (ermSubstitution executableMatch)
          applicationCondition

      Right (conditionCache', aceDecision evidence)

runRelationalApplicationConditionEvaluatorCached ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RelationalApplicationConditionPlans capability sig ->
  RelationalApplicationConditionCache capability sig ->
  Host sig ->
  ClassId ->
  Substitution ->
  CompiledApplicationCondition (CompiledGuard capability (Node sig)) (Node sig) ->
  Either
    (RelationalProgramError sig)
    ( RelationalApplicationConditionCache capability sig,
      ApplicationConditionEvidence ClassId Substitution
    )
runRelationalApplicationConditionEvaluatorCached plans initialCache host rootClass substitution condition = do
  extensionRuleName <-
    applicationConditionExtensionRuleName

  let hostRevisionValue =
        hostRevision host
      cache =
        if racRevision initialCache == hostRevisionValue
          then initialCache
          else emptyRelationalApplicationConditionCache {racRevision = hostRevisionValue}

  evaluateCompiledApplicationConditionWithState
    cache
    hostRevisionValue
    ApplicationConditionAnchor
      { acaRoot = rootClass,
        acaSubstitution = substitution
      }
    (applicationConditionExtensionExistsCached plans host extensionRuleName)
    condition

applicationConditionExtensionExistsCached ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RelationalApplicationConditionPlans capability sig ->
  Host sig ->
  RuleName ->
  RelationalApplicationConditionCache capability sig ->
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  Either (RelationalProgramError sig) (RelationalApplicationConditionCache capability sig, Bool)
applicationConditionExtensionExistsCached plans host extensionRuleName cache anchor extension =
  case Map.lookup cacheKey (racTruthValues cache) of
    Just cachedDecision ->
      Right (cache, cachedDecision)

    Nothing -> do
      extensionPlan <-
        applicationConditionPlanFor plans extension

      let preparedExtension =
            Map.findWithDefault
              (prepareApplicationConditionExtension host extensionRuleName extensionPlan)
              extension
              (racPreparedExtensions cache)

      (preparedExtension', decision) <-
        applicationConditionExtensionExistsWithPlan
          anchor
          extensionRuleName
          extensionPlan
          preparedExtension
          extension

      Right
        ( cache
            { racTruthValues =
                Map.insert cacheKey decision (racTruthValues cache),
              racPreparedExtensions =
                Map.insert extension preparedExtension' (racPreparedExtensions cache)
            },
          decision
        )
  where
    cacheKey =
      applicationConditionCacheKey anchor extension

applicationConditionCacheKey ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  RelationalApplicationConditionCacheKey capability sig
applicationConditionCacheKey anchor extension =
  RelationalApplicationConditionCacheKey
    { raccRoot = applicationConditionCacheRoot anchor extension,
      raccDependencyBindings = applicationConditionDependencyBindings anchor extension,
      raccExtension = extension
    }

applicationConditionCacheRoot ::
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  Maybe ClassId
applicationConditionCacheRoot anchor extension =
  case cpeScope extension of
    ExtensionRoot ->
      Just (acaRoot anchor)

    ExtensionLocal ->
      Nothing

    ExtensionGlobal ->
      Nothing

applicationConditionDependencyBindings ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  [ApplicationConditionDependencyBinding]
applicationConditionDependencyBindings anchor extension =
  fmap
    ( \patternVariable ->
        ApplicationConditionDependencyBinding
          { acdbVariable = patternVariable,
            acdbClass = lookupSubst patternVariable (acaSubstitution anchor)
          }
    )
    ( Set.toAscList
        (compiledPatternQueryVariablesWith compiledGuardVariables (cpeQuery extension))
    )

applicationConditionPlanFor ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RelationalApplicationConditionPlans capability sig ->
  ApplicationConditionCompiledExtension capability sig ->
  Either (RelationalProgramError sig) (ApplicationConditionRelationalPlan capability sig)
applicationConditionPlanFor (RelationalApplicationConditionPlans plans) extension =
  case Map.lookup extension plans of
    Just extensionPlan ->
      Right extensionPlan

    Nothing ->
      Left
        (RelationalProgramApplicationConditionPlanMissing (cpePath extension))

applicationConditionExtensionExistsWithPlan ::
  Ord (NodeTag sig) =>
  ApplicationConditionAnchor ClassId Substitution ->
  RuleName ->
  ApplicationConditionRelationalPlan capability sig ->
  ApplicationConditionPrepared capability sig () ->
  ApplicationConditionCompiledExtension capability sig ->
  Either
    (RelationalProgramError sig)
    (ApplicationConditionPrepared capability sig (), Bool)
applicationConditionExtensionExistsWithPlan anchor extensionRuleName extensionPlan preparedExtension extension = do
  anchorBindings <-
    first RelationalProgramRewriteApplicationError
      (applicationConditionAnchorBindings anchor extension)

  restriction <-
    applicationConditionExtensionRestriction
      extensionPlan
      anchorBindings
      anchor
      extension

  let runConfig =
        defaultRewriteRunConfig
          { rrcRestriction = restriction
          }

  (preparedExtension', result) <-
    first RelationalProgramRunError
      ( runRewrite
          runConfig
          (ExistsRule extensionRuleName)
          preparedExtension
      )

  pure (preparedExtension', rrrValue result)

applicationConditionExtensionRestriction ::
  ApplicationConditionRelationalPlan capability sig ->
  [(PatternVar, ClassId)] ->
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  Either (RelationalProgramError sig) RewriteRestriction
applicationConditionExtensionRestriction extensionPlan anchorBindings anchor extension = do
  anchorSlotBindings <-
    traverse
      (applicationConditionAnchorSlotBinding extensionPlan extension)
      anchorBindings

  let slotBindings =
        applicationConditionRootSlotBinding extensionPlan anchor extension
          <> anchorSlotBindings

      slotValues =
        IntMap.fromListWith IntSet.intersection slotBindings

  pure
    ( if IntMap.null slotValues
        then RewriteUnrestricted
        else RewriteSlots slotValues
    )

applicationConditionRootSlotBinding ::
  ApplicationConditionRelationalPlan capability sig ->
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  [(Int, IntSet.IntSet)]
applicationConditionRootSlotBinding extensionPlan anchor extension =
  case cpeScope extension of
    ExtensionRoot ->
      [ ( slotIdKey (qpRootSlot extensionPlan),
          IntSet.singleton (encodeDenseKey (acaRoot anchor))
        )
      ]

    ExtensionLocal ->
      []

    ExtensionGlobal ->
      []

applicationConditionAnchorSlotBinding ::
  ApplicationConditionRelationalPlan capability sig ->
  ApplicationConditionCompiledExtension capability sig ->
  (PatternVar, ClassId) ->
  Either (RelationalProgramError sig) (Int, IntSet.IntSet)
applicationConditionAnchorSlotBinding extensionPlan extension (patternVariable, expectedClass) =
  case Map.lookup (applicationConditionMatchVar patternVariable) (applicationConditionPlanOutputSlots extensionPlan) of
    Nothing ->
      Left
        ( RelationalProgramApplicationConditionAnchorSlotMissing
            (cpePath extension)
            patternVariable
        )

    Just slotKey ->
      Right
        ( slotKey,
          IntSet.singleton (encodeDenseKey expectedClass)
        )

applicationConditionPlanOutputSlots ::
  ApplicationConditionRelationalPlan capability sig ->
  Map ApplicationConditionMatchVar Int
applicationConditionPlanOutputSlots extensionPlan =
  Map.fromList
    ( zip
        (relationalRewriteMatchOutputVars (qpOutputRecipe extensionPlan))
        (fmap slotIdKey (Vector.toList (qpOutputSlots extensionPlan)))
    )

applicationConditionExtensionRuleNameText :: String
applicationConditionExtensionRuleNameText =
  "application-condition-extension"

applicationConditionExtensionRuleName ::
  Either (RelationalProgramError sig) RuleName
applicationConditionExtensionRuleName =
  first
    (RelationalProgramRuleNameError applicationConditionExtensionRuleNameText)
    (mkRuleName applicationConditionExtensionRuleNameText)

compileApplicationConditionExtensionPlan ::
  forall sig capability.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  (capability -> Word64) ->
  RuleName ->
  ApplicationConditionCompiledExtension capability sig ->
  Either (RelationalProgramError sig) (ApplicationConditionRelationalPlan capability sig)
compileApplicationConditionExtensionPlan capabilityDigest ruleNameValue extension =
  first
    RelationalProgramCompileError
    ( compileRelationalRulePlan
        (applicationConditionPatternAtomizeHost @sig capabilityDigest)
        ruleNameValue
        extension
    )

prepareApplicationConditionExtension ::
  Host sig ->
  RuleName ->
  ApplicationConditionRelationalPlan capability sig ->
  ApplicationConditionPrepared capability sig projection
prepareApplicationConditionExtension host ruleNameValue extensionPlan =
  prepareRelationalSystem
    (hostBackend host)
    (applicationConditionExtensionSupportIndex ruleNameValue)
    (RelationalPlanSet (Map.singleton ruleNameValue extensionPlan))

applicationConditionExtensionSupportIndex ::
  RuleName ->
  RuleSupportIndex ContextName
applicationConditionExtensionSupportIndex ruleNameValue =
  baseRuleSupportIndex (Set.singleton ruleNameValue)

applicationConditionAnchorBindings ::
  ApplicationConditionAnchor ClassId Substitution ->
  ApplicationConditionCompiledExtension capability sig ->
  Either RewriteApplicationError [(PatternVar, ClassId)]
applicationConditionAnchorBindings anchor extension =
  traverse
    (applicationConditionAnchorBinding (acaSubstitution anchor))
    (Set.toAscList (cpeAnchorVars extension))

applicationConditionAnchorBinding ::
  Substitution ->
  PatternVar ->
  Either RewriteApplicationError (PatternVar, ClassId)
applicationConditionAnchorBinding substitution patternVariable =
  maybe
    (Left (RewriteMissingBinding patternVariable))
    (\classId -> Right (patternVariable, classId))
    (lookupSubst patternVariable substitution)

applicationConditionPatternAtomizeHost ::
  forall sig capability.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  (capability -> Word64) ->
  PatternAtomizeHost
    (ApplicationConditionCompiledExtension capability sig)
    (Pattern (Node sig))
    ApplicationConditionMatchVar
    (CompiledGuard capability (Node sig))
    (NodeTag sig)
    (Node sig ClassId)
    ClassId
    ApplicationConditionMatch
applicationConditionPatternAtomizeHost capabilityDigest =
  PatternAtomizeHost
    { pahQueryPatterns = patternQueryPatterns . cpqQuery . cpeQuery,
      pahQueryResidualGuard = cpqCondition . cpeQuery,
      pahResidualWords = compiledGuardCanonicalWords capabilityDigest,
      pahPatternVar = patternVar applicationConditionMatchVar,
      pahPatternNode = patternNode,
      pahPatternVarKey = applicationConditionMatchVarOrdinal,
      pahTagDigest = nodeTagDigest (Proxy @sig)
    }

applicationConditionMatchVar ::
  PatternVar ->
  ApplicationConditionMatchVar
applicationConditionMatchVar =
  ApplicationConditionMatchVar . patternVarKey
