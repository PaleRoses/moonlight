{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Runtime.Exec.Internal
  ( InstantiationRef (..),
    InstantiationInput (..),
    BinderScopedStep (..),
    InstantiationStep (..),
    InstantiationPlan (..),
    ExecutableRewriteMatch (..),
    ExecutedRewrite (..),
    buildPatternInstantiationPlan,
    buildRewriteRhsPlan,
    lowerBinderScopedPlan,
    compileInstantiationPlan,
    compileRewriteRhs,
    compileExecutableRewriteMatch,
    executableRewriteMatchRuleKey,
  )
where

import Data.Bifunctor (first)
import Control.Monad.Trans.State.Strict (StateT (..), get, modify', runStateT)
import Data.Foldable (foldlM, toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (ClassId, Pattern (..), PatternVar, RewriteRuleId, patternVarKey)
import Moonlight.Core
  ( Substitution,
    lookupSubst,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgram,
    abortProgram,
    addCanonicalNode,
    canonicalizeClass,
    mergeCanonicalClasses,
    patternVariables,
  )
import Moonlight.Rewrite.Runtime.RulePlan
  ( RulePlan (..),
    RewriteApplicationError (..)
  )
import Moonlight.Rewrite.Runtime.PostMatch
  ( BinderSubstAlgebra,
    PostMatchSubst,
    applyPostMatchSubst,
    postMatchSubstVariables,
  )
import Moonlight.Rewrite.Runtime.Rhs.Internal
  ( RhsInstantiationSpec (..),
    RhsStaticPlan,
    RhsTemplate (..),
    RhsTemplateInput (..),
    RhsTemplateRef (..),
    RhsTemplateStep (..),
    rhsStaticPlanTemplate,
  )

type InstantiationRef :: Type
newtype InstantiationRef = InstantiationRef
  { instantiationRefKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type InstantiationInput :: Type
data InstantiationInput
  = ExistingClass !ClassId
  | PriorResult !InstantiationRef
  deriving stock (Eq, Ord, Show, Read)

type BinderScopedStep :: (Type -> Type) -> Type
data BinderScopedStep f = BinderScopedStep
  { bssAlgebra :: !(BinderSubstAlgebra f),
    bssPostSubst :: !(PostMatchSubst f),
    bssRhsPattern :: !(Pattern f),
    bssSubstitution :: !Substitution
  }

type InstantiationStep :: (Type -> Type) -> Type
data InstantiationStep f
  = LookupVar !InstantiationRef !PatternVar !ClassId
  | ConstructTerm !InstantiationRef !(f InstantiationInput)
  | BinderScoped !InstantiationRef !(BinderScopedStep f)

type InstantiationPlan :: (Type -> Type) -> Type
data InstantiationPlan f = InstantiationPlan
  { ipSteps :: ![InstantiationStep f],
    ipRoot :: !InstantiationRef
  }

type ExecutableRewriteMatch :: Type -> Type -> Type -> (Type -> Type) -> Type
data ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f = ExecutableRewriteMatch
  { ermRule :: !(RulePlan compiledGuard f),
    ermRootClass :: !ClassId,
    ermGuardEvidence :: !(Maybe guardEvidence),
    ermGuideEvidence :: !(Maybe guideEvidence),
    ermSubstitution :: !Substitution
  }

type ExecutedRewrite :: Type
data ExecutedRewrite = ExecutedRewrite
  { erwLhsClass :: !ClassId,
    erwRhsClass :: !ClassId,
    erwMergedClass :: !ClassId
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanBuilder :: (Type -> Type) -> Type
data PlanBuilder f = PlanBuilder
  { pbNextRef :: !Int,
    pbVarRefs :: !(IntMap InstantiationRef),
    pbStepsRev :: ![InstantiationStep f]
  }

emptyPlanBuilder :: PlanBuilder f
emptyPlanBuilder =
  PlanBuilder
    { pbNextRef = 0,
      pbVarRefs = IntMap.empty,
      pbStepsRev = []
    }

freshRef :: StateT (PlanBuilder f) (Either RewriteApplicationError) InstantiationRef
freshRef = do
  builder <- get
  let nextRef = InstantiationRef (pbNextRef builder)
  modify' (\state -> state {pbNextRef = pbNextRef state + 1})
  pure nextRef

emitStep :: InstantiationStep f -> StateT (PlanBuilder f) (Either RewriteApplicationError) ()
emitStep instantiationStep =
  modify' (\state -> state {pbStepsRev = instantiationStep : pbStepsRev state})

throwBuild :: Either RewriteApplicationError a -> StateT s (Either RewriteApplicationError) a
throwBuild eitherValue =
  StateT
    ( \state ->
        fmap
          (\value -> (value, state))
          eitherValue
    )

lookupSubstOrMissing ::
  PatternVar ->
  Substitution ->
  Either RewriteApplicationError ClassId
lookupSubstOrMissing patternVar =
  maybe
    (Left (RewriteMissingBinding patternVar))
    Right
    . lookupSubst patternVar

buildPatternRef ::
  Traversable f =>
  Pattern f ->
  Substitution ->
  StateT (PlanBuilder f) (Either RewriteApplicationError) InstantiationRef
buildPatternRef patternValue substitution =
  case patternValue of
    PatternVar patternVar -> do
      builder <- get
      case IntMap.lookup (patternVarKey patternVar) (pbVarRefs builder) of
        Just existingRef ->
          pure existingRef
        Nothing -> do
          boundClassId <- throwBuild (lookupSubstOrMissing patternVar substitution)
          nextRef <- freshRef
          emitStep (LookupVar nextRef patternVar boundClassId)
          modify'
            ( \state ->
                state
                  { pbVarRefs =
                      IntMap.insert (patternVarKey patternVar) nextRef (pbVarRefs state)
                  }
            )
          pure nextRef
    PatternNode patternNode -> do
      childRefs <- traverse (`buildPatternRef` substitution) patternNode
      nextRef <- freshRef
      emitStep (ConstructTerm nextRef (fmap PriorResult childRefs))
      pure nextRef

buildPatternInstantiationPlan ::
  Traversable f =>
  Pattern f ->
  Substitution ->
  Either RewriteApplicationError (InstantiationPlan f)
buildPatternInstantiationPlan patternValue substitution = do
  (rootRef, finalState) <-
    runStateT
      (buildPatternRef patternValue substitution)
      emptyPlanBuilder
  pure
    InstantiationPlan
      { ipSteps = reverse (pbStepsRev finalState),
        ipRoot = rootRef
      }

buildRewriteRhsPlan ::
  Traversable f =>
  Maybe (BinderSubstAlgebra f) ->
  RulePlan compiledGuard f ->
  Substitution ->
  Either RewriteApplicationError (InstantiationPlan f)
buildRewriteRhsPlan maybeBinderSubstAlgebra rulePlan substitution =
  case rpRhs rulePlan of
    StaticRhs _ rhsStaticPlan ->
      buildRhsStaticInstantiationPlan rhsStaticPlan substitution
    PostMatchRhs postSubst rhsPattern -> do
      binderScopeAlgebra <-
        maybe
          (Left (RewriteMissingBinderSubstAlgebra (rpId rulePlan)))
          Right
          maybeBinderSubstAlgebra
      let rootRef = InstantiationRef 0
      pure
        InstantiationPlan
          { ipSteps =
              [ BinderScoped
                  rootRef
                  BinderScopedStep
                    { bssAlgebra = binderScopeAlgebra,
                      bssPostSubst = postSubst,
                      bssRhsPattern = rhsPattern,
                      bssSubstitution = substitution
                    }
              ],
            ipRoot = rootRef
          }

buildRhsStaticInstantiationPlan ::
  Functor f =>
  RhsStaticPlan f ->
  Substitution ->
  Either RewriteApplicationError (InstantiationPlan f)
buildRhsStaticInstantiationPlan =
  buildRhsTemplateInstantiationPlan . rhsStaticPlanTemplate

buildRhsTemplateInstantiationPlan ::
  Functor f =>
  RhsTemplate f ->
  Substitution ->
  Either RewriteApplicationError (InstantiationPlan f)
buildRhsTemplateInstantiationPlan rhsTemplate substitution =
  InstantiationPlan
    <$> traverse (instantiateRhsTemplateStep substitution) (rhsTemplateSteps rhsTemplate)
    <*> pure (rhsTemplateRefToInstantiationRef (rhsTemplateRoot rhsTemplate))

instantiateRhsTemplateStep ::
  Functor f =>
  Substitution ->
  RhsTemplateStep f ->
  Either RewriteApplicationError (InstantiationStep f)
instantiateRhsTemplateStep substitution =
  \case
    RhsUseVar ref patternVar -> do
      boundClassId <- lookupSubstOrMissing patternVar substitution
      Right
        ( LookupVar
            (rhsTemplateRefToInstantiationRef ref)
            patternVar
            boundClassId
        )

    RhsConstruct ref termInputs ->
      Right
        ( ConstructTerm
            (rhsTemplateRefToInstantiationRef ref)
            (rhsTemplateInputToInstantiationInput <$> termInputs)
        )

rhsTemplateInputToInstantiationInput :: RhsTemplateInput -> InstantiationInput
rhsTemplateInputToInstantiationInput (RhsTemplatePrior ref) =
  PriorResult (rhsTemplateRefToInstantiationRef ref)

rhsTemplateRefToInstantiationRef :: RhsTemplateRef -> InstantiationRef
rhsTemplateRefToInstantiationRef (RhsTemplateRef refKey) =
  InstantiationRef refKey

lowerBinderScopedPlan ::
  Traversable f =>
  (PatternVar -> Either RewriteApplicationError (Pattern f)) ->
  InstantiationPlan f ->
  Either RewriteApplicationError (InstantiationPlan f)
lowerBinderScopedPlan resolveBindingPattern instantiationPlan =
  case ipSteps instantiationPlan of
    [BinderScoped _ scopedStep] -> do
      resolvedBindings <-
        fmap Map.fromList
          ( traverse
              ( \patternVar -> do
                  patternValue <- resolveBindingPattern patternVar
                  pure (patternVar, patternValue)
              )
              ( Set.toAscList
                  ( patternVariables (bssRhsPattern scopedStep)
                      <> postMatchSubstVariables (bssPostSubst scopedStep)
                  )
              )
          )
      let loweredRhsPattern =
            lowerResolvedBindings resolvedBindings (bssRhsPattern scopedStep)
      loweredPattern <-
        first
          RewriteMissingBinding
          ( applyPostMatchSubst
              (bssAlgebra scopedStep)
              resolvedBindings
              (bssPostSubst scopedStep)
              loweredRhsPattern
          )
      buildPatternInstantiationPlan loweredPattern (bssSubstitution scopedStep)
    _ ->
      if any isBinderScopedStep (ipSteps instantiationPlan)
        then Left RewriteUnloweredBinderScope
        else Right instantiationPlan
  where
    isBinderScopedStep :: InstantiationStep f -> Bool
    isBinderScopedStep instantiationStep =
      case instantiationStep of
        BinderScoped _ _ -> True
        _ -> False

lowerResolvedBindings ::
  Functor f =>
  Map.Map PatternVar (Pattern f) ->
  Pattern f ->
  Pattern f
lowerResolvedBindings resolvedBindings =
  \case
    PatternVar patternVar ->
      Map.findWithDefault (PatternVar patternVar) patternVar resolvedBindings
    PatternNode patternNode ->
      PatternNode (fmap (lowerResolvedBindings resolvedBindings) patternNode)

validateInstantiationPlan ::
  Foldable f =>
  InstantiationPlan f ->
  Either RewriteApplicationError ()
validateInstantiationPlan instantiationPlan =
  foldlM validateStep IntSet.empty (ipSteps instantiationPlan) >>= \seenRefs ->
    if IntSet.member (instantiationRefKey (ipRoot instantiationPlan)) seenRefs
      then Right ()
      else Left RewriteMissingInstantiatedNode
  where
    validateStep ::
      Foldable f =>
      IntSet.IntSet ->
      InstantiationStep f ->
      Either RewriteApplicationError IntSet.IntSet
    validateStep seenRefs instantiationStep =
      case instantiationStep of
        LookupVar ref _ _ ->
          insertFreshRef ref seenRefs
        ConstructTerm ref termInputs ->
          case unavailableInputKeys seenRefs (toList termInputs) of
            [] ->
              insertFreshRef ref seenRefs
            missingKey : _ ->
              Left (RewriteInstantiationInputUnavailable missingKey)
        BinderScoped {} ->
          Left RewriteUnloweredBinderScope

    insertFreshRef ref seenRefs =
      if IntSet.member (instantiationRefKey ref) seenRefs
        then Left (RewriteDuplicateInstantiationRef (instantiationRefKey ref))
        else Right (IntSet.insert (instantiationRefKey ref) seenRefs)

    unavailableInputKeys seenRefs termInputs =
      [ instantiationRefKey inputRef
        | PriorResult inputRef <- termInputs,
          not (IntSet.member (instantiationRefKey inputRef) seenRefs)
      ]

compileSteps ::
  Traversable f =>
  IntMap ClassId ->
  [InstantiationStep f] ->
  EGraphProgram RewriteApplicationError (f ClassId) (IntMap ClassId)
compileSteps env remainingSteps =
  case remainingSteps of
    [] ->
      pure env

    instantiationStep : laterSteps ->
      case instantiationStep of
        LookupVar ref _ classId -> do
          canonicalClassId <- canonicalizeClass classId
          compileSteps
            (IntMap.insert (instantiationRefKey ref) canonicalClassId env)
            laterSteps

        ConstructTerm ref termInputs -> do
          term <-
            traverse
              (resolveInstantiationInput env)
              termInputs
          resultClassId <-
            addCanonicalNode term
          compileSteps
            (IntMap.insert (instantiationRefKey ref) resultClassId env)
            laterSteps

        BinderScoped {} ->
          abortProgram RewriteUnloweredBinderScope

resolveInstantiationInput ::
  IntMap ClassId ->
  InstantiationInput ->
  EGraphProgram RewriteApplicationError term ClassId
resolveInstantiationInput env instantiationInput =
  case instantiationInput of
    ExistingClass classId ->
      canonicalizeClass classId

    PriorResult ref ->
      resolveInstantiationRef ref env

resolveInstantiationRef ::
  InstantiationRef ->
  IntMap ClassId ->
  EGraphProgram RewriteApplicationError term ClassId
resolveInstantiationRef ref env =
  case IntMap.lookup (instantiationRefKey ref) env of
    Nothing ->
      abortProgram (RewriteInstantiationInputUnavailable (instantiationRefKey ref))

    Just classId ->
      pure classId

compileInstantiationPlan ::
  Traversable f =>
  InstantiationPlan f ->
  Either
    RewriteApplicationError
    (EGraphProgram RewriteApplicationError (f ClassId) ClassId)
compileInstantiationPlan instantiationPlan = do
  validateInstantiationPlan instantiationPlan
  pure (compileTrustedInstantiationPlan instantiationPlan)

compileTrustedInstantiationPlan ::
  Traversable f =>
  InstantiationPlan f ->
  EGraphProgram RewriteApplicationError (f ClassId) ClassId
compileTrustedInstantiationPlan instantiationPlan = do
  finalEnv <-
    compileSteps IntMap.empty (ipSteps instantiationPlan)
  resolveInstantiationRef (ipRoot instantiationPlan) finalEnv

compileStaticRhsPlan ::
  Traversable f =>
  RhsStaticPlan f ->
  Substitution ->
  Either
    RewriteApplicationError
    (EGraphProgram RewriteApplicationError (f ClassId) ClassId)
compileStaticRhsPlan rhsStaticPlan substitution =
  compileTrustedInstantiationPlan
    <$> buildRhsStaticInstantiationPlan rhsStaticPlan substitution

compileRewriteRhs ::
  Traversable f =>
  Maybe (PatternVar -> Either RewriteApplicationError (Pattern f)) ->
  Maybe (BinderSubstAlgebra f) ->
  RulePlan compiledGuard f ->
  Substitution ->
  Either
    RewriteApplicationError
    (EGraphProgram RewriteApplicationError (f ClassId) ClassId)
compileRewriteRhs maybeResolveBindingPattern maybeBinderSubstAlgebra compiledRewrite substitution =
  case rpRhs compiledRewrite of
    StaticRhs _ rhsStaticPlan ->
      compileStaticRhsPlan rhsStaticPlan substitution
    PostMatchRhs {} -> do
      rhsPlan0 <-
        buildRewriteRhsPlan maybeBinderSubstAlgebra compiledRewrite substitution

      rhsPlan <-
        maybe
          (Right rhsPlan0)
          (`lowerBinderScopedPlan` rhsPlan0)
          maybeResolveBindingPattern

      compileInstantiationPlan rhsPlan

compileExecutableRewriteMatch ::
  Traversable f =>
  Maybe (PatternVar -> Either RewriteApplicationError (Pattern f)) ->
  Maybe (BinderSubstAlgebra f) ->
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  Either
    RewriteApplicationError
    (EGraphProgram RewriteApplicationError (f ClassId) ExecutedRewrite)
compileExecutableRewriteMatch maybeResolveBindingPattern maybeBinderSubstAlgebra rewriteMatch = do
  rhsProgram <-
    compileRewriteRhs
      maybeResolveBindingPattern
      maybeBinderSubstAlgebra
      (ermRule rewriteMatch)
      (ermSubstitution rewriteMatch)

  pure $ do
    lhsClassId <-
      canonicalizeClass (ermRootClass rewriteMatch)

    rhsClassId <-
      rhsProgram >>= canonicalizeClass

    mergedClassId <-
      mergeCanonicalClasses lhsClassId rhsClassId

    pure
      ExecutedRewrite
        { erwLhsClass = lhsClassId,
          erwRhsClass = rhsClassId,
          erwMergedClass = mergedClassId
        }

executableRewriteMatchRuleKey ::
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  RewriteRuleId
executableRewriteMatchRuleKey =
  rpId . ermRule
