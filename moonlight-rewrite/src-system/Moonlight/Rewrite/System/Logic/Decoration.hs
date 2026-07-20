{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

-- | Logical rewrite decoration carried by system rewrites.
-- Owns condition, application-condition, and post-match-substitution transport
-- through renaming, projection, composition, and validation.
-- Contracts: guard terms are structurally transported, and invalid variables
-- or application-condition projections surface as typed decoration failures.
module Moonlight.Rewrite.System.Logic.Decoration
  ( LogicalDecoration,
    logicalDecoration,
    logicalDecorationWithApplicationCondition,
    ldCondition,
    ldApplicationCondition,
    ldPostSubst,
    renameCompiledGuard,
    projectCompiledGuard,
    composeOptionalCompiledGuards,
    renameCompiledApplicationCondition,
    projectCompiledApplicationCondition,
    composeOptionalCompiledApplicationConditions,
    renamePostMatchSubst,
    projectPostMatchSubst,
    composeOptionalPostMatchSubst,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_)
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
  )
import Moonlight.Core
  ( Language,
    Pattern (..),
    PatternVar,
    safeIndexNatural,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionCompileError,
    CompiledApplicationCondition,
    CompiledPatternExtension,
    cpeAnchorVars,
    cpeQuery,
    compiledApplicationCondition,
    compiledApplicationConditionExpression,
    recompilePatternExtension,
    renameCompiledPatternExtension,
    validateCompiledPatternExtensionAnchors,
  )
import Moonlight.Rewrite.Algebra
  ( cpqQuery,
    compiledPatternQueryVariablesWith,
    mapCompiledPatternQuery,
    patternQueryConditions,
    patternQueryVariables,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
    GuardAtom (..),
    GuardBase (..),
    GuardChildIndex,
    GuardExpr,
    GuardPath (..),
    GuardRef (..),
    GuardTerm (..),
    compiledGuardVariables,
    guardChildIndexValue,
    guardRefTerm,
    mapCompiledGuard,
    data GuardRoot,
    data GuardVar,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst (..),
    PostMatchTerm (..),
    postMatchSubstVariables,
  )
import Moonlight.Rewrite.Algebra
  ( DecorationError (..),
    PatternProjection (..),
    PatternRenaming,
    RewriteDecoration (..),
    applyPatternRenamingVar,
    isEmptyPatternProjection,
    projectPattern,
    projectVariableSet,
    renamePattern,
  )

type LogicalDecoration :: Type -> (Type -> Type) -> Type
data LogicalDecoration capability f = LogicalDecoration
  { ldCondition :: !(Maybe (CompiledGuard capability f)),
    ldApplicationCondition :: !(Maybe (CompiledApplicationCondition (CompiledGuard capability f) f)),
    ldPostSubst :: !(Maybe (PostMatchSubst f))
  }

deriving stock instance
  ( Eq (CompiledGuard capability f),
    Eq (CompiledApplicationCondition (CompiledGuard capability f) f),
    Eq (PostMatchSubst f)
  ) =>
  Eq (LogicalDecoration capability f)

deriving stock instance
  ( Ord (CompiledGuard capability f),
    Ord (CompiledApplicationCondition (CompiledGuard capability f) f),
    Ord (PostMatchSubst f)
  ) =>
  Ord (LogicalDecoration capability f)

deriving stock instance
  ( Show (CompiledGuard capability f),
    Show (CompiledApplicationCondition (CompiledGuard capability f) f),
    Show (PostMatchSubst f)
  ) =>
  Show (LogicalDecoration capability f)

logicalDecoration ::
  Maybe (CompiledGuard capability f) ->
  Maybe (PostMatchSubst f) ->
  LogicalDecoration capability f
logicalDecoration condition =
  logicalDecorationWithApplicationCondition condition Nothing

logicalDecorationWithApplicationCondition ::
  Maybe (CompiledGuard capability f) ->
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f) ->
  Maybe (PostMatchSubst f) ->
  LogicalDecoration capability f
logicalDecorationWithApplicationCondition condition applicationCondition postSubst =
  case (condition, applicationCondition, postSubst) of
    (Nothing, Nothing, Nothing) ->
      emptyLogicalDecoration

    _ ->
      LogicalDecoration
        { ldCondition = condition,
          ldApplicationCondition = applicationCondition,
          ldPostSubst = postSubst
        }

emptyLogicalDecoration :: LogicalDecoration capability f
emptyLogicalDecoration =
  LogicalDecoration
    { ldCondition = Nothing,
      ldApplicationCondition = Nothing,
      ldPostSubst = Nothing
    }
{-# NOINLINE emptyLogicalDecoration #-}

instance Ord capability => RewriteDecoration (LogicalDecoration capability) where
  type DecorationConstraint (LogicalDecoration capability) f = Language f
  type DecorationObstruction (LogicalDecoration capability) f = ApplicationConditionCompileError

  emptyDecoration =
    emptyLogicalDecoration

  decorationVariables decoration =
    maybe Set.empty compiledGuardVariables (ldCondition decoration)
      <> maybe Set.empty compiledApplicationConditionVariables (ldApplicationCondition decoration)
      <> maybe Set.empty postMatchSubstVariables (ldPostSubst decoration)

  renameDecoration renaming decoration =
    LogicalDecoration
      { ldCondition = renameCompiledGuard renaming <$> ldCondition decoration,
        ldApplicationCondition =
          renameCompiledApplicationCondition renaming <$> ldApplicationCondition decoration,
        ldPostSubst = renamePostMatchSubst renaming <$> ldPostSubst decoration
      }

  projectDecoration projection decoration =
    if isEmptyPatternProjection projection
      then Right decoration
      else do
        projectedApplicationCondition <-
          traverse
            (first DecorationInvalidProjection . projectCompiledApplicationCondition projection)
            (ldApplicationCondition decoration)
        Right
          LogicalDecoration
            { ldCondition = projectCompiledGuard projection <$> ldCondition decoration,
              ldApplicationCondition = projectedApplicationCondition,
              ldPostSubst = projectPostMatchSubst projection <$> ldPostSubst decoration
            }

  composeDecoration leftDecorationValue rightDecorationValue =
    Right
      LogicalDecoration
        { ldCondition =
            composeOptionalCompiledGuards
              (ldCondition leftDecorationValue)
              (ldCondition rightDecorationValue),
          ldApplicationCondition =
            composeOptionalCompiledApplicationConditions
              (ldApplicationCondition leftDecorationValue)
              (ldApplicationCondition rightDecorationValue),
          ldPostSubst =
            composeOptionalPostMatchSubst
              (ldPostSubst leftDecorationValue)
              (ldPostSubst rightDecorationValue)
        }

  validateDecoration boundVariables decoration =
    validateBoundVariables
      boundVariables
      (maybe Set.empty compiledGuardVariables (ldCondition decoration))
      *> traverse_
        (validateCompiledApplicationCondition boundVariables)
        (ldApplicationCondition decoration)
      *> validateBoundVariables
        boundVariables
        (maybe Set.empty postMatchSubstVariables (ldPostSubst decoration))

validateBoundVariables ::
  Set.Set PatternVar ->
  Set.Set PatternVar ->
  Either (DecorationError obstruction f) ()
validateBoundVariables boundVariables observedVariables =
  let unboundVariables =
        Set.toAscList (Set.difference observedVariables boundVariables)
   in case unboundVariables of
        [] ->
          Right ()

        _ : _ ->
          Left (DecorationUnboundVariables unboundVariables)

applicationConditionDecorationError ::
  ApplicationConditionCompileError ->
  DecorationError ApplicationConditionCompileError f
applicationConditionDecorationError =
  DecorationInvalidProjection

validateCompiledApplicationCondition ::
  (Foldable f, Ord capability, forall a. Ord a => Ord (f a)) =>
  Set.Set PatternVar ->
  CompiledApplicationCondition (CompiledGuard capability f) f ->
  Either (DecorationError ApplicationConditionCompileError f) ()
validateCompiledApplicationCondition boundVariables applicationCondition =
  traverse_ validateExtension (compiledApplicationConditionExpression applicationCondition)
  where
    validateExtension extension = do
      first applicationConditionDecorationError
        (validateCompiledPatternExtensionAnchors boundVariables extension)

      let query = cpqQuery (cpeQuery extension)
      validateBoundVariables
        (boundVariables <> patternQueryVariables query)
        (foldMap compiledGuardVariables (patternQueryConditions query))

renameCompiledGuard :: (Functor f, Ord capability, forall a. Ord a => Ord (f a)) => PatternRenaming -> CompiledGuard capability f -> CompiledGuard capability f
renameCompiledGuard = mapCompiledGuard . renameGuardExpr

renameGuardExpr :: Functor f => PatternRenaming -> GuardExpr capability f -> GuardExpr capability f
renameGuardExpr = mapGuardExprAtoms . renameGuardAtom

renameGuardAtom :: Functor f => PatternRenaming -> GuardAtom capability f -> GuardAtom capability f
renameGuardAtom = mapGuardAtomTerms . renameGuardTerm

renameGuardTerm :: Functor f => PatternRenaming -> GuardTerm f -> GuardTerm f
renameGuardTerm renaming =
  \case
    GuardRefTerm guardRef ->
      GuardRefTerm (renameGuardRef renaming guardRef)
    GuardProjectTerm baseTerm childIndex ->
      GuardProjectTerm (renameGuardTerm renaming baseTerm) childIndex
    GuardNodeTerm guardNode ->
      GuardNodeTerm (fmap (renameGuardTerm renaming) guardNode)

renameGuardRef :: PatternRenaming -> GuardRef -> GuardRef
renameGuardRef renaming (GuardRef (guardBase, guardPath)) =
  GuardRef (renameGuardBase renaming guardBase, guardPath)

renameGuardBase :: PatternRenaming -> GuardBase -> GuardBase
renameGuardBase renaming =
  \case
    GuardFromRoot ->
      GuardFromRoot
    GuardFromVar patternVar ->
      GuardFromVar (applyPatternRenamingVar renaming patternVar)

projectCompiledGuard :: (Foldable f, Functor f, Ord capability, forall a. Ord a => Ord (f a)) => PatternProjection f -> CompiledGuard capability f -> CompiledGuard capability f
projectCompiledGuard = mapCompiledGuard . projectGuardExpr

projectGuardExpr ::
  (Foldable f, Functor f) =>
  PatternProjection f ->
  GuardExpr capability f ->
  GuardExpr capability f
projectGuardExpr = mapGuardExprAtoms . projectGuardAtom

projectGuardAtom ::
  (Foldable f, Functor f) =>
  PatternProjection f ->
  GuardAtom capability f ->
  GuardAtom capability f
projectGuardAtom = mapGuardAtomTerms . projectGuardTerm

mapGuardExprAtoms :: (GuardAtom capability f -> GuardAtom capability f) -> GuardExpr capability f -> GuardExpr capability f
mapGuardExprAtoms transformAtom =
  transformExpr
  where
    transformExpr =
      \case
        Atom guardAtom ->
          Atom (transformAtom guardAtom)
        And childExprs ->
          And (fmap transformExpr childExprs)
        Or childExprs ->
          Or (fmap transformExpr childExprs)
        Not childExpr ->
          Not (transformExpr childExpr)

mapGuardAtomTerms :: (GuardTerm f -> GuardTerm f) -> GuardAtom capability f -> GuardAtom capability f
mapGuardAtomTerms transformTerm =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      ClassesEquivalent (transformTerm leftTerm) (transformTerm rightTerm)
    HasFact factId guardTerms ->
      HasFact factId (fmap transformTerm guardTerms)
    HasCapability capability guardTerms ->
      HasCapability capability (fmap transformTerm guardTerms)

projectGuardTerm ::
  (Foldable f, Functor f) =>
  PatternProjection f ->
  GuardTerm f ->
  GuardTerm f
projectGuardTerm projection =
  \case
    GuardRefTerm guardRef ->
      projectGuardRef projection guardRef
    GuardProjectTerm baseTerm childIndex ->
      projectChildTerm childIndex (projectGuardTerm projection baseTerm)
    GuardNodeTerm guardNode ->
      GuardNodeTerm (fmap (projectGuardTerm projection) guardNode)

projectGuardRef ::
  (Foldable f, Functor f) =>
  PatternProjection f ->
  GuardRef ->
  GuardTerm f
projectGuardRef projection (GuardRef (guardBase, guardPath)) =
  applyGuardPath guardPath (projectGuardBase projection guardBase)

projectGuardBase ::
  Functor f =>
  PatternProjection f ->
  GuardBase ->
  GuardTerm f
projectGuardBase projection =
  \case
    GuardFromRoot ->
      guardRefTerm GuardRoot
    GuardFromVar patternVar ->
      case projectPattern projection (PatternVar patternVar) of
        PatternVar projectedVar ->
          guardRefTerm (GuardVar projectedVar)
        PatternNode projectedNode ->
          GuardNodeTerm (fmap patternToGuardTerm projectedNode)

applyGuardPath :: Foldable f => GuardPath -> GuardTerm f -> GuardTerm f
applyGuardPath (GuardPath childIndices) guardTerm =
  foldl' (flip projectChildTerm) guardTerm childIndices

projectChildTerm :: Foldable f => GuardChildIndex -> GuardTerm f -> GuardTerm f
projectChildTerm childIndex guardTerm =
  case guardTerm of
    GuardNodeTerm guardNode ->
      maybe
        (GuardProjectTerm guardTerm childIndex)
        id
        (selectChildTerm childIndex (toList guardNode))
    _ ->
      GuardProjectTerm guardTerm childIndex

selectChildTerm :: GuardChildIndex -> [GuardTerm f] -> Maybe (GuardTerm f)
selectChildTerm childIndex =
  safeIndexNatural (fromIntegral (guardChildIndexValue childIndex))

patternToGuardTerm :: Functor f => Pattern f -> GuardTerm f
patternToGuardTerm =
  \case
    PatternVar patternVar ->
      guardRefTerm (GuardVar patternVar)
    PatternNode patternNode ->
      GuardNodeTerm (fmap patternToGuardTerm patternNode)

composeOptionalCompiledGuards ::
  (Ord capability, forall a. Ord a => Ord (f a)) =>
  Maybe (CompiledGuard capability f) ->
  Maybe (CompiledGuard capability f) ->
  Maybe (CompiledGuard capability f)
composeOptionalCompiledGuards =
  composeOptionalWith (<>)

compiledApplicationConditionVariables ::
  (Foldable f, Ord capability, forall a. Ord a => Ord (f a)) =>
  CompiledApplicationCondition (CompiledGuard capability f) f ->
  Set.Set PatternVar
compiledApplicationConditionVariables =
  foldMap
    (compiledPatternQueryVariablesWith compiledGuardVariables . cpeQuery)
    . compiledApplicationConditionExpression

renameCompiledApplicationCondition :: (Language f, Ord capability) => PatternRenaming -> CompiledApplicationCondition (CompiledGuard capability f) f -> CompiledApplicationCondition (CompiledGuard capability f) f
renameCompiledApplicationCondition renaming =
  mapCompiledApplicationConditionExtensions
    (renameCompiledPatternExtension renaming (renameCompiledGuard renaming))

projectCompiledApplicationCondition ::
  (Language f, Ord capability) =>
  PatternProjection f ->
  CompiledApplicationCondition (CompiledGuard capability f) f ->
  Either ApplicationConditionCompileError (CompiledApplicationCondition (CompiledGuard capability f) f)
projectCompiledApplicationCondition projection =
  fmap compiledApplicationCondition
    . traverse (projectCompiledPatternExtension projection)
    . compiledApplicationConditionExpression

projectCompiledPatternExtension ::
  (Language f, Ord capability) =>
  PatternProjection f ->
  CompiledPatternExtension (CompiledGuard capability f) f ->
  Either ApplicationConditionCompileError (CompiledPatternExtension (CompiledGuard capability f) f)
projectCompiledPatternExtension projection extension =
  recompilePatternExtension
    (projectVariableSet projection (cpeAnchorVars extension))
    ( mapCompiledPatternQuery
        (projectPattern projection)
        (projectCompiledGuard projection)
        (cpeQuery extension)
    )
    extension

mapCompiledApplicationConditionExtensions :: (CompiledPatternExtension (CompiledGuard capability f) f -> CompiledPatternExtension (CompiledGuard capability f) f) -> CompiledApplicationCondition (CompiledGuard capability f) f -> CompiledApplicationCondition (CompiledGuard capability f) f
mapCompiledApplicationConditionExtensions transformExtension =
  compiledApplicationCondition
    . fmap transformExtension
    . compiledApplicationConditionExpression

composeCompiledApplicationConditions ::
  CompiledApplicationCondition (CompiledGuard capability f) f ->
  CompiledApplicationCondition (CompiledGuard capability f) f ->
  CompiledApplicationCondition (CompiledGuard capability f) f
composeCompiledApplicationConditions leftCondition rightCondition =
  compiledApplicationCondition
    ( And
        [ compiledApplicationConditionExpression leftCondition,
          compiledApplicationConditionExpression rightCondition
        ]
    )

composeOptionalCompiledApplicationConditions ::
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f) ->
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f) ->
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f)
composeOptionalCompiledApplicationConditions =
  composeOptionalWith composeCompiledApplicationConditions

renamePostMatchSubst ::
  Functor f =>
  PatternRenaming ->
  PostMatchSubst f ->
  PostMatchSubst f
renamePostMatchSubst = mapPostMatchSubstTerms . renamePostMatchTerm

renamePostMatchTerm ::
  Functor f =>
  PatternRenaming ->
  PostMatchTerm f ->
  PostMatchTerm f
renamePostMatchTerm renaming =
  \case
    PostMatchVar patternVar ->
      PostMatchVar (applyPatternRenamingVar renaming patternVar)
    PostMatchPattern patternValue ->
      PostMatchPattern (renamePattern renaming patternValue)

projectPostMatchSubst ::
  Functor f =>
  PatternProjection f ->
  PostMatchSubst f ->
  PostMatchSubst f
projectPostMatchSubst = mapPostMatchSubstTerms . projectPostMatchTerm

mapPostMatchSubstTerms :: (PostMatchTerm f -> PostMatchTerm f) -> PostMatchSubst f -> PostMatchSubst f
mapPostMatchSubstTerms transformTerm =
  transformSubst
  where
    transformSubst =
      \case
        SubstBinder targetBinderId argumentTerm ->
          SubstBinder
            targetBinderId
            (transformTerm argumentTerm)
        SequentialPostMatchSubst leftSubst rightSubst ->
          SequentialPostMatchSubst
            (transformSubst leftSubst)
            (transformSubst rightSubst)

projectPostMatchTerm ::
  Functor f =>
  PatternProjection f ->
  PostMatchTerm f ->
  PostMatchTerm f
projectPostMatchTerm projection =
  \case
    PostMatchVar patternVar ->
      case projectPattern projection (PatternVar patternVar) of
        PatternVar projectedVar ->
          PostMatchVar projectedVar
        PatternNode projectedNode ->
          PostMatchPattern (PatternNode projectedNode)
    PostMatchPattern patternValue ->
      PostMatchPattern (projectPattern projection patternValue)

composeOptionalPostMatchSubst ::
  Maybe (PostMatchSubst f) ->
  Maybe (PostMatchSubst f) ->
  Maybe (PostMatchSubst f)
composeOptionalPostMatchSubst =
  composeOptionalWith SequentialPostMatchSubst

composeOptionalWith ::
  (value -> value -> value) ->
  Maybe value ->
  Maybe value ->
  Maybe value
composeOptionalWith combine maybeLeft maybeRight =
  case (maybeLeft, maybeRight) of
    (Nothing, Nothing) ->
      Nothing
    (Just left, Nothing) ->
      Just left
    (Nothing, Just right) ->
      Just right
    (Just left, Just right) ->
      Just (combine left right)
