{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Rewrite.Kernel.Condition
  ( PatternExtension (..),
    PatternExtensionScope (..),
    patternExtension,
    patternExtensionWithScope,
    rootPatternExtension,
    globalPatternExtension,
    withExtensionAnchorVars,
    ApplicationCondition (..),
    requiresExtension,
    forbidsExtension,
    andApplicationConditions,
    ApplicationConditionPath (..),
    ApplicationConditionCompileError (..),
    CompiledPatternExtension,
    cpePath,
    cpeQuery,
    cpeAnchorVars,
    cpeScope,
    renameCompiledPatternExtension,
    recompilePatternExtension,
    ApplicationConditionEffect (..),
    CompiledApplicationCondition,
    compiledApplicationCondition,
    compiledApplicationConditionExpression,
    compiledApplicationConditionExtensions,
    runCompiledApplicationConditionPlan,
    compileApplicationCondition,
    validateCompiledPatternExtensionAnchors,
  )
where

import Control.Selective
  ( (<&&>),
    (<||>),
    Selective,
  )
import Control.Selective.Free
  ( Select,
  )
import Control.Selective.Free qualified as Selective
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
  )
import Moonlight.Core
  ( Language,
    PatternVar,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( PatternRenaming,
    renamePattern,
    renamePatternVariableSet,
  )
import Moonlight.Rewrite.Kernel.Query
  ( CompiledPatternQuery,
    cpqQuery,
    PatternQuery,
    compilePatternQueryWithScope,
    mapCompiledPatternQuery,
    patternQueryVariables,
  )

type PatternExtensionScope :: Type
data PatternExtensionScope
  = ExtensionLocal
  | ExtensionRoot
  | ExtensionGlobal
  deriving stock (Eq, Ord, Show)

type PatternExtension :: Type -> (Type -> Type) -> Type
data PatternExtension guard f = PatternExtension
  { peQuery :: !(PatternQuery guard f),
    peExplicitAnchorVars :: !(Maybe (Set PatternVar)),
    peScope :: !PatternExtensionScope
  }

deriving stock instance Eq (PatternQuery guard f) => Eq (PatternExtension guard f)

deriving stock instance Ord (PatternQuery guard f) => Ord (PatternExtension guard f)

deriving stock instance Show (PatternQuery guard f) => Show (PatternExtension guard f)

patternExtension :: PatternQuery guard f -> PatternExtension guard f
patternExtension =
  patternExtensionWithScope ExtensionLocal

patternExtensionWithScope ::
  PatternExtensionScope ->
  PatternQuery guard f ->
  PatternExtension guard f
patternExtensionWithScope scope queryValue =
  PatternExtension
    { peQuery = queryValue,
      peExplicitAnchorVars = Nothing,
      peScope = scope
    }

rootPatternExtension :: PatternQuery guard f -> PatternExtension guard f
rootPatternExtension =
  patternExtensionWithScope ExtensionRoot

globalPatternExtension :: PatternQuery guard f -> PatternExtension guard f
globalPatternExtension =
  patternExtensionWithScope ExtensionGlobal

withExtensionAnchorVars ::
  Set PatternVar ->
  PatternExtension guard f ->
  PatternExtension guard f
withExtensionAnchorVars anchorVars extension =
  extension
    { peExplicitAnchorVars = Just anchorVars
    }

type ApplicationCondition :: Type -> (Type -> Type) -> Type
newtype ApplicationCondition guard f = ApplicationCondition
  { unApplicationCondition :: ConstraintExpr (PatternExtension guard f)
  }

deriving stock instance Eq (PatternExtension guard f) => Eq (ApplicationCondition guard f)

deriving stock instance Ord (PatternExtension guard f) => Ord (ApplicationCondition guard f)

deriving stock instance Show (PatternExtension guard f) => Show (ApplicationCondition guard f)

requiresExtension ::
  PatternExtension guard f ->
  ApplicationCondition guard f
requiresExtension =
  ApplicationCondition . Atom

forbidsExtension ::
  PatternExtension guard f ->
  ApplicationCondition guard f
forbidsExtension =
  ApplicationCondition . Not . Atom

andApplicationConditions ::
  [ApplicationCondition guard f] ->
  ApplicationCondition guard f
andApplicationConditions =
  ApplicationCondition . And . fmap unApplicationCondition

type ApplicationConditionPath :: Type
newtype ApplicationConditionPath = ApplicationConditionPath
  { unApplicationConditionPath :: [Int]
  }
  deriving stock (Eq, Ord, Show, Read)

rootApplicationConditionPath :: ApplicationConditionPath
rootApplicationConditionPath =
  ApplicationConditionPath []

childApplicationConditionPath ::
  ApplicationConditionPath ->
  Int ->
  ApplicationConditionPath
childApplicationConditionPath (ApplicationConditionPath path) childIndex =
  ApplicationConditionPath (path <> [childIndex])

type ApplicationConditionCompileError :: Type
data ApplicationConditionCompileError
  = ApplicationConditionExtensionGuardIntroducesUnboundVars
      !ApplicationConditionPath
      ![PatternVar]
  | ApplicationConditionExtensionInvalidAnchors
      !ApplicationConditionPath
      ![PatternVar]
  | ApplicationConditionExtensionUnanchored
      !ApplicationConditionPath
  deriving stock (Eq, Ord, Show)

type CompiledPatternExtension :: Type -> (Type -> Type) -> Type
data CompiledPatternExtension compiledGuard f = CompiledPatternExtension
  !ApplicationConditionPath
  !(CompiledPatternQuery compiledGuard f)
  !(Set PatternVar)
  !PatternExtensionScope

deriving stock instance Eq (CompiledPatternQuery compiledGuard f) => Eq (CompiledPatternExtension compiledGuard f)

deriving stock instance Ord (CompiledPatternQuery compiledGuard f) => Ord (CompiledPatternExtension compiledGuard f)

deriving stock instance Show (CompiledPatternQuery compiledGuard f) => Show (CompiledPatternExtension compiledGuard f)

cpePath :: CompiledPatternExtension compiledGuard f -> ApplicationConditionPath
cpePath (CompiledPatternExtension path _query _anchorVars _scope) =
  path

cpeQuery :: CompiledPatternExtension compiledGuard f -> CompiledPatternQuery compiledGuard f
cpeQuery (CompiledPatternExtension _path query _anchorVars _scope) =
  query

cpeAnchorVars :: CompiledPatternExtension compiledGuard f -> Set PatternVar
cpeAnchorVars (CompiledPatternExtension _path _query anchorVars _scope) =
  anchorVars

cpeScope :: CompiledPatternExtension compiledGuard f -> PatternExtensionScope
cpeScope (CompiledPatternExtension _path _query _anchorVars scope) =
  scope

type ApplicationConditionEffect :: Type -> (Type -> Type) -> Type -> Type
data ApplicationConditionEffect compiledGuard f result = ApplicationConditionEffect
  { aceExtension :: !(CompiledPatternExtension compiledGuard f),
    aceResult :: Bool -> result
  }

instance Functor (ApplicationConditionEffect compiledGuard f) where
  fmap transform effect =
    effect
      { aceResult = transform . aceResult effect
      }

type CompiledApplicationCondition :: Type -> (Type -> Type) -> Type
data CompiledApplicationCondition compiledGuard f = CompiledApplicationCondition
  !(ConstraintExpr (CompiledPatternExtension compiledGuard f))
  !(Select (ApplicationConditionEffect compiledGuard f) Bool)

compiledApplicationConditionExpression ::
  CompiledApplicationCondition compiledGuard f ->
  ConstraintExpr (CompiledPatternExtension compiledGuard f)
compiledApplicationConditionExpression (CompiledApplicationCondition expression _plan) =
  expression

compiledApplicationConditionPlan ::
  CompiledApplicationCondition compiledGuard f ->
  Select (ApplicationConditionEffect compiledGuard f) Bool
compiledApplicationConditionPlan (CompiledApplicationCondition _expression plan) =
  plan

instance Eq (CompiledPatternExtension compiledGuard f) => Eq (CompiledApplicationCondition compiledGuard f) where
  leftCondition == rightCondition =
    compiledApplicationConditionExpression leftCondition
      == compiledApplicationConditionExpression rightCondition

instance Ord (CompiledPatternExtension compiledGuard f) => Ord (CompiledApplicationCondition compiledGuard f) where
  compare leftCondition rightCondition =
    compare
      (compiledApplicationConditionExpression leftCondition)
      (compiledApplicationConditionExpression rightCondition)

instance Show (CompiledPatternExtension compiledGuard f) => Show (CompiledApplicationCondition compiledGuard f) where
  showsPrec precedence condition =
    showParen (precedence > 10) $
      showString "compiledApplicationCondition "
        . showsPrec 11 (compiledApplicationConditionExpression condition)

compiledApplicationCondition ::
  ConstraintExpr (CompiledPatternExtension compiledGuard f) ->
  CompiledApplicationCondition compiledGuard f
compiledApplicationCondition expression =
  CompiledApplicationCondition expression (applicationConditionPlan expression)

compiledApplicationConditionExtensions ::
  CompiledApplicationCondition compiledGuard f ->
  [CompiledPatternExtension compiledGuard f]
compiledApplicationConditionExtensions =
  fmap aceExtension
    . Selective.getEffects
    . compiledApplicationConditionPlan

runCompiledApplicationConditionPlan ::
  Selective m =>
  (forall result. ApplicationConditionEffect compiledGuard f result -> m result) ->
  CompiledApplicationCondition compiledGuard f ->
  m Bool
runCompiledApplicationConditionPlan interpret =
  Selective.runSelect interpret . compiledApplicationConditionPlan

applicationConditionPlan ::
  ConstraintExpr (CompiledPatternExtension compiledGuard f) ->
  Select (ApplicationConditionEffect compiledGuard f) Bool
applicationConditionPlan =
  \case
    Atom extension ->
      Selective.liftSelect
        ( ApplicationConditionEffect
            { aceExtension = extension,
              aceResult = id
            }
        )

    Not child ->
      not <$> applicationConditionPlan child

    And children ->
      foldr ((<&&>) . applicationConditionPlan) (pure True) children

    Or children ->
      foldr ((<||>) . applicationConditionPlan) (pure False) children

compileApplicationCondition ::
  Language f =>
  ([compiledGuard] -> Maybe compiledGuard) ->
  (Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  Set PatternVar ->
  ApplicationCondition guard f ->
  Either
    ApplicationConditionCompileError
    (CompiledApplicationCondition compiledGuard f)
compileApplicationCondition combineCompiledGuards compileGuard lhsVariables =
  fmap compiledApplicationCondition
    . traverseApplicationConditionExprWithPath
      (compilePatternExtension combineCompiledGuards compileGuard lhsVariables)
      rootApplicationConditionPath
    . unApplicationCondition

traverseApplicationConditionExprWithPath ::
  (ApplicationConditionPath -> atom -> Either err atom') ->
  ApplicationConditionPath ->
  ConstraintExpr atom ->
  Either err (ConstraintExpr atom')
traverseApplicationConditionExprWithPath transform path =
  \case
    Atom atom ->
      Atom <$> transform path atom

    Not child ->
      Not <$> traverseApplicationConditionExprWithPath transform (childApplicationConditionPath path 0) child

    And children ->
      And <$> traverseChildren children

    Or children ->
      Or <$> traverseChildren children
  where
    traverseChildren =
      traverse (uncurry (traverseChild path)) . zip [0 ..]

    traverseChild parentPath childIndex =
      traverseApplicationConditionExprWithPath
        transform
        (childApplicationConditionPath parentPath childIndex)

compilePatternExtension ::
  Language f =>
  ([compiledGuard] -> Maybe compiledGuard) ->
  (Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  Set PatternVar ->
  ApplicationConditionPath ->
  PatternExtension guard f ->
  Either
    ApplicationConditionCompileError
    (CompiledPatternExtension compiledGuard f)
compilePatternExtension combineCompiledGuards compileGuard lhsVariables path extension = do
  anchorVars <-
    checkedExtensionAnchors
      path
      lhsVariables
      (patternQueryVariables (peQuery extension))
      (peExplicitAnchorVars extension)
      (peScope extension)

  compiledQuery <-
    either
      (Left . ApplicationConditionExtensionGuardIntroducesUnboundVars path)
      Right
      ( compilePatternQueryWithScope
          combineCompiledGuards
          compileGuard
          lhsVariables
          (peQuery extension)
      )

  Right (CompiledPatternExtension path compiledQuery anchorVars (peScope extension))

renameCompiledPatternExtension ::
  (Language f, Semigroup mappedGuard) =>
  PatternRenaming ->
  (compiledGuard -> mappedGuard) ->
  CompiledPatternExtension compiledGuard f ->
  CompiledPatternExtension mappedGuard f
renameCompiledPatternExtension renaming mapGuard extension =
  CompiledPatternExtension
    (cpePath extension)
    ( mapCompiledPatternQuery
        (renamePattern renaming)
        mapGuard
        (cpeQuery extension)
    )
    (renamePatternVariableSet renaming (cpeAnchorVars extension))
    (cpeScope extension)

recompilePatternExtension ::
  Foldable mappedF =>
  Set PatternVar ->
  CompiledPatternQuery mappedGuard mappedF ->
  CompiledPatternExtension compiledGuard f ->
  Either ApplicationConditionCompileError (CompiledPatternExtension mappedGuard mappedF)
recompilePatternExtension anchorVars mappedQuery extension = do
  checkedAnchors <-
    checkedExtensionAnchors
      (cpePath extension)
      anchorVars
      (patternQueryVariables (cpqQuery mappedQuery))
      (Just anchorVars)
      (cpeScope extension)
  pure
    ( CompiledPatternExtension
        (cpePath extension)
        mappedQuery
        checkedAnchors
        (cpeScope extension)
    )

compiledPatternExtensionPatternVariables ::
  Foldable f =>
  CompiledPatternExtension compiledGuard f ->
  Set PatternVar
compiledPatternExtensionPatternVariables =
  patternQueryVariables . cpqQuery . cpeQuery

validateCompiledPatternExtensionAnchors ::
  Foldable f =>
  Set PatternVar ->
  CompiledPatternExtension compiledGuard f ->
  Either ApplicationConditionCompileError ()
validateCompiledPatternExtensionAnchors lhsVariables extension =
  () <$
    checkedExtensionAnchors
      (cpePath extension)
      lhsVariables
      (compiledPatternExtensionPatternVariables extension)
      (Just (cpeAnchorVars extension))
      (cpeScope extension)

checkedExtensionAnchors ::
  ApplicationConditionPath ->
  Set PatternVar ->
  Set PatternVar ->
  Maybe (Set PatternVar) ->
  PatternExtensionScope ->
  Either ApplicationConditionCompileError (Set PatternVar)
checkedExtensionAnchors path lhsVariables queryVariables explicitAnchorVars scope =
  let anchorVars =
        fromMaybe (Set.intersection lhsVariables queryVariables) explicitAnchorVars

      invalidAnchorVars =
        Set.toAscList
          (Set.difference anchorVars (Set.intersection lhsVariables queryVariables))

      isAnchored =
        not (Set.null anchorVars) || scope /= ExtensionLocal
   in case (invalidAnchorVars, isAnchored) of
        (_ : _, _) ->
          Left (ApplicationConditionExtensionInvalidAnchors path invalidAnchorVars)

        ([], False) ->
          Left (ApplicationConditionExtensionUnanchored path)

        ([], True) ->
          Right anchorVars
