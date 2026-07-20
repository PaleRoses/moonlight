{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.System.Compose
  ( addComposedPathNamed,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Core
  ( HasConstructorTag,
    Language,
    ZipMatch,
  )
import Moonlight.Rewrite.Algebra
  ( CompositionResult (..),
    PatternRewrite,
    allPatternRewriteVariables,
    composePatternRewrites,
  )
import Moonlight.Rewrite.System.Check
  ( RewriteError (..),
    checkedSystemErrorToRewriteError,
    rewriteByRuleName,
  )
import Moonlight.Rewrite.System.Checked
  ( CheckedRewrite,
    CheckedSystem,
    checkedRewriteAlgebra,
    checkedRewriteVariables,
    lookupCheckedRewrite,
  )
import Moonlight.Rewrite.System.Checked.Internal
  ( insertDerivedRewriteInternal,
  )
import Moonlight.Rewrite.System.Logic.Decoration (LogicalDecoration)
import Moonlight.Rewrite.System.Origin (RuleOrigin)
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )
import Moonlight.Rewrite.System.Variable
  ( RuleVariables,
    RuleVariableMetadataError,
    mergeRuleVariables,
    projectRuleVariables,
    renameRuleVariables,
    restrictRuleVariables,
  )

data DerivedRewriteBody capability f = DerivedRewriteBody
  { derivedRewriteAlgebra :: !(PatternRewrite RuleOrigin (LogicalDecoration capability) f),
    derivedRewriteVariables :: !RuleVariables
  }

addComposedPathNamed ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RuleName ->
  NonEmpty RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (CheckedSystem capability f)
addComposedPathNamed newName pathNames checkedSystem = do
  validateFreshDerivedName newName checkedSystem
  derivedBody <-
    composeRewritePath pathNames checkedSystem
  first checkedSystemErrorToRewriteError
    ( insertDerivedRewriteInternal
        newName
        (derivedRewriteAlgebra derivedBody)
        (derivedRewriteVariables derivedBody)
        checkedSystem
    )

composeRewritePath ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  NonEmpty RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (DerivedRewriteBody capability f)
composeRewritePath (firstName :| remainingNames) checkedSystem = do
  firstRewrite <-
    rewriteByRuleName firstName checkedSystem
  foldlM
    (\currentBody nextName -> rewriteByRuleName nextName checkedSystem >>= composeDerivedBody currentBody)
    (derivedBodyFromChecked firstRewrite)
    remainingNames

derivedBodyFromChecked ::
  (Language f, Ord capability) =>
  CheckedRewrite capability f ->
  DerivedRewriteBody capability f
derivedBodyFromChecked rewriteValue =
  DerivedRewriteBody
    { derivedRewriteAlgebra = checkedRewriteAlgebra rewriteValue,
      derivedRewriteVariables = checkedRewriteVariables rewriteValue
    }

composeDerivedBody ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  DerivedRewriteBody capability f ->
  CheckedRewrite capability f ->
  Either (RewriteError capability f) (DerivedRewriteBody capability f)
composeDerivedBody leftBody rightRewrite = do
  compositionResult <-
    first RewriteCompositionFailure
      ( composePatternRewrites
          (derivedRewriteAlgebra leftBody)
          (checkedRewriteAlgebra rightRewrite)
      )
  composedVariables <-
    first RewriteVariableMetadataFailure
      ( transportComposedVariables
          compositionResult
          (derivedRewriteVariables leftBody)
          (checkedRewriteVariables rightRewrite)
      )
  Right
    DerivedRewriteBody
      { derivedRewriteAlgebra = crRewrite compositionResult,
        derivedRewriteVariables = composedVariables
      }

transportComposedVariables ::
  (Language f, Ord capability) =>
  CompositionResult RuleOrigin (LogicalDecoration capability) f ->
  RuleVariables ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
transportComposedVariables compositionResult leftVariables rightVariables = do
  renamedRightVariables <-
    renameRuleVariables (crRightFreshening compositionResult) rightVariables
  projectedLeftVariables <-
    projectRuleVariables (crLeftProjection compositionResult) leftVariables
  projectedRightVariables <-
    projectRuleVariables (crRightProjection compositionResult) renamedRightVariables
  mergedVariables <-
    mergeRuleVariables projectedLeftVariables projectedRightVariables
  restrictRuleVariables
    (allPatternRewriteVariables (crRewrite compositionResult))
    mergedVariables

validateFreshDerivedName ::
  RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) ()
validateFreshDerivedName name checkedSystem =
  case lookupCheckedRewrite name checkedSystem of
    Nothing ->
      Right ()

    Just _ ->
      Left (RewriteDuplicateRuleName name)
