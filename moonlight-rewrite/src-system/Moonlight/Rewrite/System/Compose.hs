{-# LANGUAGE GHC2024 #-}

-- | Composition layer for checked rewrites.
-- Owns creation and insertion of derived named rewrites by composing existing
-- algebraic rewrites, with fresh-name checks and allocated ids.
-- Contracts: origin and decoration composition come from the kernel; unknown,
-- duplicate, and composition failures remain typed as 'RewriteError'.
module Moonlight.Rewrite.System.Compose
  ( compose,
    composeNamed,
    addComposedNamed,
    addComposedPathNamed,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Core
  ( HasConstructorTag,
    RewriteRuleId,
    ZipMatch,
  )
import Moonlight.Rewrite.Algebra
  ( CompositionResult (..),
    composePatternRewrites,
  )
import Moonlight.Rewrite.System.Check
  ( RewriteError (..),
    checkedSystemErrorToRewriteError,
    rewriteByRuleName,
  )
import Moonlight.Rewrite.System.Checked
  ( CheckedRewrite (..),
    CheckedSystem,
    allocateSystemRuleId,
    appendCheckedRewrite,
    lookupCheckedRewrite,
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )

compose ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RewriteRuleId ->
  RuleName ->
  CheckedRewrite capability f ->
  CheckedRewrite capability f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
compose newRewriteId newName leftRewrite rightRewrite = do
  compositionResult <-
    first RewriteCompositionFailure
      (composePatternRewrites (checkedRewriteAlgebra leftRewrite) (checkedRewriteAlgebra rightRewrite))

  Right
    CheckedRewrite
      { checkedRewriteId = newRewriteId,
        checkedRewriteName = newName,
        checkedRewriteAlgebra = crRewrite compositionResult
      }

composeNamed ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RuleName ->
  RuleName ->
  RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
composeNamed newName leftName rightName checkedSystem = do
  leftRewrite <-
    rewriteByRuleName leftName checkedSystem

  rightRewrite <-
    rewriteByRuleName rightName checkedSystem

  let (newRewriteId, _) =
        allocateSystemRuleId checkedSystem

  compose newRewriteId newName leftRewrite rightRewrite

addComposedNamed ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RuleName ->
  RuleName ->
  RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (CheckedSystem capability f)
addComposedNamed newName leftName rightName checkedSystem = do
  validateFreshDerivedName newName checkedSystem

  leftRewrite <-
    rewriteByRuleName leftName checkedSystem

  rightRewrite <-
    rewriteByRuleName rightName checkedSystem

  let (newRewriteId, nextCandidate) =
        allocateSystemRuleId checkedSystem

  composedRewrite <-
    compose newRewriteId newName leftRewrite rightRewrite

  first checkedSystemErrorToRewriteError
    (appendCheckedRewrite nextCandidate composedRewrite checkedSystem)

addComposedPathNamed ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RuleName ->
  NonEmpty RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (CheckedSystem capability f)
addComposedPathNamed newName pathNames checkedSystem = do
  validateFreshDerivedName newName checkedSystem

  let (newRewriteId, nextCandidate) =
        allocateSystemRuleId checkedSystem

  composedRewrite <-
    rewritePath newRewriteId newName pathNames checkedSystem

  first checkedSystemErrorToRewriteError
    (appendCheckedRewrite nextCandidate composedRewrite checkedSystem)

rewritePath ::
  (HasConstructorTag f, ZipMatch f, Ord capability) =>
  RewriteRuleId ->
  RuleName ->
  NonEmpty RuleName ->
  CheckedSystem capability f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
rewritePath newRewriteId newName (firstName :| remainingNames) checkedSystem = do
  firstRewrite <-
    renameDerivedRewrite newRewriteId newName
      <$> rewriteByRuleName firstName checkedSystem

  foldlM
    ( \currentRewrite nextName -> do
        nextRewrite <-
          rewriteByRuleName nextName checkedSystem
        compose newRewriteId newName currentRewrite nextRewrite
    )
    firstRewrite
    remainingNames

renameDerivedRewrite ::
  RewriteRuleId ->
  RuleName ->
  CheckedRewrite capability f ->
  CheckedRewrite capability f
renameDerivedRewrite rewriteRuleId name rewriteValue =
  rewriteValue
    { checkedRewriteId = rewriteRuleId,
      checkedRewriteName = name
    }

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
