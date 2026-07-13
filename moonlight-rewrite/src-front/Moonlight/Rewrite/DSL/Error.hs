{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

-- | Typed error vocabulary for front-stratum program elaboration.
-- Owns duplicate-name details, variable-scope and sort-conflict reports,
-- wrapped system errors, support errors, and source-location rendering.
-- Contract: pretty rendering explains the typed obstruction; it does not
-- erase which validation boundary failed.
module Moonlight.Rewrite.DSL.Error
  ( DuplicateNameDetail (..),
    ProgramVariableSortConflictDetail (..),
    ProgramUnknownVariableDetail (..),
    ProgramUnusedVariableDetail (..),
    SomeRewriteError (..),
    ProgramError (..),
    prettyProgramError,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Stack (SrcLoc, srcLocFile, srcLocStartCol, srcLocStartLine)
import Moonlight.Core (Pattern)
import Moonlight.Rewrite.DSL.Rule
  ( ContextName,
    ContextNameError (..),
    contextNameString,
  )
import Moonlight.Rewrite.DSL.Signature (Node, NodeTag, RewriteSignature)
import Moonlight.Rewrite.DSL.Term (SortName, sortNameString)
import Moonlight.Rewrite.System
  ( RewriteError,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    RuleNameError,
  )
import Moonlight.Rewrite.System qualified as RuleName
import Moonlight.Rewrite.System
  ( RuleSupportIndexError,
  )

data SomeRewriteError f where
  SomeRewriteError :: RewriteError capability f -> SomeRewriteError f

deriving stock instance Show (Pattern f) => Show (SomeRewriteError f)

data DuplicateNameDetail name = DuplicateNameDetail
  { duplicateName :: !name,
    duplicateFirstSite :: !(Maybe SrcLoc),
    duplicateSecondSite :: !(Maybe SrcLoc)
  }
  deriving stock (Eq, Show)

data ProgramVariableSortConflictDetail = ProgramVariableSortConflictDetail
  { pvcRule :: !String,
    pvcVariable :: !String,
    pvcFirstSort :: !SortName,
    pvcSecondSort :: !SortName,
    pvcCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

data ProgramUnknownVariableDetail = ProgramUnknownVariableDetail
  { puvRule :: !String,
    puvVariable :: !String,
    puvSort :: !SortName,
    puvCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

data ProgramUnusedVariableDetail = ProgramUnusedVariableDetail
  { puuvRule :: !String,
    puuvVariable :: !String,
    puuvSort :: !SortName,
    puuvCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

data ProgramError sig
  = ProgramRuleNameError !String !RuleNameError !(Maybe SrcLoc)
  | ProgramDuplicateRuleNames !(NonEmpty (DuplicateNameDetail RuleName))
  | ProgramDuplicateContextNames !(NonEmpty (DuplicateNameDetail ContextName))
  | ProgramContextNameError !String !ContextNameError !(Maybe SrcLoc)
  | ProgramDuplicateRuleBinders !(NonEmpty (DuplicateNameDetail String))
  | ProgramUnknownRuleContext !RuleName !String !(Maybe SrcLoc)
  | ProgramUnknownMacroInput !RuleName !String !(Maybe SrcLoc)
  | ProgramUnknownVariable !ProgramUnknownVariableDetail
  | ProgramUnusedVariables !(NonEmpty ProgramUnusedVariableDetail)
  | ProgramVariableSortConflict !ProgramVariableSortConflictDetail
  | ProgramRewriteError !(SomeRewriteError (Node sig))
  | ProgramSupportError !(RuleSupportIndexError ContextName)

deriving stock instance (RewriteSignature sig, Show (NodeTag sig)) => Show (ProgramError sig)

prettyProgramError ::
  (RewriteSignature sig, Show (NodeTag sig)) =>
  ProgramError sig ->
  String
prettyProgramError programError =
  case programError of
    ProgramRuleNameError rawName err callSite ->
      unlines
        ( [ "rewrite rule name " <> show rawName <> " is invalid",
            "reason: " <> show err
          ]
            <> siteLines callSite
        )
    ProgramDuplicateRuleNames details ->
      unlines
        ( "duplicate rewrite rule names:"
            : duplicateLines RuleName.ruleNameString details
        )
    ProgramDuplicateContextNames details ->
      unlines
        ( "duplicate rewrite context names:"
            : duplicateLines contextNameString details
        )
    ProgramContextNameError rawName err callSite ->
      unlines
        ( [ "rewrite context name " <> show rawName <> " is invalid",
            "reason: " <> show err
          ]
            <> siteLines callSite
        )
    ProgramDuplicateRuleBinders details ->
      unlines
        ( "duplicate rewrite rule binders:"
            : duplicateLines id details
        )
    ProgramUnknownRuleContext ruleName rawContext callSite ->
      unlines
        ( [ "rewrite rule "
              <> show (RuleName.ruleNameString ruleName)
              <> " references unknown context "
              <> show rawContext
          ]
            <> siteLines callSite
        )
    ProgramUnknownMacroInput macroName rawName callSite ->
      unlines
        ( [ "rewrite macro " <> show (RuleName.ruleNameString macroName) <> " references unknown input " <> show rawName
          ]
            <> siteLines callSite
        )
    ProgramUnknownVariable detail ->
      unlines
        ( [ "rewrite rule " <> show (puvRule detail) <> ": variable " <> puvVariable detail <> " is not declared",
            "sort: " <> sortNameString (puvSort detail)
          ]
            <> siteLines (puvCallSite detail)
        )
    ProgramUnusedVariables details ->
      unlines
        ( "unused rewrite rule binders:"
            : fmap unusedVariableLine (NonEmpty.toList details)
        )
    ProgramVariableSortConflict detail ->
      unlines
        ( [ "rewrite rule " <> show (pvcRule detail) <> ": variable " <> pvcVariable detail <> " used at conflicting sorts",
            "first sort:  " <> sortNameString (pvcFirstSort detail),
            "second sort: " <> sortNameString (pvcSecondSort detail)
          ]
            <> siteLines (pvcCallSite detail)
        )
    ProgramRewriteError (SomeRewriteError err) ->
      "canonical rewrite error: " <> show err
    ProgramSupportError err ->
      "rewrite support error: " <> show err

duplicateLines ::
  (name -> String) ->
  NonEmpty (DuplicateNameDetail name) ->
  [String]
duplicateLines renderName =
  fmap duplicateLine . NonEmpty.toList
  where
    duplicateLine detail =
      "- "
        <> renderName (duplicateName detail)
        <> siteSummary (duplicateFirstSite detail) (duplicateSecondSite detail)

siteSummary :: Maybe SrcLoc -> Maybe SrcLoc -> String
siteSummary firstSite secondSite =
  case (firstSite, secondSite) of
    (Nothing, Nothing) ->
      ""
    _ ->
      " first: " <> renderMaybeSite firstSite <> " second: " <> renderMaybeSite secondSite

siteLines :: Maybe SrcLoc -> [String]
siteLines callSite =
  case callSite of
    Nothing ->
      []
    Just srcLoc ->
      ["call site: " <> renderSrcLoc srcLoc]

renderMaybeSite :: Maybe SrcLoc -> String
renderMaybeSite =
  maybe "<unknown>" renderSrcLoc

renderSrcLoc :: SrcLoc -> String
renderSrcLoc srcLoc =
  srcLocFile srcLoc
    <> ":"
    <> show (srcLocStartLine srcLoc)
    <> ":"
    <> show (srcLocStartCol srcLoc)

unusedVariableLine :: ProgramUnusedVariableDetail -> String
unusedVariableLine detail =
  "- "
    <> puuvVariable detail
    <> " : "
    <> sortNameString (puuvSort detail)
    <> maybe "" ((" at " <>) . renderSrcLoc) (puuvCallSite detail)
