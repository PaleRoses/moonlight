{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.DSL.Support
  ( supportIndexFromProgram,
    ruleNameFromSource,
    duplicateNameDetails,
  )
where

import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack
  ( SrcLoc,
  )
import Moonlight.Core
  ( duplicateValuesOn,
  )
import Moonlight.Rewrite.DSL.Error
  ( DuplicateNameDetail (..),
    ProgramError (..),
  )
import Moonlight.Rewrite.DSL.Program
  ( ContextDecl (..),
    MacroDecl (..),
    Program (..),
    RuleNameRef (..),
    macroPathRefs,
  )
import Moonlight.Rewrite.DSL.Rule
  ( ContextName,
    ContextRef (..),
    RuleScope (..),
    contextName,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    mkRuleName,
  )
import Moonlight.Rewrite.System
  ( RuleSupportIndex,
    mkRuleSupportIndex,
  )

supportIndexFromProgram ::
  Program sig atom ->
  Map RuleName RuleScope ->
  Set RuleName ->
  Either (ProgramError sig) (RuleSupportIndex ContextName)
supportIndexFromProgram sourceProgram ruleScopes knownRules = do
  declaredContexts <-
    declaredContextNames (Foldable.toList (pContexts sourceProgram))

  atomicSupport <-
    ruleSupportMapFromScopes
      declaredContexts
      ruleScopes

  supportByName <-
    Foldable.foldlM
      (addMacroRuleSupport declaredContexts)
      atomicSupport
      (Foldable.toList (pMacros sourceProgram))

  let materialized =
        materializeRuleSupport supportByName

  first ProgramSupportError
    ( mkRuleSupportIndex
        knownRules
        (mrsBaseSupport materialized)
        (mrsContextSupport materialized)
    )

data RuleSupport = RuleSupport
  { rsBase :: !Bool,
    rsContexts :: !(Set ContextName)
  }

data MaterializedRuleSupport = MaterializedRuleSupport
  { mrsBaseSupport :: !(Set RuleName),
    mrsContextSupport :: !(Map ContextName (Set RuleName))
  }

globalRuleSupport :: RuleSupport
globalRuleSupport =
  RuleSupport
    { rsBase = True,
      rsContexts = Set.empty
    }

contextRuleSupport :: Set ContextName -> RuleSupport
contextRuleSupport contexts =
  RuleSupport
    { rsBase = False,
      rsContexts = contexts
    }

normalizeRuleSupport :: RuleSupport -> RuleSupport
normalizeRuleSupport support
  | rsBase support =
      support {rsContexts = Set.empty}
  | otherwise =
      support

ruleSupportMapFromScopes ::
  Set ContextName ->
  Map RuleName RuleScope ->
  Either (ProgramError sig) (Map RuleName RuleSupport)
ruleSupportMapFromScopes declaredContexts =
  Map.traverseWithKey (ruleSupportFromScope declaredContexts)

ruleSupportFromScope ::
  Set ContextName ->
  RuleName ->
  RuleScope ->
  Either (ProgramError sig) RuleSupport
ruleSupportFromScope declaredContexts ruleNameValue scope =
  case scope of
    RuleGlobal ->
      Right globalRuleSupport

    RuleContexts contextRefs -> do
      contextNames <-
        traverse
          (resolveRuleContext declaredContexts ruleNameValue)
          (NonEmpty.toList contextRefs)
      Right (contextRuleSupport (Set.fromList contextNames))

resolveRuleContext ::
  Set ContextName ->
  RuleName ->
  ContextRef ->
  Either (ProgramError sig) ContextName
resolveRuleContext declaredContexts ruleNameValue contextRef =
  do
    contextNameValue <-
      contextNameFromSource (crRawName contextRef) (crCallSite contextRef)

    if Set.member contextNameValue declaredContexts
      then Right contextNameValue
      else
        Left
          ( ProgramUnknownRuleContext
              ruleNameValue
              (crRawName contextRef)
              (crCallSite contextRef)
          )

addMacroRuleSupport ::
  Set ContextName ->
  Map RuleName RuleSupport ->
  MacroDecl ->
  Either (ProgramError sig) (Map RuleName RuleSupport)
addMacroRuleSupport declaredContexts supportByName macroDecl = do
  macroName <-
    ruleNameFromSource (mdName macroDecl) (mdCallSite macroDecl)

  macroSupport <-
    macroPathSupport declaredContexts macroName supportByName (macroPathRefs (mdPath macroDecl))

  Right (Map.insert macroName macroSupport supportByName)

macroPathSupport ::
  Set ContextName ->
  RuleName ->
  Map RuleName RuleSupport ->
  NonEmpty RuleNameRef ->
  Either (ProgramError sig) RuleSupport
macroPathSupport declaredContexts macroName supportByName (firstRef :| remainingRefs) = do
  firstSupport <-
    resolveMacroInputSupport macroName supportByName firstRef

  remainingSupports <-
    traverse
      (resolveMacroInputSupport macroName supportByName)
      remainingRefs

  Right
    ( List.foldl'
        (meetRuleSupport declaredContexts)
        firstSupport
        remainingSupports
    )

resolveMacroInputSupport ::
  RuleName ->
  Map RuleName RuleSupport ->
  RuleNameRef ->
  Either (ProgramError sig) RuleSupport
resolveMacroInputSupport macroName supportByName ruleRef = do
  inputName <-
    ruleNameFromRef ruleRef

  case Map.lookup inputName supportByName of
    Nothing ->
      Left
        ( ProgramUnknownMacroInput
            macroName
            (rnrRawName ruleRef)
            (rnrCallSite ruleRef)
        )

    Just support ->
      Right support

meetRuleSupport ::
  Set ContextName ->
  RuleSupport ->
  RuleSupport ->
  RuleSupport
meetRuleSupport declaredContexts leftSupport rightSupport =
  normalizeRuleSupport
    RuleSupport
      { rsBase = rsBase leftSupport && rsBase rightSupport,
        rsContexts =
          Set.intersection
            (contextsVisibleToMeet declaredContexts leftSupport)
            (contextsVisibleToMeet declaredContexts rightSupport)
      }

contextsVisibleToMeet :: Set ContextName -> RuleSupport -> Set ContextName
contextsVisibleToMeet declaredContexts support
  | rsBase support =
      declaredContexts
  | otherwise =
      rsContexts support

materializeRuleSupport :: Map RuleName RuleSupport -> MaterializedRuleSupport
materializeRuleSupport supportByName =
  MaterializedRuleSupport
    { mrsBaseSupport =
        Map.keysSet (Map.filter rsBase supportByName),
      mrsContextSupport =
        Map.fromListWith
          Set.union
          [ (contextNameValue, Set.singleton ruleNameValue)
          | (ruleNameValue, support) <- Map.toAscList supportByName,
            contextNameValue <- Set.toAscList (rsContexts support)
          ]
    }

declaredContextNames ::
  [ContextDecl] ->
  Either (ProgramError sig) (Set ContextName)
declaredContextNames contexts = do
  namedContexts <-
    traverse declaredContextName contexts

  rejectDuplicateContextNames namedContexts

  Right (Set.fromList (fmap fst namedContexts))

declaredContextName ::
  ContextDecl ->
  Either (ProgramError sig) (ContextName, Maybe SrcLoc)
declaredContextName contextDecl = do
  name <-
    contextNameFromSource (cdName contextDecl) (cdCallSite contextDecl)

  Right (name, cdCallSite contextDecl)

contextNameFromSource ::
  String ->
  Maybe SrcLoc ->
  Either (ProgramError sig) ContextName
contextNameFromSource rawName callSite =
  first
    (\err -> ProgramContextNameError rawName err callSite)
    (contextName rawName)

ruleNameFromRef :: RuleNameRef -> Either (ProgramError sig) RuleName
ruleNameFromRef ruleRef =
  ruleNameFromSource (rnrRawName ruleRef) (rnrCallSite ruleRef)

ruleNameFromSource :: String -> Maybe SrcLoc -> Either (ProgramError sig) RuleName
ruleNameFromSource rawName callSite =
  first
    (\err -> ProgramRuleNameError rawName err callSite)
    (mkRuleName rawName)

rejectDuplicateContextNames :: [(ContextName, Maybe SrcLoc)] -> Either (ProgramError sig) ()
rejectDuplicateContextNames names =
  case duplicateNameDetails names of
    Nothing ->
      Right ()

    Just duplicates ->
      Left (ProgramDuplicateContextNames duplicates)

duplicateNameDetails ::
  Ord name =>
  [(name, Maybe SrcLoc)] ->
  Maybe (NonEmpty (DuplicateNameDetail name))
duplicateNameDetails =
  NonEmpty.nonEmpty
    . fmap
      ( \((name, firstSite), (_, secondSite)) ->
          DuplicateNameDetail
            { duplicateName = name,
              duplicateFirstSite = firstSite,
              duplicateSecondSite = secondSite
            }
      )
    . duplicateValuesOn fst
