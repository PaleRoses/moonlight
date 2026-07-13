{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Rewrite.DSL.Elaborate
  ( RuleVariables,
    ruleVariableMap,
    CanonicalProgram (..),
    elaborateProgram,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import GHC.Stack (SrcLoc)
import Moonlight.Constraint (ConstraintExpr (..))
import Moonlight.Core (Pattern (..), PatternVar, ZipMatch)
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Algebra
  ( ApplicationCondition,
    PatternExtension,
    andApplicationConditions,
    forbidsExtension,
    patternExtensionWithScope,
    requiresExtension,
  )
import Moonlight.Rewrite.Algebra
  ( guardedPatternQuery,
    singlePatternQuery,
  )
import Moonlight.Rewrite.DSL.Error
  ( DuplicateNameDetail (..),
    ProgramError (..),
    SomeRewriteError (..),
    ProgramUnknownVariableDetail (..),
    ProgramUnusedVariableDetail (..),
    ProgramVariableSortConflictDetail (..),
  )
import Moonlight.Rewrite.DSL.Support
  ( duplicateNameDetails,
    ruleNameFromSource,
    supportIndexFromProgram,
  )
import Moonlight.Rewrite.DSL.Program
  ( ContextName,
    MacroDecl (..),
    Program (..),
    RuleDecl (..),
    RuleNameRef (..),
    macroPathRefs,
  )
import Moonlight.Rewrite.DSL.Rule
  ( ApplicationConditionDSL (..),
    Extension (..),
    Guard (..),
    RewriteGuardAtom (..),
    RuleBinder (..),
    RuleBinders,
    RuleBody (..),
    RuleScope,
    ruleBinderList,
    ruleBodyBinders,
    ruleBodyScope,
  )
import Moonlight.Rewrite.DSL.Signature (K (..), Node (..), RewriteSignature (..), htraverse)
import Moonlight.Rewrite.DSL.Term
  ( SomeTypedVar (..),
    Term (..),
    TypedVar,
    someTypedVarName,
    someTypedVarSort,
    typedVarName,
    typedVarSort,
  )
import Moonlight.Rewrite.System qualified as Rewrite
import Moonlight.Rewrite.System
  ( GuardBase (..),
    GuardAtom (..),
    GuardPath (..),
    GuardRef (..),
    GuardTerm (..),
    RewriteCondition (..),
  )
import Moonlight.Rewrite.System (RuleName)
import Moonlight.Rewrite.System
  ( RuleSupportIndex,
  )

data RuleVariables = RuleVariables
  { rvPatternVariables :: !(Map PatternVar SomeTypedVar)
  }
  deriving stock (Eq, Show)

ruleVariableMap :: RuleVariables -> Map PatternVar SomeTypedVar
ruleVariableMap =
  rvPatternVariables

data CanonicalProgram sig atom = CanonicalProgram
  { canonicalSourceProgram :: !(Program sig atom),
    canonicalRuleSet :: !(Rewrite.RuleSet (GuardCapabilityKey atom) (Node sig)),
    canonicalCheckedSystem :: !(Rewrite.CheckedSystem (GuardCapabilityKey atom) (Node sig)),
    canonicalRuleVariables :: !(Map RuleName RuleVariables),
    canonicalRuleScopes :: !(Map RuleName RuleScope),
    canonicalSupportIndex :: !(RuleSupportIndex ContextName)
  }

data BinderBinding = BinderBinding
  { bbPatternVar :: !PatternVar,
    bbTypedVar :: !SomeTypedVar,
    bbCallSite :: !(Maybe SrcLoc)
  }

data RuleElabState = RuleElabState
  { resBindersByName :: !(Map String BinderBinding),
    resUsedPatternVars :: !(Set.Set PatternVar)
  }

type RuleElab sig a = StateT RuleElabState (Either (ProgramError sig)) a

elaborateProgram ::
  (RewriteSignature sig, ZipMatch (Node sig), RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  Program sig atom ->
  Either (ProgramError sig) (CanonicalProgram sig atom)
elaborateProgram sourceProgram = do
  elaboratedRules <-
    traverse elaborateRuleDecl (Foldable.toList (pRules sourceProgram))
  macroNames <-
    traverse macroRuleName (Foldable.toList (pMacros sourceProgram))
  rejectDuplicateRuleNames
    ( fmap
        (\elaboratedRule -> (elaboratedRuleName elaboratedRule, elaboratedRuleCallSite elaboratedRule))
        elaboratedRules
        <> fmap
          (\(macroDecl, macroName) -> (macroName, mdCallSite macroDecl))
          macroNames
    )
  let ruleSetValue =
        Rewrite.ruleSet (fmap elaboratedRuleSpec elaboratedRules)
      ruleVariables =
        Map.fromList
          [ (elaboratedRuleName elaboratedRule, elaboratedRuleVariables elaboratedRule)
            | elaboratedRule <- elaboratedRules
          ]
      ruleScopes =
        Map.fromList
          [ (elaboratedRuleName elaboratedRule, elaboratedRuleScope elaboratedRule)
            | elaboratedRule <- elaboratedRules
          ]
  checkedSystem <-
    first (ProgramRewriteError . SomeRewriteError) (Rewrite.checkRuleSet ruleSetValue)
  checkedWithMacros <-
    Foldable.foldlM
      addMacro
      checkedSystem
      macroNames
  supportIndex <-
    supportIndexFromProgram
      sourceProgram
      ruleScopes
      (Set.fromList (Rewrite.checkedRuleNames checkedWithMacros))
  pure
    CanonicalProgram
      { canonicalSourceProgram = sourceProgram,
        canonicalRuleSet = ruleSetValue,
        canonicalCheckedSystem = checkedWithMacros,
        canonicalRuleVariables = ruleVariables,
        canonicalRuleScopes = ruleScopes,
        canonicalSupportIndex = supportIndex
      }

elaborateTerm ::
  RewriteSignature sig =>
  String ->
  Maybe SrcLoc ->
  Term sig sort ->
  RuleElab sig (Pattern (Node sig))
elaborateTerm ruleName callSite term =
  case term of
    TVar typedVariable ->
      PatternVar <$> internTypedVar ruleName callSite typedVariable
    TNode sigNode -> do
      erasedNode <-
        htraverse
          (fmap K . elaborateTerm ruleName callSite)
          sigNode
      pure (PatternNode (Node erasedNode))

data ElaboratedRule sig atom = ElaboratedRule
  { elaboratedRuleName :: !RuleName,
    elaboratedRuleCallSite :: !(Maybe SrcLoc),
    elaboratedRuleSpec :: !(Rewrite.RuleSpec (GuardCapabilityKey atom) (Node sig)),
    elaboratedRuleVariables :: !RuleVariables,
    elaboratedRuleScope :: !RuleScope
  }

elaborateRuleDecl ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RuleDecl sig atom ->
  Either (ProgramError sig) (ElaboratedRule sig atom)
elaborateRuleDecl ruleDecl = do
  ruleNameValue <-
    ruleNameFromSource (rdName ruleDecl) (rdCallSite ruleDecl)
  initialState <-
    ruleElabStateFromBinders (rdName ruleDecl) (ruleBodyBinders (rdBody ruleDecl))
  (ruleSpec, finalState) <-
    runStateT
      (elaborateRuleBody (rdName ruleDecl) (rdCallSite ruleDecl) ruleNameValue (rdBody ruleDecl))
      initialState
  rejectUnusedRuleBinders (rdName ruleDecl) finalState
  pure
    ElaboratedRule
      { elaboratedRuleName = ruleNameValue,
        elaboratedRuleCallSite = rdCallSite ruleDecl,
        elaboratedRuleSpec = ruleSpec,
        elaboratedRuleVariables = ruleVariablesFromState finalState,
        elaboratedRuleScope = ruleBodyScope (rdBody ruleDecl)
      }

elaborateRuleBody ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  String ->
  Maybe SrcLoc ->
  RuleName ->
  RuleBody sig atom ->
  RuleElab sig (Rewrite.RuleSpec (GuardCapabilityKey atom) (Node sig))
elaborateRuleBody rawRuleName callSite ruleNameValue (RuleBody _ leftTerm rightTerm guards applicationConditions _) = do
  leftPattern <-
    elaborateTerm rawRuleName callSite leftTerm
  rightPattern <-
    elaborateTerm rawRuleName callSite rightTerm
  conditions <-
    traverse (elaborateGuard rawRuleName callSite) (Foldable.toList guards)
  elaboratedApplicationConditions <-
    traverse
      (elaborateApplicationCondition rawRuleName callSite)
      (Foldable.toList applicationConditions)
  let baseRule =
        Rewrite.rule ruleNameValue leftPattern rightPattern
      guardedRule =
        maybe
          baseRule
          (`Rewrite.when_` baseRule)
          (combineRewriteConditions conditions)
      conditionedRule =
        maybe
          guardedRule
          (`Rewrite.withApplicationCondition` guardedRule)
          (combineApplicationConditions elaboratedApplicationConditions)
  pure conditionedRule

elaborateApplicationCondition ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  String ->
  Maybe SrcLoc ->
  ApplicationConditionDSL sig atom ->
  RuleElab sig (ApplicationCondition (RewriteCondition (GuardCapabilityKey atom) (Node sig)) (Node sig))
elaborateApplicationCondition rawRuleName callSite =
  \case
    Requires extensionValue ->
      requiresExtension <$> elaborateExtension rawRuleName callSite extensionValue

    Forbids extensionValue ->
      forbidsExtension <$> elaborateExtension rawRuleName callSite extensionValue

elaborateExtension ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  String ->
  Maybe SrcLoc ->
  Extension sig atom ->
  RuleElab sig (PatternExtension (RewriteCondition (GuardCapabilityKey atom) (Node sig)) (Node sig))
elaborateExtension rawRuleName callSite (Extension termValue guards scope) = do
  extensionPattern <-
    elaborateTerm rawRuleName callSite termValue

  extensionConditions <-
    traverse (elaborateGuard rawRuleName callSite) (Foldable.toList guards)

  let baseQuery =
        singlePatternQuery extensionPattern
      guardedQuery =
        maybe
          baseQuery
          (guardedPatternQuery baseQuery)
          (combineRewriteConditions extensionConditions)

  pure (patternExtensionWithScope scope guardedQuery)

elaborateGuard ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom)) =>
  String ->
  Maybe SrcLoc ->
  Guard sig atom ->
  RuleElab sig (RewriteCondition (GuardCapabilityKey atom) (Node sig))
elaborateGuard rawRuleName callSite guard =
  RewriteCondition <$> elaborateGuardExpr rawRuleName callSite guard

elaborateGuardExpr ::
  (RewriteSignature sig, RewriteGuardAtom atom, Ord (GuardCapabilityKey atom)) =>
  String ->
  Maybe SrcLoc ->
  Guard sig atom ->
  RuleElab sig (ConstraintExpr (GuardAtom (GuardCapabilityKey atom) (Node sig)))
elaborateGuardExpr rawRuleName callSite guard =
  case guard of
    GuardEq leftTerm rightTerm ->
      elaborateGuardEquality rawRuleName callSite leftTerm rightTerm
    GuardAtom atomValue ->
      lowerGuardAtom (elaborateGuardTerm rawRuleName callSite) atomValue
    GuardNot childGuard ->
      Not <$> elaborateGuardExpr rawRuleName callSite childGuard
    GuardAnd childGuards ->
      And <$> traverse (elaborateGuardExpr rawRuleName callSite) (NonEmpty.toList childGuards)
    GuardOr childGuards ->
      Or <$> traverse (elaborateGuardExpr rawRuleName callSite) (NonEmpty.toList childGuards)

elaborateGuardEquality ::
  RewriteSignature sig =>
  String ->
  Maybe SrcLoc ->
  Term sig sort ->
  Term sig sort ->
  RuleElab sig (ConstraintExpr (GuardAtom capability (Node sig)))
elaborateGuardEquality rawRuleName callSite leftTerm rightTerm = do
  leftGuardTerm <-
    elaborateGuardTerm rawRuleName callSite leftTerm
  rightGuardTerm <-
    elaborateGuardTerm rawRuleName callSite rightTerm
  pure (Atom (ClassesEquivalent leftGuardTerm rightGuardTerm))

elaborateGuardTerm ::
  RewriteSignature sig =>
  String ->
  Maybe SrcLoc ->
  Term sig sort ->
  RuleElab sig (GuardTerm (Node sig))
elaborateGuardTerm rawRuleName callSite term =
  case term of
    TVar typedVariable ->
      GuardRefTerm . guardRefFromPatternVar <$> internTypedVar rawRuleName callSite typedVariable
    TNode sigNode -> do
      erasedNode <-
        htraverse
          (fmap K . elaborateGuardTerm rawRuleName callSite)
          sigNode
      pure (GuardNodeTerm (Node erasedNode))

internTypedVar ::
  String ->
  Maybe SrcLoc ->
  TypedVar sort ->
  RuleElab sig PatternVar
internTypedVar rawRuleName callSite typedVariable = do
  state <- get
  case Map.lookup (typedVarName typedVariable) (resBindersByName state) of
    Nothing ->
      lift
        ( Left
            ( ProgramUnknownVariable
                ProgramUnknownVariableDetail
                  { puvRule = rawRuleName,
                    puvVariable = typedVarName typedVariable,
                    puvSort = typedVarSort typedVariable,
                    puvCallSite = callSite
                  }
            )
        )
    Just binding
      | someTypedVarSort (bbTypedVar binding) /= typedVarSort typedVariable ->
          lift
            ( Left
                ( ProgramVariableSortConflict
                    ProgramVariableSortConflictDetail
                      { pvcRule = rawRuleName,
                        pvcVariable = typedVarName typedVariable,
                        pvcFirstSort = someTypedVarSort (bbTypedVar binding),
                        pvcSecondSort = typedVarSort typedVariable,
                        pvcCallSite = callSite
                      }
                )
            )
      | otherwise -> do
          modify'
            ( \currentState ->
                currentState
                  { resUsedPatternVars =
                      Set.insert (bbPatternVar binding) (resUsedPatternVars currentState)
                  }
            )
          pure (bbPatternVar binding)

ruleElabStateFromBinders ::
  String ->
  RuleBinders sig ->
  Either (ProgramError sig) RuleElabState
ruleElabStateFromBinders rawRuleName binders = do
  validatedBinders <-
    validateRuleBinders rawRuleName (ruleBinderList binders)
  pure
    RuleElabState
      { resBindersByName = binderBindingsByName validatedBinders,
        resUsedPatternVars = Set.empty
      }

validateRuleBinders ::
  String ->
  [RuleBinder] ->
  Either (ProgramError sig) [RuleBinder]
validateRuleBinders rawRuleName binders = do
  accumulator <-
    Foldable.foldlM
      (observeRuleBinder rawRuleName)
      emptyBinderValidation
      binders
  case NonEmpty.nonEmpty (reverse (bvaDuplicates accumulator)) of
    Nothing ->
      Right binders
    Just duplicates ->
      Left (ProgramDuplicateRuleBinders duplicates)

binderBindingsByName :: [RuleBinder] -> Map String BinderBinding
binderBindingsByName binders =
  Map.fromList
    [ (someTypedVarName (bbTypedVar binding), binding)
      | binding <- bindings
    ]
  where
    (_, bindings) =
      mapAccumL assignBinder 0 binders

    assignBinder nextKey binder =
      let patternVariable = EGraph.mkPatternVar nextKey
       in ( nextKey + 1,
            BinderBinding
              { bbPatternVar = patternVariable,
                bbTypedVar = rbTypedVar binder,
                bbCallSite = rbCallSite binder
              }
          )

ruleVariablesFromState :: RuleElabState -> RuleVariables
ruleVariablesFromState state =
  RuleVariables
    { rvPatternVariables =
        Map.fromList
          [ (bbPatternVar binding, bbTypedVar binding)
            | binding <- Map.elems (resBindersByName state)
          ]
    }

rejectUnusedRuleBinders :: String -> RuleElabState -> Either (ProgramError sig) ()
rejectUnusedRuleBinders rawRuleName state =
  case NonEmpty.nonEmpty unusedDetails of
    Nothing ->
      Right ()
    Just details ->
      Left (ProgramUnusedVariables details)
  where
    unusedDetails =
      fmap
        (\binding ->
           ProgramUnusedVariableDetail
             { puuvRule = rawRuleName,
               puuvVariable = someTypedVarName (bbTypedVar binding),
               puuvSort = someTypedVarSort (bbTypedVar binding),
               puuvCallSite = bbCallSite binding
             }
        )
        ( List.filter
            (\binding -> not (Set.member (bbPatternVar binding) (resUsedPatternVars state)))
            (List.sortOn bbPatternVar (Map.elems (resBindersByName state)))
        )

data BinderValidation = BinderValidation
  { bvaSeen :: !(Map String RuleBinder),
    bvaDuplicates :: ![DuplicateNameDetail String]
  }

emptyBinderValidation :: BinderValidation
emptyBinderValidation =
  BinderValidation
    { bvaSeen = Map.empty,
      bvaDuplicates = []
    }

observeRuleBinder ::
  String ->
  BinderValidation ->
  RuleBinder ->
  Either (ProgramError sig) BinderValidation
observeRuleBinder rawRuleName accumulator binder =
  case Map.lookup binderName (bvaSeen accumulator) of
    Nothing ->
      Right
        accumulator
          { bvaSeen = Map.insert binderName binder (bvaSeen accumulator)
          }
    Just existingBinder
      | someTypedVarSort (rbTypedVar existingBinder) /= someTypedVarSort (rbTypedVar binder) ->
          Left
            ( ProgramVariableSortConflict
                ProgramVariableSortConflictDetail
                  { pvcRule = rawRuleName,
                    pvcVariable = binderName,
                    pvcFirstSort = someTypedVarSort (rbTypedVar existingBinder),
                    pvcSecondSort = someTypedVarSort (rbTypedVar binder),
                    pvcCallSite = rbCallSite binder
                  }
            )
      | otherwise ->
          Right
            accumulator
              { bvaDuplicates =
                  DuplicateNameDetail
                    { duplicateName = binderName,
                      duplicateFirstSite = rbCallSite existingBinder,
                      duplicateSecondSite = rbCallSite binder
                    }
                    : bvaDuplicates accumulator
              }
  where
    binderName =
      someTypedVarName (rbTypedVar binder)

addMacro ::
  (RewriteSignature sig, ZipMatch (Node sig), Ord capability, Ord (NodeTag sig)) =>
  Rewrite.CheckedSystem capability (Node sig) ->
  (MacroDecl, RuleName) ->
  Either (ProgramError sig) (Rewrite.CheckedSystem capability (Node sig))
addMacro checkedSystem (macroDecl, macroName) = do
  pathNames <-
    traverse
      (resolveMacroInput macroName checkedSystem)
      (macroPathRefs (mdPath macroDecl))
  first
    (ProgramRewriteError . SomeRewriteError)
    (Rewrite.addComposedPathNamed macroName pathNames checkedSystem)

resolveMacroInput ::
  RuleName ->
  Rewrite.CheckedSystem capability (Node sig) ->
  RuleNameRef ->
  Either (ProgramError sig) RuleName
resolveMacroInput macroName checkedSystem ruleRef = do
  inputName <-
    ruleNameFromSource (rnrRawName ruleRef) (rnrCallSite ruleRef)
  case Rewrite.rewriteByRuleName inputName checkedSystem of
    Left _ ->
      Left (ProgramUnknownMacroInput macroName (rnrRawName ruleRef) (rnrCallSite ruleRef))
    Right _ ->
      Right inputName

macroRuleName :: MacroDecl -> Either (ProgramError sig) (MacroDecl, RuleName)
macroRuleName macroDecl = do
  name <-
    ruleNameFromSource (mdName macroDecl) (mdCallSite macroDecl)
  pure (macroDecl, name)

rejectDuplicateRuleNames :: [(RuleName, Maybe SrcLoc)] -> Either (ProgramError sig) ()
rejectDuplicateRuleNames names =
  case duplicateNameDetails names of
    Nothing ->
      Right ()
    Just duplicates ->
      Left (ProgramDuplicateRuleNames duplicates)

combineRewriteConditions ::
  (Ord capability, Ord (GuardTerm f)) =>
  [RewriteCondition capability f] ->
  Maybe (RewriteCondition capability f)
combineRewriteConditions =
  foldr combineRewriteCondition Nothing

combineRewriteCondition ::
  (Ord capability, Ord (GuardTerm f)) =>
  RewriteCondition capability f ->
  Maybe (RewriteCondition capability f) ->
  Maybe (RewriteCondition capability f)
combineRewriteCondition condition Nothing =
  Just condition
combineRewriteCondition condition (Just accumulated) =
  Just (condition <> accumulated)

combineApplicationConditions ::
  [ApplicationCondition guard f] ->
  Maybe (ApplicationCondition guard f)
combineApplicationConditions =
  fmap (andApplicationConditions . NonEmpty.toList) . NonEmpty.nonEmpty

guardRefFromPatternVar :: PatternVar -> GuardRef
guardRefFromPatternVar patternVariable =
  GuardRef (GuardFromVar patternVariable, GuardPath [])
