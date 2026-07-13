{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Typed rule-body DSL for the front stratum.
-- Owns context names, rule scopes, binders, guards, application-condition
-- extensions, and the 'RewriteGuardAtom' lowering class.
-- Contracts: term sorts are preserved by GADTs, contexts are unresolved names
-- until elaboration, and guard variables are checked downstream.
module Moonlight.Rewrite.DSL.Rule
  ( ContextName,
    ContextNameError (..),
    contextName,
    contextNameString,
    ContextRef (..),
    RuleScope (..),
    ruleScopeContextRefs,
    RuleBinder (..),
    RuleBinders,
    ruleBinderList,
    bindTypedVar,
    bind,
    RuleBody (..),
    ruleBodyScope,
    ruleBodyBinders,
    Extension (..),
    extension,
    rootExtension,
    globalExtension,
    whenExt_,
    ApplicationConditionDSL (..),
    Guard (..),
    NoGuardAtom,
    RewriteGuardAtom (..),
    (==>),
    (=:=),
    (=/=),
    atom_,
    not_,
    all_,
    any_,
    when_,
    requires_,
    forbids_,
    forall_,
    at,
    globally,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Void (Void, absurd)
import Data.Word (Word64)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    SrcLoc,
    callStack,
    getCallStack,
  )
import GHC.TypeLits (KnownSymbol)
import Moonlight.Constraint (ConstraintExpr)
import Moonlight.Core
  ( IdentifierToken,
    isValidIdentifier,
    mkIdentifierTokenWith,
    renderIdentifierToken,
  )
import Moonlight.Rewrite.Algebra
  ( PatternExtensionScope (..),
  )
import Moonlight.Rewrite.DSL.Signature (Node)
import Moonlight.Rewrite.DSL.Term
  ( SomeTypedVar (..),
    SymbolToken,
    Term,
    TypedVar,
    typedVar,
  )
import Moonlight.Rewrite.System qualified as CoreGuard

type ContextNameNamespace :: Type
data ContextNameNamespace

newtype ContextName = ContextName
  { unContextName :: IdentifierToken ContextNameNamespace
  }
  deriving stock (Eq, Ord, Show)

data ContextNameError
  = EmptyContextName
  | InvalidContextName
  deriving stock (Eq, Ord, Show, Read)

contextName :: String -> Either ContextNameError ContextName
contextName raw =
  case Text.strip (Text.pack raw) of
    normalized
      | Text.null normalized ->
        Left EmptyContextName

    normalized ->
      maybe
        (Left InvalidContextName)
        (Right . ContextName)
        (mkIdentifierTokenWith isValidContextPath normalized)

isValidContextPath :: Text.Text -> Bool
isValidContextPath =
  all isValidIdentifier . Text.splitOn (Text.pack "/")

contextNameString :: ContextName -> String
contextNameString =
  Text.unpack . renderIdentifierToken . unContextName

data ContextRef = ContextRef
  { crRawName :: !String,
    crCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Eq, Show)

data RuleScope
  = RuleGlobal
  | RuleContexts !(NonEmpty ContextRef)
  deriving stock (Eq, Show)

ruleScopeContextRefs :: RuleScope -> [ContextRef]
ruleScopeContextRefs scope =
  case scope of
    RuleGlobal ->
      []
    RuleContexts refs ->
      NonEmpty.toList refs

data RuleBinder = RuleBinder
  { rbTypedVar :: !SomeTypedVar,
    rbCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

newtype RuleBinders sig = RuleBinders
  { unRuleBinders :: Seq RuleBinder
  }
  deriving stock (Show)

instance Semigroup (RuleBinders sig) where
  RuleBinders left <> RuleBinders right =
    RuleBinders (left <> right)

instance Monoid (RuleBinders sig) where
  mempty =
    RuleBinders Seq.empty

ruleBinderList :: RuleBinders sig -> [RuleBinder]
ruleBinderList (RuleBinders binders) =
  Foldable.toList binders

bindTypedVar :: HasCallStack => TypedVar sort -> RuleBinders sig
bindTypedVar typedVariable =
  RuleBinders
    ( Seq.singleton
        RuleBinder
          { rbTypedVar = SomeTypedVar typedVariable,
            rbCallSite = currentSrcLoc callStack
          }
    )

bind ::
  (HasCallStack, KnownSymbol name, KnownSymbol sort) =>
  SymbolToken name ->
  SymbolToken sort ->
  RuleBinders sig
bind nameToken sortToken =
  bindTypedVar (typedVar nameToken sortToken)

data NoGuardAtom sig

class RewriteGuardAtom atom where
  type GuardCapabilityKey atom :: Type

  guardCapabilityDigest :: Proxy atom -> GuardCapabilityKey atom -> Word64

  lowerGuardAtom ::
    Applicative m =>
    (forall sort. Term sig sort -> m (CoreGuard.GuardTerm (Node sig))) ->
    atom sig ->
    m (ConstraintExpr (CoreGuard.GuardAtom (GuardCapabilityKey atom) (Node sig)))

instance RewriteGuardAtom NoGuardAtom where
  type GuardCapabilityKey NoGuardAtom = Void

  guardCapabilityDigest _ =
    absurd

  lowerGuardAtom _ atomValue =
    case atomValue of {}

data Guard sig atom where
  GuardEq ::
    Term sig sort ->
    Term sig sort ->
    Guard sig atom
  GuardAtom ::
    atom sig ->
    Guard sig atom
  GuardNot ::
    Guard sig atom ->
    Guard sig atom
  GuardAnd ::
    NonEmpty (Guard sig atom) ->
    Guard sig atom
  GuardOr ::
    NonEmpty (Guard sig atom) ->
    Guard sig atom

data Extension sig atom where
  Extension ::
    Term sig sort ->
    Seq (Guard sig atom) ->
    PatternExtensionScope ->
    Extension sig atom

extension :: Term sig sort -> Extension sig atom
extension termValue =
  Extension termValue Seq.empty ExtensionLocal

rootExtension :: Term sig sort -> Extension sig atom
rootExtension termValue =
  Extension termValue Seq.empty ExtensionRoot

globalExtension :: Term sig sort -> Extension sig atom
globalExtension termValue =
  Extension termValue Seq.empty ExtensionGlobal

whenExt_ ::
  Extension sig atom ->
  Guard sig atom ->
  Extension sig atom
whenExt_ (Extension termValue guards scope) guardValue =
  Extension termValue (guards |> guardValue) scope

data ApplicationConditionDSL sig atom
  = Requires !(Extension sig atom)
  | Forbids !(Extension sig atom)

data RuleBody sig atom where
  RuleBody ::
    RuleBinders sig ->
    Term sig sort ->
    Term sig sort ->
    Seq (Guard sig atom) ->
    Seq (ApplicationConditionDSL sig atom) ->
    RuleScope ->
    RuleBody sig atom

ruleBodyScope :: RuleBody sig atom -> RuleScope
ruleBodyScope (RuleBody _ _ _ _ _ scope) =
  scope

ruleBodyBinders :: RuleBody sig atom -> RuleBinders sig
ruleBodyBinders (RuleBody binders _ _ _ _ _) =
  binders

infix 1 ==>

(==>) :: Term sig sort -> Term sig sort -> RuleBody sig atom
leftTerm ==> rightTerm =
  RuleBody mempty leftTerm rightTerm Seq.empty Seq.empty RuleGlobal

infix 4 =:=

(=:=) :: Term sig sort -> Term sig sort -> Guard sig atom
leftTerm =:= rightTerm =
  GuardEq leftTerm rightTerm

infix 4 =/=

(=/=) :: Term sig sort -> Term sig sort -> Guard sig atom
leftTerm =/= rightTerm =
  GuardNot (GuardEq leftTerm rightTerm)

atom_ :: atom sig -> Guard sig atom
atom_ =
  GuardAtom

not_ :: Guard sig atom -> Guard sig atom
not_ =
  GuardNot

all_ :: NonEmpty (Guard sig atom) -> Guard sig atom
all_ =
  GuardAnd

any_ :: NonEmpty (Guard sig atom) -> Guard sig atom
any_ =
  GuardOr

when_ :: RuleBody sig atom -> Guard sig atom -> RuleBody sig atom
when_ (RuleBody binders leftTerm rightTerm guards applicationConditions scope) guardValue =
  RuleBody binders leftTerm rightTerm (guards |> guardValue) applicationConditions scope

requires_ ::
  RuleBody sig atom ->
  Extension sig atom ->
  RuleBody sig atom
requires_ (RuleBody binders leftTerm rightTerm guards applicationConditions scope) extensionValue =
  RuleBody binders leftTerm rightTerm guards (applicationConditions |> Requires extensionValue) scope

forbids_ ::
  RuleBody sig atom ->
  Extension sig atom ->
  RuleBody sig atom
forbids_ (RuleBody binders leftTerm rightTerm guards applicationConditions scope) extensionValue =
  RuleBody binders leftTerm rightTerm guards (applicationConditions |> Forbids extensionValue) scope

forall_ :: RuleBinders sig -> RuleBody sig atom -> RuleBody sig atom
forall_ binders (RuleBody existingBinders leftTerm rightTerm guards applicationConditions scope) =
  RuleBody (existingBinders <> binders) leftTerm rightTerm guards applicationConditions scope

at :: HasCallStack => String -> RuleBody sig atom -> RuleBody sig atom
at rawName (RuleBody binders leftTerm rightTerm guards applicationConditions scope) =
  RuleBody
    binders
    leftTerm
    rightTerm
    guards
    applicationConditions
    (appendRuleScopeContext (contextRefFromCallStack rawName callStack) scope)

globally :: RuleBody sig atom -> RuleBody sig atom
globally (RuleBody binders leftTerm rightTerm guards applicationConditions _) =
  RuleBody binders leftTerm rightTerm guards applicationConditions RuleGlobal

appendRuleScopeContext :: ContextRef -> RuleScope -> RuleScope
appendRuleScopeContext contextRef scope =
  case scope of
    RuleGlobal ->
      RuleContexts (contextRef :| [])
    RuleContexts refs ->
      RuleContexts (refs <> (contextRef :| []))

contextRefFromCallStack :: String -> CallStack -> ContextRef
contextRefFromCallStack rawName stack =
  ContextRef
    { crRawName = rawName,
      crCallSite = currentSrcLoc stack
    }

currentSrcLoc :: CallStack -> Maybe SrcLoc
currentSrcLoc stack =
  case getCallStack stack of
    [] ->
      Nothing
    (_, srcLoc) : _ ->
      Just srcLoc
