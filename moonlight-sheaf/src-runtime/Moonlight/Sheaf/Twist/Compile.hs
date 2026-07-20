{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Twist.Compile
  ( TwistCompilation (..),
    TwistCompileError (..),
    TwistCompileMismatch (..),
    compileContextualRulesWith,
    compileSupportedRuleBookWith,
    compileSupportedFactBookWith,
    insertCompiledRule,
    insertCompiledSupportedRule,
    insertCompiledSupportedFactRule,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( SupportIndexedRule (..),
  )
import Moonlight.Sheaf.Context.Core
  ( ClassSiteSupport,
  )
import Moonlight.Sheaf.Twist.FactClosure
  ( CompiledSupportedFactRule (..),
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedFactSpec (..),
    SupportedRuleBook,
    SupportedRuleSpec (..),
    supportedFactSpecs,
    supportedRules,
  )

type TwistCompilation :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data TwistCompilation owner site ctx rawRule compiledRule rawFact compiledFact compiledSupport err = TwistCompilation
  { tcCompileSupportProgram ::
      site ->
      SupportedRuleBook owner ctx rawRule ->
      SupportedFactBook owner ctx rawFact ->
      Either err compiledSupport
  }

type TwistCompileMismatch :: Type -> Type -> Type
data TwistCompileMismatch context ruleId
  = TwistContextualCompiledRuleMissing !context !ruleId
  | TwistSupportedCompiledRuleMissing !ruleId
  | TwistSupportedCompiledFactMissing !ruleId
  deriving stock (Eq, Ord, Show)

type TwistCompileError :: Type -> Type -> Type -> Type
data TwistCompileError context ruleId err
  = TwistCompileSourceError !err
  | TwistCompileMismatch !(TwistCompileMismatch context ruleId)
  deriving stock (Eq, Ord, Show)

compileContextualRulesWith ::
  (Ord ctx, Ord ruleId) =>
  (rawRule -> ruleId) ->
  ([rawRule] -> Either err [compiledRule]) ->
  (compiledRule -> ruleId) ->
  [(ctx, [rawRule])] ->
  Either (TwistCompileError ctx ruleId err) (Map ctx [compiledRule])
compileContextualRulesWith rawRuleId compileRules compiledRuleId rawRulesByContext = do
  compiledRules <-
    first TwistCompileSourceError (compileRules (Map.elems uniqueRawRules))
  let compiledRuleIndex =
        Map.fromList
          (fmap (\compiledRule -> (compiledRuleId compiledRule, compiledRule)) compiledRules)
  indexedRules <-
    traverse
      ( \(contextValue, rawRules) ->
          fmap
            ((,) contextValue)
            ( first
                TwistCompileMismatch
                (traverse (lookupContextualCompiledRule rawRuleId compiledRuleIndex contextValue) rawRules)
            )
      )
      rawRulesByContext
  pure (Map.fromList indexedRules)
  where
    uniqueRawRules =
      foldr
        ( \(_, rawRules) accumulatedRules ->
            foldr
              (\rawRule innerAcc -> Map.insert (rawRuleId rawRule) rawRule innerAcc)
              accumulatedRules
              rawRules
        )
        Map.empty
        rawRulesByContext

compileSupportedRuleBookWith ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  ([rawRule] -> Either err [compiledRule]) ->
  (compiledRule -> ruleId) ->
  SupportedRuleBook owner context rawRule ->
  Either (TwistCompileError context ruleId err) [SupportIndexedRule (ClassSiteSupport context) compiledRule]
compileSupportedRuleBookWith rawRuleId compileRules compiledRuleId supportedRuleBookValue = do
  compiledRules <-
    first TwistCompileSourceError (compileRules (Map.elems uniqueRawRules))
  let compiledRuleIndex =
        Map.fromList
          (fmap (\compiledRule -> (compiledRuleId compiledRule, compiledRule)) compiledRules)
  first
    TwistCompileMismatch
    (traverse (lookupCompiledSupportedRule rawRuleId compiledRuleIndex) (supportedRules supportedRuleBookValue))
  where
    uniqueRawRules =
      foldr
        (\supportedRuleSpec accumulatedRules -> Map.insert (rawRuleId (srsRule supportedRuleSpec)) (srsRule supportedRuleSpec) accumulatedRules)
        Map.empty
        (supportedRules supportedRuleBookValue)

compileSupportedFactBookWith ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  ([rawRule] -> Either err [compiledRule]) ->
  (compiledRule -> ruleId) ->
  SupportedFactBook owner context rawRule ->
  Either (TwistCompileError context ruleId err) [CompiledSupportedFactRule (ClassSiteSupport context) compiledRule]
compileSupportedFactBookWith rawRuleId compileRules compiledRuleId supportedFactBookValue = do
  compiledRules <-
    first TwistCompileSourceError (compileRules (Map.elems uniqueRawRules))
  let compiledRuleIndex =
        Map.fromList
          (fmap (\compiledRule -> (compiledRuleId compiledRule, compiledRule)) compiledRules)
  first
    TwistCompileMismatch
    (traverse (lookupCompiledSupportedFactRule rawRuleId compiledRuleIndex) (supportedFactSpecs supportedFactBookValue))
  where
    uniqueRawRules =
      foldr
        (\supportedFactSpec accumulatedRules -> Map.insert (rawRuleId (sfsRule supportedFactSpec)) (sfsRule supportedFactSpec) accumulatedRules)
        Map.empty
        (supportedFactSpecs supportedFactBookValue)

insertCompiledRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  context ->
  rawRule ->
  [compiledRule] ->
  Either (TwistCompileMismatch context ruleId) [compiledRule]
insertCompiledRule rawRuleId compiledRuleIndex contextValue rawRule compiledRules =
  fmap
    (: compiledRules)
    (lookupContextualCompiledRule rawRuleId compiledRuleIndex contextValue rawRule)

insertCompiledSupportedRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  SupportedRuleSpec context rawRule ->
  [SupportIndexedRule (ClassSiteSupport context) compiledRule] ->
  Either (TwistCompileMismatch context ruleId) [SupportIndexedRule (ClassSiteSupport context) compiledRule]
insertCompiledSupportedRule rawRuleId compiledRuleIndex supportedRuleSpec accumulatedRules =
  fmap
    ( \compiledRule ->
        SupportIndexedRule
          { sirSupport = srsSupport supportedRuleSpec,
            sirRule = compiledRule
          }
          : accumulatedRules
    )
    (lookupCompiledRule rawRuleId compiledRuleIndex TwistSupportedCompiledRuleMissing (srsRule supportedRuleSpec))

insertCompiledSupportedFactRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  SupportedFactSpec context rawRule ->
  [CompiledSupportedFactRule (ClassSiteSupport context) compiledRule] ->
  Either (TwistCompileMismatch context ruleId) [CompiledSupportedFactRule (ClassSiteSupport context) compiledRule]
insertCompiledSupportedFactRule rawRuleId compiledRuleIndex supportedFactSpec accumulatedRules =
  fmap
    ( \compiledRule ->
        CompiledSupportedFactRule
          { csfrSupport = sfsSupport supportedFactSpec,
            csfrRule = compiledRule
          }
          : accumulatedRules
    )
    (lookupCompiledRule rawRuleId compiledRuleIndex TwistSupportedCompiledFactMissing (sfsRule supportedFactSpec))

lookupContextualCompiledRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  context ->
  rawRule ->
  Either (TwistCompileMismatch context ruleId) compiledRule
lookupContextualCompiledRule rawRuleId compiledRuleIndex contextValue =
  lookupCompiledRule
    rawRuleId
    compiledRuleIndex
    (TwistContextualCompiledRuleMissing contextValue)

lookupCompiledSupportedRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  SupportedRuleSpec context rawRule ->
  Either (TwistCompileMismatch context ruleId) (SupportIndexedRule (ClassSiteSupport context) compiledRule)
lookupCompiledSupportedRule rawRuleId compiledRuleIndex supportedRuleSpec =
  fmap
    ( \compiledRule ->
        SupportIndexedRule
          { sirSupport = srsSupport supportedRuleSpec,
            sirRule = compiledRule
          }
    )
    (lookupCompiledRule rawRuleId compiledRuleIndex TwistSupportedCompiledRuleMissing (srsRule supportedRuleSpec))

lookupCompiledSupportedFactRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  SupportedFactSpec context rawRule ->
  Either (TwistCompileMismatch context ruleId) (CompiledSupportedFactRule (ClassSiteSupport context) compiledRule)
lookupCompiledSupportedFactRule rawRuleId compiledRuleIndex supportedFactSpec =
  fmap
    ( \compiledRule ->
        CompiledSupportedFactRule
          { csfrSupport = sfsSupport supportedFactSpec,
            csfrRule = compiledRule
          }
    )
    (lookupCompiledRule rawRuleId compiledRuleIndex TwistSupportedCompiledFactMissing (sfsRule supportedFactSpec))

lookupCompiledRule ::
  Ord ruleId =>
  (rawRule -> ruleId) ->
  Map ruleId compiledRule ->
  (ruleId -> TwistCompileMismatch context ruleId) ->
  rawRule ->
  Either (TwistCompileMismatch context ruleId) compiledRule
lookupCompiledRule rawRuleId compiledRuleIndex missing rawRule =
  let ruleId =
        rawRuleId rawRule
   in maybe
        (Left (missing ruleId))
        Right
        (Map.lookup ruleId compiledRuleIndex)
