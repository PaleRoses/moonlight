{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedRuleSpec (..),
    SupportedRuleBook,
    supportedRuleBook,
    supportedRules,
    specsActiveAt,
    rulesActiveAt,
    SupportedFactSpec (..),
    SupportedFactBook,
    supportedFactBook,
    supportedFactSpecs,
    factSpecsActiveAt,
    factRulesActiveAt,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Context.Core
  ( ClassSiteSupport,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
    SupportCarrier,
    contextObjectKeyFor,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
  )

type SupportedRuleSpec :: Type -> Type -> Type
data SupportedRuleSpec context rule = SupportedRuleSpec
  { srsSupport :: !(ClassSiteSupport context),
    srsRule :: !rule
  }
  deriving stock (Eq, Ord, Show)

type SupportedRuleBook :: Type -> Type -> Type
data SupportedRuleBook context rule = SupportedRuleBook
  { srbRules :: ![SupportedRuleSpec context rule],
    srbCarriers :: ![(SupportedRuleSpec context rule, SupportCarrier context)]
  }
  deriving stock (Eq, Ord, Show)

instance Ord context => Semigroup (SupportedRuleBook context rule) where
  leftBook <> rightBook =
    SupportedRuleBook
      { srbRules = srbRules leftBook <> srbRules rightBook,
        srbCarriers = srbCarriers leftBook <> srbCarriers rightBook
      }

instance Ord context => Monoid (SupportedRuleBook context rule) where
  mempty =
    SupportedRuleBook
      { srbRules = mempty,
        srbCarriers = mempty
      }

supportedRuleBook ::
  Ord context =>
  PreparedContextSite context ->
  [SupportedRuleSpec context rule] ->
  Either (PreparedContextSupportError context) (SupportedRuleBook context rule)
supportedRuleBook site ruleSpecs =
  SupportedRuleBook ruleSpecs <$> prepareSupportedEntries site srsSupport ruleSpecs

supportedRules :: SupportedRuleBook context rule -> [SupportedRuleSpec context rule]
supportedRules =
  srbRules

specsActiveAt ::
  Ord context =>
  PreparedContextSite context ->
  context ->
  SupportedRuleBook context rule ->
  Either (PreparedContextSupportError context) [SupportedRuleSpec context rule]
specsActiveAt site contextValue ruleBookValue = do
  contextKey <- contextObjectKeyFor site contextValue
  pure
    [ ruleSpec
    | (ruleSpec, supportCarrier) <- srbCarriers ruleBookValue,
      supportCarrierContainsKey site supportCarrier contextKey
    ]

rulesActiveAt ::
  Ord context =>
  PreparedContextSite context ->
  context ->
  SupportedRuleBook context rule ->
  Either (PreparedContextSupportError context) [rule]
rulesActiveAt site contextValue =
  fmap (fmap srsRule) . specsActiveAt site contextValue

type SupportedFactSpec :: Type -> Type -> Type
data SupportedFactSpec context rule = SupportedFactSpec
  { sfsSupport :: !(ClassSiteSupport context),
    sfsRule :: !rule
  }
  deriving stock (Eq, Ord, Show)

type SupportedFactBook :: Type -> Type -> Type
data SupportedFactBook context rule = SupportedFactBook
  { sfbFacts :: ![SupportedFactSpec context rule],
    sfbCarriers :: ![(SupportedFactSpec context rule, SupportCarrier context)]
  }
  deriving stock (Eq, Ord, Show)

instance Ord context => Semigroup (SupportedFactBook context rule) where
  leftBook <> rightBook =
    SupportedFactBook
      { sfbFacts = sfbFacts leftBook <> sfbFacts rightBook,
        sfbCarriers = sfbCarriers leftBook <> sfbCarriers rightBook
      }

instance Ord context => Monoid (SupportedFactBook context rule) where
  mempty =
    SupportedFactBook
      { sfbFacts = mempty,
        sfbCarriers = mempty
      }

supportedFactBook ::
  Ord context =>
  PreparedContextSite context ->
  [SupportedFactSpec context rule] ->
  Either (PreparedContextSupportError context) (SupportedFactBook context rule)
supportedFactBook site factSpecs =
  SupportedFactBook factSpecs <$> prepareSupportedEntries site sfsSupport factSpecs

supportedFactSpecs :: SupportedFactBook context rule -> [SupportedFactSpec context rule]
supportedFactSpecs =
  sfbFacts

factSpecsActiveAt ::
  Ord context =>
  PreparedContextSite context ->
  context ->
  SupportedFactBook context rule ->
  Either (PreparedContextSupportError context) [SupportedFactSpec context rule]
factSpecsActiveAt site contextValue factBookValue = do
  contextKey <- contextObjectKeyFor site contextValue
  pure
    [ factSpec
    | (factSpec, supportCarrier) <- sfbCarriers factBookValue,
      supportCarrierContainsKey site supportCarrier contextKey
    ]

factRulesActiveAt ::
  Ord context =>
  PreparedContextSite context ->
  context ->
  SupportedFactBook context rule ->
  Either (PreparedContextSupportError context) [rule]
factRulesActiveAt site contextValue =
  fmap (fmap sfsRule) . factSpecsActiveAt site contextValue

prepareSupportedEntries ::
  Ord context =>
  PreparedContextSite context ->
  (entry -> ClassSiteSupport context) ->
  [entry] ->
  Either (PreparedContextSupportError context) [(entry, SupportCarrier context)]
prepareSupportedEntries site supportFor =
  traverse
    ( \entry ->
        fmap
          ((,) entry)
          (supportCarrierFromSupport site (supportFor entry))
    )
{-# INLINE prepareSupportedEntries #-}
