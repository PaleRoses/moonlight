{-# LANGUAGE GHC2024 #-}

-- | Contextual support model for proof-aware rewrite strata.
-- Owns support-basis re-exports plus supported rewrites, fact rules, fact
-- stores, derivation indexes, and match witnesses indexed by context.
-- Contracts: filtering asks the finite lattice whether a support contains a
-- context, and global fact rules are supported from the lattice bottom.
module Moonlight.Rewrite.ContextualSupport
  ( SupportBasis,
    supportBasis,
    emptySupport,
    supportGenerators,
    principalSupport,
    supportContains,
    supportReachableContexts,
    normalizeSupport,
    supportUnion,
    supportMeet,
    SupportedRewrite (..),
    ContextualSupportModel (..),
    contextualSupportModel,
    contextualSupportModelRewrites,
    contextualSupportModelRewritesAt,
    supportForAtomicRewriteId,
    SupportedFactRule (..),
    SupportedFactFamily (..),
    supportedFactFamily,
    supportedFactFamilyRules,
    supportedFactFamilyRulesAt,
    globalSupportedFactFamily,
    SupportedFactStore (..),
    emptySupportedFactStore,
    singletonSupportedFactStore,
    supportForFactWitness,
    supportedFactStoreAt,
    SupportedFactDerivationIndex (..),
    emptySupportedFactDerivationIndex,
    singletonSupportedFactDerivationIndex,
    supportForFactDerivation,
    supportedFactDerivationIndexAt,
    SupportMatchWitness (..),
    SupportedRewriteMatch (..),
  )
where

import Data.Foldable (find)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad (filterM)
import Moonlight.Core (ClassId, RewriteRuleId)
import Moonlight.Core (GuideEvidence)
import Moonlight.Rewrite.System (CompiledGuard, GuardEvidence)
import Moonlight.Rewrite.System
  ( FactDerivation,
    FactDerivationIndex,
    FactRule,
    singletonFactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactStore,
    FactWitness (..),
    emptyFactStore,
    insertFact,
  )
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch)
import Moonlight.Rewrite.Algebra
  ( PatternRewrite,
    prOrigin,
  )
import Moonlight.Rewrite.System
  ( RuleOrigin (..),
    rewriteOriginRuleIds,
  )
import Moonlight.FiniteLattice
  ( ContextLattice (clBottom),
    ContextLatticeLookupError,
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    emptySupport,
    normalizeSupport,
    principalSupport,
    supportBasis,
    supportContains,
    supportGenerators,
    supportMeet,
    supportReachableContexts,
    supportUnion
  )

type SupportedRewrite :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data SupportedRewrite c dec f = SupportedRewrite
  { srSupport :: SupportBasis c,
    srRewrite :: PatternRewrite RuleOrigin dec f
  }

type ContextualSupportModel :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype ContextualSupportModel c dec f = ContextualSupportModel
  { unContextualSupportModel :: [SupportedRewrite c dec f]
  }

instance Semigroup (ContextualSupportModel c dec f) where
  ContextualSupportModel leftRewrites <> ContextualSupportModel rightRewrites =
    ContextualSupportModel (leftRewrites <> rightRewrites)

instance Monoid (ContextualSupportModel c dec f) where
  mempty =
    ContextualSupportModel []

type SupportedFactRule :: Type -> Type -> (Type -> Type) -> Type
data SupportedFactRule c capability f = SupportedFactRule
  { sfrSupport :: SupportBasis c,
    sfrRule :: FactRule capability f
  }

type SupportedFactFamily :: Type -> Type -> (Type -> Type) -> Type
newtype SupportedFactFamily c capability f = SupportedFactFamily
  { unSupportedFactFamily :: [SupportedFactRule c capability f]
  }

instance Semigroup (SupportedFactFamily c capability f) where
  SupportedFactFamily leftRules <> SupportedFactFamily rightRules =
    SupportedFactFamily (leftRules <> rightRules)

instance Monoid (SupportedFactFamily c capability f) where
  mempty = SupportedFactFamily []

type SupportMatchWitness :: (Type -> Type) -> Type
data SupportMatchWitness f = SupportMatchWitness
  { smwFactStore :: FactStore,
    smwFactDerivations :: Set FactDerivation,
    smwGuardEvidence :: Maybe GuardEvidence,
    smwGuideEvidence :: Maybe (GuideEvidence ClassId)
  }

type SupportedRewriteMatch :: Type -> Type -> (Type -> Type) -> Type
data SupportedRewriteMatch c capability f = SupportedRewriteMatch
  { srmMatch :: ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f,
    srmSupport :: SupportBasis c,
    srmWitnesses :: Map c (SupportMatchWitness f)
  }

contextualSupportModel :: [SupportedRewrite c dec f] -> ContextualSupportModel c dec f
contextualSupportModel =
  ContextualSupportModel

supportContainsKnown :: Ord c => ContextLattice c -> SupportBasis c -> c -> Either (ContextLatticeLookupError c) Bool
supportContainsKnown contextLatticeValue supportValue contextValue =
  supportContains contextLatticeValue supportValue contextValue


contextualSupportModelRewrites :: ContextualSupportModel c dec f -> [SupportedRewrite c dec f]
contextualSupportModelRewrites =
  unContextualSupportModel

contextualSupportModelRewritesAt ::
  Ord c =>
  ContextLattice c ->
  c ->
  ContextualSupportModel c dec f ->
  Either (ContextLatticeLookupError c) [PatternRewrite RuleOrigin dec f]
contextualSupportModelRewritesAt contextLatticeValue contextValue =
  fmap (fmap srRewrite)
    . filterM
      (\supportedRewrite -> supportContainsKnown contextLatticeValue (srSupport supportedRewrite) contextValue)
    . unContextualSupportModel

supportForAtomicRewriteId :: RewriteRuleId -> ContextualSupportModel c dec f -> Maybe (SupportBasis c)
supportForAtomicRewriteId rewriteRuleId =
  fmap srSupport
    . find
      ( \supportedRewrite ->
          rewriteRuleId
            `Set.member` rewriteOriginRuleIds (prOrigin (srRewrite supportedRewrite))
      )
    . unContextualSupportModel

supportedFactFamily :: [SupportedFactRule c capability f] -> SupportedFactFamily c capability f
supportedFactFamily =
  SupportedFactFamily

supportedFactFamilyRules :: SupportedFactFamily c capability f -> [SupportedFactRule c capability f]
supportedFactFamilyRules =
  unSupportedFactFamily

supportedFactFamilyRulesAt ::
  Ord c =>
  ContextLattice c ->
  c ->
  SupportedFactFamily c capability f ->
  Either (ContextLatticeLookupError c) [FactRule capability f]
supportedFactFamilyRulesAt contextLatticeValue contextValue =
  fmap (fmap sfrRule)
    . filterM
      (\supportedRule -> supportContainsKnown contextLatticeValue (sfrSupport supportedRule) contextValue)
    . unSupportedFactFamily

globalSupportedFactFamily :: ContextLattice c -> [FactRule capability f] -> SupportedFactFamily c capability f
globalSupportedFactFamily contextLatticeValue =
  SupportedFactFamily
    . fmap
      (\factRule -> SupportedFactRule (principalSupport (clBottom contextLatticeValue)) factRule)

type SupportedFactStore :: Type -> Type
newtype SupportedFactStore c = SupportedFactStore
  { unSupportedFactStore :: Map FactWitness (SupportBasis c)
  }
  deriving stock (Eq, Show)

deriving stock instance Read (SupportBasis c) => Read (SupportedFactStore c)

emptySupportedFactStore :: SupportedFactStore c
emptySupportedFactStore =
  SupportedFactStore Map.empty

singletonSupportedFactStore :: FactWitness -> SupportBasis c -> SupportedFactStore c
singletonSupportedFactStore factWitness supportValue =
  SupportedFactStore (Map.singleton factWitness supportValue)

supportForFactWitness :: FactWitness -> SupportedFactStore c -> Maybe (SupportBasis c)
supportForFactWitness factWitness =
  Map.lookup factWitness . unSupportedFactStore

supportedFactStoreAt :: Ord c => ContextLattice c -> c -> SupportedFactStore c -> Either (ContextLatticeLookupError c) FactStore
supportedFactStoreAt contextLatticeValue contextValue =
  fmap
    ( foldr
        (\factWitness -> insertFact (fwFactId factWitness) (fwTuple factWitness))
        emptyFactStore
        . fmap fst
    )
    . filterM
      (\(_factWitness, supportValue) -> supportContainsKnown contextLatticeValue supportValue contextValue)
    . Map.toAscList
    . unSupportedFactStore

type SupportedFactDerivationIndex :: Type -> Type
newtype SupportedFactDerivationIndex c = SupportedFactDerivationIndex
  { unSupportedFactDerivationIndex :: Map FactDerivation (SupportBasis c)
  }
  deriving stock (Eq, Show)

deriving stock instance Read (SupportBasis c) => Read (SupportedFactDerivationIndex c)

emptySupportedFactDerivationIndex :: SupportedFactDerivationIndex c
emptySupportedFactDerivationIndex =
  SupportedFactDerivationIndex Map.empty

singletonSupportedFactDerivationIndex :: FactDerivation -> SupportBasis c -> SupportedFactDerivationIndex c
singletonSupportedFactDerivationIndex factDerivation supportValue =
  SupportedFactDerivationIndex (Map.singleton factDerivation supportValue)

supportForFactDerivation :: FactDerivation -> SupportedFactDerivationIndex c -> Maybe (SupportBasis c)
supportForFactDerivation factDerivation =
  Map.lookup factDerivation . unSupportedFactDerivationIndex

supportedFactDerivationIndexAt :: Ord c => ContextLattice c -> c -> SupportedFactDerivationIndex c -> Either (ContextLatticeLookupError c) FactDerivationIndex
supportedFactDerivationIndexAt contextLatticeValue contextValue =
  fmap
    ( foldMap
        singletonFactDerivationIndex
        . fmap fst
    )
    . filterM
      (\(_derivation, supportValue) -> supportContainsKnown contextLatticeValue supportValue contextValue)
    . Map.toAscList
    . unSupportedFactDerivationIndex
