{-# LANGUAGE StandaloneDeriving #-}

-- | Fact-rule compiler and derivation owner for logic closure.
-- Owns compiled pattern queries, projection validation, derivation indexes,
-- and the naive reference derivation used as the closure oracle.
-- Contracts: inputs live on canonical quotient facts and derivations
-- canonicalize witnesses/evidence at birth.
module Moonlight.Rewrite.System.Logic.Rule
  ( FactRuleId (..),
    RawFactRule (..),
    FactRule,
    CompiledFactRule,
    cfrId,
    cfrName,
    cfrQuery,
    cfrCompiledQuery,
    cfrProjection,
    cfrFactId,
    compiledFactRuleToRawFactRule,
    SemiNaiveMatcher,
    mkSemiNaiveMatcher,
    runSemiNaiveMatcher,
    FactDerivation (..),
    FactDerivationIndex,
    emptyFactDerivationIndex,
    nullFactDerivationIndex,
    factDerivationCount,
    lookupFactDerivations,
    singletonFactDerivationIndex,
    selectFactDerivations,
    differenceFactDerivationIndexes,
    factStoreFromDerivations,
    canonicalizeFactDerivationIndex,
    FactRuleCompileError (..),
    compileFactRule,
    compileFactRules,
    ruleConditionEvidence,
    factDerivationFor,
    deriveFactDerivations,
    deriveFacts,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (ClassId, Pattern, PatternVar, Substitution, patternVariables)
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
    GuardEvidence,
    GuardRef,
    GuardTerm,
    RewriteCondition,
    canonicalizeGuardEvidence,
    combineCompiledGuards,
    compileGuard,
    emptyGuardCapabilityResolver,
    evaluateCompiledGuardWithEvidenceAndCapabilities,
    guardRefTerm,
    guardRefVariables,
    GuardCapabilityResolver,
  )
import Moonlight.Rewrite.System.Logic.Delta
  ( differenceAlignedSetMap,
  )
import Moonlight.Rewrite.System.Logic.Store
  ( FactId,
    FactStore,
    FactWitness (..),
    FactTuple (..),
    canonicalizeFactStore,
    canonicalizeFactWitness,
    emptyFactStore,
    factWitnesses,
    insertFact,
    unionFactStores,
  )
import Moonlight.Rewrite.System.Logic.SemiNaive.Input
  ( SemiNaiveInput (..),
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqCondition,
    PatternQuery (..),
    cpqPrimaryPattern,
    compilePatternQuery,
    guardedPatternQuery,
    singlePatternQuery,
  )

type FactRuleId :: Type
newtype FactRuleId = FactRuleId
  { unFactRuleId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type RawFactRule :: Type -> Type -> (Type -> Type) -> Type
data RawFactRule guardRef guard f = FactRule
  { frId :: FactRuleId,
    frName :: String,
    frPattern :: Pattern f,
    frProjection :: [guardRef],
    frFactId :: FactId,
    frCondition :: Maybe guard
  }

type FactRule :: Type -> (Type -> Type) -> Type
type FactRule capability f = RawFactRule GuardRef (RewriteCondition capability f) f

type CompiledFactRule :: Type -> (Type -> Type) -> Type
data CompiledFactRule capability f = CompiledFactRule
  !FactRuleId
  !String
  !(PatternQuery (RewriteCondition capability f) f)
  !(CompiledPatternQuery (CompiledGuard capability f) f)
  ![GuardRef]
  !FactId

deriving stock instance
  ( Eq capability,
    forall a. Ord a => Ord (f a)
  ) =>
  Eq (CompiledFactRule capability f)

cfrId :: CompiledFactRule capability f -> FactRuleId
cfrId (CompiledFactRule ruleId _name _query _compiledQuery _projection _factId) =
  ruleId

cfrName :: CompiledFactRule capability f -> String
cfrName (CompiledFactRule _ruleId name _query _compiledQuery _projection _factId) =
  name

cfrQuery :: CompiledFactRule capability f -> PatternQuery (RewriteCondition capability f) f
cfrQuery (CompiledFactRule _ruleId _name query _compiledQuery _projection _factId) =
  query

cfrCompiledQuery ::
  CompiledFactRule capability f ->
  CompiledPatternQuery (CompiledGuard capability f) f
cfrCompiledQuery (CompiledFactRule _ruleId _name _query compiledQuery _projection _factId) =
  compiledQuery

cfrProjection :: CompiledFactRule capability f -> [GuardRef]
cfrProjection (CompiledFactRule _ruleId _name _query _compiledQuery projection _factId) =
  projection

cfrFactId :: CompiledFactRule capability f -> FactId
cfrFactId (CompiledFactRule _ruleId _name _query _compiledQuery _projection factId) =
  factId

compiledFactRuleToRawFactRule :: CompiledFactRule capability f -> FactRule capability f
compiledFactRuleToRawFactRule compiledFactRule =
  let (patternValue, conditionValue) =
        case cfrQuery compiledFactRule of
          SinglePatternQuery patternOnly ->
            (patternOnly, Nothing)
          GuardedPatternQuery (SinglePatternQuery patternOnly) conditionOnly ->
            (patternOnly, Just conditionOnly)
          GuardedPatternQuery _ conditionOnly ->
            (cpqPrimaryPattern (cfrCompiledQuery compiledFactRule), Just conditionOnly)
          ConjunctivePatternQuery _ ->
            (cpqPrimaryPattern (cfrCompiledQuery compiledFactRule), Nothing)
   in FactRule
        { frId = cfrId compiledFactRule,
          frName = cfrName compiledFactRule,
          frPattern = patternValue,
          frProjection = cfrProjection compiledFactRule,
          frFactId = cfrFactId compiledFactRule,
          frCondition = conditionValue
        }

type FactDerivation :: Type
data FactDerivation = FactDerivation
  { fdRuleId :: FactRuleId,
    fdRuleName :: String,
    fdFactWitness :: FactWitness,
    fdGuardEvidence :: Maybe GuardEvidence
  }
  deriving stock (Eq, Ord, Show, Read)

type FactDerivationIndex :: Type
newtype FactDerivationIndex = FactDerivationIndex
  { unFactDerivationIndex :: Map FactWitness (Set.Set FactDerivation)
  }
  deriving stock (Eq, Show)

instance Semigroup FactDerivationIndex where
  FactDerivationIndex leftIndex <> FactDerivationIndex rightIndex =
    FactDerivationIndex (Map.unionWith (<>) leftIndex rightIndex)

instance Monoid FactDerivationIndex where
  mempty =
    FactDerivationIndex Map.empty

emptyFactDerivationIndex :: FactDerivationIndex
emptyFactDerivationIndex =
  mempty

type SemiNaiveMatcher :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type -> Type
newtype SemiNaiveMatcher capability state root f host obstruction = SemiNaiveMatcher
  { runSemiNaiveMatcher ::
      state ->
      SemiNaiveInput FactStore FactDerivationIndex ->
      CompiledFactRule capability f ->
      host ->
      (state, Either obstruction [(root, Substitution)])
  }

mkSemiNaiveMatcher ::
  ( state ->
    SemiNaiveInput FactStore FactDerivationIndex ->
    CompiledFactRule capability f ->
    host ->
    (state, Either obstruction [(root, Substitution)])
  ) ->
  SemiNaiveMatcher capability state root f host obstruction
mkSemiNaiveMatcher =
  SemiNaiveMatcher


nullFactDerivationIndex :: FactDerivationIndex -> Bool
nullFactDerivationIndex =
  Map.null . unFactDerivationIndex

factDerivationCount :: FactDerivationIndex -> Int
factDerivationCount =
  Map.foldr ((+) . Set.size) 0 . unFactDerivationIndex

lookupFactDerivations :: FactWitness -> FactDerivationIndex -> Set.Set FactDerivation
lookupFactDerivations factWitness =
  Map.findWithDefault Set.empty factWitness . unFactDerivationIndex

singletonFactDerivationIndex :: FactDerivation -> FactDerivationIndex
singletonFactDerivationIndex factDerivation =
  FactDerivationIndex
    (Map.singleton (fdFactWitness factDerivation) (Set.singleton factDerivation))

selectFactDerivations :: FactStore -> FactDerivationIndex -> FactDerivationIndex
selectFactDerivations factStore (FactDerivationIndex derivationIndex) =
  FactDerivationIndex
    (Map.restrictKeys derivationIndex (factWitnesses factStore))

differenceFactDerivationIndexes ::
  FactDerivationIndex ->
  FactDerivationIndex ->
  FactDerivationIndex
differenceFactDerivationIndexes (FactDerivationIndex leftIndex) (FactDerivationIndex rightIndex) =
  FactDerivationIndex (differenceAlignedSetMap leftIndex rightIndex)

factStoreFromDerivations :: FactDerivationIndex -> FactStore
factStoreFromDerivations =
  foldr
    (\factWitness -> insertFact (fwFactId factWitness) (fwTuple factWitness))
    emptyFactStore
    . Map.keys
    . unFactDerivationIndex

canonicalizeFactDerivationIndex :: (ClassId -> ClassId) -> FactDerivationIndex -> FactDerivationIndex
canonicalizeFactDerivationIndex canonicalizeClassId (FactDerivationIndex derivationIndex) =
  FactDerivationIndex
    ( Map.fromListWith (<>)
        ( Map.toList derivationIndex
            >>= \(factWitness, derivations) ->
              [ ( canonicalizeFactWitness canonicalizeClassId factWitness,
                  Set.map (canonicalizeFactDerivation canonicalizeClassId) derivations
                )
              ]
        )
    )

canonicalizeFactDerivation :: (ClassId -> ClassId) -> FactDerivation -> FactDerivation
canonicalizeFactDerivation canonicalizeClassId factDerivation =
  factDerivation
    { fdFactWitness = canonicalizeFactWitness canonicalizeClassId (fdFactWitness factDerivation),
      fdGuardEvidence = canonicalizeGuardEvidence canonicalizeClassId <$> fdGuardEvidence factDerivation
    }

type FactRuleCompileError :: Type
data FactRuleCompileError
  = FactRuleProjectionIntroducesUnboundVars FactRuleId [PatternVar]
  | FactRuleGuardIntroducesUnboundVars FactRuleId [PatternVar]
  deriving stock (Eq, Show)

compileFactRule ::
  (Traversable f, forall a. Ord a => Ord (f a), Ord capability) =>
  FactRule capability f ->
  Either FactRuleCompileError (CompiledFactRule capability f)
compileFactRule factRule =
  let boundPatternVariables = patternVariables (frPattern factRule)
      unboundProjectionVariables =
        Set.toAscList
          (Set.difference (foldMap guardRefVariables (frProjection factRule)) boundPatternVariables)
      patternQuery =
        maybe
          (singlePatternQuery (frPattern factRule))
          (guardedPatternQuery (singlePatternQuery (frPattern factRule)))
          (frCondition factRule)
      compiledQuery =
        either
          (Left . FactRuleGuardIntroducesUnboundVars (frId factRule))
          Right
          (compilePatternQuery combineCompiledGuards compileGuard patternQuery)
   in if null unboundProjectionVariables
        then
          fmap
            (\query ->
               CompiledFactRule
                 (frId factRule)
                 (frName factRule)
                 patternQuery
                 query
                 (frProjection factRule)
                 (frFactId factRule)
            )
            compiledQuery
        else Left (FactRuleProjectionIntroducesUnboundVars (frId factRule) unboundProjectionVariables)

compileFactRules ::
  (Traversable f, forall a. Ord a => Ord (f a), Ord capability) =>
  [FactRule capability f] ->
  Either FactRuleCompileError [CompiledFactRule capability f]
compileFactRules =
  traverse compileFactRule

deriveFacts ::
  FactStore ->
  (CompiledFactRule capability f -> host -> [(root, Substitution)]) ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  [CompiledFactRule capability f] ->
  host ->
  FactStore
deriveFacts initialFacts matcher resolveTerm canonicalizeClassId compiledRules host =
  let canonicalFacts =
        canonicalizeFactStore canonicalizeClassId initialFacts
   in unionFactStores canonicalFacts
        (factStoreFromDerivations (deriveFactDerivations canonicalFacts matcher resolveTerm canonicalizeClassId compiledRules host))

deriveFactDerivations ::
  FactStore ->
  (CompiledFactRule capability f -> host -> [(root, Substitution)]) ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  [CompiledFactRule capability f] ->
  host ->
  FactDerivationIndex
deriveFactDerivations factStore matcher resolveTerm canonicalizeClassId compiledRules host =
  foldMap
    (deriveRuleFactDerivations emptyGuardCapabilityResolver matcher resolveTerm canonicalizeClassId (canonicalizeFactStore canonicalizeClassId factStore) host)
    compiledRules

deriveRuleFactDerivations ::
  GuardCapabilityResolver capability ->
  (CompiledFactRule capability f -> host -> [(root, Substitution)]) ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  FactStore ->
  host ->
  CompiledFactRule capability f ->
  FactDerivationIndex
deriveRuleFactDerivations capabilityResolver matcher resolveTerm canonicalizeClassId factStore host compiledRule =
  foldMap
    (\(rootValue, substitution) ->
       maybe
         emptyFactDerivationIndex
         singletonFactDerivationIndex
         (factDerivationFor capabilityResolver resolveTerm canonicalizeClassId compiledRule factStore rootValue substitution)
    )
    (matcher compiledRule host)

factDerivationFor ::
  GuardCapabilityResolver capability ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  CompiledFactRule capability f ->
  FactStore ->
  root ->
  Substitution ->
  Maybe FactDerivation
factDerivationFor capabilityResolver resolveTerm canonicalizeClassId compiledRule factStore rootValue substitution = do
  guardEvidence <- ruleConditionEvidence capabilityResolver resolveTerm canonicalizeClassId compiledRule factStore rootValue substitution
  factTuple <-
    FactTuple
      <$> traverse
        (\guardRef -> canonicalizeClassId <$> resolveTerm rootValue substitution (guardRefTerm guardRef))
        (cfrProjection compiledRule)
  pure
    FactDerivation
      { fdRuleId = cfrId compiledRule,
        fdRuleName = cfrName compiledRule,
        fdFactWitness = FactWitness (cfrFactId compiledRule) factTuple,
        fdGuardEvidence = guardEvidence
      }

ruleConditionEvidence ::
  GuardCapabilityResolver capability ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  CompiledFactRule capability f ->
  FactStore ->
  root ->
  Substitution ->
  Maybe (Maybe GuardEvidence)
ruleConditionEvidence capabilityResolver resolveTerm canonicalizeClassId compiledRule factStore rootValue substitution =
  case cpqCondition (cfrCompiledQuery compiledRule) of
    Nothing ->
      Just Nothing
    Just compiledGuard ->
      Just
        <$> evaluateCompiledGuardWithEvidenceAndCapabilities
          factStore
          capabilityResolver
          canonicalizeClassId
          (resolveTerm rootValue substitution)
          compiledGuard
