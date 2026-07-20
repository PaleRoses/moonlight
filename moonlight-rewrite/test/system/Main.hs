{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Constraint
  ( CNF,
    ConstraintExpr (..),
    Literal,
    literalPolarity,
    literalVariable,
    normalize,
    toCNF,
  )
import Moonlight.Core
  ( ClassId (..),
    BinderId (..),
    HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    RewriteRuleId (..),
    Substitution,
    emptySubstitution,
    insertSubst,
    lookupSubst,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Pale.Test.Site.Assertion (expectRight, expectSome)
import Moonlight.Rewrite.Algebra
  ( PatternProjection (..),
  )
import Moonlight.Rewrite.Runtime
  ( BinderSubstAlgebra (..),
    PostMatchSubst (..),
    PostMatchTerm (..),
    applyPostMatchSubst,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    CompiledGuard,
    FactClosureRun (..),
    FactClosureRunError,
    FactDerivation (..),
    FactDerivationIndex,
    FactId (..),
    FactRule,
    FactRuleId (..),
    FactStore,
    FactTuple (..),
    FactWitness (..),
    GuardCapabilityResolver (..),
    GuardAtom (..),
    GuardBase (..),
    GuardClauseEvidence (..),
    GuardEvidence,
    GuardExpr,
    GuardLiteralEvidence (..),
    GuardPath (..),
    GuardRef (..),
    data GuardVar,
    GuardTerm (..),
    RawFactRule (..),
    RawRewriteRule (..),
    RewriteCondition (..),
    RewriteError (..),
    RuleName,
    RuleNameError (..),
    RuleSet,
    SemiNaiveClosure (..),
    canonicalizeFactDerivationIndex,
    canonicalizeFactStore,
    cfrId,
    cfrName,
    checkRawRewriteSystem,
    checkRuleSet,
    checkedRuleNames,
    combineCompiledGuards,
    compileFactRule,
    compileFactRules,
    compileGuard,
    compiledGuardClauses,
    compiledGuardCanonicalNodeWordsWith,
    compiledGuardCanonicalWordsWith,
    compiledGuardDigestWith,
    compiledGuardNormalizedExpression,
    defaultSemiNaiveConfig,
    deriveFactDerivations,
    deriveSeededFactClosureWithStateAndConfig,
    differenceFactDerivationIndexes,
    emptyFactDerivationIndex,
    emptyGuardCapabilityResolver,
    emptyFactStore,
    evaluateCompiledGuardWithEvidenceAndCapabilities,
    factDerivationCount,
    factStoreFromDerivations,
    factStoreSize,
    geClauses,
    geFactWitnesses,
    guardEquivalent,
    guardChildIndex,
    guardChildIndexValue,
    guardEvidenceFromClauses,
    guardHasCapability,
    guardHasFact,
    guardHasFactTerms,
    hasFact,
    insertFact,
    insertFacts,
    mkRuleName,
    mkSemiNaiveMatcher,
    nullFactDerivationIndex,
    nullFactStore,
    projectPostMatchSubst,
    rewriteByRuleName,
    ruleNameString,
    ruleSet,
    ruleWithId,
    selectFactDerivations,
    singletonFactDerivationIndex,
    unionFactStores,
  )
import Test.Tasty.HUnit (assertEqual)

data Capability
  = NeedsSurface
  | NeedsOtherSurface
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  assertEqual "rule names trim at the system boundary" (Right "alpha") (fmap ruleNameString (mkRuleName " alpha "))
  assertEqual "rule names reject empty input" (Left EmptyRuleName) (mkRuleName "  ")
  assertEqual "rule names reject invalid identifiers" (Left InvalidRuleName) (mkRuleName "bad rule")
  assertEqual "rule names accept dotted identifier paths" (Right "assoc.forward") (fmap ruleNameString (mkRuleName "assoc.forward"))
  assertEqual "rule names reject empty path segments" (Left InvalidRuleName) (mkRuleName "assoc..forward")
  assertEqual "rule names reject leading dots" (Left InvalidRuleName) (mkRuleName ".forward")
  assertEqual "rule names reject trailing dots" (Left InvalidRuleName) (mkRuleName "assoc.")
  assertGuardCompileAndCapabilityEval
  assertGuardCanonicalWordsInvariances
  assertNestedGuardCanonicalWordsAndDigest
  assertGuardEvidenceRecordsEverySatisfiedLiteral
  assertGuardEvidenceMarksUnresolvedNegatedLiterals
  assertCanonicalWordsInjectiveForGuardRefs
  assertProjectedPostMatchTermGroundsRecursively
  assertRawRuleSyntheticNameUsesRuleIdKey
  assertDuplicateRuleIdsReject
  assertMissingRuleReportsTypedError
  assertFactStoresRejectEmptyBuckets
  assertFactDerivationIndexIsProvenanceOnly
  assertChainClosureMatchesOncePerRule
  assertClosureQuotientInvariant
  assertAliasSeededDerivationsProduceNoPhantomRounds
  assertGatedClosureMatchesNaiveOracle
  assertGuardSoundnessOnQuotient
  assertPlanCompilationFaithfulness
  assertSemiNaiveMatchCompleteness

assertFactStoresRejectEmptyBuckets :: IO ()
assertFactStoresRejectEmptyBuckets = do
  let storeAfterEmptyInsertion =
        insertFacts (FactId 90) Set.empty emptyFactStore
  assertEqual "empty fact insertion is identity" emptyFactStore storeAfterEmptyInsertion
  assertEqual "empty fact store remains null" True (nullFactStore storeAfterEmptyInsertion)
  assertEqual "empty fact store has zero tuples" 0 (factStoreSize storeAfterEmptyInsertion)

assertFactDerivationIndexIsProvenanceOnly :: IO ()
assertFactDerivationIndexIsProvenanceOnly = do
  let witnessedFact =
        FactWitness (FactId 91) (FactTuple [ClassId 1])
      unwitnessedFact =
        FactWitness (FactId 92) (FactTuple [ClassId 2])
      requestedFacts =
        insertFact
          (fwFactId unwitnessedFact)
          (fwTuple unwitnessedFact)
          (insertFact (fwFactId witnessedFact) (fwTuple witnessedFact) emptyFactStore)
      derivation =
        FactDerivation
          { fdRuleId = FactRuleId 91,
            fdRuleName = "provenance-only",
            fdFactWitness = witnessedFact,
            fdGuardEvidence = Nothing
          }
      sourceIndex =
        singletonFactDerivationIndex derivation
      selectedIndex =
        selectFactDerivations requestedFacts sourceIndex
      missingSelection =
        selectFactDerivations requestedFacts emptyFactDerivationIndex

  assertEqual "selection retains only actual derivations" 1 (factDerivationCount selectedIndex)
  assertEqual
    "derivation-to-fact conversion uses derivation-bearing witnesses only"
    (insertFact (fwFactId witnessedFact) (fwTuple witnessedFact) emptyFactStore)
    (factStoreFromDerivations selectedIndex)
  assertEqual "selection cannot mint empty provenance buckets" True (nullFactDerivationIndex missingSelection)
  assertEqual "missing provenance cannot mint facts" emptyFactStore (factStoreFromDerivations missingSelection)
  assertEqual
    "derivation difference removes exhausted buckets"
    True
    (nullFactDerivationIndex (differenceFactDerivationIndexes sourceIndex selectedIndex))

assertGuardCompileAndCapabilityEval :: IO ()
assertGuardCompileAndCapabilityEval =
  case compileGuard Set.empty capabilityCondition of
    Left unbound ->
      fail ("capability guard should compile without unbound vars: " <> show unbound)
    Right compiled -> do
      assertEqual
        "compiled guard clauses are derived from its normalized expression"
        (toCNF (compiledGuardNormalizedExpression compiled))
        (compiledGuardClauses compiled)
      assertEqual "guard child index zero round-trips" 0 (guardChildIndexValue (guardChildIndex 0))
      assertEqual "guard child index range is Word64-backed" (maxBound :: Word64) (guardChildIndexValue (guardChildIndex maxBound))
      assertEqual
        "empty resolver rejects typed capability atoms"
        False
        (guardAccepted emptyGuardCapabilityResolver compiled)
      assertEqual
        "typed capability resolver accepts the matching atom and class tuple"
        True
        (guardAccepted surfaceResolver compiled)
  where
    guardAccepted :: GuardCapabilityResolver capability -> CompiledGuard capability [] -> Bool
    guardAccepted resolver =
      isJust . evaluateCompiledGuardWithEvidenceAndCapabilities emptyFactStore resolver id resolveRoot

    capabilityCondition :: RewriteCondition Capability []
    capabilityCondition =
      RewriteCondition (guardHasCapability NeedsSurface [rootRef])

    surfaceResolver :: GuardCapabilityResolver Capability
    surfaceResolver =
      GuardCapabilityResolver
        (\capabilityValue classIds -> capabilityValue == NeedsSurface && classIds == [ClassId 11])

    resolveRoot :: GuardTerm [] -> Maybe ClassId
    resolveRoot = \case
      GuardRefTerm observedRef | observedRef == rootRef ->
        Just (ClassId 11)
      _ ->
        Nothing

    rootRef :: GuardRef
    rootRef =
      GuardRef (GuardFromRoot, GuardPath [])

data GuardNodeF child
  = GuardNodePair child child
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasConstructorTag GuardNodeF where
  type ConstructorTag GuardNodeF = ()

  constructorTag _ =
    ()

assertNestedGuardCanonicalWordsAndDigest :: IO ()
assertNestedGuardCanonicalWordsAndDigest = do
  compiledGuard <-
    either
      (\unbound -> fail ("nested guard should compile without unbound variables: " <> show unbound))
      pure
      ( compileGuard
          Set.empty
          ( RewriteCondition
                ( guardHasFactTerms
                    (FactId 32)
                    [ GuardProjectTerm
                        (GuardProjectTerm (GuardRefTerm nestedGuardRootRef) (guardChildIndex 1))
                        (guardChildIndex 23)
                    ]
                )
              :: RewriteCondition Capability GuardNodeF
          )
      )
  let expectedWords =
        [0x400, 1, 0x401, 1, 0x402, 0x20, 32, 1, 0x101, 0x101, 0x100, 0x110, 0, 1, 23]
  assertEqual
    "nested projection canonical words are exact"
    expectedWords
    (compiledGuardCanonicalWordsWith (const 0) (const 0) compiledGuard)
  assertEqual
    "nested projection canonical digest is exact"
    0x8d0384fea81ac133
    (compiledGuardDigestWith (const 0) (const 0) compiledGuard)
  where
    nestedGuardRootRef :: GuardRef
    nestedGuardRootRef =
      GuardRef (GuardFromRoot, GuardPath [])

assertProjectedPostMatchTermGroundsRecursively :: IO ()
assertProjectedPostMatchTermGroundsRecursively = do
  let projectedVariable = EGraph.mkPatternVar 20
      leftChildVariable = EGraph.mkPatternVar 21
      rightChildVariable = EGraph.mkPatternVar 22
      projectedSubstitution =
        projectPostMatchSubst
          ( PatternProjection
              ( Map.singleton
                  projectedVariable
                  (PatternNode (GuardNodePair (PatternVar leftChildVariable) (PatternVar rightChildVariable)))
              )
          )
          (SubstBinder (BinderId 0) (PostMatchVar projectedVariable))
      resolvedChildren :: Map PatternVar (Pattern GuardNodeF)
      resolvedChildren =
        Map.fromList
          [ (leftChildVariable, PatternVar (EGraph.mkPatternVar 30)),
            (rightChildVariable, PatternVar (EGraph.mkPatternVar 31))
          ]
      expectedArgument =
        PatternNode
          ( GuardNodePair
              (PatternVar (EGraph.mkPatternVar 30))
              (PatternVar (EGraph.mkPatternVar 31))
          )
      substitutionAlgebra :: BinderSubstAlgebra GuardNodeF
      substitutionAlgebra =
        BinderSubstAlgebra
          { bsaSubstituteBinder = \_ resolvedArgument _ -> resolvedArgument
          }
  assertEqual
    "projection may turn a post-match variable into a recursively grounded node"
    (Right expectedArgument)
    (applyPostMatchSubst substitutionAlgebra resolvedChildren projectedSubstitution (PatternVar projectedVariable))

data SmallCanonicalizer = SmallCanonicalizer
  { scUniverseSize :: Int,
    scMapping :: Map Int ClassId
  }
  deriving stock (Show)

data TestFactRuleSpec = TestFactRuleSpec
  { tfrsRuleId :: FactRuleId,
    tfrsName :: String,
    tfrsProjection :: [GuardRef],
    tfrsFactId :: FactId,
    tfrsCondition :: Maybe (RewriteCondition Capability GuardNodeF)
  }
  deriving stock (Show)

data GuardSoundnessCase = GuardSoundnessCase
  { gscCanonicalizer :: SmallCanonicalizer,
    gscStore :: FactStore,
    gscResolverEnv :: Map GuardRef ClassId,
    gscCondition :: RewriteCondition Capability []
  }
  deriving stock (Show)

data PlanFaithfulnessCase = PlanFaithfulnessCase
  { pfcCanonicalizer :: SmallCanonicalizer,
    pfcStore :: FactStore,
    pfcRuleSpec :: TestFactRuleSpec,
    pfcMatches :: [(ClassId, Substitution)]
  }
  deriving stock (Show)

data SemiNaiveCompletenessCase = SemiNaiveCompletenessCase
  { snccCanonicalizer :: SmallCanonicalizer,
    snccStore :: FactStore,
    snccSeedDerivations :: FactDerivationIndex,
    snccRules :: [(TestFactRuleSpec, [(ClassId, Substitution)])]
  }
  deriving stock (Show)

data NaiveLiteralAssessment = NaiveLiteralAssessment
  { nlaSatisfied :: Bool,
    nlaEvidence :: [GuardLiteralEvidence]
  }

data NaiveAtomAssessment = NaiveAtomAssessment
  { naaSatisfied :: Bool,
    naaPositiveEvidence :: [GuardLiteralEvidence],
    naaNegativeEvidence :: [GuardLiteralEvidence]
  }

canonicalizeSmallClass :: SmallCanonicalizer -> ClassId -> ClassId
canonicalizeSmallClass smallCanonicalizer classId@(ClassId classKey) =
  Map.findWithDefault classId classKey (scMapping smallCanonicalizer)

smallCanonicalClasses :: SmallCanonicalizer -> [ClassId]
smallCanonicalClasses smallCanonicalizer =
  fmap ClassId [0 .. scUniverseSize smallCanonicalizer - 1]

testPatternVar0 :: PatternVar
testPatternVar0 =
  EGraph.mkPatternVar 0

testPatternVar1 :: PatternVar
testPatternVar1 =
  EGraph.mkPatternVar 1

testRulePattern :: Pattern GuardNodeF
testRulePattern =
  PatternNode
    ( GuardNodePair
        (PatternVar testPatternVar0)
        (PatternVar testPatternVar1)
    )

testGuardRefs :: [GuardRef]
testGuardRefs =
  [ closureRootRef,
    GuardVar testPatternVar0,
    GuardVar testPatternVar1
  ]

ruleFromTestSpec :: TestFactRuleSpec -> FactRule Capability GuardNodeF
ruleFromTestSpec ruleSpec =
  FactRule
    { frId = tfrsRuleId ruleSpec,
      frName = tfrsName ruleSpec,
      frPattern = testRulePattern,
      frProjection = tfrsProjection ruleSpec,
      frFactId = tfrsFactId ruleSpec,
      frCondition = tfrsCondition ruleSpec
    }

testResolveTerm :: ClassId -> Substitution -> GuardTerm GuardNodeF -> Maybe ClassId
testResolveTerm rootClass substitution = \case
  GuardRefTerm (GuardRef (GuardFromRoot, GuardPath [])) ->
    Just rootClass
  GuardRefTerm (GuardRef (GuardFromVar patternVar, GuardPath [])) ->
    lookupSubst patternVar substitution
  _ ->
    Nothing

guardSoundnessResolveTerm :: Map GuardRef ClassId -> GuardTerm [] -> Maybe ClassId
guardSoundnessResolveTerm resolverEnv = \case
  GuardRefTerm guardRef ->
    Map.lookup guardRef resolverEnv
  _ ->
    Nothing

testCapabilityResolver :: GuardCapabilityResolver Capability
testCapabilityResolver =
  GuardCapabilityResolver
    ( \capabilityValue classIds ->
        let classKeys =
              fmap (\(ClassId classKey) -> classKey) classIds
         in case capabilityValue of
              NeedsSurface ->
                even (sum classKeys)
              NeedsOtherSurface ->
                odd (length classKeys + sum classKeys)
    )

naiveCompiledGuardSatisfied ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  CompiledGuard capability f ->
  Bool
naiveCompiledGuardSatisfied factStore capabilityResolver canonicalizeClassId resolveTerm compiledGuard =
  all
    ( any
        ( naiveLiteralSatisfied
            factStore
            capabilityResolver
            canonicalizeClassId
            resolveTerm
        )
        . Set.toAscList
    )
    (compiledGuardClauses compiledGuard)

naiveLiteralSatisfied ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  Literal (GuardAtom capability f) ->
  Bool
naiveLiteralSatisfied factStore capabilityResolver canonicalizeClassId resolveTerm literal =
  let atomSatisfied =
        naiveAtomSatisfied
          factStore
          capabilityResolver
          canonicalizeClassId
          resolveTerm
          (literalVariable literal)
   in if literalPolarity literal
        then atomSatisfied
        else not atomSatisfied

naiveAtomSatisfied ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  GuardAtom capability f ->
  Bool
naiveAtomSatisfied factStore capabilityResolver canonicalizeClassId resolveTerm =
  naaSatisfied
    . naiveAtomAssessment factStore capabilityResolver canonicalizeClassId resolveTerm

naiveGuardEvidenceFromExpr ::
  FactStore ->
  GuardCapabilityResolver Capability ->
  (ClassId -> ClassId) ->
  (GuardTerm GuardNodeF -> Maybe ClassId) ->
  RewriteCondition Capability GuardNodeF ->
  Maybe GuardEvidence
naiveGuardEvidenceFromExpr factStore capabilityResolver canonicalizeClassId resolveTerm =
  naiveGuardEvidenceFromCNF factStore capabilityResolver canonicalizeClassId resolveTerm
    . toCNF
    . normalize
    . rewriteGuardExpr

naiveGuardEvidenceFromCNF ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  CNF (GuardAtom capability f) ->
  Maybe GuardEvidence
naiveGuardEvidenceFromCNF factStore capabilityResolver canonicalizeClassId resolveTerm clauses =
  guardEvidenceFromClauses
    <$> traverse
      (naiveClauseEvidence factStore capabilityResolver canonicalizeClassId resolveTerm)
      clauses

naiveClauseEvidence ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  Set.Set (Literal (GuardAtom capability f)) ->
  Maybe GuardClauseEvidence
naiveClauseEvidence factStore capabilityResolver canonicalizeClassId resolveTerm clause =
  let literalAssessments =
        fmap
          (naiveLiteralEvidence factStore capabilityResolver canonicalizeClassId resolveTerm)
          (Set.toAscList clause)
      satisfiedEvidence =
        foldMap
          ( \literalAssessment ->
              if nlaSatisfied literalAssessment
                then nlaEvidence literalAssessment
                else []
          )
          literalAssessments
   in if any nlaSatisfied literalAssessments
        then Just (GuardClauseEvidence satisfiedEvidence)
        else Nothing

naiveLiteralEvidence ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  Literal (GuardAtom capability f) ->
  NaiveLiteralAssessment
naiveLiteralEvidence factStore capabilityResolver canonicalizeClassId resolveTerm literal =
  let atomAssessment =
        naiveAtomAssessment
          factStore
          capabilityResolver
          canonicalizeClassId
          resolveTerm
          (literalVariable literal)
      satisfied =
        if literalPolarity literal
          then naaSatisfied atomAssessment
          else not (naaSatisfied atomAssessment)
   in NaiveLiteralAssessment
        { nlaSatisfied = satisfied,
          nlaEvidence =
            if satisfied
              then
                if literalPolarity literal
                  then naaPositiveEvidence atomAssessment
                  else naaNegativeEvidence atomAssessment
              else []
        }

naiveAtomAssessment ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  GuardAtom capability f ->
  NaiveAtomAssessment
naiveAtomAssessment factStore capabilityResolver canonicalizeClassId resolveTerm =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      case (,) <$> resolveCanonicalTerm leftTerm <*> resolveCanonicalTerm rightTerm of
        Nothing ->
          naiveUnresolvedAtom
        Just (leftClassId, rightClassId) ->
          let classesEqual =
                leftClassId == rightClassId
           in NaiveAtomAssessment
                { naaSatisfied = classesEqual,
                  naaPositiveEvidence =
                    if classesEqual
                      then [GuardClassesEqual leftClassId rightClassId]
                      else [],
                  naaNegativeEvidence =
                    if classesEqual
                      then []
                      else [GuardClassesDistinct leftClassId rightClassId]
                }
    HasFact factId guardTerms ->
      case FactTuple <$> traverse resolveCanonicalTerm guardTerms of
        Nothing ->
          naiveUnresolvedAtom
        Just factTuple ->
          let factWitness =
                FactWitness factId factTuple
              factPresent =
                hasFact factId factTuple factStore
           in NaiveAtomAssessment
                { naaSatisfied = factPresent,
                  naaPositiveEvidence =
                    if factPresent
                      then [GuardFactPresent factWitness]
                      else [],
                  naaNegativeEvidence =
                    if factPresent
                      then []
                      else [GuardFactAbsent factId factTuple]
                }
    HasCapability capability guardTerms ->
      case traverse resolveCanonicalTerm guardTerms of
        Nothing ->
          naiveUnresolvedAtom
        Just classIds ->
          let capabilityHeld =
                runGuardCapabilityResolver capabilityResolver capability classIds
           in NaiveAtomAssessment
                { naaSatisfied = capabilityHeld,
                  naaPositiveEvidence =
                    if capabilityHeld
                      then [GuardCapabilityHeld]
                      else [],
                  naaNegativeEvidence =
                    if capabilityHeld
                      then []
                      else [GuardCapabilityMissing]
                }
  where
    resolveCanonicalTerm guardTerm =
      canonicalizeClassId <$> resolveTerm guardTerm

naiveUnresolvedAtom :: NaiveAtomAssessment
naiveUnresolvedAtom =
  NaiveAtomAssessment
    { naaSatisfied = False,
      naaPositiveEvidence = [],
      naaNegativeEvidence = [GuardAtomUnresolved]
    }

guardEvidenceFactsPresent :: FactStore -> GuardEvidence -> Bool
guardEvidenceFactsPresent factStore guardEvidence =
  all
    (\factWitness -> hasFact (fwFactId factWitness) (fwTuple factWitness) factStore)
    (geFactWitnesses guardEvidence)
    && all
      (all literalEvidenceFactsPresent . gceSatisfiedLiterals)
      (geClauses guardEvidence)
  where
    literalEvidenceFactsPresent = \case
      GuardFactPresent factWitness ->
        hasFact (fwFactId factWitness) (fwTuple factWitness) factStore
      _ ->
        True

directRuleDerivations ::
  GuardCapabilityResolver Capability ->
  FactStore ->
  (root -> Substitution -> GuardTerm GuardNodeF -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  FactRule Capability GuardNodeF ->
  [(root, Substitution)] ->
  FactDerivationIndex
directRuleDerivations capabilityResolver factStore resolveTerm canonicalizeClassId factRule matches =
  foldMap
    ( maybe
        emptyFactDerivationIndex
        singletonFactDerivationIndex
        . uncurry (directRuleDerivationFor capabilityResolver canonicalFactStore resolveTerm canonicalizeClassId factRule)
    )
    matches
  where
    canonicalFactStore =
      canonicalizeFactStore canonicalizeClassId factStore

directRuleDerivationFor ::
  GuardCapabilityResolver Capability ->
  FactStore ->
  (root -> Substitution -> GuardTerm GuardNodeF -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  FactRule Capability GuardNodeF ->
  root ->
  Substitution ->
  Maybe FactDerivation
directRuleDerivationFor capabilityResolver factStore resolveTerm canonicalizeClassId factRule rootValue substitution = do
  guardEvidence <-
    case frCondition factRule of
      Nothing ->
        Just Nothing
      Just condition ->
        Just
          <$> naiveGuardEvidenceFromExpr
            factStore
            capabilityResolver
            canonicalizeClassId
            (resolveTerm rootValue substitution)
            condition
  factTuple <-
    FactTuple
      <$> traverse
        (\guardRef -> canonicalizeClassId <$> resolveTerm rootValue substitution (GuardRefTerm guardRef))
        (frProjection factRule)
  pure
    FactDerivation
      { fdRuleId = frId factRule,
        fdRuleName = frName factRule,
        fdFactWitness = FactWitness (frFactId factRule) factTuple,
        fdGuardEvidence = guardEvidence
      }

ruleMatchHost :: [(TestFactRuleSpec, [(ClassId, Substitution)])] -> Map FactRuleId [(ClassId, Substitution)]
ruleMatchHost =
  Map.fromList . fmap (\(ruleSpec, matches) -> (tfrsRuleId ruleSpec, matches))

matchesForCompiledRule ::
  CompiledFactRule Capability GuardNodeF ->
  Map FactRuleId [(ClassId, Substitution)] ->
  [(ClassId, Substitution)]
matchesForCompiledRule compiledRule =
  Map.findWithDefault [] (cfrId compiledRule)

naiveGeneratedClosureOracle ::
  (ClassId -> ClassId) ->
  [CompiledFactRule Capability GuardNodeF] ->
  Map FactRuleId [(ClassId, Substitution)] ->
  FactStore ->
  FactDerivationIndex ->
  (FactStore, FactDerivationIndex)
naiveGeneratedClosureOracle canonicalizeClassId compiledRules matchesByRule seedStore seedDerivations =
  go initialFacts canonicalSeedDerivations
  where
    canonicalSeedDerivations =
      canonicalizeFactDerivationIndex canonicalizeClassId seedDerivations

    initialFacts =
      canonicalizeFactStore
        canonicalizeClassId
        (unionFactStores seedStore (factStoreFromDerivations canonicalSeedDerivations))

    go facts derivations =
      let roundDerivations =
            deriveFactDerivations
              facts
              matchesForCompiledRule
              testResolveTerm
              canonicalizeClassId
              compiledRules
              matchesByRule
          nextDerivations =
            derivations <> roundDerivations
          nextFacts =
            unionFactStores facts (factStoreFromDerivations roundDerivations)
       in if nextFacts == facts && nextDerivations == derivations
            then (facts, derivations)
            else go nextFacts nextDerivations

genSmallCanonicalizer :: Hedgehog.Gen SmallCanonicalizer
genSmallCanonicalizer = do
  universeSize <- Gen.int (Range.linear 1 6)
  representativeCount <- Gen.int (Range.linear 1 universeSize)
  representativeAssignments <-
    traverse
      ( \classKey ->
          if classKey < representativeCount
            then pure (ClassId classKey)
            else Gen.element (fmap ClassId [0 .. representativeCount - 1])
      )
      [0 .. universeSize - 1]
  pure
    SmallCanonicalizer
      { scUniverseSize = universeSize,
        scMapping = Map.fromList (zip [0 .. universeSize - 1] representativeAssignments)
      }

genClassIdIn :: SmallCanonicalizer -> Hedgehog.Gen ClassId
genClassIdIn =
  Gen.element . smallCanonicalClasses

genFactId :: Hedgehog.Gen FactId
genFactId =
  FactId <$> Gen.int (Range.linear 0 7)

genFactTupleIn :: SmallCanonicalizer -> Hedgehog.Gen FactTuple
genFactTupleIn smallCanonicalizer =
  FactTuple <$> Gen.list (Range.linear 0 3) (genClassIdIn smallCanonicalizer)

genFactWitnessIn :: SmallCanonicalizer -> Hedgehog.Gen FactWitness
genFactWitnessIn smallCanonicalizer =
  FactWitness <$> genFactId <*> genFactTupleIn smallCanonicalizer

genFactStoreIn :: SmallCanonicalizer -> Hedgehog.Gen FactStore
genFactStoreIn smallCanonicalizer =
  foldr
    (\factWitness -> insertFact (fwFactId factWitness) (fwTuple factWitness))
    emptyFactStore
    <$> Gen.list (Range.linear 0 18) (genFactWitnessIn smallCanonicalizer)

genTestMatchIn :: SmallCanonicalizer -> Hedgehog.Gen (ClassId, Substitution)
genTestMatchIn smallCanonicalizer = do
  rootClass <- genClassIdIn smallCanonicalizer
  firstClass <- genClassIdIn smallCanonicalizer
  secondClass <- genClassIdIn smallCanonicalizer
  pure
    ( rootClass,
      insertSubst
        testPatternVar1
        secondClass
        (insertSubst testPatternVar0 firstClass emptySubstitution)
    )

genGuardEnvIn :: SmallCanonicalizer -> Hedgehog.Gen (Map GuardRef ClassId)
genGuardEnvIn smallCanonicalizer =
  Map.fromList
    <$> traverse
      ( \guardRef -> do
          classId <- genClassIdIn smallCanonicalizer
          pure (guardRef, classId)
      )
      testGuardRefs

genGuardTermFrom :: [GuardRef] -> Hedgehog.Gen (GuardTerm f)
genGuardTermFrom guardRefs =
  GuardRefTerm <$> Gen.element guardRefs

genGuardAtomFrom :: Bool -> [GuardRef] -> Hedgehog.Gen (GuardAtom Capability f)
genGuardAtomFrom includeCapabilities guardRefs =
  Gen.choice
    ( [ ClassesEquivalent
          <$> genGuardTermFrom guardRefs
          <*> genGuardTermFrom guardRefs,
        HasFact
          <$> genFactId
          <*> Gen.list (Range.linear 0 3) (genGuardTermFrom guardRefs)
      ]
        <> if includeCapabilities
          then
            [ HasCapability
                <$> Gen.element [NeedsSurface, NeedsOtherSurface]
                <*> Gen.list (Range.linear 0 3) (genGuardTermFrom guardRefs)
            ]
          else []
    )

genLiteralExprFrom :: Bool -> [GuardRef] -> Hedgehog.Gen (GuardExpr Capability f)
genLiteralExprFrom includeCapabilities guardRefs = do
  atom <- genGuardAtomFrom includeCapabilities guardRefs
  positive <- Gen.bool
  pure
    ( if positive
        then Atom atom
        else Not (Atom atom)
    )

genCnfGuardExprFrom :: Bool -> [GuardRef] -> Hedgehog.Gen (GuardExpr Capability f)
genCnfGuardExprFrom includeCapabilities guardRefs =
  And
    <$> Gen.list
      (Range.linear 0 4)
      ( Or
          <$> Gen.list
            (Range.linear 0 4)
            (genLiteralExprFrom includeCapabilities guardRefs)
      )

genGuardSoundnessCase :: Hedgehog.Gen GuardSoundnessCase
genGuardSoundnessCase = do
  smallCanonicalizer <- genSmallCanonicalizer
  factStore <- genFactStoreIn smallCanonicalizer
  resolverEnv <- genGuardEnvIn smallCanonicalizer
  condition <- RewriteCondition <$> genCnfGuardExprFrom True testGuardRefs
  pure
    GuardSoundnessCase
      { gscCanonicalizer = smallCanonicalizer,
        gscStore = factStore,
        gscResolverEnv = resolverEnv,
        gscCondition = condition
      }

genTestFactRuleSpec :: Int -> Hedgehog.Gen TestFactRuleSpec
genTestFactRuleSpec ruleKey = do
  projection <- Gen.list (Range.linear 0 3) (Gen.element testGuardRefs)
  factId <- genFactId
  condition <- Gen.maybe (RewriteCondition <$> genCnfGuardExprFrom False testGuardRefs)
  pure
    TestFactRuleSpec
      { tfrsRuleId = FactRuleId ruleKey,
        tfrsName = "generated-" <> show ruleKey,
        tfrsProjection = projection,
        tfrsFactId = factId,
        tfrsCondition = condition
      }

genPlanFaithfulnessCase :: Hedgehog.Gen PlanFaithfulnessCase
genPlanFaithfulnessCase = do
  smallCanonicalizer <- genSmallCanonicalizer
  factStore <- genFactStoreIn smallCanonicalizer
  ruleSpec <- genTestFactRuleSpec 1
  matches <- Gen.list (Range.linear 0 8) (genTestMatchIn smallCanonicalizer)
  pure
    PlanFaithfulnessCase
      { pfcCanonicalizer = smallCanonicalizer,
        pfcStore = factStore,
        pfcRuleSpec = ruleSpec,
        pfcMatches = matches
      }

genSeedDerivationIndexIn :: SmallCanonicalizer -> Hedgehog.Gen FactDerivationIndex
genSeedDerivationIndexIn smallCanonicalizer =
  foldMap singletonFactDerivationIndex
    <$> Gen.list
      (Range.linear 0 5)
      ( genSeedDerivationIn smallCanonicalizer
      )

genSeedDerivationIn :: SmallCanonicalizer -> Hedgehog.Gen FactDerivation
genSeedDerivationIn smallCanonicalizer = do
  factWitness <- genFactWitnessIn smallCanonicalizer
  pure
    FactDerivation
      { fdRuleId = FactRuleId 0,
        fdRuleName = "seed",
        fdFactWitness = factWitness,
        fdGuardEvidence = Nothing
      }

genRuleWithMatchesIn :: SmallCanonicalizer -> Int -> Hedgehog.Gen (TestFactRuleSpec, [(ClassId, Substitution)])
genRuleWithMatchesIn smallCanonicalizer ruleKey = do
  ruleSpec <- genTestFactRuleSpec ruleKey
  matches <- Gen.list (Range.linear 0 8) (genTestMatchIn smallCanonicalizer)
  pure (ruleSpec, matches)

genSemiNaiveCompletenessCase :: Hedgehog.Gen SemiNaiveCompletenessCase
genSemiNaiveCompletenessCase = do
  smallCanonicalizer <- genSmallCanonicalizer
  factStore <- genFactStoreIn smallCanonicalizer
  seedDerivations <- genSeedDerivationIndexIn smallCanonicalizer
  ruleCount <- Gen.int (Range.linear 0 8)
  rules <- traverse (genRuleWithMatchesIn smallCanonicalizer) [1 .. ruleCount]
  pure
    SemiNaiveCompletenessCase
      { snccCanonicalizer = smallCanonicalizer,
        snccStore = factStore,
        snccSeedDerivations = seedDerivations,
        snccRules = rules
      }

assertGuardCanonicalWordsInvariances :: IO ()
assertGuardCanonicalWordsInvariances = do
  equivForward <- compileTestGuard boundVars (guardEquivalent (EGraph.mkPatternVar 1) (EGraph.mkPatternVar 2))
  equivBackward <- compileTestGuard boundVars (guardEquivalent (EGraph.mkPatternVar 2) (EGraph.mkPatternVar 1))
  assertEqual
    "canonical words are symmetric in class equivalence"
    (canonicalWords equivForward)
    (canonicalWords equivBackward)
  factSeven <- compileTestGuard Set.empty (guardHasFact (FactId 7) [rootRef])
  factNine <- compileTestGuard Set.empty (guardHasFact (FactId 9) [rootRef])
  combinedForward <- expectSome "two compiled guards combine" (combineCompiledGuards [factSeven, factNine])
  combinedBackward <- expectSome "two compiled guards combine" (combineCompiledGuards [factNine, factSeven])
  assertEqual
    "canonical words are invariant under clause order"
    (canonicalWords combinedForward)
    (canonicalWords combinedBackward)
  assertEqual
    "canonical words discriminate distinct fact ids"
    False
    (canonicalWords factSeven == canonicalWords factNine)
  where
    boundVars =
      Set.fromList [EGraph.mkPatternVar 1, EGraph.mkPatternVar 2]

    rootRef =
      GuardRef (GuardFromRoot, GuardPath [])

    compileTestGuard ::
      Set.Set PatternVar ->
      GuardExpr Capability GuardNodeF ->
      IO (CompiledGuard Capability GuardNodeF)
    compileTestGuard boundSet guardExpr =
      either
        (\unbound -> fail ("guard should compile without unbound vars: " <> show unbound))
        pure
        (compileGuard boundSet (RewriteCondition guardExpr))

    canonicalWords :: CompiledGuard Capability GuardNodeF -> [Word64]
    canonicalWords =
      compiledGuardCanonicalWordsWith capabilityWord (const 0)

    capabilityWord :: Capability -> Word64
    capabilityWord = \case
      NeedsSurface -> 1
      NeedsOtherSurface -> 2

assertGuardEvidenceRecordsEverySatisfiedLiteral :: IO ()
assertGuardEvidenceRecordsEverySatisfiedLiteral = do
  compiledGuard <-
    compileUnitGuard
      ( Or
          [ guardHasFact (FactId 21) [closureRootRef],
            guardHasFact (FactId 22) [closureRootRef]
          ]
      )
  guardEvidence <-
    expectSome
      "guard with two satisfied positive fact literals"
      ( evaluateCompiledGuardWithEvidenceAndCapabilities
          twoFactStore
          emptyGuardCapabilityResolver
          id
          resolveRoot
          compiledGuard
      )
  assertEqual
    "evidence projection records both positive fact witnesses"
    (Set.fromList [leftWitness, rightWitness])
    (geFactWitnesses guardEvidence)
  case geClauses guardEvidence of
    [GuardClauseEvidence literalEvidences] ->
      assertEqual
        "clause evidence keeps every satisfied positive literal"
        (Set.fromList [GuardFactPresent leftWitness, GuardFactPresent rightWitness])
        (Set.fromList literalEvidences)
    otherClauses ->
      fail ("expected one CNF clause evidence, got " <> show otherClauses)
  where
    twoFactStore =
      insertFact (FactId 21) closureTuple (insertFact (FactId 22) closureTuple emptyFactStore)

    leftWitness =
      FactWitness (FactId 21) closureTuple

    rightWitness =
      FactWitness (FactId 22) closureTuple

    resolveRoot :: GuardTerm [] -> Maybe ClassId
    resolveRoot = \case
      GuardRefTerm observedRef | observedRef == closureRootRef ->
        Just (ClassId 0)
      _ ->
        Nothing

assertCanonicalWordsInjectiveForGuardRefs :: IO ()
assertCanonicalWordsInjectiveForGuardRefs = do
  rootChildGuard <-
    compileTestGuard Set.empty (guardHasFact (FactId 31) [oldFingerprintRootChildRef])
  collidingVarGuard <-
    compileTestGuard
      (Set.singleton oldFingerprintCollidingVar)
      (guardHasFact (FactId 31) [GuardVar oldFingerprintCollidingVar])
  assertEqual
    "structural guard ref words split an old fingerprint collision"
    False
    (canonicalWords rootChildGuard == canonicalWords collidingVarGuard)
  pathRefGuard <-
    compileTestGuard Set.empty (guardHasFact (FactId 32) [GuardRef (GuardFromRoot, GuardPath [guardChildIndex 1, guardChildIndex 23])])
  projectedGuard <-
    compileTestGuard
      Set.empty
      ( guardHasFactTerms
          (FactId 32)
          [ GuardProjectTerm
              (GuardProjectTerm (GuardRefTerm closureRootRef) (guardChildIndex 1))
              (guardChildIndex 23)
          ]
      )
  assertEqual
    "structural words distinguish stored guard paths from projected terms"
    False
    (canonicalWords pathRefGuard == canonicalWords projectedGuard)
  where
    oldFingerprintCollidingVar =
      EGraph.mkPatternVar (7 * 16777619 + 97 - 11)

    oldFingerprintRootChildRef =
      GuardRef (GuardFromRoot, GuardPath [guardChildIndex 0])

    compileTestGuard ::
      Set.Set PatternVar ->
      GuardExpr Capability [] ->
      IO (CompiledGuard Capability [])
    compileTestGuard boundSet guardExpr =
      either
        (\unbound -> fail ("guard should compile without unbound vars: " <> show unbound))
        pure
        (compileGuard boundSet (RewriteCondition guardExpr))

    canonicalWords :: CompiledGuard Capability [] -> [Word64]
    canonicalWords =
      compiledGuardCanonicalNodeWordsWith capabilityWord (const 0)

    capabilityWord :: Capability -> Word64
    capabilityWord = \case
      NeedsSurface -> 1
      NeedsOtherSurface -> 2

compileUnitGuard :: GuardExpr Capability [] -> IO (CompiledGuard Capability [])
compileUnitGuard guardExpr =
  either
    (\unbound -> fail ("unit guard should compile without unbound vars: " <> show unbound))
    pure
    (compileGuard Set.empty (RewriteCondition guardExpr))

assertGuardEvidenceMarksUnresolvedNegatedLiterals :: IO ()
assertGuardEvidenceMarksUnresolvedNegatedLiterals = do
  compiledGuard <-
    compileUnitGuard (Not (guardHasFact (FactId 23) [closureRootRef]))
  guardEvidence <-
    expectSome
      "negated fact literal over an unresolvable term remains satisfied"
      ( evaluateCompiledGuardWithEvidenceAndCapabilities
          emptyFactStore
          emptyGuardCapabilityResolver
          id
          (const Nothing)
          compiledGuard
      )
  assertEqual
    "satisfaction by unresolvability is recorded as evidence, not silence"
    [GuardClauseEvidence [GuardAtomUnresolved]]
    (geClauses guardEvidence)

assertRawRuleSyntheticNameUsesRuleIdKey :: IO ()
assertRawRuleSyntheticNameUsesRuleIdKey =
  case checkRawRewriteSystem [rawIdentityRule] of
    Left rewriteError ->
      fail ("raw identity rule should check, got " <> show rewriteError)
    Right checkedSystem ->
      assertEqual
        "raw rules synthesize identifier-safe names from RewriteRuleId keys"
        ["raw-7"]
        (fmap ruleNameString (checkedRuleNames checkedSystem))

assertDuplicateRuleIdsReject :: IO ()
assertDuplicateRuleIdsReject = do
  firstName <- expectRight (mkRuleName "first")
  secondName <- expectRight (mkRuleName "second")
  case checkRuleSet (duplicateRuleSet firstName secondName) of
    Left (RewriteDuplicateRuleId (RewriteRuleId 3)) ->
      pure ()
    Left otherError ->
      fail ("duplicate rule id should be typed, got " <> show otherError)
    Right _ ->
      fail "duplicate rule id should not check"

assertMissingRuleReportsTypedError :: IO ()
assertMissingRuleReportsTypedError = do
  knownName <- expectRight (mkRuleName "known")
  missingName <- expectRight (mkRuleName "missing")
  checkedSystem <- expectRight (checkRuleSet (singleRuleSet knownName))
  case rewriteByRuleName missingName checkedSystem of
    Left (RewriteUnknownRule observedName) ->
      assertEqual "missing rule preserves its typed name" "missing" (ruleNameString observedName)
    Left otherError ->
      fail ("missing rule should report RewriteUnknownRule, got " <> show otherError)
    Right _ ->
      fail "missing rule should not resolve"

rawIdentityRule :: RawRewriteRule (RewriteCondition Capability []) []
rawIdentityRule =
  RawRewriteRule
    { rrId = RewriteRuleId 7,
      rrLhs = identityPattern,
      rrRhs = identityPattern,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

identityPattern :: Pattern []
identityPattern =
  PatternVar (EGraph.mkPatternVar 0)

duplicateRuleSet :: RuleName -> RuleName -> RuleSet Capability []
duplicateRuleSet firstName secondName =
  ruleSet
    [ ruleWithId (RewriteRuleId 3) firstName identityPattern identityPattern,
      ruleWithId (RewriteRuleId 3) secondName identityPattern identityPattern
    ]

singleRuleSet :: RuleName -> RuleSet Capability []
singleRuleSet knownName =
  ruleSet
    [ ruleWithId (RewriteRuleId 1) knownName identityPattern identityPattern
    ]

closureRootRef :: GuardRef
closureRootRef =
  GuardRef (GuardFromRoot, GuardPath [])

closureTuple :: FactTuple
closureTuple =
  FactTuple [ClassId 0]

closureResolveTerm :: () -> Substitution -> GuardTerm [] -> Maybe ClassId
closureResolveTerm () _substitution = \case
  GuardRefTerm observedRef | observedRef == closureRootRef ->
    Just (ClassId 0)
  _ ->
    Nothing

mkClosureRule :: Int -> [FactId] -> Maybe FactId -> FactId -> FactRule Capability []
mkClosureRule ruleKey positives negative target =
  FactRule
    { frId = FactRuleId ruleKey,
      frName = "closure-" <> show ruleKey,
      frPattern = identityPattern,
      frProjection = [closureRootRef],
      frFactId = target,
      frCondition =
        case fmap positiveLiteral positives <> maybe [] (pure . negativeLiteral) negative of
          [] -> Nothing
          literals -> Just (RewriteCondition (And literals))
    }
  where
    positiveLiteral :: FactId -> GuardExpr Capability []
    positiveLiteral factId =
      guardHasFact factId [closureRootRef]

    negativeLiteral :: FactId -> GuardExpr Capability []
    negativeLiteral factId =
      Not (guardHasFact factId [closureRootRef])

seedStoreFrom :: [FactId] -> FactStore
seedStoreFrom =
  foldr (`insertFact` closureTuple) emptyFactStore

runGatedClosure ::
  [CompiledFactRule Capability []] ->
  FactStore ->
  IO (Int, SemiNaiveClosure)
runGatedClosure compiledRules seedStore =
  either
    (\(_ :: FactClosureRunError ()) -> fail "gated closure should not hit limits or obstructions")
    pure
    (gatedClosure compiledRules seedStore)

gatedClosure ::
  [CompiledFactRule Capability []] ->
  FactStore ->
  Either (FactClosureRunError ()) (Int, SemiNaiveClosure)
gatedClosure compiledRules seedStore =
  gatedClosureWith id closureResolveTerm compiledRules seedStore emptyFactDerivationIndex

gatedClosureWith ::
  (ClassId -> ClassId) ->
  (() -> Substitution -> GuardTerm [] -> Maybe ClassId) ->
  [CompiledFactRule Capability []] ->
  FactStore ->
  FactDerivationIndex ->
  Either (FactClosureRunError ()) (Int, SemiNaiveClosure)
gatedClosureWith canonicalizeClassId resolveTerm compiledRules seedStore seedDerivations =
  ( deriveSeededFactClosureWithStateAndConfig
        FactClosureRun
          { fcrConfig = defaultSemiNaiveConfig,
            fcrCapabilityResolver = emptyGuardCapabilityResolver,
            fcrInitialFacts = seedStore,
            fcrSeedDerivations = seedDerivations,
            fcrInitialState = 0 :: Int,
            fcrMatcher =
              mkSemiNaiveMatcher
                (\matchCalls _input _rule _host -> (matchCalls + 1, Right [((), emptySubstitution)])),
            fcrResolveTerm = resolveTerm,
            fcrCanonicalClass = canonicalizeClassId,
            fcrRules = compiledRules,
            fcrHost = ()
          }
    )

naiveClosureOracle ::
  [CompiledFactRule Capability []] ->
  FactStore ->
  (FactStore, FactDerivationIndex)
naiveClosureOracle compiledRules seedStore =
  go seedStore emptyFactDerivationIndex
  where
    go facts derivations =
      let roundDerivations =
            deriveFactDerivations
              facts
              (\_rule () -> [((), emptySubstitution)])
              closureResolveTerm
              id
              compiledRules
              ()
          nextDerivations = derivations <> roundDerivations
          nextFacts = unionFactStores facts (factStoreFromDerivations roundDerivations)
       in if nextFacts == facts && nextDerivations == derivations
            then (facts, derivations)
            else go nextFacts nextDerivations

assertChainClosureMatchesOncePerRule :: IO ()
assertChainClosureMatchesOncePerRule = do
  compiledRules <-
    expectRight
      ( compileFactRules
          [ mkClosureRule 1 [] Nothing (FactId 1),
            mkClosureRule 2 [FactId 1] Nothing (FactId 2),
            mkClosureRule 3 [FactId 2] Nothing (FactId 3)
          ]
      )
  (matchCalls, closure) <- runGatedClosure compiledRules emptyFactStore
  assertEqual
    "chain closure derives the full chain"
    (seedStoreFrom [FactId 1, FactId 2, FactId 3])
    (sncFacts closure)
  assertEqual
    "structural matching runs exactly once per rule for the whole closure"
    3
    matchCalls

assertClosureQuotientInvariant :: IO ()
assertClosureQuotientInvariant = do
  compiledRules <-
    expectRight
      ( compileFactRules
          [ mkClosureRule 1 [FactId 41] Nothing (FactId 42)
          ]
      )
  canonicalRun <-
    expectRight
      (gatedClosureWith aliasCanonicalClass canonicalResolveTerm compiledRules canonicalSeedStore emptyFactDerivationIndex)
  aliasRun <-
    expectRight
      (gatedClosureWith aliasCanonicalClass aliasResolveTerm compiledRules aliasSeedStore emptyFactDerivationIndex)
  assertEqual
    "alias-keyed seeds descend to the same canonical closure"
    canonicalRun
    aliasRun
  where
    canonicalSeedStore =
      insertFact (FactId 41) (FactTuple [ClassId 0]) emptyFactStore

    aliasSeedStore =
      insertFact (FactId 41) (FactTuple [ClassId 1]) emptyFactStore

assertAliasSeededDerivationsProduceNoPhantomRounds :: IO ()
assertAliasSeededDerivationsProduceNoPhantomRounds = do
  compiledRule <-
    expectRight (compileFactRule (mkClosureRule 1 [] Nothing (FactId 51)))
  (_matchCalls, closure) <-
    expectRight
      ( gatedClosureWith
          aliasCanonicalClass
          aliasResolveTerm
          [compiledRule]
          (insertFact (FactId 51) (FactTuple [ClassId 1]) emptyFactStore)
          (singletonFactDerivationIndex (aliasSeedDerivation compiledRule))
      )
  assertEqual
    "alias-seeded facts and derivations produce no phantom delta rounds"
    []
    (sncRounds closure)
  assertEqual
    "alias-seeded facts are canonicalized at closure entry"
    (seedStoreFrom [FactId 51])
    (sncFacts closure)
  where
    aliasSeedDerivation :: CompiledFactRule Capability [] -> FactDerivation
    aliasSeedDerivation compiledRule =
      FactDerivation
        { fdRuleId = cfrId compiledRule,
          fdRuleName = cfrName compiledRule,
          fdFactWitness = FactWitness (FactId 51) (FactTuple [ClassId 1]),
          fdGuardEvidence = Nothing
        }

aliasCanonicalClass :: ClassId -> ClassId
aliasCanonicalClass = \case
  ClassId 1 ->
    ClassId 0
  classId ->
    classId

canonicalResolveTerm :: () -> Substitution -> GuardTerm [] -> Maybe ClassId
canonicalResolveTerm () _substitution = \case
  GuardRefTerm observedRef | observedRef == closureRootRef ->
    Just (ClassId 0)
  _ ->
    Nothing

aliasResolveTerm :: () -> Substitution -> GuardTerm [] -> Maybe ClassId
aliasResolveTerm () _substitution = \case
  GuardRefTerm observedRef | observedRef == closureRootRef ->
    Just (ClassId 1)
  _ ->
    Nothing

assertGatedClosureMatchesNaiveOracle :: IO ()
assertGatedClosureMatchesNaiveOracle = do
  passed <-
    Hedgehog.check
      (Hedgehog.withTests 240 (Hedgehog.property gatedClosureMatchesNaiveOracle))
  if passed
    then pure ()
    else fail "gated semi-naive closure diverged from the naive fixpoint oracle"
  where
    gatedClosureMatchesNaiveOracle :: Hedgehog.PropertyT IO ()
    gatedClosureMatchesNaiveOracle = do
      (ruleSpecs, seeds) <- Hedgehog.forAll genClosureUniverse
      compiledRules <-
        either
          (\compileError -> Hedgehog.annotateShow compileError *> Hedgehog.failure)
          pure
          (compileFactRules (fmap ruleFromSpec ruleSpecs))
      let seedStore = seedStoreFrom (fmap FactId seeds)
      case gatedClosure compiledRules seedStore of
        Left (_ :: FactClosureRunError ()) ->
          Hedgehog.failure
        Right (_matchCalls, closure) -> do
          let (oracleFacts, oracleDerivations) = naiveClosureOracle compiledRules seedStore
          sncFacts closure === oracleFacts
          sncDerivations closure === oracleDerivations

    ruleFromSpec :: (Int, [Int], Maybe Int, Int) -> FactRule Capability []
    ruleFromSpec (ruleKey, positives, negative, target) =
      mkClosureRule ruleKey (fmap FactId positives) (fmap FactId negative) (FactId target)

    genClosureUniverse :: Hedgehog.Gen ([(Int, [Int], Maybe Int, Int)], [Int])
    genClosureUniverse = do
      factCount <- Gen.int (Range.linear 1 6)
      ruleCount <- Gen.int (Range.linear 0 8)
      ruleSpecs <-
        traverse
          (\ruleKey -> do
             positives <- Gen.subsequence [1 .. factCount]
             negative <- Gen.maybe (Gen.int (Range.linear 1 factCount))
             target <- Gen.int (Range.linear 1 factCount)
             pure (ruleKey, positives, negative, target))
          [1 .. ruleCount]
      seeds <- Gen.subsequence [1 .. factCount]
      pure (ruleSpecs, seeds)

assertGuardSoundnessOnQuotient :: IO ()
assertGuardSoundnessOnQuotient = do
  passed <-
    Hedgehog.check
      (Hedgehog.withTests 240 (Hedgehog.property guardSoundnessOnQuotient))
  if passed
    then pure ()
    else fail "compiled guard evaluation diverged from quotient CNF semantics"
  where
    guardSoundnessOnQuotient :: Hedgehog.PropertyT IO ()
    guardSoundnessOnQuotient = do
      guardCase <- Hedgehog.forAll genGuardSoundnessCase
      compiledGuard <-
        either
          (\unbound -> Hedgehog.annotateShow unbound *> Hedgehog.failure)
          pure
          (compileGuard (Set.fromList [testPatternVar0, testPatternVar1]) (gscCondition guardCase))
      let canonicalizeClassId =
            canonicalizeSmallClass (gscCanonicalizer guardCase)
          canonicalFactStore =
            canonicalizeFactStore canonicalizeClassId (gscStore guardCase)
          resolveTerm =
            guardSoundnessResolveTerm (gscResolverEnv guardCase)
          observedEvidence =
            evaluateCompiledGuardWithEvidenceAndCapabilities
              canonicalFactStore
              testCapabilityResolver
              canonicalizeClassId
              resolveTerm
              compiledGuard
          expectedEvidence =
            naiveGuardEvidenceFromCNF
              canonicalFactStore
              testCapabilityResolver
              canonicalizeClassId
              resolveTerm
              (compiledGuardClauses compiledGuard)
          expectedSatisfied =
            naiveCompiledGuardSatisfied
              canonicalFactStore
              testCapabilityResolver
              canonicalizeClassId
              resolveTerm
              compiledGuard
      observedEvidence === expectedEvidence
      isJust observedEvidence === expectedSatisfied
      case observedEvidence of
        Nothing ->
          pure ()
        Just guardEvidence ->
          Hedgehog.assert (guardEvidenceFactsPresent canonicalFactStore guardEvidence)

assertPlanCompilationFaithfulness :: IO ()
assertPlanCompilationFaithfulness = do
  passed <-
    Hedgehog.check
      (Hedgehog.withTests 240 (Hedgehog.property planCompilationFaithfulness))
  if passed
    then pure ()
    else fail "compiled fact-rule derivations diverged from direct rule interpretation"
  where
    planCompilationFaithfulness :: Hedgehog.PropertyT IO ()
    planCompilationFaithfulness = do
      planCase <- Hedgehog.forAll genPlanFaithfulnessCase
      let factRule =
            ruleFromTestSpec (pfcRuleSpec planCase)
      compiledRule <-
        either
          (\compileError -> Hedgehog.annotateShow compileError *> Hedgehog.failure)
          pure
          (compileFactRule factRule)
      let canonicalizeClassId =
            canonicalizeSmallClass (pfcCanonicalizer planCase)
          compiledDerivations =
            deriveFactDerivations
              (pfcStore planCase)
              (\_rule () -> pfcMatches planCase)
              testResolveTerm
              canonicalizeClassId
              [compiledRule]
              ()
          directDerivations =
            directRuleDerivations
              emptyGuardCapabilityResolver
              (pfcStore planCase)
              testResolveTerm
              canonicalizeClassId
              factRule
              (pfcMatches planCase)
      canonicalizeFactDerivationIndex canonicalizeClassId compiledDerivations
        === canonicalizeFactDerivationIndex canonicalizeClassId directDerivations

assertSemiNaiveMatchCompleteness :: IO ()
assertSemiNaiveMatchCompleteness = do
  passed <-
    Hedgehog.check
      (Hedgehog.withTests 240 (Hedgehog.property semiNaiveMatchCompleteness))
  if passed
    then pure ()
    else fail "generated semi-naive closure diverged from full re-derivation saturation"
  where
    semiNaiveMatchCompleteness :: Hedgehog.PropertyT IO ()
    semiNaiveMatchCompleteness = do
      closureCase <- Hedgehog.forAll genSemiNaiveCompletenessCase
      compiledRules <-
        either
          (\compileError -> Hedgehog.annotateShow compileError *> Hedgehog.failure)
          pure
          (compileFactRules (fmap (ruleFromTestSpec . fst) (snccRules closureCase)))
      let canonicalizeClassId =
            canonicalizeSmallClass (snccCanonicalizer closureCase)
          matchesByRule =
            ruleMatchHost (snccRules closureCase)
          closureResult :: Either (FactClosureRunError ()) (Int, SemiNaiveClosure)
          closureResult =
            deriveSeededFactClosureWithStateAndConfig
              FactClosureRun
                { fcrConfig = defaultSemiNaiveConfig,
                  fcrCapabilityResolver = emptyGuardCapabilityResolver,
                  fcrInitialFacts = snccStore closureCase,
                  fcrSeedDerivations = snccSeedDerivations closureCase,
                  fcrInitialState = 0 :: Int,
                  fcrMatcher =
                    mkSemiNaiveMatcher
                      ( \matchCalls _input compiledRule host ->
                          (matchCalls + 1, Right (matchesForCompiledRule compiledRule host))
                      ),
                  fcrResolveTerm = testResolveTerm,
                  fcrCanonicalClass = canonicalizeClassId,
                  fcrRules = compiledRules,
                  fcrHost = matchesByRule
                }
      case closureResult of
        Left closureError ->
          Hedgehog.annotateShow closureError *> Hedgehog.failure
        Right (_matchCalls, closure) -> do
          let (oracleFacts, oracleDerivations) =
                naiveGeneratedClosureOracle
                  canonicalizeClassId
                  compiledRules
                  matchesByRule
                  (snccStore closureCase)
                  (snccSeedDerivations closureCase)
          sncFacts closure === oracleFacts
          sncDerivations closure === oracleDerivations
