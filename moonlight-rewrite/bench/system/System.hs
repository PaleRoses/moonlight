{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module System
  ( systemBenchmarkPreflight,
    systemBenchmarks,
  )
where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
  )
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Moonlight.Core
  ( Pattern (..),
    RewriteRuleId (..),
    Substitution,
    emptySubstitution,
    mkPatternVar,
  )
import Moonlight.Rewrite
  ( ClassId,
    hostFromTerm,
  )
import Moonlight.Rewrite.DSL (Node (..))
import Common
  ( expectBench,
  )
import Fixture (BenchSig (..), leaf)
import Moonlight.Rewrite.System
  ( checkRawRewriteSystem,
    checkRuleSet,
  )
import Moonlight.Rewrite.System
  ( CheckedSystem,
    addComposedPathNamed,
    checkedRewrites,
    checkedRuleNames,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardBase (..),
    GuardExpr,
    GuardPath (..),
    GuardRef (..),
    GuardTerm (..),
    RewriteCondition (..),
    compileGuard,
    compiledGuardCanonicalNodeWordsWith,
    emptyGuardCapabilityResolver,
    guardChildIndex,
    guardHasFact,
    guardHasFactTerms,
    guardProjectTerm,
    guardRefTerm,
    data GuardRoot,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
    FactRuleId (..),
    RawFactRule (..),
    compileFactRules,
    emptyFactDerivationIndex,
    mkSemiNaiveMatcher,
  )
import Moonlight.Rewrite.System
  ( FactClosureRun (..),
    SemiNaiveClosure (..),
    defaultSemiNaiveConfig,
    deriveSeededFactClosureWithStateAndConfig,
  )
import Moonlight.Rewrite.System
  ( FactId (..),
    FactStore,
    FactTuple (..),
    emptyFactStore,
    factStoreSize,
    insertFact,
  )
import Moonlight.Rewrite.System
  ( RawRewriteRule (..),
    checkRawRewrites,
  )
import Moonlight.Rewrite.System
  ( elaborateCheckedRewrites,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    mkRuleName,
  )
import Moonlight.Rewrite.System
  ( RuleSet,
    rule,
    ruleSet,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data Capability
  = NeedsSeed
  deriving stock (Eq, Ord)

data SystemFixture = SystemFixture
  { systemRootClass :: !ClassId,
    systemFactRules :: ![CompiledFactRule Capability []],
    systemRuleSet :: !(RuleSet Capability []),
    systemRawRules :: ![RawRewriteRule (RewriteCondition Capability []) []],
    systemCheckedBase :: !(CheckedSystem Capability (Node BenchSig)),
    systemDerivedName :: !RuleName,
    systemDerivedPath :: !(NonEmpty RuleName),
    systemDeepGuard :: !(CompiledGuard Capability [])
  }

instance NFData SystemFixture where
  rnf fixture =
    systemRootClass fixture
      `seq` systemFactRules fixture
      `seq` systemRuleSet fixture
      `seq` systemRawRules fixture
      `seq` systemCheckedBase fixture
      `seq` systemDerivedName fixture
      `seq` systemDerivedPath fixture
      `seq` systemDeepGuard fixture
      `seq` ()

systemBenchmarks :: Benchmark
systemBenchmarks =
  env systemFixture $ \fixture ->
    bgroup
      "system"
      [ bench "semi-naive closure" (nf semiNaiveClosureWeight fixture),
        bench "compile fact rules" (nf compileFactRulesWeight fixture),
        bench "check rule set" (nf checkRuleSetWeight fixture),
        bench "check raw rewrite system" (nf checkRawRewriteSystemWeight fixture),
        bench "elaborate checked rewrites" (nf elaborateCheckedRewritesWeight fixture),
        bench "checked/derived insertion 1024" (nf checkedSystemDerivedInsertionWeight fixture),
        bench "checked/ordered projection 1024" (nf checkedSystemProjectionWeight fixture),
        bench "guard/encoding depth=512" (nf guardEncodingWeight fixture)
      ]

systemBenchmarkPreflight :: IO ()
systemBenchmarkPreflight =
  systemFixture >>= \fixture ->
    sequence_
      [ void (expectBench "semi-naive closure" (deriveSeededFactClosureWithStateAndConfig (closureRun fixture))),
        void (expectBench "compile fact rules" (compileFactRules closureRules)),
        void (expectBench "check rule set" (checkRuleSet (systemRuleSet fixture))),
        void (expectBench "check raw rewrite system" (checkRawRewriteSystem (systemRawRules fixture))),
        preflightElaborateCheckedRewrites fixture,
        void
          ( expectBench
              "checked system derived insertion"
              ( addComposedPathNamed
                  (systemDerivedName fixture)
                  (systemDerivedPath fixture)
                  (systemCheckedBase fixture)
              )
          ),
        void (evaluate (force (checkedSystemProjectionWeight fixture))),
        void (evaluate (force (guardEncodingWeight fixture)))
      ]

preflightElaborateCheckedRewrites :: SystemFixture -> IO ()
preflightElaborateCheckedRewrites fixture =
  expectBench
    "check raw rewrites for elaboration"
    (checkRawRewrites compileGuard (systemRawRules fixture))
    >>= void
      . expectBench "elaborate checked rewrites"
      . elaborateCheckedRewrites ["first", "second"]

systemFixture :: IO SystemFixture
systemFixture = do
  (_host, rootClass) <-
    expectBench "hostFromTerm fact root" (hostFromTerm (leaf 0))
  firstName <-
    expectBench "rule name first" (mkRuleName "identity.first")
  secondName <-
    expectBench "rule name second" (mkRuleName "identity.second")
  factRules <-
    expectBench "compileFactRules" (compileFactRules closureRules)
  checkedBase <-
    expectBench
      "checked system base"
      (checkRawRewriteSystem (fmap compositionRawRule [0 .. 1023]))
  firstDerivedInput <-
    expectBench "checked system first derived input" (mkRuleName "raw-0")
  secondDerivedInput <-
    expectBench "checked system second derived input" (mkRuleName "raw-1")
  derivedName <-
    expectBench "checked system derived name" (mkRuleName "derived-1024")
  deepGuard <-
    expectBench
      "deep guard"
      ( compileGuard
          Set.empty
          (RewriteCondition (guardHasFactTerms (FactId 99) [deepGuardTerm]))
      )
  let rawRules =
        rawSystemRules
      checkedRules =
        ruleSet
          [ rule firstName identityPattern identityPattern,
            rule secondName identityPattern identityPattern
          ]
  pure
    SystemFixture
      { systemRootClass = rootClass,
        systemFactRules = factRules,
        systemRuleSet = checkedRules,
        systemRawRules = rawRules,
        systemCheckedBase = checkedBase,
        systemDerivedName = derivedName,
        systemDerivedPath = firstDerivedInput :| [secondDerivedInput],
        systemDeepGuard = deepGuard
      }

semiNaiveClosureWeight :: SystemFixture -> Maybe Int
semiNaiveClosureWeight fixture =
  either (const Nothing) (Just . closureWeight) $
    deriveSeededFactClosureWithStateAndConfig (closureRun fixture)

compileFactRulesWeight :: SystemFixture -> Maybe Int
compileFactRulesWeight _fixture =
  either (const Nothing) (Just . length) $
    compileFactRules closureRules

checkRuleSetWeight :: SystemFixture -> Maybe Int
checkRuleSetWeight fixture =
  either (const Nothing) (Just . length . checkedRuleNames) $
    checkRuleSet (systemRuleSet fixture)

checkRawRewriteSystemWeight :: SystemFixture -> Maybe Int
checkRawRewriteSystemWeight fixture =
  either (const Nothing) (Just . length . checkedRuleNames) $
    checkRawRewriteSystem (systemRawRules fixture)

elaborateCheckedRewritesWeight :: SystemFixture -> Maybe Int
elaborateCheckedRewritesWeight fixture =
  either
    (const Nothing)
    (either (const Nothing) (Just . length) . elaborateCheckedRewrites ["first", "second"])
    (checkRawRewrites compileGuard (systemRawRules fixture))

checkedSystemDerivedInsertionWeight :: SystemFixture -> Maybe Int
checkedSystemDerivedInsertionWeight fixture =
  either
    (const Nothing)
    (Just . length . checkedRewrites)
    ( addComposedPathNamed
        (systemDerivedName fixture)
        (systemDerivedPath fixture)
        (systemCheckedBase fixture)
    )

checkedSystemProjectionWeight :: SystemFixture -> Int
checkedSystemProjectionWeight =
  length . checkedRewrites . systemCheckedBase

guardEncodingWeight :: SystemFixture -> Int
guardEncodingWeight =
  foldl'
    (\digest word -> digest * 16777619 + fromIntegral word)
    17
    . compiledGuardCanonicalNodeWordsWith (const 0) (const 0)
    . systemDeepGuard

deepGuardTerm :: GuardTerm []
deepGuardTerm =
  foldl'
    guardProjectTerm
    (guardRefTerm GuardRoot)
    (replicate 512 (guardChildIndex 0))

closureRun :: SystemFixture -> FactClosureRun Capability Int () [] () ()
closureRun fixture =
  FactClosureRun
    { fcrConfig = defaultSemiNaiveConfig,
      fcrCapabilityResolver = emptyGuardCapabilityResolver,
      fcrInitialFacts = seedStore (systemRootClass fixture),
      fcrSeedDerivations = emptyFactDerivationIndex,
      fcrInitialState = 0,
      fcrMatcher =
        mkSemiNaiveMatcher
          ( \matchCalls _input _rule _host ->
              (matchCalls + 1, Right [((), emptySubstitution)])
          ),
      fcrResolveTerm = resolveRoot (systemRootClass fixture),
      fcrCanonicalClass = id,
      fcrRules = systemFactRules fixture,
      fcrHost = ()
    }

closureWeight :: (Int, SemiNaiveClosure) -> Int
closureWeight (matchCalls, closure) =
  matchCalls
    + factStoreSize (sncFacts closure)
    + length (sncRounds closure)

closureRules :: [FactRule Capability []]
closureRules =
  [ closureRule 1 [] (FactId 1),
    closureRule 2 [FactId 1] (FactId 2),
    closureRule 3 [FactId 2] (FactId 3)
  ]

closureRule :: Int -> [FactId] -> FactId -> FactRule Capability []
closureRule ruleKey positiveFacts targetFact =
  FactRule
    { frId = FactRuleId ruleKey,
      frName = "closure-" <> show ruleKey,
      frPattern = identityPattern,
      frProjection = [closureRootRef],
      frFactId = targetFact,
      frCondition =
        case fmap positiveFactGuard positiveFacts of
          [] ->
            Nothing
          [singleGuard] ->
            Just (RewriteCondition singleGuard)
          guards ->
            Just (RewriteCondition (And guards))
    }

positiveFactGuard :: FactId -> GuardExpr Capability []
positiveFactGuard factId =
  guardHasFact factId [closureRootRef]

seedStore :: ClassId -> FactStore
seedStore rootClass =
  insertFact (FactId 0) (FactTuple [rootClass]) emptyFactStore

resolveRoot :: ClassId -> () -> Substitution -> GuardTerm [] -> Maybe ClassId
resolveRoot rootClass () _substitution =
  \case
    GuardRefTerm observedRef | observedRef == closureRootRef ->
      Just rootClass
    _ ->
      Nothing

closureRootRef :: GuardRef
closureRootRef =
  GuardRef (GuardFromRoot, GuardPath [])

identityPattern :: Pattern []
identityPattern =
  PatternVar (mkPatternVar 0)

rawSystemRules :: [RawRewriteRule (RewriteCondition Capability []) []]
rawSystemRules =
  [ rawRule 1,
    rawRule 2
  ]

rawRule :: Int -> RawRewriteRule (RewriteCondition Capability []) []
rawRule ruleKey =
  RawRewriteRule
    { rrId = RewriteRuleId ruleKey,
      rrLhs = identityPattern,
      rrRhs = identityPattern,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

compositionRawRule ::
  Int ->
  RawRewriteRule (RewriteCondition Capability (Node BenchSig)) (Node BenchSig)
compositionRawRule ruleKey =
  RawRewriteRule
    { rrId = RewriteRuleId ruleKey,
      rrLhs = compositionPattern,
      rrRhs = compositionPattern,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

compositionPattern :: Pattern (Node BenchSig)
compositionPattern =
  PatternNode (Node (Leaf 0))
