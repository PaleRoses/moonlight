{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Bench.Suite.Exact.Thesy
  ( thesyBenchmarks,
    SExpr (..),
    parseThesyBundle,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Fix (Fix (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    UnionFindAllocationError,
    ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.EGraph.Bench.Corpus
  ( caseLabel,
    resolveBenchmarkFixturePath,
  )
import Moonlight.EGraph.Bench.Harness.Digest (graphDigest)
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    emptyEGraph,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

thesyBenchmarks :: Benchmark
thesyBenchmarks =
  bgroup "thesy" thesySuiteBenches

data ThesyF a = ThesyNode !String ![a]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch ThesyF where
  zipMatch =
    zipSameNodeShape

data ThesySuiteName
  = ThesyClam
  | ThesyHipspecRevEquiv
  | ThesyHipspecRotate
  | ThesyIsaplanner
  | ThesyLeonAmortizeQueue
  | ThesyLeonHeap
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype ThesyColor = ThesyColor Int
  deriving stock (Eq, Ord)

instance Show ThesyColor where
  show (ThesyColor colorIndex) =
    "C" <> show colorIndex

data ThesyRule = ThesyRule
  { trName :: !String,
    trLhs :: !SExpr,
    trRhs :: !SExpr
  }
  deriving stock (Eq, Ord, Show)

data ThesySuiteCorpus = ThesySuiteCorpus
  { tscKnownGroundAtoms :: !(Set String),
    tscRules :: ![ThesyRule],
    tscProofGoals :: ![SExpr]
  }
  deriving stock (Eq, Ord, Show)

data ThesySuiteFixture = ThesySuiteFixture
  { tsfSuite :: !ThesySuiteName,
    tsfContextCount :: !Int,
    tsfTermCount :: !Int,
    tsfKnownGroundAtoms :: !(Set String),
    tsfSeedPool :: !(NonEmpty SExpr),
    tsfSeedCount :: !Int,
    tsfRuleCount :: !Int,
    tsfProofGoalCount :: !Int
  }

instance NFData ThesySuiteFixture where
  rnf fixture =
    length (thesySuiteLabel (tsfSuite fixture))
      `seq` tsfContextCount fixture
      `seq` tsfTermCount fixture
      `seq` Set.size (tsfKnownGroundAtoms fixture)
      `seq` tsfSeedCount fixture
      `seq` tsfRuleCount fixture
      `seq` tsfProofGoalCount fixture
      `seq` ()

data SExpr
  = SAtom !String
  | SList ![SExpr]
  deriving stock (Eq, Ord, Show)

data ThesyToken
  = ThesyOpen
  | ThesyClose
  | ThesyAtom !String
  deriving stock (Eq, Ord, Show)

data ThesyFixtureError
  = ThesyBundleParseFailed !FilePath
  | ThesyEmptySeedPool !ThesySuiteName
  deriving stock (Eq, Show)

thesySuiteBenches :: [Benchmark]
thesySuiteBenches =
  [ thesySuiteBench suite contextCount termCount
  | suite <- thesyBenchmarkSuites,
    contextCount <- [1, 2, 4, 8, 16, 32],
    termCount <- thesySuiteTermCounts
  ]

thesySuiteTermCounts :: [Int]
thesySuiteTermCounts =
  [1000, 100000]

thesyBenchmarkSuites :: [ThesySuiteName]
thesyBenchmarkSuites =
  [minBound .. maxBound]

thesySuiteBench :: ThesySuiteName -> Int -> Int -> Benchmark
thesySuiteBench suite contextCount termCount =
  env (prepareThesySuiteFixture suite contextCount termCount) $ \fixture ->
    bgroup
      (thesySuiteLabel suite <> "/" <> caseLabel [("K", contextCount), ("N", termCount)])
      [ bench "our-shared-context-build" (nf thesySharedContextBuildDigest fixture),
        bench "materialized-colored-build" (nf thesyMaterializedBuildDigest fixture)
      ]

prepareThesySuiteFixture :: ThesySuiteName -> Int -> Int -> IO ThesySuiteFixture
prepareThesySuiteFixture suite contextCount termCount = do
  corpus <- readThesySuiteCorpus suite
  seedPool <- requireRight "thesy seed pool" (thesySeedPool suite corpus)
  let fixture =
        ThesySuiteFixture
          { tsfSuite = suite,
            tsfContextCount = contextCount,
            tsfTermCount = termCount,
            tsfKnownGroundAtoms = tscKnownGroundAtoms corpus,
            tsfSeedPool = seedPool,
            tsfSeedCount = length (NonEmpty.toList seedPool),
            tsfRuleCount = length (tscRules corpus),
            tsfProofGoalCount = length (tscProofGoals corpus)
          }
  putStrLn (thesyFixtureHeader fixture corpus)
  evaluate (rnf fixture) *> pure fixture

thesyFixtureHeader :: ThesySuiteFixture -> ThesySuiteCorpus -> String
thesyFixtureHeader fixture corpus =
  "colored-egraph-thesy-suite "
    <> thesySuiteLabel (tsfSuite fixture)
    <> " K="
    <> show (tsfContextCount fixture)
    <> " N="
    <> show (tsfTermCount fixture)
    <> ": exact rules "
    <> show (tsfRuleCount fixture)
    <> ", proof goals "
    <> show (tsfProofGoalCount fixture)
    <> ", seed terms "
    <> show (tsfSeedCount fixture)
    <> ", ground atoms "
    <> show (Set.size (tsfKnownGroundAtoms fixture))
    <> ", active colors "
    <> show (thesyActiveContexts fixture)
    <> ", rule-name digest "
    <> show (thesyRuleNameDigest corpus)

thesyRuleNameDigest :: ThesySuiteCorpus -> Int
thesyRuleNameDigest corpus =
  foldl' (\total rule -> total + length (trName rule)) 0 (tscRules corpus)

thesySharedContextBuildDigest :: ThesySuiteFixture -> Either UnionFindAllocationError Int
thesySharedContextBuildDigest fixture =
  fmap
    (\(baseGraph, _classIds) -> graphDigest baseGraph + length (thesyActiveContexts fixture))
    (buildThesyGraph (tsfKnownGroundAtoms fixture) (tsfSeedPool fixture) (tsfTermCount fixture))

thesyMaterializedBuildDigest :: ThesySuiteFixture -> Either UnionFindAllocationError Int
thesyMaterializedBuildDigest fixture =
  fmap sum $
    traverse
      (\(colorIndex, _contextValue) -> thesyMaterializedColorBuildDigest fixture colorIndex)
      (zip [0 ..] (thesyActiveContexts fixture))

thesyMaterializedColorBuildDigest :: ThesySuiteFixture -> Int -> Either UnionFindAllocationError Int
thesyMaterializedColorBuildDigest fixture colorIndex =
  fmap
    (\(coloredGraph, _classIds) -> graphDigest coloredGraph)
    ( buildThesyGraphWithSaltOffset
          ((colorIndex + 1) * tsfTermCount fixture)
          (tsfKnownGroundAtoms fixture)
          (tsfSeedPool fixture)
          (tsfTermCount fixture)
    )

thesyActiveContexts :: ThesySuiteFixture -> [ThesyColor]
thesyActiveContexts fixture =
  ThesyColor <$> [0 .. tsfContextCount fixture - 1]

readThesySuiteCorpus :: ThesySuiteName -> IO ThesySuiteCorpus
readThesySuiteCorpus suite = do
  path <- resolveBenchmarkFixturePath (thesySuiteFixturePath suite)
  source <- readFile path
  forms <- requireRight ("thesy bundle parse " <> path) (parseThesyBundle path source)
  pure (thesySuiteCorpus forms)

parseThesyBundle :: FilePath -> String -> Either ThesyFixtureError [SExpr]
parseThesyBundle path source =
  case parseThesyForms (thesyTokens source) of
    Just (forms, []) ->
      Right forms
    _ ->
      Left (ThesyBundleParseFailed path)

thesyTokens :: String -> [ThesyToken]
thesyTokens source =
  case thesyNextToken source of
    Just (token, rest) ->
      token : thesyTokens rest
    Nothing ->
      []

thesyNextToken :: String -> Maybe (ThesyToken, String)
thesyNextToken source =
  case thesyDropJunk source of
    [] ->
      Nothing
    '(' : rest ->
      Just (ThesyOpen, rest)
    ')' : rest ->
      Just (ThesyClose, rest)
    rest ->
      let (atom, trailing) =
            span thesyAtomChar rest
       in Just (ThesyAtom atom, trailing)

thesyDropJunk :: String -> String
thesyDropJunk source =
  case dropWhile isSpace source of
    ';' : rest ->
      thesyDropJunk (dropWhile (/= '\n') rest)
    rest ->
      rest

parseThesyForms :: [ThesyToken] -> Maybe ([SExpr], [ThesyToken])
parseThesyForms tokens =
  case tokens of
    [] ->
      Just ([], [])
    ThesyClose : _ ->
      Just ([], tokens)
    _ -> do
      (expr, afterExpr) <- parseThesyExpr tokens
      (exprs, afterForms) <- parseThesyForms afterExpr
      pure (expr : exprs, afterForms)

parseThesyExpr :: [ThesyToken] -> Maybe (SExpr, [ThesyToken])
parseThesyExpr tokens =
  case tokens of
    ThesyAtom atom : rest ->
      Just (SAtom atom, rest)
    ThesyOpen : rest -> do
      (children, afterChildren) <- parseThesyForms rest
      case afterChildren of
        ThesyClose : afterClose ->
          Just (SList children, afterClose)
        _ ->
          Nothing
    ThesyClose : _ ->
      Nothing
    [] ->
      Nothing

thesyAtomChar :: Char -> Bool
thesyAtomChar charValue =
  not (isSpace charValue || charValue == '(' || charValue == ')' || charValue == ';')

thesySuiteCorpus :: [SExpr] -> ThesySuiteCorpus
thesySuiteCorpus forms =
  ThesySuiteCorpus
    { tscKnownGroundAtoms =
        Set.unions (thesyBuiltinGroundAtoms : fmap thesyKnownGroundAtoms forms),
      tscRules =
        foldMap thesyRules forms,
      tscProofGoals =
        [goal | Just goal <- fmap thesyProofGoal forms]
    }

thesyBuiltinGroundAtoms :: Set String
thesyBuiltinGroundAtoms =
  Set.fromList ["false", "true"]

thesyKnownGroundAtoms :: SExpr -> Set String
thesyKnownGroundAtoms sexpr =
  case sexpr of
    SList [SAtom "datatype", _typeName, _parameters, SList constructors] ->
      Set.fromList [constructorName | SList (SAtom constructorName : _) <- constructors]
    SList [SAtom "declare-fun", SAtom functionName, SList [], _resultType] ->
      Set.singleton functionName
    _ ->
      Set.empty

thesyRules :: SExpr -> [ThesyRule]
thesyRules sexpr =
  case sexpr of
    SList [SAtom "=>", SAtom ruleName, lhs, rhs] ->
      [ThesyRule ruleName lhs rhs]
    SList [SAtom "<=>", SAtom ruleName, lhs, rhs] ->
      [ ThesyRule (ruleName <> ":forward") lhs rhs,
        ThesyRule (ruleName <> ":backward") rhs lhs
      ]
    _ ->
      []

thesyProofGoal :: SExpr -> Maybe SExpr
thesyProofGoal sexpr =
  case sexpr of
    SList [SAtom "prove", goal] ->
      Just (thesyStripForall goal)
    _ ->
      Nothing

thesyStripForall :: SExpr -> SExpr
thesyStripForall sexpr =
  case sexpr of
    SList [SAtom "forall", SList _, body] ->
      body
    _ ->
      sexpr

thesySeedPool :: ThesySuiteName -> ThesySuiteCorpus -> Either ThesyFixtureError (NonEmpty SExpr)
thesySeedPool suite corpus =
  maybe
    (Left (ThesyEmptySeedPool suite))
    Right
    (NonEmpty.nonEmpty (thesyRuleSeeds (tscRules corpus) <> tscProofGoals corpus))

thesyRuleSeeds :: [ThesyRule] -> [SExpr]
thesyRuleSeeds rules =
  foldMap (\rule -> [trLhs rule, trRhs rule]) rules

buildThesyGraph :: Set String -> NonEmpty SExpr -> Int -> Either UnionFindAllocationError (EGraph ThesyF Int, [ClassId])
buildThesyGraph knownGroundAtoms seedPool termCount =
  buildThesyGraphWithSaltOffset 0 knownGroundAtoms seedPool termCount

buildThesyGraphWithSaltOffset :: Int -> Set String -> NonEmpty SExpr -> Int -> Either UnionFindAllocationError (EGraph ThesyF Int, [ClassId])
buildThesyGraphWithSaltOffset saltOffset knownGroundAtoms seedPool termCount =
  fmap (fmap reverse) $
  foldM
    ( \(graph, classIds) (salt, seed) ->
        fmap
          (\(classId, nextGraph) -> (nextGraph, classId : classIds))
          (addTerm (thesyTermFromSExpr knownGroundAtoms salt seed) graph)
    )
    (emptyEGraph thesyAnalysisSpec, [])
    (thesyScaledSeeds saltOffset seedPool termCount)

thesyScaledSeeds :: Int -> NonEmpty SExpr -> Int -> [(Int, SExpr)]
thesyScaledSeeds saltOffset seedPool termCount =
  take termCount (zip [saltOffset ..] (concat (repeat (NonEmpty.toList seedPool))))

thesyTermFromSExpr :: Set String -> Int -> SExpr -> Fix ThesyF
thesyTermFromSExpr knownGroundAtoms salt sexpr =
  case sexpr of
    SAtom atom ->
      Fix (ThesyNode (thesyGroundAtom knownGroundAtoms salt atom) [])
    SList (SAtom operatorName : arguments) ->
      Fix (ThesyNode operatorName (fmap (thesyTermFromSExpr knownGroundAtoms salt) arguments))
    SList arguments ->
      Fix (ThesyNode "$list" (fmap (thesyTermFromSExpr knownGroundAtoms salt) arguments))

thesyGroundAtom :: Set String -> Int -> String -> String
thesyGroundAtom knownGroundAtoms salt atom
  | Set.member atom knownGroundAtoms =
      atom
  | otherwise =
      atom <> "#" <> show salt

thesyAnalysisSpec :: AnalysisSpec ThesyF Int
thesyAnalysisSpec =
  AnalysisSpec
    { asMake =
        \(ThesyNode _ children) -> 1 + sum children,
      asJoin =
        max,
      asJoinChanged =
        \oldValue newValue ->
          let joinedValue =
                max oldValue newValue
           in (joinedValue, joinedValue /= oldValue)
    }

thesySuiteFixturePath :: ThesySuiteName -> FilePath
thesySuiteFixturePath suite =
  "bench/fixtures/thesy-cvc4/" <> thesySuiteLabel suite <> ".thbundle"

thesySuiteLabel :: ThesySuiteName -> String
thesySuiteLabel suite =
  case suite of
    ThesyClam ->
      "clam"
    ThesyHipspecRevEquiv ->
      "hipspec-rev-equiv"
    ThesyHipspecRotate ->
      "hipspec-rotate"
    ThesyIsaplanner ->
      "isaplanner"
    ThesyLeonAmortizeQueue ->
      "leon-amortize-queue"
    ThesyLeonHeap ->
      "leon-heap"
