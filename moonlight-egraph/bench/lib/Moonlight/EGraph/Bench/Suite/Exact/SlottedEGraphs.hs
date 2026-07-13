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

module Moonlight.EGraph.Bench.Suite.Exact.SlottedEGraphs
  ( slottedEGraphBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( UnionFindAllocationError,
    ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.EGraph.Bench.Corpus
  ( caseLabel,
    resolveBenchmarkFixturePath,
  )
import Moonlight.EGraph.Bench.Harness.Digest (graphDigest)
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Bench.Suite.Exact.Thesy
  ( SExpr (..),
    parseThesyBundle,
  )
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

slottedEGraphBenchmarks :: Benchmark
slottedEGraphBenchmarks =
  bgroup "slotted-egraphs" slottedEGraphArtifactBenches

data SlottedRiseF a
  = SlottedRiseSymbol !String
  | SlottedRiseVar !String
  | SlottedRiseLam !String !a
  | SlottedRiseApp !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch SlottedRiseF where
  zipMatch =
    zipSameNodeShape

data SlottedBindingMode
  = SlottedNameful
  | SlottedBinderCanonical
  deriving stock (Eq, Ord, Show)

data SlottedFunctionalArrayCase = SlottedFunctionalArrayCase
  { sfacN :: !Int,
    sfacM :: !Int,
    sfacO :: !Int,
    sfacVars :: !Bool,
    sfacLhs :: !SExpr,
    sfacRhs :: !SExpr
  }
  deriving stock (Eq, Ord, Show)

data SlottedFunctionalArrayFixture = SlottedFunctionalArrayFixture
  { sfafTermCount :: !Int,
    sfafCases :: !(NonEmpty SlottedFunctionalArrayCase)
  }
  deriving stock (Eq, Show)

instance NFData SlottedFunctionalArrayFixture where
  rnf fixture =
    sfafTermCount fixture
      `seq` length (NonEmpty.toList (sfafCases fixture))
      `seq` ()

data SlottedFixtureError
  = SlottedSExprParseFailed !FilePath
  | SlottedMalformedFunctionalArrayCase !SExpr
  | SlottedInvalidIntegerAtom !String
  | SlottedInvalidBoolAtom !String
  | SlottedEmptyFunctionalArrayCorpus !FilePath
  | SlottedMalformedRiseTerm !SExpr
  | SlottedClassIdAllocationFailed !UnionFindAllocationError
  deriving stock (Eq, Show)

slottedEGraphArtifactBenches :: [Benchmark]
slottedEGraphArtifactBenches =
  [ bgroup
      "functional-array-language"
      (slottedFunctionalArrayBench <$> slottedFunctionalArrayTermCounts)
  ]

slottedFunctionalArrayTermCounts :: [Int]
slottedFunctionalArrayTermCounts =
  [1000, 100000]

slottedFunctionalArrayBench :: Int -> Benchmark
slottedFunctionalArrayBench termCount =
  env (prepareSlottedFunctionalArrayFixture termCount) $ \fixture ->
    bgroup
      (caseLabel [("roots", termCount)])
      [ bench "nameful-alpha-renamed-build" (nf (slottedFunctionalArrayDigest SlottedNameful) fixture),
        bench "binder-canonical-build" (nf (slottedFunctionalArrayDigest SlottedBinderCanonical) fixture)
      ]

prepareSlottedFunctionalArrayFixture :: Int -> IO SlottedFunctionalArrayFixture
prepareSlottedFunctionalArrayFixture termCount = do
  cases <- readSlottedFunctionalArrayCases
  let fixture =
        SlottedFunctionalArrayFixture
          { sfafTermCount = termCount,
            sfafCases = cases
          }
  putStrLn
    ( "slotted-egraph-artifact/functional-array-language roots="
        <> show termCount
        <> ": exact cases "
        <> show (length (NonEmpty.toList cases))
        <> ", O-plane "
        <> show (slottedFunctionalArrayOValues cases)
        <> ", source N/M/VARS "
        <> show (slottedFunctionalArraySourceShape cases)
    )
  evaluate (rnf fixture) *> pure fixture

slottedFunctionalArrayDigest :: SlottedBindingMode -> SlottedFunctionalArrayFixture -> Either String Int
slottedFunctionalArrayDigest mode fixture =
  first show (graphDigest <$> buildSlottedFunctionalArrayGraph mode fixture)

buildSlottedFunctionalArrayGraph ::
  SlottedBindingMode ->
  SlottedFunctionalArrayFixture ->
  Either SlottedFixtureError (EGraph SlottedRiseF Int)
buildSlottedFunctionalArrayGraph mode fixture =
  foldM
    ( \graphValue (salt, sexpr) -> do
        term <- slottedRiseTermFromSExpr mode salt sexpr
        first SlottedClassIdAllocationFailed (snd <$> addTerm term graphValue)
    )
    (emptyEGraph slottedRiseAnalysisSpec)
    (slottedFunctionalArrayScaledRoots (sfafCases fixture) (sfafTermCount fixture))

slottedFunctionalArrayScaledRoots :: NonEmpty SlottedFunctionalArrayCase -> Int -> [(Int, SExpr)]
slottedFunctionalArrayScaledRoots cases termCount =
  take termCount (zip [0 ..] (cycle (foldMap slottedFunctionalArrayCaseRoots (NonEmpty.toList cases))))

slottedFunctionalArrayCaseRoots :: SlottedFunctionalArrayCase -> [SExpr]
slottedFunctionalArrayCaseRoots benchCase =
  [sfacLhs benchCase, sfacRhs benchCase]

slottedRiseTermFromSExpr :: SlottedBindingMode -> Int -> SExpr -> Either SlottedFixtureError (Fix SlottedRiseF)
slottedRiseTermFromSExpr mode salt =
  slottedRiseTermFromSExprWithEnv mode salt 0 Map.empty

slottedRiseTermFromSExprWithEnv ::
  SlottedBindingMode ->
  Int ->
  Int ->
  Map.Map String String ->
  SExpr ->
  Either SlottedFixtureError (Fix SlottedRiseF)
slottedRiseTermFromSExprWithEnv mode salt binderDepth envBySource sexpr =
  case sexpr of
    SAtom symbolName ->
      Right (Fix (SlottedRiseSymbol symbolName))
    SList [SAtom "var", SAtom variableName] ->
      Right (Fix (SlottedRiseVar (slottedEncodedVariable mode salt variableName envBySource)))
    SList [SAtom "lam", SAtom binderName, body] -> do
      let encodedBinder =
            slottedEncodedBinder mode salt binderDepth binderName
          nextEnv =
            Map.insert binderName encodedBinder envBySource
      encodedBody <- slottedRiseTermFromSExprWithEnv mode salt (binderDepth + 1) nextEnv body
      Right (Fix (SlottedRiseLam encodedBinder encodedBody))
    SList [SAtom "app", functionTerm, argumentTerm] -> do
      encodedFunction <- slottedRiseTermFromSExprWithEnv mode salt binderDepth envBySource functionTerm
      encodedArgument <- slottedRiseTermFromSExprWithEnv mode salt binderDepth envBySource argumentTerm
      Right (Fix (SlottedRiseApp encodedFunction encodedArgument))
    _ ->
      Left (SlottedMalformedRiseTerm sexpr)

slottedEncodedBinder :: SlottedBindingMode -> Int -> Int -> String -> String
slottedEncodedBinder mode salt binderDepth binderName =
  case mode of
    SlottedNameful ->
      slottedSaltedName salt binderName
    SlottedBinderCanonical ->
      "$b" <> show binderDepth

slottedEncodedVariable :: SlottedBindingMode -> Int -> String -> Map.Map String String -> String
slottedEncodedVariable mode salt variableName envBySource =
  case Map.lookup variableName envBySource of
    Just encodedName ->
      encodedName
    Nothing ->
      case mode of
        SlottedNameful ->
          slottedSaltedName salt variableName
        SlottedBinderCanonical ->
          "$free:" <> variableName

slottedSaltedName :: Int -> String -> String
slottedSaltedName salt rawName =
  rawName <> "#" <> show salt

slottedRiseAnalysisSpec :: AnalysisSpec SlottedRiseF Int
slottedRiseAnalysisSpec =
  AnalysisSpec
    { asMake =
        \case
          SlottedRiseSymbol _ ->
            1
          SlottedRiseVar _ ->
            1
          SlottedRiseLam _ bodySize ->
            bodySize + 1
          SlottedRiseApp functionSize argumentSize ->
            functionSize + argumentSize + 1,
      asJoin =
        max,
      asJoinChanged =
        \oldValue newValue ->
          let joinedValue =
                max oldValue newValue
           in (joinedValue, joinedValue /= oldValue)
    }

readSlottedFunctionalArrayCases :: IO (NonEmpty SlottedFunctionalArrayCase)
readSlottedFunctionalArrayCases = do
  path <- resolveBenchmarkFixturePath slottedFunctionalArrayFixturePath
  source <- readFile path
  forms <-
    requireRight
      ("slotted functional-array parse " <> path)
      (first (const (SlottedSExprParseFailed path)) (parseThesyBundle path source))
  cases <-
    requireRight
      "slotted functional-array cases"
      (traverse slottedFunctionalArrayCaseFromSExpr forms)
  requireRight
    "slotted functional-array non-empty corpus"
    ( maybe
        (Left (SlottedEmptyFunctionalArrayCorpus path))
        Right
        (NonEmpty.nonEmpty cases)
    )

slottedFunctionalArrayCaseFromSExpr :: SExpr -> Either SlottedFixtureError SlottedFunctionalArrayCase
slottedFunctionalArrayCaseFromSExpr sexpr =
  case sexpr of
    SList
      [ SAtom "case",
        SList [SAtom "N", SAtom nAtom],
        SList [SAtom "M", SAtom mAtom],
        SList [SAtom "O", SAtom oAtom],
        SList [SAtom "VARS", SAtom varsAtom],
        SList [SAtom "lhs", lhs],
        SList [SAtom "rhs", rhs]
        ] -> do
        nValue <- slottedParseIntAtom nAtom
        mValue <- slottedParseIntAtom mAtom
        oValue <- slottedParseIntAtom oAtom
        varsValue <- slottedParseBoolAtom varsAtom
        Right
          SlottedFunctionalArrayCase
            { sfacN = nValue,
              sfacM = mValue,
              sfacO = oValue,
              sfacVars = varsValue,
              sfacLhs = lhs,
              sfacRhs = rhs
            }
    _ ->
      Left (SlottedMalformedFunctionalArrayCase sexpr)

slottedParseIntAtom :: String -> Either SlottedFixtureError Int
slottedParseIntAtom atom =
  case reads atom of
    [(value, "")] ->
      Right value
    _ ->
      Left (SlottedInvalidIntegerAtom atom)

slottedParseBoolAtom :: String -> Either SlottedFixtureError Bool
slottedParseBoolAtom atom =
  case atom of
    "true" ->
      Right True
    "false" ->
      Right False
    _ ->
      Left (SlottedInvalidBoolAtom atom)

slottedFunctionalArrayOValues :: NonEmpty SlottedFunctionalArrayCase -> [Int]
slottedFunctionalArrayOValues cases =
  sfacO <$> NonEmpty.toList cases

slottedFunctionalArraySourceShape :: NonEmpty SlottedFunctionalArrayCase -> [(Int, Int, Bool)]
slottedFunctionalArraySourceShape cases =
  Set.toAscList
    ( Set.fromList
        [ (sfacN benchCase, sfacM benchCase, sfacVars benchCase)
        | benchCase <- NonEmpty.toList cases
        ]
    )

slottedFunctionalArrayFixturePath :: FilePath
slottedFunctionalArrayFixturePath =
  "bench/fixtures/slotted-egraphs/functional-array-language/n2-m2-o0-10-var.sexp"
