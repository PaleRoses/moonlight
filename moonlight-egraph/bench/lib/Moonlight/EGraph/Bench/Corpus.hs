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

module Moonlight.EGraph.Bench.Corpus
  ( BenchMonoSig,
    BenchRingSig,
    RingCompiledQuery,
    ArithCompiledQuery,
    benchMonoNode,
    benchMonoAnalysisSpec,
    buildArithGraph,
    buildRingGraph,
    ringNum,
    ringAdd,
    addXYPattern,
    arithAddXYPattern,
    arithAddXXPattern,
    compileRingPatternQuery,
    compileArithPatternQuery,
    adjacentPairs,
    requireMergePairs,
    requireFirstPair,
    nonOverlappingPairs,
    resolveBenchmarkFixturePath,
    activateContextMerges,
    mergeableAnatomyRegions,
    caseLabel,
    allBenchmarkAnatomyRegions,
    anatomyPropagationTargets,
  ) where

import Control.Monad (filterM, foldM)
import Data.Fix (Fix (..))
import Data.Hashable (hash)
import Data.Kind (Type)
import Data.List (unfoldr)
import GHC.TypeLits (Symbol)
import Moonlight.Core
  ( ClassId (..),
    Language,
    Pattern (..),
    UnionFindAllocationError,
    ZipMatch (..),
  )
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Bench.Harness.Run (abortBench)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextEGraph,
    contextMerge,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Saturation.Front (Term, node)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    anatomyLeq,
  )
import Moonlight.EGraph.Test.Ring.Core qualified as Ring
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compilePatternQuery,
    singlePatternQuery,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    combineCompiledGuards,
    compileGuard,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

type BenchMonoSig :: (Type -> Type) -> Symbol -> (Symbol -> Type) -> Type
data BenchMonoSig f result r where
  BenchMonoNode :: f (r "Expr") -> BenchMonoSig f "Expr" r

instance Traversable f => HTraversable (BenchMonoSig f) where
  htraverseWithSort transform =
    \case
      BenchMonoNode layer ->
        BenchMonoNode <$> traverse (transform SortWitness) layer

instance (Traversable f, Show (f ())) => RewriteSignature (BenchMonoSig f) where
  type NodeTag (BenchMonoSig f) = f ()

  nodeTag =
    \case
      BenchMonoNode layer -> () <$ layer

  nodeTagDigest _ =
    fromIntegral . hash . show

  nodeResultSort =
    \case
      BenchMonoNode {} -> SortWitness

instance (ZipMatch f, Show (f ())) => ZipMatch (Node (BenchMonoSig f)) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node (BenchMonoNode leftLayer), Node (BenchMonoNode rightLayer)) ->
        Node . BenchMonoNode . fmap adaptBenchMonoZipChild <$> zipMatch leftLayer rightLayer

type BenchRingSig =
  BenchMonoSig Ring.RingF

type RingCompiledQuery =
  CompiledPatternQuery (CompiledGuard SurfaceKind Ring.RingF) Ring.RingF

type ArithCompiledQuery =
  CompiledPatternQuery (CompiledGuard SurfaceKind Arith.ArithF) Arith.ArithF


arithAddXYPattern :: Pattern Arith.ArithF
arithAddXYPattern =
  PatternNode (Arith.Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

arithAddXXPattern :: Pattern Arith.ArithF
arithAddXXPattern =
  PatternNode (Arith.Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 0)))

compileArithPatternQuery :: Pattern Arith.ArithF -> Either [EGraph.PatternVar] ArithCompiledQuery
compileArithPatternQuery patternValue =
  compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue)


activateContextMerges ::
  [(ClassId, ClassId)] ->
  ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion ->
  Either (ContextDeltaError Arith.ArithF AnatomyRegion) (ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion)
activateContextMerges mergePairs contextGraph =
  foldM
    (\graphValue (region, (leftClass, rightClass)) -> contextMerge region leftClass rightClass graphValue)
    contextGraph
    (zip (take contextCount mergeableAnatomyRegions) mergePairs)
  where
    contextCount =
      length mergePairs

mergeableAnatomyRegions :: [AnatomyRegion]
mergeableAnatomyRegions =
  [Upper, Lower, Head, Torso, ArmLeft, ArmRight, LegLeft, LegRight]

benchMonoNode :: f (Term (BenchMonoSig f) "Expr") -> Term (BenchMonoSig f) "Expr"
benchMonoNode =
  node . BenchMonoNode

benchMonoAnalysisSpec :: Functor f => AnalysisSpec f analysis -> AnalysisSpec (Node (BenchMonoSig f)) analysis
benchMonoAnalysisSpec source =
  AnalysisSpec
    { asMake =
        \case
          Node (BenchMonoNode layer) -> asMake source (fmap unK layer),
      asJoin = asJoin source,
      asJoinChanged = asJoinChanged source
    }

adaptBenchMonoZipChild :: (K left sort, K right sort) -> K (left, right) sort
adaptBenchMonoZipChild (leftChild, rightChild) =
  K (unK leftChild, unK rightChild)

buildArithGraph :: Int -> Either UnionFindAllocationError (EGraph Arith.ArithF Arith.NodeCount, [ClassId])
buildArithGraph termCount =
  buildGraphFromTerms Arith.analysisSpec (arithAddTerms termCount)

arithAddTerms :: Int -> [Fix Arith.ArithF]
arithAddTerms termCount =
  (\index -> Arith.addTermNode (Arith.numTerm index) (Arith.numTerm (index + 1)))
    <$> [0 .. termCount - 1]

buildRingGraph :: Int -> Either UnionFindAllocationError (EGraph Ring.RingF Ring.NodeCount, [ClassId])
buildRingGraph termCount =
  buildGraphFromTerms Ring.ringAnalysis (ringAddTerms termCount)

buildGraphFromTerms :: Language f => AnalysisSpec f analysis -> [Fix f] -> Either UnionFindAllocationError (EGraph f analysis, [ClassId])
buildGraphFromTerms analysis terms =
  fmap (fmap reverse) $
    foldM
      ( \(graph, classIds) term ->
          fmap
            (\(classId, nextGraph) -> (nextGraph, classId : classIds))
            (addTerm term graph)
      )
      (emptyEGraph analysis, [])
      terms

ringAddTerms :: Int -> [Fix Ring.RingF]
ringAddTerms termCount =
  (\index -> ringAdd (ringNum index) (ringNum (index + 1)))
    <$> [0 .. termCount - 1]

ringNum :: Int -> Fix Ring.RingF
ringNum value =
  Fix (Ring.Num value)

ringAdd :: Fix Ring.RingF -> Fix Ring.RingF -> Fix Ring.RingF
ringAdd leftTerm rightTerm =
  Fix (Ring.Add leftTerm rightTerm)

addXYPattern :: Pattern Ring.RingF
addXYPattern =
  PatternNode (Ring.Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

compileRingPatternQuery :: Pattern Ring.RingF -> Either [EGraph.PatternVar] RingCompiledQuery
compileRingPatternQuery patternValue =
  compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue)

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs values =
  zip values (drop 1 values)

requireMergePairs :: String -> Int -> [ClassId] -> IO [(ClassId, ClassId)]
requireMergePairs label pairCount classIds =
  let selectedPairs =
        take pairCount (nonOverlappingPairs classIds)
   in if length selectedPairs == pairCount
        then pure selectedPairs
        else abortBench (label <> " fixture expected " <> show pairCount <> " class pairs")

requireFirstPair :: String -> [(ClassId, ClassId)] -> IO (ClassId, ClassId)
requireFirstPair label pairs =
  case pairs of
    firstPair : _ ->
      pure firstPair
    [] ->
      abortBench (label <> " fixture expected at least one class pair")

nonOverlappingPairs :: [a] -> [(a, a)]
nonOverlappingPairs =
  unfoldr $ \case
    leftValue : rightValue : remainingValues ->
      Just ((leftValue, rightValue), remainingValues)
    _ ->
      Nothing

resolveBenchmarkFixturePath :: FilePath -> IO FilePath
resolveBenchmarkFixturePath packageRelativePath = do
  existingPaths <- filterM doesFileExist (benchmarkFixtureCandidates packageRelativePath)
  case existingPaths of
    resolvedPath : _ ->
      pure resolvedPath
    [] ->
      abortBench
        ( "benchmark fixture missing: "
            <> packageRelativePath
            <> "; tried "
            <> show (benchmarkFixtureCandidates packageRelativePath)
        )

benchmarkFixtureCandidates :: FilePath -> [FilePath]
benchmarkFixtureCandidates packageRelativePath =
  [ packageRelativePath,
    "foundation" </> "moonlight-egraph" </> packageRelativePath,
    "compiler" </> "foundation" </> "moonlight-egraph" </> packageRelativePath
  ]

caseLabel :: [(String, Int)] -> String
caseLabel =
  foldr
    ( \(name, value) suffix ->
        name <> "=" <> show value <> if null suffix then "" else "/" <> suffix
    )
    ""

allBenchmarkAnatomyRegions :: [AnatomyRegion]
allBenchmarkAnatomyRegions =
  [minBound .. maxBound]

anatomyPropagationTargets :: AnatomyRegion -> [AnatomyRegion]
anatomyPropagationTargets authoringContext =
  filter (anatomyLeq authoringContext) allBenchmarkAnatomyRegions
