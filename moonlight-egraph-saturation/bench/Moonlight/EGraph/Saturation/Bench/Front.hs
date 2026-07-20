{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Saturation.Bench.Front
  ( MoonlightFrontSummary (..),
    MoonlightFrontObservedSummary (..),
    runMoonlightFrontCompileOnly,
    runMoonlightFrontSeedOnly,
    runMoonlightFrontSaturation,
    runMoonlightFrontSaturationObserved,
  )
where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import GHC.TypeLits (Symbol)
import Moonlight.Core (ZipMatch (..))
import Moonlight.Algebra
  ( JoinSemilattice (..),
  )
import Moonlight.FiniteLattice
  ( singletonContextLattice
  )
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontObservedReport (..),
    FrontPhase (Authored),
    EGraphFrontReport (..),
    RulesetM,
    SaturationBudget (..),
    Term,
    compileEGraphFront,
    defNamed,
    done,
    egraph,
    frontErrorMessage,
    node,
    rewrite,
    ruleset,
    run,
    runEGraphFront,
    runEGraphFrontObserved,
    runFor,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (..))
import Moonlight.EGraph.Pure.Types
  ( eGraphClassCount,
    eGraphNodeCount,
    emptyEGraphWithTheory,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Moonlight.Saturation.Context.Runtime.Report (reportIterationCount, reportMatchesApplied)
import Moonlight.Saturation.Context.Runtime.Engine (RuntimeIOTiming)

data MoonlightFrontSummary = MoonlightFrontSummary
  { mfsTermCount :: !Int,
    mfsClassCount :: !Int,
    mfsNodeCount :: !Int,
    mfsIterations :: !Int,
    mfsMatchesApplied :: !Int
  }
  deriving stock (Eq, Show)

data MoonlightFrontObservedSummary = MoonlightFrontObservedSummary
  { mfoSummary :: !MoonlightFrontSummary,
    mfoTimings :: ![RuntimeIOTiming]
  }
  deriving stock (Show)

instance NFData MoonlightFrontSummary where
  rnf summary =
    mfsTermCount summary
      `seq` mfsClassCount summary
      `seq` mfsNodeCount summary
      `seq` mfsIterations summary
      `seq` mfsMatchesApplied summary
      `seq` ()

data BenchFrontSig (result :: Symbol) r where
  BenchNumNode :: Int -> BenchFrontSig "Expr" r
  BenchAddNode :: r "Expr" -> r "Expr" -> BenchFrontSig "Expr" r
  BenchMulNode :: r "Expr" -> r "Expr" -> BenchFrontSig "Expr" r
  BenchNegNode :: r "Expr" -> BenchFrontSig "Expr" r

data BenchFrontTag
  = BenchNumTag Int
  | BenchAddTag
  | BenchMulTag
  | BenchNegTag
  deriving stock (Eq, Ord, Show)

newtype BenchFrontAnalysis = BenchFrontAnalysis Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice BenchFrontAnalysis where
  join (BenchFrontAnalysis left) (BenchFrontAnalysis right) =
    BenchFrontAnalysis (max left right)

instance HTraversable BenchFrontSig where
  htraverseWithSort transform =
    \case
      BenchNumNode value ->
        pure (BenchNumNode value)
      BenchAddNode left right ->
        BenchAddNode
          <$> transform SortWitness left
          <*> transform SortWitness right
      BenchMulNode left right ->
        BenchMulNode
          <$> transform SortWitness left
          <*> transform SortWitness right
      BenchNegNode term ->
        BenchNegNode <$> transform SortWitness term

instance RewriteSignature BenchFrontSig where
  type NodeTag BenchFrontSig = BenchFrontTag

  nodeTag =
    \case
      BenchNumNode value -> BenchNumTag value
      BenchAddNode {} -> BenchAddTag
      BenchMulNode {} -> BenchMulTag
      BenchNegNode {} -> BenchNegTag

  nodeTagDigest _ =
    \case
      BenchNumTag value -> fromIntegral (1009 + value)
      BenchAddTag -> 17
      BenchMulTag -> 23
      BenchNegTag -> 29

  nodeResultSort =
    \case
      BenchNumNode {} -> SortWitness
      BenchAddNode {} -> SortWitness
      BenchMulNode {} -> SortWitness
      BenchNegNode {} -> SortWitness

instance ZipMatch (Node BenchFrontSig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node (BenchNumNode left), Node (BenchNumNode right))
        | left == right -> Just (Node (BenchNumNode left))
      (Node (BenchAddNode leftA rightA), Node (BenchAddNode leftB rightB)) ->
        Just (Node (BenchAddNode (zipChild leftA leftB) (zipChild rightA rightB)))
      (Node (BenchMulNode leftA rightA), Node (BenchMulNode leftB rightB)) ->
        Just (Node (BenchMulNode (zipChild leftA leftB) (zipChild rightA rightB)))
      (Node (BenchNegNode left), Node (BenchNegNode right)) ->
        Just (Node (BenchNegNode (zipChild left right)))
      _ ->
        Nothing

zipChild :: K left sortLeft -> K right sortRight -> K (left, right) sortResult
zipChild leftChild rightChild =
  K (unK leftChild, unK rightChild)

runMoonlightFrontSaturation :: Int -> IO (Either String MoonlightFrontSummary)
runMoonlightFrontSaturation rawTermCount = do
  let result =
        first frontErrorMessage $
          summarizeFrontRun termCount <$> runEGraphFront (benchProgram BenchSaturate termCount) emptyBenchFrontGraph
  evaluate (force result)
  where
    termCount =
      max 0 rawTermCount

runMoonlightFrontSaturationObserved :: Int -> IO (Either String MoonlightFrontObservedSummary)
runMoonlightFrontSaturationObserved rawTermCount = do
  frontResult <-
    runEGraphFrontObserved (benchProgram BenchSaturate termCount) emptyBenchFrontGraph
  let result =
        first
          frontErrorMessage
          ( observedSummary termCount <$> frontResult
          )
  _ <- evaluate (force (mfoSummary <$> result))
  pure result
  where
    termCount =
      max 0 rawTermCount

    observedSummary termCountValue observed =
      MoonlightFrontObservedSummary
        { mfoSummary = summarizeFrontRun termCountValue (eforReport observed),
          mfoTimings = eforScheduleTimings observed
        }

runMoonlightFrontCompileOnly :: Int -> IO (Either String ())
runMoonlightFrontCompileOnly rawTermCount = do
  let result =
        first frontErrorMessage $
          () <$ compileEGraphFront (benchProgram BenchSaturate termCount)
  evaluate (force result)
  where
    termCount =
      max 0 rawTermCount

runMoonlightFrontSeedOnly :: Int -> IO (Either String MoonlightFrontSummary)
runMoonlightFrontSeedOnly rawTermCount = do
  let result =
        first frontErrorMessage $
          summarizeFrontRun termCount <$> runEGraphFront (benchProgram BenchSeedOnly termCount) emptyBenchFrontGraph
  evaluate (force result)
  where
    termCount =
      max 0 rawTermCount

summarizeFrontRun ::
  Int ->
  EGraphFrontReport BenchFrontSig BenchFrontAnalysis () () ->
  MoonlightFrontSummary
summarizeFrontRun termCount report =
  let baseGraph = cegBase (sceContextGraph (efrFinalGraph report))
   in MoonlightFrontSummary
        { mfsTermCount = termCount,
          mfsClassCount = eGraphClassCount baseGraph,
          mfsNodeCount = eGraphNodeCount baseGraph,
          mfsIterations =
            getSumMatches
              (foldMap (SumMatches . reportIterationCount . elrSaturation) (efrScheduleReports report)),
          mfsMatchesApplied =
            getSumMatches
              (foldMap (SumMatches . reportMatchesApplied . elrSaturation) (efrScheduleReports report))
        }

newtype SumMatches = SumMatches {getSumMatches :: Int}

instance Semigroup SumMatches where
  SumMatches left <> SumMatches right =
    SumMatches (left + right)

instance Monoid SumMatches where
  mempty =
    SumMatches 0

data BenchFrontRunMode
  = BenchSeedOnly
  | BenchSaturate
  deriving stock (Eq, Show)

benchProgram :: BenchFrontRunMode -> Int -> EGraphFront 'Authored BenchFrontSig BenchFrontAnalysis () ()
benchProgram runMode termCount =
  egraph $ do
    selectedRules <- ruleset @"bench-arith" benchRules
    traverse_ (uncurry defNamed) (benchTerms termCount)
    case runMode of
      BenchSeedOnly ->
        pure ()
      BenchSaturate ->
        run (runFor (benchBudget termCount) selectedRules)
    pure done

benchRules :: RulesetM BenchFrontSig ()
benchRules = do
  rewrite @"add-zero-right" $
    add #x zero ==> #x
  rewrite @"add-zero-left" $
    add zero #x ==> #x
  rewrite @"mul-zero-right" $
    mul #x zero ==> zero
  rewrite @"mul-zero-left" $
    mul zero #x ==> zero
  rewrite @"mul-one-right" $
    mul #x one ==> #x
  rewrite @"mul-one-left" $
    mul one #x ==> #x
  rewrite @"double-negation" $
    neg (neg #x) ==> #x
  rewrite @"neg-zero" $
    neg zero ==> zero
  rewrite @"add-neg-self" $
    add #x (neg #x) ==> zero
  rewrite @"add-self" $
    add #x #x ==> mul (num 2) #x

benchTerms :: Int -> [(String, Term BenchFrontSig "Expr")]
benchTerms termCount =
  foldMap termsForIndex [0 .. termCount - 1]

termsForIndex :: Int -> [(String, Term BenchFrontSig "Expr")]
termsForIndex termIndex =
  let termName suffix =
        "seed-" <> show termIndex <> "-" <> suffix
      term = num termIndex
   in [ (termName "add-zero", add term zero),
        (termName "mul-one", mul term one),
        (termName "mul-zero", mul term zero),
        (termName "double-negation", neg (neg term)),
        (termName "add-neg-self", add term (neg term))
      ]

benchBudget :: Int -> SaturationBudget
benchBudget termCount =
  SaturationBudget
    { sbMaxIterations = 100,
      sbMaxNodes = max 100000 (termCount * 64 + 1024)
    }

emptyBenchFrontGraph :: SaturatingContextEGraph SurfaceKind (Node BenchFrontSig) BenchFrontAnalysis ()
emptyBenchFrontGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraph (singletonContextLattice ()) $
      emptyEGraphWithTheory benchAnalysis emptyTheorySpec

benchAnalysis :: AnalysisSpec (Node BenchFrontSig) BenchFrontAnalysis
benchAnalysis =
  semilatticeAnalysis $
    \case
      Node BenchNumNode {} -> BenchFrontAnalysis 1
      Node (BenchAddNode (K (BenchFrontAnalysis left)) (K (BenchFrontAnalysis right))) ->
        BenchFrontAnalysis (left + right + 1)
      Node (BenchMulNode (K (BenchFrontAnalysis left)) (K (BenchFrontAnalysis right))) ->
        BenchFrontAnalysis (left + right + 1)
      Node (BenchNegNode (K (BenchFrontAnalysis term))) ->
        BenchFrontAnalysis (term + 1)

num :: Int -> Term BenchFrontSig "Expr"
num value =
  node (BenchNumNode value)

zero :: Term BenchFrontSig "Expr"
zero =
  num 0

one :: Term BenchFrontSig "Expr"
one =
  num 1

add :: Term BenchFrontSig "Expr" -> Term BenchFrontSig "Expr" -> Term BenchFrontSig "Expr"
add left right =
  node (BenchAddNode left right)

mul :: Term BenchFrontSig "Expr" -> Term BenchFrontSig "Expr" -> Term BenchFrontSig "Expr"
mul left right =
  node (BenchMulNode left right)

neg :: Term BenchFrontSig "Expr" -> Term BenchFrontSig "Expr"
neg term =
  node (BenchNegNode term)
