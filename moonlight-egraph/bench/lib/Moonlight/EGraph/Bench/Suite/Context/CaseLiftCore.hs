{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Direct context-sensitive benchmark for the case-analysis seam: branch-local
-- facts at alternative carriers, plus an explicit exhaustive-cover parent lift.
-- No Haskell source lowering participates in this lane.
module Moonlight.EGraph.Bench.Suite.Context.CaseLiftCore
  ( caseLiftCoreBenchmarks,
  )
where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (ClassId, UnionFindAllocationError, classIdKey)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMergePlan,
    ContextRebaseBatch,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    emptyContextEGraphFromSite,
    planContextMerges,
    stageContextMerges,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    contextVisibleClassKeys,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    eGraphClassCount,
    eGraphNodeCount,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Context.SimpleArith
  ( ArithF,
    Depth,
    depthSpec,
    lit,
    plus,
  )
import Moonlight.Sheaf.Context.Algebra (contextEquivalentAt)
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

caseLiftCoreBenchmarks :: Benchmark
caseLiftCoreBenchmarks =
  bgroup "case-lift-core" (caseLiftCoreBench <$> caseLiftCoreCases)

caseLiftCoreCases :: [(Int, Int)]
caseLiftCoreCases =
  [(branchCount, termCount) | branchCount <- [4, 8, 16, 32], termCount <- [1000, 4000]]

caseLiftCoreBench :: (Int, Int) -> Benchmark
caseLiftCoreBench (branchCount, termCount) =
  case
    withPreparedContextSiteFromPowersetAtoms
      [0 .. branchCount - 1]
      (caseLiftCoreBenchmarkForSite branchCount termCount)
  of
    Left siteError ->
      env
        (ioError (userError ("case-lift-core site failed: " <> show siteError)) :: IO ())
        (\fixtureFailure -> bench caseLabelValue (nf id fixtureFailure))
    Right benchmark ->
      benchmark
  where
    caseLabelValue = "K=" <> show branchCount <> ",N=" <> show termCount

caseLiftCoreBenchmarkForSite ::
  Int ->
  Int ->
  PreparedContextSite owner CaseContext ->
  Benchmark
caseLiftCoreBenchmarkForSite branchCount termCount site =
  env (prepareCaseLiftCoreFixture site branchCount termCount) $ \fixture ->
    bgroup
      ("K=" <> show branchCount <> ",N=" <> show termCount)
      [ bgroup
          "branch-local-facts"
          [ bench "ours-context" (nf oursBranchLocalDigest fixture),
            bench "product-replay" (nf productBranchLocalDigest fixture)
          ],
        bgroup
          "parent-case-lift"
          [ bench "ours-context" (nf oursParentLiftDigest fixture),
            bench "product-replay" (nf productParentLiftDigest fixture)
          ]
      ]

type CaseContext = Set Int

data CaseLiftCoreFixture owner = CaseLiftCoreFixture
  { clcfBranchCount :: !Int,
    clcfTermCount :: !Int,
    clcfSite :: !(PreparedContextSite owner CaseContext),
    clcfBaseGraph :: !(EGraph ArithF Depth),
    clcfScrutineeClass :: !ClassId,
    clcfRedexClass :: !ClassId,
    clcfReplacementClass :: !ClassId,
    clcfBranches :: ![CaseContext]
  }

instance NFData (CaseLiftCoreFixture owner) where
  rnf fixture =
    clcfBranchCount fixture
      `seq` clcfTermCount fixture
      `seq` classIdKey (clcfScrutineeClass fixture)
      `seq` classIdKey (clcfRedexClass fixture)
      `seq` classIdKey (clcfReplacementClass fixture)
      `seq` length (clcfBranches fixture)
      `seq` ()

prepareCaseLiftCoreFixture ::
  PreparedContextSite owner CaseContext ->
  Int ->
  Int ->
  IO (CaseLiftCoreFixture owner)
prepareCaseLiftCoreFixture site branchCount termCount = do
  fixture <-
    either
      (ioError . userError . ("case-lift-core fixture failed: " <>))
      pure
      (mkCaseLiftCoreFixture site branchCount termCount)
  validatedDigest <-
    either
      (ioError . userError . ("case-lift-core validation failed: " <>))
      (evaluate . force)
      (validateCaseLiftCoreFixture fixture)
  _ <-
    either
      (ioError . userError . ("case-lift-core incomplete-coverage law failed: " <>))
      pure
      (validateIncompleteBranchCoverage fixture)
  putStrLn
    ( "case-lift-core K="
        <> show branchCount
        <> " N="
        <> show termCount
        <> " branch_facts="
        <> show branchCount
        <> " branch_proofs="
        <> show branchCount
        <> " parent_lifts=1 parent_lift_strategy=explicit-stage-after-exhaustive-branch-validation"
        <> " colored=NO_NATIVE_DESCENT slotted=NO_NATIVE_DESCENT digest="
        <> show validatedDigest
    )
  pure fixture

mkCaseLiftCoreFixture ::
  PreparedContextSite owner CaseContext ->
  Int ->
  Int ->
  Either String (CaseLiftCoreFixture owner)
mkCaseLiftCoreFixture site branchCount termCount = do
  (baseGraph, classes) <- buildCaseBaseGraph termCount
  case classes of
    scrutineeClass : redexClass : replacementClass : _ ->
      Right
        CaseLiftCoreFixture
          { clcfBranchCount = branchCount,
            clcfTermCount = termCount,
            clcfSite = site,
            clcfBaseGraph = baseGraph,
            clcfScrutineeClass = scrutineeClass,
            clcfRedexClass = redexClass,
            clcfReplacementClass = replacementClass,
            clcfBranches = singletonBranchContexts branchCount
          }
    _ -> Left "base graph did not produce the required three classes"

buildCaseBaseGraph :: Int -> Either String (EGraph ArithF Depth, [ClassId])
buildCaseBaseGraph termCount =
  if termCount < 3
    then Left "termCount must be at least 3"
    else
      first show $
        fmap (fmap reverse) $
          foldM addOne (emptyEGraph depthSpec, []) (caseBaseTerms termCount)
  where
    addOne :: (EGraph ArithF Depth, [ClassId]) -> Fix ArithF -> Either UnionFindAllocationError (EGraph ArithF Depth, [ClassId])
    addOne (graphValue, classIds) termValue =
      fmap
        (\(classId, nextGraph) -> (nextGraph, classId : classIds))
        (addTerm termValue graphValue)

caseBaseTerms :: Int -> [Fix ArithF]
caseBaseTerms termCount =
  [lit 0, plus (lit 1) (lit 2), lit 1]
    <> fmap (\index -> plus (lit index) (lit (index + 1))) [3 .. termCount - 1]

singletonBranchContexts :: Int -> [CaseContext]
singletonBranchContexts branchCount =
  Set.singleton <$> [0 .. branchCount - 1]

parentContext :: CaseContext
parentContext =
  Set.empty

oursBranchLocalDigest :: CaseLiftCoreFixture owner -> Either String Int
oursBranchLocalDigest fixture =
  stageAllBranchFacts fixture >>= branchLocalDigest

productBranchLocalDigest :: CaseLiftCoreFixture owner -> Either String Int
productBranchLocalDigest fixture =
  sum <$> traverse (stageOneProductBranchFact fixture) (clcfBranches fixture)

oursParentLiftDigest :: CaseLiftCoreFixture owner -> Either String Int
oursParentLiftDigest fixture =
  stageOursParentLift fixture >>= liftedDigest

productParentLiftDigest :: CaseLiftCoreFixture owner -> Either String Int
productParentLiftDigest fixture =
  do
    branchProofDigests <- traverse (stageOneProductBranchProof fixture) (clcfBranches fixture)
    parentGraph <- stageParentLift fixture (baseContextGraph fixture)
    parentDigest <- productLiftDigest parentGraph
    pure (sum branchProofDigests + parentDigest)

validateCaseLiftCoreFixture :: CaseLiftCoreFixture owner -> Either String Int
validateCaseLiftCoreFixture fixture = do
  branchGraph <- stageAllBranchFacts fixture
  proofGraph <- stageAllBranchProofs fixture branchGraph
  validateParentLiftNonVacuity fixture proofGraph

validateIncompleteBranchCoverage :: CaseLiftCoreFixture owner -> Either String ()
validateIncompleteBranchCoverage fixture = do
  branchGraph <- stageAllBranchFacts fixture
  incompleteProofGraph <-
    stageBranchProofsAt
      fixture
      (drop 1 (clcfBranches fixture))
      branchGraph
  case validateParentLiftNonVacuity fixture incompleteProofGraph of
    Left _ -> Right ()
    Right _ -> Left "parent-lift validation accepted incomplete branch coverage"

validateParentLiftNonVacuity ::
  CaseLiftCoreFixture owner ->
  ContextEGraph owner ArithF Depth CaseContext ->
  Either String Int
validateParentLiftNonVacuity fixture proofGraph = do
  parentBefore <- equivalentAt parentContext (clcfRedexClass fixture) (clcfReplacementClass fixture) proofGraph
  branchProofs <- traverse (\branch -> equivalentAt branch (clcfRedexClass fixture) (clcfReplacementClass fixture) proofGraph) (clcfBranches fixture)
  liftedGraph <- stageParentLift fixture proofGraph
  parentAfter <- equivalentAt parentContext (clcfRedexClass fixture) (clcfReplacementClass fixture) liftedGraph
  if and branchProofs && not parentBefore && parentAfter
    then liftedDigest liftedGraph
    else
      Left
        ( "non-vacuity failed: branchProofs="
            <> show branchProofs
            <> " parentBefore="
            <> show parentBefore
            <> " parentAfter="
            <> show parentAfter
        )

stageAllBranchFacts :: CaseLiftCoreFixture owner -> Either String (ContextEGraph owner ArithF Depth CaseContext)
stageAllBranchFacts fixture = do
  construction <-
    foldM
      planBranchFact
      (beginCaseLiftConstruction (baseContextGraph fixture))
      (clcfBranches fixture)
  commitCaseLiftConstruction construction
  where
    planBranchFact construction branchContext = do
      (patternClass, batchWithPattern) <-
        first show
          ( stageTermAtContext
              branchContext
              (branchPatternTerm branchContext)
              (clcBatch construction)
          )
      mergePlan <-
        first show (planContextMerges [branchContext] (clcfScrutineeClass fixture) patternClass batchWithPattern)
      pure
        construction
          { clcBatch = batchWithPattern,
            clcMergePlansReversed = mergePlan : clcMergePlansReversed construction
          }

stageAllBranchProofs ::
  CaseLiftCoreFixture owner ->
  ContextEGraph owner ArithF Depth CaseContext ->
  Either String (ContextEGraph owner ArithF Depth CaseContext)
stageAllBranchProofs fixture contextGraph =
  stageBranchProofsAt fixture (clcfBranches fixture) contextGraph

stageBranchProofsAt ::
  CaseLiftCoreFixture owner ->
  [CaseContext] ->
  ContextEGraph owner ArithF Depth CaseContext ->
  Either String (ContextEGraph owner ArithF Depth CaseContext)
stageBranchProofsAt fixture branches contextGraph =
  foldM
    planBranchProof
    (beginCaseLiftConstruction contextGraph)
    branches
    >>= commitCaseLiftConstruction
  where
    planBranchProof construction branchContext = do
      mergePlan <-
        first show
          ( planContextMerges
              [branchContext]
              (clcfRedexClass fixture)
              (clcfReplacementClass fixture)
              (clcBatch construction)
          )
      pure
        construction
          { clcMergePlansReversed = mergePlan : clcMergePlansReversed construction
          }

stageOursParentLift :: CaseLiftCoreFixture owner -> Either String (ContextEGraph owner ArithF Depth CaseContext)
stageOursParentLift fixture = do
  branchFacts <- stageAllBranchFacts fixture
  branchProofs <- stageAllBranchProofs fixture branchFacts
  stageParentLift fixture branchProofs

stageParentLift ::
  CaseLiftCoreFixture owner ->
  ContextEGraph owner ArithF Depth CaseContext ->
  Either String (ContextEGraph owner ArithF Depth CaseContext)
stageParentLift fixture contextGraph =
  let batchValue = beginContextRebaseBatch contextGraph
   in do
        mergePlan <- first show (planContextMerges [parentContext] (clcfRedexClass fixture) (clcfReplacementClass fixture) batchValue)
        commitGraph =<< first show (stageContextMerges mergePlan batchValue)

stageOneProductBranchFact :: CaseLiftCoreFixture owner -> CaseContext -> Either String Int
stageOneProductBranchFact fixture branchContext =
  do
    (patternClass, batchWithPattern) <-
      first show (stageTermAtContext branchContext (branchPatternTerm branchContext) (beginContextRebaseBatch (baseContextGraph fixture)))
    mergePlan <- first show (planContextMerges [branchContext] (clcfScrutineeClass fixture) patternClass batchWithPattern)
    branchGraph <- commitGraph =<< first show (stageContextMerges mergePlan batchWithPattern)
    branchLocalDigest branchGraph

stageOneProductBranchProof :: CaseLiftCoreFixture owner -> CaseContext -> Either String Int
stageOneProductBranchProof fixture branchContext =
  do
    let batchValue = beginContextRebaseBatch (baseContextGraph fixture)
    mergePlan <- first show (planContextMerges [branchContext] (clcfRedexClass fixture) (clcfReplacementClass fixture) batchValue)
    branchGraph <- commitGraph =<< first show (stageContextMerges mergePlan batchValue)
    branchLocalDigest branchGraph

baseContextGraph :: CaseLiftCoreFixture owner -> ContextEGraph owner ArithF Depth CaseContext
baseContextGraph fixture =
  emptyContextEGraphFromSite (clcfSite fixture) (clcfBaseGraph fixture)

data CaseLiftConstruction owner = CaseLiftConstruction
  { clcBatch :: !(ContextRebaseBatch owner ArithF Depth CaseContext),
    clcMergePlansReversed :: ![ContextMergePlan CaseContext]
  }

beginCaseLiftConstruction :: ContextEGraph owner ArithF Depth CaseContext -> CaseLiftConstruction owner
beginCaseLiftConstruction contextGraph =
  CaseLiftConstruction
    { clcBatch = beginContextRebaseBatch contextGraph,
      clcMergePlansReversed = []
    }

commitCaseLiftConstruction :: CaseLiftConstruction owner -> Either String (ContextEGraph owner ArithF Depth CaseContext)
commitCaseLiftConstruction construction =
  foldM
    (\batchValue mergePlan -> first show (stageContextMerges mergePlan batchValue))
    (clcBatch construction)
    (reverse (clcMergePlansReversed construction))
    >>= commitGraph

commitGraph :: ContextRebaseBatch owner ArithF Depth CaseContext -> Either String (ContextEGraph owner ArithF Depth CaseContext)
commitGraph batchValue =
  snd <$> first show (commitContextRebaseBatch batchValue)

branchPatternTerm :: CaseContext -> Fix ArithF
branchPatternTerm branchContext =
  lit (1000000 + Set.foldl' (+) 0 branchContext)

equivalentAt ::
  CaseContext ->
  ClassId ->
  ClassId ->
  ContextEGraph owner ArithF Depth CaseContext ->
  Either String Bool
equivalentAt contextValue leftClass rightClass contextGraph =
  first show (contextEquivalentAt contextValue leftClass rightClass contextGraph)

branchLocalDigest :: ContextEGraph owner ArithF Depth CaseContext -> Either String Int
branchLocalDigest contextGraph =
  fmap
    (graphShapeDigest contextGraph +)
    (contextDigest parentContext contextGraph)

liftedDigest :: ContextEGraph owner ArithF Depth CaseContext -> Either String Int
liftedDigest contextGraph =
  fmap
    ((graphShapeDigest contextGraph +) . (17 *))
    (contextDigest parentContext contextGraph)

productLiftDigest :: ContextEGraph owner ArithF Depth CaseContext -> Either String Int
productLiftDigest contextGraph =
  fmap
    ((graphShapeDigest contextGraph +) . (31 *))
    (contextDigest parentContext contextGraph)

graphShapeDigest :: ContextEGraph owner ArithF Depth CaseContext -> Int
graphShapeDigest contextGraph =
  eGraphNodeCount (cegBase contextGraph)
    + eGraphClassCount (cegBase contextGraph)

contextDigest :: CaseContext -> ContextEGraph owner ArithF Depth CaseContext -> Either String Int
contextDigest contextValue contextGraph =
  first show
    (IntSet.size <$> contextVisibleClassKeys contextValue contextGraph)
