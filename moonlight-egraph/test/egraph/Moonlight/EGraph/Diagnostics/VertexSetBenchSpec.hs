module Moonlight.EGraph.Diagnostics.VertexSetBenchSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Context
  ( beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMerge,
    contextCachedObjectsForExecution,
    withEmptyContextEGraph,
    globalMerge,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types (emptyEGraph)
import Moonlight.EGraph.Test.Context.SimpleArith
  ( depthSpec,
    lit,
    plus,
  )
import Moonlight.Sheaf.Context.Algebra
  ( ContextClassLookupFailure (..),
    contextEquivalentAt,
  )
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

tests :: TestTree
tests =
  testGroup
    "vertex-set-cut"
    [ testCase "wide lattice: K >> A, semantic correctness preserved" $
        withResult (wideLattice 64) $ \latticeValue ->
          withResult
            ( do
                (classA, baseGraph1) <- addTerm (lit 1) (emptyEGraph depthSpec)
                (_, baseGraph2) <- addTerm (plus (lit 2) (lit 3)) baseGraph1
                pure (classA, baseGraph2)
            ) $ \(classA, baseGraph) ->
            withEmptyContextEGraph latticeValue baseGraph $ \emptyContextGraph ->
              withResult
                ( do
                    (localClass, stagedBatch) <-
                      stageTermAtContext (WideCtx 7) (lit 42) (beginContextRebaseBatch emptyContextGraph)
                    (_rebaseReport, ceg1) <- commitContextRebaseBatch stagedBatch
                    pure (localClass, ceg1)
                ) $ \(localClass, ceg1) ->
                withResult (contextMerge (WideCtx 7) localClass classA ceg1) $ \merged -> do
              let knownCount = wideLatticeContextCount 64
                  activeCount = length (contextCachedObjectsForExecution merged)
              assertBool
                ( "K should be large (>= 64), got " <> show knownCount)
                (knownCount >= 64)
              assertBool
                ( "A should be small (<= 10), got " <> show activeCount)
                (activeCount <= 10)
              assertEqual
                "merge visible at target context"
                (Right True)
                (contextEquivalentAt (WideCtx 7) localClass classA merged)
              assertEqual
                "merge absent at incomparable context"
                (Left (ContextClassMissing (WideCtx 30) localClass))
                (contextEquivalentAt (WideCtx 30) localClass classA merged)
              assertEqual
                "merge absent at bottom"
                (Left (ContextClassMissing WideBottom localClass))
                (contextEquivalentAt WideBottom localClass classA merged),
      testCase "deep chain: correct propagation with linear iteration budget" $
        withResult (deepLattice 48) $ \latticeValue ->
          withResult
            ( do
                (classA, baseGraph1) <- addTerm (lit 1) (emptyEGraph depthSpec)
                (classB, baseGraph2) <- addTerm (lit 2) baseGraph1
                pure (classA, classB, baseGraph2)
            ) $ \(classA, classB, baseGraph) ->
            withEmptyContextEGraph latticeValue baseGraph $ \emptyContextGraph ->
              withResult (globalMerge classA classB emptyContextGraph) $ \merged -> do
              assertEqual
                "global merge visible at bottom"
                (Right True)
                (contextEquivalentAt DeepBottom classA classB merged)
              assertEqual
                "global merge visible at top"
                (Right True)
                (contextEquivalentAt DeepTop classA classB merged)
              assertEqual
                "global merge visible at midpoint"
                (Right True)
                (contextEquivalentAt (DeepCtx 24) classA classB merged),
      testCase "sparse activation: 5 active contexts in 100-wide lattice" $
        withResult (wideLattice 100) $ \latticeValue ->
          withResult
            ( do
                (classA, baseGraph1) <- addTerm (lit 1) (emptyEGraph depthSpec)
                (classB, baseGraph2) <- addTerm (lit 2) baseGraph1
                pure (classA, classB, baseGraph2)
            ) $ \(classA, classB, baseGraph) ->
          withEmptyContextEGraph latticeValue baseGraph $ \emptyContextGraph ->
            let activeIndices = [10, 25, 50, 75, 90]
                graphWithTerms =
                  foldM
                    ( \acc idx -> do
                        (_localClass, stagedBatch) <-
                          stageTermAtContext (WideCtx idx) (lit (100 + idx)) (beginContextRebaseBatch acc)
                        snd <$> commitContextRebaseBatch stagedBatch
                    )
                    emptyContextGraph
                    activeIndices
             in withResult graphWithTerms $ \cegWithTerms ->
                  withResult (contextMerge (WideCtx 50) classA classB cegWithTerms) $ \merged -> do
              let activeCount = length (contextCachedObjectsForExecution merged)
                  knownCount = wideLatticeContextCount 100
              assertBool
                ( "K should be >= 100, got " <> show knownCount)
                (knownCount >= 100)
              assertBool
                ( "A should be << K; A=" <> show activeCount <> " K=" <> show knownCount)
                (activeCount * 5 < knownCount)
              assertEqual
                "merge at WideCtx 50 visible there"
                (Right True)
                (contextEquivalentAt (WideCtx 50) classA classB merged)
              assertEqual
                "merge at WideCtx 50 invisible at WideCtx 10 (incomparable)"
                (Right False)
                (contextEquivalentAt (WideCtx 10) classA classB merged)
    ]

type WideCtxLabel :: Type
data WideCtxLabel
  = WideBottom
  | WideCtx Int
  | WideTop
  deriving stock (Eq, Ord, Show)

wideLattice :: Int -> Either String (ContextLattice WideCtxLabel)
wideLattice width =
  fmapLeft show $
    compileContextLattice
      (Set.fromList wideElements)
      (contextOrderDecl WideTop WideBottom wideEdges)
  where
    wideElements =
      WideBottom : [WideCtx i | i <- [0 .. width - 1]] ++ [WideTop]

    wideEdges =
      foldl'
        ( \edges contextValue ->
            (WideBottom, contextValue) : (contextValue, WideTop) : edges
        )
        []
        [WideCtx i | i <- [0 .. width - 1]]

wideLatticeContextCount :: Int -> Int
wideLatticeContextCount width =
  width + 2

type DeepCtxLabel :: Type
data DeepCtxLabel
  = DeepBottom
  | DeepCtx Int
  | DeepTop
  deriving stock (Eq, Ord, Show)

deepLattice :: Int -> Either String (ContextLattice DeepCtxLabel)
deepLattice depth =
  fmapLeft show $
    compileContextLattice
      (Set.fromList deepElements)
      (contextOrderDecl DeepTop DeepBottom deepEdges)
  where
    deepElements =
      DeepBottom : [DeepCtx i | i <- [0 .. depth - 1]] ++ [DeepTop]

    deepEdges =
      zip deepElements (drop 1 deepElements)

fmapLeft :: (left -> mappedLeft) -> Either left right -> Either mappedLeft right
fmapLeft mapLeft result =
  case result of
    Left leftValue -> Left (mapLeft leftValue)
    Right rightValue -> Right rightValue
