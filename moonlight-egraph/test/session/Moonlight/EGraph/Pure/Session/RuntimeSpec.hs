{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Session.RuntimeSpec
  ( tests,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec (..),
  )
import Moonlight.EGraph.Pure.Session
  ( EGraphMutationResult (..),
    EGraphMutationTrace (..),
    EGraphScript,
    GraphPhase (..),
    PhaseWitness (..),
    EGraphRebuildTrace,
    rebuildGraph,
    runEGraphScript,
  )
import Moonlight.EGraph.Pure.Types
  ( emptyEGraph,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
  )

type SessionRuntimeNode :: Type -> Type
data SessionRuntimeNode child = SessionRuntimeNode
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

tests :: TestTree
tests =
  testGroup
    "runtime invariants"
    [ testCase "stable no-op rebuild does not materialize a repair index" $
        case runEGraphScript StableWitness stableNoOpRebuildScript (emptyEGraph unitAnalysisSpec) of
          Left scriptError ->
            assertFailure ("unexpected stable rebuild failure: " <> show scriptError)
          Right EGraphMutationResult {emrResult = rebuildReport, emrTrace = mutationTrace} -> do
            assertBool
              "stable no-op rebuild must not allocate a rebuild trace"
              (maybe True (const False) rebuildReport)
            assertBool
              "stable no-op rebuild must not record rebuild traces in the mutation trace"
              (null (emtRebuildTraces mutationTrace))
    ]

stableNoOpRebuildScript ::
  EGraphScript SessionRuntimeNode () 'Stable 'Stable (Maybe (EGraphRebuildTrace SessionRuntimeNode))
stableNoOpRebuildScript =
  rebuildGraph

unitAnalysisSpec :: AnalysisSpec SessionRuntimeNode ()
unitAnalysisSpec =
  AnalysisSpec
    { asMake = const (),
      asJoin = const id,
      asJoinChanged = \_ _ -> ((), False)
    }
