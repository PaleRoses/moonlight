module Moonlight.Sheaf.Core.PruningSpec
  ( tests,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Sheaf.Pruning
  ( PruningCertificate (..),
    PruningDecision (..),
    PruningGate (..),
    PruningReport (..),
    rejectedPruningDecision,
    pruneWithGate,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "pruning"
    [ testCase "pruneWithGate preserves input order" testPruneWithGatePreservesOrder
    ]

testPruneWithGatePreservesOrder :: Assertion
testPruneWithGatePreservesOrder =
  runIdentity (pruneWithGate gate [1 :: Int, 2, 3, 4])
    @?=
          PruningReport
            { prLive = [(1, "odd-footprint"), (3, "odd-footprint")],
          prPruned =
            [ (2, evenCertificate),
              (4, evenCertificate)
            ]
        }
  where
    evenCertificate =
      PruningCertificate
        { pcObstructions = "even" :| [],
          pcFootprint = "even-footprint",
          pcDiagnostic = Nothing :: Maybe ()
        }

    gate =
      PruningGate
        ( \candidate ->
            pure
              ( if even candidate
                  then rejectedPruningDecision "even-footprint" (Nothing :: Maybe ()) ("even" :| [])
                  else PruningAccepted "odd-footprint"
              )
        )
