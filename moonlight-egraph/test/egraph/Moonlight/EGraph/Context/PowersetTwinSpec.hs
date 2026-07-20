{-# LANGUAGE RankNTypes #-}

-- | Charter oracle: the context suite driven through both preparations of the same small powerset with identical verdicts everywhere.
module Moonlight.EGraph.Context.PowersetTwinSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Moonlight.EGraph.Pure.Context
  ( checkedContextRestrictionMismatchesAt,
    contextCachedObjectsForExecution,
    contextPreparedObjects,
    contextVisibleClassKeys,
  )
import Moonlight.EGraph.Test.Context.Powerset
  ( PowersetTwinObstruction,
    PowersetTwinWorkload (..),
    powersetContextOf,
    powersetTwinAtoms,
    powersetTwinProbeContexts,
    powersetTwinProbePairs,
    powersetTwinWorkload,
  )
import Moonlight.Sheaf.Context.Algebra
  ( contextEquivalentAt,
    propagationTargets,
    restrictionMap,
  )
import Moonlight.Sheaf.Context.Witness (contextRestrictionIdentity)
import Moonlight.Sheaf.Descent.Context
  ( DescentReport (..),
    descentAt,
    fullDescentCheck,
  )
import Moonlight.Sheaf.Obstruction (obstructionReport)
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "powerset twin oracle"
    [ testGroup
        "site discipline"
        [ testCase "enumerable objects agree and are the inhabited join closure" testEnumerableAgreement,
          testCase "cached execution objects agree" testCachedAgreement
        ],
      testGroup
        "matching views"
        [ testCase "visible class keys agree at every probe" testVisibleClassKeyAgreement
        ],
      testGroup
        "guards"
        [ testCase "equivalence verdicts agree and pin upward-only visibility" testEquivalenceVerdicts,
          testCase "restriction maps agree on every probe pair" testRestrictionMaps,
          testCase "restriction identity law agrees and holds at every probe" testRestrictionIdentity,
          testCase "propagation targets agree at every probe" testPropagationTargets
        ],
      testGroup
        "descent"
        [ testCase "full descent reports agree and are satisfied" testFullDescentAgreement,
          testCase "pointwise descent verdicts agree at every probe" testPointwiseDescentAgreement
        ],
      testGroup
        "obstruction"
        [ testCase "obstruction reports agree at every probe" testObstructionReports,
          testCase "restriction mismatch stats agree at every probe" testRestrictionMismatchStats
        ]
    ]

withTwins :: (forall denseOwner symbolicOwner. PowersetTwinWorkload denseOwner symbolicOwner -> Assertion) -> Assertion
withTwins continue =
  withResult (powersetTwinWorkload powersetTwinAtoms continue) id

testEnumerableAgreement :: Assertion
testEnumerableAgreement =
  withTwins $ \workload -> do
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
    contextPreparedObjects denseTwin @?= contextPreparedObjects symbolicTwin
    contextPreparedObjects denseTwin @?= [powersetContextOf "", powersetContextOf "a"]

testCachedAgreement :: Assertion
testCachedAgreement =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in contextCachedObjectsForExecution denseTwin
          @?= contextCachedObjectsForExecution symbolicTwin

testVisibleClassKeyAgreement :: Assertion
testVisibleClassKeyAgreement =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext ->
              contextVisibleClassKeys probeContext denseTwin
                @?= contextVisibleClassKeys probeContext symbolicTwin
          )
          powersetTwinProbeContexts

testEquivalenceVerdicts :: Assertion
testEquivalenceVerdicts =
  withTwins $ \workload -> do
    let classA = ptwClassA workload
        classB = ptwClassB workload
        denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
    traverse_
      ( \probeContext ->
          contextEquivalentAt probeContext classA classB denseTwin
            @?= contextEquivalentAt probeContext classA classB symbolicTwin
      )
      powersetTwinProbeContexts
    contextEquivalentAt (powersetContextOf "a") classA classB denseTwin @?= Right True
    contextEquivalentAt (powersetContextOf "ab") classA classB denseTwin @?= Right True
    contextEquivalentAt (powersetContextOf "abc") classA classB denseTwin @?= Right True
    contextEquivalentAt (powersetContextOf "") classA classB denseTwin @?= Right False
    contextEquivalentAt (powersetContextOf "b") classA classB denseTwin @?= Right False

testRestrictionMaps :: Assertion
testRestrictionMaps =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \(sourceContext, targetContext) ->
              restrictionMap sourceContext targetContext denseTwin
                @?= restrictionMap sourceContext targetContext symbolicTwin
          )
          powersetTwinProbePairs

testRestrictionIdentity :: Assertion
testRestrictionIdentity =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext -> do
              contextRestrictionIdentity probeContext denseTwin
                @?= contextRestrictionIdentity probeContext symbolicTwin
              contextRestrictionIdentity probeContext denseTwin @?= Right True
          )
          powersetTwinProbeContexts

testPropagationTargets :: Assertion
testPropagationTargets =
  withTwins $ \workload ->
    let classA = ptwClassA workload
        classB = ptwClassB workload
        denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext ->
              propagationTargets probeContext classA classB denseTwin
                @?= propagationTargets probeContext classA classB symbolicTwin
          )
          powersetTwinProbeContexts

testFullDescentAgreement :: Assertion
testFullDescentAgreement =
  withTwins $ \workload -> do
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
    let denseReport = fullDescentCheck denseTwin
        symbolicReport = fullDescentCheck symbolicTwin
    denseReport @?= symbolicReport
    drSatisfied denseReport @?= True
    drObstructionCount denseReport @?= 0

testPointwiseDescentAgreement :: Assertion
testPointwiseDescentAgreement =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext ->
              descentAt probeContext denseTwin
                @?= descentAt probeContext symbolicTwin
          )
          powersetTwinProbeContexts

testObstructionReports :: Assertion
testObstructionReports =
  withTwins $ \workload ->
    let classA = ptwClassA workload
        classB = ptwClassB workload
        denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext ->
              (obstructionReport classA classB probeContext denseTwin :: [PowersetTwinObstruction])
                @?= obstructionReport classA classB probeContext symbolicTwin
          )
          powersetTwinProbeContexts

testRestrictionMismatchStats :: Assertion
testRestrictionMismatchStats =
  withTwins $ \workload ->
    let denseTwin = ptwDenseGraph workload
        symbolicTwin = ptwSymbolicGraph workload
     in traverse_
          ( \probeContext ->
              checkedContextRestrictionMismatchesAt probeContext denseTwin
                @?= checkedContextRestrictionMismatchesAt probeContext symbolicTwin
          )
          powersetTwinProbeContexts
