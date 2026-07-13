module Moonlight.EGraph.Core.RebuildSpec
  ( tests,
  )
where

import Moonlight.Core (find)
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Pure.Types (eGraphRevision, eGraphUnionFind)
import Moonlight.EGraph.Test.Arith.Fixture
  ( insertArith,
    one,
    onePlusOne,
    seedArithPair,
    three,
    two,
    twoPlusOne,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "rebuild"
    [ testCase "rebuild restores congruence after child merge" $ do
        (leftLeafClass, rightLeafClass, graph2) <- expectRight (seedArithPair one two)
        (leftParentClass, graph3) <- expectRight (insertArith onePlusOne graph2)
        (rightParentClass, graph4) <- expectRight (insertArith twoPlusOne graph3)
        let rebuiltGraph = rebuild (merge leftLeafClass rightLeafClass graph4)
            (leftParentRoot, unionFindAfterLeftParent) = find leftParentClass (eGraphUnionFind rebuiltGraph)
            (rightParentRoot, _) = find rightParentClass unionFindAfterLeftParent
        rightParentRoot @?= leftParentRoot,
      testCase "rebuild is idempotent once congruence is restored" $ do
        (leftLeafClass, rightLeafClass, graph) <- expectRight (seedArithPair one two)
        let rebuiltGraph = rebuild (merge leftLeafClass rightLeafClass graph)
            rebuiltAgain = rebuild rebuiltGraph
            (leftRoot, unionFindAfterLeft) = find leftLeafClass (eGraphUnionFind rebuiltGraph)
            (rightRoot, unionFindAfterRight) = find leftLeafClass (eGraphUnionFind rebuiltAgain)
            (otherRoot, _) = find rightLeafClass unionFindAfterLeft
            (otherRootAgain, _) = find rightLeafClass unionFindAfterRight
        (leftRoot, otherRoot) @?= (rightRoot, otherRootAgain),
      testCase "rebuild batches transitive pending merges" $ do
        (leftLeafClass, middleLeafClass, graph2) <- expectRight (seedArithPair one two)
        (rightLeafClass, graph3) <- expectRight (insertArith three graph2)
        let rebuiltGraph =
              rebuild
                ( merge middleLeafClass rightLeafClass
                    (merge leftLeafClass middleLeafClass graph3)
                )
            (leftRoot, unionFindAfterLeft) = find leftLeafClass (eGraphUnionFind rebuiltGraph)
            (middleRoot, unionFindAfterMiddle) = find middleLeafClass unionFindAfterLeft
            (rightRoot, _) = find rightLeafClass unionFindAfterMiddle
        (leftRoot, middleRoot) @?= (middleRoot, rightRoot),
      testCase "staged merge and rebuild advance revisions at their semantic boundaries" $ do
        (leftLeafClass, rightLeafClass, graph) <- expectRight (seedArithPair one two)
        let stagedGraph = merge leftLeafClass rightLeafClass graph
            rebuiltGraph = rebuild stagedGraph
        assertBool "staged merge should advance the revision" (eGraphRevision stagedGraph > eGraphRevision graph)
        assertBool "rebuild should advance the revision while draining staged unions" (eGraphRevision rebuiltGraph > eGraphRevision stagedGraph)
    ]
