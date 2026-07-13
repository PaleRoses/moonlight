module Moonlight.EGraph.Core.UnionFindSpec
  ( tests,
  )
where

import Moonlight.Core
    ( UnionFind, UnionFindAllocationError, canonicalMap, emptyUnionFind, equivalent, find, makeSet, union )
import Moonlight.EGraph.Pure.Types ( ClassId (ClassId) )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=) )
import Data.IntMap.Strict qualified as IntMap ( fromList )

tests :: TestTree
tests =
  testGroup "union-find" . hunitCases $
    [ HUnitCase "makeSet allocates deterministically" $
        withResult twoFreshClasses $ \(classId0, classId1, _) ->
          (classId0, classId1) @?= (ClassId 0, ClassId 1),
      HUnitCase "find is idempotent" $
        withResult (makeSet emptyUnionFind) $ \(classId0, unionFind1) ->
          let (firstRoot, unionFind2) = find classId0 unionFind1
              (secondRoot, _) = find firstRoot unionFind2
           in secondRoot @?= firstRoot,
      HUnitCase "merge is commutative on representatives" $
        withResult twoFreshClasses $ \(classId0, classId1, unionFind2) ->
          let leftMerge = union classId0 classId1 unionFind2
              rightMerge = union classId1 classId0 unionFind2
              (leftRoot0, leftRoot1) = rootsOf classId0 classId1 leftMerge
              (rightRoot0, rightRoot1) = rootsOf classId0 classId1 rightMerge
           in (leftRoot0, leftRoot1, rightRoot0, rightRoot1) @?= (ClassId 0, ClassId 0, ClassId 0, ClassId 0),
      HUnitCase "merge is idempotent" $
        withResult (makeSet emptyUnionFind) $ \(classId0, unionFind1) ->
          let merged = union classId0 classId0 unionFind1
              (rootClassId, _) = find classId0 merged
           in rootClassId @?= classId0,
      HUnitCase "equivalent tracks merged classes" $
        withResult twoFreshClasses $ \(classId0, classId1, unionFind2) ->
          equivalent classId0 classId1 (union classId0 classId1 unionFind2) @?= True,
      HUnitCase "canonicalMap compresses to canonical root" $
        withResult twoFreshClasses $ \(classId0, classId1, unionFind2) ->
          canonicalMap (union classId0 classId1 unionFind2) @?= IntMap.fromList [(0, ClassId 0), (1, ClassId 0)]
    ]

twoFreshClasses :: Either UnionFindAllocationError (ClassId, ClassId, UnionFind)
twoFreshClasses = do
  (classId0, unionFind1) <- makeSet emptyUnionFind
  (classId1, unionFind2) <- makeSet unionFind1
  pure (classId0, classId1, unionFind2)

rootsOf :: ClassId -> ClassId -> UnionFind -> (ClassId, ClassId)
rootsOf left right unionFindState =
  let (leftRoot, unionFindAfterLeft) = find left unionFindState
      (rightRoot, _) = find right unionFindAfterLeft
   in (leftRoot, rightRoot)
