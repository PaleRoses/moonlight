module Moonlight.EGraph.Core.AntiUnifySpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core qualified as EGraph
import Moonlight.Core (patternVarKey)
import Moonlight.EGraph.Effect.Harness (antiUnifyGeneralizes)
import Moonlight.EGraph.Pure.AntiUnify
  ( AntiUnifyObstruction (..),
    AntiUnifyInputIndex (..),
    BinaryLGGResult (binaryLggLeftBindings, binaryLggRightBindings, binaryLggSharedStructure),
    NaryLGGResult (naryLggBindings, naryLggSharedStructure),
    antiUnify,
    antiUnifyAll,
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Extraction (depthCost)
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph)
import Moonlight.EGraph.Test.Arith.Core (ArithF, NodeCount)
import Moonlight.EGraph.Test.Arith.Fixture
  ( classOfArith,
    onePlusThree,
    onePlusFour,
    onePlusTwo,
    four,
    seedArithPair,
    three,
    two,
  )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.Pale.Test.Site.Assertion (expectRight, withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure)

tests :: TestTree
tests =
  testGroup "anti-unify" . hunitCases $
    [ HUnitCase "anti-unify generalizes both classes" $
        withResult fixtureGraph $ \(leftClassId, rightClassId, graph) ->
          assertBool "expected generalization" (antiUnifyGeneralizes leftClassId rightClassId graph),
      HUnitCase "anti-unify records shared structure and bindings" $
        withResult fixtureGraph $ \(leftClassId, rightClassId, graph) -> do
          twoClassId <- expectRight (classOfArith two graph)
          threeClassId <- expectRight (classOfArith three graph)
          lggResult <- expectAntiUnifyRight (antiUnify depthCost leftClassId rightClassId graph)
          binaryLggSharedStructure lggResult @?= 2
          lookupBinding 0 (binaryLggLeftBindings lggResult) @?= Just twoClassId
          lookupBinding 0 (binaryLggRightBindings lggResult) @?= Just threeClassId,
      HUnitCase "anti-unify-all records the same n-ary shared spine" $
        withResult fixtureGraph $ \(leftClassId, rightClassId, graph0) -> do
          (thirdClassId, graph) <- expectRight (addTerm onePlusFour graph0)
          twoClassId <- expectRight (classOfArith two graph)
          threeClassId <- expectRight (classOfArith three graph)
          fourClassId <- expectRight (classOfArith four graph)
          lggResult <- expectAntiUnifyRight (antiUnifyAll depthCost (leftClassId :| [rightClassId, thirdClassId]) graph)
          naryLggSharedStructure lggResult @?= 2
          length (NonEmpty.toList (naryLggBindings lggResult)) @?= 3
          fmap (lookupBinding 0) (naryLggBindings lggResult)
            @?= (Just twoClassId :| [Just threeClassId, Just fourClassId]),
      HUnitCase "anti-unify refuses dirty graph instead of fabricating a universal variable" $
        withResult fixtureGraph $ \(leftClassId, rightClassId, graph) ->
          let EGraphMutationResult {emrGraph = dirtyGraph} =
                equateClassesTracked leftClassId rightClassId graph
           in expectAntiUnifyLeft AntiUnifyGraphNotStable (antiUnify depthCost leftClassId rightClassId dirtyGraph),
      HUnitCase "anti-unify refuses a missing class instead of fabricating a universal variable" $
        withResult fixtureGraph $ \(_leftClassId, rightClassId, graph) ->
          let missingClassId = ClassId 999999
           in expectAntiUnifyLeft
                (AntiUnifyClassNotExtractable (AntiUnifyInputIndex 0) missingClassId)
                (antiUnify depthCost missingClassId rightClassId graph),
      HUnitCase "n-ary anti-unification identifies the exact failing input" $
        withResult fixtureGraph $ \(leftClassId, rightClassId, graph) ->
          let missingClassId = ClassId 999999
           in expectAntiUnifyLeft
                (AntiUnifyClassNotExtractable (AntiUnifyInputIndex 1) missingClassId)
                (antiUnifyAll depthCost (leftClassId :| [missingClassId, rightClassId]) graph)
    ]

fixtureGraph :: Either EGraph.UnionFindAllocationError (ClassId, ClassId, EGraph ArithF NodeCount)
fixtureGraph =
  seedArithPair onePlusTwo onePlusThree

lookupBinding :: Int -> IntMap.IntMap value -> Maybe value
lookupBinding varKey =
  IntMap.lookup (patternVarKey (EGraph.mkPatternVar varKey))

expectAntiUnifyRight :: Either AntiUnifyObstruction value -> IO value
expectAntiUnifyRight resultValue =
  case resultValue of
    Right value -> pure value
    Left obstruction -> assertFailure ("expected anti-unification success, got " <> show obstruction)

expectAntiUnifyLeft :: AntiUnifyObstruction -> Either AntiUnifyObstruction value -> IO ()
expectAntiUnifyLeft expectedObstruction resultValue =
  case resultValue of
    Left obstruction -> obstruction @?= expectedObstruction
    Right _ -> assertFailure ("expected anti-unification obstruction " <> show expectedObstruction)
