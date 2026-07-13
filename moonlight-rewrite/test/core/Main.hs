{-# LANGUAGE DeriveTraversable #-}

module Main (main) where

import Data.Fix (Fix (..))
import Data.Foldable (traverse_)
import Data.Functor.Classes (Eq1 (..))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Moonlight.Constraint (ConstraintExpr (..))
import Moonlight.Core (BinderId (..), ClassId (..), Pattern (..), ZipMatch (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionPath (..),
    CompiledApplicationCondition,
    CompiledPatternExtension (..),
    PatternExtensionScope (..),
    compiledApplicationCondition,
    compiledApplicationConditionExtensions,
  )
import Moonlight.Rewrite.Runtime
  ( ApplicationConditionAnchor (..),
    ApplicationConditionEvidence (..),
    evaluateCompiledApplicationConditionWithState,
  )
import Moonlight.Core
  ( Substitution,
    emptySubstitution
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compiledSinglePatternQuery,
  )
import Moonlight.Rewrite.Runtime
  ( InstantiationInput (..),
    InstantiationPlan (..),
    InstantiationRef (..),
    InstantiationStep (..),
    compileInstantiationPlan,
  )
import Moonlight.Rewrite.Runtime (RewriteApplicationError (..))
import Moonlight.Rewrite.Runtime
  ( BinderSubstAlgebra (..),
    PostMatchSubst (..),
    PostMatchTerm (..),
    applyPostMatchSubst,
  )
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyAllTerms,
    antiUnifyTerms,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain (testGroup "moonlight-rewrite-core" [antiUnifyTests, postMatchTests, applicationConditionTests, instantiationPlanErrorTests])

antiUnifyTests :: TestTree
antiUnifyTests =
  testGroup
    "anti-unify pure bindings"
    [ testCase "pure binary result binds exact disagreeing subterms with no ClassId payload" testPureBinaryAntiUnifyBindsTerms,
      testCase "pure n-ary result has one binding row per input term" testPureNaryAntiUnifyArity,
      testCase "wide n-ary fixtures preserve pattern, binding rows, and shared structure" testWideNaryAntiUnifyFixtures,
      QC.testProperty "binary anti-unification is the two-term n-ary projection" binaryNaryAgreementProperty
    ]

data AntiNode child
  = AntiLeaf Int
  | AntiPair child child
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 AntiNode where
  liftEq eq leftNode rightNode =
    case (leftNode, rightNode) of
      (AntiLeaf leftValue, AntiLeaf rightValue) ->
        leftValue == rightValue
      (AntiPair leftA leftB, AntiPair rightA rightB) ->
        eq leftA rightA && eq leftB rightB
      _ ->
        False

instance ZipMatch AntiNode where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (AntiLeaf leftValue, AntiLeaf rightValue)
        | leftValue == rightValue ->
            Just (AntiLeaf leftValue)
      (AntiPair leftA leftB, AntiPair rightA rightB) ->
        Just (AntiPair (leftA, rightA) (leftB, rightB))
      _ ->
        Nothing

antiLeaf :: Int -> Fix AntiNode
antiLeaf =
  Fix . AntiLeaf

antiPair :: Fix AntiNode -> Fix AntiNode -> Fix AntiNode
antiPair leftTerm rightTerm =
  Fix (AntiPair leftTerm rightTerm)

antiLeafPattern :: Int -> Pattern AntiNode
antiLeafPattern =
  PatternNode . AntiLeaf

newtype GeneratedAntiTerm = GeneratedAntiTerm (Fix AntiNode)

instance Show GeneratedAntiTerm where
  show _ =
    "GeneratedAntiTerm"

instance QC.Arbitrary GeneratedAntiTerm where
  arbitrary =
    GeneratedAntiTerm <$> QC.sized generatedAntiTerm

generatedAntiTerm :: Int -> QC.Gen (Fix AntiNode)
generatedAntiTerm size
  | size <= 0 =
      antiLeaf <$> QC.arbitrary
  | otherwise =
      QC.frequency
        [ (2, antiLeaf <$> QC.arbitrary),
          ( 3,
            antiPair
              <$> QC.resize childSize (generatedAntiTerm childSize)
              <*> QC.resize childSize (generatedAntiTerm childSize)
          )
        ]
  where
    childSize = size `div` 2

binaryNaryAgreementProperty :: GeneratedAntiTerm -> GeneratedAntiTerm -> Bool
binaryNaryAgreementProperty (GeneratedAntiTerm leftTerm) (GeneratedAntiTerm rightTerm) =
  let binaryResult = antiUnifyTerms leftTerm rightTerm
      naryResult = antiUnifyAllTerms (leftTerm :| [rightTerm])
   in binaryLggPattern binaryResult == naryLggPattern naryResult
        && binaryLggLeftBindings binaryResult :| [binaryLggRightBindings binaryResult] == naryLggBindings naryResult
        && binaryLggSharedStructure binaryResult == naryLggSharedStructure naryResult

testPureBinaryAntiUnifyBindsTerms :: IO ()
testPureBinaryAntiUnifyBindsTerms = do
  let leftTerm =
        antiPair (antiLeaf 1) (antiLeaf 2)
      rightTerm =
        antiPair (antiLeaf 1) (antiLeaf 3)
      result :: BinaryLGGResult AntiNode (Fix AntiNode)
      result =
        antiUnifyTerms leftTerm rightTerm
  binaryLggPattern result
    @?= PatternNode (AntiPair (antiLeafPattern 1) (PatternVar (EGraph.mkPatternVar 0)))
  binaryLggLeftBindings result
    `assertEqualNoShow` IntMap.singleton 0 (antiLeaf 2)
  binaryLggRightBindings result
    `assertEqualNoShow` IntMap.singleton 0 (antiLeaf 3)

testPureNaryAntiUnifyArity :: IO ()
testPureNaryAntiUnifyArity = do
  let firstTerm =
        antiPair (antiLeaf 1) (antiLeaf 2)
      secondTerm =
        antiPair (antiLeaf 1) (antiLeaf 3)
      thirdTerm =
        antiPair (antiLeaf 1) (antiLeaf 4)
      result :: NaryLGGResult AntiNode (Fix AntiNode)
      result =
        antiUnifyAllTerms (firstTerm :| [secondTerm, thirdTerm])
  naryLggPattern result
    @?= PatternNode (AntiPair (antiLeafPattern 1) (PatternVar (EGraph.mkPatternVar 0)))
  NonEmpty.toList (naryLggBindings result)
    `assertEqualNoShow`
      [ IntMap.singleton 0 (antiLeaf 2),
        IntMap.singleton 0 (antiLeaf 3),
        IntMap.singleton 0 (antiLeaf 4)
      ]

data WideAntiNode child
  = WideAntiLeaf !Int
  | WideAntiBranch ![child]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 WideAntiNode where
  liftEq eqChild leftNode rightNode =
    case (leftNode, rightNode) of
      (WideAntiLeaf leftValue, WideAntiLeaf rightValue) ->
        leftValue == rightValue
      (WideAntiBranch leftChildren, WideAntiBranch rightChildren) ->
        liftEq eqChild leftChildren rightChildren
      _ ->
        False

instance ZipMatch WideAntiNode where
  zipMatch =
    EGraph.zipSameNodeShape

testWideNaryAntiUnifyFixtures :: IO ()
testWideNaryAntiUnifyFixtures =
  traverse_
    (uncurry assertWideNaryAntiUnifyFixture)
    ((,) <$> [0, 1, 8, 64, 512] <*> [3, 16])

assertWideNaryAntiUnifyFixture :: Int -> Int -> IO ()
assertWideNaryAntiUnifyFixture arity termCount = do
  let termOffsets = 0 :| [1 .. termCount - 1]
      result = antiUnifyAllTerms (fmap (wideAntiTerm arity) termOffsets)
      expectedPattern =
        PatternNode
          ( WideAntiBranch
              [ PatternVar (EGraph.mkPatternVar childIndex)
                | childIndex <- [0 .. arity - 1]
              ]
          )
      expectedBindingRows =
        fmap
          ( \termOffset ->
              IntMap.fromList
                [ ( childIndex,
                    Fix (WideAntiLeaf (termOffset + childIndex))
                  )
                  | childIndex <- [0 .. arity - 1]
                ]
          )
          termOffsets
  naryLggPattern result @?= expectedPattern
  naryLggBindings result `assertEqualNoShow` expectedBindingRows
  naryLggSharedStructure result @?= 1

wideAntiTerm :: Int -> Int -> Fix WideAntiNode
wideAntiTerm arity termOffset =
  Fix
    ( WideAntiBranch
        [ Fix (WideAntiLeaf (termOffset + childIndex))
          | childIndex <- [0 .. arity - 1]
        ]
    )

assertEqualNoShow :: Eq value => value -> value -> IO ()
assertEqualNoShow actual expected =
  if actual == expected
    then pure ()
    else assertFailure "values were not equal"

postMatchTests :: TestTree
postMatchTests =
  testGroup
    "post-match substitution"
    [ testCase "structured post-match arguments resolve every nested variable" $ do
        applyStructuredArgument completeBindings
          @?= Right (PatternNode (AntiPair (antiLeafPattern 2) (antiLeafPattern 3))),
      testCase "structured post-match arguments report the first missing nested variable" $ do
        applyStructuredArgument (Map.delete secondVariable completeBindings)
          @?= Left secondVariable
    ]
  where
    firstVariable = EGraph.mkPatternVar 0
    secondVariable = EGraph.mkPatternVar 1
    completeBindings =
      Map.fromList
        [ (firstVariable, antiLeafPattern 2),
          (secondVariable, antiLeafPattern 3)
        ]
    structuredArgument =
      PostMatchPattern
        (PatternNode (AntiPair (PatternVar firstVariable) (PatternVar secondVariable)))
    applyStructuredArgument bindings =
      applyPostMatchSubst
        BinderSubstAlgebra
          { bsaSubstituteBinder = \_ resolvedArgument _ -> resolvedArgument
          }
        bindings
        (SubstBinder (BinderId 0) structuredArgument)
        (antiLeafPattern 9)

instantiationPlanErrorTests :: TestTree
instantiationPlanErrorTests =
  testGroup
    "instantiation plan error constructors"
    [ testCase "duplicate instantiation refs report the offending ref key" $
        compilePlanError
          (instantiationPlan [lookupStep 0 0 1, lookupStep 0 1 2] 0)
          @?= Just (RewriteDuplicateInstantiationRef 0),
      testCase "construct steps report the first unavailable input ref key" $
        compilePlanError
          (instantiationPlan [constructStep 0 [priorResult 7]] 0)
          @?= Just (RewriteInstantiationInputUnavailable 7),
      testCase "a root that no step produces stays RewriteMissingInstantiatedNode" $
        compilePlanError
          (instantiationPlan [lookupStep 0 0 1] 1)
          @?= Just RewriteMissingInstantiatedNode,
      testCase "well-formed plans compile" $
        compilePlanError
          (instantiationPlan [lookupStep 0 0 1, constructStep 1 [priorResult 0]] 1)
          @?= Nothing
    ]
  where
    compilePlanError :: InstantiationPlan [] -> Maybe RewriteApplicationError
    compilePlanError =
      either Just (const Nothing) . compileInstantiationPlan

instantiationPlan :: [InstantiationStep []] -> Int -> InstantiationPlan []
instantiationPlan steps rootRef =
  InstantiationPlan {ipSteps = steps, ipRoot = InstantiationRef rootRef}

lookupStep :: Int -> Int -> Int -> InstantiationStep []
lookupStep refKey patternVar classId =
  LookupVar (InstantiationRef refKey) (EGraph.mkPatternVar patternVar) (ClassId classId)

constructStep :: Int -> [InstantiationInput] -> InstantiationStep []
constructStep refKey =
  ConstructTerm (InstantiationRef refKey)

priorResult :: Int -> InstantiationInput
priorResult =
  PriorResult . InstantiationRef

applicationConditionTests :: TestTree
applicationConditionTests =
  testGroup
    "application-condition selective plan"
    [ testCase "static plan extraction sees all possible atom effects" $ do
        let firstExtension = testExtension (ApplicationConditionPath [0])
            secondExtension = testExtension (ApplicationConditionPath [1])
            condition =
              compiledApplicationCondition
                (And [Atom firstExtension, Or [Atom secondExtension]])

        compiledApplicationConditionExtensions condition
          @?= [firstExtension, secondExtension],
      assertShortCircuit "And short-circuits and records only executed atom evidence" And False True,
      assertShortCircuit "Or short-circuits and records only executed atom evidence" Or True False
    ]
  where
    assertShortCircuit label mkCondition firstDecision secondDecision =
      testCase label $ do
        let firstPath = ApplicationConditionPath [0]
            secondPath = ApplicationConditionPath [1]
            condition =
              compiledApplicationCondition
                (mkCondition [Atom (testExtension firstPath), Atom (testExtension secondPath)])
            decisions = Map.fromList [(firstPath, firstDecision), (secondPath, secondDecision)]

        case evaluateWith decisions condition of
          Left errorValue ->
            assertFailure errorValue

          Right (visitedPaths, evidence) -> do
            visitedPaths @?= [firstPath]
            aceAtomResults evidence @?= Map.singleton firstPath firstDecision

data TestNode child = TestNode
  deriving stock (Eq, Ord, Show)

testPattern :: Pattern TestNode
testPattern =
  PatternVar (EGraph.mkPatternVar 0)

testQuery :: CompiledPatternQuery () TestNode
testQuery =
  compiledSinglePatternQuery testPattern Nothing

testExtension :: ApplicationConditionPath -> CompiledPatternExtension () TestNode
testExtension path =
  CompiledPatternExtension
    { cpePath = path,
      cpeQuery = testQuery,
      cpeAnchorVars = Set.empty,
      cpeScope = ExtensionGlobal
    }

testAnchor :: ApplicationConditionAnchor ClassId Substitution
testAnchor =
  ApplicationConditionAnchor
    { acaRoot = ClassId 0,
      acaSubstitution = emptySubstitution
    }

evaluateWith ::
  Map ApplicationConditionPath Bool ->
  CompiledApplicationCondition () TestNode ->
  Either String ([ApplicationConditionPath], ApplicationConditionEvidence ClassId Substitution)
evaluateWith decisions condition =
  evaluateCompiledApplicationConditionWithState
    []
    0
    testAnchor
    (\visitedPaths _ extension ->
       Right
         ( visitedPaths <> [cpePath extension],
           Map.findWithDefault False (cpePath extension) decisions
         )
    )
    condition
