module FiniteComposableSpec
  ( tests,
  )
where

import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FinCat
  ( FinObjectId (..),
    finCatHomMorphism,
    finObjectIdentityMor,
    mkFinObject,
    sampleFinCat,
  )
import Moonlight.Category.Pure.FiniteComposable
  ( FiniteComposableCategory (..),
    chainDimension,
    chainMorphisms,
    chainTerminalObject,
    mkComposableChain,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "FiniteComposable"
    [ testCase "checked chains cache their terminal without changing their morphisms" testCheckedChain,
      testCase "Natural dimension bounds do not overflow through Int" testNaturalDimensionBound,
      testCase "identity construction requires a validated object" testCheckedIdentity
    ]

testCheckedChain :: Assertion
testCheckedChain = do
  object0 <- expectRight "object 0" (mkFinObject sampleFinCat (FinObjectId 0))
  object2 <- expectRight "object 2" (mkFinObject sampleFinCat (FinObjectId 2))
  morphism01 <- expectJust "morphism 0 -> 1" (finCatHomMorphism sampleFinCat (FinObjectId 0) (FinObjectId 1))
  morphism12 <- expectJust "morphism 1 -> 2" (finCatHomMorphism sampleFinCat (FinObjectId 1) (FinObjectId 2))
  case mkComposableChain sampleFinCat object0 [morphism01, morphism12] of
    Left _ -> assertFailure "expected a composable chain"
    Right chainValue -> do
      chainDimension chainValue @?= 2
      chainTerminalObject chainValue @?= object2
      chainMorphisms chainValue @?= [morphism01, morphism12]

testNaturalDimensionBound :: Assertion
testNaturalDimensionBound =
  assertBool
    "a valid enormous Natural bound must retain the dimension-zero chains"
    (not (null (take 1 (enumerateComposableChains sampleFinCat (fromIntegral (maxBound :: Int))))))

testCheckedIdentity :: Assertion
testCheckedIdentity = do
  case mkFinObject sampleFinCat (FinObjectId 99) of
    Left _ -> pure ()
    Right _ -> assertFailure "an undeclared raw object id crossed the validated boundary"
  object0 <- expectRight "object 0" (mkFinObject sampleFinCat (FinObjectId 0))
  identity sampleFinCat object0 @?= Right (finObjectIdentityMor object0)

expectRight :: Show error => String -> Either error value -> IO value
expectRight label result =
  case result of
    Left problem -> assertFailure (label <> ": " <> show problem)
    Right value -> pure value

expectJust :: String -> Maybe value -> IO value
expectJust label result =
  case result of
    Nothing -> assertFailure ("expected " <> label)
    Just value -> pure value
