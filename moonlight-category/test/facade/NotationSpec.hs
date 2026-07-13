{-# LANGUAGE TypeFamilies #-}

module NotationSpec
  ( tests,
  )
where

import Moonlight.Category
  ( Category (..),
    FinCat,
    FinObjectId (..),
    finMorId,
    mkFinObject,
  )
import Moonlight.Category.Notation
  ( cod,
    codObj,
    composeIn,
    dom,
    domObj,
    hom,
    idOf,
    reachableIn,
  )
import Moonlight.Category.Presentation
  ( FinCatBuildError,
    after,
    arrow,
    below,
    equate,
    finCategory,
    object,
    objects,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Notation"
    [ testCase "dom and cod agree with the category's source and target" testDomCodAgreeWithCategory,
      testCase "composeIn realises the categorical composite" testComposeInRealisesComposite,
      testCase "reachableIn reads a preorder, identities included" testReachableInPreorder,
      testCase "idOf is the identity morphism the category provides" testIdOfIsIdentity
    ]

triangle :: Either FinCatBuildError FinCat
triangle =
  finCategory $ do
    a <- object "A"
    b <- object "B"
    c <- object "C"
    f <- arrow a b "f"
    g <- arrow b c "g"
    h <- arrow a c "h"
    equate (g `after` f) h

chain :: Either FinCatBuildError FinCat
chain =
  finCategory $ do
    [x, y, z] <- objects ["x", "y", "z"]
    below x y
    below y z

testDomCodAgreeWithCategory :: Assertion
testDomCodAgreeWithCategory =
  withTriangle $ \category -> do
    f <- expectJust "the morphism 0 -> 1" (hom category (FinObjectId 0) (FinObjectId 1))
    dom f @?= FinObjectId 0
    cod f @?= FinObjectId 1
    source category f @?= Right (domObj f)
    target category f @?= Right (codObj f)

testComposeInRealisesComposite :: Assertion
testComposeInRealisesComposite =
  withTriangle $ \category -> do
    f <- expectJust "the morphism 0 -> 1" (hom category (FinObjectId 0) (FinObjectId 1))
    g <- expectJust "the morphism 1 -> 2" (hom category (FinObjectId 1) (FinObjectId 2))
    h <- expectJust "the morphism 0 -> 2" (hom category (FinObjectId 0) (FinObjectId 2))
    fmap finMorId (composeIn category g f) @?= Right (finMorId h)

testReachableInPreorder :: Assertion
testReachableInPreorder =
  withChain $ \category -> do
    assertBool "0 reaches 2 transitively" (reachableIn category (FinObjectId 0) (FinObjectId 2))
    assertBool "2 does not reach 0" (not (reachableIn category (FinObjectId 2) (FinObjectId 0)))
    assertBool "0 reaches 0 via the identity" (reachableIn category (FinObjectId 0) (FinObjectId 0))

testIdOfIsIdentity :: Assertion
testIdOfIsIdentity =
  withTriangle $ \category -> do
    object0 <- expectRight "the object 0" (mkFinObject category (FinObjectId 0))
    let identityMorphism = idOf object0
    dom identityMorphism @?= FinObjectId 0
    cod identityMorphism @?= FinObjectId 0
    Right identityMorphism @?= identity category object0

withTriangle :: (FinCat -> Assertion) -> Assertion
withTriangle k = either (assertFailure . show) k triangle

withChain :: (FinCat -> Assertion) -> Assertion
withChain k = either (assertFailure . show) k chain

expectJust :: String -> Maybe a -> IO a
expectJust label = maybe (assertFailure ("expected Just for " <> label)) pure

expectRight :: Show e => String -> Either e a -> IO a
expectRight label = either (\buildError -> assertFailure (label <> ": " <> show buildError)) pure
