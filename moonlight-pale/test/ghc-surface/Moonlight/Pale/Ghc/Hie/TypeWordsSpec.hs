{-# LANGUAGE PatternSynonyms #-}

module Moonlight.Pale.Ghc.Hie.TypeWordsSpec (tests) where

import Data.Array (Array, array)
import GHC.Iface.Ext.Types (HieType (..), HieTypeFlat, TypeIndex)
import GHC.Types.Name (Name, mkSystemName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (mkUnique)
import Language.Haskell.Syntax.Specificity (data Specified)
import Moonlight.Pale.Ghc.Hie.TypeWords (hieTypeIndexTypeWords)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.hie.typewords"
    [ testCase "forall binder names are alpha-normalized" $
        assertEqual
          "forall a. a -> a and forall b. b -> b encode identically"
          (hieTypeIndexTypeWords (forallIdentityTable "a") forallRoot)
          (hieTypeIndexTypeWords (forallIdentityTable "b") forallRoot),
      testCase "free type variables keep their identity" $
        assertBool
          "free a and free b encode differently"
          (hieTypeIndexTypeWords (freeVariableTable "a") freeRoot /= hieTypeIndexTypeWords (freeVariableTable "b") freeRoot)
    ]

forallRoot :: TypeIndex
forallRoot =
  4

freeRoot :: TypeIndex
freeRoot =
  0

forallIdentityTable :: String -> Array TypeIndex HieTypeFlat
forallIdentityTable nameText =
  let binderName = testName nameText
   in array
        (0, 4)
        [ (0, HCoercionTy),
          (1, HTyVarTy binderName),
          (2, HCoercionTy),
          (3, HFunTy 2 1 1),
          (4, HForAllTy ((binderName, 0), Specified) 3)
        ]

freeVariableTable :: String -> Array TypeIndex HieTypeFlat
freeVariableTable nameText =
  array (0, 0) [(0, HTyVarTy (testName nameText))]

testName :: String -> Name
testName nameText =
  mkSystemName (mkUnique 't' (fromIntegral (sum (fmap fromEnum nameText)))) (mkTyVarOcc nameText)
