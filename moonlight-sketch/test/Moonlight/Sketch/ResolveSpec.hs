module Moonlight.Sketch.ResolveSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Moonlight.Sketch
  ( RefId,
    SchemaNode (..),
    SchemaRegistry (..),
    detectCycles,
    mkRefId,
    resolve,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

simpleRegistry :: SchemaRegistry
simpleRegistry =
  SchemaRegistry
    ( Map.fromList
        [ (requiredRefId "foo", SBool),
          (requiredRefId "bar", SRef (requiredRefId "foo")),
          (requiredRefId "self", SRef (requiredRefId "self"))
        ]
    )

tests :: TestTree
tests =
  testGroup
    "resolve"
    [ testCase "resolve direct reference" $
        resolve simpleRegistry (SRef (requiredRefId "foo")) @?= SBool,
      testCase "resolve nested reference" $
        resolve simpleRegistry (SRef (requiredRefId "bar")) @?= SBool,
      testCase "missing reference remains ref" $
        resolve simpleRegistry (SRef (requiredRefId "missing")) @?= SRef (requiredRefId "missing"),
      testCase "cycles detected" $
        detectCycles simpleRegistry (SRef (requiredRefId "self")) @?= [requiredRefId "self"],
      testCase "ref identifiers reject invalid tokens" $
        mkRefId "missing/slash" @?= Nothing,
      QC.testProperty "idempotent" $ \(node :: SchemaNode) ->
        resolve simpleRegistry (resolve simpleRegistry node) == resolve simpleRegistry node
    ]

requiredRefId :: Text -> RefId
requiredRefId =
  requiredIdentifier mkRefId

requiredIdentifier :: (Text -> Maybe identifier) -> Text -> identifier
requiredIdentifier mkIdentifier rawIdentifier =
  case mkIdentifier rawIdentifier of
    Just identifier -> identifier
    Nothing -> error "expected valid resolve test identifier"
