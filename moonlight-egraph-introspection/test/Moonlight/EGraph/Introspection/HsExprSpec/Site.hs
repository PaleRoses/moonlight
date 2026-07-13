module Moonlight.EGraph.Introspection.HsExprSpec.Site
  ( tests,
  )
where

import Data.Bits (popCount)
import Data.List (nub)
import GHC.Data.FastString (mkFastString)
import GHC.Hs (HsLit (..), HsOverLit (..), OverLitVal (..))
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import GHC.Types.SourceText (IntegralLit (..), SourceText (..))
import Language.Haskell.Syntax.Extension (noExtField)
import Moonlight.Core (BinderId (..), HasConstructorTag (constructorTag))
import Moonlight.EGraph.Introspection.Core.HsExpr
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "site"
    [ testCase "constructor tags are distinct" testConstructorTagsDistinct,
      testCase "tag signatures are closed Word64 bitsets" testTagSignaturesAreClosedWord64Bitsets,
      testCase "normalized literals ignore source text" testNormalizedLitIgnoresSourceText,
      testCase "normalized over-literals ignore source text" testNormalizedOverLitIgnoresSourceText
    ]

testConstructorTagsDistinct :: IO ()
testConstructorTagsDistinct =
  let fieldLabelValue =
        NormalizedFieldLabel
          { nflSelector = "field",
            nflAllowsDuplicateRecordFields = False,
            nflHasSelector = True
          }
      tags =
        [ constructorTag (VarF (GlobalName (mkRdrUnqual (mkVarOcc "x"))) :: HsExprF ()),
          constructorTag (AppF () () :: HsExprF ()),
          constructorTag (LamF (BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "x"))) () :: HsExprF ()),
          constructorTag (LetF (LetMode NonRecursiveBinds LetSyntax) [] () :: HsExprF ()),
          constructorTag (OpAppF () () () :: HsExprF ()),
          constructorTag (SectionLF () () :: HsExprF ()),
          constructorTag (SectionRF () () :: HsExprF ()),
          constructorTag (ParF () :: HsExprF ()),
          constructorTag (LitF (NormalizedInt 1) :: HsExprF ()),
          constructorTag (OverLitF (NormalizedIntegralOverLit 1) :: HsExprF ()),
          constructorTag (IfF () () () :: HsExprF ()),
          constructorTag (CaseF () [(PWildP, ())] :: HsExprF ()),
          constructorTag (DoF [BodyStmtF ()] :: HsExprF ()),
          constructorTag (NegF () :: HsExprF ()),
          constructorTag (ExplicitListF [] :: HsExprF ()),
          constructorTag (ExplicitTupleF [] :: HsExprF ()),
          constructorTag (RecordConF () [(fieldLabelValue, ())] :: HsExprF ()),
          constructorTag (RecordUpdF () [(fieldLabelValue, ())] :: HsExprF ()),
          constructorTag (ArithSeqF (ArithSeqFrom ()) :: HsExprF ()),
          constructorTag (OpaqueF OpaqueOverLabel :: HsExprF ())
        ]
   in assertBool "all HsExpr tags should be pairwise distinct" (length tags == length (nub tags))

testTagSignaturesAreClosedWord64Bitsets :: IO ()
testTagSignaturesAreClosedWord64Bitsets =
  let tags = [minBound .. maxBound] :: [HsExprTag]
      TagSignature signature = foldMap tagSignatureFromTag tags
   in do
        assertBool "HsExprTag universe should fit in Word64" (fromEnum (maxBound :: HsExprTag) < 64)
        assertEqual "folded tag signature should retain one bit per tag" (length tags) (popCount signature)
        assertBool
          "singleton tag signatures should contain their tag"
          (all (\tag -> tagSignatureMember tag (tagSignatureFromTag tag)) tags)

testNormalizedLitIgnoresSourceText :: IO ()
testNormalizedLitIgnoresSourceText =
  let leftLiteral =
        HsInt
          noExtField
          (IL (SourceText (mkFastString "0x10")) False 16)
      rightLiteral =
        HsInt
          noExtField
          (IL NoSourceText False 16)
   in assertEqual
        "normalized HsLit should ignore source-text spelling"
        (normalizeHsLit rightLiteral)
        (normalizeHsLit leftLiteral)

testNormalizedOverLitIgnoresSourceText :: IO ()
testNormalizedOverLitIgnoresSourceText =
  let leftLiteral =
        OverLit
          noExtField
          (HsIntegral (IL (SourceText (mkFastString "0x10")) False 16))
      rightLiteral =
        OverLit
          noExtField
          (HsIntegral (IL NoSourceText False 16))
   in assertEqual
        "normalized HsOverLit should ignore source-text spelling"
        (normalizeHsOverLit rightLiteral)
        (normalizeHsOverLit leftLiteral)
