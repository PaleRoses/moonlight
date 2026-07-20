module Moonlight.Flow.Plan.ResidualSpec (tests) where

import Data.Word (Word64)
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Flow.Plan.Residual
  ( RawResidual (..),
    ResidualContainmentProof (..),
    ResidualContainmentRejection (..),
    ResidualImplicationProof (..),
    ResidualShape (..),
    emptyResidualTheoryRegistry,
    normalizeRawResidual,
    residualContainmentProof,
    residualShapeWords,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Residual"
    [ testProperty "equal digest and words accept as residual-equal" propEqualShapeAccepts,
      testProperty "equal digest with differing words rejects as collision" propCollisionRejects,
      testProperty "digest mismatch rejects regardless of words" propDigestMismatchRejects,
      testProperty "shape word encoding is injective on digest-only shapes" propShapeWordsInjective,
      testProperty "raw digest residual normalizes shape-faithfully" propRawDigestNormalizes
    ]

genDigest :: Gen Word64
genDigest =
  Gen.word64 Range.linearBounded

genWordStream :: Gen [Word64]
genWordStream =
  Gen.list (Range.linear 0 32) genDigest

propEqualShapeAccepts :: Property
propEqualShapeAccepts =
  property $ do
    digestValue <- forAll genDigest
    identityWords <- forAll genWordStream
    let shape = ResidualDigestOnly digestValue identityWords
    residualContainmentProof emptyResidualTheoryRegistry shape shape
      === ResidualContainmentAccepted (ResidualEqualDigest digestValue)

propCollisionRejects :: Property
propCollisionRejects =
  property $ do
    digestValue <- forAll genDigest
    sourceWords <- forAll genWordStream
    requestedWords <- forAll (Gen.filter (/= sourceWords) genWordStream)
    residualContainmentProof
      emptyResidualTheoryRegistry
      (ResidualDigestOnly digestValue sourceWords)
      (ResidualDigestOnly digestValue requestedWords)
      === ResidualContainmentRejected (ResidualDigestCollision digestValue)

propDigestMismatchRejects :: Property
propDigestMismatchRejects =
  property $ do
    sourceDigest <- forAll genDigest
    requestedDigest <- forAll (Gen.filter (/= sourceDigest) genDigest)
    sourceWords <- forAll genWordStream
    requestedWords <- forAll genWordStream
    residualContainmentProof
      emptyResidualTheoryRegistry
      (ResidualDigestOnly sourceDigest sourceWords)
      (ResidualDigestOnly requestedDigest requestedWords)
      === ResidualContainmentRejected (ResidualDigestMismatch sourceDigest requestedDigest)

propShapeWordsInjective :: Property
propShapeWordsInjective =
  property $ do
    leftDigest <- forAll genDigest
    leftWords <- forAll genWordStream
    rightDigest <- forAll genDigest
    rightWords <- forAll genWordStream
    let leftShape = ResidualDigestOnly leftDigest leftWords
        rightShape = ResidualDigestOnly rightDigest rightWords
    (residualShapeWords leftShape == residualShapeWords rightShape)
      === (leftShape == rightShape)

propRawDigestNormalizes :: Property
propRawDigestNormalizes =
  property $ do
    digestValue <- forAll genDigest
    identityWords <- forAll genWordStream
    normalizeRawResidual emptyResidualTheoryRegistry (RawResidualDigest digestValue identityWords)
      === Right (ResidualDigestOnly digestValue identityWords)
