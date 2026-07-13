{-# LANGUAGE OverloadedStrings #-}

module OpticsTests (tests) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TH
import qualified Test.Tasty.QuickCheck as QC
import OpticsTestSupport
  ( AnyMReal,
    Sample,
    canonicalAddressingLaw,
    channelLensProperty,
    deltaAccumulationLaw,
    indexedCompositionCoherence,
    indexedHedgehog,
    readOpticGetterCoherence,
    restrictionCompatibilityProperty,
    restrictionFunctorialProperty,
    sampleFocusLens,
    sampleManyTraversal,
  )
import Moonlight.Optics.Effect.LawNames (LawName (..), lawName)
import Moonlight.Optics.Effect.Laws
  ( lensGetPutLaw,
    lensPutGetLaw,
    lensPutPutLaw,
    prismPreviewReviewLaw,
    prismReviewPreviewLaw,
    traversalCompositionLaw,
    traversalIdentityLaw,
  )
import qualified Moonlight.Optics as VO

lensTests :: Tasty.TestTree
lensTests =
  Tasty.testGroup
    "lens"
    [ QC.testProperty (lawName LensGetPut) (lensGetPutLaw sampleFocusLens),
      QC.testProperty (lawName LensPutGet) (lensPutGetLaw sampleFocusLens),
      QC.testProperty (lawName LensPutPut) (lensPutPutLaw sampleFocusLens),
      QC.testProperty "channel_lens" (channelLensProperty :: AnyMReal -> Bool)
    ]

prismTests :: Tasty.TestTree
prismTests =
  Tasty.testGroup
    "prism"
    [ QC.testProperty (lawName PrismPreviewReview) (prismPreviewReviewLaw VO._Just :: Int -> Bool),
      QC.testProperty (lawName PrismReviewPreview) (prismReviewPreviewLaw VO._Just :: Maybe Int -> Bool)
    ]

readTests :: Tasty.TestTree
readTests =
  Tasty.testGroup
    "read"
    [ QC.testProperty "read_optic_getter_coherence" readOpticGetterCoherence
    ]

restrictionTests :: Tasty.TestTree
restrictionTests =
  Tasty.testGroup
    "restriction"
    [ QC.testProperty (lawName RestrictionFunctorial) restrictionFunctorialProperty,
      QC.testProperty (lawName RestrictionCompat) restrictionCompatibilityProperty
    ]

traversalTests :: Tasty.TestTree
traversalTests =
  Tasty.testGroup
    "traversal"
    [ QC.testProperty (lawName TraversalIdentity) (traversalIdentityLaw sampleManyTraversal),
      QC.testProperty
        (lawName TraversalCompose)
        (\(QC.Fun _ first) (QC.Fun _ second) -> traversalCompositionLaw sampleManyTraversal first second),
      QC.testProperty "delta_accumulation" (deltaAccumulationLaw :: Sample -> Bool)
    ]

indexedTests :: Tasty.TestTree
indexedTests =
  Tasty.testGroup
    "indexed"
    [ QC.testProperty "indexed_compose_coherence" indexedCompositionCoherence,
      TH.testProperty "indexed_compose_hedgehog" indexedHedgehog,
      QC.testProperty "canonical_world_path" canonicalAddressingLaw
    ]

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "moonlight-discrete:optics"
    [ lensTests,
      prismTests,
      readTests,
      restrictionTests,
      traversalTests,
      indexedTests
    ]
