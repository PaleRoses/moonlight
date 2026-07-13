-- | Internal-suite forgery evidence: façade certification refuses sections whose owner was swapped behind the abstraction.
module Moonlight.Sheaf.Surface.OwnerForgerySpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Sheaf
import Moonlight.Sheaf.Internal.PublicModel
  ( PreparedSite (..),
    Section (..),
  )
import Moonlight.Sheaf.Section.Model qualified as Model
import Moonlight.Sheaf.Surface.MiniSiteFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "owner-forgery"
    [ testCase "facade certification refuses equal-version equal-cardinality foreign model structure" testFacadeCertificationRejectsForeignModelStructure
    ]

testFacadeCertificationRejectsForeignModelStructure :: Assertion
testFacadeCertificationRejectsForeignModelStructure = do
  parentFirstPreparedSite <-
    either
      (assertFailure . ("expected parent-first prepared site, received " <>) . show)
      pure
      (compile (siteSpec ParentFirstMiniSite))
  childFirstPreparedSite <-
    either
      (assertFailure . ("expected child-first prepared site, received " <>) . show)
      pure
      (compile (siteSpec ChildFirstMiniSite))
  childFirstSection <-
    either
      (assertFailure . ("expected child-first section, received " <>) . show)
      pure
      (section childFirstPreparedSite (Map.fromList [(Parent, MiniStalk 7), (Child, MiniStalk 7)]))
  let parentFirstModel = preparedSiteModelInternal parentFirstPreparedSite
      childFirstModel = preparedSiteModelInternal childFirstPreparedSite
  assertEqual
    "foreign facade models share semantic version"
    (Model.sheafModelVersion parentFirstModel)
    (Model.sheafModelVersion childFirstModel)
  assertEqual
    "foreign facade models share object count"
    (length (Model.modelCells parentFirstModel))
    (length (Model.modelCells childFirstModel))
  assertBool
    "foreign facade models differ in restriction structure"
    (Model.sheafModelFingerprint parentFirstModel /= Model.sheafModelFingerprint childFirstModel)
  let reinterpretedSection =
        childFirstSection
          { sectionOwnerInternal = parentFirstPreparedSite
          }
  case certify orderedMiniAlgebra reinterpretedSection of
    Left
      ( SectionCertificationStoreFailed
          (SectionStoreModelFingerprintMismatch expectedFingerprint actualFingerprint)
        ) ->
        do
          assertEqual
            "facade expected owner fingerprint"
            (Model.sheafModelFingerprint parentFirstModel)
            expectedFingerprint
          assertEqual
            "facade actual section fingerprint"
            (Model.sheafModelFingerprint childFirstModel)
            actualFingerprint
    Left failure ->
      assertFailure ("expected facade fingerprint refusal, received " <> show failure)
    Right certification ->
      assertFailure ("expected facade fingerprint refusal, received " <> show certification)
