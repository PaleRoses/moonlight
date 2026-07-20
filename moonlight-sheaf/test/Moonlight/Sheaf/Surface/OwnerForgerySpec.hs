-- | Internal-suite evidence that the old structural digest is diagnostic only.
module Moonlight.Sheaf.Surface.OwnerForgerySpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Sheaf
import Moonlight.Sheaf.Internal.PublicModel
  ( PreparedSite (..),
  )
import Moonlight.Sheaf.Section.Model qualified as Model
import Moonlight.Sheaf.Surface.MiniSiteFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "owner-forgery"
    [ testCase "layout digests remain diagnostic while model owners stay nominal" testLayoutDigestIsNotOwnership
    ]

testLayoutDigestIsNotOwnership :: Assertion
testLayoutDigestIsNotOwnership =
  case
      compile (siteSpec ParentFirstMiniSite) $ \parentFirstPreparedSite ->
        compile (siteSpec ChildFirstMiniSite) $ \childFirstPreparedSite -> do
          let parentFirstModel = preparedSiteModelInternal parentFirstPreparedSite
              childFirstModel = preparedSiteModelInternal childFirstPreparedSite
              sourceEntries = Map.fromList [(Parent, MiniStalk 7), (Child, MiniStalk 7)]
          assertBool
            "reordered dense layouts have distinct diagnostic digests"
            (Model.sheafModelLayoutDigest parentFirstModel /= Model.sheafModelLayoutDigest childFirstModel)
          parentSection <-
            either
              (assertFailure . ("expected parent-first section, received " <>) . show)
              pure
              (section parentFirstPreparedSite sourceEntries)
          childSection <-
            either
              (assertFailure . ("expected child-first section, received " <>) . show)
              pure
              (section childFirstPreparedSite sourceEntries)
          assertEqual "parent owner certifies its section" (Right SectionCertified) (certify orderedMiniAlgebra parentSection)
          assertEqual "child owner certifies its section" (Right SectionCertified) (certify orderedMiniAlgebra childSection)
    of
      Left failure ->
        assertFailure ("expected parent-first prepared site, received " <> show failure)
      Right (Left failure) ->
        assertFailure ("expected child-first prepared site, received " <> show failure)
      Right (Right assertion) ->
        assertion
