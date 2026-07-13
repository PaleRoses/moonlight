module Moonlight.Sheaf.Core.LinearizeSpec
  ( tests,
  )
where

import Moonlight.Homology
  ( BoundaryIncidence,
    emptyBoundaryIncidence,
    identityBoundaryIncidenceOf,
  )
import Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization (..),
    constantRestrictionLinearization,
    identityBoundaryIncidence,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "linearize"
    [ testCase "constant linearization clamps negative dimensions to the empty incidence" testConstantLinearizationClampsNegative,
      testCase "constant linearization is the identity incidence at its dimension" testConstantLinearizationIdentity,
      testCase "identity incidence degenerates to empty at dimension zero" testIdentityIncidenceZero
    ]

testConstantLinearizationClampsNegative :: Assertion
testConstantLinearizationClampsNegative = do
  let linearization = constantRestrictionLinearization (-3) :: StalkLinearization () Int
  slStalkDimension linearization () @?= 0
  slRestrictionIncidence linearization () () @?= emptyBoundaryIncidence

testConstantLinearizationIdentity :: Assertion
testConstantLinearizationIdentity = do
  let linearization = constantRestrictionLinearization 2 :: StalkLinearization () Int
  slStalkDimension linearization () @?= 2
  slRestrictionIncidence linearization () () @?= identityBoundaryIncidenceOf 2

testIdentityIncidenceZero :: Assertion
testIdentityIncidenceZero =
  (identityBoundaryIncidence 0 :: BoundaryIncidence Int) @?= emptyBoundaryIncidence
