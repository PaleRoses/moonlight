module Moonlight.Sketch.InstancesSpec
  ( tests,
  )
where

import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    MeetSemilattice (..),
  )
import Moonlight.Sketch
  ( SchemaNode (..),
    normalize,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "instances"
    [ testCase "join identity" $
        join SBool bottom @?= SBool,
      testCase "meet identity" $
        meet SBool top @?= SBool,
      QC.testProperty "join absorption" $ \(left :: SchemaNode) right ->
        normalize (join left (meet left right)) == normalize left,
      QC.testProperty "meet absorption" $ \(left :: SchemaNode) right ->
        normalize (meet left (join left right)) == normalize left
    ]
