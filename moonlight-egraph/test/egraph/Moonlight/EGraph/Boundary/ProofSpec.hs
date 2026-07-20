module Moonlight.EGraph.Boundary.ProofSpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Context.Proof (proofAtContext)
import Moonlight.EGraph.Pure.Types
    ( ClassId(ClassId), RewriteRuleId(RewriteRuleId) )
import Moonlight.EGraph.Test.Context.ThreeLevel ( Scope(..) )
import Moonlight.EGraph.Test.Context.ThreeLevelArith
    ( fixtureProofEGraph )
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Moonlight.Rewrite.ProofContext
    ( ProofKind(..), ProofStep(..) )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), assertFailure, testCase )

tests :: TestTree
tests =
  testGroup
    "proof"
    [ testCase "proof is available in every equivalent context" $
        withResult
          ( fixtureProofEGraph $ \proofGraph ->
              case proofAtContext ModuleCtx (ClassId 2) (ClassId 0) proofGraph of
                Right (Just proofStepValue) ->
                  psKind proofStepValue @?= ProofRewrite (RewriteRuleId 0)
                Right Nothing ->
                  assertFailure "expected equivalent context proof at ModuleCtx"
                Left proofError ->
                  assertFailure ("expected proof at ModuleCtx, got " <> show proofError)
          )
          id
    ]
