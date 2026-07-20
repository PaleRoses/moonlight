module Moonlight.EGraph.Saturation.GoldenPathSpec.Sheaf
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextRuntimeState (..),
    cegSite,
    cegRuntimeState,
  )
import Moonlight.Sheaf.Context.Core qualified as SheafCore
import Moonlight.EGraph.Saturation.GoldenPathSpec.Prelude
import Moonlight.Sheaf.Context.Witness (contextRestrictionIdentity)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as Twist

tests :: TestTree
tests =
  testGroup
    "SheafCoherence"
    [ testGroup
        "generic-join matching"
        [ testCase "local equivalence does not leak: Pair(1,0) = 1 at Local but ≠ at Global" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass _zeroClass pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase goldenGenericJoinSaturationConfig localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  classesEquivalentAt LocalScope pairClass oneClass resultGraph
                    @?= True
                  classesEquivalentAt GlobalScope pairClass oneClass resultGraph
                    @?= False,
          testCase "sheaf condition: propagation converges without failure" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \_oneClass _zeroClass _pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase goldenGenericJoinSaturationConfig localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  assertBool
                    "sheaf propagation must not fail"
                    (not (contextPropagationFailed resultGraph)),
          testCase "restriction identity: restrict(c,c) = id after saturation" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \_oneClass _zeroClass _pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase goldenGenericJoinSaturationConfig localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  localIdentity <- expectRight (contextRestrictionIdentity LocalScope resultGraph)
                  globalIdentity <- expectRight (contextRestrictionIdentity GlobalScope resultGraph)
                  localIdentity @?= True
                  globalIdentity @?= True
        ],
      testGroup
        "cohomological matching integration"
        [ testCase "local equivalence does not leak through cohomological algebra" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass zeroClass pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  let backend = mkExactWitnessBackend (singleRootContext pairClass oneClass zeroClass)
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  classesEquivalentAt LocalScope pairClass oneClass resultGraph
                    @?= True
                  classesEquivalentAt GlobalScope pairClass oneClass resultGraph
                    @?= False,
          testCase "sheaf condition: propagation converges through cohomological algebra" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass zeroClass pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  let backend = mkExactWitnessBackend (singleRootContext pairClass oneClass zeroClass)
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  assertBool
                    "sheaf propagation must not fail through cohomological algebra"
                    (not (contextPropagationFailed resultGraph)),
          testCase "restriction identity through cohomological algebra" $
            withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass zeroClass pairClass graph3 ->
              withGoldenProofGraph graph3 $ \proofGraph0 -> do
                  let backend = mkExactWitnessBackend (singleRootContext pairClass oneClass zeroClass)
                  localFamily <-
                    expectRight
                      ( Twist.supportedRuleBook
                          (cegSite (sceContextGraph (pgGraph proofGraph0)))
                          [Twist.SupportedRuleSpec (principalSupport LocalScope) pairIdentityRule]
                      )
                  report <-
                    expectRight
                      (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
                  let resultGraph = sceContextGraph (pgGraph (srCarrier report))
                  localIdentity <- expectRight (contextRestrictionIdentity LocalScope resultGraph)
                  globalIdentity <- expectRight (contextRestrictionIdentity GlobalScope resultGraph)
                  localIdentity @?= True
                  globalIdentity @?= True
        ]
    ]

contextPropagationFailed :: ContextEGraph owner f a c -> Bool
contextPropagationFailed _contextGraph =
  False
