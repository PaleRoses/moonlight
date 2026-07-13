{-# LANGUAGE TypeApplications #-}

module Moonlight.Saturation.RuntimeCandidateSpec
  ( runtimeCandidateTests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (MatchActivationIndex (..), SiteProgram (..), SupportIndexedRule (..))
import Moonlight.Saturation.Context.Runtime.Match.Candidates
  ( enumerateContextSiteMatches,
    enumerateProjectedBaseSiteMatches,
  )
import Moonlight.Saturation.Context.Runtime.Round.Input
  ( RoundInput (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( initialPlainRuntimeState,
    rsCarrier,
    rsCore,
    runtimeCoreFactDerivationsAt,
    runtimeCoreFactsAt,
  )
import Moonlight.Saturation.Substrate
  ( SatGraph,
    SatRuleKey,
    supportedMatchBasis,
    supportedMatchWitnesses,
  )
import Moonlight.Saturation.TestSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Moonlight.FiniteLattice
  ( supportBasis
  )
import Moonlight.Sheaf.Context.Site
  ( supportCarrierFromSupport,
  )

runtimeCandidateTests :: TestTree
runtimeCandidateTests =
  testGroup
    "runtime candidate enumeration"
    [ projectedBaseEnumerationRunsOnceAndGluesActivatedContexts,
      contextEnumerationScalesByDistinctRoots,
      supportedRuleCarrierActivatesWithoutContextualExpansion
    ]

expectRight :: Show err => Either err value -> IO value
expectRight eitherValue =
  case eitherValue of
    Right value -> pure value
    Left err -> assertFailure (show err)

seedState :: [Int] -> TestContextState
seedState classIds =
  primeBaseContextState
    (initialPlainRuntimeState @TestSubstrate emptyTestMatchState (graphFromClasses classIds))

roundInputFor ::
  TestContextState ->
  RoundInput TestSubstrate (SatGraph TestSubstrate) (SatRuleKey TestSubstrate)
roundInputFor state =
  let graph =
        rsCarrier state
      core =
        rsCore state
   in RoundInput
        { riState = state,
          riGraph = graph,
          riBaseContext = BaseContext,
          riBaseGraph = graph,
          riBaseFacts = runtimeCoreFactsAt @TestSubstrate BaseContext core,
          riBaseFactDerivations =
            runtimeCoreFactDerivationsAt @TestSubstrate BaseContext core,
          riRewriteContext = (),
          riCapabilityResolver = ()
        }

projectedBaseEnumerationRunsOnceAndGluesActivatedContexts :: TestTree
projectedBaseEnumerationRunsOnceAndGluesActivatedContexts =
  testCase "projected base enumeration runs once and glues activated contexts" $ do
    let baseRule = makeBaseRule 17 [1] False noEffect
        siteProgram =
          (siteProgramWith [baseRule] Map.empty [] Map.empty)
            { spRewriteActivation =
                MatchActivationIndex
                  { maiBase = Set.singleton (trId baseRule),
                    maiContexts =
                      Map.fromList
                        [ (LeftContext, Set.singleton (trId baseRule)),
                          (RightContext, Set.singleton (trId baseRule)),
                          (TopContext, Set.singleton (trId baseRule))
                        ]
                  }
            }
        initialState = seedState [1]
    (matchState, supportedMatches) <-
      expectRight
        ( enumerateProjectedBaseSiteMatches
            @TestSubstrate
            (roundInputFor initialState)
            0
            emptyTestMatchState
            siteProgram
        )
    tmsBaseCalls matchState @?= 1
    expectedSupports <-
      traverse
        expectRight
        [supportBasis testContextLattice [BaseContext, LeftContext, RightContext, TopContext]]
    fmap (supportedMatchBasis @TestSubstrate) supportedMatches
      @?= expectedSupports
    fmap (supportedMatchWitnesses @TestSubstrate) supportedMatches
      @?= [ Map.fromList
              [ (BaseContext, IntSet.singleton 1),
                (LeftContext, IntSet.singleton 1),
                (RightContext, IntSet.singleton 1),
                (TopContext, IntSet.singleton 1)
              ]
          ]

contextEnumerationScalesByDistinctRoots :: TestTree
contextEnumerationScalesByDistinctRoots =
  testCase "context enumeration scales by distinct roots" $ do
    let sharedRule =
          (makeContextRule 29 LeftContext [] False noEffect)
            { trContextRoots =
                Map.fromList
                  [ (LeftContext, [1, 2]),
                    (RightContext, [1, 2]),
                    (TopContext, [1, 2])
                  ]
            }
        contextRules =
          Map.fromList
            [ (LeftContext, [sharedRule]),
              (RightContext, [sharedRule]),
              (TopContext, [sharedRule])
            ]
        initialState = seedState [1, 2]
    (matchState, supportedMatches) <-
      expectRight
        ( enumerateContextSiteMatches
            @TestSubstrate
            (roundInputFor initialState)
            0
            emptyTestMatchState
            contextRules
            []
        )
    tmsContextCalls matchState
      @?= Map.fromList [(BaseContext, 1), (LeftContext, 1), (RightContext, 1), (TopContext, 1)]
    length supportedMatches @?= 2
    expectedSupports <-
      traverse
        expectRight
        [ supportBasis testContextLattice [LeftContext, RightContext, TopContext],
          supportBasis testContextLattice [LeftContext, RightContext, TopContext]
        ]
    fmap (supportedMatchBasis @TestSubstrate) supportedMatches
      @?= expectedSupports
    fmap (supportedMatchWitnesses @TestSubstrate) supportedMatches
      @?= [ Map.fromList
              [ (LeftContext, IntSet.singleton 1),
                (RightContext, IntSet.singleton 1),
                (TopContext, IntSet.singleton 1)
              ],
            Map.fromList
              [ (LeftContext, IntSet.singleton 2),
                (RightContext, IntSet.singleton 2),
                (TopContext, IntSet.singleton 2)
              ]
          ]

supportedRuleCarrierActivatesWithoutContextualExpansion :: TestTree
supportedRuleCarrierActivatesWithoutContextualExpansion =
  testCase "support carrier activates rules without a contextualized rule map" $ do
    supportValue <-
      expectRight (supportBasis testContextLattice [LeftContext])
    supportCarrier <-
      expectRight (supportCarrierFromSupport testPreparedSite supportValue)
    let supportedRule =
          (makeContextRule 31 LeftContext [] False noEffect)
            { trContextRoots =
                Map.fromList
                  [ (LeftContext, [1]),
                    (TopContext, [1])
                  ]
            }
        initialState = seedState [1]
    (matchState, supportedMatches) <-
      expectRight
        ( enumerateContextSiteMatches
            @TestSubstrate
            (roundInputFor initialState)
            0
            emptyTestMatchState
            Map.empty
            [(SupportIndexedRule supportValue supportedRule, supportCarrier)]
        )
    tmsContextCalls matchState
      @?= Map.fromList [(BaseContext, 1), (LeftContext, 1), (RightContext, 1), (TopContext, 1)]
    fmap (supportedMatchBasis @TestSubstrate) supportedMatches
      @?= [supportValue]
    fmap (supportedMatchWitnesses @TestSubstrate) supportedMatches
      @?= [Map.fromList [(LeftContext, IntSet.singleton 1), (TopContext, IntSet.singleton 1)]]
