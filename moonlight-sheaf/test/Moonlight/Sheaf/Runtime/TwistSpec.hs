module Moonlight.Sheaf.Runtime.TwistSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Sheaf.Context.Core
  ( ClassSiteSupport,
    ContextLattice,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    fromFiniteLattice,
    preparedSupportFromContexts,
  )
import Moonlight.Sheaf.Twist.Extraction
  ( ContextualExtractionPartition (..),
    ExtractionGate (..),
    contextualExtractionPartitionsWithGate,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedRuleSpec (..),
    rulesActiveAt,
    supportedRuleBook,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    testCase,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( compileContextLattice,
    contextOrderDecl
  )

data TwistCtx
  = BottomCtx
  | LeftCtx
  | RightCtx
  | TopCtx
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "twist"
    [ testCase "SupportedRuleSpec activation is owned by ClassSiteSupport" testSupportedRuleSpecActivation,
      testCase "ExtractionGate filters blocked representatives before grouping" testExtractionGateFiltersBlockedRepresentatives
    ]

testSupportedRuleSpecActivation :: Assertion
testSupportedRuleSpecActivation =
  let book =
        fixtureValue
          "twist fixture rule book"
          ( supportedRuleBook
              twistSite
              [ SupportedRuleSpec (support [BottomCtx]) "everywhere",
                SupportedRuleSpec (support [LeftCtx]) "left-only",
                SupportedRuleSpec (support [RightCtx]) "right-only"
              ]
          )
   in do
        rulesActiveAt twistSite BottomCtx book @?= Right ["everywhere"]
        rulesActiveAt twistSite LeftCtx book @?= Right ["everywhere", "left-only"]
        rulesActiveAt twistSite RightCtx book @?= Right ["everywhere", "right-only"]
        rulesActiveAt twistSite TopCtx book @?= Right ["everywhere", "left-only", "right-only"]

testExtractionGateFiltersBlockedRepresentatives :: Assertion
testExtractionGateFiltersBlockedRepresentatives =
  let gate =
        ExtractionGate
          ( \contextValue resultValue ->
              not (contextValue == RightCtx && resultValue == "shared")
          )
      extractAt contextValue =
        case contextValue of
          BottomCtx -> Nothing
          LeftCtx -> Just "shared"
          RightCtx -> Just "shared"
          TopCtx -> Just "top"
   in contextualExtractionPartitionsWithGate gate [BottomCtx, LeftCtx, RightCtx, TopCtx] extractAt
        @?= [ ContextualExtractionPartition (Set.singleton LeftCtx) "shared",
              ContextualExtractionPartition (Set.singleton TopCtx) "top"
            ]

support :: [TwistCtx] -> ClassSiteSupport TwistCtx
support =
  fixtureValue "twist fixture support"
    . preparedSupportFromContexts twistSite

twistSite :: PreparedContextSite TwistCtx
twistSite =
  fromFiniteLattice twistLattice

twistLattice :: ContextLattice TwistCtx
twistLattice =
  fixtureValue "twist fixture lattice"
    ( compileContextLattice
        (Set.fromList [BottomCtx, LeftCtx, RightCtx, TopCtx])
        ( contextOrderDecl
            TopCtx
            BottomCtx
            [ (BottomCtx, LeftCtx),
              (BottomCtx, RightCtx),
              (LeftCtx, TopCtx),
              (RightCtx, TopCtx)
            ]
        )
    )

fixtureValue :: Show err => String -> Either err value -> value
fixtureValue label =
  either (error . ((label <> ": ") <>) . show) id
