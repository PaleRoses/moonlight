{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Introspection.NerveSpec.Fixture.Site
  ( ArithF (..),
    ArithTag (..),
    TwistScopeCtx (..),
    sampleSpan,
    reverseSpan,
    disjointLeft,
    disjointRight,
    reversibleSystem,
    disjointSystem,
    acyclicChainSystem,
    identifiedAcyclicChainSystem,
    ambiguousIdentifiedSystem,
    singleRuleSystem,
    contextSpanAB,
    contextSpanBC,
    contextRuleAB,
    contextRuleBC,
    multiContextSystemResult,
    identifiedMultiContextSystemResult,
    acyclicTrace,
    reversibleSite,
    adaptiveAnalysisSpec,
    adaptiveRuleAB,
    adaptiveRuleBC,
    adaptiveEngineRuleAB,
    adaptiveEngineRuleBC,
    adaptiveRewriteSystem,
    arithNumTerm,
    arithAddTermNode,
    sampleSupportTrace,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), zipSameNodeShape)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.NerveSpec.FixturePrelude
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Moonlight.Pale.Diagnostic.Section.Rewrite qualified as PaleRewrite
import Moonlight.Pale.Diagnostic.Section.Saturation qualified as PaleSaturation
import Moonlight.Sheaf.Twist.Report qualified as TwistReport
type ArithF :: Type -> Type
data ArithF a
  = Num Int
  | Add a a
  | Var Int
  | Mul a a
  | Neg a
  deriving stock (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)

type ArithTag :: Type
data ArithTag
  = ArithNumTag Int
  | ArithAddTag
  | ArithVarTag Int
  | ArithMulTag
  | ArithNegTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag ArithF where
  type ConstructorTag ArithF = ArithTag

  constructorTag arithNode =
    case arithNode of
      Num value -> ArithNumTag value
      Add _ _ -> ArithAddTag
      Var v -> ArithVarTag v
      Mul {} -> ArithMulTag
      Neg {} -> ArithNegTag

instance ZipMatch ArithF where
  zipMatch = zipSameNodeShape

type TwistScopeCtx :: Type
data TwistScopeCtx
  = GlobalTwistCtx
  | ModuleTwistCtx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice TwistScopeCtx where
  join = max

instance BoundedJoinSemilattice TwistScopeCtx where
  bottom = GlobalTwistCtx

instance MeetSemilattice TwistScopeCtx where
  meet = min

instance BoundedMeetSemilattice TwistScopeCtx where
  top = ModuleTwistCtx

instance Lattice TwistScopeCtx

expectFixtureRight :: Show error => Either error value -> value
expectFixtureRight =
  either
    (\failure -> error ("fixture rewrite span rejected: " <> show failure))
    id

sampleSpan :: RewriteMorphism ArithF
sampleSpan =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "expand"
      (PatternVar (EGraph.mkPatternVar 0))
      (Set.singleton (EGraph.mkPatternVar 0))
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))))
      Nothing
      Nothing

reverseSpan :: RewriteMorphism ArithF
reverseSpan =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "shrink"
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))))
      (Set.singleton (EGraph.mkPatternVar 0))
      (PatternVar (EGraph.mkPatternVar 0))
      Nothing
      Nothing

disjointLeft :: RewriteMorphism ArithF
disjointLeft =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "left"
      (PatternNode (Num 1))
      Set.empty
      (PatternNode (Num 2))
      Nothing
      Nothing

disjointRight :: RewriteMorphism ArithF
disjointRight =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "right"
      (PatternNode (Num 3))
      Set.empty
      (PatternNode (Num 4))
      Nothing
      Nothing

reversibleSystem :: RewriteSystem ArithF
reversibleSystem =
  mkRewriteSystem [sampleSpan, reverseSpan]

disjointSystem :: RewriteSystem ArithF
disjointSystem =
  mkRewriteSystem [disjointLeft, disjointRight]

acyclicChainSystem :: RewriteSystem ArithF
acyclicChainSystem =
  mkRewriteSystem [contextSpanAB, contextSpanBC]

identifiedAcyclicChainSystem :: RewriteSystem ArithF
identifiedAcyclicChainSystem =
  mkIdentifiedRewriteSystem
    [ expectFixtureRight (identifiedSpanFromRewriteRule contextRuleAB),
      expectFixtureRight (identifiedSpanFromRewriteRule contextRuleBC)
    ]

ambiguousIdentifiedSystem :: RewriteSystem ArithF
ambiguousIdentifiedSystem =
  mkIdentifiedRewriteSystem
    [ expectFixtureRight (identifiedSpanFromRewriteRule contextRuleAB),
      expectFixtureRight (identifiedSpanFromRewriteRule contextRuleAB {rrId = RewriteRuleId 7})
    ]

singleRuleSystem :: RewriteSystem ArithF
singleRuleSystem =
  mkRewriteSystem [contextSpanAB]

contextSpanAB :: RewriteMorphism ArithF
contextSpanAB =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "ab"
      (PatternNode (Num 1))
      Set.empty
      (PatternNode (Num 2))
      Nothing
      Nothing

contextSpanBC :: RewriteMorphism ArithF
contextSpanBC =
  expectFixtureRight $
    rewriteMorphismWithInterface
      "bc"
      (PatternNode (Num 2))
      Set.empty
      (PatternNode (Num 3))
      Nothing
      Nothing

contextRuleAB :: RewriteRule ArithF
contextRuleAB =
  RawRewriteRule
    { rrId = RewriteRuleId 0,
      rrLhs = rewriteMorphismLeft contextSpanAB,
      rrRhs = rewriteMorphismRight contextSpanAB,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

contextRuleBC :: RewriteRule ArithF
contextRuleBC =
  RawRewriteRule
    { rrId = RewriteRuleId 1,
      rrLhs = rewriteMorphismLeft contextSpanBC,
      rrRhs = rewriteMorphismRight contextSpanBC,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

multiContextSystemResult :: Either (RewriteContextPresentationError ArithF) (RewriteSystem ArithF)
multiContextSystemResult =
  mkRewriteSystemWithContexts
    [contextSpanAB, contextSpanBC]
    [[PatternNode (Num 1), PatternNode (Num 2)]]

identifiedMultiContextSystemResult :: Either (RewriteContextPresentationError ArithF) (RewriteSystem ArithF)
identifiedMultiContextSystemResult =
  mkIdentifiedRewriteSystemWithContexts
    [ expectFixtureRight (identifiedSpanFromRewriteRule contextRuleAB),
      expectFixtureRight (identifiedSpanFromRewriteRule contextRuleBC)
    ]
    [[PatternNode (Num 1), PatternNode (Num 2)]]

acyclicTrace :: PaleSaturation.SaturationTrace RewriteRuleId
acyclicTrace =
  PaleSaturation.SaturationTrace
    [ PaleSaturation.SaturationIterationTrace
        { sitIteration = 0,
          sitNodeCountBefore = 1,
          sitNodeCountAfter = 3,
          sitBaseEligibleCount = 0,
          sitContextEligibleCount = 0,
          sitAggregatedEligibleCount = 0,
          sitGuidedCount = 0,
          sitScheduledCount = 0,
          sitFactsChanged = False,
          sitFactRoundCount = 0,
          sitContextRevision = 0,
          sitRuleTraces =
            [ PaleRewrite.RuleTrace
                { rtRuleId = RewriteRuleId 0,
                  rtMatchedCount = 2,
                  rtFilteredCount = 0,
                  rtScheduledCount = 1,
                  rtSkippedByScheduler = False,
                  rtBannedUntil = Nothing
                },
              PaleRewrite.RuleTrace
                { rtRuleId = RewriteRuleId 1,
                  rtMatchedCount = 1,
                  rtFilteredCount = 0,
                  rtScheduledCount = 1,
                  rtSkippedByScheduler = False,
                  rtBannedUntil = Nothing
                }
            ]
        }
    ]

reversibleSite :: NerveSite (RewriteTag ArithF)
reversibleSite =
  mkRewriteNerveSite reversibleSystem 2

adaptiveAnalysisSpec :: AnalysisSpec ArithF ()
adaptiveAnalysisSpec =
  AnalysisSpec
    { asMake = const (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

adaptiveRuleAB :: RewriteRule ArithF
adaptiveRuleAB =
  contextRuleAB
    { rrId = RewriteRuleId 1
    }

adaptiveRuleBC :: RewriteRule ArithF
adaptiveRuleBC =
  contextRuleBC
    { rrId = RewriteRuleId 0
    }

adaptiveEngineRuleAB :: RewriteRule ArithF
adaptiveEngineRuleAB =
  RawRewriteRule
    { rrId = RewriteRuleId 1,
      rrLhs = rewriteMorphismLeft contextSpanAB,
      rrRhs = rewriteMorphismRight contextSpanAB,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

adaptiveEngineRuleBC :: RewriteRule ArithF
adaptiveEngineRuleBC =
  RawRewriteRule
    { rrId = RewriteRuleId 0,
      rrLhs = rewriteMorphismLeft contextSpanBC,
      rrRhs = rewriteMorphismRight contextSpanBC,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

adaptiveRewriteSystem :: RewriteSystem ArithF
adaptiveRewriteSystem =
  mkIdentifiedRewriteSystem
    [ expectFixtureRight (identifiedSpanFromRewriteRule adaptiveRuleAB),
      expectFixtureRight (identifiedSpanFromRewriteRule adaptiveRuleBC)
    ]

arithNumTerm :: Int -> Fix ArithF
arithNumTerm =
  Fix . Num

arithAddTermNode :: Fix ArithF -> Fix ArithF -> Fix ArithF
arithAddTermNode leftValue rightValue =
  Fix (Add leftValue rightValue)

sampleSupportTrace :: [TwistReport.SupportTraceEntry (SupportBasis TwistScopeCtx) RewriteRuleId]
sampleSupportTrace =
  let moduleSupport = principalSupport ModuleTwistCtx
      globalSupport = principalSupport GlobalTwistCtx
   in [ TwistReport.SupportTraceEntry
          { TwistReport.steRound = 0,
            TwistReport.steRuleId = RewriteRuleId 0,
            TwistReport.steSupport = moduleSupport,
            TwistReport.steMatchedCount = 2,
            TwistReport.steScheduledCount = 1,
            TwistReport.steSuppressedCount = 1,
            TwistReport.steSuppressedByCooldown = True
          },
        TwistReport.SupportTraceEntry
          { TwistReport.steRound = 1,
            TwistReport.steRuleId = RewriteRuleId 0,
            TwistReport.steSupport = moduleSupport,
            TwistReport.steMatchedCount = 1,
            TwistReport.steScheduledCount = 1,
            TwistReport.steSuppressedCount = 0,
            TwistReport.steSuppressedByCooldown = False
          },
        TwistReport.SupportTraceEntry
          { TwistReport.steRound = 1,
            TwistReport.steRuleId = RewriteRuleId 0,
            TwistReport.steSupport = globalSupport,
            TwistReport.steMatchedCount = 1,
            TwistReport.steScheduledCount = 0,
            TwistReport.steSuppressedCount = 1,
            TwistReport.steSuppressedByCooldown = False
          },
        TwistReport.SupportTraceEntry
          { TwistReport.steRound = 1,
            TwistReport.steRuleId = RewriteRuleId 1,
            TwistReport.steSupport = moduleSupport,
            TwistReport.steMatchedCount = 4,
            TwistReport.steScheduledCount = 3,
            TwistReport.steSuppressedCount = 0,
            TwistReport.steSuppressedByCooldown = False
          }
      ]
