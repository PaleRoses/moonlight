module Functor
  ( benchmarks
  , probeCases
  ) where

import Fixture
  ( BenchmarkCaseClass (..)
  , BenchmarkChecksum (..)
  , BenchmarkResult
  , BenchmarkFixture (..)
  , ProbeFamily (..)
  , ProbeBudgetClass (..)
  , ProbeCase
  , benchmarkEitherWith
  , benchmarkSuccess
  , checksumDerivedGF2
  , checksumInjectiveComplexGF2
  , mkHostileProbeCase
  , probeRunFromBenchmarkResult
  , probeBudgetClassForFamily
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.Functor.ClosedSupport (closedSupportResolution)
import Moonlight.Derived.Pure.Functor.ExceptionalPullback (exceptionalPullback)
import Moonlight.Derived.Pure.Functor.ExceptionalPushforward (exceptionalPushforward)
import Moonlight.Derived.Pure.Functor.ProperPullback (properPullback)
import Moonlight.Derived.Pure.Functor.ProperPushforward (properPushforward)
import Moonlight.Derived.Pure.Functor.Pullback (pullback)
import Moonlight.Derived.Pure.Functor.Pushforward (pushforward)
import Moonlight.Derived.Pure.Functor.QuillenA
  ( QuillenACertificate (..)
  , quillenAMaximumCertificate
  )
import Moonlight.Derived.Pure.Site.Microsupport (localClosedNodes)
import Moonlight.Derived.Pure.Functor.Tensor
  ( TensorProfileStage (..)
  , TensorProfileSummary (..)
  , internalHom
  , tensorProduct
  , tensorProductPresentation
  , tensorProfileStageSummary
  )
import Moonlight.Derived.Pure.Functor.VerdierDual (verdierDualComplex)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Bool -> [BenchmarkFixture] -> Benchmark
benchmarks includeHostile fixtures =
  bgroup
    "functor"
    (fmap (benchmarkFamily fixtures) selectedCases)
  where
    selectedCases =
      filter shouldBenchmarkCase functorCases

    shouldBenchmarkCase functorCase =
      fcClass functorCase == SafeMicro || includeHostile

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases fixtures =
  concatMap (fixtureProbeCases hostileCases) fixtures
  where
    hostileCases =
      filter ((== HostileProbe) . fcClass) functorCases

data FunctorCase = FunctorCase
  { fcClass :: !BenchmarkCaseClass
  , fcId :: !String
  , fcRun :: BenchmarkFixture -> BenchmarkResult
  }

functorCases :: [FunctorCase]
functorCases =
  [ FunctorCase SafeMicro "pushforward" runPushforward
  , FunctorCase SafeMicro "pullback" runPullback
  , FunctorCase SafeMicro "proper-pushforward" runProperPushforward
  , FunctorCase SafeMicro "proper-pullback" runProperPullback
  , FunctorCase SafeMicro "exceptional-pushforward" runExceptionalPushforward
  , FunctorCase SafeMicro "exceptional-pullback" runExceptionalPullback
  , FunctorCase SafeMicro "closed-support" runClosedSupport
  , FunctorCase SafeMicro "verdier-dual" runVerdierDual
  , FunctorCase SafeMicro "quillen-a" runQuillenA
  , FunctorCase HostileProbe "tensor/profile-support" (runTensorProfileStage TensorSupportStage)
  , FunctorCase HostileProbe "tensor/profile-expansion" (runTensorProfileStage TensorExpansionStage)
  , FunctorCase HostileProbe "tensor/profile-pairs" (runTensorProfileStage TensorPairStage)
  , FunctorCase HostileProbe "tensor/profile-layout" (runTensorProfileStage TensorLayoutStage)
  , FunctorCase HostileProbe "tensor/profile-differential" (runTensorProfileStage TensorDifferentialStage)
  , FunctorCase HostileProbe "tensor/profile-presentation" (runTensorProfileStage TensorPresentationStage)
  , FunctorCase HostileProbe "tensor/presentation" runTensorPresentation
  , FunctorCase HostileProbe "tensor/minimized" runTensorMinimized
  , FunctorCase HostileProbe "tensor" runTensor
  , FunctorCase HostileProbe "internal-hom" runInternalHom
  ]

fixtureProbeCases :: [FunctorCase] -> BenchmarkFixture -> [ProbeCase]
fixtureProbeCases cases fixture =
  fmap
    (\functorCase -> mkHostileProbeCase (hostileProbeId functorCase fixture) (probeBudgetClass fixture) (pure (probeRunFromBenchmarkResult (fcRun functorCase fixture))))
    cases

hostileProbeId :: FunctorCase -> BenchmarkFixture -> String
hostileProbeId functorCase fixture =
  "hostile/functor/" <> fcId functorCase <> "/" <> bfLabel fixture

probeBudgetClass :: BenchmarkFixture -> ProbeBudgetClass
probeBudgetClass =
  probeBudgetClassForFamily ProbeFamilyFunctor

benchmarkFamily :: [BenchmarkFixture] -> FunctorCase -> Benchmark
benchmarkFamily fixtures functorCase =
  bgroup
    (fcId functorCase)
    (fmap (\fixture -> bench (bfLabel fixture) (nf (fcRun functorCase) fixture)) fixtures)

runPushforward :: BenchmarkFixture -> BenchmarkResult
runPushforward fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (pushforward (bfFunctor fixture) (bfSourceDerived fixture))

runPullback :: BenchmarkFixture -> BenchmarkResult
runPullback fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (pullback (bfFunctor fixture) (bfTargetDerived fixture))

runProperPushforward :: BenchmarkFixture -> BenchmarkResult
runProperPushforward fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (properPushforward (bfOuterClosedSupport fixture) (bfSourceDerived fixture))

runProperPullback :: BenchmarkFixture -> BenchmarkResult
runProperPullback fixture =
  benchmarkSuccess
    ( checksumDerivedGF2
        (properPullback (bfPreparedProperPullback fixture))
    )

runExceptionalPushforward :: BenchmarkFixture -> BenchmarkResult
runExceptionalPushforward fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (exceptionalPushforward (bfFunctor fixture) (bfSourceDerived fixture))

runExceptionalPullback :: BenchmarkFixture -> BenchmarkResult
runExceptionalPullback fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (exceptionalPullback (bfFunctor fixture) (bfTargetDerived fixture))

runClosedSupport :: BenchmarkFixture -> BenchmarkResult
runClosedSupport fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (closedSupportResolution (bfOuterClosedSupport fixture))

runVerdierDual :: BenchmarkFixture -> BenchmarkResult
runVerdierDual fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (verdierDualComplex (bfSourceDerived fixture))

runQuillenA :: BenchmarkFixture -> BenchmarkResult
runQuillenA fixture =
  benchmarkEitherWith
    checksumQuillenACertificate
    (either (Left . show) Right (quillenAMaximumCertificate (bfFunctor fixture)))

runTensorProfileStage :: TensorProfileStage -> BenchmarkFixture -> BenchmarkResult
runTensorProfileStage stage fixture =
  benchmarkEitherWith
    checksumTensorProfileSummary
    ( tensorProfileStageSummary
        stage
        (bfSourceDerived fixture)
        (bfSecondaryDerived fixture)
    )

runTensorPresentation :: BenchmarkFixture -> BenchmarkResult
runTensorPresentation fixture =
  benchmarkEitherWith
    checksumInjectiveComplexGF2
    ( tensorProductPresentation
        (bfSourceDerived fixture)
        (bfSecondaryDerived fixture)
    )

runTensorMinimized :: BenchmarkFixture -> BenchmarkResult
runTensorMinimized fixture =
  benchmarkEitherWith
    checksumInjectiveComplexGF2
    ( do
        tensorComplex <-
          tensorProductPresentation
            (bfSourceDerived fixture)
            (bfSecondaryDerived fixture)

        minimizeComplex tensorComplex
    )

runTensor :: BenchmarkFixture -> BenchmarkResult
runTensor fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (tensorProduct (bfSourceDerived fixture) (bfSecondaryDerived fixture))

runInternalHom :: BenchmarkFixture -> BenchmarkResult
runInternalHom fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (internalHom (bfSourceDerived fixture) (bfSecondaryDerived fixture))

checksumQuillenACertificate :: QuillenACertificate -> BenchmarkChecksum
checksumQuillenACertificate certificateValue =
  BenchmarkChecksum
    ( case certificateValue of
        QuillenACertifiedByMaximum ->
          1
        QuillenARefutedByEmptyFiber _ ->
          0
        QuillenAInconclusive _ ->
          2
    )

checksumTensorProfileSummary :: TensorProfileSummary -> BenchmarkChecksum
checksumTensorProfileSummary summaryValue =
  BenchmarkChecksum
    ( tpsSupportPresentations summaryValue
        + 3 * tpsExpandedDegrees summaryValue
        + 5 * tpsExpandedBasisCells summaryValue
        + 7 * tpsPairInstances summaryValue
        + 11 * tpsSummands summaryValue
        + 13 * tpsLayoutDegrees summaryValue
        + 17 * tpsLayoutBasisCells summaryValue
        + 19 * tpsDifferentials summaryValue
        + 23 * tpsDifferentialCells summaryValue
        + 29 * tpsDifferentialNonZeros summaryValue
        + 31 * tpsRestrictionCacheEntries summaryValue
        + 37 * tpsPresentationDifferentials summaryValue
        + 41 * tpsPresentationBlocks summaryValue
        + 43 * tpsPresentationBlockCells summaryValue
        + 47 * tpsPresentationBlockNonZeros summaryValue
    )
