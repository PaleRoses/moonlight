-- | Runnable law-bundle suite over the differential harness.
module Moonlight.Differential.Effect.Laws
  ( tests,
  )
where

import Moonlight.Differential.Effect.Laws.Algebra qualified as Algebra
import Moonlight.Differential.Effect.Laws.Arrangement qualified as Arrangement
import Moonlight.Differential.Effect.Laws.Circuit qualified as Circuit
import Moonlight.Differential.Effect.Laws.Index qualified as Index
import Moonlight.Differential.Effect.Laws.Operator qualified as Operator
import Moonlight.Differential.Effect.Laws.Projection qualified as Projection
import Moonlight.Differential.Effect.Laws.Runtime qualified as Runtime
import Moonlight.Differential.Effect.Laws.Stream qualified as Stream
import Moonlight.Differential.Effect.Laws.TimeFrontier qualified as TimeFrontier
import Moonlight.Differential.Effect.Laws.Trace qualified as Trace
import Moonlight.Differential.Effect.Laws.WCOJ qualified as WCOJ
import Moonlight.Pale.Test.LawSuite (LawBundle, lawSuiteGroup, renderLawBundles)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  lawSuiteGroup
    "moonlight-differential"
    (renderLawBundles id differentialLawBundles)

differentialLawBundles :: [LawBundle String]
differentialLawBundles =
  Algebra.lawBundles
    <> Trace.lawBundles
    <> Arrangement.lawBundles
    <> Operator.lawBundles
    <> Projection.lawBundles
    <> Stream.lawBundles
    <> Index.lawBundles
    <> WCOJ.lawBundles
    <> TimeFrontier.lawBundles
    <> Runtime.lawBundles
    <> Circuit.lawBundles
