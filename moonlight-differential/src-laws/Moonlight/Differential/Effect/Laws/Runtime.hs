module Moonlight.Differential.Effect.Laws.Runtime
  ( lawBundles,
  )
where

import Control.Monad.Trans.State.Strict
  ( StateT,
    evalStateT,
    gets,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Differential.Context.RowsCache
  ( ContextRowsCache,
    ContextRowsRuntime (..),
    contextRowsKey,
    emptyContextRowsCache,
    insertContextRows,
    withPinnedContext,
  )
import Moonlight.Differential.Effect.Harness.Runtime qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Runtime.Settle
  ( RuntimeSettleStep (..),
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "runtime"
      [ quickCheckLawDefinition SettleQuiescentInputIsFixpoint propSettleQuiescentInputIsFixpoint,
        quickCheckLawDefinition SettleBudgetExhaustionHonest propSettleBudgetExhaustionHonest,
        quickCheckLawDefinition ContextRestrictionUnknownEndpointRefused propContextRestrictionUnknownEndpointRefused,
        quickCheckLawDefinition RowsCachePinnedDropRefused propRowsCachePinnedDropRefused,
        quickCheckLawDefinition RowsCacheOverBudgetObservable propRowsCacheOverBudgetObservable,
        quickCheckLawDefinition RowsCacheOverBudgetRequiresPins propRowsCacheOverBudgetRequiresPins
      ]
  ]

propSettleQuiescentInputIsFixpoint :: QC.Property
propSettleQuiescentInputIsFixpoint =
  QC.forAll (QC.chooseInt (0, 6)) $ \iterationLimit ->
    QC.forAll (QC.chooseInt (-4, 0)) $ \state0 ->
      Harness.settleQuiescentInputIsFixpoint iterationLimit countdownSettleStep state0

propSettleBudgetExhaustionHonest :: QC.Property
propSettleBudgetExhaustionHonest =
  QC.forAll (QC.chooseInt (0, 6)) $ \iterationLimit ->
    QC.forAll (QC.chooseInt (-2, 9)) $ \state0 ->
      Harness.settleBudgetExhaustionHonest iterationLimit countdownSettleStep state0

countdownSettleStep :: RuntimeSettleStep Identity Int Int
countdownSettleStep =
  RuntimeSettleStep
    { rssDrain = \state -> pure (state - 1),
      rssFlush = pure,
      rssQuiescent = (<= 0),
      rssResidual = id
    }

propContextRestrictionUnknownEndpointRefused :: QC.Property
propContextRestrictionUnknownEndpointRefused =
  QC.forAll restrictionCaseGen $ \(contexts, edges) ->
    Harness.contextRestrictionUnknownEndpointRefused (Set.fromList contexts) edges

restrictionCaseGen :: QC.Gen ([Int], [ContextRestrictionEdge Int])
restrictionCaseGen = do
  contexts <- QC.sublistOf [0 .. 4]
  edges <- QC.listOf edgeGen
  pure (contexts, edges)
  where
    edgeGen =
      ContextRestrictionEdge
        <$> QC.chooseInt (0, 5)
        <*> QC.chooseInt (0, 5)

propRowsCachePinnedDropRefused :: QC.Property
propRowsCachePinnedDropRefused =
  QC.forAll rowsCacheCaseGen $ \(budget, inserted, pinned, dirty) ->
    observeUnderPins budget inserted pinned $
      Harness.rowsCachePinnedDropRefused (Set.fromList dirty)

propRowsCacheOverBudgetObservable :: QC.Property
propRowsCacheOverBudgetObservable =
  QC.forAll rowsCacheCaseGen $ \(budget, inserted, pinned, _) ->
    observeUnderPins budget inserted pinned Harness.rowsCacheOverBudgetObservable
      QC..&&. observeAfterPins budget inserted pinned Harness.rowsCacheOverBudgetObservable

propRowsCacheOverBudgetRequiresPins :: QC.Property
propRowsCacheOverBudgetRequiresPins =
  QC.forAll rowsCacheCaseGen $ \(budget, inserted, pinned, _) ->
    observeUnderPins budget inserted pinned Harness.rowsCacheOverBudgetRequiresPins
      QC..&&. observeAfterPins budget inserted pinned Harness.rowsCacheOverBudgetRequiresPins

rowsCacheCaseGen :: QC.Gen (Natural, [Int], [Int], [Int])
rowsCacheCaseGen =
  (,,,)
    <$> (fromIntegral <$> QC.chooseInt (0, 6))
    <*> QC.sublistOf [0 .. 4]
    <*> QC.sublistOf [0 .. 4]
    <*> QC.sublistOf [0 .. 5]

observeUnderPins ::
  Natural ->
  [Int] ->
  [Int] ->
  (ContextRowsCache Int [Int] -> Bool) ->
  Bool
observeUnderPins budget inserted pinned observe =
  runRowsCacheProgram budget $ do
    traverse_ insertFixtureRows inserted
    holdingPins pinned $ do
      traverse_ insertFixtureRows inserted
      gets observe

observeAfterPins ::
  Natural ->
  [Int] ->
  [Int] ->
  (ContextRowsCache Int [Int] -> Bool) ->
  Bool
observeAfterPins budget inserted pinned observe =
  runRowsCacheProgram budget $ do
    traverse_ insertFixtureRows inserted
    _ <- holdingPins pinned (traverse_ insertFixtureRows inserted)
    gets observe

runRowsCacheProgram ::
  Natural ->
  StateT (ContextRowsCache Int [Int]) Identity a ->
  a
runRowsCacheProgram budget program =
  runIdentity (evalStateT program (emptyContextRowsCache budget))

holdingPins ::
  [Int] ->
  StateT (ContextRowsCache Int [Int]) Identity a ->
  StateT (ContextRowsCache Int [Int]) Identity a
holdingPins pinned action =
  foldr (withPinnedContext fixtureRowsRuntime) action pinned

insertFixtureRows ::
  Int ->
  StateT (ContextRowsCache Int [Int]) Identity ()
insertFixtureRows contextValue =
  insertContextRows fixtureRowsRuntime contextValue [contextValue, contextValue + 1]

fixtureRowsRuntime :: ContextRowsRuntime Identity Int [Int]
fixtureRowsRuntime =
  ContextRowsRuntime
    { crrKeyFor = contextRowsKey 0 0 0,
      crrChooseRestrictionSource = \_ _ -> pure Nothing,
      crrMaterializeRootRows = \contextValue -> pure [contextValue],
      crrDeriveByRestriction = \_ _ rows -> pure rows,
      crrRowsBytes = fromIntegral . length
    }
