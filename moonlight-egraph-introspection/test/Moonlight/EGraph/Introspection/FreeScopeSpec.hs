module Moonlight.EGraph.Introspection.FreeScopeSpec (tests) where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF (..),
    ScopedExpr (..),
    TopLevelBinding (..),
    convertHaskellSource,
    convertedModuleContextLattice,
    hsExprScopeGuardCapabilityResolver,
    identityInsertionSeeding,
    insertConvertedModuleWithMetrics,
    scopeObservedContexts,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness (..),
    freeScopeWitnessEmpty,
    freeScopeWitnessScopes,
    hsExprFreeScopeAnalysisSpec,
    hsExprFreeScopeWitness,
  )
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Types (EClass (..), eGraphClasses, emptyEGraph, lookupEClass)
import Moonlight.Rewrite.System (GuardCapabilityResolver (..))
import Moonlight.Pale.Ghc.Expr (ScopeCtx (ActualScope), freeScopeSummaryToList, scopeCtxLeq)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "moonlight-egraph-introspection.freescope"
    [ agreementCase,
      severingCase,
      resolverOrderCase,
      joinCase
    ]

agreementSource :: String
agreementSource =
  unlines
    [ "module FreeScope.Agreement where",
      "transform items = let helper value = combine value seed in map helper items",
      "guarded = \\x -> case x of { Just y -> use y; _ -> fallback x }"
    ]

severingSource :: String
severingSource =
  unlines
    [ "module FreeScope.Severing where",
      "outer = \\y -> use y"
    ]

fixture :: String -> String -> IO ConvertedModule
fixture label source =
  either (\err -> assertFailure (label <> ": " <> show err)) pure (convertHaskellSource (label <> ".hs") source)

expectRight :: Show err => String -> Either err a -> IO a
expectRight label =
  either (\err -> assertFailure (label <> ": " <> show err)) pure

scopedSubterms :: ScopedExpr -> [ScopedExpr]
scopedSubterms scopedTerm =
  scopedTerm : foldMap scopedSubterms (toList (seNode scopedTerm))

foldedWitness :: ConvertedModule -> ScopedExpr -> FreeScopeWitness
foldedWitness convertedModule =
  go
  where
    go scopedTerm =
      hsExprFreeScopeWitness (cmScopeIndex convertedModule) (fmap go (seNode scopedTerm))

agreementCase :: TestTree
agreementCase =
  testCase "the witness algebra agrees with conversion free-scope summaries on every authored subterm" $ do
    convertedModule <- fixture "agreement" agreementSource
    let checkSubterm subterm =
          case freeScopeWitnessScopes (foldedWitness convertedModule subterm) of
            Nothing ->
              assertFailure ("authored subterm folded to an unknown witness: " <> show (seNode subterm))
            Just witnessScopes ->
              Set.fromList witnessScopes @?= Set.fromList (freeScopeSummaryToList (seFreeScopes subterm))
    mapM_
      (mapM_ checkSubterm . scopedSubterms . tlbScopedTerm)
      (cmBindings convertedModule)

severingCase :: TestTree
severingCase =
  testCase "binder constructors sever their bound scope from the witness" $ do
    convertedModule <- fixture "severing" severingSource
    let lambdaPairs scopedTerm =
          let deeper = foldMap lambdaPairs (toList (seNode scopedTerm))
           in case seNode scopedTerm of
                LamF _ body -> (scopedTerm, body) : deeper
                _ -> deeper
        pairs = foldMap (lambdaPairs . tlbScopedTerm) (cmBindings convertedModule)
    assertBool "the severing fixture must contain a lambda" (not (null pairs))
    mapM_
      ( \(lambdaTerm, bodyTerm) -> do
          bodyScopes <-
            maybe (assertFailure "lambda body folded to an unknown witness") pure
              (freeScopeWitnessScopes (foldedWitness convertedModule bodyTerm))
          assertBool "the lambda body must witness its bound scope free" (not (null bodyScopes))
          foldedWitness convertedModule lambdaTerm @?= freeScopeWitnessEmpty
      )
      pairs

resolverOrderCase :: TestTree
resolverOrderCase =
  testCase "the resolver verdict on authored classes is exactly the scope order over the witness chain" $ do
    convertedModule <- fixture "resolver-order" agreementSource
    latticeValue <- expectRight "resolver-order lattice" (convertedModuleContextLattice convertedModule)
    let scopeIndex = cmScopeIndex convertedModule
        contextGraph0 =
          emptyContextEGraph latticeValue (emptyEGraph (hsExprFreeScopeAnalysisSpec scopeIndex))
    (_, _, _, contextGraph1) <-
      expectRight
        "resolver-order insertion"
        (insertConvertedModuleWithMetrics identityInsertionSeeding convertedModule contextGraph0)
    observedContexts <-
      expectRight "observed contexts" (scopeObservedContexts scopeIndex)
    let resolve = runGuardCapabilityResolver (hsExprScopeGuardCapabilityResolver contextGraph1)
        authoredClassIds = fmap eClassId (IntMap.elems (eGraphClasses (cegBase contextGraph1)))
        witnessFor classId =
          maybe FreeScopeUnknown eClassData (lookupEClass (cegBase contextGraph1) classId)
        orderVerdict requiredCtx classId =
          case witnessFor classId of
            FreeScopeUnknown ->
              False
            FreeScopeKnown chain ->
              all
                (\(_, scopeId) -> scopeCtxLeq scopeIndex (ActualScope scopeId) requiredCtx == Right True)
                chain
        legalityPairs =
          [(requiredCtx, classId) | requiredCtx <- observedContexts, classId <- authoredClassIds]
        verdicts =
          fmap (\(requiredCtx, classId) -> resolve requiredCtx [classId]) legalityPairs
    mapM_
      ( \(requiredCtx, classId) ->
          assertBool
            ( "resolver and scope order must agree at "
                <> show requiredCtx
                <> " for class "
                <> show classId
                <> " with witness "
                <> show (witnessFor classId)
            )
            (resolve requiredCtx [classId] == orderVerdict requiredCtx classId)
      )
      legalityPairs
    assertBool "some authored class must be legal somewhere" (or verdicts)
    assertBool "some authored class must be illegal somewhere" (not (and verdicts))

joinCase :: TestTree
joinCase =
  testCase "the witness join prefers known witnesses and selects the better chain" $ do
    convertedModule <- fixture "join" severingSource
    let lambdaBodies scopedTerm =
          let deeper = foldMap lambdaBodies (toList (seNode scopedTerm))
           in case seNode scopedTerm of
                LamF _ body -> body : deeper
                _ -> deeper
        bodies = foldMap (lambdaBodies . tlbScopedTerm) (cmBindings convertedModule)
    bodyWitness <- case bodies of
      body : _ -> pure (foldedWitness convertedModule body)
      [] -> assertFailure "the join fixture must contain a lambda body"
    assertBool "the harvested witness must be known and inhabited" (freeScopeWitnessScopes bodyWitness /= Just [])
    join FreeScopeUnknown bodyWitness @?= bodyWitness
    join bodyWitness FreeScopeUnknown @?= bodyWitness
    join bodyWitness bodyWitness @?= bodyWitness
    join bodyWitness freeScopeWitnessEmpty @?= freeScopeWitnessEmpty
    join freeScopeWitnessEmpty bodyWitness @?= freeScopeWitnessEmpty
    join FreeScopeUnknown FreeScopeUnknown @?= FreeScopeUnknown
