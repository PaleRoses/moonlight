{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Introspection.HsExprSpec.LawFront
  ( tests,
  )
where

import Data.Monoid (Sum (..))
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual)
import Moonlight.Constraint (ConstraintExpr (..))
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr.Front (HsExprFoldError (..), HsExprLawEmitError (..), emitExpr, foldNodeToHsExprF, foldRewriteConditionToHsExprF)
import Moonlight.EGraph.Introspection.Core.HsExpr.Sig (HsExprSig (..))
import Moonlight.Rewrite.DSL (HTraversable (..), K (..), Node (..))
import Moonlight.Rewrite.DSL (SortName, Term (..), sortName, symbolToken)
import Moonlight.Rewrite.System (GuardAtom (..), GuardTerm (..), RewriteCondition (..), pattern GuardRoot)
import Moonlight.Pale.Ghc.Expr
  ( HsExprF (..),
    HsPatF (..),
    HsVarRef (..),
    LetMode (..),
    LetProvenance (..),
    LetRecursion (..),
    NormalizedFieldLabel (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "law-front"
    [ testCase "emits map-fusion left pattern with stable binder order" $
        emitExpr ["f", "g", "xs"] mapFusionLeftTerm @?= Right mapFusionLeftPattern,
      testCase "rejects duplicate emitter variables" $
        emitExpr ["x", "x"] (#x :: Term HsExprSig "expr") @?= Left (DuplicateEmitterVariable "x"),
      testCase "rejects unknown emitter variables" $
        emitExpr [] (#x :: Term HsExprSig "expr") @?= Left (UnknownEmitterVariable "x" exprSort),
      testCase "rejects non-expr holes under stmt children" $
        foldNodeToHsExprF stmtHoleExpr @?= Left (NonExprHoleError (EGraph.mkPatternVar 0) stmtSort),
      testCase "rejects wrong-sort nodes at expr boundary" $
        foldNodeToHsExprF stmtNodeAtExprBoundary @?= Left (UnexpectedNodeSort exprSort stmtSort),
      testCase "lowers guard terms through the same HsExpr fold" $
        foldRewriteConditionToHsExprF guardCondition @?= Right loweredGuardCondition,
      testCase "counts holes inside assoc-lists and arith shapes" $
        fmap holeCount representativeSigNodes @?= [2, 2, 3, 2, 1]
    ]

mapFusionLeftTerm :: Term HsExprSig "expr"
mapFusionLeftTerm =
  TNode
    ( SAppF
        (TNode (SAppF (globalTerm mapName) #f))
        ( TNode
            ( SParF
                ( TNode
                    ( SAppF
                        (TNode (SAppF (globalTerm mapName) #g))
                        #xs
                    )
                )
            )
        )
    )

mapFusionLeftPattern :: Pattern HsExprF
mapFusionLeftPattern =
  PatternNode
    ( AppF
        ( PatternNode
            ( AppF
                (PatternNode (VarF (GlobalName mapName)))
                (PatternVar (EGraph.mkPatternVar 0))
            )
        )
        ( PatternNode
            ( ParF
                ( PatternNode
                    ( AppF
                        ( PatternNode
                            ( AppF
                                (PatternNode (VarF (GlobalName mapName)))
                                (PatternVar (EGraph.mkPatternVar 1))
                            )
                        )
                        (PatternVar (EGraph.mkPatternVar 2))
                    )
                )
            )
        )
    )

stmtHoleExpr :: Pattern (Node HsExprSig)
stmtHoleExpr =
  PatternNode (Node (SDoF [K (PatternVar (EGraph.mkPatternVar 0))]))

stmtNodeAtExprBoundary :: Pattern (Node HsExprSig)
stmtNodeAtExprBoundary =
  PatternNode (Node (SBodyStmtF (K (PatternVar (EGraph.mkPatternVar 0)))))

guardCondition :: RewriteCondition () (Node HsExprSig)
guardCondition =
  RewriteCondition
    ( Atom
        ( ClassesEquivalent
            (GuardNodeTerm (Node (SVarF (GlobalName mapName))))
            (GuardRefTerm GuardRoot)
        )
    )

loweredGuardCondition :: RewriteCondition () HsExprF
loweredGuardCondition =
  RewriteCondition
    ( Atom
        ( ClassesEquivalent
            (GuardNodeTerm (VarF (GlobalName mapName)))
            (GuardRefTerm GuardRoot)
        )
    )

data SomeHsExprSigNode where
  SomeHsExprSigNode :: HsExprSig sort (Term HsExprSig) -> SomeHsExprSigNode

representativeSigNodes :: [SomeHsExprSigNode]
representativeSigNodes =
  [ SomeHsExprSigNode (SLetF sampleLetMode [(PWildP, #x)] #y),
    SomeHsExprSigNode (SRecordConF #record [(sampleFieldLabel, #field)]),
    SomeHsExprSigNode (SArithFromThenToF #start #step #end),
    SomeHsExprSigNode (SGuardedAltF [#guard] #body),
    SomeHsExprSigNode (SDoF [#stmt])
  ]

holeCount :: SomeHsExprSigNode -> Int
holeCount (SomeHsExprSigNode sigNode) =
  getSum (hfoldMap (const (Sum 1)) sigNode)

globalTerm :: RdrName -> Term HsExprSig "expr"
globalTerm =
  TNode . SVarF . GlobalName

mapName :: RdrName
mapName =
  mkRdrUnqual (mkVarOcc "map")

sampleLetMode :: LetMode
sampleLetMode =
  LetMode
    { lmRecursion = NonRecursiveBinds,
      lmProvenance = LetSyntax
    }

sampleFieldLabel :: NormalizedFieldLabel
sampleFieldLabel =
  NormalizedFieldLabel
    { nflSelector = "field",
      nflAllowsDuplicateRecordFields = False,
      nflHasSelector = True
    }

exprSort :: SortName
exprSort =
  sortName (symbolToken @"expr")

stmtSort :: SortName
stmtSort =
  sortName (symbolToken @"stmt")
