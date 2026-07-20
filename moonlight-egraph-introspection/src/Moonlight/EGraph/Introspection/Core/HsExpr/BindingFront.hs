{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.BindingFront
  ( HsExprBindingSig (..),
    SurfaceName (..),
    HsExprBindingRule,
    HsExprBindingFactRule,
    HsExprBindingFrontError (..),
    HsExprBindingRuleMetrics (..),
    HsExprBindingCorpus (..),
    hsExprBindingRuleIdBase,
    hsExprSubstitutionAllowedFactId,
    hsExprBindingRelations,
    hsExprScopedBindingSyntax,
    hsExprBindingLanguageSyntax,
    hsExprFresheningSyntax,
    hsExprChildBinderEdges,
    hsExprBindingCorpus,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Monoid (First (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import GHC.TypeLits (Symbol)
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import Moonlight.Core (Pattern (..), RewriteRuleId (..), binderIdKey)
import Moonlight.EGraph.Pure.Saturation.Front
  ( relationRefFactId,
    relationRefWithFactId,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding
  ( BindingChild,
    BindingFact (..),
    BindingIngestError,
    BindingPath,
    BindingPathSegment,
    BindingPlanEntry (..),
    bindingChild,
    bindingPathChildNamed,
    bindingPathName,
    bindingPathSegmentName,
    bindingPlanEntries,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingElaboration (..),
    BindingFresheningPlan (..),
    BindingFresheningSyntax (..),
    BindingGeneratedRewrite (..),
    BindingLanguageError (..),
    BindingLanguageRelations (..),
    BindingLanguageReport (..),
    BindingLanguageSyntax (..),
    BindingRewriteGuard (..),
    BindingSubstitutionDecision (..),
    BindingSubstitutionOutcome (..),
    BindingSubstitutionSite (..),
    compileBindingElaboration,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Scoped
  ( ScopedBindingNode (..),
    ScopedBindingSyntax (..),
    ScopedBindingTree (..),
    scopedBindingChildPathNamed,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.DSL (Term (..), typedVarName)
import Moonlight.Rewrite.System
  ( RewriteCondition (..),
    guardHasFact,
    data GuardRoot,
  )
import Moonlight.Rewrite.System (FactRuleId (..), RawFactRule (..))
import Moonlight.Rewrite.System qualified as LogicRule
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Pale.Ghc.Expr
import Moonlight.FiniteLattice
  ( principalSupport
  )

type HsExprBindingSig :: Symbol -> (Symbol -> Type) -> Type
data HsExprBindingSig sort r where
  HsExprBindingNode :: HsExprF (r "Expr") -> HsExprBindingSig "Expr" r

type SurfaceName :: Type
newtype SurfaceName = SurfaceName
  { unSurfaceName :: String
  }
  deriving stock (Eq, Ord, Show)

type HsExprBindingRule :: Type
type HsExprBindingRule = RawRewriteRule (RewriteCondition ScopeCtx HsExprF) HsExprF

type HsExprBindingFactRule :: Type
type HsExprBindingFactRule = LogicRule.FactRule ScopeCtx HsExprF

type HsExprBindingFrontError :: Type
data HsExprBindingFrontError
  = HsExprBindingElaborationError !String !(BindingLanguageError SurfaceName)
  | HsExprBindingUnexpectedPatternVariable !String
  | HsExprBindingMissingContextPath !String
  | HsExprBindingRuleBookSupportFailure !(PreparedContextSupportError ScopeCtx)
  | HsExprBindingFactBookSupportFailure !(PreparedContextSupportError ScopeCtx)
  deriving stock (Eq, Show)

type HsExprBindingRuleMetrics :: Type
data HsExprBindingRuleMetrics = HsExprBindingRuleMetrics
  { hbrmRedexSiteCount :: !Int,
    hbrmAllowedCount :: !Int,
    hbrmFresheningCount :: !Int,
    hbrmObstructionCount :: !Int,
    hbrmGeneratedRuleCount :: !Int,
    hbrmFactRuleCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type HsExprBindingCorpus :: Type -> Type
data HsExprBindingCorpus owner = HsExprBindingCorpus
  { hbcRules :: !(SheafTwist.SupportedRuleBook owner ScopeCtx HsExprBindingRule),
    hbcFacts :: !(SheafTwist.SupportedFactBook owner ScopeCtx HsExprBindingFactRule),
    hbcMetrics :: !HsExprBindingRuleMetrics
  }

hsExprBindingRuleIdBase :: Int
hsExprBindingRuleIdBase = 1000000

hsExprSubstitutionAllowedFactId :: FactId
hsExprSubstitutionAllowedFactId = FactId 900000

hsExprBindingRelations :: BindingLanguageRelations HsExprBindingSig
hsExprBindingRelations =
  BindingLanguageRelations
    { blrSubstitutionAllowed =
        relationRefWithFactId
          "hsexpr/substitution-allowed"
          hsExprSubstitutionAllowedFactId
    }

hsExprSurfaceName :: RdrName -> SurfaceName
hsExprSurfaceName =
  SurfaceName . occNameString . rdrNameOcc

binderSurfaceName :: BinderAnn -> SurfaceName
binderSurfaceName =
  hsExprSurfaceName . baName

bindingRowBinders :: (HsPatF, r) -> [BinderAnn]
bindingRowBinders =
  patBinders . fst

bindingRowSurfaceNames :: [(HsPatF, r)] -> Set SurfaceName
bindingRowSurfaceNames =
  Set.fromList . fmap binderSurfaceName . foldMap bindingRowBinders

bindingRowsShadow :: SurfaceName -> [(HsPatF, r)] -> Bool
bindingRowsShadow targetName =
  any ((== targetName) . binderSurfaceName) . foldMap bindingRowBinders

bindingRowBinderKeys :: [(HsPatF, r)] -> [Int]
bindingRowBinderKeys =
  fmap (binderIdKey . baId) . foldMap bindingRowBinders

scopedExprFix :: ScopedExpr -> Fix HsExprF
scopedExprFix scopedExpr =
  Fix (fmap scopedExprFix (seNode scopedExpr))

hsExprBindingTerm :: Fix HsExprF -> Term HsExprBindingSig "Expr"
hsExprBindingTerm (Fix layer) =
  TNode (HsExprBindingNode (fmap hsExprBindingTerm layer))

loweredGroundPattern :: Term HsExprBindingSig "Expr" -> Either HsExprBindingFrontError (Pattern HsExprF)
loweredGroundPattern = \case
  TVar typedVariable ->
    Left (HsExprBindingUnexpectedPatternVariable (typedVarName typedVariable))
  TNode (HsExprBindingNode layer) ->
    PatternNode <$> traverse loweredGroundPattern layer

bindingTermFix :: Term HsExprBindingSig "Expr" -> Either HsExprBindingFrontError (Fix HsExprF)
bindingTermFix = \case
  TVar typedVariable ->
    Left (HsExprBindingUnexpectedPatternVariable (typedVarName typedVariable))
  TNode (HsExprBindingNode layer) ->
    Fix <$> traverse bindingTermFix layer

fixNodeCount :: Fix HsExprF -> Int
fixNodeCount (Fix layer) =
  1 + sum (fmap fixNodeCount layer)

functionSegment :: String
functionSegment = "function"

argumentSegment :: String
argumentSegment = "argument"

bodySegment :: String
bodySegment = "body"

innerSegment :: String
innerSegment = "inner"

letRhsSegment :: Int -> String
letRhsSegment bindingIndex =
  "let-rhs-" <> show bindingIndex

branchSegment :: Int -> String
branchSegment branchIndex =
  "branch-" <> show branchIndex

clauseBodySegment :: Int -> String
clauseBodySegment clauseIndex =
  "clause-" <> show clauseIndex <> "-body"

doStmtSegment :: Int -> String
doStmtSegment statementIndex =
  "stmt-" <> show statementIndex

doStmtRhsSegment :: Int -> Int -> String
doStmtRhsSegment statementIndex bindingIndex =
  "stmt-" <> show statementIndex <> "-rhs-" <> show bindingIndex

guardSegment :: Int -> Int -> String
guardSegment alternativeIndex guardIndex =
  "guard-alt-" <> show alternativeIndex <> "-guard-" <> show guardIndex

guardRhsSegment :: Int -> Int -> Int -> String
guardRhsSegment alternativeIndex guardIndex bindingIndex =
  guardSegment alternativeIndex guardIndex <> "-rhs-" <> show bindingIndex

guardBodySegment :: Int -> String
guardBodySegment alternativeIndex =
  "guard-alt-" <> show alternativeIndex <> "-body"

itemSegment :: Int -> String
itemSegment itemIndex =
  "item-" <> show itemIndex

fieldSegment :: Int -> String
fieldSegment fieldIndex =
  "field-" <> show fieldIndex

recordSegment :: String
recordSegment = "record"

arithFromSegment :: String
arithFromSegment = "from"

arithThenSegment :: String
arithThenSegment = "then"

arithToSegment :: String
arithToSegment = "to"

hsExprScopedBindingSyntax :: ScopedExpr -> ScopedBindingSyntax HsExprF HsExprBindingSig ScopeCtx ScopedExpr
hsExprScopedBindingSyntax rootScoped =
  ScopedBindingSyntax
    { sbsInitialScope = rootScoped,
      sbsRootContext = ActualScope (seOccScope rootScoped),
      sbsChildren = \_ scopedParent _ -> hsExprScopedChildren scopedParent,
      sbsFactsAtNode = noNodeFacts,
      sbsTermAtPath = \_ _ -> hsExprBindingTerm
    }
  where
    noNodeFacts :: ScopedBindingNode HsExprF ScopeCtx ScopedExpr -> Either BindingIngestError [BindingFact HsExprBindingSig]
    noNodeFacts = const (Right [])

hsExprScopedChildren :: ScopedExpr -> Either BindingIngestError [BindingChild HsExprF ScopeCtx ScopedExpr]
hsExprScopedChildren scopedParent =
  traverse
    ( \(segmentName, childScoped) ->
        bindingChild
          segmentName
          (ActualScope (seOccScope childScoped))
          childScoped
          (scopedExprFix childScoped)
    )
    (hsExprChildSegments (seNode scopedParent))

hsExprVirtualBindingSyntax :: ScopeCtx -> ScopedBindingSyntax HsExprF HsExprBindingSig ScopeCtx ()
hsExprVirtualBindingSyntax ruleCtx =
  ScopedBindingSyntax
    { sbsInitialScope = (),
      sbsRootContext = ruleCtx,
      sbsChildren = \_ _ (Fix layer) ->
        traverse
          (\(segmentName, childTerm) -> bindingChild segmentName ruleCtx () childTerm)
          (hsExprChildSegments layer),
      sbsFactsAtNode = const (Right []),
      sbsTermAtPath = \_ _ -> hsExprBindingTerm
    }

hsExprChildSegments :: HsExprF r -> [(String, r)]
hsExprChildSegments = \case
  VarF {} -> []
  AppF functionExpr argumentExpr ->
    [(functionSegment, functionExpr), (argumentSegment, argumentExpr)]
  LamF _ bodyExpr ->
    [(bodySegment, bodyExpr)]
  LetF _ bindings bodyExpr ->
    [ (letRhsSegment bindingIndex, rhsExpr)
    | (bindingIndex, (_, rhsExpr)) <- zip [0 :: Int ..] bindings
    ]
      <> [(bodySegment, bodyExpr)]
  OpAppF leftExpr operatorExpr rightExpr ->
    [("operand-left", leftExpr), ("operator", operatorExpr), ("operand-right", rightExpr)]
  SectionLF leftExpr operatorExpr ->
    [("expr", leftExpr), ("operator", operatorExpr)]
  SectionRF operatorExpr rightExpr ->
    [("operator", operatorExpr), ("expr", rightExpr)]
  ParF innerExpr ->
    [(innerSegment, innerExpr)]
  LitF {} -> []
  OverLitF {} -> []
  IfF conditionExpr thenExpr elseExpr ->
    [("condition", conditionExpr), ("then", thenExpr), ("else", elseExpr)]
  CaseF scrutineeExpr branches ->
    ("scrutinee", scrutineeExpr)
      : [ (branchSegment branchIndex, branchExpr)
        | (branchIndex, (_, branchExpr)) <- zip [0 :: Int ..] branches
        ]
  ClausesF clauses ->
    [ (clauseBodySegment clauseIndex, bodyExpr)
    | (clauseIndex, (_, bodyExpr)) <- zip [0 :: Int ..] clauses
    ]
  DoF statements ->
    concat
      [ case statement of
          BindStmtF _ bodyExpr -> [(doStmtSegment statementIndex, bodyExpr)]
          BodyStmtF bodyExpr -> [(doStmtSegment statementIndex, bodyExpr)]
          LetStmtF _ bindings ->
            [ (doStmtRhsSegment statementIndex bindingIndex, rhsExpr)
            | (bindingIndex, (_, rhsExpr)) <- zip [0 :: Int ..] bindings
            ]
      | (statementIndex, statement) <- zip [0 :: Int ..] statements
      ]
  NegF innerExpr ->
    [(innerSegment, innerExpr)]
  ExplicitListF items ->
    [(itemSegment itemIndex, itemExpr) | (itemIndex, itemExpr) <- zip [0 :: Int ..] items]
  ExplicitTupleF items ->
    [(itemSegment itemIndex, itemExpr) | (itemIndex, itemExpr) <- zip [0 :: Int ..] items]
  RecordConF constructorExpr fields ->
    ("constructor", constructorExpr)
      : [ (fieldSegment fieldIndex, fieldExpr)
        | (fieldIndex, (_, fieldExpr)) <- zip [0 :: Int ..] fields
        ]
  RecordUpdF recordExpr fields ->
    (recordSegment, recordExpr)
      : [ (fieldSegment fieldIndex, fieldExpr)
        | (fieldIndex, (_, fieldExpr)) <- zip [0 :: Int ..] fields
        ]
  ArithSeqF arithSeqValue ->
    arithSeqChildSegments arithSeqValue
  GuardedF alternatives ->
    concat
      [ guardedAltChildSegments alternativeIndex alternative
      | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives
      ]
  MultiIfF alternatives ->
    concat
      [ guardedAltChildSegments alternativeIndex alternative
      | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives
      ]
  ExprWithTySigF innerExpr _ ->
    [(innerSegment, innerExpr)]
  AppTypeF innerExpr _ ->
    [(innerSegment, innerExpr)]
  OpaqueF {} -> []

arithSeqChildSegments :: NormalizedArithSeq r -> [(String, r)]
arithSeqChildSegments = \case
  ArithSeqFrom fromValue ->
    [(arithFromSegment, fromValue)]
  ArithSeqFromThen fromValue thenValue ->
    [(arithFromSegment, fromValue), (arithThenSegment, thenValue)]
  ArithSeqFromTo fromValue toValue ->
    [(arithFromSegment, fromValue), (arithToSegment, toValue)]
  ArithSeqFromThenTo fromValue thenValue toValue ->
    [(arithFromSegment, fromValue), (arithThenSegment, thenValue), (arithToSegment, toValue)]

guardedAltChildSegments :: Int -> GuardedAltF r -> [(String, r)]
guardedAltChildSegments alternativeIndex (GuardedAltF guards bodyExpr) =
  concat
    [ guardStmtChildSegments alternativeIndex guardIndex guardStmt
    | (guardIndex, guardStmt) <- zip [0 :: Int ..] guards
    ]
    <> [(guardBodySegment alternativeIndex, bodyExpr)]

guardStmtChildSegments :: Int -> Int -> HsGuardStmtF r -> [(String, r)]
guardStmtChildSegments alternativeIndex guardIndex = \case
  GuardBoolF guardExpr ->
    [(guardSegment alternativeIndex guardIndex, guardExpr)]
  GuardPatF _ guardExpr ->
    [(guardSegment alternativeIndex guardIndex, guardExpr)]
  GuardLetF _ bindings ->
    [ (guardRhsSegment alternativeIndex guardIndex bindingIndex, rhsExpr)
    | (bindingIndex, (_, rhsExpr)) <- zip [0 :: Int ..] bindings
    ]

hsExprBindingLanguageSyntax :: BindingLanguageSyntax HsExprF SurfaceName
hsExprBindingLanguageSyntax =
  BindingLanguageSyntax
    { blsOccurrencesAt = \_ -> hsExprOccurrences,
      blsBindersEnteringChild = \_ parentTerm childSegment _ ->
        hsExprBindersEnteringChild parentTerm childSegment,
      blsSubstitutionSitesAt = hsExprSubstitutionSites
    }

hsExprOccurrences :: Fix HsExprF -> Set SurfaceName
hsExprOccurrences (Fix layer) =
  case layer of
    VarF (GlobalName rdrName) -> Set.singleton (hsExprSurfaceName rdrName)
    VarF (LocalName binderAnn) -> Set.singleton (binderSurfaceName binderAnn)
    _ -> Set.empty

hsExprBindersEnteringChild :: Fix HsExprF -> BindingPathSegment -> Set SurfaceName
hsExprBindersEnteringChild (Fix layer) childSegment =
  Map.findWithDefault Set.empty (bindingPathSegmentName childSegment) (hsExprChildBinderEdges layer)

hsExprChildBinderEdges :: HsExprF (Fix HsExprF) -> Map String (Set SurfaceName)
hsExprChildBinderEdges = \case
  LamF binderAnn _ ->
    Map.singleton bodySegment (Set.singleton (binderSurfaceName binderAnn))
  LetF letMode bindings _ ->
    let boundNames = bindingRowSurfaceNames bindings
        rhsEdges =
          case lmRecursion letMode of
            NonRecursiveBinds -> Map.empty
            RecursiveOpaqueBinds ->
              Map.fromList
                [ (letRhsSegment bindingIndex, boundNames)
                | (bindingIndex, _) <- zip [0 :: Int ..] bindings
                ]
     in Map.insert bodySegment boundNames rhsEdges
  CaseF _ branches ->
    Map.fromList
      [ (branchSegment branchIndex, Set.fromList (fmap binderSurfaceName (patBinders branchPattern)))
      | (branchIndex, (branchPattern, _)) <- zip [0 :: Int ..] branches
      ]
  ClausesF clauses ->
    Map.fromList
      [ (clauseBodySegment clauseIndex, Set.fromList (fmap binderSurfaceName (clausePatternBinders clausePatterns)))
      | (clauseIndex, (clausePatterns, _)) <- zip [0 :: Int ..] clauses
      ]
  DoF statements ->
    hsExprDoBinderEdges statements
  GuardedF alternatives ->
    hsExprGuardedBinderEdges alternatives
  MultiIfF alternatives ->
    hsExprGuardedBinderEdges alternatives
  _ ->
    Map.empty

clausePatternBinders :: [HsPatF] -> [BinderAnn]
clausePatternBinders =
  foldMap patBinders

hsExprDoBinderEdges :: [HsStmtF (Fix HsExprF)] -> Map String (Set SurfaceName)
hsExprDoBinderEdges statements =
  fst (foldl' step (Map.empty, Set.empty) (zip [0 :: Int ..] statements))
  where
    step (edges, priorBinders) (statementIndex, statement) =
      case statement of
        BindStmtF bindPattern _ ->
          ( Map.insert (doStmtSegment statementIndex) priorBinders edges,
            priorBinders <> Set.fromList (fmap binderSurfaceName (patBinders bindPattern))
          )
        BodyStmtF _ ->
          ( Map.insert (doStmtSegment statementIndex) priorBinders edges,
            priorBinders
          )
        LetStmtF letMode bindings ->
          let boundNames = bindingRowSurfaceNames bindings
              rhsScope =
                case lmRecursion letMode of
                  NonRecursiveBinds -> priorBinders
                  RecursiveOpaqueBinds -> priorBinders <> boundNames
              rhsEdges =
                Map.fromList
                  [ (doStmtRhsSegment statementIndex bindingIndex, rhsScope)
                  | (bindingIndex, _) <- zip [0 :: Int ..] bindings
                  ]
           in (edges <> rhsEdges, priorBinders <> boundNames)

hsExprGuardedBinderEdges :: [GuardedAltF (Fix HsExprF)] -> Map String (Set SurfaceName)
hsExprGuardedBinderEdges alternatives =
  fold
    [ guardedAltBinderEdges alternativeIndex alternative
    | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives
    ]

guardedAltBinderEdges :: Int -> GuardedAltF (Fix HsExprF) -> Map String (Set SurfaceName)
guardedAltBinderEdges alternativeIndex (GuardedAltF guards _) =
  let (guardEdges, finalBinders) =
        foldl' step (Map.empty, Set.empty) (zip [0 :: Int ..] guards)
   in Map.insert (guardBodySegment alternativeIndex) finalBinders guardEdges
  where
    step (edges, priorBinders) (guardIndex, guardStmt) =
      case guardStmt of
        GuardBoolF _ ->
          (Map.insert (guardSegment alternativeIndex guardIndex) priorBinders edges, priorBinders)
        GuardPatF guardPattern _ ->
          let patternBinders = Set.fromList (fmap binderSurfaceName (patBinders guardPattern))
           in (Map.insert (guardSegment alternativeIndex guardIndex) priorBinders edges, priorBinders <> patternBinders)
        GuardLetF letMode bindings ->
          let boundNames = bindingRowSurfaceNames bindings
              rhsScope =
                case lmRecursion letMode of
                  NonRecursiveBinds -> priorBinders
                  RecursiveOpaqueBinds -> priorBinders <> boundNames
              rhsEdges =
                Map.fromList
                  [ (guardRhsSegment alternativeIndex guardIndex bindingIndex, rhsScope)
                  | (bindingIndex, _) <- zip [0 :: Int ..] bindings
                  ]
           in (edges <> rhsEdges, priorBinders <> boundNames)

hsExprSubstitutionSites ::
  ScopedBindingNode HsExprF context scope ->
  Either (BindingLanguageError SurfaceName) [BindingSubstitutionSite SurfaceName]
hsExprSubstitutionSites node =
  case sbnTerm node of
    Fix (AppF (Fix (ParF (Fix (LamF binderAnn _)))) _)
      | termContainsOpaque (sbnTerm node) -> Right []
      | otherwise -> do
          functionPath <- childPathAt functionSegment node
          innerPath <- pathUnder functionPath innerSegment
          bodyPath <- pathUnder innerPath bodySegment
          argumentPath <- childPathAt argumentSegment node
          Right [substitutionSite binderAnn bodyPath argumentPath]
    Fix (AppF (Fix (LamF binderAnn _)) _)
      | termContainsOpaque (sbnTerm node) -> Right []
      | otherwise -> do
          functionPath <- childPathAt functionSegment node
          bodyPath <- pathUnder functionPath bodySegment
          argumentPath <- childPathAt argumentSegment node
          Right [substitutionSite binderAnn bodyPath argumentPath]
    Fix (LetF letMode [(PVarP binderAnn, _)] _)
      | lmRecursion letMode == NonRecursiveBinds ->
          if termContainsOpaque (sbnTerm node)
            then Right []
            else do
              rhsPath <- childPathAt (letRhsSegment 0) node
              bodyPath <- childPathAt bodySegment node
              Right [substitutionSite binderAnn bodyPath rhsPath]
    _ ->
      Right []
  where
    substitutionSite binderAnn bodyPath argumentPath =
      BindingSubstitutionSite
        { bssBinder = binderSurfaceName binderAnn,
          bssBodyPath = bodyPath,
          bssArgumentPath = argumentPath
        }

childPathAt ::
  String ->
  ScopedBindingNode HsExprF context scope ->
  Either (BindingLanguageError SurfaceName) BindingPath
childPathAt rawSegment node =
  either (Left . BindingLanguageIngestError) Right (scopedBindingChildPathNamed rawSegment node)

pathUnder ::
  BindingPath ->
  String ->
  Either (BindingLanguageError SurfaceName) BindingPath
pathUnder parentPath rawSegment =
  either (Left . BindingLanguageIngestError) Right (bindingPathChildNamed parentPath rawSegment)

termContainsOpaque :: Fix HsExprF -> Bool
termContainsOpaque (Fix layer) =
  case layer of
    OpaqueF {} -> True
    _ -> any termContainsOpaque layer

hsExprFresheningSyntax :: Int -> Set SurfaceName -> BindingFresheningSyntax HsExprF HsExprBindingSig SurfaceName
hsExprFresheningSyntax binderFloor avoidNames =
  BindingFresheningSyntax
    { bfsFreshenBinders = hsExprFreshenBinders avoidNames,
      bfsFreshenedRedex = \renames tree decision ->
        hsExprBindingTerm <$> hsExprFreshenedRedexFix binderFloor renames tree decision,
      bfsContractedResult = \renames tree decision ->
        hsExprBindingTerm <$> hsExprContractedFix binderFloor renames tree decision
    }

hsExprFreshenBinders ::
  Set SurfaceName ->
  BindingSubstitutionDecision SurfaceName ->
  Either (BindingLanguageError SurfaceName) (Map SurfaceName SurfaceName)
hsExprFreshenBinders avoidNames decision =
  fmap fst (foldl' selectFresh (Right (Map.empty, initialAvoid)) (Set.toAscList captured))
  where
    captured =
      Set.intersection (bsdArgumentFreeBinders decision) (bsdBodyCapturingBinders decision)
    initialAvoid =
      Set.unions
        [ avoidNames,
          bsdArgumentFreeBinders decision,
          bsdBodyCapturingBinders decision,
          Set.singleton (bsdBinder decision)
        ]
    selectFresh accumulated binderName = do
      (renames, avoid) <- accumulated
      freshName <-
        maybe
          (Left (BindingLanguageFreshNameExhausted binderName))
          Right
          (find (`Set.notMember` avoid) (freshCandidates binderName))
      Right (Map.insert binderName freshName renames, Set.insert freshName avoid)
    freshCandidates (SurfaceName rawName) =
      fmap (\candidateIndex -> SurfaceName (rawName <> show candidateIndex)) [0 .. 1024 :: Int]

type BinderAccum :: Type -> Type
newtype BinderAccum a = BinderAccum
  { runBinderAccum :: (Int, Map SurfaceName BinderAnn) -> (a, (Int, Map SurfaceName BinderAnn))
  }

instance Functor BinderAccum where
  fmap mapResult (BinderAccum runFirst) =
    BinderAccum
      ( \threaded ->
          let (firstValue, threaded') = runFirst threaded
           in (mapResult firstValue, threaded')
      )

instance Applicative BinderAccum where
  pure value = BinderAccum (\threaded -> (value, threaded))
  BinderAccum runFunction <*> BinderAccum runArgument =
    BinderAccum
      ( \threaded ->
          let (functionValue, threaded') = runFunction threaded
              (argumentValue, threaded'') = runArgument threaded'
           in (functionValue argumentValue, threaded'')
      )

type HsExprRedexShape :: Type
data HsExprRedexShape
  = RedexParApp
  | RedexBareApp
  | RedexLet !LetMode

type HsExprRedex :: Type
data HsExprRedex = HsExprRedex
  { herBinder :: !BinderAnn,
    herBody :: !(Fix HsExprF),
    herArgument :: !(Fix HsExprF),
    herShape :: !HsExprRedexShape
  }

hsExprRedexAt ::
  ScopedBindingTree HsExprF context scope ->
  BindingSubstitutionDecision SurfaceName ->
  Either (BindingLanguageError SurfaceName) HsExprRedex
hsExprRedexAt tree decision = do
  redexTree <-
    maybe
      (Left (BindingLanguageUnknownPath (bsdRedexPath decision)))
      Right
      (findBindingTree (bsdRedexPath decision) tree)
  case sbnTerm (sbtNode redexTree) of
    Fix (AppF (Fix (ParF (Fix (LamF binderAnn bodyTerm)))) argumentTerm)
      | binderSurfaceName binderAnn == bsdBinder decision ->
          Right (HsExprRedex binderAnn bodyTerm argumentTerm RedexParApp)
    Fix (AppF (Fix (LamF binderAnn bodyTerm)) argumentTerm)
      | binderSurfaceName binderAnn == bsdBinder decision ->
          Right (HsExprRedex binderAnn bodyTerm argumentTerm RedexBareApp)
    Fix (LetF letMode [(PVarP binderAnn, rhsTerm)] bodyTerm)
      | lmRecursion letMode == NonRecursiveBinds
          && binderSurfaceName binderAnn == bsdBinder decision ->
          Right (HsExprRedex binderAnn bodyTerm rhsTerm (RedexLet letMode))
    _ ->
      Left (BindingLanguageUnexpectedSubstitutionShape (bsdRedexPath decision))

findBindingTree ::
  BindingPath ->
  ScopedBindingTree f context scope ->
  Maybe (ScopedBindingTree f context scope)
findBindingTree path tree
  | sbnPath (sbtNode tree) == path = Just tree
  | otherwise =
      getFirst (foldMap (First . findBindingTree path . snd) (sbtChildren tree))

hsExprFreshenedRedexFix ::
  Int ->
  Map SurfaceName SurfaceName ->
  ScopedBindingTree HsExprF context scope ->
  BindingSubstitutionDecision SurfaceName ->
  Either (BindingLanguageError SurfaceName) (Fix HsExprF)
hsExprFreshenedRedexFix binderFloor renames tree decision = do
  redex <- hsExprRedexAt tree decision
  let freshBody = freshenTerm binderFloor renames (herBody redex)
  Right $
    case herShape redex of
      RedexParApp ->
        Fix (AppF (Fix (ParF (Fix (LamF (herBinder redex) freshBody)))) (herArgument redex))
      RedexBareApp ->
        Fix (AppF (Fix (LamF (herBinder redex) freshBody)) (herArgument redex))
      RedexLet letMode ->
        Fix (LetF letMode [(PVarP (herBinder redex), herArgument redex)] freshBody)

hsExprContractedFix ::
  Int ->
  Map SurfaceName SurfaceName ->
  ScopedBindingTree HsExprF context scope ->
  BindingSubstitutionDecision SurfaceName ->
  Either (BindingLanguageError SurfaceName) (Fix HsExprF)
hsExprContractedFix binderFloor renames tree decision = do
  redex <- hsExprRedexAt tree decision
  let freshBody = freshenTerm binderFloor renames (herBody redex)
  Right (substituteSurfaceName (binderSurfaceName (herBinder redex)) (herArgument redex) freshBody)

freshenTerm :: Int -> Map SurfaceName SurfaceName -> Fix HsExprF -> Fix HsExprF
freshenTerm binderFloor renames term
  | Map.null renames = term
  | otherwise = snd (freshenWalk (binderFloor + 1) Map.empty term)
  where
    freshenWalk :: Int -> Map SurfaceName BinderAnn -> Fix HsExprF -> (Int, Fix HsExprF)
    freshenWalk counter active (Fix layer) =
      case layer of
        VarF (LocalName binderAnn) ->
          case Map.lookup (binderSurfaceName binderAnn) active of
            Just freshAnn -> (counter, Fix (VarF (LocalName freshAnn)))
            Nothing -> (counter, Fix (VarF (LocalName binderAnn)))
        LamF binderAnn bodyExpr ->
          let (counter1, binderAnn', active') = freshenBinder counter active binderAnn
              (counter2, body') = freshenWalk counter1 active' bodyExpr
           in (counter2, Fix (LamF binderAnn' body'))
        LetF letMode bindings bodyExpr ->
          case lmRecursion letMode of
            NonRecursiveBinds ->
              let ((counter2, active2), bindings') =
                    freshenNonRecursiveBindingRows active counter bindings
                  (counter3, body') = freshenWalk counter2 active2 bodyExpr
               in (counter3, Fix (LetF letMode bindings' body'))
            RecursiveOpaqueBinds ->
              let ((counter2, active1), bindings') =
                    freshenRecursiveBindingRows active counter bindings
                  (counter3, body') = freshenWalk counter2 active1 bodyExpr
               in (counter3, Fix (LetF letMode bindings' body'))
        CaseF scrutineeExpr branches ->
          let (counter1, scrutinee') = freshenWalk counter active scrutineeExpr
              (counter2, branches') =
                mapAccumL
                  ( \cnt (branchPattern, branchExpr) ->
                      let ((cnt1, branchActive), branchPattern') = freshenPattern cnt active branchPattern
                          (cnt2, branch') = freshenWalk cnt1 branchActive branchExpr
                       in (cnt2, (branchPattern', branch'))
                  )
                  counter1
                  branches
           in (counter2, Fix (CaseF scrutinee' branches'))
        ClausesF clauses ->
          let (counter1, clauses') =
                mapAccumL
                  ( \cnt (clausePatterns, bodyExpr) ->
                      let ((cnt1, clauseActive), clausePatterns') =
                            mapAccumL
                              ( \(cntPattern, activePattern) clausePattern ->
                                  freshenPattern cntPattern activePattern clausePattern
                              )
                              (cnt, active)
                              clausePatterns
                          (cnt2, body') = freshenWalk cnt1 clauseActive bodyExpr
                       in (cnt2, (clausePatterns', body'))
                  )
                  counter
                  clauses
           in (counter1, Fix (ClausesF clauses'))
        DoF statements ->
          let ((counterFinal, _), statements') =
                mapAccumL
                  ( \(cnt, activeAcc) statement ->
                      case statement of
                        BindStmtF bindPattern bodyExpr ->
                          let (cnt1, body') = freshenWalk cnt activeAcc bodyExpr
                              ((cnt2, activeNext), bindPattern') = freshenPattern cnt1 activeAcc bindPattern
                           in ((cnt2, activeNext), BindStmtF bindPattern' body')
                        BodyStmtF bodyExpr ->
                          let (cnt1, body') = freshenWalk cnt activeAcc bodyExpr
                           in ((cnt1, activeAcc), BodyStmtF body')
                        LetStmtF letMode bindings ->
                          case lmRecursion letMode of
                            NonRecursiveBinds ->
                              let ((cnt2, activeNext), bindings') =
                                    freshenNonRecursiveBindingRows activeAcc cnt bindings
                               in ((cnt2, activeNext), LetStmtF letMode bindings')
                            RecursiveOpaqueBinds ->
                              let ((cnt2, activeNext), bindings') =
                                    freshenRecursiveBindingRows activeAcc cnt bindings
                               in ((cnt2, activeNext), LetStmtF letMode bindings')
                  )
                  (counter, active)
                  statements
           in (counterFinal, Fix (DoF statements'))
        GuardedF alternatives ->
          let (counter1, alternatives') =
                mapAccumL (freshenGuardedAlternative active) counter alternatives
           in (counter1, Fix (GuardedF alternatives'))
        MultiIfF alternatives ->
          let (counter1, alternatives') =
                mapAccumL (freshenGuardedAlternative active) counter alternatives
           in (counter1, Fix (MultiIfF alternatives'))
        _ ->
          let (counter1, layer') =
                mapAccumL (\cnt childExpr -> freshenWalk cnt active childExpr) counter layer
           in (counter1, Fix layer')

    freshenBinder :: Int -> Map SurfaceName BinderAnn -> BinderAnn -> (Int, BinderAnn, Map SurfaceName BinderAnn)
    freshenBinder counter active binderAnn =
      let surfaceName = binderSurfaceName binderAnn
       in case Map.lookup surfaceName renames of
            Just (SurfaceName freshRawName) ->
              let freshAnn =
                    BinderAnn
                      { baId = toEnum counter,
                        baName = mkRdrUnqual (mkVarOcc freshRawName)
                      }
               in (counter + 1, freshAnn, Map.insert surfaceName freshAnn active)
            Nothing ->
              (counter, binderAnn, Map.delete surfaceName active)

    freshenPattern :: Int -> Map SurfaceName BinderAnn -> HsPatF -> ((Int, Map SurfaceName BinderAnn), HsPatF)
    freshenPattern counter active patternValue =
      let (freshenedPattern, finalState) = runBinderAccum (traversePatBinders step patternValue) (counter, active)
       in (finalState, freshenedPattern)
      where
        step binderAnn =
          BinderAccum
            ( \(cnt, activeAcc) ->
                let (cnt', ann', activeAcc') = freshenBinder cnt activeAcc binderAnn
                 in (ann', (cnt', activeAcc'))
            )

    freshenNonRecursiveBindingRows activeRoot counterValue bindings =
      let (counter1, rhsValues) =
            mapAccumL (\cnt (_, rhsExpr) -> freshenWalk cnt activeRoot rhsExpr) counterValue bindings
          ((counter2, activeNext), patterns') =
            mapAccumL
              (\(cnt, activeAcc) (rowPattern, _) -> freshenPattern cnt activeAcc rowPattern)
              (counter1, activeRoot)
              bindings
       in ((counter2, activeNext), zip patterns' rhsValues)

    freshenRecursiveBindingRows activeRoot counterValue bindings =
      let ((counter1, activeNext), patterns') =
            mapAccumL
              (\(cnt, activeAcc) (rowPattern, _) -> freshenPattern cnt activeAcc rowPattern)
              (counterValue, activeRoot)
              bindings
          (counter2, rhsValues) =
            mapAccumL (\cnt (_, rhsExpr) -> freshenWalk cnt activeNext rhsExpr) counter1 bindings
       in ((counter2, activeNext), zip patterns' rhsValues)

    freshenGuardedAlternative :: Map SurfaceName BinderAnn -> Int -> GuardedAltF (Fix HsExprF) -> (Int, GuardedAltF (Fix HsExprF))
    freshenGuardedAlternative activeRoot counterValue (GuardedAltF guards bodyExpr) =
      let ((counter1, activeFinal), guards') =
            mapAccumL freshenGuard (counterValue, activeRoot) guards
          (counter2, body') = freshenWalk counter1 activeFinal bodyExpr
       in (counter2, GuardedAltF guards' body')

    freshenGuard :: (Int, Map SurfaceName BinderAnn) -> HsGuardStmtF (Fix HsExprF) -> ((Int, Map SurfaceName BinderAnn), HsGuardStmtF (Fix HsExprF))
    freshenGuard (counterValue, activeAcc) = \case
      GuardBoolF guardExpr ->
        let (counter1, guardExpr') = freshenWalk counterValue activeAcc guardExpr
         in ((counter1, activeAcc), GuardBoolF guardExpr')
      GuardPatF guardPattern guardExpr ->
        let (counter1, guardExpr') = freshenWalk counterValue activeAcc guardExpr
            ((counter2, activeNext), guardPattern') = freshenPattern counter1 activeAcc guardPattern
         in ((counter2, activeNext), GuardPatF guardPattern' guardExpr')
      GuardLetF letMode bindings ->
        case lmRecursion letMode of
          NonRecursiveBinds ->
            let ((counter2, activeNext), bindings') =
                  freshenNonRecursiveBindingRows activeAcc counterValue bindings
             in ((counter2, activeNext), GuardLetF letMode bindings')
          RecursiveOpaqueBinds ->
            let ((counter2, activeNext), bindings') =
                  freshenRecursiveBindingRows activeAcc counterValue bindings
             in ((counter2, activeNext), GuardLetF letMode bindings')

substituteSurfaceName :: SurfaceName -> Fix HsExprF -> Fix HsExprF -> Fix HsExprF
substituteSurfaceName targetName replacement =
  substituteWalk
  where
    wrappedReplacement =
      case replacement of
        Fix (LamF {}) -> Fix (ParF replacement)
        _ -> replacement

    substituteWalk term@(Fix layer) =
      case layer of
        VarF (LocalName binderAnn)
          | binderSurfaceName binderAnn == targetName -> wrappedReplacement
        VarF {} ->
          term
        LamF binderAnn bodyExpr
          | binderSurfaceName binderAnn == targetName -> term
          | otherwise -> Fix (LamF binderAnn (substituteWalk bodyExpr))
        LetF letMode bindings bodyExpr ->
          case lmRecursion letMode of
            NonRecursiveBinds ->
              let bindings' = fmap (fmap substituteWalk) bindings
                  shadowed = bindingRowsShadow targetName bindings
                  body' = if shadowed then bodyExpr else substituteWalk bodyExpr
               in Fix (LetF letMode bindings' body')
            RecursiveOpaqueBinds
              | bindingRowsShadow targetName bindings -> term
              | otherwise ->
                  Fix (LetF letMode (fmap (fmap substituteWalk) bindings) (substituteWalk bodyExpr))
        CaseF scrutineeExpr branches ->
          Fix
            ( CaseF
                (substituteWalk scrutineeExpr)
                ( fmap
                    ( \(branchPattern, branchExpr) ->
                        if any ((== targetName) . binderSurfaceName) (patBinders branchPattern)
                          then (branchPattern, branchExpr)
                          else (branchPattern, substituteWalk branchExpr)
                    )
                    branches
                )
            )
        ClausesF clauses ->
          Fix
            ( ClausesF
                ( fmap
                    ( \(clausePatterns, bodyExpr) ->
                        if any ((== targetName) . binderSurfaceName) (clausePatternBinders clausePatterns)
                          then (clausePatterns, bodyExpr)
                          else (clausePatterns, substituteWalk bodyExpr)
                    )
                    clauses
                )
            )
        DoF statements ->
          Fix (DoF (substituteStatements statements))
        GuardedF alternatives ->
          Fix (GuardedF (fmap substituteGuardedAlternative alternatives))
        MultiIfF alternatives ->
          Fix (MultiIfF (fmap substituteGuardedAlternative alternatives))
        _ ->
          Fix (fmap substituteWalk layer)

    substituteStatements = \case
      [] -> []
      (statement : remaining) ->
        case statement of
          BindStmtF bindPattern bodyExpr ->
            let statement' = BindStmtF bindPattern (substituteWalk bodyExpr)
             in if any ((== targetName) . binderSurfaceName) (patBinders bindPattern)
                  then statement' : remaining
                  else statement' : substituteStatements remaining
          BodyStmtF bodyExpr ->
            BodyStmtF (substituteWalk bodyExpr) : substituteStatements remaining
          LetStmtF letMode bindings ->
            case lmRecursion letMode of
              NonRecursiveBinds ->
                let statement' = LetStmtF letMode (fmap (fmap substituteWalk) bindings)
                 in if bindingRowsShadow targetName bindings
                      then statement' : remaining
                      else statement' : substituteStatements remaining
              RecursiveOpaqueBinds
                | bindingRowsShadow targetName bindings ->
                    LetStmtF letMode bindings : remaining
                | otherwise ->
                    LetStmtF letMode (fmap (fmap substituteWalk) bindings) : substituteStatements remaining

    substituteGuardedAlternative (GuardedAltF guards bodyExpr) =
      let (guards', shadowed) = substituteGuards guards
       in GuardedAltF guards' (if shadowed then bodyExpr else substituteWalk bodyExpr)

    substituteGuards = \case
      [] ->
        ([], False)
      guardStmt : remaining ->
        case guardStmt of
          GuardBoolF guardExpr ->
            let (remaining', shadowed) = substituteGuards remaining
             in (GuardBoolF (substituteWalk guardExpr) : remaining', shadowed)
          GuardPatF guardPattern guardExpr ->
            let guard' = GuardPatF guardPattern (substituteWalk guardExpr)
             in if any ((== targetName) . binderSurfaceName) (patBinders guardPattern)
                  then (guard' : remaining, True)
                  else
                    let (remaining', shadowed) = substituteGuards remaining
                     in (guard' : remaining', shadowed)
          GuardLetF letMode bindings ->
            case lmRecursion letMode of
              NonRecursiveBinds ->
                let guard' = GuardLetF letMode (fmap (fmap substituteWalk) bindings)
                 in if bindingRowsShadow targetName bindings
                      then (guard' : remaining, True)
                      else
                        let (remaining', shadowed) = substituteGuards remaining
                         in (guard' : remaining', shadowed)
              RecursiveOpaqueBinds
                | bindingRowsShadow targetName bindings ->
                    (GuardLetF letMode bindings : remaining, True)
                | otherwise ->
                    let (remaining', shadowed) = substituteGuards remaining
                     in (GuardLetF letMode (fmap (fmap substituteWalk) bindings) : remaining', shadowed)

hsExprBindingCorpus ::
  PreparedContextSite owner ScopeCtx ->
  ConvertedModule ->
  Either HsExprBindingFrontError (HsExprBindingCorpus owner)
hsExprBindingCorpus site convertedModule = do
  let avoidNames = moduleSurfaceNames convertedModule
      binderFloor = moduleBinderFloor convertedModule
      fresheningSyntax = hsExprFresheningSyntax binderFloor avoidNames
  bindingOutcomes <-
    traverse
      (bindingElaborationOutcome fresheningSyntax)
      (zip [0 :: Int ..] (cmBindings convertedModule))
  let ruleSpecs =
        [ SheafTwist.SupportedRuleSpec
            { SheafTwist.srsSupport = principalSupport ruleCtx,
              SheafTwist.srsRule = rawRule (RewriteRuleId (hsExprBindingRuleIdBase + ruleIndex))
            }
        | (ruleIndex, (ruleCtx, rawRule)) <-
            zip [0 :: Int ..] (concatMap boRules bindingOutcomes)
        ]
      factSpecs =
        [ SheafTwist.SupportedFactSpec
            { SheafTwist.sfsSupport = principalSupport factCtx,
              SheafTwist.sfsRule = rawFact (FactRuleId (hsExprBindingRuleIdBase + factIndex))
            }
        | (factIndex, (factCtx, rawFact)) <-
            zip [0 :: Int ..] (concatMap boFacts bindingOutcomes)
        ]
      metrics =
        HsExprBindingRuleMetrics
          { hbrmRedexSiteCount = sum (fmap boDecisionCount bindingOutcomes),
            hbrmAllowedCount = sum (fmap boAllowedCount bindingOutcomes),
            hbrmFresheningCount = sum (fmap boFresheningCount bindingOutcomes),
            hbrmObstructionCount = sum (fmap boObstructionCount bindingOutcomes),
            hbrmGeneratedRuleCount = length ruleSpecs,
            hbrmFactRuleCount = length factSpecs
          }
  ruleBook <-
    first HsExprBindingRuleBookSupportFailure (SheafTwist.supportedRuleBook site ruleSpecs)
  factBook <-
    first HsExprBindingFactBookSupportFailure (SheafTwist.supportedFactBook site factSpecs)
  pure
    HsExprBindingCorpus
      { hbcRules = ruleBook,
        hbcFacts = factBook,
        hbcMetrics = metrics
      }

type BindingOutcome :: Type
data BindingOutcome = BindingOutcome
  { boRules :: ![(ScopeCtx, RewriteRuleId -> HsExprBindingRule)],
    boFacts :: ![(ScopeCtx, FactRuleId -> HsExprBindingFactRule)],
    boDecisionCount :: !Int,
    boAllowedCount :: !Int,
    boFresheningCount :: !Int,
    boObstructionCount :: !Int
  }

instance Semigroup BindingOutcome where
  left <> right =
    BindingOutcome
      { boRules = boRules left <> boRules right,
        boFacts = boFacts left <> boFacts right,
        boDecisionCount = boDecisionCount left + boDecisionCount right,
        boAllowedCount = boAllowedCount left + boAllowedCount right,
        boFresheningCount = boFresheningCount left + boFresheningCount right,
        boObstructionCount = boObstructionCount left + boObstructionCount right
      }

bindingElaborationOutcome ::
  BindingFresheningSyntax HsExprF HsExprBindingSig SurfaceName ->
  (Int, TopLevelBinding) ->
  Either HsExprBindingFrontError BindingOutcome
bindingElaborationOutcome fresheningSyntax (bindingIndex, binding) =
  elaborationOutcome
    fresheningSyntax
    (bindingRootName bindingIndex binding)
    (hsExprScopedBindingSyntax scopedTerm)
    (scopedExprFix scopedTerm)
  where
    scopedTerm = tlbScopedTerm binding

elaborationOutcome ::
  BindingFresheningSyntax HsExprF HsExprBindingSig SurfaceName ->
  String ->
  ScopedBindingSyntax HsExprF HsExprBindingSig ScopeCtx scope ->
  Fix HsExprF ->
  Either HsExprBindingFrontError BindingOutcome
elaborationOutcome fresheningSyntax rootName scopedSyntax rootTerm = do
  elaboration <-
    either
      (Left . HsExprBindingElaborationError rootName)
      Right
      ( compileBindingElaboration
          hsExprBindingRelations
          hsExprBindingLanguageSyntax
          fresheningSyntax
          scopedSyntax
          rootName
          rootTerm
      )
  let report = beReport elaboration
      pathContexts =
        Map.fromList
          [ (bpePath entry, bpeContext entry)
          | entry <- bindingPlanEntries (bePlan elaboration)
          ]
      entryTerms =
        Map.fromList
          [ (bpePath entry, bpeTerm entry)
          | entry <- bindingPlanEntries (bePlan elaboration)
          ]
      freshenedRedexPaths =
        Map.fromList
          [ (bfpFreshenedPath fresheningPlan, bfpRedexPath fresheningPlan)
          | BindingSubstitutionNeedsFreshening fresheningPlan <- blrSubstitutionOutcomes report
          ]
  rules <-
    traverse (generatedRuleOutcome pathContexts) (beGeneratedRules elaboration)
  facts <-
    fmap
      concat
      ( traverse
          (factRuleOutcomes entryTerms freshenedRedexPaths)
          (bindingPlanEntries (bePlan elaboration))
      )
  contractionOutcomes <-
    traverse
      (uncurry (contractionElaborationOutcome fresheningSyntax rootName pathContexts))
      (zip [0 :: Int ..] (beGeneratedRules elaboration))
  let decisions = blrSubstitutionDecisions report
      ownOutcome =
        BindingOutcome
          { boRules = rules,
            boFacts = facts,
            boDecisionCount = length decisions,
            boAllowedCount = length (filter bsdAllowed decisions),
            boFresheningCount =
              length
                [ ()
                | BindingSubstitutionNeedsFreshening _ <- blrSubstitutionOutcomes report
                ],
            boObstructionCount = length (blrCaptureObstructions report)
          }
  Right (foldl' (<>) ownOutcome (catMaybes contractionOutcomes))

contractionElaborationOutcome ::
  BindingFresheningSyntax HsExprF HsExprBindingSig SurfaceName ->
  String ->
  Map BindingPath ScopeCtx ->
  Int ->
  BindingGeneratedRewrite HsExprBindingSig ->
  Either HsExprBindingFrontError (Maybe BindingOutcome)
contractionElaborationOutcome fresheningSyntax parentName pathContexts rewriteIndex generatedRewrite = do
  ruleCtx <- contextForPath pathContexts (bgrContextPath generatedRewrite)
  lhsTerm <- bindingTermFix (bgrLhs generatedRewrite)
  rhsTerm <- bindingTermFix (bgrRhs generatedRewrite)
  if fixNodeCount rhsTerm < fixNodeCount lhsTerm
    then
      Just
        <$> elaborationOutcome
          fresheningSyntax
          (parentName <> "-contracted-" <> show rewriteIndex)
          (hsExprVirtualBindingSyntax ruleCtx)
          rhsTerm
    else Right Nothing

contextForPath ::
  Map BindingPath ScopeCtx ->
  BindingPath ->
  Either HsExprBindingFrontError ScopeCtx
contextForPath pathContexts contextPath =
  maybe
    (Left (HsExprBindingMissingContextPath (bindingPathName contextPath)))
    Right
    (Map.lookup contextPath pathContexts)

generatedRuleOutcome ::
  Map BindingPath ScopeCtx ->
  BindingGeneratedRewrite HsExprBindingSig ->
  Either HsExprBindingFrontError (ScopeCtx, RewriteRuleId -> HsExprBindingRule)
generatedRuleOutcome pathContexts generatedRewrite = do
  ruleCtx <- contextForPath pathContexts (bgrContextPath generatedRewrite)
  lhsPattern <- loweredGroundPattern (bgrLhs generatedRewrite)
  rhsPattern <- loweredGroundPattern (bgrRhs generatedRewrite)
  let condition =
        case bgrGuard generatedRewrite of
          BindingRewriteUnguarded ->
            Nothing
          BindingRewriteRequiresFact relationRef _guardTerm ->
            Just (RewriteCondition (guardHasFact (relationRefFactId relationRef) [GuardRoot]))
  Right
    ( ruleCtx,
      \ruleId ->
        RawRewriteRule
          { rrId = ruleId,
            rrLhs = lhsPattern,
            rrRhs = rhsPattern,
            rrCondition = condition,
            rrApplicationCondition = Nothing,
            rrPostSubst = Nothing
          }
    )

factRuleOutcomes ::
  Map BindingPath (Term HsExprBindingSig "Expr") ->
  Map BindingPath BindingPath ->
  BindingPlanEntry HsExprBindingSig ScopeCtx ->
  Either HsExprBindingFrontError [(ScopeCtx, FactRuleId -> HsExprBindingFactRule)]
factRuleOutcomes entryTerms freshenedRedexPaths entry =
  traverse factRuleOutcome (bpeFacts entry)
  where
    derivablePatternTerm =
      case Map.lookup (bpePath entry) freshenedRedexPaths of
        Nothing ->
          Right (bpeTerm entry)
        Just redexPath ->
          maybe
            (Left (HsExprBindingMissingContextPath (bindingPathName redexPath)))
            Right
            (Map.lookup redexPath entryTerms)
    factRuleOutcome (BindingFact relationRef _factArgs) = do
      patternTerm <- derivablePatternTerm
      entryPattern <- loweredGroundPattern patternTerm
      Right
        ( bpeContext entry,
          \factRuleId ->
            FactRule
              { frId = factRuleId,
                frName = bindingPathName (bpePath entry) <> "/substitution-allowed",
                frPattern = entryPattern,
                frProjection = [GuardRoot],
                frFactId = relationRefFactId relationRef,
                frCondition = Nothing
              }
        )

bindingRootName :: Int -> TopLevelBinding -> String
bindingRootName bindingIndex binding =
  case tlbNames binding of
    [] -> "unnamed-" <> show bindingIndex
    (firstName : _) ->
      case sanitizeSegment (occNameString (rdrNameOcc firstName)) of
        "" -> "unnamed-" <> show bindingIndex
        sanitized -> sanitized

sanitizeSegment :: String -> String
sanitizeSegment =
  fmap (\character -> if character == '/' then '-' else character)

moduleSurfaceNames :: ConvertedModule -> Set SurfaceName
moduleSurfaceNames convertedModule =
  foldMap (termSurfaceNames . scopedExprFix . tlbScopedTerm) (cmBindings convertedModule)

termSurfaceNames :: Fix HsExprF -> Set SurfaceName
termSurfaceNames (Fix layer) =
  ownNames <> foldMap termSurfaceNames layer
  where
    ownNames =
      case layer of
        VarF (GlobalName rdrName) -> Set.singleton (hsExprSurfaceName rdrName)
        VarF (LocalName binderAnn) -> Set.singleton (binderSurfaceName binderAnn)
        LamF binderAnn _ -> Set.singleton (binderSurfaceName binderAnn)
        LetF _ bindings _ -> bindingRowSurfaceNames bindings
        CaseF _ branches ->
          fold
            [ Set.fromList (fmap binderSurfaceName (patBinders branchPattern))
            | (branchPattern, _) <- branches
            ]
        ClausesF clauses ->
          fold
            [ Set.fromList (fmap binderSurfaceName (clausePatternBinders clausePatterns))
            | (clausePatterns, _) <- clauses
            ]
        DoF statements ->
          fold
            [ case statement of
                BindStmtF bindPattern _ -> Set.fromList (fmap binderSurfaceName (patBinders bindPattern))
                LetStmtF _ bindings -> bindingRowSurfaceNames bindings
                BodyStmtF _ -> Set.empty
            | statement <- statements
            ]
        GuardedF alternatives ->
          foldMap guardedAltSurfaceNames alternatives
        MultiIfF alternatives ->
          foldMap guardedAltSurfaceNames alternatives
        _ -> Set.empty

guardedAltSurfaceNames :: GuardedAltF r -> Set SurfaceName
guardedAltSurfaceNames (GuardedAltF guards _) =
  foldMap guardStmtSurfaceNames guards

guardStmtSurfaceNames :: HsGuardStmtF r -> Set SurfaceName
guardStmtSurfaceNames = \case
  GuardBoolF _ -> Set.empty
  GuardPatF guardPattern _ -> Set.fromList (fmap binderSurfaceName (patBinders guardPattern))
  GuardLetF _ bindings -> bindingRowSurfaceNames bindings

moduleBinderFloor :: ConvertedModule -> Int
moduleBinderFloor convertedModule =
  foldl' max 0 (foldMap (termBinderKeys . scopedExprFix . tlbScopedTerm) (cmBindings convertedModule))

termBinderKeys :: Fix HsExprF -> [Int]
termBinderKeys (Fix layer) =
  ownKeys <> foldMap termBinderKeys layer
  where
    ownKeys =
      case layer of
        VarF (LocalName binderAnn) -> [binderIdKey (baId binderAnn)]
        LamF binderAnn _ -> [binderIdKey (baId binderAnn)]
        LetF _ bindings _ -> bindingRowBinderKeys bindings
        CaseF _ branches ->
          concat
            [ fmap (binderIdKey . baId) (patBinders branchPattern)
            | (branchPattern, _) <- branches
            ]
        ClausesF clauses ->
          concat
            [ fmap (binderIdKey . baId) (clausePatternBinders clausePatterns)
            | (clausePatterns, _) <- clauses
            ]
        DoF statements ->
          concat
            [ case statement of
                BindStmtF bindPattern _ -> fmap (binderIdKey . baId) (patBinders bindPattern)
                LetStmtF _ bindings -> bindingRowBinderKeys bindings
                BodyStmtF _ -> []
            | statement <- statements
            ]
        GuardedF alternatives ->
          concatMap guardedAltBinderKeys alternatives
        MultiIfF alternatives ->
          concatMap guardedAltBinderKeys alternatives
        _ -> []

guardedAltBinderKeys :: GuardedAltF r -> [Int]
guardedAltBinderKeys (GuardedAltF guards _) =
  concatMap guardStmtBinderKeys guards

guardStmtBinderKeys :: HsGuardStmtF r -> [Int]
guardStmtBinderKeys = \case
  GuardBoolF _ -> []
  GuardPatF guardPattern _ -> fmap (binderIdKey . baId) (patBinders guardPattern)
  GuardLetF _ bindings -> bindingRowBinderKeys bindings
