{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.FreeScopes
  ( ScopeAlgebra (..),
    inferLetRecursion,
    freeScopesExpr,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Kind (Type)
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

type ScopeAlgebra :: Type -> Type
data ScopeAlgebra failure = ScopeAlgebra
  { saScopeDepth :: ScopeId -> Either failure Int,
    saBinderIntro :: BinderAnn -> Either failure ScopeId
  }

inferLetRecursion :: ScopeAlgebra failure -> [(HsPatF, ScopedExpr)] -> Either failure LetRecursion
inferLetRecursion scopeAlgebra bindingRows =
  case bindingRows of
    [(rowPattern, rhsExpr)] -> do
      binderScopes <- traverse (saBinderIntro scopeAlgebra) (patBinders rowPattern)
      pure $
        if any (\scopeValue -> freeScopeSummaryContains scopeValue (seFreeScopes rhsExpr)) binderScopes
          then RecursiveOpaqueBinds
          else NonRecursiveBinds
    _ ->
      pure RecursiveOpaqueBinds

mergeScopeSummary :: ScopeAlgebra failure -> FreeScopeSummary -> FreeScopeSummary -> Either failure FreeScopeSummary
mergeScopeSummary scopeAlgebra =
  mergeFreeScopeSummaryByEither (saScopeDepth scopeAlgebra)

mergeScopeSummaries :: ScopeAlgebra failure -> [FreeScopeSummary] -> Either failure FreeScopeSummary
mergeScopeSummaries scopeAlgebra =
  foldM (mergeScopeSummary scopeAlgebra) emptyFreeScopeSummary

deleteBinderScope :: ScopeAlgebra failure -> BinderAnn -> FreeScopeSummary -> Either failure FreeScopeSummary
deleteBinderScope scopeAlgebra binderAnn summaryValue = do
  binderScope <- saBinderIntro scopeAlgebra binderAnn
  pure (deleteFreeScopeSummary binderScope summaryValue)

deletePatBinderScopes :: ScopeAlgebra failure -> HsPatF -> FreeScopeSummary -> Either failure FreeScopeSummary
deletePatBinderScopes scopeAlgebra patternValue summaryValue =
  foldM (\acc binderAnn -> deleteBinderScope scopeAlgebra binderAnn acc) summaryValue (patBinders patternValue)

freeScopesExpr :: ScopeAlgebra failure -> HsExprF ScopedExpr -> Either failure FreeScopeSummary
freeScopesExpr scopeAlgebra nodeValue =
  case nodeValue of
    VarF (GlobalName _) ->
      pure emptyFreeScopeSummary
    VarF (LocalName binderAnn) ->
      singletonFreeScopeSummary <$> saBinderIntro scopeAlgebra binderAnn
    LamF binderAnn bodyExpr ->
      deleteBinderScope scopeAlgebra binderAnn (seFreeScopes bodyExpr)
    LetF letModeValue bindingValues bodyExpr ->
      freeScopesLet scopeAlgebra letModeValue bindingValues (seFreeScopes bodyExpr)
    CaseF scrutineeExpr branchValues -> do
      branchFree <- traverse (freeScopesCaseAlternative scopeAlgebra) branchValues >>= mergeScopeSummaries scopeAlgebra
      mergeScopeSummary scopeAlgebra (seFreeScopes scrutineeExpr) branchFree
    DoF statementValues ->
      freeScopesDo scopeAlgebra statementValues
    GuardedF guardedAlts ->
      traverse (freeScopesGuardedAlt scopeAlgebra) guardedAlts >>= mergeScopeSummaries scopeAlgebra
    MultiIfF guardedAlts ->
      traverse (freeScopesGuardedAlt scopeAlgebra) guardedAlts >>= mergeScopeSummaries scopeAlgebra
    ClausesF clauseValues ->
      traverse (freeScopesClause scopeAlgebra) clauseValues >>= mergeScopeSummaries scopeAlgebra
    _ ->
      mergeScopeSummaries scopeAlgebra (fmap seFreeScopes (toList nodeValue))

freeScopesLet ::
  ScopeAlgebra failure ->
  LetMode ->
  [(HsPatF, ScopedExpr)] ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesLet scopeAlgebra letModeValue bindingValues bodyFree0 = do
  bodyFree <-
    foldM
      (\acc (rowPattern, _) -> deletePatBinderScopes scopeAlgebra rowPattern acc)
      bodyFree0
      bindingValues
  rhsFree <-
    case lmRecursion letModeValue of
      NonRecursiveBinds ->
        mergeScopeSummaries scopeAlgebra (fmap (seFreeScopes . snd) bindingValues)
      RecursiveOpaqueBinds ->
        mergeScopeSummaries scopeAlgebra =<<
          traverse
            (\(rowPattern, rhsExpr) -> deletePatBinderScopes scopeAlgebra rowPattern (seFreeScopes rhsExpr))
            bindingValues
  mergeScopeSummary scopeAlgebra rhsFree bodyFree

freeScopesCaseAlternative :: ScopeAlgebra failure -> (HsPatF, ScopedExpr) -> Either failure FreeScopeSummary
freeScopesCaseAlternative scopeAlgebra (casePattern, branchExpr) =
  deletePatBinderScopes scopeAlgebra casePattern (seFreeScopes branchExpr)

freeScopesClause :: ScopeAlgebra failure -> ([HsPatF], ScopedExpr) -> Either failure FreeScopeSummary
freeScopesClause scopeAlgebra (clausePatterns, bodyExpr) =
  foldM (flip (deletePatBinderScopes scopeAlgebra)) (seFreeScopes bodyExpr) clausePatterns

freeScopesDo :: ScopeAlgebra failure -> [HsStmtF ScopedExpr] -> Either failure FreeScopeSummary
freeScopesDo scopeAlgebra = \case
  [] ->
    pure emptyFreeScopeSummary
  statementValue : remainingValues -> do
    laterFree <- freeScopesDo scopeAlgebra remainingValues
    freeScopesStmt scopeAlgebra statementValue laterFree

freeScopesStmt :: ScopeAlgebra failure -> HsStmtF ScopedExpr -> FreeScopeSummary -> Either failure FreeScopeSummary
freeScopesStmt scopeAlgebra statementValue laterFree =
  case statementValue of
    BindStmtF bindPattern rhsExpr -> do
      visibleLaterFree <- deletePatBinderScopes scopeAlgebra bindPattern laterFree
      mergeScopeSummary scopeAlgebra (seFreeScopes rhsExpr) visibleLaterFree
    BodyStmtF exprValue ->
      mergeScopeSummary scopeAlgebra (seFreeScopes exprValue) laterFree
    LetStmtF letModeValue bindingValues ->
      freeScopesLet scopeAlgebra letModeValue bindingValues laterFree

freeScopesGuardedAlt :: ScopeAlgebra failure -> GuardedAltF ScopedExpr -> Either failure FreeScopeSummary
freeScopesGuardedAlt scopeAlgebra guardedAlt =
  freeScopesGuardStmts scopeAlgebra (gaGuards guardedAlt) (seFreeScopes (gaBody guardedAlt))

freeScopesGuardStmts ::
  ScopeAlgebra failure ->
  [HsGuardStmtF ScopedExpr] ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesGuardStmts scopeAlgebra guardStatements bodyFree =
  case guardStatements of
    [] ->
      pure bodyFree
    guardStatement : remainingStatements -> do
      laterFree <- freeScopesGuardStmts scopeAlgebra remainingStatements bodyFree
      freeScopesGuardStmt scopeAlgebra guardStatement laterFree

freeScopesGuardStmt ::
  ScopeAlgebra failure ->
  HsGuardStmtF ScopedExpr ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesGuardStmt scopeAlgebra guardStatement laterFree =
  case guardStatement of
    GuardBoolF exprValue ->
      mergeScopeSummary scopeAlgebra (seFreeScopes exprValue) laterFree
    GuardPatF guardPattern rhsExpr -> do
      visibleLaterFree <- deletePatBinderScopes scopeAlgebra guardPattern laterFree
      mergeScopeSummary scopeAlgebra (seFreeScopes rhsExpr) visibleLaterFree
    GuardLetF letModeValue bindingValues ->
      freeScopesLet scopeAlgebra letModeValue bindingValues laterFree
