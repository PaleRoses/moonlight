module Moonlight.Pale.Ghc.Expr.Convert.Metrics
  ( ConvertedModuleMetrics (..),
    convertedModuleMetrics,
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra (ConvertedModule (..), TopLevelBinding (..))
import Moonlight.Pale.Ghc.Expr.Scope (freeScopeSummarySize, scopeObservedCount)
import Moonlight.Pale.Ghc.Expr.Syntax

type ConvertedModuleMetrics :: Type
data ConvertedModuleMetrics = ConvertedModuleMetrics
  { cmmBindingCount :: !Int,
    cmmObservedContextCount :: !Int,
    cmmLambdaSiteCount :: !Int,
    cmmLetSiteCount :: !Int,
    cmmScopedExprCount :: !Int,
    cmmGlobalVarRefCount :: !Int,
    cmmLocalVarRefCount :: !Int,
    cmmMaxFreeScopeCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ScopedExprMetrics :: Type
data ScopedExprMetrics = ScopedExprMetrics
  { semScopedExprCount :: !Int,
    semGlobalVarRefCount :: !Int,
    semLocalVarRefCount :: !Int,
    semMaxFreeScopeCount :: !Int
  }

instance Semigroup ScopedExprMetrics where
  leftMetrics <> rightMetrics =
    ScopedExprMetrics
      { semScopedExprCount = semScopedExprCount leftMetrics + semScopedExprCount rightMetrics,
        semGlobalVarRefCount = semGlobalVarRefCount leftMetrics + semGlobalVarRefCount rightMetrics,
        semLocalVarRefCount = semLocalVarRefCount leftMetrics + semLocalVarRefCount rightMetrics,
        semMaxFreeScopeCount = max (semMaxFreeScopeCount leftMetrics) (semMaxFreeScopeCount rightMetrics)
      }

instance Monoid ScopedExprMetrics where
  mempty =
    ScopedExprMetrics
      { semScopedExprCount = 0,
        semGlobalVarRefCount = 0,
        semLocalVarRefCount = 0,
        semMaxFreeScopeCount = 0
      }

convertedModuleMetrics :: ConvertedModule -> ConvertedModuleMetrics
convertedModuleMetrics convertedModule =
  let scopeIndex = cmScopeIndex convertedModule
      scopedMetrics =
        foldMap
          (scopedExprMetrics . tlbScopedTerm)
          (cmBindings convertedModule)
   in ConvertedModuleMetrics
        { cmmBindingCount = length (cmBindings convertedModule),
          cmmObservedContextCount = scopeObservedCount scopeIndex,
          cmmLambdaSiteCount = length (cmLambdaSites convertedModule),
          cmmLetSiteCount = length (cmLetSites convertedModule),
          cmmScopedExprCount = semScopedExprCount scopedMetrics,
          cmmGlobalVarRefCount = semGlobalVarRefCount scopedMetrics,
          cmmLocalVarRefCount = semLocalVarRefCount scopedMetrics,
          cmmMaxFreeScopeCount = semMaxFreeScopeCount scopedMetrics
        }

scopedExprMetrics :: ScopedExpr -> ScopedExprMetrics
scopedExprMetrics scopedExpr =
  let nodeValue = seNode scopedExpr
      childMetrics = foldMap scopedExprMetrics nodeValue
      localMetrics =
        case nodeValue of
          VarF (GlobalName _) ->
            mempty {semGlobalVarRefCount = 1}
          VarF (LocalName _) ->
            mempty {semLocalVarRefCount = 1}
          _ ->
            mempty
      freeScopeCount = freeScopeSummarySize (seFreeScopes scopedExpr)
   in childMetrics
        <> localMetrics
        <> mempty
          { semScopedExprCount = 1,
            semMaxFreeScopeCount = freeScopeCount
          }
