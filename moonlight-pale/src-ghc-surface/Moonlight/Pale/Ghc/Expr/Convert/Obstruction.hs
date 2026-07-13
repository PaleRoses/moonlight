module Moonlight.Pale.Ghc.Expr.Convert.Obstruction
  ( ConvertObstruction (..),
  )
where

import Data.Kind (Type)
import Moonlight.Core (BinderId)
import Moonlight.Pale.Ghc.Expr.Scope
  ( ScopeId,
    ScopeIdFailure,
    ScopeIndexFailure,
  )

type ConvertObstruction :: Type
data ConvertObstruction
  = ConvertParseFailure !String
  | ConvertScopeIndexFailure !ScopeIndexFailure
  | ConvertFreshScopeIdFailure !Int !ScopeIdFailure
  | ConvertMissingScopeDepth !ScopeId
  | ConvertMissingBinderIntro !BinderId
  | ConvertMissingScopeSummaryDepth !ScopeId
  deriving stock (Eq, Ord, Show)
