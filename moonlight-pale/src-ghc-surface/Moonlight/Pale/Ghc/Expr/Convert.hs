module Moonlight.Pale.Ghc.Expr.Convert
  ( SourceRegion (..),
    TopLevelBinding (..),
    ConvertedModule (..),
    ConvertedModuleMetrics (..),
    ConvertObstruction (..),
    convertHsExpr,
    convertModule,
    convertHaskellSource,
    convertedModuleMetrics,
    hsOpaqueTagName,
    hsPatOpaqueTagName,
  )
where

import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
import Moonlight.Pale.Ghc.Expr.Convert.Metrics
import Moonlight.Pale.Ghc.Expr.Opaque (hsOpaqueTagName, hsPatOpaqueTagName)
import Moonlight.Pale.Ghc.Expr.Syntax (SourceRegion (..))
