{-# LANGUAGE PackageImports #-}
module Moonlight.EGraph.Saturation.CohomologicalSpec.Prelude
  ( module Moonlight.EGraph.Saturation.CohomologicalSpec.Fixture,
    module Data.Function,
    module Data.List,
    ConstraintExpr (And),
    module Moonlight.EGraph.Pure.Analysis,
    module Moonlight.EGraph.Effect.CoveringSurface,
    module Moonlight.Rewrite.Algebra,
    module Moonlight.EGraph.Pure.Kernel.HashCons,
    module Moonlight.Core,
    module Moonlight.EGraph.Pure.Relational,
    module Moonlight.EGraph.Test.Saturation,
    module Moonlight.EGraph.Pure.Saturation.Matching,
    module Moonlight.EGraph.Pure.Context,
    module Moonlight.Rewrite.System,
    module Moonlight.EGraph.Pure.Context.Proof,
    module Moonlight.EGraph.Pure.Rebuild,
    module Moonlight.EGraph.Introspection.Analysis.Resolution,
    module Moonlight.Sheaf.Obstruction.Cohomological.Cache,
    module Moonlight.Derived.Pruning,
    module Moonlight.EGraph.Pure.Types,
    module Moonlight.Homology,
    module Test.Tasty,
    module Test.Tasty.HUnit
  )
where

import Data.Function
import Data.List
import Moonlight.Constraint (ConstraintExpr (And))
import Moonlight.EGraph.Effect.CoveringSurface
import Moonlight.EGraph.Pure.Analysis
import Moonlight.Rewrite.Algebra
import Moonlight.EGraph.Pure.Kernel.HashCons
import Moonlight.Core hiding (Edge, find, projection, union)
import Moonlight.EGraph.Pure.Relational
import Moonlight.EGraph.Test.Saturation
import Moonlight.EGraph.Pure.Saturation.Matching
import "moonlight-egraph-introspection" Moonlight.EGraph.Introspection.Analysis.Resolution
import Moonlight.EGraph.Pure.Context
import Moonlight.Rewrite.System
import Moonlight.EGraph.Pure.Context.Proof
import Moonlight.EGraph.Pure.Rebuild
import Moonlight.EGraph.Pure.Types
import Moonlight.EGraph.Saturation.CohomologicalSpec.Fixture
import Moonlight.Homology
import Moonlight.Sheaf.Obstruction as Moonlight.Sheaf.Obstruction.Cohomological.Cache
  ( CohomologicalCache,
    ObstructionCacheKey (..),
    emptyCohomologicalCache,
    insertCachedObstruction,
    insertCachedObstructionForDependencies,
    invalidateCachedObstructions,
    lookupCachedObstruction,
  )
import Moonlight.Derived.Pruning
import Test.Tasty
import Test.Tasty.HUnit
