module Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
  ( module Data.Function,
    module Moonlight.Algebra,
    module Moonlight.Analysis.Cohomology,
    module Moonlight.Sheaf.Site,
    module Moonlight.EGraph.Introspection.Core.Context.Pairs,
    module Moonlight.EGraph.Introspection.Core.Context.Tag,
    module Moonlight.EGraph.Introspection.Analysis.Descent,
    module Moonlight.Analysis.Equivariant,
    module Moonlight.EGraph.Homology.Gerbe,
    module Moonlight.Analysis.Obstruction,
    module Moonlight.EGraph.Introspection.Analysis.Resolution,
    module Moonlight.Analysis.Homotopy,
    module Moonlight.EGraph.Introspection.Core.HsExpr,
    module Moonlight.Analysis.Relative,
    module Moonlight.Analysis.Reduction,
    module Moonlight.EGraph.Introspection.Analysis.Resolution.Descent,
    module Moonlight.EGraph.Introspection.Core.Rewrite,
    module Moonlight.Control.Schedule,
    module Moonlight.Control.Candidate,
    module Moonlight.Control.Count,
    module Moonlight.Control.Weight,
    module Moonlight.Control.Scheduling.Successor,
    module Moonlight.Control.Scheduling.Successor.Runtime,
    module Moonlight.Control.Scheduling.Support,
    module Moonlight.EGraph.Introspection.Analysis.Spectral,
    module Moonlight.Sheaf.Obstruction,
    module Moonlight.Analysis.Persistence.Filtration,
    module Moonlight.Derived.Pruning,
    module Moonlight.Derived.Pruning,
    module Moonlight.Derived.Pruning,
    module Moonlight.Analysis.Summary,
    module Moonlight.Analysis.Termination,
    module Moonlight.Homology,
    checkCoboundaryNilpotence,
    module Test.Tasty,
    module Test.Tasty.QuickCheck,
  )
where

import Data.Function ((&))
import Moonlight.Algebra (JoinSemilattice (..), MeetSemilattice (..))
import Moonlight.Analysis.Cohomology
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.EGraph.Introspection.Core.Context.Pairs
import Moonlight.EGraph.Introspection.Core.Context.Tag
import Moonlight.EGraph.Introspection.Analysis.Descent
import Moonlight.Analysis.Equivariant
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.EGraph.Homology.Gerbe
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Analysis.Obstruction
import Moonlight.EGraph.Introspection.Analysis.Resolution
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Analysis.Homotopy
import Moonlight.Sheaf.Site (grothendieckChainComplexFromSite)
import Moonlight.EGraph.Introspection.Core.HsExpr
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Analysis.Relative
import Moonlight.Analysis.Reduction
import Moonlight.EGraph.Introspection.Analysis.Resolution.Descent
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Obstruction hiding (Obstruction, ObstructionVerdict, WitnessMismatch)
import Moonlight.EGraph.Introspection.Core.Rewrite
import Moonlight.Control.Schedule
import Moonlight.Control.Candidate
import Moonlight.Control.Count
import Moonlight.Control.Weight
import Moonlight.Control.Schedule
import Moonlight.Control.Scheduling.Successor
import Moonlight.Control.Scheduling.Successor.Runtime
import Moonlight.Control.Scheduling.Support
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.EGraph.Introspection.Analysis.Spectral
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Analysis.Persistence.Filtration
import Moonlight.Derived.Pruning
import Moonlight.Derived.Pruning
import Moonlight.Derived.Pruning
import Moonlight.Control.Schedule
import Moonlight.Control.Candidate
import Moonlight.Control.Count
import Moonlight.Control.Weight
import Moonlight.Control.Schedule
import Moonlight.Control.Scheduling.Support
import Moonlight.Analysis.Summary
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Analysis.Termination
import Moonlight.Homology hiding (cellDimension)
import Moonlight.Sheaf.Cochain.Coboundary (checkCoboundaryNilpotence)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckMaxSize (..),
    counterexample,
    property,
    testProperty,
  )
