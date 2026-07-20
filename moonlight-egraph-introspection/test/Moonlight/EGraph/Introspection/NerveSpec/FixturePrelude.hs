{-# LANGUAGE DuplicateRecordFields #-}

module Moonlight.EGraph.Introspection.NerveSpec.FixturePrelude
  ( module Data.Function,
    module Data.List,
    module Moonlight.Algebra,
    module Moonlight.Core,
    module Moonlight.EGraph.Pure.Analysis,
    module Moonlight.EGraph.Pure.Context,
    module Moonlight.EGraph.Pure.Kernel.HashCons,
    module Moonlight.EGraph.Pure.Context.Proof,
    module Moonlight.Saturation.Context.Error,
    module Moonlight.Saturation.Context.Program.Spec,
    module Moonlight.Saturation.Context.Program.View,
    module Moonlight.Saturation.Context.Runtime.Report,
    module Moonlight.Saturation.Core,
    module Moonlight.EGraph.Pure.Saturation.Matching,
    module Moonlight.Control.Schedule,
    module Moonlight.Rewrite.ProofContext,
    module Moonlight.Rewrite.Runtime,
    module Moonlight.EGraph.Pure.Types,
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
    module Moonlight.EGraph.Introspection.Analysis.Reduction,
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
    module Data.Fix,
    module Moonlight.Rewrite.Algebra,
    module Moonlight.Rewrite.System,
    module Test.Tasty.HUnit,
  )
where

import Data.Function ((&))
import Data.List (dropWhileEnd)
import Moonlight.Algebra hiding (composeDelta, degree, identityDelta, member, neg, normalizeDelta)
import Moonlight.Core hiding (Edge, emptySupport, find, member)
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
import Moonlight.EGraph.Introspection.Analysis.Reduction
import Moonlight.EGraph.Introspection.Analysis.Resolution.Descent
import Moonlight.Sheaf.Site hiding (applyDelta)
import Moonlight.Sheaf.Obstruction hiding (Obstruction, ObstructionVerdict, WitnessMismatch)
import Moonlight.EGraph.Introspection.Core.Rewrite hiding (CompositionError, PatternRewriteError)
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
import Moonlight.EGraph.Pure.Analysis
import Moonlight.EGraph.Pure.Context
import Moonlight.EGraph.Pure.Kernel.HashCons
import Moonlight.EGraph.Pure.Context.Proof
import Moonlight.Saturation.Context.Error
import Moonlight.Saturation.Context.Program.Spec
import Moonlight.Saturation.Context.Program.View
import Moonlight.Saturation.Context.Runtime.Report
import Moonlight.Saturation.Core
import Moonlight.EGraph.Pure.Saturation.Matching hiding (MatchSite)
import Moonlight.Control.Schedule
import Moonlight.Rewrite.ProofContext hiding
  ( ProofGraph (..),
    proofBetween,
    proofClassWitnesses,
    proofClassesReachableFrom,
    proofGraph,
    proofReachability,
    proofRelated,
    recordAnnotatedProofStep,
    recordProofStepWith,
    serializeProofLog,
    summarizeProofLog,
  )
import Moonlight.Rewrite.Runtime
import Moonlight.EGraph.Pure.Types
import Moonlight.Homology hiding (cellDimension)
import Data.Fix
import Moonlight.Rewrite.Algebra
import Moonlight.Rewrite.System
import Test.Tasty.HUnit
