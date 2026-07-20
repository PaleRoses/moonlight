{-|
Authoritative frozen umbrella re-export of the moonlight-core foundation
surface. Direct @Moonlight.Core.*@ module imports outside this module are
internal-use-at-own-risk, except for the deliberately explicit
"Moonlight.Core.Unsound" trust boundary. Unsafe refinement constructors are not
re-exported here.
-}
module Moonlight.Core
  ( -- * Numeric tower
    module ScalarX,
    module NumericX,
    module ApproxEqX,
    -- * Canonical and exact numbers
    module CanonX,
    module CanonicalNumberX,
    module ExactTokenX,
    -- * Validation, refinement and niches
    module ValidationX,
    module RefinementX,
    module NicheX,
    -- * Type-level utilities
    module TypeLevelX,
    -- * Identifiers, domains, capabilities and language
    module DomainIdX,
    PatternVar,
    mkPatternVar,
    module IdentifierX,
    module IdentifierEGraphX,
    module CapabilityX,
    module LanguageX,
    module ModuleIdentifierX,
    -- * Theories and patterns
    module TheoryX,
    module PatternX,
    module MatchX,
    module SubstitutionX,
    -- * Errors
    module ErrorX,
    -- * Hashing
    module StableHashX,
    module HashX,
    module DenseKeyX,
    -- * Finiteness, order and fixpoints
    module FiniteX,
    module OrderX,
    module OrdCollectionX,
    module FixOrderX,
    module FixpointX,
    module FixpointDenseX,
    -- * Maps, registries, queues and normalization
    module AggregateX,
    module BoundaryX,
    module CardinalityX,
    module DedupX,
    module GuidanceX,
    module LawNameX,
    module MapAccumX,
    module MapInvertX,
    module ProofManifestX,
    module QueueX,
    module RelationalX,
    module ScanX,
    module EGraphProgramX,
    module SiteProgramX,
    module TermDatabaseX,
    module TermDatabaseCanonicalizeX,
    module TotalRegistryX,
    module UnionFindX,
    module UnionFindTransactionX,
    module IsoNormX,
  )
where

import Moonlight.Core.Aggregate as AggregateX
import Moonlight.Core.ApproxEq as ApproxEqX
import Moonlight.Core.Boundary as BoundaryX
import Moonlight.Core.Cardinality as CardinalityX
import Moonlight.Core.Canon as CanonX
import Moonlight.Core.CanonicalNumber as CanonicalNumberX
  ( CanonicalNumber (..),
    CanonicalFiniteValue,
    mkCanonicalFiniteValue,
    mkCanonicalFiniteNumber,
    canonicalFiniteValue,
    canonicalNumberFromDouble,
    canonicalNumberToMaybeDouble,
  )
import Moonlight.Core.Capability as CapabilityX
import Moonlight.Core.Dedup as DedupX
import Moonlight.Core.DenseKey as DenseKeyX
import Moonlight.Core.DomainId as DomainIdX
import Moonlight.Core.Error as ErrorX
import Moonlight.Core.EGraph.Program as EGraphProgramX
import Moonlight.Core.ExactToken as ExactTokenX
import Moonlight.Core.Finite as FiniteX
import Moonlight.Core.Fix.Order as FixOrderX
import Moonlight.Core.Fixpoint as FixpointX
import Moonlight.Core.Fixpoint.Dense as FixpointDenseX
import Moonlight.Core.Guidance as GuidanceX
import Moonlight.Core.Hash as HashX
import Moonlight.Core.Identifier as IdentifierX
import Moonlight.Core.Identifier.EGraph (PatternVar, mkPatternVar)
import Moonlight.Core.Identifier.EGraph as IdentifierEGraphX hiding (PatternVar, mkPatternVar)
import Moonlight.Core.IsoNorm as IsoNormX
import Moonlight.Core.Language as LanguageX
import Moonlight.Core.LawName as LawNameX
import Moonlight.Core.MapAccum as MapAccumX
import Moonlight.Core.MapInvert as MapInvertX
import Moonlight.Core.Match as MatchX
import Moonlight.Core.ModuleIdentifier as ModuleIdentifierX
import Moonlight.Core.Niche as NicheX
import Moonlight.Core.Numeric as NumericX
  ( OrderedRing,
    OrderedField,
    ContinuousField,
  )
import Moonlight.Core.OrdCollection as OrdCollectionX
import Moonlight.Core.Order as OrderX
import Moonlight.Core.Pattern as PatternX
import Moonlight.Core.ProofManifest as ProofManifestX
import Moonlight.Core.Queue as QueueX
import Moonlight.Core.Refinement as RefinementX
import Moonlight.Core.Relational as RelationalX
import Moonlight.Core.Scan as ScanX
import Moonlight.Core.Scalar as ScalarX
import Moonlight.Core.Site.Program as SiteProgramX
import Moonlight.Core.StableHash as StableHashX
import Moonlight.Core.Substitution as SubstitutionX
import Moonlight.Core.Term.Database as TermDatabaseX
import Moonlight.Core.Term.Database.Canonicalize as TermDatabaseCanonicalizeX
import Moonlight.Core.Theory as TheoryX
import Moonlight.Core.TotalRegistry as TotalRegistryX
import Moonlight.Core.TypeLevel as TypeLevelX
import Moonlight.Core.UnionFind as UnionFindX
import Moonlight.Core.UnionFind.Transaction as UnionFindTransactionX
import Moonlight.Core.Validation as ValidationX
