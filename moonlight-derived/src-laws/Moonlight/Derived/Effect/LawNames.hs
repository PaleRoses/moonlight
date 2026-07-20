{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Effect.LawNames
  ( LawName (..)
  , lawName
  ) where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..))

type LawName :: Type
data LawName
  = PosetReflexive
  | PosetAntisymmetric
  | PosetTransitive
  | PosetUpperLowerDual
  | PosetTopoRespectsEdges
  | MatrixIdentity
  | MatrixTransposeInvolution
  | MatrixRestrictIdempotent
  | MatrixBlockedSparseRepresentationAgreement
  | ComplexDifferentialSquaresZero
  | ComplexNormalizationIdempotent
  | ComplexMinimizationHypercohomologyInvariant
  | ComplexMinimizationMicrosupportInvariant
  | ComplexMinimizationDegreeWindowStable
  | ShiftReindexesHypercohomology
  | MapSquaresCommute
  | ConeEulerAdditive
  | TriangleRotationInvariant
  | QuasiIsoConeAcyclic
  | VerdierInvolutionInvariants
  | RHomTensorAdjunctionDims
  | TruncationTriangleExact
  | FunctorQuillenARejectsBadFiber
  | MorseSparseDigestCacheCoherence
  | DerivedDeterministicFixture
  deriving stock (Eq, Ord, Show)

instance IsLawName LawName where
  lawNameText = lawName

lawName :: LawName -> String
lawName lawNameValue =
  case lawNameValue of
    PosetReflexive -> "derived_poset_reflexive"
    PosetAntisymmetric -> "derived_poset_antisymmetric"
    PosetTransitive -> "derived_poset_transitive"
    PosetUpperLowerDual -> "derived_poset_upper_lower_dual"
    PosetTopoRespectsEdges -> "derived_poset_topo_respects_edges"
    MatrixIdentity -> "derived_matrix_identity"
    MatrixTransposeInvolution -> "derived_matrix_transpose_involution"
    MatrixRestrictIdempotent -> "derived_matrix_restrict_idempotent"
    MatrixBlockedSparseRepresentationAgreement -> "derived_matrix_blocked_sparse_representation_agreement"
    ComplexDifferentialSquaresZero -> "derived_complex_differential_squares_zero"
    ComplexNormalizationIdempotent -> "derived_complex_normalization_idempotent"
    ComplexMinimizationHypercohomologyInvariant -> "derived_complex_minimization_hypercohomology_invariant"
    ComplexMinimizationMicrosupportInvariant -> "derived_complex_minimization_microsupport_invariant"
    ComplexMinimizationDegreeWindowStable -> "derived_complex_minimization_degree_window_stable"
    ShiftReindexesHypercohomology -> "derived_shift_reindexes_hypercohomology"
    MapSquaresCommute -> "derived_map_squares_commute"
    ConeEulerAdditive -> "derived_cone_euler_additive"
    TriangleRotationInvariant -> "derived_triangle_rotation_invariant"
    QuasiIsoConeAcyclic -> "derived_quasi_iso_cone_acyclic"
    VerdierInvolutionInvariants -> "derived_verdier_involution_invariants"
    RHomTensorAdjunctionDims -> "derived_rhom_tensor_adjunction_dims"
    TruncationTriangleExact -> "derived_truncation_triangle_exact"
    FunctorQuillenARejectsBadFiber -> "derived_functor_quillen_a_rejects_bad_fiber"
    MorseSparseDigestCacheCoherence -> "derived_morse_sparse_digest_cache_coherence"
    DerivedDeterministicFixture -> "derived_deterministic_fixture"
