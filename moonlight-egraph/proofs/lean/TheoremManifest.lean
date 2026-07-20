import EGraphKernel
import EGraphRefinement
import ContextLattice
import ContextSheaf
import SupportStratification
import TwistScoping
import ObstructionCohomology
import CoverageFromCocycles
import Lean

open Lean

namespace Moonlight.EGraph

def theoremDeclarations : List Lean.Name :=
  [ ``Moonlight.EGraph.congruence_closure,
    ``Moonlight.EGraph.context_global_section,
    ``Moonlight.EGraph.context_restriction_composition,
    ``Moonlight.EGraph.context_restriction_exists_iff_order,
    ``Moonlight.EGraph.context_sheaf_gluing_restricts,
    ``Moonlight.EGraph.context_sheaf_gluing_unique,
    ``Moonlight.EGraph.Refinement.contextual_equivalence_kernel_correct,
    ``Moonlight.EGraph.contextual_extraction_respects_scope,
    ``Moonlight.EGraph.extract_in_class,
    ``Moonlight.EGraph.extract_optimal,
    ``Moonlight.EGraph.find_idempotent,
    ``Moonlight.EGraph.lattice_absorption_join_meet,
    ``Moonlight.EGraph.lattice_absorption_meet_join,
    ``Moonlight.EGraph.lattice_join_associative,
    ``Moonlight.EGraph.lattice_join_commutative,
    ``Moonlight.EGraph.lattice_join_idempotent,
    ``Moonlight.EGraph.lattice_meet_associative,
    ``Moonlight.EGraph.lattice_meet_commutative,
    ``Moonlight.EGraph.lattice_meet_idempotent,
    ``Moonlight.EGraph.merge_monotonicity,
    ``Moonlight.EGraph.obstruction_coboundary_squared_zero,
    ``Moonlight.EGraph.obstruction_report_complete,
    ``Moonlight.EGraph.poset_cech_differential_squared_zero,
    ``Moonlight.EGraph.poset_cohomology_respects_restrictions,
    ``Moonlight.EGraph.positive_first_cohomology_obstructs_gluing,
    ``Moonlight.EGraph.principal_support_contains_generator,
    ``Moonlight.EGraph.proof_context_evidence_sound,
    ``Moonlight.EGraph.proof_soundness,
    ``Moonlight.EGraph.rebuild_restores_congruence,
    ``Moonlight.EGraph.region_path_compression_preserves_equivalence,
    ``Moonlight.EGraph.rewrite_composition_associative_up_to_alpha,
    ``Moonlight.EGraph.rewrite_decoration_scope_preserved,
    ``Moonlight.EGraph.rewrite_identity_left,
    ``Moonlight.EGraph.rewrite_identity_right,
    ``Moonlight.EGraph.rewrite_restriction_commutes_with_composition,
    ``Moonlight.EGraph.scoped_saturation_trace_respects_scope,
    ``Moonlight.EGraph.sheaf_capability_environment_sound,
    ``Moonlight.EGraph.support_aware_proof_evidence_sound,
    ``Moonlight.EGraph.support_family_rewrites_at_exact_support,
    ``Moonlight.EGraph.support_normalization_preserves_semantics,
    ``Moonlight.EGraph.supported_fact_family_rules_at_exact_support,
    ``Moonlight.EGraph.unifier_side_projection_apex
  ]

def theoremIdentifiers : List String :=
  theoremDeclarations.map Lean.Name.getString!

def theoremManifestJson : Json :=
  Json.mkObj [("theorems", toJson theoremIdentifiers)]

def main (_arguments : List String) : IO UInt32 := do
  IO.println (Json.compress theoremManifestJson)
  pure 0

end Moonlight.EGraph

def main (arguments : List String) : IO UInt32 :=
  Moonlight.EGraph.main arguments
