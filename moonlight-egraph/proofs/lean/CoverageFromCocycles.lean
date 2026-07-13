import ObstructionCohomology

namespace Moonlight.EGraph

universe u v

structure CocycleBasis where
  rank : Nat
  support : Fin rank → List Nat

def CocycleBasis.nontrivial (basis : CocycleBasis) : Prop :=
  basis.rank > 0 ∧ ∃ i : Fin basis.rank, ∃ node, node ∈ basis.support i

structure SeedCoverage (Seed Region : Type u) where
  seeds : List Seed
  seedNode : Seed → Nat
  materialize : Seed → Option Region
  cocycleBasis : CocycleBasis
  seed_covers_support :
    ∀ node,
      (∃ i : Fin cocycleBasis.rank, node ∈ cocycleBasis.support i) →
      ∃ seed ∈ seeds, seedNode seed = node
  cocycle_support_materializes :
    ∀ seed,
      seed ∈ seeds →
      (∃ i : Fin cocycleBasis.rank, seedNode seed ∈ cocycleBasis.support i) →
      materialize seed ≠ none

theorem nontrivial_cocycles_imply_materialization
    {Seed Region : Type u}
    (coverage : SeedCoverage Seed Region)
    (nontrivial : coverage.cocycleBasis.nontrivial) :
    ∃ seed ∈ coverage.seeds, coverage.materialize seed ≠ none := by
  obtain ⟨_, ⟨i, ⟨node, nodeInSupport⟩⟩⟩ := nontrivial
  obtain ⟨seed, seedInSeeds, seedNodeEq⟩ :=
    coverage.seed_covers_support node ⟨i, nodeInSupport⟩
  exact ⟨seed, seedInSeeds,
    coverage.cocycle_support_materializes seed seedInSeeds
      ⟨i, seedNodeEq ▸ nodeInSupport⟩⟩

theorem coverage_contrapositive
    {Seed Region : Type u}
    (coverage : SeedCoverage Seed Region)
    (no_materialization : ∀ seed ∈ coverage.seeds, coverage.materialize seed = none) :
    ¬ coverage.cocycleBasis.nontrivial := by
  intro nontrivial
  obtain ⟨seed, seedInSeeds, materializes⟩ :=
    nontrivial_cocycles_imply_materialization coverage nontrivial
  exact materializes (no_materialization seed seedInSeeds)

end Moonlight.EGraph
