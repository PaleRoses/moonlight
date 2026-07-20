import ContextSheaf

namespace Moonlight.EGraph

universe u v w x

structure ObstructionReporter
    (Context : Type u)
    (ClassId : Type v)
    (Obstruction : Type w) where
  equivalent : Context → ClassId → ClassId → Prop
  report : Context → ClassId → ClassId → List Obstruction
  complete :
    ∀ context left right,
      report context left right = [] ↔ equivalent context left right

theorem obstruction_report_complete
    {Context : Type u}
    {ClassId : Type v}
    {Obstruction : Type w}
    (reporter : ObstructionReporter Context ClassId Obstruction)
    (context : Context)
    (left right : ClassId) :
    reporter.report context left right = [] ↔ reporter.equivalent context left right :=
  reporter.complete context left right

structure CochainComplex2
    (ZeroCochain : Type u)
    (OneCochain : Type v)
    (TwoCochain : Type w) where
  d₀ : ZeroCochain → OneCochain
  d₁ : OneCochain → TwoCochain
  zeroTwo : TwoCochain
  nilpotent : ∀ value, d₁ (d₀ value) = zeroTwo

theorem obstruction_coboundary_squared_zero
    {ZeroCochain : Type u}
    {OneCochain : Type v}
    {TwoCochain : Type w}
    (complex : CochainComplex2 ZeroCochain OneCochain TwoCochain)
    (value : ZeroCochain) :
    complex.d₁ (complex.d₀ value) = complex.zeroTwo :=
  complex.nilpotent value

structure CohomologicalGluingProblem (Witness : Type u) where
  hasGlobalSection : Prop
  firstCohomologyRank : Nat
  witness : firstCohomologyRank > 0 → Witness
  obstructed : ∀ _positiveRank : firstCohomologyRank > 0, ¬ hasGlobalSection

theorem positive_first_cohomology_obstructs_gluing
    {Witness : Type u}
    (problem : CohomologicalGluingProblem Witness)
    (positiveRank : problem.firstCohomologyRank > 0) :
    ¬ problem.hasGlobalSection :=
  problem.obstructed positiveRank

theorem gluing_implies_vanishing_first_cohomology
    {Witness : Type u}
    (problem : CohomologicalGluingProblem Witness)
    (globalSection : problem.hasGlobalSection) :
    problem.firstCohomologyRank = 0 :=
  Nat.eq_zero_of_not_pos
    (fun positiveRank => problem.obstructed positiveRank globalSection)

structure PosetCechSystem
    (ZeroCochain : Type u)
    (OneCochain : Type v)
    (TwoCochain : Type w)
    extends CochainComplex2 ZeroCochain OneCochain TwoCochain

theorem poset_cech_differential_squared_zero
    {ZeroCochain : Type u}
    {OneCochain : Type v}
    {TwoCochain : Type w}
    (system : PosetCechSystem ZeroCochain OneCochain TwoCochain)
    (value : ZeroCochain) :
    system.d₁ (system.d₀ value) = system.zeroTwo :=
  system.nilpotent value

structure PosetSheafCohomologySystem (Node : Type u) (Cohomology : Type v) where
  leq : Node → Node → Prop
  restrict : {source target : Node} → leq target source → Cohomology → Cohomology
  cohomology : Node → Cohomology
  restriction_respects :
    ∀ {source target : Node} (restrictionWitness : leq target source),
      restrict restrictionWitness (cohomology source) = cohomology target

theorem poset_cohomology_respects_restrictions
    {Node : Type u}
    {Cohomology : Type v}
    (system : PosetSheafCohomologySystem Node Cohomology)
    {source target : Node}
    (restrictionWitness : system.leq target source) :
    system.restrict restrictionWitness (system.cohomology source) = system.cohomology target :=
  system.restriction_respects restrictionWitness

end Moonlight.EGraph
