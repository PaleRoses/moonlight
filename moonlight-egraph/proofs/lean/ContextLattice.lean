
import Lean

namespace Moonlight.EGraph

universe u v

structure BoundedLattice (α : Type u) where
  join : α → α → α
  meet : α → α → α
  bottom : α
  top : α
  leq : α → α → Prop
  leq_refl : ∀ a, leq a a
  leq_antisymm : ∀ a b, leq a b → leq b a → a = b
  leq_trans : ∀ a b c, leq a b → leq b c → leq a c
  join_upper_left : ∀ a b, leq a (join a b)
  join_upper_right : ∀ a b, leq b (join a b)
  join_lub : ∀ a b c, leq a c → leq b c → leq (join a b) c
  meet_lower_left : ∀ a b, leq (meet a b) a
  meet_lower_right : ∀ a b, leq (meet a b) b
  meet_glb : ∀ a b c, leq c a → leq c b → leq c (meet a b)
  bottom_least : ∀ a, leq bottom a
  top_greatest : ∀ a, leq a top

theorem lattice_join_commutative {α : Type u} (L : BoundedLattice α) (a b : α) :
    L.join a b = L.join b a :=
  L.leq_antisymm _ _
    (L.join_lub a b (L.join b a) (L.join_upper_right b a) (L.join_upper_left b a))
    (L.join_lub b a (L.join a b) (L.join_upper_right a b) (L.join_upper_left a b))

theorem lattice_meet_commutative {α : Type u} (L : BoundedLattice α) (a b : α) :
    L.meet a b = L.meet b a :=
  L.leq_antisymm _ _
    (L.meet_glb b a (L.meet a b) (L.meet_lower_right a b) (L.meet_lower_left a b))
    (L.meet_glb a b (L.meet b a) (L.meet_lower_right b a) (L.meet_lower_left b a))

theorem lattice_join_idempotent {α : Type u} (L : BoundedLattice α) (a : α) :
    L.join a a = a :=
  L.leq_antisymm _ _
    (L.join_lub a a a (L.leq_refl a) (L.leq_refl a))
    (L.join_upper_left a a)

theorem lattice_meet_idempotent {α : Type u} (L : BoundedLattice α) (a : α) :
    L.meet a a = a :=
  L.leq_antisymm _ _
    (L.meet_lower_left a a)
    (L.meet_glb a a a (L.leq_refl a) (L.leq_refl a))

theorem lattice_absorption_join_meet {α : Type u} (L : BoundedLattice α) (a b : α) :
    L.join a (L.meet a b) = a :=
  L.leq_antisymm _ _
    (L.join_lub a (L.meet a b) a (L.leq_refl a) (L.meet_lower_left a b))
    (L.join_upper_left a (L.meet a b))

theorem lattice_absorption_meet_join {α : Type u} (L : BoundedLattice α) (a b : α) :
    L.meet a (L.join a b) = a :=
  L.leq_antisymm _ _
    (L.meet_lower_left a (L.join a b))
    (L.meet_glb a (L.join a b) a (L.leq_refl a) (L.join_upper_left a b))

theorem lattice_join_associative {α : Type u} (L : BoundedLattice α) (a b c : α) :
    L.join (L.join a b) c = L.join a (L.join b c) := by
  apply L.leq_antisymm
  · apply L.join_lub
    · apply L.join_lub
      · exact L.join_upper_left a (L.join b c)
      · exact L.leq_trans b (L.join b c) (L.join a (L.join b c))
          (L.join_upper_left b c) (L.join_upper_right a (L.join b c))
    · exact L.leq_trans c (L.join b c) (L.join a (L.join b c))
        (L.join_upper_right b c) (L.join_upper_right a (L.join b c))
  · apply L.join_lub
    · exact L.leq_trans a (L.join a b) (L.join (L.join a b) c)
        (L.join_upper_left a b) (L.join_upper_left (L.join a b) c)
    · apply L.join_lub
      · exact L.leq_trans b (L.join a b) (L.join (L.join a b) c)
          (L.join_upper_right a b) (L.join_upper_left (L.join a b) c)
      · exact L.join_upper_right (L.join a b) c

theorem lattice_meet_associative {α : Type u} (L : BoundedLattice α) (a b c : α) :
    L.meet (L.meet a b) c = L.meet a (L.meet b c) := by
  apply L.leq_antisymm
  · apply L.meet_glb
    · exact L.leq_trans (L.meet (L.meet a b) c) (L.meet a b) a
        (L.meet_lower_left (L.meet a b) c) (L.meet_lower_left a b)
    · apply L.meet_glb
      · exact L.leq_trans (L.meet (L.meet a b) c) (L.meet a b) b
          (L.meet_lower_left (L.meet a b) c) (L.meet_lower_right a b)
      · exact L.meet_lower_right (L.meet a b) c
  · apply L.meet_glb
    · apply L.meet_glb
      · exact L.meet_lower_left a (L.meet b c)
      · exact L.leq_trans (L.meet a (L.meet b c)) (L.meet b c) b
          (L.meet_lower_right a (L.meet b c)) (L.meet_lower_left b c)
    · exact L.leq_trans (L.meet a (L.meet b c)) (L.meet b c) c
        (L.meet_lower_right a (L.meet b c)) (L.meet_lower_right b c)

structure MonotoneMerge (Context : Type u) (ClassId : Type v) where
  leq : Context → Context → Prop
  contextEquivalent : Context → ClassId → ClassId → Prop
  monotone : ∀ {finerContext coarserContext : Context} {first second : ClassId},
    leq finerContext coarserContext →
    contextEquivalent finerContext first second →
    contextEquivalent coarserContext first second

theorem merge_monotonicity {Context : Type u} {ClassId : Type v}
    (system : MonotoneMerge Context ClassId)
    {finerContext coarserContext : Context}
    {first second : ClassId} :
    system.leq finerContext coarserContext →
    system.contextEquivalent finerContext first second →
    system.contextEquivalent coarserContext first second :=
  fun orderWitness equivalenceWitness =>
    system.monotone orderWitness equivalenceWitness

end Moonlight.EGraph
