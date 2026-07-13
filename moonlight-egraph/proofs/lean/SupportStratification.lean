import Lean

namespace Moonlight.EGraph

universe u v

structure SupportBasis (Context : Type u) where
  contains : Context → Bool

def supportContains {Context : Type u} (support : SupportBasis Context) (context : Context) : Prop :=
  support.contains context = true

def emptySupport {Context : Type u} : SupportBasis Context :=
  { contains := fun _ => false }

def principalSupport {Context : Type u} [DecidableEq Context] (anchor : Context) : SupportBasis Context :=
  { contains := fun context => decide (context = anchor) }

def supportUnion {Context : Type u}
    (leftSupport rightSupport : SupportBasis Context) :
    SupportBasis Context :=
  { contains := fun context => leftSupport.contains context || rightSupport.contains context }

def supportMeet {Context : Type u}
    (leftSupport rightSupport : SupportBasis Context) :
    SupportBasis Context :=
  { contains := fun context => leftSupport.contains context && rightSupport.contains context }

theorem principal_support_contains_generator
    {Context : Type u}
    [DecidableEq Context]
    (anchor : Context) :
    supportContains (principalSupport anchor) anchor := by
  simp [supportContains, principalSupport]

structure SupportNormalizer (Context : Type u) where
  normalize : SupportBasis Context → SupportBasis Context
  preserves : ∀ support context,
    supportContains (normalize support) context ↔ supportContains support context

theorem support_normalization_preserves_semantics
    {Context : Type u}
    (normalizer : SupportNormalizer Context)
    (support : SupportBasis Context)
    (context : Context) :
    supportContains (normalizer.normalize support) context ↔ supportContains support context :=
  normalizer.preserves support context

structure SupportedRewrite (Context : Type u) (Rewrite : Type v) where
  support : SupportBasis Context
  rewrite : Rewrite

def supportFamilyRewritesAt {Context : Type u} {Rewrite : Type v}
    (context : Context)
    (entries : List (SupportedRewrite Context Rewrite)) :
    List (SupportedRewrite Context Rewrite) :=
  entries.filter fun entry => entry.support.contains context

theorem support_family_rewrites_at_exact_support
    {Context : Type u}
    {Rewrite : Type v}
    (context : Context)
    (entries : List (SupportedRewrite Context Rewrite))
    (entry : SupportedRewrite Context Rewrite) :
    entry ∈ supportFamilyRewritesAt context entries ↔
      entry ∈ entries ∧ supportContains entry.support context := by
  simp [supportFamilyRewritesAt, supportContains]

structure RewriteAlgebra (Rewrite : Type u) where
  compose : Rewrite → Rewrite → Rewrite
  identityAtSource : Rewrite → Rewrite
  identityAtTarget : Rewrite → Rewrite
  restrict : Rewrite → Rewrite
  equivalentUpToAlpha : Rewrite → Rewrite → Prop
  decorationScoped : Rewrite → Prop
  associativity : ∀ left middle right,
    equivalentUpToAlpha
      (compose (compose left middle) right)
      (compose left (compose middle right))
  leftIdentity : ∀ rewrite,
    equivalentUpToAlpha (compose (identityAtSource rewrite) rewrite) rewrite
  rightIdentity : ∀ rewrite,
    equivalentUpToAlpha (compose rewrite (identityAtTarget rewrite)) rewrite
  decorationScope : ∀ left right,
    decorationScoped (compose left right)
  restrictionComposition : ∀ left right,
    equivalentUpToAlpha
      (restrict (compose left right))
      (compose (restrict left) (restrict right))

theorem rewrite_composition_associative_up_to_alpha
    {Rewrite : Type u}
    (algebra : RewriteAlgebra Rewrite)
    (left middle right : Rewrite) :
    algebra.equivalentUpToAlpha
      (algebra.compose (algebra.compose left middle) right)
      (algebra.compose left (algebra.compose middle right)) :=
  algebra.associativity left middle right

theorem rewrite_identity_left
    {Rewrite : Type u}
    (algebra : RewriteAlgebra Rewrite)
    (rewrite : Rewrite) :
    algebra.equivalentUpToAlpha
      (algebra.compose (algebra.identityAtSource rewrite) rewrite)
      rewrite :=
  algebra.leftIdentity rewrite

theorem rewrite_identity_right
    {Rewrite : Type u}
    (algebra : RewriteAlgebra Rewrite)
    (rewrite : Rewrite) :
    algebra.equivalentUpToAlpha
      (algebra.compose rewrite (algebra.identityAtTarget rewrite))
      rewrite :=
  algebra.rightIdentity rewrite

theorem rewrite_decoration_scope_preserved
    {Rewrite : Type u}
    (algebra : RewriteAlgebra Rewrite)
    (left right : Rewrite) :
    algebra.decorationScoped (algebra.compose left right) :=
  algebra.decorationScope left right

theorem rewrite_restriction_commutes_with_composition
    {Rewrite : Type u}
    (algebra : RewriteAlgebra Rewrite)
    (left right : Rewrite) :
    algebra.equivalentUpToAlpha
      (algebra.restrict (algebra.compose left right))
      (algebra.compose (algebra.restrict left) (algebra.restrict right)) :=
  algebra.restrictionComposition left right

structure RewriteUnifier (Pattern : Type u) where
  leftPattern : Pattern
  rightPattern : Pattern
  unifiedPattern : Pattern
  applyLeft : Pattern → Pattern
  applyRight : Pattern → Pattern
  leftProjection : applyLeft leftPattern = unifiedPattern
  rightProjection : applyRight rightPattern = unifiedPattern

theorem unifier_side_projection_apex
    {Pattern : Type u}
    (unifier : RewriteUnifier Pattern) :
    unifier.applyLeft unifier.leftPattern = unifier.unifiedPattern ∧
      unifier.applyRight unifier.rightPattern = unifier.unifiedPattern := by
  exact And.intro unifier.leftProjection unifier.rightProjection

structure SupportedFactRule (Context : Type u) (Fact : Type v) where
  support : SupportBasis Context
  fact : Fact

def supportedFactFamilyRulesAt {Context : Type u} {Fact : Type v}
    (context : Context)
    (entries : List (SupportedFactRule Context Fact)) :
    List (SupportedFactRule Context Fact) :=
  entries.filter fun entry => entry.support.contains context

theorem supported_fact_family_rules_at_exact_support
    {Context : Type u}
    {Fact : Type v}
    (context : Context)
    (entries : List (SupportedFactRule Context Fact))
    (entry : SupportedFactRule Context Fact) :
    entry ∈ supportedFactFamilyRulesAt context entries ↔
      entry ∈ entries ∧ supportContains entry.support context := by
  simp [supportedFactFamilyRulesAt, supportContains]

end Moonlight.EGraph
