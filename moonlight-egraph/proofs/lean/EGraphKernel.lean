
import Lean

namespace Moonlight.EGraph

universe u v

structure RepresentativeSystem (α : Type u) where
  repr : α → α
  repr_idempotent : ∀ value, repr (repr value) = repr value

theorem find_idempotent {α : Type u} (system : RepresentativeSystem α) (value : α) :
    system.repr (system.repr value) = system.repr value :=
  system.repr_idempotent value

/-!
The regional parent model is the proof-side counterpart of the contextual
equivalence kernel.  A parent lookup is a function at each context, so a class
cannot acquire two parents at one point, and every stored edge strictly lowers
the numeric class key.  Regions are represented extensionally by membership
at a context; no tree-shaped version assumption is smuggled into the model.
-/

abbrev ContextRegion (Context : Type u) := Context → Bool

structure RegionalParentKernel (Context : Type u) where
  parent : Context → Nat → Option Nat
  parent_descends :
    ∀ {context child parentClass},
      parent context child = some parentClass → parentClass < child

inductive EquivalenceClosure {α : Type u} (edge : α → α → Prop) : α → α → Prop where
  | refl (value : α) : EquivalenceClosure edge value value
  | edge {left right : α} : edge left right → EquivalenceClosure edge left right
  | symm {left right : α} :
      EquivalenceClosure edge left right → EquivalenceClosure edge right left
  | trans {left middle right : α} :
      EquivalenceClosure edge left middle →
      EquivalenceClosure edge middle right →
      EquivalenceClosure edge left right

theorem EquivalenceClosure.map {α : Type u} {sourceEdge targetEdge : α → α → Prop}
    (mapEdge : ∀ {left right}, sourceEdge left right → EquivalenceClosure targetEdge left right)
    {left right : α} :
    EquivalenceClosure sourceEdge left right → EquivalenceClosure targetEdge left right := by
  intro sourceWitness
  induction sourceWitness with
  | refl value => exact .refl value
  | edge edgeWitness => exact mapEdge edgeWitness
  | symm _ mappedWitness => exact .symm mappedWitness
  | trans _ _ mappedLeft mappedRight => exact .trans mappedLeft mappedRight

inductive ParentPath {Context : Type u}
    (parent : Context → Nat → Option Nat) (context : Context) : Nat → Nat → Prop where
  | refl (value : Nat) : ParentPath parent context value value
  | step {child next root : Nat} :
      parent context child = some next →
      ParentPath parent context next root →
      ParentPath parent context child root

theorem ParentPath.nontrivialStep {Context : Type u}
    {parent : Context → Nat → Option Nat} {context : Context} {child root : Nat}
    (rootBelowChild : root < child)
    (pathWitness : ParentPath parent context child root) :
    ∃ next,
      parent context child = some next ∧
        ParentPath parent context next root := by
  cases pathWitness with
  | refl => exact (Nat.lt_irrefl child rootBelowChild).elim
  | step edgeWitness tailPath => exact ⟨_, edgeWitness, tailPath⟩

def ParentEquivalent {Context : Type u}
    (parent : Context → Nat → Option Nat) (context : Context) : Nat → Nat → Prop :=
  EquivalenceClosure (fun child parentClass => parent context child = some parentClass)

theorem ParentPath.toEquivalence {Context : Type u}
    {parent : Context → Nat → Option Nat} {context : Context} {child root : Nat} :
    ParentPath parent context child root → ParentEquivalent parent context child root := by
  intro pathWitness
  induction pathWitness with
  | refl value => exact .refl value
  | step edgeWitness _ tailEquivalent =>
      exact .trans (.edge edgeWitness) tailEquivalent

def compressParent {Context : Type u}
    (parent : Context → Nat → Option Nat)
    (region : ContextRegion Context)
    (compressedClass root : Nat) :
    Context → Nat → Option Nat :=
  fun context value =>
    if region context then
      if value = compressedClass then some root else parent context value
    else
      parent context value

theorem compressed_parent_descends {Context : Type u}
    (kernel : RegionalParentKernel Context)
    (region : ContextRegion Context)
    (compressedClass root : Nat)
    (rootDescends : root < compressedClass)
    {context : Context} {child parentClass : Nat} :
    compressParent kernel.parent region compressedClass root context child = some parentClass →
      parentClass < child := by
  intro compressedEdge
  by_cases contextCovered : region context = true
  · by_cases isCompressedClass : child = compressedClass
    · subst child
      have rootIsParent : root = parentClass := by
        simpa [compressParent, contextCovered] using compressedEdge
      subst parentClass
      exact rootDescends
    · have originalEdge : kernel.parent context child = some parentClass := by
        simpa [compressParent, contextCovered, isCompressedClass] using compressedEdge
      exact kernel.parent_descends originalEdge
  · have contextResidual : region context = false := by
      cases regionAtContext : region context <;> simp_all
    have originalEdge : kernel.parent context child = some parentClass := by
      simpa [compressParent, contextResidual] using compressedEdge
    exact kernel.parent_descends originalEdge

theorem compression_preserves_path_below {Context : Type u}
    (kernel : RegionalParentKernel Context)
    (region : ContextRegion Context)
    (compressedClass root : Nat)
    {context : Context} {start finish : Nat}
    (startBelowCompressedClass : start < compressedClass)
    (pathWitness : ParentPath kernel.parent context start finish) :
    ParentPath
      (compressParent kernel.parent region compressedClass root)
      context
      start
      finish := by
  induction pathWitness with
  | refl value => exact .refl value
  | @step child next finish edgeWitness _ translatedTail =>
      have childIsNotCompressedClass : child ≠ compressedClass :=
        Nat.ne_of_lt startBelowCompressedClass
      have translatedEdge :
          compressParent kernel.parent region compressedClass root context child = some next := by
        simp [compressParent, childIsNotCompressedClass, edgeWitness]
      have nextBelowCompressedClass : next < compressedClass :=
        Nat.lt_trans (kernel.parent_descends edgeWitness) startBelowCompressedClass
      exact .step translatedEdge (translatedTail nextBelowCompressedClass)

theorem compressed_edge_refines_original {Context : Type u}
    (kernel : RegionalParentKernel Context)
    (region : ContextRegion Context)
    (compressedClass root : Nat)
    (coveredPath :
      ∀ context, region context = true →
        ParentPath kernel.parent context compressedClass root)
    {context : Context} {child parentClass : Nat} :
    compressParent kernel.parent region compressedClass root context child = some parentClass →
      ParentEquivalent kernel.parent context child parentClass := by
  intro compressedEdge
  by_cases contextCovered : region context = true
  · by_cases isCompressedClass : child = compressedClass
    · subst child
      have rootIsParent : root = parentClass := by
        simpa [compressParent, contextCovered] using compressedEdge
      subst parentClass
      exact (coveredPath context contextCovered).toEquivalence
    · have originalEdge : kernel.parent context child = some parentClass := by
        simpa [compressParent, contextCovered, isCompressedClass] using compressedEdge
      exact .edge originalEdge
  · have contextResidual : region context = false := by
      cases regionAtContext : region context <;> simp_all
    have originalEdge : kernel.parent context child = some parentClass := by
      simpa [compressParent, contextResidual] using compressedEdge
    exact .edge originalEdge

theorem original_edge_refines_compressed {Context : Type u}
    (kernel : RegionalParentKernel Context)
    (region : ContextRegion Context)
    (compressedClass root : Nat)
    (rootDescends : root < compressedClass)
    (coveredPath :
      ∀ context, region context = true →
        ParentPath kernel.parent context compressedClass root)
    {context : Context} {child parentClass : Nat} :
    kernel.parent context child = some parentClass →
      ParentEquivalent
        (compressParent kernel.parent region compressedClass root)
        context
        child
        parentClass := by
  intro originalEdge
  by_cases contextCovered : region context = true
  · by_cases isCompressedClass : child = compressedClass
    · subst child
      have parentBelowCompressedClass : parentClass < compressedClass :=
        kernel.parent_descends originalEdge
      obtain ⟨next, firstEdge, tailPath⟩ :=
        (coveredPath context contextCovered).nontrivialStep rootDescends
      have nextIsParent : next = parentClass := by
        rw [originalEdge] at firstEdge
        exact Option.some.inj firstEdge.symm
      subst next
      have translatedTail :=
        compression_preserves_path_below
          kernel
          region
          compressedClass
          root
          parentBelowCompressedClass
          tailPath
      have compressedClassToRoot :
          ParentEquivalent
            (compressParent kernel.parent region compressedClass root)
            context
            compressedClass
            root :=
        .edge (by simp [compressParent, contextCovered])
      exact .trans compressedClassToRoot (.symm translatedTail.toEquivalence)
    · have translatedEdge :
          compressParent kernel.parent region compressedClass root context child =
            some parentClass := by
        simp [compressParent, contextCovered, isCompressedClass, originalEdge]
      exact .edge translatedEdge
  · have contextResidual : region context = false := by
      cases regionAtContext : region context <;> simp_all
    have translatedEdge :
        compressParent kernel.parent region compressedClass root context child =
          some parentClass := by
      simp [compressParent, contextResidual, originalEdge]
    exact .edge translatedEdge

theorem region_path_compression_preserves_equivalence {Context : Type u}
    (kernel : RegionalParentKernel Context)
    (region : ContextRegion Context)
    (compressedClass root : Nat)
    (rootDescends : root < compressedClass)
    (coveredPath :
      ∀ context, region context = true →
        ParentPath kernel.parent context compressedClass root) :
    (∀ {context child parentClass},
        compressParent kernel.parent region compressedClass root context child = some parentClass →
          parentClass < child) ∧
      (∀ context left right,
        ParentEquivalent
            (compressParent kernel.parent region compressedClass root)
            context
            left
            right ↔
          ParentEquivalent kernel.parent context left right) ∧
      (∀ context, region context = false → ∀ value,
        compressParent kernel.parent region compressedClass root context value =
          kernel.parent context value) := by
  refine ⟨?_, ?_, ?_⟩
  · exact compressed_parent_descends kernel region compressedClass root rootDescends
  · intro context left right
    constructor
    · exact EquivalenceClosure.map
        (compressed_edge_refines_original kernel region compressedClass root coveredPath)
    · exact EquivalenceClosure.map
        (original_edge_refines_compressed
          kernel
          region
          compressedClass
          root
          rootDescends
          coveredPath)
  · intro context contextResidual value
    simp [compressParent, contextResidual]

structure UnaryCongruence (α : Type u) (β : Type v) where
  equivalent : α → α → Prop
  embed : α → β
  respects : ∀ {left right}, equivalent left right → embed left = embed right

theorem congruence_closure {α : Type u} {β : Type v} (system : UnaryCongruence α β)
    {left right : α} :
    system.equivalent left right → system.embed left = system.embed right := by
  intro equivalentWitness
  exact system.respects equivalentWitness

def EClass (α : Type u) := α → Prop

structure ExtractionWitness (α : Type u) (eclass : EClass α) where
  term : α
  in_class : eclass term

theorem extract_in_class {α : Type u} {eclass : EClass α}
    (witness : ExtractionWitness α eclass) :
    eclass witness.term :=
  witness.in_class

structure RestrictionSystem (Context : Type u) (Section : Type v) where
  Morphism : Context → Context → Type v
  identityMorphism : (context : Context) → Morphism context context
  compose :
    {sourceContext midContext targetContext : Context} →
      Morphism sourceContext midContext →
      Morphism midContext targetContext →
      Morphism sourceContext targetContext
  restrict :
    {sourceContext targetContext : Context} →
      Morphism sourceContext targetContext →
      Section →
      Section
  identity :
    ∀ {context} sectionValue,
      restrict (identityMorphism context) sectionValue = sectionValue
  composes :
    ∀ {sourceContext midContext targetContext : Context}
      (sourceMorphism : Morphism sourceContext midContext)
      (targetMorphism : Morphism midContext targetContext)
      (sectionValue : Section),
      restrict targetMorphism (restrict sourceMorphism sectionValue) =
        restrict (compose sourceMorphism targetMorphism) sectionValue
  compose_left_identity :
    ∀ {sourceContext targetContext : Context}
      (targetMorphism : Morphism sourceContext targetContext),
      compose (identityMorphism sourceContext) targetMorphism = targetMorphism
  compose_right_identity :
    ∀ {sourceContext targetContext : Context}
      (sourceMorphism : Morphism sourceContext targetContext),
      compose sourceMorphism (identityMorphism targetContext) = sourceMorphism
  compose_associative :
    ∀ {sourceContext firstMidContext secondMidContext targetContext : Context}
      (firstMorphism : Morphism sourceContext firstMidContext)
      (secondMorphism : Morphism firstMidContext secondMidContext)
      (thirdMorphism : Morphism secondMidContext targetContext),
      compose (compose firstMorphism secondMorphism) thirdMorphism =
        compose firstMorphism (compose secondMorphism thirdMorphism)

theorem context_restriction_identity {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    (context : Context) (sectionValue : Section) :
    system.restrict (system.identityMorphism context) sectionValue = sectionValue :=
  system.identity (context := context) sectionValue

theorem context_restriction_composition {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {sourceContext midContext targetContext : Context}
    (sourceMorphism : system.Morphism sourceContext midContext)
    (targetMorphism : system.Morphism midContext targetContext)
    (sectionValue : Section) :
    system.restrict targetMorphism (system.restrict sourceMorphism sectionValue) =
      system.restrict (system.compose sourceMorphism targetMorphism) sectionValue :=
  system.composes sourceMorphism targetMorphism sectionValue

theorem context_morphism_left_identity {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {sourceContext targetContext : Context}
    (targetMorphism : system.Morphism sourceContext targetContext) :
    system.compose (system.identityMorphism sourceContext) targetMorphism = targetMorphism :=
  system.compose_left_identity targetMorphism

theorem context_morphism_right_identity {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {sourceContext targetContext : Context}
    (sourceMorphism : system.Morphism sourceContext targetContext) :
    system.compose sourceMorphism (system.identityMorphism targetContext) = sourceMorphism :=
  system.compose_right_identity sourceMorphism

theorem context_morphism_associative {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {sourceContext firstMidContext secondMidContext targetContext : Context}
    (firstMorphism : system.Morphism sourceContext firstMidContext)
    (secondMorphism : system.Morphism firstMidContext secondMidContext)
    (thirdMorphism : system.Morphism secondMidContext targetContext) :
    system.compose (system.compose firstMorphism secondMorphism) thirdMorphism =
      system.compose firstMorphism (system.compose secondMorphism thirdMorphism) :=
  system.compose_associative firstMorphism secondMorphism thirdMorphism

theorem context_restriction_functorial_identity {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    (context : Context) (sectionValue : Section) :
    system.restrict (system.identityMorphism context) sectionValue = sectionValue :=
  context_restriction_identity system context sectionValue

theorem context_restriction_functorial_action {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {sourceContext firstMidContext secondMidContext targetContext : Context}
    (firstMorphism : system.Morphism sourceContext firstMidContext)
    (secondMorphism : system.Morphism firstMidContext secondMidContext)
    (thirdMorphism : system.Morphism secondMidContext targetContext)
    (sectionValue : Section) :
    system.restrict thirdMorphism
        (system.restrict secondMorphism (system.restrict firstMorphism sectionValue)) =
      system.restrict
        (system.compose firstMorphism (system.compose secondMorphism thirdMorphism))
        sectionValue := by
  calc
    system.restrict thirdMorphism
        (system.restrict secondMorphism (system.restrict firstMorphism sectionValue))
      = system.restrict thirdMorphism
          (system.restrict (system.compose firstMorphism secondMorphism) sectionValue) := by
            rw [← context_restriction_composition system firstMorphism secondMorphism sectionValue]
    _ = system.restrict
          (system.compose (system.compose firstMorphism secondMorphism) thirdMorphism)
          sectionValue := by
            exact
              context_restriction_composition
                system
                (system.compose firstMorphism secondMorphism)
                thirdMorphism
                sectionValue
    _ = system.restrict
          (system.compose firstMorphism (system.compose secondMorphism thirdMorphism))
          sectionValue := by
            rw [context_morphism_associative system firstMorphism secondMorphism thirdMorphism]

def IsGlobalSection {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section) (rootContext : Context) (sectionValue : Section) : Prop :=
  ∀ ⦃targetContext : Context⦄,
    (reachWitness : system.Morphism rootContext targetContext) →
      system.restrict reachWitness sectionValue = sectionValue

theorem context_global_section {Context : Type u} {Section : Type v}
    (system : RestrictionSystem Context Section)
    {rootContext reachableContext : Context}
    (reachWitness : system.Morphism rootContext reachableContext)
    (sectionValue : Section) :
    IsGlobalSection system rootContext sectionValue →
    system.restrict reachWitness sectionValue = sectionValue ∧
      IsGlobalSection system reachableContext sectionValue := by
  intro globalWitness
  constructor
  · exact globalWitness reachWitness
  · intro targetContext onwardWitness
    have reachInvariant :
        system.restrict reachWitness sectionValue = sectionValue :=
      globalWitness reachWitness
    calc
      system.restrict onwardWitness sectionValue
        = system.restrict onwardWitness
            (system.restrict reachWitness sectionValue) := by
              rw [reachInvariant]
      _ = system.restrict (system.compose reachWitness onwardWitness) sectionValue := by
            exact context_restriction_composition system reachWitness onwardWitness sectionValue
      _ = sectionValue := globalWitness (system.compose reachWitness onwardWitness)

structure OptimalExtractionSystem (Term : Type u) (Cost : Type v) where
  cost : Term → Cost
  cheaperThan : Cost → Cost → Prop
  extract : Term
  noCheaper :
    ∀ candidate, ¬ cheaperThan (cost candidate) (cost extract)

theorem extract_optimal {Term : Type u} {Cost : Type v}
    (system : OptimalExtractionSystem Term Cost)
    (candidate : Term) :
    ¬ system.cheaperThan (system.cost candidate) (system.cost system.extract) :=
  system.noCheaper candidate

structure BinaryRebuildSystem (α : Type u) (β : Type v) where
  equivalent : α → α → Prop
  canonicalize : α → β
  parent : β → β → β
  respects : ∀ {left right}, equivalent left right → canonicalize left = canonicalize right

theorem rebuild_restores_congruence {α : Type u} {β : Type v}
    (system : BinaryRebuildSystem α β)
    {leftParentChild leftCanonicalChild rightParentChild rightCanonicalChild : α} :
    system.equivalent leftParentChild leftCanonicalChild →
    system.equivalent rightParentChild rightCanonicalChild →
    system.parent (system.canonicalize leftParentChild) (system.canonicalize rightParentChild) =
      system.parent (system.canonicalize leftCanonicalChild) (system.canonicalize rightCanonicalChild) := by
  intro leftEquivalent rightEquivalent
  rw [system.respects leftEquivalent, system.respects rightEquivalent]

structure ProofSystem (Certificate : Type u) (Judgment : Type v) where
  proves : Certificate → Judgment → Prop
  soundModel : Judgment → Prop
  sound :
    ∀ {certificate judgment},
      proves certificate judgment → soundModel judgment

theorem proof_soundness {Certificate : Type u} {Judgment : Type v}
    (system : ProofSystem Certificate Judgment)
    {certificate : Certificate} {judgment : Judgment} :
    system.proves certificate judgment → system.soundModel judgment := by
  intro proofWitness
  exact system.sound proofWitness

end Moonlight.EGraph
