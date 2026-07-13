import Lean

namespace Moonlight.EGraph

universe u v w

structure RestrictionMapSystem (Context : Type u) (Section : Type v) where
  leq : Context → Context → Prop
  restrictionMap : Context → Context → Option (Section → Section)
  restrictionMap_spec : ∀ source target, Option.isSome (restrictionMap source target) ↔ leq target source

theorem context_restriction_exists_iff_order
    {Context : Type u}
    {Section : Type v}
    (system : RestrictionMapSystem Context Section)
    (source target : Context) :
    Option.isSome (system.restrictionMap source target) ↔ system.leq target source :=
  system.restrictionMap_spec source target

structure GluingDatum (Cover : Type w) (Context : Type u) (Section : Type v) where
  whole : Context
  pieces : Cover → Context
  localSections : Cover → Section

structure SheafSystem (Cover : Type w) (Context : Type u) (Section : Type v)
    extends RestrictionMapSystem Context Section where
  restrict : {source target : Context} → leq target source → Section → Section
  compatible : GluingDatum Cover Context Section → Prop
  glue : (datum : GluingDatum Cover Context Section) → compatible datum → Section
  glueRestricts :
    ∀ (datum : GluingDatum Cover Context Section)
      (compatibility : compatible datum)
      (piece : Cover),
      ∃ restrictionWitness : leq (datum.pieces piece) datum.whole,
        restrict restrictionWitness (glue datum compatibility) = datum.localSections piece
  glueUnique :
    ∀ (datum : GluingDatum Cover Context Section)
      (compatibility : compatible datum)
      (candidate : Section),
      (∀ piece : Cover, ∃ restrictionWitness : leq (datum.pieces piece) datum.whole,
        restrict restrictionWitness candidate = datum.localSections piece) →
      candidate = glue datum compatibility

theorem context_sheaf_gluing_restricts
    {Cover : Type w}
    {Context : Type u}
    {Section : Type v}
    (system : SheafSystem Cover Context Section)
    (datum : GluingDatum Cover Context Section)
    (compatibility : system.compatible datum)
    (piece : Cover) :
    ∃ restrictionWitness : system.leq (datum.pieces piece) datum.whole,
      system.restrict restrictionWitness (system.glue datum compatibility) = datum.localSections piece :=
  system.glueRestricts datum compatibility piece

theorem context_sheaf_gluing_unique
    {Cover : Type w}
    {Context : Type u}
    {Section : Type v}
    (system : SheafSystem Cover Context Section)
    (datum : GluingDatum Cover Context Section)
    (compatibility : system.compatible datum)
    (candidate : Section)
    (candidateRestricts :
      ∀ piece : Cover, ∃ restrictionWitness : system.leq (datum.pieces piece) datum.whole,
        system.restrict restrictionWitness candidate = datum.localSections piece) :
    candidate = system.glue datum compatibility :=
  system.glueUnique datum compatibility candidate candidateRestricts

end Moonlight.EGraph
