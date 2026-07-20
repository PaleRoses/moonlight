import SupportStratification

namespace Moonlight.EGraph

universe u v w

structure RestrictionEvidence (Context : Type u) where
  source : Context
  target : Context

structure ContextualExtractionWitness (Context : Type u) (Result : Type v) where
  activeContext : Context
  support : SupportBasis Context
  extracted : Result
  supportContainsActiveContext : supportContains support activeContext

theorem contextual_extraction_respects_scope
    {Context : Type u}
    {Result : Type v}
    (witness : ContextualExtractionWitness Context Result) :
    supportContains witness.support witness.activeContext :=
  witness.supportContainsActiveContext

structure ScopedTraceEntry (Context : Type u) (Payload : Type v) where
  activeContext : Context
  support : SupportBasis Context
  payload : Payload

def ScopedTraceEntry.wellScoped
    {Context : Type u}
    {Payload : Type v}
    (entry : ScopedTraceEntry Context Payload) :
    Prop :=
  supportContains entry.support entry.activeContext

structure ScopedSaturationTrace (Context : Type u) (Payload : Type v) where
  entries : List (ScopedTraceEntry Context Payload)
  wellScopedEntries :
    ∀ entry : ScopedTraceEntry Context Payload,
      entry ∈ entries →
      entry.wellScoped

theorem scoped_saturation_trace_respects_scope
    {Context : Type u}
    {Payload : Type v}
    (trace : ScopedSaturationTrace Context Payload) :
    ∀ entry : ScopedTraceEntry Context Payload,
      entry ∈ trace.entries →
      entry.wellScoped :=
  trace.wellScopedEntries

structure ProofContextEvidence (Context : Type u) where
  activeContext : Option Context
  restrictions : List (RestrictionEvidence Context)
  sound :
    ∀ restriction : RestrictionEvidence Context,
      restriction ∈ restrictions →
      ∀ active : Context,
        activeContext = some active →
        restriction.target = active

theorem proof_context_evidence_sound
    {Context : Type u}
    (evidence : ProofContextEvidence Context)
    (restriction : RestrictionEvidence Context)
    (membership : restriction ∈ evidence.restrictions)
    (active : Context)
    (activeWitness : evidence.activeContext = some active) :
    restriction.target = active :=
  evidence.sound restriction membership active activeWitness

structure SupportAwareProofEvidence (Context : Type u) where
  support : SupportBasis Context
  activeContext : Context
  restrictions : List (RestrictionEvidence Context)
  supportSound : supportContains support activeContext
  restrictionSound :
    ∀ restriction : RestrictionEvidence Context,
      restriction ∈ restrictions →
      restriction.target = activeContext

theorem support_aware_proof_evidence_sound
    {Context : Type u}
    (evidence : SupportAwareProofEvidence Context) :
    supportContains evidence.support evidence.activeContext ∧
      ∀ restriction : RestrictionEvidence Context,
        restriction ∈ evidence.restrictions →
        restriction.target = evidence.activeContext :=
  And.intro evidence.supportSound evidence.restrictionSound

structure CapabilitySupport (Capability : Type u) (Anchor : Type v) where
  label : Capability
  anchor : Anchor

structure SheafCapabilityEnvironment (Capability : Type u) (Anchor : Type v) where
  admissible : CapabilitySupport Capability Anchor → Prop
  supports : List (CapabilitySupport Capability Anchor)
  sound :
    ∀ support : CapabilitySupport Capability Anchor,
      support ∈ supports →
      admissible support

theorem sheaf_capability_environment_sound
    {Capability : Type u}
    {Anchor : Type v}
    (environment : SheafCapabilityEnvironment Capability Anchor)
    (support : CapabilitySupport Capability Anchor)
    (membership : support ∈ environment.supports) :
    environment.admissible support :=
  environment.sound support membership

end Moonlight.EGraph
