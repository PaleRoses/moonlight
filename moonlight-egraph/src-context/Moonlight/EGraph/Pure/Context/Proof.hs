module Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (..),
    ProofEGraph,
    ContextProofQueryError (..),
    emptyProofGraph,
    emptyProofGraphWithRetention,
    emptyProofEGraph,
    emptyProofEGraphWithRetention,
    contextualProofEvidence,
    supportAwareProofEvidence,
    proofClassWitnesses,
    recordAnnotatedProofStep,
    recordProofStepWith,
    proofBetween,
    proofGraph,
    proofReachability,
    proofClassesReachableFrom,
    proofRelated,
    proofBaseGraph,
    serializeProofLog,
    summarizeProofLog,
    proofAtContext,
  )
where

import Data.Bifunctor (first)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Core (Language, classIdKey)
import Moonlight.Sheaf.Context.Algebra (ContextClassLookupFailure, contextEquivalentAt)
import Moonlight.EGraph.Pure.Context.Core
  ( ContextEGraph,
    cegBase,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    canonicalizeClassId,
  )
import Data.Fix (Fix)
import Moonlight.Rewrite.ProofContext
  ( ProofAnnotationBuilder,
    ProofAnnotationInput (..),
    ProofCompressionSummary,
    ProofContextEvidence (..),
    ProofContextRestriction (..),
    ProofReachability,
    ProofQueryError,
    ProofRetention,
    ProofStep,
    ProofStepInput (..),
    SupportAwareProofEvidence (..),
  )
import Moonlight.Rewrite.ProofContext qualified as Rewrite
import Moonlight.FiniteLattice (SupportBasis)

type ProofGraph :: Type -> (Type -> Type) -> Type -> Type -> Type
data ProofGraph graph f c p = ProofGraph
  { pgGraph :: !graph,
    pgProofRegistry :: !(Rewrite.ProofRegistry f c p)
  }

type ProofEGraph :: (Type -> Type) -> Type -> Type -> Type -> Type
type ProofEGraph f a c p = ProofGraph (ContextEGraph f a c) f c p

type ContextProofQueryError :: Type -> Type
data ContextProofQueryError c
  = ContextProofRegistry !ProofQueryError
  | ContextProofClassLookup !(ContextClassLookupFailure c ClassId)
  deriving stock (Eq, Ord, Show)

emptyProofGraph :: graph -> ProofGraph graph f c p
emptyProofGraph graph =
  ProofGraph
    { pgGraph = graph,
      pgProofRegistry = Rewrite.emptyProofRegistry
    }

emptyProofGraphWithRetention :: ProofRetention -> graph -> ProofGraph graph f c p
emptyProofGraphWithRetention retention graph =
  ProofGraph
    { pgGraph = graph,
      pgProofRegistry = Rewrite.emptyProofRegistryWithRetention retention
    }

emptyProofEGraph :: ContextEGraph f a c -> ProofEGraph f a c p
emptyProofEGraph =
  emptyProofGraph

emptyProofEGraphWithRetention :: ProofRetention -> ContextEGraph f a c -> ProofEGraph f a c p
emptyProofEGraphWithRetention =
  emptyProofGraphWithRetention

contextualProofEvidence :: Maybe c -> [(c, c)] -> ProofContextEvidence c
contextualProofEvidence activeContext restrictionPairs =
  ProofContextEvidence
    { pceActiveContext = activeContext,
      pceRestrictions =
        fmap
          (uncurry ProofContextRestriction)
          restrictionPairs
    }

supportAwareProofEvidence :: SupportBasis c -> [(c, c)] -> SupportAwareProofEvidence c
supportAwareProofEvidence supportValue restrictionPairs =
  SupportAwareProofEvidence
    { sapeSupport = supportValue,
      sapeRestrictions =
        fmap
          (uncurry ProofContextRestriction)
          restrictionPairs
    }

recordProofStepWith :: Ord c => (ClassId -> ClassId) -> ProofStepInput f c p -> ProofGraph graph f c p -> ProofGraph graph f c p
recordProofStepWith canonicalize proofStepInput proofGraphValue =
  proofGraphValue
    { pgProofRegistry =
        Rewrite.recordProofStepWith
          (canonicalizeProofStepInput canonicalize proofStepInput)
          (pgProofRegistry proofGraphValue)
    }

recordAnnotatedProofStep :: Ord c => (ClassId -> ClassId) -> ProofAnnotationBuilder c p -> ProofAnnotationInput c -> Maybe (Fix f) -> Maybe (Fix f) -> ProofGraph graph f c p -> ProofGraph graph f c p
recordAnnotatedProofStep canonicalize proofAnnotationBuilder annotationInput lhsWitness rhsWitness proofGraphValue =
  proofGraphValue
    { pgProofRegistry =
        Rewrite.recordAnnotatedProofStep
          proofAnnotationBuilder
          (canonicalizeAnnotationInput canonicalize annotationInput)
          lhsWitness
          rhsWitness
          (pgProofRegistry proofGraphValue)
    }

proofBetween :: ClassId -> ClassId -> ProofEGraph f a c p -> Either ProofQueryError (ProofStep f c p)
proofBetween leftClassId rightClassId proofEGraph =
  Rewrite.proofBetween
    (canonicalizeProofClass proofEGraph leftClassId)
    (canonicalizeProofClass proofEGraph rightClassId)
    (pgProofRegistry proofEGraph)

proofGraph :: ProofGraph graph f c p -> Either ProofQueryError (Rewrite.ProofGraph f)
proofGraph proofGraphValue =
  Rewrite.proofGraph (pgProofRegistry proofGraphValue)

proofReachability :: ProofGraph graph f c p -> Either ProofQueryError ProofReachability
proofReachability proofGraphValue =
  Rewrite.proofReachability (pgProofRegistry proofGraphValue)

proofClassesReachableFrom :: ClassId -> ProofReachability -> IntSet
proofClassesReachableFrom =
  Rewrite.proofClassesReachableFrom

proofRelated :: ClassId -> ClassId -> ProofReachability -> Bool
proofRelated leftClassId rightClassId reachability =
  IntSet.member
    (classIdKey rightClassId)
    (proofClassesReachableFrom leftClassId reachability)

serializeProofLog :: ProofGraph graph f c p -> [ProofStep f c p]
serializeProofLog =
  Rewrite.serializeProofLog . pgProofRegistry

summarizeProofLog :: ProofGraph graph f c p -> ProofCompressionSummary
summarizeProofLog proofGraphValue =
  Rewrite.summarizeProofLog (pgProofRegistry proofGraphValue)

proofAtContext ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ClassId ->
  ProofEGraph f a c p ->
  Either (ContextProofQueryError c) (Maybe (ProofStep f c p))
proofAtContext context leftClassId rightClassId proofEGraph =
  first
    ContextProofClassLookup
    (contextEquivalentAt context leftClassId rightClassId (pgGraph proofEGraph))
    >>= \equivalent ->
      if equivalent
        then
          Just
            <$> first ContextProofRegistry (proofBetween leftClassId rightClassId proofEGraph)
        else
          Right Nothing

proofClassWitnesses :: (graph -> EGraph f a) -> ClassId -> ProofGraph graph f c p -> Either ProofQueryError [Fix f]
proofClassWitnesses projectBaseGraph classId proofEGraph =
  Rewrite.proofClassWitnesses
    (canonicalizeClassId (projectBaseGraph (pgGraph proofEGraph)) classId)
    (pgProofRegistry proofEGraph)

canonicalizeProofClass :: ProofEGraph f a c p -> ClassId -> ClassId
canonicalizeProofClass proofEGraph =
  canonicalizeClassId (proofBaseGraph proofEGraph)

canonicalizeProofStepInput :: (ClassId -> ClassId) -> ProofStepInput f c p -> ProofStepInput f c p
canonicalizeProofStepInput canonicalize proofStepInput =
  proofStepInput
    { psiLhsClass = canonicalize (psiLhsClass proofStepInput),
      psiRhsClass = canonicalize (psiRhsClass proofStepInput)
    }

canonicalizeAnnotationInput :: (ClassId -> ClassId) -> ProofAnnotationInput c -> ProofAnnotationInput c
canonicalizeAnnotationInput canonicalize annotationInput =
  annotationInput
    { paiLhsClass = canonicalize (paiLhsClass annotationInput),
      paiRhsClass = canonicalize (paiRhsClass annotationInput)
    }

proofBaseGraph :: ProofEGraph f a c p -> EGraph f a
proofBaseGraph =
  cegBase . pgGraph
