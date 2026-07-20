{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Proof.Certificate
  ( NebulaProvenance (..),
    ProvenanceEntry (..),
    HunkCertificate (..),
    nebulaProofBuilder,
    auditAdmissibleProofSteps,
    hunkCertificate,
    replayStepOf,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Foldable (traverse_)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula.Proof.Audit
  ( ReplayStep (..),
    TypeVerdict (..),
    verdictConflictWords,
    verdictWord,
  )
import Melusine.Nebula.Core (NebulaError (..), NebulaAnalysis)
import Melusine.Nebula.Rewrite.Corpus (LawStamp (..))
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr (HsExprF, ScopeCtx)
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, ENode (..), canonicalizeClassId, classIdKey, eGraphClassNodes)
import Moonlight.Flow.Model.Schema.Digest (StableDigest128, stableDigest128)
import Moonlight.Rewrite.ProofContext
  ( ProofAnnotationBuilder (..),
    ProofAnnotationInput (..),
    ProofKind (..),
    ProofStep (..),
  )
import Moonlight.Rewrite.System (rewriteOriginRuleIds)
import Moonlight.Rewrite.System (SemanticFidelity (..), TrustTier (..), lawIdKey)

type NebulaProvenance :: Type
data NebulaProvenance = NebulaProvenance
  { npStamp :: !(Maybe LawStamp),
    npRule :: !RewriteRuleId,
    npGuarded :: !Bool,
    npFactful :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type ProvenanceEntry :: Type
data ProvenanceEntry = ProvenanceEntry
  { peProvenance :: !NebulaProvenance,
    peLhsClass :: !ClassId,
    peRhsClass :: !ClassId
  }
  deriving stock (Eq, Ord, Show)

type HunkCertificate :: Type
data HunkCertificate = HunkCertificate
  { hcBinding :: !String,
    hcEntries :: ![ProvenanceEntry],
    hcTypeVerdict :: !TypeVerdict,
    hcDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

nebulaProofBuilder :: Map RewriteRuleId LawStamp -> ProofAnnotationBuilder ScopeCtx NebulaProvenance
nebulaProofBuilder lawTable =
  ProofAnnotationBuilder $
    \annotationInput ->
      NebulaProvenance
        { npStamp = Map.lookup (paiRewriteRuleId annotationInput) lawTable,
          npRule = paiRewriteRuleId annotationInput,
          npGuarded = maybe False (const True) (paiGuardEvidence annotationInput),
          npFactful = not (Set.null (paiFactDerivations annotationInput))
        }

auditAdmissibleProofSteps ::
  Map RewriteRuleId LawStamp ->
  [ProofStep HsExprF ScopeCtx NebulaProvenance] ->
  Either NebulaError ()
auditAdmissibleProofSteps lawTable =
  traverse_ auditStep
  where
    auditStep proofStep =
      traverse_ (auditRule proofStep) (proofStepRuleIds proofStep)
    auditRule proofStep ruleIdValue =
      case Map.lookup ruleIdValue lawTable of
        Nothing ->
          Left
            ( NebulaRuleDerivationError
                ("proof step cites an inadmissible rewrite rule: " <> rewriteRuleLabel ruleIdValue)
            )
        Just lawStamp ->
          if npStamp (psAnnotation proofStep) == Just lawStamp || psKind proofStep /= ProofRewrite ruleIdValue
            then Right ()
            else
              Left
                ( NebulaRuleDerivationError
                    ("proof annotation stamp disagrees with law table for " <> rewriteRuleLabel ruleIdValue)
                )

hunkCertificate ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  String ->
  ClassId ->
  [(ProofStep HsExprF ScopeCtx NebulaProvenance, TypeVerdict)] ->
  HunkCertificate
hunkCertificate contextGraph bindingName seedClass judgedSteps =
  let baseGraph = cegBase contextGraph
      canonicalSeed = canonicalizeClassId baseGraph seedClass
      canonicalJudged =
        [ (canonicalizeProofStep baseGraph proofStep, stepVerdict)
        | (proofStep, stepVerdict) <- judgedSteps
        ]
      relevantClasses =
        reachableProofClasses
          (fmap fst canonicalJudged)
          (descendantClassKeys baseGraph canonicalSeed)
      touching =
        [ judged
        | judged@(proofStep, _) <- canonicalJudged,
          proofStepTouches relevantClasses proofStep
        ]
      entries =
        Set.toAscList (Set.fromList (fmap (provenanceEntry . fst) touching))
      typeVerdict =
        foldMap snd touching
   in HunkCertificate
        { hcBinding = bindingName,
          hcEntries = entries,
          hcTypeVerdict = typeVerdict,
          hcDigest = provenanceDigest typeVerdict entries
        }

replayStepOf :: ProofStep HsExprF ScopeCtx NebulaProvenance -> ReplayStep
replayStepOf proofStep =
  ReplayStep
    { repRule = npRule (psAnnotation proofStep),
      repLhs = psLhsClass proofStep,
      repRhs = psRhsClass proofStep,
      repLhsWitness = psLhsWitness proofStep,
      repRhsWitness = psRhsWitness proofStep
    }

proofStepRuleIds :: ProofStep HsExprF ScopeCtx NebulaProvenance -> [RewriteRuleId]
proofStepRuleIds proofStep =
  case psKind proofStep of
    ProofRewrite ruleIdValue ->
      [ruleIdValue]
    ProofRewriteOrigin rewriteOrigin ->
      Set.toAscList (rewriteOriginRuleIds rewriteOrigin)
    ProofCongruence ->
      []
    ProofAnalysis ->
      []

canonicalizeProofStep ::
  EGraph HsExprF NebulaAnalysis ->
  ProofStep HsExprF ScopeCtx NebulaProvenance ->
  ProofStep HsExprF ScopeCtx NebulaProvenance
canonicalizeProofStep baseGraph proofStep =
  proofStep
    { psLhsClass = canonicalizeClassId baseGraph (psLhsClass proofStep),
      psRhsClass = canonicalizeClassId baseGraph (psRhsClass proofStep)
    }

descendantClassKeys :: EGraph HsExprF NebulaAnalysis -> ClassId -> IntSet
descendantClassKeys graph seedClass =
  untilFixed expand (IntSet.singleton (classIdKey seedClass))
  where
    expand classKeys =
      IntSet.union classKeys (foldMap childClassKeys (IntSet.toList classKeys))

    childClassKeys classKey =
      IntSet.fromList
        [ classIdKey (canonicalizeClassId graph childClass)
        | ENode node <- Set.toList (eGraphClassNodes graph (ClassId classKey)),
          childClass <- Foldable.toList node
        ]

reachableProofClasses :: [ProofStep HsExprF ScopeCtx NebulaProvenance] -> IntSet -> IntSet
reachableProofClasses proofSteps seedClassKeys =
  untilFixed expand seedClassKeys
  where
    adjacency = proofAdjacency proofSteps
    expand classKeys =
      IntSet.union classKeys (foldMap (\classKey -> IntMap.findWithDefault IntSet.empty classKey adjacency) (IntSet.toList classKeys))

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed step seed =
  until (\value -> step value == value) step seed

proofAdjacency :: [ProofStep HsExprF ScopeCtx NebulaProvenance] -> IntMap IntSet
proofAdjacency =
  IntMap.fromListWith
    IntSet.union
    . foldMap
      ( \proofStep ->
          let lhsKey = classIdKey (psLhsClass proofStep)
              rhsKey = classIdKey (psRhsClass proofStep)
           in [ (lhsKey, IntSet.singleton rhsKey),
                (rhsKey, IntSet.singleton lhsKey)
              ]
      )

proofStepTouches :: IntSet -> ProofStep HsExprF ScopeCtx NebulaProvenance -> Bool
proofStepTouches classKeys proofStep =
  not
    ( IntSet.null
        ( IntSet.intersection
            classKeys
            (IntSet.fromList [classIdKey (psLhsClass proofStep), classIdKey (psRhsClass proofStep)])
        )
    )

provenanceEntry :: ProofStep HsExprF ScopeCtx NebulaProvenance -> ProvenanceEntry
provenanceEntry proofStep =
  ProvenanceEntry
    { peProvenance = provenanceForKind (psKind proofStep) (psAnnotation proofStep),
      peLhsClass = psLhsClass proofStep,
      peRhsClass = psRhsClass proofStep
    }

provenanceForKind :: ProofKind -> NebulaProvenance -> NebulaProvenance
provenanceForKind = \case
  ProofRewrite {} ->
    id
  ProofRewriteOrigin {} ->
    id
  ProofCongruence ->
    clearStamp
  ProofAnalysis ->
    clearStamp
  where
    clearStamp provenance =
      provenance {npStamp = Nothing}

provenanceDigest :: TypeVerdict -> [ProvenanceEntry] -> StableDigest128
provenanceDigest typeVerdict entries =
  stableDigest128
    ( [verdictWord typeVerdict]
        <> verdictConflictWords typeVerdict
        <> [fromIntegral (length entries)]
        <> foldMap entryWords entries
    )

entryWords :: ProvenanceEntry -> [Word64]
entryWords entry =
  let provenance = peProvenance entry
   in [ rewriteRuleWord (npRule provenance),
        maybe 0 (fromIntegral . lawIdKey . lsLaw) (npStamp provenance),
        maybe 0 (trustTierWord . lsTier) (npStamp provenance),
        maybe 0 (semanticFidelityWord . lsFidelity) (npStamp provenance),
        boolWord (npGuarded provenance),
        boolWord (npFactful provenance),
        fromIntegral (classIdKey (peLhsClass entry)),
        fromIntegral (classIdKey (peRhsClass entry))
      ]

rewriteRuleWord :: RewriteRuleId -> Word64
rewriteRuleWord (RewriteRuleId ruleKey) =
  fromIntegral ruleKey

trustTierWord :: TrustTier -> Word64
trustTierWord = \case
  ParserVerified -> 1
  GhcVerified -> 2
  RegistryTrusted -> 3
  MachineProved -> 4
  ModuleDerived -> 5

semanticFidelityWord :: SemanticFidelity -> Word64
semanticFidelityWord = \case
  Observational -> 1
  UpToBottom -> 2

boolWord :: Bool -> Word64
boolWord = \case
  False -> 0
  True -> 1

rewriteRuleLabel :: RewriteRuleId -> String
rewriteRuleLabel (RewriteRuleId ruleKey) =
  "rule-" <> show ruleKey
