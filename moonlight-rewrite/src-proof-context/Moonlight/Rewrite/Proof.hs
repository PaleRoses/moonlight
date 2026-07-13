{-# LANGUAGE LambdaCase #-}

-- | Proof registry and query surface for rewrite execution.
-- Owns proof steps, annotations, context/support evidence, retention-aware
-- logs, direct proof lookup, reachability graphs, summaries, and witnesses.
-- Contracts: summaries count every recorded step even when logs are pruned,
-- while direct graph and witness queries obey the retention policy.
module Moonlight.Rewrite.Proof
  ( ProofKind (..),
    ProofAnnotationInput (..),
    ProofAnnotationBuilder (..),
    defaultProofAnnotationBuilder,
    ProofContextRestriction (..),
    ProofContextEvidence (..),
    SupportAwareProofEvidence (..),
    ProofStepInput (..),
    ProofStep (..),
    ProofNode (..),
    ProofGraph (..),
    ProofReachability,
    ProofRegistry,
    emptyProofRegistry,
    emptyProofRegistryWithRetention,
    proofRegistryRetention,
    proofRegistryRecordedStepCount,
    proofRegistryRetainedStepCount,
    proofRegistryDroppedStepCount,
    ProofRetention (..),
    defaultProofRetention,
    ProofQueryError (..),
    ProofCompressionSummary (..),
    defaultProofStepInput,
    proofInputFromRewriteOrigin,
    recordAnnotatedProofStep,
    recordProofStepWith,
    proofBetween,
    proofGraph,
    proofReachability,
    proofClassesReachableFrom,
    proofRelated,
    serializeProofLog,
    summarizeProofLog,
    proofClassWitnesses,
  )
where

import Data.Fix (Fix)
import Data.Foldable qualified as Foldable
import Data.Graph qualified as Graph
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    ProofStepId (..),
    RewriteRuleId,
    classIdKey,
  )
import Moonlight.Core (GuideEvidence)
import Moonlight.Core (Substitution)
import Moonlight.FiniteLattice
  ( SupportBasis
  )
import Moonlight.Control.Count
  ( naturalToBoundedInt,
  )
import Moonlight.Rewrite.Algebra
  ( RewriteOrigin (..),
  )
import Moonlight.Rewrite.System (GuardEvidence)
import Moonlight.Rewrite.System (FactDerivation)
import Moonlight.Rewrite.System
  ( RuleOrigin (..),
    rewriteOriginRuleIds,
  )
import Moonlight.Rewrite.System
  ( ProofQueryError (..),
    ProofRetention (..),
    defaultProofRetention,
  )
import Numeric.Natural (Natural)

type ProofKind :: Type
data ProofKind
  = ProofRewrite !RewriteRuleId
  | ProofRewriteOrigin !(RewriteOrigin RuleOrigin)
  | ProofCongruence
  | ProofAnalysis
  deriving stock (Eq, Ord, Show)

type ProofAnnotationInput :: Type -> Type
data ProofAnnotationInput c = ProofAnnotationInput
  { paiRewriteRuleId :: RewriteRuleId,
    paiLhsClass :: ClassId,
    paiRhsClass :: ClassId,
    paiSubstitution :: Substitution,
    paiGuardEvidence :: Maybe GuardEvidence,
    paiGuideEvidence :: Maybe (GuideEvidence ClassId),
    paiFactDerivations :: Set FactDerivation,
    paiContextEvidence :: Maybe (ProofContextEvidence c),
    paiSupportEvidence :: Maybe (SupportAwareProofEvidence c)
  }
  deriving stock (Eq, Ord, Show)

type ProofAnnotationBuilder :: Type -> Type -> Type
newtype ProofAnnotationBuilder c p = ProofAnnotationBuilder
  { buildProofAnnotation :: ProofAnnotationInput c -> p
  }

defaultProofAnnotationBuilder :: Monoid p => ProofAnnotationBuilder c p
defaultProofAnnotationBuilder =
  ProofAnnotationBuilder (const mempty)

type ProofContextRestriction :: Type -> Type
data ProofContextRestriction c = ProofContextRestriction
  { pcrSourceContext :: c,
    pcrTargetContext :: c
  }
  deriving stock (Eq, Ord, Show, Read)

type ProofContextEvidence :: Type -> Type
data ProofContextEvidence c = ProofContextEvidence
  { pceActiveContext :: Maybe c,
    pceRestrictions :: [ProofContextRestriction c]
  }
  deriving stock (Eq, Ord, Show, Read)

type SupportAwareProofEvidence :: Type -> Type
data SupportAwareProofEvidence c = SupportAwareProofEvidence
  { sapeSupport :: SupportBasis c,
    sapeRestrictions :: [ProofContextRestriction c]
  }
  deriving stock (Eq, Ord, Show)

deriving stock instance (Read c, Read (SupportBasis c)) => Read (SupportAwareProofEvidence c)

type ProofStepInput :: (Type -> Type) -> Type -> Type -> Type
data ProofStepInput f c p = ProofStepInput
  { psiProofKind :: ProofKind,
    psiLhsClass :: ClassId,
    psiRhsClass :: ClassId,
    psiSubstitution :: Substitution,
    psiAnnotation :: p,
    psiGuardEvidence :: Maybe GuardEvidence,
    psiFactDerivations :: Set FactDerivation,
    psiContextEvidence :: Maybe (ProofContextEvidence c),
    psiSupportEvidence :: Maybe (SupportAwareProofEvidence c),
    psiLhsWitness :: Maybe (Fix f),
    psiRhsWitness :: Maybe (Fix f)
  }

type ProofStep :: (Type -> Type) -> Type -> Type -> Type
data ProofStep f c p = ProofStep
  { psId :: ProofStepId,
    psKind :: ProofKind,
    psLhsClass :: ClassId,
    psRhsClass :: ClassId,
    psLhsWitness :: Maybe (Fix f),
    psRhsWitness :: Maybe (Fix f),
    psSubstitution :: Substitution,
    psGuardEvidence :: Maybe GuardEvidence,
    psFactDerivations :: Set FactDerivation,
    psContextEvidence :: Maybe (ProofContextEvidence c),
    psSupportEvidence :: Maybe (SupportAwareProofEvidence c),
    psAnnotation :: p,
    psTimestamp :: Int
  }

instance (Show c, Show p) => Show (ProofStep f c p) where
  showsPrec precedence proofStepValue =
    showParen (precedence > 10) $
      showString "ProofStep "
        . showsPrec 11 (psId proofStepValue)
        . showChar ' '
        . showsPrec 11 (psKind proofStepValue)
        . showChar ' '
        . showsPrec 11 (psLhsClass proofStepValue)
        . showChar ' '
        . showsPrec 11 (psRhsClass proofStepValue)
        . showChar ' '
        . showsPrec 11 (fmap (const "<witness>") (psLhsWitness proofStepValue))
        . showChar ' '
        . showsPrec 11 (fmap (const "<witness>") (psRhsWitness proofStepValue))
        . showChar ' '
        . showsPrec 11 (psSubstitution proofStepValue)
        . showChar ' '
        . showsPrec 11 (psGuardEvidence proofStepValue)
        . showChar ' '
        . showsPrec 11 (psFactDerivations proofStepValue)
        . showChar ' '
        . showsPrec 11 (psContextEvidence proofStepValue)
        . showChar ' '
        . showsPrec 11 (psSupportEvidence proofStepValue)
        . showChar ' '
        . showsPrec 11 (psAnnotation proofStepValue)
        . showChar ' '
        . showsPrec 11 (psTimestamp proofStepValue)

type ProofNode :: (Type -> Type) -> Type
data ProofNode f
  = ProofAxiom !RewriteRuleId
  | ProofIdentityNode
  | ProofCompositeNode !(ProofNode f) !(ProofNode f)
  | ProofCongruenceNode ![ProofStepId]
  deriving stock (Eq, Show)

type ProofGraph :: (Type -> Type) -> Type
data ProofGraph f = ProofGraph
  { pgNodes :: IntMap (ProofNode f),
    pgEdges :: [(ProofStepId, ClassId, ClassId)]
  }
  deriving stock (Eq, Show)

type ProofReachability :: Type
data ProofReachability
  = ContiguousProofReachability {-# UNPACK #-} !Int !Graph.Graph
  | DenseProofReachability
      !Graph.Graph
      !(IntMap Graph.Vertex)
      !(IntMap Int)
  deriving stock (Eq, Show)

type ProofRegistry :: (Type -> Type) -> Type -> Type -> Type
data ProofRegistry f c p = ProofRegistry
  { prProofRetention :: !ProofRetention,
    prProofLog :: !(Seq (ProofStep f c p)),
    prProofIndex :: !(IntMap (Seq ProofStepId)),
    prNextProofStepId :: !ProofStepId,
    prFirstRetainedProofStepId :: !ProofStepId,
    prTimestamp :: {-# UNPACK #-} !Int,
    prProofSummaryState :: !(ProofSummaryState c)
  }

data ProofSummaryState c = ProofSummaryState
  { pssTotalSteps :: {-# UNPACK #-} !Int,
    pssClassPairs :: !(Set.Set (Int, Int)),
    pssRewriteRules :: !(Set.Set RewriteRuleId),
    pssWitnessedSteps :: {-# UNPACK #-} !Int,
    pssGuardedSteps :: {-# UNPACK #-} !Int,
    pssContextualSteps :: {-# UNPACK #-} !Int,
    pssSupportAwareSteps :: {-# UNPACK #-} !Int,
    pssUniqueSupports :: !(Set.Set (SupportBasis c)),
    pssFactfulSteps :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

emptyProofSummaryState :: ProofSummaryState c
emptyProofSummaryState =
  ProofSummaryState
    { pssTotalSteps = 0,
      pssClassPairs = Set.empty,
      pssRewriteRules = Set.empty,
      pssWitnessedSteps = 0,
      pssGuardedSteps = 0,
      pssContextualSteps = 0,
      pssSupportAwareSteps = 0,
      pssUniqueSupports = Set.empty,
      pssFactfulSteps = 0
    }

type ProofCompressionSummary :: Type
data ProofCompressionSummary = ProofCompressionSummary
  { pcsTotalSteps :: Int,
    pcsUniqueClassPairs :: Int,
    pcsUniqueRewriteRules :: Int,
    pcsCompressionSavings :: Int,
    pcsWitnessedSteps :: Int,
    pcsGuardedSteps :: Int,
    pcsContextualSteps :: Int,
    pcsSupportAwareSteps :: Int,
    pcsUniqueSupports :: Int,
    pcsFactfulSteps :: Int
  }
  deriving stock (Eq, Show)

emptyProofRegistry :: ProofRegistry f c p
emptyProofRegistry =
  emptyProofRegistryWithRetention defaultProofRetention

emptyProofRegistryWithRetention :: ProofRetention -> ProofRegistry f c p
emptyProofRegistryWithRetention retention =
  ProofRegistry
    { prProofRetention = retention,
      prProofLog = Seq.empty,
      prProofIndex = IntMap.empty,
      prNextProofStepId = ProofStepId 0,
      prFirstRetainedProofStepId = ProofStepId 0,
      prTimestamp = 0,
      prProofSummaryState = emptyProofSummaryState
    }

proofRegistryRetention :: ProofRegistry f c p -> ProofRetention
proofRegistryRetention =
  prProofRetention

proofRegistryRecordedStepCount :: ProofRegistry f c p -> Int
proofRegistryRecordedStepCount =
  pssTotalSteps . prProofSummaryState

proofRegistryRetainedStepCount :: ProofRegistry f c p -> Int
proofRegistryRetainedStepCount =
  Seq.length . prProofLog

proofRegistryDroppedStepCount :: ProofRegistry f c p -> Int
proofRegistryDroppedStepCount =
  proofStepKey . prFirstRetainedProofStepId

defaultProofStepInput :: RewriteRuleId -> ClassId -> ClassId -> Substitution -> p -> ProofStepInput f c p
defaultProofStepInput =
  proofStepInputWithKind ProofRewrite

proofInputFromRewriteOrigin ::
  RewriteOrigin RuleOrigin ->
  ClassId ->
  ClassId ->
  Substitution ->
  p ->
  ProofStepInput f c p
proofInputFromRewriteOrigin =
  proofStepInputWithKind ProofRewriteOrigin

proofStepInputWithKind ::
  (source -> ProofKind) ->
  source ->
  ClassId ->
  ClassId ->
  Substitution ->
  p ->
  ProofStepInput f c p
proofStepInputWithKind mkProofKind proofSource leftClassId rightClassId substitution annotation =
  ProofStepInput
    { psiProofKind = mkProofKind proofSource,
      psiLhsClass = leftClassId,
      psiRhsClass = rightClassId,
      psiSubstitution = substitution,
      psiAnnotation = annotation,
      psiGuardEvidence = Nothing,
      psiFactDerivations = Set.empty,
      psiContextEvidence = Nothing,
      psiSupportEvidence = Nothing,
      psiLhsWitness = Nothing,
      psiRhsWitness = Nothing
    }

recordProofStepWith :: Ord c => ProofStepInput f c p -> ProofRegistry f c p -> ProofRegistry f c p
recordProofStepWith proofStepInput proofRegistry =
  let stepId =
        prNextProofStepId proofRegistry
      stepTimestamp =
        prTimestamp proofRegistry
      proofStep =
        ProofStep
          { psId = stepId,
            psKind = psiProofKind proofStepInput,
            psLhsClass = psiLhsClass proofStepInput,
            psRhsClass = psiRhsClass proofStepInput,
            psLhsWitness = psiLhsWitness proofStepInput,
            psRhsWitness = psiRhsWitness proofStepInput,
            psSubstitution = psiSubstitution proofStepInput,
            psGuardEvidence = psiGuardEvidence proofStepInput,
            psFactDerivations = psiFactDerivations proofStepInput,
            psContextEvidence = psiContextEvidence proofStepInput,
            psSupportEvidence = psiSupportEvidence proofStepInput,
            psAnnotation = psiAnnotation proofStepInput,
            psTimestamp = stepTimestamp
          }
      retained =
        retainProofStep proofStep proofRegistry
   in proofRegistry
        { prProofLog = rpsLog retained,
          prProofIndex = rpsIndex retained,
          prNextProofStepId = succ stepId,
          prFirstRetainedProofStepId = rpsFirstRetainedProofStepId retained,
          prTimestamp = stepTimestamp + 1,
          prProofSummaryState = observeProofStep proofStep (prProofSummaryState proofRegistry)
        }

data RetainedProofState f c p = RetainedProofState
  { rpsLog :: !(Seq (ProofStep f c p)),
    rpsIndex :: !(IntMap (Seq ProofStepId)),
    rpsFirstRetainedProofStepId :: !ProofStepId
  }

retainProofStep :: ProofStep f c p -> ProofRegistry f c p -> RetainedProofState f c p
retainProofStep proofStep proofRegistry =
  case prProofRetention proofRegistry of
    KeepNoProof ->
      droppedProofState proofStep
    KeepProofSummary ->
      droppedProofState proofStep
    KeepFullProof ->
      let nextLog =
            prProofLog proofRegistry |> proofStep
          nextIndex =
            insertStepIntoProofIndex proofStep (prProofIndex proofRegistry)
       in RetainedProofState
            { rpsLog = nextLog,
              rpsIndex = nextIndex,
              rpsFirstRetainedProofStepId = prFirstRetainedProofStepId proofRegistry
            }
    KeepRecentProofSteps retainedBound ->
      retainRecentProofStep retainedBound proofStep proofRegistry

retainRecentProofStep :: Natural -> ProofStep f c p -> ProofRegistry f c p -> RetainedProofState f c p
retainRecentProofStep retainedBound proofStep proofRegistry
  | retainedBound == 0 =
      droppedProofState proofStep
  | otherwise =
      let appendedLog =
            prProofLog proofRegistry |> proofStep
          retainedCount =
            naturalToBoundedInt retainedBound
          dropCount =
            max 0 (Seq.length appendedLog - retainedCount)
          trimmedLog =
            Seq.drop dropCount appendedLog
       in RetainedProofState
            { rpsLog = trimmedLog,
              rpsIndex = proofIndexFromSteps trimmedLog,
              rpsFirstRetainedProofStepId =
                maybe (succ (psId proofStep)) psId (Seq.lookup 0 trimmedLog)
            }

droppedProofState :: ProofStep f c p -> RetainedProofState f c p
droppedProofState proofStep =
  RetainedProofState
    { rpsLog = Seq.empty,
      rpsIndex = IntMap.empty,
      rpsFirstRetainedProofStepId = succ (psId proofStep)
    }

insertStepIntoProofIndex :: ProofStep f c p -> IntMap (Seq ProofStepId) -> IntMap (Seq ProofStepId)
insertStepIntoProofIndex proofStep index =
  IntMap.unionWith
    (Seq.><)
    index
    (proofStepIndexEntries proofStep)

proofIndexFromSteps :: Seq (ProofStep f c p) -> IntMap (Seq ProofStepId)
proofIndexFromSteps =
  IntMap.fromListWith
    (flip (Seq.><))
    . foldMap proofStepIndexEntryList

proofStepIndexEntries :: ProofStep f c p -> IntMap (Seq ProofStepId)
proofStepIndexEntries =
  IntMap.fromListWith
    (flip (Seq.><))
    . proofStepIndexEntryList

proofStepIndexEntryList :: ProofStep f c p -> [(Int, Seq ProofStepId)]
proofStepIndexEntryList proofStep =
  fmap
    ( \classId ->
        ( classIdKey classId,
          Seq.singleton (psId proofStep)
        )
    )
    (proofStepEndpointClasses proofStep)

proofStepEndpointClasses :: ProofStep f c p -> [ClassId]
proofStepEndpointClasses proofStep =
  if psLhsClass proofStep == psRhsClass proofStep
    then [psLhsClass proofStep]
    else [psLhsClass proofStep, psRhsClass proofStep]

observeProofStep :: Ord c => ProofStep f c p -> ProofSummaryState c -> ProofSummaryState c
observeProofStep proofStep summary =
  summary
    { pssTotalSteps = pssTotalSteps summary + 1,
      pssClassPairs = Set.insert (orderedClassPair (psLhsClass proofStep) (psRhsClass proofStep)) (pssClassPairs summary),
      pssRewriteRules = Set.union (rewriteRulesOfProofStep proofStep) (pssRewriteRules summary),
      pssWitnessedSteps = pssWitnessedSteps summary + boolAsInt (hasWitness (psLhsWitness proofStep) || hasWitness (psRhsWitness proofStep)),
      pssGuardedSteps = pssGuardedSteps summary + boolAsInt (hasWitness (psGuardEvidence proofStep)),
      pssContextualSteps = pssContextualSteps summary + boolAsInt (hasWitness (psContextEvidence proofStep)),
      pssSupportAwareSteps = pssSupportAwareSteps summary + boolAsInt (hasWitness (psSupportEvidence proofStep)),
      pssUniqueSupports = maybeInsert (fmap sapeSupport (psSupportEvidence proofStep)) (pssUniqueSupports summary),
      pssFactfulSteps = pssFactfulSteps summary + boolAsInt (not (Set.null (psFactDerivations proofStep)))
    }

orderedClassPair :: ClassId -> ClassId -> (Int, Int)
orderedClassPair leftClass rightClass =
  let leftKey =
        classIdKey leftClass
      rightKey =
        classIdKey rightClass
   in if leftKey <= rightKey
        then (leftKey, rightKey)
        else (rightKey, leftKey)

rewriteRulesOfProofStep :: ProofStep f c p -> Set.Set RewriteRuleId
rewriteRulesOfProofStep proofStep =
  case psKind proofStep of
    ProofRewrite rewriteRuleId ->
      Set.singleton rewriteRuleId
    ProofRewriteOrigin rewriteOrigin ->
      rewriteOriginRuleIds rewriteOrigin
    ProofCongruence ->
      Set.empty
    ProofAnalysis ->
      Set.empty

maybeInsert :: Ord value => Maybe value -> Set.Set value -> Set.Set value
maybeInsert maybeValue values =
  case maybeValue of
    Nothing ->
      values
    Just value ->
      Set.insert value values

hasWitness :: Maybe value -> Bool
hasWitness =
  isJust

boolAsInt :: Bool -> Int
boolAsInt flag =
  if flag then 1 else 0

recordAnnotatedProofStep ::
  Ord c =>
  ProofAnnotationBuilder c p ->
  ProofAnnotationInput c ->
  Maybe (Fix f) ->
  Maybe (Fix f) ->
  ProofRegistry f c p ->
  ProofRegistry f c p
recordAnnotatedProofStep proofAnnotationBuilder annotationInput lhsWitness rhsWitness =
  recordProofStepWith
    ( (defaultProofStepInput
          (paiRewriteRuleId annotationInput)
          (paiLhsClass annotationInput)
          (paiRhsClass annotationInput)
          (paiSubstitution annotationInput)
          (buildProofAnnotation proofAnnotationBuilder annotationInput)
      )
        { psiGuardEvidence = paiGuardEvidence annotationInput,
          psiFactDerivations = paiFactDerivations annotationInput,
          psiContextEvidence = paiContextEvidence annotationInput,
          psiSupportEvidence = paiSupportEvidence annotationInput,
          psiLhsWitness = lhsWitness,
          psiRhsWitness = rhsWitness
        }
    )

proofBetween :: ClassId -> ClassId -> ProofRegistry f c p -> Either ProofQueryError (ProofStep f c p)
proofBetween leftClass rightClass proofRegistry = do
  requireDirectProofQueryAvailable proofRegistry
  case findProofStepBetween leftClass rightClass proofRegistry of
    Just proofStep ->
      Right proofStep
    Nothing
      | proofSummaryRecordedPair leftClass rightClass proofRegistry,
        proofRegistryDroppedStepCount proofRegistry > 0 ->
          Left ProofPruned
      | otherwise ->
          Left ProofNotRecorded

proofSummaryRecordedPair :: ClassId -> ClassId -> ProofRegistry f c p -> Bool
proofSummaryRecordedPair leftClass rightClass =
  Set.member (orderedClassPair leftClass rightClass)
    . pssClassPairs
    . prProofSummaryState

findProofStepBetween :: ClassId -> ClassId -> ProofRegistry f c p -> Maybe (ProofStep f c p)
findProofStepBetween leftClass rightClass =
  Foldable.find (proofStepConnects leftClass rightClass)
    . proofStepCandidatesFromIndex leftClass

proofStepCandidatesFromIndex :: ClassId -> ProofRegistry f c p -> [ProofStep f c p]
proofStepCandidatesFromIndex classId proofRegistry =
  mapMaybe
    (resolveProofStepId proofRegistry)
    ( Foldable.toList
        ( IntMap.findWithDefault
            Seq.empty
            (classIdKey classId)
            (prProofIndex proofRegistry)
        )
    )

resolveProofStepId :: ProofRegistry f c p -> ProofStepId -> Maybe (ProofStep f c p)
resolveProofStepId proofRegistry proofStepId =
  let retainedOffset =
        proofStepKey proofStepId - proofStepKey (prFirstRetainedProofStepId proofRegistry)
   in if retainedOffset < 0
        then Nothing
        else
          Seq.lookup retainedOffset (prProofLog proofRegistry)
            >>= \proofStep ->
              if psId proofStep == proofStepId
                then Just proofStep
                else Nothing

requireDirectProofQueryAvailable :: ProofRegistry f c p -> Either ProofQueryError ()
requireDirectProofQueryAvailable proofRegistry =
  case prProofRetention proofRegistry of
    KeepFullProof ->
      Right ()
    KeepRecentProofSteps retained
      | retained > 0 ->
          Right ()
      | otherwise ->
          Left (ProofUnavailableForRetention (prProofRetention proofRegistry))
    KeepNoProof ->
      Left (ProofUnavailableForRetention KeepNoProof)
    KeepProofSummary ->
      Left (ProofUnavailableForRetention KeepProofSummary)

proofStepConnects :: ClassId -> ClassId -> ProofStep f c p -> Bool
proofStepConnects leftClass rightClass proofStep =
  (psLhsClass proofStep == leftClass && psRhsClass proofStep == rightClass)
    || (psLhsClass proofStep == rightClass && psRhsClass proofStep == leftClass)

proofGraph :: ProofRegistry f c p -> Either ProofQueryError (ProofGraph f)
proofGraph proofRegistry = do
  requireFullProofAvailable proofRegistry
  let proofSteps =
        serializeProofLog proofRegistry
      nodes =
        IntMap.fromList
          [ (proofStepKey (psId proofStep), proofNodeFromKind (psKind proofStep) (psId proofStep))
            | proofStep <- proofSteps
          ]
      edges =
        [ (psId proofStep, psLhsClass proofStep, psRhsClass proofStep)
          | proofStep <- proofSteps
        ]
  Right
    ProofGraph
      { pgNodes = nodes,
        pgEdges = edges
      }

proofReachability :: ProofRegistry f c p -> Either ProofQueryError ProofReachability
proofReachability proofRegistry =
  if proofClassIndexIsContiguous proofClassIndex
    then do
      graphValue <- proofGraph proofRegistry
      Right (contiguousProofReachabilityGraph (pgEdges graphValue))
    else do
      requireFullProofAvailable proofRegistry
      Right
        ( denseProofReachabilityGraph
            (snd (IntMap.split (-1) proofClassIndex))
            (fmap proofGraphEdge (serializeProofLog proofRegistry))
        )
  where
    proofClassIndex =
      prProofIndex proofRegistry
{-# INLINE proofReachability #-}

proofClassesReachableFrom :: ClassId -> ProofReachability -> IntSet
proofClassesReachableFrom classId reachability =
  case reachability of
    ContiguousProofReachability vertexCount graphValue
      | classKey < 0 || classKey >= vertexCount ->
          IntSet.singleton classKey
      | otherwise ->
          IntSet.fromList (Graph.reachable graphValue classKey)
    DenseProofReachability graphValue denseVertexByClassKey classKeyByDenseVertex ->
      case IntMap.lookup classKey denseVertexByClassKey of
        Nothing ->
          IntSet.singleton classKey
        Just denseVertex ->
          IntSet.fromList
            ( mapMaybe
                (`IntMap.lookup` classKeyByDenseVertex)
                (Graph.reachable graphValue denseVertex)
            )
  where
    classKey =
      classIdKey classId

proofRelated :: ClassId -> ClassId -> ProofRegistry f c p -> Either ProofQueryError Bool
proofRelated sourceClass targetClass proofRegistry = do
  reachability <- proofReachability proofRegistry
  Right
    ( IntSet.member
        (classIdKey targetClass)
        (proofClassesReachableFrom sourceClass reachability)
    )

serializeProofLog :: ProofRegistry f c p -> [ProofStep f c p]
serializeProofLog =
  Foldable.toList . prProofLog

summarizeProofLog :: ProofRegistry f c p -> ProofCompressionSummary
summarizeProofLog proofRegistry =
  let summary =
        prProofSummaryState proofRegistry
      totalSteps =
        pssTotalSteps summary
      uniqueClassPairs =
        Set.size (pssClassPairs summary)
   in ProofCompressionSummary
        { pcsTotalSteps = totalSteps,
          pcsUniqueClassPairs = uniqueClassPairs,
          pcsUniqueRewriteRules = Set.size (pssRewriteRules summary),
          pcsCompressionSavings = max 0 (totalSteps - uniqueClassPairs),
          pcsWitnessedSteps = pssWitnessedSteps summary,
          pcsGuardedSteps = pssGuardedSteps summary,
          pcsContextualSteps = pssContextualSteps summary,
          pcsSupportAwareSteps = pssSupportAwareSteps summary,
          pcsUniqueSupports = Set.size (pssUniqueSupports summary),
          pcsFactfulSteps = pssFactfulSteps summary
        }

proofClassWitnesses :: ClassId -> ProofRegistry f c p -> Either ProofQueryError [Fix f]
proofClassWitnesses classId proofRegistry = do
  requireProofLogAvailable proofRegistry
  Right
    (serializeProofLog proofRegistry >>= witnessTermsForStep classId)

requireProofLogAvailable :: ProofRegistry f c p -> Either ProofQueryError ()
requireProofLogAvailable proofRegistry =
  case prProofRetention proofRegistry of
    KeepFullProof ->
      Right ()
    KeepRecentProofSteps retained
      | retained > 0,
        proofRegistryDroppedStepCount proofRegistry == 0 ->
          Right ()
      | retained > 0 ->
          Left ProofPruned
      | otherwise ->
          Left (ProofUnavailableForRetention (prProofRetention proofRegistry))
    KeepNoProof ->
      Left (ProofUnavailableForRetention KeepNoProof)
    KeepProofSummary ->
      Left (ProofUnavailableForRetention KeepProofSummary)

requireFullProofAvailable :: ProofRegistry f c p -> Either ProofQueryError ()
requireFullProofAvailable proofRegistry =
  case prProofRetention proofRegistry of
    KeepFullProof
      | proofRegistryDroppedStepCount proofRegistry == 0 ->
          Right ()
      | otherwise ->
          Left ProofPruned
    retention ->
      Left (ProofUnavailableForRetention retention)

proofClassIndexIsContiguous :: IntMap value -> Bool
proofClassIndexIsContiguous proofClassIndex =
  case (IntMap.lookupMin proofClassIndex, IntMap.lookupMax proofClassIndex) of
    (Nothing, Nothing) ->
      True
    (Just (minimumClassKey, _), Just (maximumClassKey, _)) ->
      minimumClassKey == 0
        && IntMap.size proofClassIndex == maximumClassKey + 1
    _ ->
      False
{-# INLINE proofClassIndexIsContiguous #-}

contiguousProofReachabilityGraph :: [(ProofStepId, ClassId, ClassId)] -> ProofReachability
contiguousProofReachabilityGraph proofEdges =
  ContiguousProofReachability
    vertexCount
    ( Graph.buildG
        (0, max 0 (vertexCount - 1))
        boundedEdges
    )
  where
    vertexCount =
      Foldable.foldl' maximumProofVertex 0 proofEdges

    maximumProofVertex :: Int -> (ProofStepId, ClassId, ClassId) -> Int
    maximumProofVertex count (_stepId, leftClass, rightClass) =
      max count (max (classIdKey leftClass) (classIdKey rightClass) + 1)

    boundedEdges =
      [ (source, target)
        | (source, target) <- proofAdjacencyEdges proofEdges,
          source >= 0,
          source < vertexCount,
          target >= 0,
          target < vertexCount
      ]
{-# INLINE contiguousProofReachabilityGraph #-}

denseProofReachabilityGraph :: IntMap value -> [(ProofStepId, ClassId, ClassId)] -> ProofReachability
denseProofReachabilityGraph observedClassIndex proofEdges =
  DenseProofReachability
    ( Graph.buildG
        (0, observedClassCount - 1)
        denseEdges
    )
    denseVertexByClassKey
    classKeyByDenseVertex
  where
    observedClassCount =
      IntMap.size observedClassIndex

    observedClassKeys =
      IntMap.keys observedClassIndex

    denseVertexByClassKey =
      IntMap.fromDistinctAscList (zip observedClassKeys [0 ..])

    classKeyByDenseVertex =
      IntMap.fromDistinctAscList (zip [0 ..] observedClassKeys)

    denseEdges =
      mapMaybe
        (traverseProofAdjacencyEdge (`IntMap.lookup` denseVertexByClassKey))
        (proofAdjacencyEdges proofEdges)
{-# NOINLINE denseProofReachabilityGraph #-}

traverseProofAdjacencyEdge ::
  (Int -> Maybe Graph.Vertex) ->
  (Int, Int) ->
  Maybe (Graph.Vertex, Graph.Vertex)
traverseProofAdjacencyEdge remapClassKey (sourceClassKey, targetClassKey) =
  (,)
    <$> remapClassKey sourceClassKey
    <*> remapClassKey targetClassKey

proofAdjacencyEdges :: [(ProofStepId, ClassId, ClassId)] -> [(Int, Int)]
proofAdjacencyEdges proofEdges =
  [ (classIdKey classIdValue, classIdKey adjacentClassId)
    | (_, leftClass, rightClass) <- proofEdges,
      (classIdValue, adjacentClassId) <-
        [ (leftClass, rightClass),
          (rightClass, leftClass)
        ]
    ]

proofGraphEdge :: ProofStep f c p -> (ProofStepId, ClassId, ClassId)
proofGraphEdge proofStep =
  (psId proofStep, psLhsClass proofStep, psRhsClass proofStep)
{-# INLINE proofGraphEdge #-}

proofNodeFromKind :: ProofKind -> ProofStepId -> ProofNode f
proofNodeFromKind proofKindValue proofStepId =
  case proofKindValue of
    ProofRewrite rewriteRuleId ->
      ProofAxiom rewriteRuleId
    ProofRewriteOrigin rewriteOrigin ->
      proofNodeFromRewriteOrigin rewriteOrigin
    ProofCongruence ->
      ProofCongruenceNode [proofStepId]
    ProofAnalysis ->
      ProofCongruenceNode [proofStepId]

proofNodeFromRewriteOrigin :: RewriteOrigin RuleOrigin -> ProofNode f
proofNodeFromRewriteOrigin =
  \case
    RewriteIdentity ->
      ProofIdentityNode
    RewriteAtomic ruleOrigin ->
      ProofAxiom (roRuleId ruleOrigin)
    RewriteComposite leftOrigin rightOrigin ->
      ProofCompositeNode
        (proofNodeFromRewriteOrigin leftOrigin)
        (proofNodeFromRewriteOrigin rightOrigin)

proofStepKey :: ProofStepId -> Int
proofStepKey (ProofStepId key) = key

witnessTermsForStep :: ClassId -> ProofStep f c p -> [Fix f]
witnessTermsForStep classId proofStepValue =
  lhsWitnessTerms <> rhsWitnessTerms
  where
    lhsWitnessTerms
      | psLhsClass proofStepValue == classId =
          maybeToList (psLhsWitness proofStepValue)
      | otherwise =
          []

    rhsWitnessTerms
      | psRhsClass proofStepValue == classId =
          maybeToList (psRhsWitness proofStepValue)
      | otherwise =
          []
