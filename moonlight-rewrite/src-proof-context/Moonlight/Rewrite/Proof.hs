{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Proof
  ( ProofKind (..),
    ProofAnnotationInput (..),
    ProofAnnotationBuilder (..),
    defaultProofAnnotationBuilder,
    ProofContextRestriction (..),
    ProofContextEvidence (..),
    SupportAwareProofEvidence (..),
    ProofStepSummaryInput (..),
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
    defaultProofStepSummaryInput,
    proofInputFromRewriteOrigin,
    recordAnnotatedProofStep,
    recordProofStepByRetention,
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
import Data.Bits ((.|.), finiteBitSize, shiftL)
import Data.Foldable qualified as Foldable
import Data.Graph qualified as Graph
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Int (Int32)
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
    GuideEvidence,
    ProofStepId (..),
    RewriteRuleId,
    Substitution,
    classIdKey,
    rewriteRuleIdKey,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )
import Moonlight.Control.Count
  ( naturalToBoundedInt,
  )
import Moonlight.Rewrite.Algebra
  ( RewriteOrigin (..),
  )
import Moonlight.Rewrite.System
  ( FactDerivation,
    GuardEvidence,
    ProofQueryError (..),
    ProofRetention (..),
    RuleOrigin (..),
    defaultProofRetention,
    proofRetentionStoresAnyLog,
    proofRetentionStoresFullLog,
    rewriteOriginRuleIds,
  )
import Numeric.Natural (Natural)
import Data.Word (Word32, Word64)
import Data.Vector.Unboxed qualified as UnboxedVector

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

type ProofStepSummaryInput :: Type -> Type
data ProofStepSummaryInput c = ProofStepSummaryInput
  { pssiProofKind :: !ProofKind,
    pssiLhsClass :: !ClassId,
    pssiRhsClass :: !ClassId,
    pssiWitnessed :: !Bool,
    pssiGuarded :: !Bool,
    pssiContextEvidence :: !(Maybe (ProofContextEvidence c)),
    pssiSupportEvidence :: !(Maybe (SupportAwareProofEvidence c)),
    pssiFactful :: !Bool
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
    prProofIndex :: !ProofIndex,
    prNextProofStepId :: !ProofStepId,
    prFirstRetainedProofStepId :: !ProofStepId,
    prTimestamp :: {-# UNPACK #-} !Int,
    prProofSummaryState :: !(ProofSummaryState c)
  }

data ProofIndex
  = NoProofIndex
  | RecentProofIndex !(UnboxedVector.Vector (Int, Int))
  | FullProofIndex !(IntMap ProofStepIds)

data ProofStepIds
  = OneProofStepId !ProofStepId
  | ManyProofStepIds !(Seq ProofStepId)

data ProofSummaryState c = ProofSummaryState
  { pssTotalSteps :: {-# UNPACK #-} !Int,
    pssClassPairs :: !ClassPairSet,
    pssRewriteRules :: !RewriteRuleKeySet,
    pssWitnessedSteps :: {-# UNPACK #-} !Int,
    pssGuardedSteps :: {-# UNPACK #-} !Int,
    pssContextualSteps :: {-# UNPACK #-} !Int,
    pssSupportAwareSteps :: {-# UNPACK #-} !Int,
    pssUniqueSupports :: !(Set.Set (SupportBasis c)),
    pssFactfulSteps :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data RewriteRuleKeySet
  = NoRewriteRuleKeys
  | OneRewriteRuleKey {-# UNPACK #-} !Int
  | TwoRewriteRuleKeys {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ManyRewriteRuleKeys !IntSet
  deriving stock (Eq, Show)

data ClassPairSet = ClassPairSet
  {-# UNPACK #-} !Int
  ![PackedClassPairChunk]
  !IntSet
  !(Set.Set ClassPair)
  deriving stock (Eq, Show)

data PackedClassPairChunk = PackedClassPairChunk
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  !(UnboxedVector.Vector Int)
  deriving stock (Eq, Show)

data ClassPair = ClassPair
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

emptyProofSummaryState :: ProofSummaryState c
emptyProofSummaryState =
  ProofSummaryState
    { pssTotalSteps = 0,
      pssClassPairs = emptyClassPairSet,
      pssRewriteRules = NoRewriteRuleKeys,
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
      prProofIndex = emptyProofIndexForRetention retention,
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

defaultProofStepSummaryInput :: RewriteRuleId -> ClassId -> ClassId -> ProofStepSummaryInput c
defaultProofStepSummaryInput rewriteRuleId leftClassId rightClassId =
  ProofStepSummaryInput
    { pssiProofKind = ProofRewrite rewriteRuleId,
      pssiLhsClass = leftClassId,
      pssiRhsClass = rightClassId,
      pssiWitnessed = False,
      pssiGuarded = False,
      pssiContextEvidence = Nothing,
      pssiSupportEvidence = Nothing,
      pssiFactful = False
    }

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

recordProofStepByRetention ::
  Ord c =>
  ProofStepSummaryInput c ->
  ProofStepInput f c p ->
  ProofRegistry f c p ->
  ProofRegistry f c p
recordProofStepByRetention summaryInput proofStepInput proofRegistry =
  case prProofRetention proofRegistry of
    KeepNoProof ->
      proofRegistry
    KeepProofSummary ->
      recordProofSummaryInput summaryInput proofRegistry
    KeepRecentProofSteps retained
      | retained == 0 ->
          recordProofSummaryInput summaryInput proofRegistry
      | otherwise ->
          recordRetainedProofStep proofStepInput proofRegistry
    KeepFullProof ->
      recordRetainedProofStep proofStepInput proofRegistry

recordProofStepWith :: Ord c => ProofStepInput f c p -> ProofRegistry f c p -> ProofRegistry f c p
recordProofStepWith proofStepInput =
  recordProofStepByRetention (proofStepSummaryInput proofStepInput) proofStepInput

recordRetainedProofStep :: Ord c => ProofStepInput f c p -> ProofRegistry f c p -> ProofRegistry f c p
recordRetainedProofStep proofStepInput proofRegistry =
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

recordProofSummaryInput :: Ord c => ProofStepSummaryInput c -> ProofRegistry f c p -> ProofRegistry f c p
recordProofSummaryInput summaryInput proofRegistry =
  let nextStepId =
        succ (prNextProofStepId proofRegistry)
   in proofRegistry
        { prProofLog = Seq.empty,
          prProofIndex = NoProofIndex,
          prNextProofStepId = nextStepId,
          prFirstRetainedProofStepId = nextStepId,
          prTimestamp = prTimestamp proofRegistry + 1,
          prProofSummaryState = observeProofSummaryInput summaryInput (prProofSummaryState proofRegistry)
        }

data RetainedProofState f c p = RetainedProofState
  { rpsLog :: !(Seq (ProofStep f c p)),
    rpsIndex :: !ProofIndex,
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
          appendedIndex =
            insertStepIntoProofIndex proofStep (prProofIndex proofRegistry)
          dropCount =
            max 0 (Seq.length appendedLog - naturalToBoundedInt retainedBound)
          (droppedLog, trimmedLog) =
            Seq.splitAt dropCount appendedLog
          trimmedIndex =
            Foldable.foldl'
              (flip removeProofStepFromIndex)
              appendedIndex
              droppedLog
       in RetainedProofState
            { rpsLog = trimmedLog,
              rpsIndex = trimmedIndex,
              rpsFirstRetainedProofStepId =
                maybe (succ (psId proofStep)) psId (Seq.lookup 0 trimmedLog)
            }

droppedProofState :: ProofStep f c p -> RetainedProofState f c p
droppedProofState proofStep =
  RetainedProofState
    { rpsLog = Seq.empty,
      rpsIndex = NoProofIndex,
      rpsFirstRetainedProofStepId = succ (psId proofStep)
    }

emptyProofIndexForRetention :: ProofRetention -> ProofIndex
emptyProofIndexForRetention retention =
  case retention of
    KeepRecentProofSteps retained
      | retained > 0 ->
          RecentProofIndex UnboxedVector.empty
    KeepFullProof ->
      FullProofIndex IntMap.empty
    _ ->
      NoProofIndex

insertStepIntoProofIndex :: ProofStep f c p -> ProofIndex -> ProofIndex
insertStepIntoProofIndex proofStep proofIndex =
  case proofIndex of
    NoProofIndex ->
      NoProofIndex
    RecentProofIndex entries ->
      RecentProofIndex
        ( entries
            <> UnboxedVector.fromList
              [ (classIdKey classId, proofStepKey (psId proofStep))
                | classId <- proofStepEndpointClasses proofStep
              ]
        )
    FullProofIndex index ->
      FullProofIndex (insertStepIntoFullProofIndex proofStep index)

insertStepIntoFullProofIndex :: ProofStep f c p -> IntMap ProofStepIds -> IntMap ProofStepIds
insertStepIntoFullProofIndex proofStep index =
  Foldable.foldl'
    ( \currentIndex classId ->
        IntMap.alter
          ( Just
              . maybe
                (OneProofStepId (psId proofStep))
                (\proofStepIds -> appendProofStepId proofStepIds (psId proofStep))
          )
          (classIdKey classId)
          currentIndex
    )
    index
    (proofStepEndpointClasses proofStep)

appendProofStepId :: ProofStepIds -> ProofStepId -> ProofStepIds
appendProofStepId proofStepIds proofStepId =
  case proofStepIds of
    OneProofStepId existingProofStepId ->
      ManyProofStepIds (Seq.fromList [existingProofStepId, proofStepId])
    ManyProofStepIds existingProofStepIds ->
      ManyProofStepIds (existingProofStepIds |> proofStepId)

removeProofStepFromIndex :: ProofStep f c p -> ProofIndex -> ProofIndex
removeProofStepFromIndex proofStep proofIndex =
  case proofIndex of
    NoProofIndex ->
      NoProofIndex
    RecentProofIndex entries ->
      RecentProofIndex
        (UnboxedVector.filter ((/= proofStepKey (psId proofStep)) . snd) entries)
    FullProofIndex index ->
      FullProofIndex
        ( Foldable.foldl'
            ( \currentIndex classId ->
                IntMap.update
                  dropOldestProofStepId
                  (classIdKey classId)
                  currentIndex
            )
            index
            (proofStepEndpointClasses proofStep)
        )

dropOldestProofStepId :: ProofStepIds -> Maybe ProofStepIds
dropOldestProofStepId proofStepIds =
  case proofStepIds of
    OneProofStepId _ ->
      Nothing
    ManyProofStepIds existingProofStepIds ->
      case Seq.viewl (Seq.drop 1 existingProofStepIds) of
        Seq.EmptyL ->
          Nothing
        remainingProofStepId Seq.:< remainingProofStepIds
          | Seq.null remainingProofStepIds ->
              Just (OneProofStepId remainingProofStepId)
          | otherwise ->
              Just (ManyProofStepIds (remainingProofStepId Seq.<| remainingProofStepIds))

proofStepEndpointClasses :: ProofStep f c p -> [ClassId]
proofStepEndpointClasses proofStep =
  if psLhsClass proofStep == psRhsClass proofStep
    then [psLhsClass proofStep]
    else [psLhsClass proofStep, psRhsClass proofStep]

observeProofStep :: Ord c => ProofStep f c p -> ProofSummaryState c -> ProofSummaryState c
observeProofStep proofStep summary =
  summary
    { pssTotalSteps = pssTotalSteps summary + 1,
      pssClassPairs = insertClassPair (psLhsClass proofStep) (psRhsClass proofStep) (pssClassPairs summary),
      pssRewriteRules = insertProofKindRewriteRuleKeys (psKind proofStep) (pssRewriteRules summary),
      pssWitnessedSteps = pssWitnessedSteps summary + boolAsInt (hasWitness (psLhsWitness proofStep) || hasWitness (psRhsWitness proofStep)),
      pssGuardedSteps = pssGuardedSteps summary + boolAsInt (hasWitness (psGuardEvidence proofStep)),
      pssContextualSteps = pssContextualSteps summary + boolAsInt (hasWitness (psContextEvidence proofStep)),
      pssSupportAwareSteps = pssSupportAwareSteps summary + boolAsInt (hasWitness (psSupportEvidence proofStep)),
      pssUniqueSupports = maybeInsert (fmap sapeSupport (psSupportEvidence proofStep)) (pssUniqueSupports summary),
      pssFactfulSteps = pssFactfulSteps summary + boolAsInt (not (Set.null (psFactDerivations proofStep)))
    }

proofStepSummaryInput :: ProofStepInput f c p -> ProofStepSummaryInput c
proofStepSummaryInput proofStepInput =
  ProofStepSummaryInput
    { pssiProofKind = psiProofKind proofStepInput,
      pssiLhsClass = psiLhsClass proofStepInput,
      pssiRhsClass = psiRhsClass proofStepInput,
      pssiWitnessed = hasWitness (psiLhsWitness proofStepInput) || hasWitness (psiRhsWitness proofStepInput),
      pssiGuarded = hasWitness (psiGuardEvidence proofStepInput),
      pssiContextEvidence = psiContextEvidence proofStepInput,
      pssiSupportEvidence = psiSupportEvidence proofStepInput,
      pssiFactful = not (Set.null (psiFactDerivations proofStepInput))
    }

observeProofSummaryInput :: Ord c => ProofStepSummaryInput c -> ProofSummaryState c -> ProofSummaryState c
observeProofSummaryInput summaryInput summary =
  summary
    { pssTotalSteps = pssTotalSteps summary + 1,
      pssClassPairs = insertClassPair (pssiLhsClass summaryInput) (pssiRhsClass summaryInput) (pssClassPairs summary),
      pssRewriteRules = insertProofKindRewriteRuleKeys (pssiProofKind summaryInput) (pssRewriteRules summary),
      pssWitnessedSteps = pssWitnessedSteps summary + boolAsInt (pssiWitnessed summaryInput),
      pssGuardedSteps = pssGuardedSteps summary + boolAsInt (pssiGuarded summaryInput),
      pssContextualSteps = pssContextualSteps summary + boolAsInt (hasWitness (pssiContextEvidence summaryInput)),
      pssSupportAwareSteps = pssSupportAwareSteps summary + boolAsInt (hasWitness (pssiSupportEvidence summaryInput)),
      pssUniqueSupports = maybeInsert (fmap sapeSupport (pssiSupportEvidence summaryInput)) (pssUniqueSupports summary),
      pssFactfulSteps = pssFactfulSteps summary + boolAsInt (pssiFactful summaryInput)
    }

orderedClassPairKeys :: ClassId -> ClassId -> (Int, Int)
orderedClassPairKeys leftClass rightClass =
  let leftKey =
        classIdKey leftClass
      rightKey =
        classIdKey rightClass
   in if leftKey <= rightKey
        then (leftKey, rightKey)
        else (rightKey, leftKey)

emptyClassPairSet :: ClassPairSet
emptyClassPairSet =
  ClassPairSet 0 [] IntSet.empty Set.empty

insertClassPair :: ClassId -> ClassId -> ClassPairSet -> ClassPairSet
insertClassPair leftClass rightClass pairSet@(ClassPairSet pairCount packedChunks activePackedPairs widePairs) =
  let (leftKey, rightKey) =
        orderedClassPairKeys leftClass rightClass
   in case packClassPairKey leftKey rightKey of
        Just packedPair
          | packedClassPairMember packedPair packedChunks activePackedPairs ->
              pairSet
          | otherwise ->
              sealActiveClassPairs
                (ClassPairSet (pairCount + 1) packedChunks (IntSet.insert packedPair activePackedPairs) widePairs)
        Nothing
          | Set.member (ClassPair leftKey rightKey) widePairs ->
              pairSet
          | otherwise ->
              ClassPairSet
                (pairCount + 1)
                packedChunks
                activePackedPairs
                (Set.insert (ClassPair leftKey rightKey) widePairs)

classPairSetMember :: ClassId -> ClassId -> ClassPairSet -> Bool
classPairSetMember leftClass rightClass (ClassPairSet _pairCount packedChunks activePackedPairs widePairs) =
  let (leftKey, rightKey) =
        orderedClassPairKeys leftClass rightClass
   in case packClassPairKey leftKey rightKey of
        Just packedPair ->
          packedClassPairMember packedPair packedChunks activePackedPairs
        Nothing ->
          Set.member (ClassPair leftKey rightKey) widePairs

classPairSetSize :: ClassPairSet -> Int
classPairSetSize (ClassPairSet pairCount _packedChunks _activePackedPairs _widePairs) =
  pairCount

packedClassPairChunkSize :: Int
packedClassPairChunkSize =
  128

sealActiveClassPairs :: ClassPairSet -> ClassPairSet
sealActiveClassPairs pairSet@(ClassPairSet pairCount packedChunks activePackedPairs widePairs)
  | IntSet.size activePackedPairs < packedClassPairChunkSize =
      pairSet
  | otherwise =
      case (IntSet.lookupMin activePackedPairs, IntSet.lookupMax activePackedPairs) of
        (Just minimumPair, Just maximumPair) ->
          ClassPairSet
            pairCount
            ( PackedClassPairChunk
                minimumPair
                maximumPair
                (UnboxedVector.fromList (IntSet.toAscList activePackedPairs))
                : packedChunks
            )
            IntSet.empty
            widePairs
        _ ->
          pairSet

packedClassPairMember :: Int -> [PackedClassPairChunk] -> IntSet -> Bool
packedClassPairMember packedPair packedChunks activePackedPairs =
  IntSet.member packedPair activePackedPairs
    || any (packedClassPairChunkMember packedPair) packedChunks

packedClassPairChunkMember :: Int -> PackedClassPairChunk -> Bool
packedClassPairChunkMember target (PackedClassPairChunk minimumPair maximumPair values) =
  target >= minimumPair
    && target <= maximumPair
    && unboxedVectorMember target values

unboxedVectorMember :: Int -> UnboxedVector.Vector Int -> Bool
unboxedVectorMember target values =
  search 0 (UnboxedVector.length values - 1)
  where
    search lowerBound upperBound
      | lowerBound > upperBound =
          False
      | otherwise =
          let midpoint =
                lowerBound + (upperBound - lowerBound) `div` 2
           in case values UnboxedVector.!? midpoint of
                Nothing ->
                  False
                Just observed
                  | target < observed ->
                      search lowerBound (midpoint - 1)
                  | target > observed ->
                      search (midpoint + 1) upperBound
                  | otherwise ->
                      True

packClassPairKey :: Int -> Int -> Maybe Int
packClassPairKey leftKey rightKey
  | finiteBitSize leftKey < 64 =
      Nothing
  | leftKey < int32Minimum || leftKey > int32Maximum =
      Nothing
  | rightKey < int32Minimum || rightKey > int32Maximum =
      Nothing
  | otherwise =
      let leftWord =
            fromIntegral (fromIntegral leftKey :: Word32) :: Word64
          rightWord =
            fromIntegral (fromIntegral rightKey :: Word32) :: Word64
       in Just (fromIntegral ((leftWord `shiftL` 32) .|. rightWord))
  where
    int32Minimum =
      fromIntegral (minBound :: Int32)
    int32Maximum =
      fromIntegral (maxBound :: Int32)

insertProofKindRewriteRuleKeys :: ProofKind -> RewriteRuleKeySet -> RewriteRuleKeySet
insertProofKindRewriteRuleKeys proofKind existingKeys =
  case proofKind of
    ProofRewrite rewriteRuleId ->
      insertRewriteRuleKey (rewriteRuleIdKey rewriteRuleId) existingKeys
    ProofRewriteOrigin rewriteOrigin ->
      Set.foldl'
        (\rewriteRuleKeys rewriteRuleId -> insertRewriteRuleKey (rewriteRuleIdKey rewriteRuleId) rewriteRuleKeys)
        existingKeys
        (rewriteOriginRuleIds rewriteOrigin)
    ProofCongruence ->
      existingKeys
    ProofAnalysis ->
      existingKeys

insertRewriteRuleKey :: Int -> RewriteRuleKeySet -> RewriteRuleKeySet
insertRewriteRuleKey rewriteRuleKey rewriteRuleKeys =
  case rewriteRuleKeys of
    NoRewriteRuleKeys ->
      OneRewriteRuleKey rewriteRuleKey
    OneRewriteRuleKey existingKey
      | rewriteRuleKey == existingKey ->
          rewriteRuleKeys
      | rewriteRuleKey < existingKey ->
          TwoRewriteRuleKeys rewriteRuleKey existingKey
      | otherwise ->
          TwoRewriteRuleKeys existingKey rewriteRuleKey
    TwoRewriteRuleKeys firstKey secondKey
      | rewriteRuleKey == firstKey || rewriteRuleKey == secondKey ->
          rewriteRuleKeys
      | otherwise ->
          ManyRewriteRuleKeys (IntSet.fromList [firstKey, secondKey, rewriteRuleKey])
    ManyRewriteRuleKeys existingKeySet ->
      ManyRewriteRuleKeys (IntSet.insert rewriteRuleKey existingKeySet)

rewriteRuleKeySetSize :: RewriteRuleKeySet -> Int
rewriteRuleKeySetSize rewriteRuleKeys =
  case rewriteRuleKeys of
    NoRewriteRuleKeys ->
      0
    OneRewriteRuleKey _ ->
      1
    TwoRewriteRuleKeys _ _ ->
      2
    ManyRewriteRuleKeys existingKeySet ->
      IntSet.size existingKeySet

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
  classPairSetMember leftClass rightClass
    . pssClassPairs
    . prProofSummaryState

findProofStepBetween :: ClassId -> ClassId -> ProofRegistry f c p -> Maybe (ProofStep f c p)
findProofStepBetween leftClass rightClass =
  Foldable.find (proofStepConnects leftClass rightClass)
    . proofStepCandidatesForClass leftClass

proofStepCandidatesForClass :: ClassId -> ProofRegistry f c p -> [ProofStep f c p]
proofStepCandidatesForClass classId proofRegistry =
  mapMaybe
    (resolveProofStepId proofRegistry)
    (proofStepIdsForClass classId (prProofIndex proofRegistry))

proofStepIdsForClass :: ClassId -> ProofIndex -> [ProofStepId]
proofStepIdsForClass classId proofIndex =
  case proofIndex of
    NoProofIndex ->
      []
    RecentProofIndex entries ->
      UnboxedVector.foldr
        ( \(observedClassKey, observedProofStepKey) proofStepIds ->
            if observedClassKey == classIdKey classId
              then ProofStepId observedProofStepKey : proofStepIds
              else proofStepIds
        )
        []
        entries
    FullProofIndex index ->
      maybe
        []
        proofStepIdsToList
        (IntMap.lookup (classIdKey classId) index)

proofStepIdsToList :: ProofStepIds -> [ProofStepId]
proofStepIdsToList proofStepIds =
  case proofStepIds of
    OneProofStepId proofStepId ->
      [proofStepId]
    ManyProofStepIds existingProofStepIds ->
      Foldable.toList existingProofStepIds

resolveProofStepId :: ProofRegistry f c p -> ProofStepId -> Maybe (ProofStep f c p)
resolveProofStepId proofRegistry proofStepId =
  let retainedOffset =
        proofStepKey proofStepId - proofStepKey (prFirstRetainedProofStepId proofRegistry)
   in if retainedOffset < 0
        then Nothing
        else
          Seq.lookup retainedOffset (prProofLog proofRegistry)
            >>= \proofStep ->
              -- Deliberate tripwire for the contiguous-ascending-id registry invariant, not a correctness requirement.
              if psId proofStep == proofStepId
                then Just proofStep
                else Nothing

requireDirectProofQueryAvailable :: ProofRegistry f c p -> Either ProofQueryError ()
requireDirectProofQueryAvailable =
  requireProofRetention proofRetentionStoresAnyLog

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
      fullProofClassIndex (prProofIndex proofRegistry)
{-# INLINE proofReachability #-}

fullProofClassIndex :: ProofIndex -> IntMap ProofStepIds
fullProofClassIndex proofIndex =
  case proofIndex of
    FullProofIndex index ->
      index
    _ ->
      IntMap.empty

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
        classPairSetSize (pssClassPairs summary)
   in ProofCompressionSummary
        { pcsTotalSteps = totalSteps,
          pcsUniqueClassPairs = uniqueClassPairs,
          pcsUniqueRewriteRules = rewriteRuleKeySetSize (pssRewriteRules summary),
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
requireProofLogAvailable =
  requireUnprunedProofRetention proofRetentionStoresAnyLog

requireFullProofAvailable :: ProofRegistry f c p -> Either ProofQueryError ()
requireFullProofAvailable =
  requireUnprunedProofRetention proofRetentionStoresFullLog

requireUnprunedProofRetention ::
  (ProofRetention -> Bool) ->
  ProofRegistry f c p ->
  Either ProofQueryError ()
requireUnprunedProofRetention acceptsRetention proofRegistry = do
  requireProofRetention acceptsRetention proofRegistry
  if proofRegistryDroppedStepCount proofRegistry == 0
    then Right ()
    else Left ProofPruned

requireProofRetention ::
  (ProofRetention -> Bool) ->
  ProofRegistry f c p ->
  Either ProofQueryError ()
requireProofRetention acceptsRetention proofRegistry =
  let retention = prProofRetention proofRegistry
   in if acceptsRetention retention
        then Right ()
        else Left (ProofUnavailableForRetention retention)

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
