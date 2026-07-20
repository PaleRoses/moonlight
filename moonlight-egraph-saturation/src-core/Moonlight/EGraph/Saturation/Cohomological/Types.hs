{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Cohomological.Types
  ( cachePolicyFromEnvironmentFingerprint,
    PatternOccurrence (..),
    SheafCapabilityLabel (..),
    SheafCapabilityAtom,
    mkSheafCapabilityLabel,
    mkSheafCapabilityEnvironment,
    TypedCapabilitySupport (..),
    EqualityModalityEnvironment (..),
    GuardModalityEnvironment (..),
    FactModalityEnvironment (..),
    ProofModalityEnvironment (..),
    CapabilityModalityEnvironment,
    EGraphSectionCertification,
    SheafModalityKey,
    data EqualityModalityKey,
    data GuardModalityKey,
    data FactModalityKey,
    data ProofModalityKey,
    data CapabilityModalityKey,
    sheafMatchingRequestEnvironmentAlgebra,
    sheafEnvironmentAlgebra,
    refineSheafRegion,
    sheafEnvironmentFingerprintFor,
    mixFingerprint,
  )
where

import Data.Dependent.Sum (DSum ((:=>)))
import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Core (Language)
import Moonlight.EGraph.Introspection.Core.HsExpr (ScopeCtx)
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqPrimaryPattern,
    cpqQuery,
    patternQueryPatterns,
  )
import Moonlight.Rewrite.System (CompiledGuard, GuardAtom)
import Moonlight.Core
  ( Pattern,
    PatternVar,
    patternVarKey
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchSite,
    MatchingRequest,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.EGraph.Pure.Types (ClassId (..))
import Moonlight.EGraph.Pure.Types
  ( classIdKey,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Modality.Standard qualified as Standard
import Moonlight.Sheaf.Obstruction hiding
  ( data CapabilityModalityKey,
    data EqualityModalityKey,
    data FactModalityKey,
    data GuardModalityKey,
    data ProofModalityKey,
    SheafModalityKey
  )

cachePolicyFromEnvironmentFingerprint :: MatchSite c -> Maybe EnvironmentCacheKey -> CachePolicy
cachePolicyFromEnvironmentFingerprint matchSite maybeEnvironmentFingerprint =
  case maybeEnvironmentFingerprint of
    Just environmentFingerprint ->
      EnvironmentScoped environmentFingerprint
    Nothing ->
      case matchSite of
        GenericMatching.BaseSite -> SharedAcrossEnvironments
        GenericMatching.ContextSite _ -> DoNotCache

type PatternOccurrence :: (Type -> Type) -> Type
data PatternOccurrence f = PatternOccurrence
  { poId :: !OccurrenceId,
    poPath :: ![Int],
    poPattern :: !(Pattern f),
    poBoundVariable :: !(Maybe PatternVar)
  }

type EqualityModalityEnvironment :: (Type -> Type) -> Type
data EqualityModalityEnvironment f = EqualityModalityEnvironment
  { emeOccurrences :: ![PatternOccurrence f],
    emeOccurrenceDomains :: !(Map OccurrenceId IntSet)
  }

type GuardModalityEnvironment :: (Type -> Type) -> Type
data GuardModalityEnvironment f = GuardModalityEnvironment
  { gmeRootKey :: !Int,
    gmeGuardAtoms :: ![GuardAtom SheafCapabilityAtom f],
    gmeOccurrenceDomains :: !(Map OccurrenceId IntSet),
    gmeGuardDomains :: !(Map (GuardAtom SheafCapabilityAtom f) IntSet),
    gmeRepresentativeAnchors :: !(Map Int (Anchor OccurrenceId))
  }

type FactModalityEnvironment :: Type -> Type -> (Type -> Type) -> Type -> Type
data FactModalityEnvironment owner c f runtime = FactModalityEnvironment
  { fmeRequest :: !(MatchingRequest owner c SheafCapabilityAtom f runtime),
    fmeGuardEnvironment :: !(GuardModalityEnvironment f)
  }

type ProofModalityEnvironment :: Type -> Type -> (Type -> Type) -> Type -> Type
data ProofModalityEnvironment owner c f runtime = ProofModalityEnvironment
  { pmeRequest :: !(MatchingRequest owner c SheafCapabilityAtom f runtime),
    pmeGuardEnvironment :: !(GuardModalityEnvironment f)
  }

type SheafCapabilityLabel :: Type
newtype SheafCapabilityLabel = SheafCapabilityLabel
  { unSheafCapabilityLabel :: CapabilityRow SheafCapabilityAtom
  }
  deriving stock (Eq, Ord, Show, Read)

type SheafCapabilityAtom :: Type
type SheafCapabilityAtom = ScopeCtx

mkSheafCapabilityLabel :: [SheafCapabilityAtom] -> SheafCapabilityLabel
mkSheafCapabilityLabel =
  SheafCapabilityLabel
    . capabilityRowFromList

mkSheafCapabilityEnvironment ::
  [SheafCapabilityAtom] ->
  [TypedCapabilitySupport SheafCapabilityLabel (Anchor OccurrenceId)] ->
  CapabilityModalityEnvironment OccurrenceId
mkSheafCapabilityEnvironment capabilityUniverse capabilitySupports =
  let typedUniverse =
        capabilityUniverse
      rowAlgebra =
        finiteCapabilityRowAlgebra typedUniverse
   in TypedCapabilityEnvironment
        ( mapCapabilityLabelAlgebra
            unSheafCapabilityLabel
            SheafCapabilityLabel
            rowAlgebra
        )
        capabilitySupports

type CapabilityModalityEnvironment :: Type -> Type
type CapabilityModalityEnvironment occurrence =
  TypedCapabilityEnvironment SheafCapabilityLabel (Anchor occurrence)

type EGraphSectionCertification :: Type -> Type -> (Type -> Type) -> Type
type EGraphSectionCertification owner c f =
  SectionCertificationAlgebra
    (MatchingRequest owner c SheafCapabilityAtom f)
    (Pattern f)
    (PatternOccurrence f)
    (GuardAtom SheafCapabilityAtom f)
    (CandidateRegion ClassId)
    CandidateStalk
    (CapabilityModalityEnvironment OccurrenceId)
    ()

type SheafModalityKey :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
type SheafModalityKey owner c f =
  Standard.SheafModalityKey
    (EqualityModalityEnvironment f)
    (GuardModalityEnvironment f)
    (FactModalityEnvironment owner c f)
    (ProofModalityEnvironment owner c f)
    (CapabilityModalityEnvironment OccurrenceId)

pattern EqualityModalityKey ::
  SheafModalityKey owner c f runtime (EqualityModalityEnvironment f)
pattern EqualityModalityKey =
  Standard.EqualityModalityKey

pattern GuardModalityKey ::
  SheafModalityKey owner c f runtime (GuardModalityEnvironment f)
pattern GuardModalityKey =
  Standard.GuardModalityKey

pattern FactModalityKey ::
  SheafModalityKey owner c f runtime (FactModalityEnvironment owner c f runtime)
pattern FactModalityKey =
  Standard.FactModalityKey

pattern ProofModalityKey ::
  SheafModalityKey owner c f runtime (ProofModalityEnvironment owner c f runtime)
pattern ProofModalityKey =
  Standard.ProofModalityKey

pattern CapabilityModalityKey ::
  SheafModalityKey owner c f runtime (CapabilityModalityEnvironment OccurrenceId)
pattern CapabilityModalityKey =
  Standard.CapabilityModalityKey

{-# COMPLETE
  EqualityModalityKey,
  GuardModalityKey,
  FactModalityKey,
  ProofModalityKey,
  CapabilityModalityKey
  #-}

sheafEnvironmentAlgebra ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  ObstructionEnvironmentAlgebra
    (MatchingRequest owner c SheafCapabilityAtom f)
    (SheafModalityKey owner c f)
    (CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f)
    (PatternOccurrence f)
    (GuardAtom SheafCapabilityAtom f)
    (CandidateRegion ClassId)
sheafEnvironmentAlgebra context canonicalize =
  ObstructionEnvironmentAlgebra
    { oeaCollectOccurrences = collectQueryOccurrences (socCollectOccurrences context),
      oeaEnumerateRegions =
        \_request _compiledQuery -> [],
      oeaRefineRegion =
        \request compiledQuery regionValue ->
          socRefineRegion
            context
            request
            (cpqPrimaryPattern compiledQuery)
            regionValue,
      oeaIndexedEnvironmentAlgebra = sheafMatchingRequestEnvironmentAlgebra context canonicalize,
      oeaQueryFingerprint = queryFingerprintFor (socPatternFingerprint context),
      oeaEnvironmentFingerprint = socEnvironmentFingerprint context
    }

sheafMatchingRequestEnvironmentAlgebra ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  IndexedEnvironmentAlgebra
    (MatchingRequest owner c SheafCapabilityAtom f runtime)
    (CandidateRegion ClassId)
    (PatternOccurrence f)
    (GuardAtom SheafCapabilityAtom f)
    (SheafModalityKey owner c f runtime)
sheafMatchingRequestEnvironmentAlgebra context canonicalize =
  indexedEnvironmentAlgebraFromList
    [ EqualityModalityKey :=> IndexedEnvironmentBuilder
          (buildEqualityModalityEnvironment context canonicalize),
      GuardModalityKey :=> IndexedEnvironmentBuilder
          (buildGuardModalityEnvironment context canonicalize),
      FactModalityKey :=> IndexedEnvironmentBuilder
          (buildFactModalityEnvironment context canonicalize),
      ProofModalityKey :=> IndexedEnvironmentBuilder
          (buildProofModalityEnvironment context canonicalize),
      CapabilityModalityKey :=> IndexedEnvironmentBuilder
          (buildCapabilityModalityEnvironment context)
    ]

refineSheafRegion ::
  EGraphSectionCertification owner c f ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f ->
  CandidateRegion ClassId ->
  [CandidateRegion ClassId]
refineSheafRegion context request compiledQuery =
  socRefineRegion
    context
    request
    (cpqPrimaryPattern compiledQuery)

sheafEnvironmentFingerprintFor ::
  EGraphSectionCertification owner c f ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Maybe Int
sheafEnvironmentFingerprintFor context =
  socEnvironmentFingerprint context

buildEqualityModalityEnvironment ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  EqualityModalityEnvironment f
buildEqualityModalityEnvironment context canonicalize request regionValue occurrences _ =
  let sharedEnvironment = buildSharedSheafEnvironment context canonicalize request regionValue occurrences []
   in EqualityModalityEnvironment
        { emeOccurrences = occurrences,
          emeOccurrenceDomains = sseOccurrenceDomains sharedEnvironment
        }

buildGuardModalityEnvironment ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  GuardModalityEnvironment f
buildGuardModalityEnvironment context canonicalize request regionValue occurrences guardAtoms =
  let sharedEnvironment = buildSharedSheafEnvironment context canonicalize request regionValue occurrences guardAtoms
   in GuardModalityEnvironment
        { gmeRootKey = sseRootKey sharedEnvironment,
          gmeGuardAtoms = guardAtoms,
          gmeOccurrenceDomains = sseOccurrenceDomains sharedEnvironment,
          gmeGuardDomains = sseGuardDomains sharedEnvironment,
          gmeRepresentativeAnchors = sseRepresentativeAnchors sharedEnvironment
        }

buildFactModalityEnvironment ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  FactModalityEnvironment owner c f runtime
buildFactModalityEnvironment context canonicalize request regionValue occurrences guardAtoms =
  FactModalityEnvironment
    { fmeRequest = request,
      fmeGuardEnvironment = buildGuardModalityEnvironment context canonicalize request regionValue occurrences guardAtoms
    }

buildProofModalityEnvironment ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  ProofModalityEnvironment owner c f runtime
buildProofModalityEnvironment context canonicalize request regionValue occurrences guardAtoms =
  ProofModalityEnvironment
    { pmeRequest = request,
      pmeGuardEnvironment = buildGuardModalityEnvironment context canonicalize request regionValue occurrences guardAtoms
    }

buildCapabilityModalityEnvironment ::
  EGraphSectionCertification owner c f ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  CapabilityModalityEnvironment OccurrenceId
buildCapabilityModalityEnvironment context request regionValue occurrences guardAtoms =
  socCapabilityEnvironment context request regionValue occurrences guardAtoms

type SharedSheafEnvironment :: (Type -> Type) -> Type
data SharedSheafEnvironment f = SharedSheafEnvironment
  { sseRootKey :: !Int,
    sseOccurrenceDomains :: !(Map OccurrenceId IntSet),
    sseGuardDomains :: !(Map (GuardAtom SheafCapabilityAtom f) IntSet),
    sseRepresentativeAnchors :: !(Map Int (Anchor OccurrenceId))
  }

buildSharedSheafEnvironment ::
  Language f =>
  EGraphSectionCertification owner c f ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [PatternOccurrence f] ->
  [GuardAtom SheafCapabilityAtom f] ->
  SharedSheafEnvironment f
buildSharedSheafEnvironment context canonicalize request regionValue occurrences guardAtoms =
  let rootKey = canonicalClassKey canonicalize (crRoot regionValue)
      occurrenceDomains =
        Map.fromList
          ( fmap
              (\occurrenceValue ->
                 ( poId occurrenceValue,
                   canonicalizeStalk canonicalize (socOccurrenceDomain context request occurrenceValue regionValue)
                 )
              )
              occurrences
          )
      guardDomains =
        Map.fromList
          ( fmap
              ( \guardAtom ->
                  (guardAtom, canonicalizeStalk canonicalize (socGuardDomain context request guardAtom regionValue))
              )
              guardAtoms
          )
   in SharedSheafEnvironment
        { sseRootKey = rootKey,
          sseOccurrenceDomains = occurrenceDomains,
          sseGuardDomains = guardDomains,
          sseRepresentativeAnchors = representativeAnchorMap occurrences
        }

representativeAnchorMap :: [PatternOccurrence f] -> Map Int (Anchor OccurrenceId)
representativeAnchorMap =
  Map.fromListWith (flip const)
    . mapMaybe
      (\occurrenceValue ->
         fmap
           (\patternVar -> (patternVarKey patternVar, OccurrenceAnchor (poId occurrenceValue)))
           (poBoundVariable occurrenceValue)
      )

canonicalizeStalk :: (ClassId -> ClassId) -> CandidateStalk -> IntSet
canonicalizeStalk canonicalize (CandidateStalk supportSet) =
  supportSet
    & IntSet.toAscList
    & fmap (canonicalMemberKey canonicalize)
    & IntSet.fromList

canonicalClassKey :: (ClassId -> ClassId) -> ClassId -> Int
canonicalClassKey canonicalize =
  classIdKey . canonicalize

canonicalMemberKey :: (ClassId -> ClassId) -> Int -> Int
canonicalMemberKey canonicalize =
  canonicalClassKey canonicalize . ClassId

collectQueryOccurrences ::
  (Pattern f -> [PatternOccurrence f]) ->
  CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f ->
  [PatternOccurrence f]
collectQueryOccurrences collectOccurrences compiledQuery =
  patternQueryPatterns (cpqQuery compiledQuery)
    & foldMap collectOccurrences
    & zipWith reindexOccurrence [0 :: Int ..]

reindexOccurrence :: Int -> PatternOccurrence f -> PatternOccurrence f
reindexOccurrence occurrenceIndex occurrenceValue =
  occurrenceValue {poId = OccurrenceId occurrenceIndex}

mixFingerprint :: Int -> Int -> Int
mixFingerprint seed nextValue =
  seed * 16777619 + nextValue + 97

queryFingerprintFor :: (Pattern f -> Int) -> CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f -> Int
queryFingerprintFor patternFingerprint =
  List.foldl'
    mixFingerprint
    146959810
    . fmap patternFingerprint
    . patternQueryPatterns
    . cpqQuery
