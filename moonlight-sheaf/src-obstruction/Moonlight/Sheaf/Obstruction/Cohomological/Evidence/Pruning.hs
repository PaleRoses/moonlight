{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( PruningEvidence (..),
    PruningEvidenceFamily (..),
    CohomologicalPruningFootprint (..),
    CohomologicalPruningCertificate,
    RetainedCohomologicalRegion (..),
    CohomologicalPruningObstruction (..),
    CohomologicalPruningGates (..),
    gatesFromEvidence,
    buildPruningGates,
    seedPruningGate,
    regionPruningGate,
    keepSeed,
    keepRegion,
    retainedRegionFromDecision,
    retainRegion,
    seedFootprint,
    regionFootprint,
    cohomologicalFootprintMeasures,
    prunedSeeds,
    prunedRegions,
    pruningEvidenceFamily,
    pruningEvidenceFamilyEvidence,
    pruningFamilyNonCriticalNodes,
    pruningFamilyRelevantOrdinals,
    pruningFamilyObstructedNodes,
    microsupportResultPruningEvidence,
  )
where

import Data.Functor.Identity (Identity, runIdentity)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core (RegionNodeId (..))
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( CandidateRegion (crMembers, crNodeId),
    CandidateRegionSeed (crsContextOrdinal, crsNodeId),
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure,
    FootprintMeasureBasis (..),
    FootprintMeasureUnit (..),
    exactRepresentedFootprintMeasure,
  )
import Moonlight.Sheaf.Pruning
  ( PruningCertificate,
    PruningDecision (..),
    PruningGate,
    PruningReport,
    pruningDecisionAllowed,
    pruningDecisionFootprint,
    pruningDecisionFromVerdict,
    pruningDecisionRejectedList,
    pruneWithGate,
    purePruningGate,
    rejectedPruningDecision,
  )
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    Verdict (..),
    rejectedFromList,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( WitnessClass (..),
  )

type PruningEvidence :: Type
data PruningEvidence
  = MicrosupportNonCritical (Set.Set RegionNodeId)
  | ContextRelevant (Set.Set Int)
  | WitnessClassification (Map.Map RegionNodeId WitnessClass)

type PruningEvidenceFamily :: Type
data PruningEvidenceFamily = PruningEvidenceFamily
  { pefMicrosupportNonCritical :: ![Set.Set RegionNodeId],
    pefContextRelevant :: ![Set.Set Int],
    pefWitnessClassifications :: ![Map.Map RegionNodeId WitnessClass]
  }
  deriving stock (Eq, Show)

emptyPruningEvidenceFamily :: PruningEvidenceFamily
emptyPruningEvidenceFamily =
  PruningEvidenceFamily
    { pefMicrosupportNonCritical = [],
      pefContextRelevant = [],
      pefWitnessClassifications = []
    }

pruningEvidenceFamily :: [PruningEvidence] -> PruningEvidenceFamily
pruningEvidenceFamily =
  foldl' insertEvidence emptyPruningEvidenceFamily
  where
    insertEvidence familyValue evidence =
      case evidence of
        MicrosupportNonCritical nonCriticalNodes ->
          familyValue
            { pefMicrosupportNonCritical =
                nonCriticalNodes : pefMicrosupportNonCritical familyValue
            }
        ContextRelevant relevantOrdinals ->
          familyValue
            { pefContextRelevant =
                relevantOrdinals : pefContextRelevant familyValue
            }
        WitnessClassification witnessByNode ->
          familyValue
            { pefWitnessClassifications =
                witnessByNode : pefWitnessClassifications familyValue
            }

pruningEvidenceFamilyEvidence :: PruningEvidenceFamily -> [PruningEvidence]
pruningEvidenceFamilyEvidence familyValue =
  fmap MicrosupportNonCritical (pefMicrosupportNonCritical familyValue)
    <> fmap ContextRelevant (pefContextRelevant familyValue)
    <> fmap WitnessClassification (pefWitnessClassifications familyValue)

pruningFamilyNonCriticalNodes :: PruningEvidenceFamily -> Set.Set RegionNodeId
pruningFamilyNonCriticalNodes =
  Set.unions . pefMicrosupportNonCritical

pruningFamilyRelevantOrdinals :: PruningEvidenceFamily -> Maybe (Set.Set Int)
pruningFamilyRelevantOrdinals familyValue =
  case pefContextRelevant familyValue of
    [] ->
      Nothing
    firstRelevantSet : remainingRelevantSets ->
      Just (foldl' Set.intersection firstRelevantSet remainingRelevantSets)

pruningFamilyObstructedNodes :: PruningEvidenceFamily -> Set.Set RegionNodeId
pruningFamilyObstructedNodes =
  Set.unions . fmap obstructedWitnessNodes . pefWitnessClassifications

type CohomologicalPruningObstruction :: Type
data CohomologicalPruningObstruction
  = MicrosupportNonCriticalObstruction !RegionNodeId
  | ContextIrrelevantObstruction !Int
  | WitnessObstructedObstruction !RegionNodeId
  deriving stock (Eq, Ord, Show, Read)

type CohomologicalPruningFootprint :: Type
data CohomologicalPruningFootprint = CohomologicalPruningFootprint
  { cpfSeedNodes :: !(Set.Set RegionNodeId),
    cpfRegionNodes :: !(Set.Set RegionNodeId),
    cpfContextOrdinals :: !(Set.Set Int),
    cpfRegionMembers :: !IntSet,
    cpfCandidateSeedCount :: !Natural,
    cpfCandidateRegionCount :: !Natural
  }
  deriving stock (Eq, Show, Read)

instance Semigroup CohomologicalPruningFootprint where
  left <> right =
    CohomologicalPruningFootprint
      { cpfSeedNodes = Set.union (cpfSeedNodes left) (cpfSeedNodes right),
        cpfRegionNodes = Set.union (cpfRegionNodes left) (cpfRegionNodes right),
        cpfContextOrdinals = Set.union (cpfContextOrdinals left) (cpfContextOrdinals right),
        cpfRegionMembers = IntSet.union (cpfRegionMembers left) (cpfRegionMembers right),
        cpfCandidateSeedCount = cpfCandidateSeedCount left + cpfCandidateSeedCount right,
        cpfCandidateRegionCount = cpfCandidateRegionCount left + cpfCandidateRegionCount right
      }

instance Monoid CohomologicalPruningFootprint where
  mempty =
    CohomologicalPruningFootprint
      { cpfSeedNodes = Set.empty,
        cpfRegionNodes = Set.empty,
        cpfContextOrdinals = Set.empty,
        cpfRegionMembers = IntSet.empty,
        cpfCandidateSeedCount = 0,
        cpfCandidateRegionCount = 0
      }

type CohomologicalPruningCertificate :: Type
type CohomologicalPruningCertificate =
  PruningCertificate CohomologicalPruningFootprint () CohomologicalPruningObstruction

type RetainedCohomologicalRegion :: Type -> Type
data RetainedCohomologicalRegion root = RetainedCohomologicalRegion
  { rcrRegion :: !(CandidateRegion root),
    rcrFootprint :: !CohomologicalPruningFootprint
  }
  deriving stock (Eq, Show)

seedFootprint :: CandidateRegionSeed root -> CohomologicalPruningFootprint
seedFootprint seedValue =
  CohomologicalPruningFootprint
    { cpfSeedNodes = Set.singleton (crsNodeId seedValue),
      cpfRegionNodes = Set.empty,
      cpfContextOrdinals =
        maybe Set.empty Set.singleton (crsContextOrdinal seedValue),
      cpfRegionMembers = IntSet.empty,
      cpfCandidateSeedCount = 1,
      cpfCandidateRegionCount = 0
    }

regionFootprint :: CandidateRegion root -> CohomologicalPruningFootprint
regionFootprint regionValue =
  CohomologicalPruningFootprint
    { cpfSeedNodes = Set.empty,
      cpfRegionNodes =
        maybe Set.empty Set.singleton (crNodeId regionValue),
      cpfContextOrdinals = Set.empty,
      cpfRegionMembers = crMembers regionValue,
      cpfCandidateSeedCount = 0,
      cpfCandidateRegionCount = 1
    }

cohomologicalFootprintMeasures :: CohomologicalPruningFootprint -> [FootprintMeasure Natural]
cohomologicalFootprintMeasures footprint =
  foldMap
    measuredPositive
    [ (CandidateSeedUnit, RepresentedCandidateCarrier, cpfCandidateSeedCount footprint),
      (CandidateRegionUnit, RepresentedCandidateCarrier, cpfCandidateRegionCount footprint),
      (RegionNodeUnit, NormalizedSetCarrier, fromIntegral (Set.size (Set.union (cpfSeedNodes footprint) (cpfRegionNodes footprint)))),
      (ContextOrdinalUnit, NormalizedSetCarrier, fromIntegral (Set.size (cpfContextOrdinals footprint))),
      (RegionNodeUnit, NormalizedIntSetCarrier, fromIntegral (IntSet.size (cpfRegionMembers footprint)))
    ]
  where
    measuredPositive (unitValue, basisValue, countValue)
      | countValue == 0 = []
      | otherwise = [exactRepresentedFootprintMeasure unitValue basisValue countValue]

combineSameCarrierDecision ::
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction ->
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction ->
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction
combineSameCarrierDecision left right =
  case NonEmpty.nonEmpty (pruningDecisionRejectedList left <> pruningDecisionRejectedList right) of
    Nothing ->
      PruningAccepted (pruningDecisionFootprint left)
    Just obstructions ->
      rejectedPruningDecision (pruningDecisionFootprint left) Nothing obstructions

type CohomologicalPruningGates :: Type -> Type
data CohomologicalPruningGates root = CohomologicalPruningGates
  { cpgSeedDecision ::
      CandidateRegionSeed root ->
      PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction,
    cpgRegionDecision ::
      CandidateRegion root ->
      PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction
  }

instance Semigroup (CohomologicalPruningGates root) where
  a <> b =
    CohomologicalPruningGates
      { cpgSeedDecision = \seedValue ->
          combineSameCarrierDecision
            (cpgSeedDecision a seedValue)
            (cpgSeedDecision b seedValue),
        cpgRegionDecision = \regionValue ->
          combineSameCarrierDecision
            (cpgRegionDecision a regionValue)
            (cpgRegionDecision b regionValue)
      }

instance Monoid (CohomologicalPruningGates root) where
  mempty =
    CohomologicalPruningGates
      { cpgSeedDecision = PruningAccepted . seedFootprint,
        cpgRegionDecision = PruningAccepted . regionFootprint
      }

gatesFromEvidence :: PruningEvidence -> CohomologicalPruningGates root
gatesFromEvidence =
  gatesFromAccumulator . accumulateEvidence emptyPruningAccumulator

buildPruningGates :: [PruningEvidence] -> CohomologicalPruningGates root
buildPruningGates =
  gatesFromAccumulator . foldl' accumulateEvidence emptyPruningAccumulator

seedPruningGate ::
  CohomologicalPruningGates root ->
  PruningGate Identity (CandidateRegionSeed root) CohomologicalPruningFootprint () CohomologicalPruningObstruction
seedPruningGate pruningGates =
  purePruningGate (cpgSeedDecision pruningGates)

regionPruningGate ::
  CohomologicalPruningGates root ->
  PruningGate Identity (CandidateRegion root) CohomologicalPruningFootprint () CohomologicalPruningObstruction
regionPruningGate pruningGates =
  purePruningGate (cpgRegionDecision pruningGates)

keepSeed ::
  CohomologicalPruningGates root ->
  CandidateRegionSeed root ->
  Bool
keepSeed pruningGates seedValue =
  pruningDecisionAllowed (cpgSeedDecision pruningGates seedValue)
{-# INLINE keepSeed #-}

keepRegion ::
  CohomologicalPruningGates root ->
  CandidateRegion root ->
  Bool
keepRegion pruningGates regionValue =
  pruningDecisionAllowed (cpgRegionDecision pruningGates regionValue)
{-# INLINE keepRegion #-}

retainedRegionFromDecision ::
  CandidateRegion root ->
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction ->
  Maybe (RetainedCohomologicalRegion root)
retainedRegionFromDecision regionValue decision =
  case decision of
    PruningAccepted footprint ->
      Just
        RetainedCohomologicalRegion
          { rcrRegion = regionValue,
            rcrFootprint = footprint
          }
    PruningRejected _ ->
      Nothing

retainRegion ::
  CohomologicalPruningGates root ->
  CandidateRegion root ->
  Maybe (RetainedCohomologicalRegion root)
retainRegion pruningGates regionValue =
  retainedRegionFromDecision regionValue (cpgRegionDecision pruningGates regionValue)

prunedSeeds ::
  CohomologicalPruningGates root ->
  [CandidateRegionSeed root] ->
  PruningReport (CandidateRegionSeed root) CohomologicalPruningFootprint () CohomologicalPruningObstruction
prunedSeeds pruningGates =
  runIdentity . pruneWithGate (seedPruningGate pruningGates)

prunedRegions ::
  CohomologicalPruningGates root ->
  [CandidateRegion root] ->
  PruningReport (CandidateRegion root) CohomologicalPruningFootprint () CohomologicalPruningObstruction
prunedRegions pruningGates =
  runIdentity . pruneWithGate (regionPruningGate pruningGates)

microsupportResultPruningEvidence :: MicrosupportResult -> [PruningEvidence]
microsupportResultPruningEvidence microsupportResult =
  evidenceFromNonCriticalNodes
    (foldMap nonCriticalNode (mrCriticalFibers microsupportResult))
  where
    nonCriticalNode (FinObjectId ordinalValue, NonCritical) =
      Set.singleton (RegionNodeId ordinalValue)
    nonCriticalNode _ =
      Set.empty

    evidenceFromNonCriticalNodes nonCriticalNodes
      | Set.null nonCriticalNodes = []
      | otherwise = [MicrosupportNonCritical nonCriticalNodes]

type PruningAccumulator :: Type
data PruningAccumulator = PruningAccumulator
  { paNonCriticalNodes :: Set.Set RegionNodeId,
    paRelevantOrdinals :: Maybe (Set.Set Int),
    paObstructedNodes :: Set.Set RegionNodeId
  }

emptyPruningAccumulator :: PruningAccumulator
emptyPruningAccumulator =
  PruningAccumulator
    { paNonCriticalNodes = Set.empty,
      paRelevantOrdinals = Nothing,
      paObstructedNodes = Set.empty
    }

accumulateEvidence :: PruningAccumulator -> PruningEvidence -> PruningAccumulator
accumulateEvidence accumulator evidence =
  case evidence of
    MicrosupportNonCritical nonCriticalNodes ->
      accumulator
        { paNonCriticalNodes =
            Set.union nonCriticalNodes (paNonCriticalNodes accumulator)
        }
    ContextRelevant relevantOrdinals ->
      accumulator
        { paRelevantOrdinals =
            Just
              ( maybe
                  relevantOrdinals
                  (Set.intersection relevantOrdinals)
                  (paRelevantOrdinals accumulator)
              )
        }
    WitnessClassification witnessByNode ->
      accumulator
        { paObstructedNodes =
            Set.union
              (obstructedWitnessNodes witnessByNode)
              (paObstructedNodes accumulator)
        }

obstructedWitnessNodes :: Map.Map RegionNodeId WitnessClass -> Set.Set RegionNodeId
obstructedWitnessNodes =
  Map.foldrWithKey
    ( \nodeValue witnessClassValue obstructedNodes ->
        case witnessClassValue of
          WitnessObstructed ->
            Set.insert nodeValue obstructedNodes
          _ ->
            obstructedNodes
    )
    Set.empty

gatesFromAccumulator :: PruningAccumulator -> CohomologicalPruningGates root
gatesFromAccumulator accumulator =
  CohomologicalPruningGates
    { cpgSeedDecision = seedDecision accumulator,
      cpgRegionDecision = regionDecision accumulator
    }

seedDecision ::
  PruningAccumulator ->
  CandidateRegionSeed root ->
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction
seedDecision accumulator seedValue =
  pruningDecisionFromVerdict
    (seedFootprint seedValue)
    Nothing
    (seedVerdict accumulator seedValue)

seedVerdict ::
  PruningAccumulator ->
  CandidateRegionSeed root ->
  ObstructionVerdict CohomologicalPruningObstruction
seedVerdict accumulator seedValue =
  rejectedFromList
    ( nonCriticalReason
        <> contextReason
        <> witnessReason
    )
  where
    nodeValue =
      crsNodeId seedValue

    nonCriticalReason =
      [ MicrosupportNonCriticalObstruction nodeValue
      | Set.member nodeValue (paNonCriticalNodes accumulator)
      ]

    contextReason =
      case (paRelevantOrdinals accumulator, crsContextOrdinal seedValue) of
        (Just relevantOrdinals, Just contextOrdinal)
          | not (Set.member contextOrdinal relevantOrdinals) ->
              [ContextIrrelevantObstruction contextOrdinal]
        _ ->
          []

    witnessReason =
      [ WitnessObstructedObstruction nodeValue
      | Set.member nodeValue (paObstructedNodes accumulator)
      ]

regionVerdict ::
  PruningAccumulator ->
  CandidateRegion root ->
  ObstructionVerdict CohomologicalPruningObstruction
regionVerdict accumulator regionValue =
  case crNodeId regionValue of
    Nothing ->
      Accepted ()
    Just nodeValue ->
      rejectedFromList
        ( nonCriticalReason nodeValue
            <> witnessReason nodeValue
        )
  where
    nonCriticalReason nodeValue =
      [ MicrosupportNonCriticalObstruction nodeValue
      | Set.member nodeValue (paNonCriticalNodes accumulator)
      ]

    witnessReason nodeValue =
      [ WitnessObstructedObstruction nodeValue
      | Set.member nodeValue (paObstructedNodes accumulator)
      ]

regionDecision ::
  PruningAccumulator ->
  CandidateRegion root ->
  PruningDecision CohomologicalPruningFootprint () CohomologicalPruningObstruction
regionDecision accumulator regionValue =
  pruningDecisionFromVerdict
    (regionFootprint regionValue)
    Nothing
    (regionVerdict accumulator regionValue)
