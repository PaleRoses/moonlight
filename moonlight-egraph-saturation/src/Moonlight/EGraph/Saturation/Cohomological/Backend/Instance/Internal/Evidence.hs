module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Evidence
  ( deriveRelationEvidence,
    deriveProofRelationsFromContext,
    mergeRelationEvidence,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Proof
  ( classIdFromLabel,
    proofTupleSupportedWithReachability,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofReachability,
  )
import Moonlight.Rewrite.System
  ( FactDerivation,
    FactDerivationIndex,
    lookupFactDerivations,
  )
import Moonlight.Rewrite.System
  ( FactId,
    FactTuple (..),
    FactWitness (..),
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    ConstraintId,
    OccurrenceId,
    RelationFlavor (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( RelationEvidence (..),
    SectionCoordinate (..),
  )

deriveRelationEvidence ::
  FactDerivationIndex ->
  Map.Map ConstraintId FactId ->
  RelationFlavor ->
  (Set.Set FactDerivation -> Bool) ->
  [RelationEvidence coordinate] ->
  [RelationEvidence coordinate]
deriveRelationEvidence factDerivations factConstraintOrigins relationFlavor selectDerivations =
  mapMaybe
    (\relationEvidence ->
       Map.lookup (reConstraintId relationEvidence) factConstraintOrigins
         >>= \factId ->
           let supportedTuples =
                 filter
                   (\tupleValue ->
                      maybe
                        False
                        (\factWitness ->
                           selectDerivations
                             (lookupFactDerivations factWitness factDerivations)
                        )
                        (fmap (FactWitness factId . FactTuple) (traverse classIdFromLabel tupleValue))
                   )
                   (reMatchingTuples relationEvidence)
            in if null supportedTuples
                 then Nothing
                 else
                   Just
                     relationEvidence
                       { reFlavor = relationFlavor,
                         reMatchingTuples = supportedTuples
                       }
    )

mergeRelationEvidence ::
  Ord coordinate =>
  [RelationEvidence coordinate] ->
  [RelationEvidence coordinate] ->
  [RelationEvidence coordinate]
mergeRelationEvidence leftEvidence rightEvidence =
  Set.union (Set.fromList leftEvidence) (Set.fromList rightEvidence)
    & Set.toAscList

deriveProofRelationsFromContext ::
  (ClassId -> ClassId) ->
  Maybe ProofReachability ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  ClassId ->
  [RelationEvidence (SectionCoordinate (Anchor OccurrenceId))] ->
  [RelationEvidence (SectionCoordinate (Anchor OccurrenceId))]
deriveProofRelationsFromContext canonicalize maybeProofReachability _request rootClass =
  case maybeProofReachability of
    Nothing -> const []
    Just proofReachability ->
      mapMaybe
        (\relationEvidence ->
           let supportedTuples =
                 filter
                   (proofTupleSupportedWithReachability canonicalize proofReachability rootClass (proofAnchors relationEvidence))
                   (reMatchingTuples relationEvidence)
            in if null supportedTuples
                 then Nothing
                 else
                   Just
                     relationEvidence
                       { reFlavor = ProofFlavor,
                         reMatchingTuples = supportedTuples
                       }
        )

proofAnchors ::
  RelationEvidence (SectionCoordinate (Anchor OccurrenceId)) ->
  [Anchor OccurrenceId]
proofAnchors relationEvidence =
  reCoordinates relationEvidence
    & fmap
      (\sectionCoordinate ->
         case sectionCoordinate of
           StructuralCoordinate anchorValue -> anchorValue
           RelationCoordinate _ anchorValue -> anchorValue
      )
