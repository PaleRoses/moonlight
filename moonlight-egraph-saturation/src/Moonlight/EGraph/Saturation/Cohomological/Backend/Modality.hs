{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Modality
  ( SheafModalityKey,
    data EqualityModalityKey,
    data GuardModalityKey,
    data FactModalityKey,
    data ProofModalityKey,
    data CapabilityModalityKey,
    SheafCapabilityLabel (..),
    mkSheafCapabilityLabel,
    mkSheafCapabilityEnvironment,
    TypedCapabilitySupport (..),
    EqualityModalityEnvironment (..),
    GuardModalityEnvironment (..),
    FactModalityEnvironment (..),
    ProofModalityEnvironment (..),
    CapabilityModalityEnvironment,
    sheafEnvironmentAlgebra,
    sheafEnvironmentFingerprintFor,
    eGraphModalityRegistry,
    evaluateEGraphModalitySupport,
    eGraphSectionProjection,
    validateSheafModalityCoverage,
  )
where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Moonlight.Core (Language, Pattern)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingRequest)
import Moonlight.EGraph.Pure.Types
  ( ClassId,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofReachability,
  )
import Moonlight.Rewrite.System
  ( GuardRef,
  )
import Moonlight.Rewrite.System
  ( FactId,
    FactStore,
  )
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphAnchor,
    EGraphObstructionModality,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Constraint
  ( factConstraintsOf,
    guardConstraintsOf,
    proofConstraintsOf,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Projection
  ( equalityConstraints,
    equalityReification,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( CapabilityModalityEnvironment,
    EGraphSectionCertification,
    EqualityModalityEnvironment (..),
    FactModalityEnvironment (..),
    GuardModalityEnvironment (..),
    ProofModalityEnvironment (..),
    SheafCapabilityLabel (..),
    SheafModalityKey,
    TypedCapabilitySupport (..),
    data CapabilityModalityKey,
    data EqualityModalityKey,
    data FactModalityKey,
    data GuardModalityKey,
    data ProofModalityKey,
    mkSheafCapabilityLabel,
    mkSheafCapabilityEnvironment,
    sheafEnvironmentAlgebra,
    sheafEnvironmentFingerprintFor,
  )
import Moonlight.Sheaf.Obstruction
  ( ConstraintId (..),
    OccurrenceId,
    RelationFlavor (FactFlavor, ProofFlavor),
  )
import Moonlight.Sheaf.Obstruction
  ( IndexedEnvironment,
    lookupEnvironmentBinding,
  )
import Moonlight.Sheaf.Obstruction
  ( ModalityContribution (..),
    ModalityRegistry,
    ObstructionModality (..),
    modalityRegistryFromList,
    modalityRegistryProjection,
    obstructionModality,
    obstructionModalityWithReification,
    typedCapabilityModality,
  )
import Moonlight.Sheaf.Obstruction
  ( RelationProjectionConflict,
    RelationProjectionMode (StructuralProjection),
    RelationProjectionPolicy,
    SectionCoordinate,
    SectionProjection,
    emptyRelationProjectionPolicy,
    relationProjectionPolicyFor,
  )
import Moonlight.Sheaf.Obstruction
  ( SheafModalityCoverage,
    sheafModalityTagFromKey,
    validateModalityCoverageWithEnvironmentKeys,
  )


validateSheafModalityCoverage ::
  forall owner c f.
  Language f =>
  EGraphSectionCertification owner c f ->
  SheafModalityCoverage
validateSheafModalityCoverage _context =
  validateModalityCoverageWithEnvironmentKeys
    (DMap.fromList (eGraphModalityEnvironmentKeys :: [DSum (SheafModalityKey owner c f ()) Proxy]))
    (eGraphModalityRegistry :: ModalityRegistry (SheafModalityKey owner c f ()) EGraphAnchor Substitution GuardRef)
    sheafModalityTagFromKey

eGraphModalityEnvironmentKeys :: [DSum (SheafModalityKey owner c f runtime) Proxy]
eGraphModalityEnvironmentKeys =
  [ EqualityModalityKey :=> Proxy,
    GuardModalityKey :=> Proxy,
    FactModalityKey :=> Proxy,
    ProofModalityKey :=> Proxy,
    CapabilityModalityKey :=> Proxy
  ]

equalityModality :: EGraphObstructionModality (EqualityModalityEnvironment f)
equalityModality =
  obstructionModalityWithReification
    emptyRelationProjectionPolicy
    equalityReification
    (stripOrigins equalityContribution)

guardModality :: Language f => EGraphObstructionModality (GuardModalityEnvironment f)
guardModality =
  modalityFromRunner emptyRelationProjectionPolicy guardContribution

factModality :: EGraphObstructionModality (FactModalityEnvironment owner c f a)
factModality =
  projectionOnlyModality
    (relationProjectionPolicyFor FactFlavor StructuralProjection)

proofModality :: EGraphObstructionModality (ProofModalityEnvironment owner c f a)
proofModality =
  projectionOnlyModality
    (relationProjectionPolicyFor ProofFlavor StructuralProjection)

type EGraphRegisteredModality :: Type -> Type -> (Type -> Type) -> Type -> Type
data EGraphRegisteredModality owner c f runtime where
  EGraphRegisteredModality ::
    SheafModalityKey owner c f runtime value ->
    EGraphObstructionModality value ->
    ( (ClassId -> ClassId) ->
      FactStore ->
      Maybe ProofReachability ->
      ConstraintId ->
      value ->
      (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
    ) ->
    EGraphRegisteredModality owner c f runtime

eGraphRegisteredModalities :: Language f => [EGraphRegisteredModality owner c f runtime]
eGraphRegisteredModalities =
  [ EGraphRegisteredModality EqualityModalityKey equalityModality (\_ _ _ -> equalityContribution),
    EGraphRegisteredModality GuardModalityKey guardModality (\_ _ _ -> guardContribution),
    EGraphRegisteredModality FactModalityKey factModality factContribution,
    EGraphRegisteredModality ProofModalityKey proofModality proofContribution,
    EGraphRegisteredModality CapabilityModalityKey typedCapabilityModality (\_ _ _ -> capabilityContribution)
  ]

eGraphModalityRegistry :: Language f => ModalityRegistry (SheafModalityKey owner c f a) EGraphAnchor Substitution GuardRef
eGraphModalityRegistry =
  modalityRegistryFromList
    [ modalityKey :=> modalityValue
    | EGraphRegisteredModality modalityKey modalityValue _ <- eGraphRegisteredModalities
    ]

evaluateEGraphModalitySupport ::
  Language f =>
  (ClassId -> ClassId) ->
  FactStore ->
  Maybe ProofReachability ->
  IndexedEnvironment (SheafModalityKey owner c f runtime) ->
  (ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
evaluateEGraphModalitySupport canonicalize factStore maybeProofReachability indexedEnvironment =
  let (_, contribution, factOrigins) =
        List.foldl'
          (stepRegisteredModality canonicalize factStore maybeProofReachability indexedEnvironment)
          (ConstraintId 0, mempty, Map.empty)
          eGraphRegisteredModalities
   in (contribution, factOrigins)

eGraphSectionProjection :: Either [RelationProjectionConflict] (SectionProjection EGraphAnchor (SectionCoordinate EGraphAnchor))
eGraphSectionProjection =
  modalityRegistryProjection
    ( modalityRegistryFromList
        [ EqualityModalityKey :=> projectionOnlyModality emptyRelationProjectionPolicy,
          GuardModalityKey :=> projectionOnlyModality emptyRelationProjectionPolicy,
          FactModalityKey :=> projectionOnlyModality (relationProjectionPolicyFor FactFlavor StructuralProjection),
          ProofModalityKey :=> projectionOnlyModality (relationProjectionPolicyFor ProofFlavor StructuralProjection),
          CapabilityModalityKey :=> projectionOnlyModality (modalityProjectionPolicy (typedCapabilityModality :: EGraphObstructionModality (CapabilityModalityEnvironment OccurrenceId)))
        ]
    )

projectionOnlyModality :: RelationProjectionPolicy -> EGraphObstructionModality value
projectionOnlyModality projectionPolicy =
  obstructionModality
    projectionPolicy
    (\startingId _ -> (startingId, ModalityContribution {mcExactConstraints = [], mcLoweringGaps = []}))

modalityFromRunner ::
  RelationProjectionPolicy ->
  (ConstraintId -> env -> (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)) ->
  EGraphObstructionModality env
modalityFromRunner projectionPolicy runSupport =
  obstructionModality projectionPolicy (stripOrigins runSupport)

stripOrigins ::
  (ConstraintId -> env -> (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)) ->
  ConstraintId ->
  env ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef)
stripOrigins runSupport startingId environment =
  case runSupport startingId environment of
    (nextConstraintId, contribution, _) ->
      (nextConstraintId, contribution)

equalityContribution ::
  ConstraintId ->
  EqualityModalityEnvironment f ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
equalityContribution startingId environment =
  let constraints =
        equalityConstraints
          (emeOccurrenceDomains environment)
          (emeOccurrences environment)
          startingId
   in ( ConstraintId (unConstraintId startingId + length constraints),
        ModalityContribution
          { mcExactConstraints = constraints,
            mcLoweringGaps = []
          },
        Map.empty
      )

guardContribution ::
  Language f =>
  ConstraintId ->
  GuardModalityEnvironment f ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
guardContribution startingId environment =
  let (constraints, nextConstraintId) =
        guardConstraintsOf
          (gmeRootKey environment)
          (gmeOccurrenceDomains environment)
          (gmeGuardDomains environment)
          (gmeRepresentativeAnchors environment)
          (gmeGuardAtoms environment)
          startingId
   in ( nextConstraintId,
        ModalityContribution
          { mcExactConstraints = constraints,
            mcLoweringGaps = []
          },
        Map.empty
      )

factContribution ::
  Language f =>
  (ClassId -> ClassId) ->
  FactStore ->
  Maybe ProofReachability ->
  ConstraintId ->
  FactModalityEnvironment owner c f runtime ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
factContribution canonicalize factStore _maybeProofReachability startingId environment =
  let guardEnvironment = fmeGuardEnvironment environment
      (constraints, _unsupported, loweringGaps, origins, nextConstraintId) =
        factConstraintsOf
          factStore
          canonicalize
          (gmeRootKey guardEnvironment)
          (gmeOccurrenceDomains guardEnvironment)
          (gmeGuardDomains guardEnvironment)
          (gmeRepresentativeAnchors guardEnvironment)
          (gmeGuardAtoms guardEnvironment)
          startingId
   in ( nextConstraintId,
        ModalityContribution
          { mcExactConstraints = constraints,
            mcLoweringGaps = loweringGaps
          },
        origins
      )

proofContribution ::
  Language f =>
  (ClassId -> ClassId) ->
  FactStore ->
  Maybe ProofReachability ->
  ConstraintId ->
  ProofModalityEnvironment owner c f runtime ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
proofContribution canonicalize factStore maybeProofReachability startingId environment =
  let guardEnvironment = pmeGuardEnvironment environment
      (constraints, loweringGaps, nextConstraintId) =
        proofConstraintsOf
          factStore
          maybeProofReachability
          canonicalize
          (pmeRequest environment)
          (gmeRootKey guardEnvironment)
          (gmeOccurrenceDomains guardEnvironment)
          (gmeGuardDomains guardEnvironment)
          (gmeRepresentativeAnchors guardEnvironment)
          (gmeGuardAtoms guardEnvironment)
          startingId
   in ( nextConstraintId,
        ModalityContribution
          { mcExactConstraints = constraints,
            mcLoweringGaps = loweringGaps
          },
        Map.empty
      )

capabilityContribution ::
  ConstraintId ->
  CapabilityModalityEnvironment OccurrenceId ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
capabilityContribution startingId environment =
  let (nextConstraintId, contribution) =
        runObstructionModality typedCapabilityModality startingId environment
   in (nextConstraintId, contribution, Map.empty)

stepRegisteredModality ::
  (ClassId -> ClassId) ->
  FactStore ->
  Maybe ProofReachability ->
  IndexedEnvironment (SheafModalityKey owner c f runtime) ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId) ->
  EGraphRegisteredModality owner c f runtime ->
  (ConstraintId, ModalityContribution EGraphAnchor GuardRef, Map ConstraintId FactId)
stepRegisteredModality canonicalize factStore maybeProofReachability indexedEnvironment (nextConstraintId, contribution, factOrigins) registeredModality =
  case registeredModality of
    EGraphRegisteredModality modalityKey _ runSupport ->
      case lookupEnvironmentBinding modalityKey indexedEnvironment of
        Nothing ->
          (nextConstraintId, contribution, factOrigins)
        Just environment ->
          let (nextConstraintId', contribution', factOrigins') =
                runSupport canonicalize factStore maybeProofReachability nextConstraintId environment
           in ( nextConstraintId',
                contribution <> contribution',
                factOrigins <> factOrigins'
              )
