{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Constraint
  ( guardConstraintsOf,
    factConstraintsOf,
    proofConstraintsOf,
    factConstraintPlanOf,
  )
where

import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Proof
  ( ProofReachability,
    proofTupleSupportedWithReachability,
    supportsProofConstraints,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphAnchor,
    EGraphExactConstraint,
    anchorDomain,
    anchorForGuardRef,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( SheafCapabilityAtom,
    GuardModalityEnvironment (..),
  )
import Moonlight.Rewrite.System
  ( GuardAtom,
    GuardRef,
    GuardTerm (..),
    guardAtomCase,
  )
import Moonlight.Rewrite.System
  ( FactId,
    FactStore,
    FactTuple (..),
    factsFor,
  )
import Moonlight.Sheaf.Obstruction
  ( ConstraintId (..),
    ExactLabelCode (..),
    LoweringGap,
    OccurrenceId,
    RelationConstraintPlan (..),
    RelationFlavor (FactFlavor, ProofFlavor),
    RelationLoweringAlgebra (..),
    guardEqualityConstraintsBy,
    lowerRelationConstraintPlan,
  )

type FactAtom :: (Type -> Type) -> Type
type FactAtom f =
  (GuardAtom SheafCapabilityAtom f, FactId, [GuardRef])

type EGraphLoweringGap :: Type
type EGraphLoweringGap =
  LoweringGap EGraphAnchor GuardRef

guardConstraintsOf ::
  Language f =>
  Int ->
  Map OccurrenceId IntSet ->
  Map (GuardAtom SheafCapabilityAtom f) IntSet ->
  Map Int EGraphAnchor ->
  [GuardAtom SheafCapabilityAtom f] ->
  ConstraintId ->
  ([EGraphExactConstraint], ConstraintId)
guardConstraintsOf rootKey occurrenceDomains guardDomains representativeAnchors guardAtoms startingId =
  guardEqualityConstraintsBy
    rootKey
    occurrenceDomains
    guardDomains
    guardEqualityReferences
    (anchorForGuardRef representativeAnchors)
    guardAtoms
    startingId

guardEqualityReferences ::
  GuardAtom SheafCapabilityAtom f ->
  Maybe (GuardRef, GuardRef)
guardEqualityReferences guardAtom =
  guardAtomCase
    (\leftTerm rightTerm -> (,) <$> guardRefFromTerm leftTerm <*> guardRefFromTerm rightTerm)
    (\_ _ -> Nothing)
    (\_ _ -> Nothing)
    guardAtom

factConstraintPlanOf ::
  Language f =>
  FactStore ->
  (ClassId -> ClassId) ->
  Int ->
  Map OccurrenceId IntSet ->
  Map (GuardAtom SheafCapabilityAtom f) IntSet ->
  Map Int EGraphAnchor ->
  [GuardAtom SheafCapabilityAtom f] ->
  ConstraintId ->
  RelationConstraintPlan EGraphAnchor GuardRef FactId
factConstraintPlanOf factStore canonicalize rootKey occurrenceDomains guardDomains representativeAnchors guardAtoms startingId =
  lowerRelationConstraintPlan
    FactFlavor
    RelationLoweringAlgebra
      { rlaReferencesOf =
          \(_, _, guardRefs) -> Just guardRefs,
        rlaAnchorForReference =
          anchorForGuardRef representativeAnchors,
        rlaSupportActive =
          \(guardAtom, _, _) ->
            not (IntSet.null (Map.findWithDefault IntSet.empty guardAtom guardDomains)),
        rlaSupportTuples =
          \(_, factId, _) anchorValues ->
            factSupportTuples factStore canonicalize rootKey occurrenceDomains factId anchorValues,
        rlaOriginOf =
          \(_, factId, _) -> Just factId
      }
    startingId
    (factAtomsOf guardAtoms)

factConstraintsOf ::
  Language f =>
  FactStore ->
  (ClassId -> ClassId) ->
  Int ->
  Map OccurrenceId IntSet ->
  Map (GuardAtom SheafCapabilityAtom f) IntSet ->
  Map Int EGraphAnchor ->
  [GuardAtom SheafCapabilityAtom f] ->
  ConstraintId ->
  ([EGraphExactConstraint], [ConstraintId], [EGraphLoweringGap], Map ConstraintId FactId, ConstraintId)
factConstraintsOf factStore canonicalize rootKey occurrenceDomains guardDomains representativeAnchors guardAtoms startingId =
  let factPlan =
        factConstraintPlanOf
          factStore
          canonicalize
          rootKey
          occurrenceDomains
          guardDomains
          representativeAnchors
          guardAtoms
          startingId
   in ( rcpExactConstraints factPlan,
        rcpUnsupportedConstraints factPlan,
        rcpLoweringGaps factPlan,
        rcpOrigins factPlan,
        rcpNextConstraintId factPlan
      )

proofConstraintsOf ::
  Language f =>
  FactStore ->
  Maybe ProofReachability ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Int ->
  Map OccurrenceId IntSet ->
  Map (GuardAtom SheafCapabilityAtom f) IntSet ->
  Map Int EGraphAnchor ->
  [GuardAtom SheafCapabilityAtom f] ->
  ConstraintId ->
  ([EGraphExactConstraint], [EGraphLoweringGap], ConstraintId)
proofConstraintsOf factStore maybeReachability canonicalize request rootKey occurrenceDomains guardDomains representativeAnchors guardAtoms startingId =
  if supportsProofConstraints maybeReachability request
    then
      let proofPlan =
            lowerRelationConstraintPlan
              ProofFlavor
              RelationLoweringAlgebra
                { rlaReferencesOf =
                    \(_, _, guardRefs) -> Just guardRefs,
                  rlaAnchorForReference =
                    anchorForGuardRef representativeAnchors,
                  rlaSupportActive =
                    \(guardAtom, _, _) ->
                      not (IntSet.null (Map.findWithDefault IntSet.empty guardAtom guardDomains)),
                  rlaSupportTuples =
                    \(_, factId, _) anchorValues ->
                      factSupportTuples factStore canonicalize rootKey occurrenceDomains factId anchorValues
                        & filter
                          ( \tupleValue ->
                              maybe
                                False
                                ( \proofReachability ->
                                    proofTupleSupportedWithReachability
                                      canonicalize
                                      proofReachability
                                      (ClassId rootKey)
                                      anchorValues
                                      tupleValue
                                )
                                maybeReachability
                          ),
                  rlaOriginOf =
                    const Nothing
                }
              startingId
              (factAtomsOf guardAtoms)
       in ( rcpExactConstraints proofPlan,
            rcpLoweringGaps proofPlan,
            rcpNextConstraintId proofPlan
          )
    else
      ([], [], startingId)

factAtomsOf ::
  [GuardAtom SheafCapabilityAtom f] ->
  [FactAtom f]
factAtomsOf =
  mapMaybe
    ( \guardAtom ->
        guardAtomCase
          (\_ _ -> Nothing)
          (\factId guardTerms -> (\guardRefs -> (guardAtom, factId, guardRefs)) <$> traverse guardRefFromTerm guardTerms)
          (\_ _ -> Nothing)
          guardAtom
    )

guardRefFromTerm :: GuardTerm f -> Maybe GuardRef
guardRefFromTerm =
  \case
    GuardRefTerm guardRef ->
      Just guardRef
    _ ->
      Nothing

factSupportTuples ::
  FactStore ->
  (ClassId -> ClassId) ->
  Int ->
  Map OccurrenceId IntSet ->
  FactId ->
  [EGraphAnchor] ->
  [[ExactLabelCode]]
factSupportTuples factStore canonicalize rootKey occurrenceDomains factId anchorValues =
  factsFor factId factStore
    & Set.toAscList
    & mapMaybe (supportedTuple canonicalize rootKey occurrenceDomains anchorValues)

supportedTuple ::
  (ClassId -> ClassId) ->
  Int ->
  Map OccurrenceId IntSet ->
  [EGraphAnchor] ->
  FactTuple ->
  Maybe [ExactLabelCode]
supportedTuple canonicalize rootKey occurrenceDomains anchorValues factTuple =
  let tupleValues =
        fmap
          (canonicalClassKey canonicalize)
          (unFactTuple factTuple)
   in if length tupleValues /= length anchorValues
        then Nothing
        else
          if and
            ( zipWith
                IntSet.member
                tupleValues
                (fmap (anchorDomain rootKey occurrenceDomains) anchorValues)
            )
            then Just (fmap ClassLabelCode tupleValues)
            else Nothing

canonicalClassKey :: (ClassId -> ClassId) -> ClassId -> Int
canonicalClassKey canonicalize =
  classIdKey . canonicalize
