{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Rules
  ( applyPlanRewriteRound,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralStore,
    structuralRepairClosure,
    structuralTuplesForResultKey,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphStore,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Analysis
  ( knownShapeDigestInClass,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.RewriteState
  ( PlanRewriteState (..),
    addPlanENodeToState,
    lawEnabled,
    mergeClassesForSimpleLaw,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis,
    PlanSaturationError,
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanNode (..),
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEqualityLaw (..),
    PlanEquivalenceStep,
    PlanRewriteSystem,
  )
import Moonlight.Flow.Plan.Rewrite.Transform.Coverage
  ( coverageTransformCompose,
    coverSingletonEliminates,
  )
import Moonlight.Flow.Plan.Rewrite.Transform.ProjectionRestriction
  ( composeProjectionShapes,
    composeRestrictionShapes,
    projectionIdentity,
    projectionRestrictionCommutes,
    projectionShapeFromPayload,
    restrictionIdentity,
    restrictionProjectionCommutes,
    restrictionShapeFromPayload,
  )
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.Term
  ( CoverageTransformPayload (..),
    PlanShape (..),
    PlanStage (..),
  )

applyPlanRewriteRound ::
  PlanRewriteSystem ->
  IntSet ->
  EGraph PlanNode PlanAnalysis ->
  Either PlanSaturationError (EGraph PlanNode PlanAnalysis, IntSet, [PlanEquivalenceStep])
applyPlanRewriteRound rewriteSystem dirtyClassKeys graph
  | IntSet.null dirtyClassKeys =
      Right (graph, IntSet.empty, [])
  | otherwise =
      let store =
            eGraphStore graph
          rewriteClassKeys =
            structuralRepairClosure
              store
              (canonicalizeClassKeys graph dirtyClassKeys)
          entries =
            planEntriesForClassKeys store rewriteClassKeys
          finalStateResult =
            foldr
              (\entry stateResult -> stateResult >>= applyPlanRewriteEntry rewriteSystem entry)
              ( Right
                  PlanRewriteState
                    { pwsGraph = graph,
                      pwsDirtyClassKeys = IntSet.empty,
                      pwsStepsRev = []
                    }
              )
              entries
       in fmap
            (\finalState -> (pwsGraph finalState, pwsDirtyClassKeys finalState, reverse (pwsStepsRev finalState)))
            finalStateResult

canonicalizeClassKeys ::
  EGraph PlanNode PlanAnalysis ->
  IntSet ->
  IntSet
canonicalizeClassKeys graph =
  IntSet.map (classIdKey . canonicalizeClassId graph . ClassId)

planEntriesForClassKeys ::
  StructuralStore PlanNode ->
  IntSet ->
  [(ClassId, PlanNode ClassId)]
planEntriesForClassKeys store =
  IntSet.foldr
    ( \classKey entries ->
        fmap
          (\(ENode node) -> (ClassId classKey, node))
          (structuralTuplesForResultKey classKey store)
          <> entries
    )
    []

nodesForClass ::
  ClassId ->
  PlanRewriteState ->
  [PlanNode ClassId]
nodesForClass classId state
  | canonicalKey == directKey =
      canonicalNodes
  | otherwise =
      Set.toAscList (Set.fromList (canonicalNodes <> directNodes))
  where
    graph = pwsGraph state
    store =
      eGraphStore graph

    canonicalKey =
      classIdKey (canonicalizeClassId graph classId)

    directKey =
      classIdKey classId

    canonicalNodes =
      nodesForClassKey store canonicalKey

    directNodes =
      nodesForClassKey store directKey

nodesForClassKey ::
  StructuralStore PlanNode ->
  Int ->
  [PlanNode ClassId]
nodesForClassKey store classKey =
  fmap (\(ENode node) -> node) (structuralTuplesForResultKey classKey store)

applyPlanRewriteEntry ::
  PlanRewriteSystem ->
  (ClassId, PlanNode ClassId) ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyPlanRewriteEntry rewriteSystem (resultClass, node) state =
  case node of
    PlanProjectionNode planShape sourceClass ->
      applyProjectionRewrites rewriteSystem resultClass planShape sourceClass state
    PlanRestrictionNode planShape sourceClass ->
      applyRestrictionRewrites rewriteSystem resultClass planShape sourceClass state
    PlanAmalgamationNode planShape members ->
      applyCoverRewrites rewriteSystem resultClass planShape members state
    PlanCoverageTransformNode planShape sourceClass ->
      applyCoverageTransformRewrites rewriteSystem resultClass planShape sourceClass state
    _ ->
      Right state

applyProjectionRewrites ::
  PlanRewriteSystem ->
  ClassId ->
  PlanShape 'Projection ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyProjectionRewrites rewriteSystem resultClass planShape sourceClass state = do
  let afterIdentity =
        if lawEnabled LawProjectionId rewriteSystem && projectionIdentity (psPayload planShape)
          then mergeClassesForSimpleLaw LawProjectionId (psDigest planShape) resultClass sourceClass state
          else state
  afterCompose <-
    if lawEnabled LawProjectionCompose rewriteSystem
      then applyProjectionCompose resultClass planShape sourceClass afterIdentity
      else Right afterIdentity
  afterCommute <-
    if lawEnabled LawProjectionRestrictionCommute rewriteSystem
      then applyProjectionRestrictionCommute resultClass planShape sourceClass afterCompose
      else Right afterCompose
  if lawEnabled LawProjectionRestrictionFuse rewriteSystem
    then applyProjectionRestrictionFuse resultClass planShape sourceClass afterCommute
    else Right afterCommute

applyRestrictionRewrites ::
  PlanRewriteSystem ->
  ClassId ->
  PlanShape 'Restriction ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyRestrictionRewrites rewriteSystem resultClass planShape sourceClass state = do
  let afterIdentity =
        if lawEnabled LawRestrictionId rewriteSystem && restrictionIdentity (psPayload planShape)
          then mergeClassesForSimpleLaw LawRestrictionId (psDigest planShape) resultClass sourceClass state
          else state
  afterCompose <-
    if lawEnabled LawRestrictionCompose rewriteSystem
      then applyRestrictionCompose resultClass planShape sourceClass afterIdentity
      else Right afterIdentity
  if lawEnabled LawRestrictionProjectionCommute rewriteSystem
    then applyRestrictionProjectionCommute resultClass planShape sourceClass afterCompose
    else Right afterCompose

applyProjectionCompose ::
  ClassId ->
  PlanShape 'Projection ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyProjectionCompose resultClass outerShape sourceClass =
  applyShapeCompose resultClass sourceClass LawProjectionCompose outerShape matchProjectionNode composeProjectionShapes PlanProjectionNode

applyRestrictionCompose ::
  ClassId ->
  PlanShape 'Restriction ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyRestrictionCompose resultClass outerShape sourceClass =
  applyShapeCompose resultClass sourceClass LawRestrictionCompose outerShape matchRestrictionNode composeRestrictionShapes PlanRestrictionNode

applyCoverageTransformCompose ::
  ClassId ->
  PlanShape 'CoverageTransform ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyCoverageTransformCompose resultClass outerShape sourceClass =
  applyShapeCompose resultClass sourceClass LawCoverageTransformCompose outerShape matchCoverageTransformNode composeCoverageTransformShapes PlanCoverageTransformNode

applyShapeCompose ::
  ClassId ->
  ClassId ->
  PlanEqualityLaw ->
  PlanShape outerStage ->
  (PlanNode ClassId -> Maybe (PlanShape innerStage, ClassId)) ->
  (PlanShape innerStage -> PlanShape outerStage -> Maybe (PlanShape resultStage)) ->
  (PlanShape resultStage -> ClassId -> PlanNode ClassId) ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyShapeCompose resultClass sourceClass law outerShape matchChild composeShapes buildNode state =
  foldr
    (\childNode stateResult -> stateResult >>= applyChild childNode)
    (Right state)
    (nodesForClass sourceClass state)
  where
    applyChild childNode currentState =
      case matchChild childNode of
        Nothing ->
          Right currentState
        Just (innerShape, grandChild) ->
          case composeShapes innerShape outerShape of
            Nothing ->
              Right currentState
            Just composedShape ->
              addNodeAndMergeWithLaw law (psDigest composedShape) resultClass (buildNode composedShape grandChild) currentState

composeCoverageTransformShapes ::
  PlanShape 'CoverageTransform ->
  PlanShape 'CoverageTransform ->
  Maybe (PlanShape 'CoverageTransform)
composeCoverageTransformShapes innerShape outerShape =
  ShapeBuild.mkCoverageTransformShape <$> coverageTransformCompose (psPayload outerShape) (psPayload innerShape)

applyProjectionRestrictionCommute ::
  ClassId ->
  PlanShape 'Projection ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyProjectionRestrictionCommute resultClass outerShape sourceClass =
  applyShapeCommute
    resultClass sourceClass LawProjectionRestrictionCommute outerShape matchRestrictionNode
    (\projectionShape restrictionShape -> projectionRestrictionCommutes (psPayload projectionShape) (psPayload restrictionShape))
    projectionShapeFromPayload (Just . restrictionShapeFromPayload) PlanProjectionNode PlanRestrictionNode

applyRestrictionProjectionCommute ::
  ClassId ->
  PlanShape 'Restriction ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyRestrictionProjectionCommute resultClass outerShape sourceClass =
  applyShapeCommute
    resultClass sourceClass LawRestrictionProjectionCommute outerShape matchProjectionNode
    (\restrictionShape projectionShape -> restrictionProjectionCommutes (psPayload restrictionShape) (psPayload projectionShape))
    (Just . restrictionShapeFromPayload) projectionShapeFromPayload PlanRestrictionNode PlanProjectionNode

applyShapeCommute ::
  ClassId ->
  ClassId ->
  PlanEqualityLaw ->
  PlanShape outerStage ->
  (PlanNode ClassId -> Maybe (PlanShape innerStage, ClassId)) ->
  (PlanShape outerStage -> PlanShape innerStage -> Maybe (leftPayload, rightPayload)) ->
  (leftPayload -> Maybe (PlanShape leftStage)) ->
  (rightPayload -> Maybe (PlanShape rightStage)) ->
  (PlanShape leftStage -> ClassId -> PlanNode ClassId) ->
  (PlanShape rightStage -> ClassId -> PlanNode ClassId) ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyShapeCommute resultClass sourceClass law outerShape matchChild commutePayloads makeLeftShape makeRightShape buildLeftNode buildRightNode state =
  foldr
    (\childNode stateResult -> stateResult >>= applyChild childNode)
    (Right state)
    (nodesForClass sourceClass state)
  where
    applyChild childNode currentState =
      case matchChild childNode of
        Nothing ->
          Right currentState
        Just (innerShape, grandChild) ->
          case commutePayloads outerShape innerShape of
            Nothing ->
              Right currentState
            Just (leftPayload, rightPayload) ->
              case (makeLeftShape leftPayload, makeRightShape rightPayload) of
                (Just leftShape, Just rightShape) ->
                  addLinearPairAndMergeWithLaw
                    law
                    (psDigest rightShape)
                    resultClass
                    (buildLeftNode leftShape grandChild)
                    (buildRightNode rightShape)
                    currentState
                _ ->
                  Right currentState

applyProjectionRestrictionFuse ::
  ClassId ->
  PlanShape 'Projection ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyProjectionRestrictionFuse resultClass projectionShape sourceClass state =
  foldr
    (\childNode stateResult -> stateResult >>= applyChildRestriction childNode)
    (Right state)
    (nodesForClass sourceClass state)
  where
    applyChildRestriction childNode currentState =
      case childNode of
        PlanRestrictionNode restrictionShape middleClass ->
          case projectionRestrictionCommutes (psPayload projectionShape) (psPayload restrictionShape) of
            Nothing ->
              Right currentState
            Just (commutedProjectionPayload, commutedRestrictionPayload) ->
              foldr
                ( \grandChildNode stateResult ->
                    stateResult
                      >>= applyGrandChildProjection commutedProjectionPayload commutedRestrictionPayload grandChildNode
                )
                (Right currentState)
                (nodesForClass middleClass currentState)
        _ ->
          Right currentState

    applyGrandChildProjection commutedProjectionPayload commutedRestrictionPayload grandChildNode currentState =
      case grandChildNode of
        PlanProjectionNode innerProjectionShape baseClass ->
          case projectionShapeFromPayload commutedProjectionPayload of
            Nothing ->
              Right currentState
            Just commutedProjectionShape ->
              case composeProjectionShapes innerProjectionShape commutedProjectionShape of
                Nothing ->
                  Right currentState
                Just composedProjectionShape ->
                  let restrictionShape =
                        restrictionShapeFromPayload commutedRestrictionPayload
                   in addLinearPairAndMergeWithLaw
                        LawProjectionRestrictionFuse
                        (psDigest restrictionShape)
                        resultClass
                        (PlanProjectionNode composedProjectionShape baseClass)
                        (PlanRestrictionNode restrictionShape)
                        currentState
        _ ->
          Right currentState

applyCoverRewrites ::
  PlanRewriteSystem ->
  ClassId ->
  PlanShape 'Cover ->
  [ClassId] ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyCoverRewrites rewriteSystem resultClass planShape members state = do
  afterOrder <-
    if lawEnabled LawCoverMemberOrder rewriteSystem
      then applyCoverMemberOrder resultClass planShape members state
      else Right state
  if lawEnabled LawCoverSingleton rewriteSystem
    then Right (applyCoverSingleton resultClass planShape members afterOrder)
    else Right afterOrder

applyCoverMemberOrder ::
  ClassId ->
  PlanShape 'Cover ->
  [ClassId] ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyCoverMemberOrder resultClass planShape members state
  | canonicalMembers == members =
      Right state
  | otherwise =
      addNodeAndMergeWithLaw
        LawCoverMemberOrder
        (psDigest planShape)
        resultClass
        (PlanAmalgamationNode planShape canonicalMembers)
        state
  where
    canonicalMembers =
      canonicalCoverMembers (pwsGraph state) members

applyCoverSingleton ::
  ClassId ->
  PlanShape 'Cover ->
  [ClassId] ->
  PlanRewriteState ->
  PlanRewriteState
applyCoverSingleton resultClass planShape members state =
  case (coverSingletonEliminates (psPayload planShape), canonicalCoverMembers (pwsGraph state) members) of
    (Just targetDigest, [member])
      | knownShapeDigestInClass (pwsGraph state) member targetDigest ->
          mergeClassesForSimpleLaw LawCoverSingleton targetDigest resultClass member state
    _ ->
      state

canonicalCoverMembers :: EGraph PlanNode PlanAnalysis -> [ClassId] -> [ClassId]
canonicalCoverMembers graph =
  Set.toAscList . Set.fromList . fmap (canonicalizeClassId graph)

applyCoverageTransformRewrites ::
  PlanRewriteSystem ->
  ClassId ->
  PlanShape 'CoverageTransform ->
  ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
applyCoverageTransformRewrites rewriteSystem resultClass planShape sourceClass state =
  let afterIdentity =
        if lawEnabled LawCoverageTransformId rewriteSystem && psPayload planShape == CoveragePreserveExact
          then mergeClassesForSimpleLaw LawCoverageTransformId (psDigest planShape) resultClass sourceClass state
          else state
   in if lawEnabled LawCoverageTransformCompose rewriteSystem
        then applyCoverageTransformCompose resultClass planShape sourceClass afterIdentity
        else Right afterIdentity

matchProjectionNode :: PlanNode ClassId -> Maybe (PlanShape 'Projection, ClassId)
matchProjectionNode node =
  case node of
    PlanProjectionNode planShape sourceClass -> Just (planShape, sourceClass)
    _ -> Nothing

matchRestrictionNode :: PlanNode ClassId -> Maybe (PlanShape 'Restriction, ClassId)
matchRestrictionNode node =
  case node of
    PlanRestrictionNode planShape sourceClass -> Just (planShape, sourceClass)
    _ -> Nothing

matchCoverageTransformNode :: PlanNode ClassId -> Maybe (PlanShape 'CoverageTransform, ClassId)
matchCoverageTransformNode node =
  case node of
    PlanCoverageTransformNode planShape sourceClass -> Just (planShape, sourceClass)
    _ -> Nothing

addNodeAndMergeWithLaw ::
  PlanEqualityLaw ->
  StableDigest128 ->
  ClassId ->
  PlanNode ClassId ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
addNodeAndMergeWithLaw law sideConditionDigest resultClass replacementNode state = do
  (replacementClass, state1) <- addPlanENodeToState replacementNode state
  pure (mergeClassesForSimpleLaw law sideConditionDigest resultClass replacementClass state1)

addLinearPairAndMergeWithLaw ::
  PlanEqualityLaw ->
  StableDigest128 ->
  ClassId ->
  PlanNode ClassId ->
  (ClassId -> PlanNode ClassId) ->
  PlanRewriteState ->
  Either PlanSaturationError PlanRewriteState
addLinearPairAndMergeWithLaw law sideConditionDigest resultClass firstNode secondNode state0 = do
  (firstClass, state1) <- addPlanENodeToState firstNode state0
  (targetClass, state2) <- addPlanENodeToState (secondNode firstClass) state1
  pure (mergeClassesForSimpleLaw law sideConditionDigest resultClass targetClass state2)
