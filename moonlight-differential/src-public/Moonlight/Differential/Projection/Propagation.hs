{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Projection.Propagation
  ( ProjectionSupportIndex,
    ProjectionPropagationState (..),
    ProjectionPropagationReport (..),
    ProjectionCommit (..),
    buildDependencyIndexFromSupports,
    affectedContextsForDirtyKeys,
    reindexContextSupport,
    commitProjection,
    reportProjectionPropagationState,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Epoch (Version)

type ProjectionSupportIndex :: Type -> Type
type ProjectionSupportIndex context =
  IntMap (Set context)

type ProjectionPropagationState ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize =
  ProjectionPropagationState
    { cpsContextGraph :: !site,
      cpsCanonEpoch :: !(Maybe epoch),
      cpsRepairIndex :: !(Maybe repair),
      cpsImpacted :: !IntSet,
      cpsDirtyResults :: !IntSet,
      cpsVersion :: !Version,
      cpsPruneSignals :: !(Map context IntSet),
      cpsContextViews :: !(Map context view),
      cpsBaseToCtx :: !(ProjectionSupportIndex context),
      cpsEpochDelta :: !(Maybe epochDelta),
      cpsAnalysis :: !analysis,
      cpsCanonicalize :: !canonicalize
    }

type ProjectionPropagationReport ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data ProjectionPropagationReport site epoch repair context analysis canonicalize =
  ProjectionPropagationReport
    { cprContextGraph :: !site,
      cprCanonEpoch :: !(Maybe epoch),
      cprRepairIndex :: !(Maybe repair),
      cprImpacted :: !IntSet,
      cprDirtyResults :: !IntSet,
      cprVersion :: !Version,
      cprPruneSignals :: !(Map context IntSet),
      cprAnalysis :: !analysis,
      cprCanonicalize :: !canonicalize
  }

type ProjectionCommit :: Type -> Type
data ProjectionCommit state = ProjectionCommit
  { cpcState :: !state,
    cpcSectionChanged :: !Bool
  }

buildDependencyIndexFromSupports ::
  Ord context =>
  [context] ->
  (context -> IntSet) ->
  ProjectionSupportIndex context
buildDependencyIndexFromSupports contexts supportFor =
  IntMap.fromListWith
    Set.union
    [ (key, Set.singleton contextValue)
    | contextValue <- contexts
    , key <- IntSet.toAscList (supportFor contextValue)
    ]

affectedContextsForDirtyKeys ::
  Ord context =>
  IntSet ->
  ProjectionSupportIndex context ->
  [context]
affectedContextsForDirtyKeys dirtyKeys supportIndex =
  Set.toAscList (supportDependentsOfMany dirtyKeys supportIndex)

reindexContextSupport ::
  Ord context =>
  context ->
  IntSet ->
  IntSet ->
  ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize ->
  ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize
reindexContextSupport contextValue oldSupport newSupport stateValue =
  stateValue
    { cpsBaseToCtx =
        insertSupportDependencies
          newSupport
          contextValue
          ( removeSupportDependencies
              oldSupport
              contextValue
              (cpsBaseToCtx stateValue)
          )
    }

supportDependentsOfMany :: Ord context => IntSet -> ProjectionSupportIndex context -> Set context
supportDependentsOfMany keys supportIndex =
  foldMap (\key -> IntMap.findWithDefault Set.empty key supportIndex) (IntSet.toAscList keys)

insertSupportDependencies :: Ord context => IntSet -> context -> ProjectionSupportIndex context -> ProjectionSupportIndex context
insertSupportDependencies keys contextValue supportIndex =
  IntSet.foldl'
    (\current key -> IntMap.insertWith Set.union key (Set.singleton contextValue) current)
    supportIndex
    keys

removeSupportDependencies :: Ord context => IntSet -> context -> ProjectionSupportIndex context -> ProjectionSupportIndex context
removeSupportDependencies keys contextValue supportIndex =
  IntSet.foldl'
    (\current key -> IntMap.update (nonEmptySet . Set.delete contextValue) key current)
    supportIndex
    keys

nonEmptySet :: Set value -> Maybe (Set value)
nonEmptySet values
  | Set.null values = Nothing
  | otherwise = Just values

commitProjection ::
  Ord context =>
  (context -> section -> site -> site) ->
  (context -> site -> section -> section) ->
  (view -> IntSet) ->
  (context -> site -> IntSet) ->
  context ->
  view ->
  section ->
  IntSet ->
  Bool ->
  ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize ->
  ProjectionCommit (ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize)
commitProjection installSection normalizeSection viewSupport defaultSupport contextValue nextView rawNextSection dirtyResults sectionChanged stateValue =
  let currentSite =
        cpsContextGraph stateValue
      maybeOldView =
        Map.lookup contextValue (cpsContextViews stateValue)
      oldSupport =
        maybe (defaultSupport contextValue currentSite) viewSupport maybeOldView
      nextSupport =
        viewSupport nextView
      nextSection =
        normalizeSection contextValue currentSite rawNextSection
      nextSite =
        installSection contextValue nextSection currentSite
      nextState =
        (reindexContextSupport contextValue oldSupport nextSupport stateValue)
          { cpsContextGraph = nextSite,
            cpsContextViews =
              Map.insert contextValue nextView (cpsContextViews stateValue),
            cpsDirtyResults =
              IntSet.union (cpsDirtyResults stateValue) dirtyResults
          }
   in ProjectionCommit
        { cpcState = nextState,
          cpcSectionChanged = sectionChanged
        }

reportProjectionPropagationState ::
  ProjectionPropagationState site epoch repair context view epochDelta analysis canonicalize ->
  ProjectionPropagationReport site epoch repair context analysis canonicalize
reportProjectionPropagationState stateValue =
  ProjectionPropagationReport
    { cprContextGraph = cpsContextGraph stateValue,
      cprCanonEpoch = cpsCanonEpoch stateValue,
      cprRepairIndex = cpsRepairIndex stateValue,
      cprImpacted = cpsImpacted stateValue,
      cprDirtyResults = cpsDirtyResults stateValue,
      cprVersion = cpsVersion stateValue,
      cprPruneSignals = cpsPruneSignals stateValue,
      cprAnalysis = cpsAnalysis stateValue,
      cprCanonicalize = cpsCanonicalize stateValue
    }
