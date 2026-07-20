{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingStrategies #-}


module Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite,
    ContextSiteOwner,
    contextCachedContexts,
    contextEnumerableContexts,
    contextPreparedSite,
    contextGlobalRepresentative,
    contextClassSupportIndex,
    classesFor,
    contextAnalysisFor,
    contextAnalysisJoin,
    ContextClassLookupFailure (..),
    contextClassAt,
    contextEquivalentAt,
    restrictionMap,
    classSupportFor,
    propagationTargets,
    contextAnalysisAt,
    restrictAnalysisToTarget,
    restrictClassIdToTarget,
  )
where

import Data.Kind (Constraint, Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Context.Core
  ( ClassSiteSupport,
  )
import Moonlight.Sheaf.Context.Site
  ( ClassSupportIndex,
    PreparedContextSite,
    PreparedContextSupportError (..),
    classSupportExplicitCarrierForKey,
    contextObjectKeyFor,
    defaultPreparedSupport,
    preparedContextRestrictsTo,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
    supportCarrierMeet,
    supportCarrierToSupport,
    supportCarrierUnion,
  )
import Moonlight.Sheaf.Context.Section
  ( restrictClassIdWith,
  )

type ContextAlgebraSite :: Type -> Type -> Type -> Type -> Constraint
class (Ord ctx, DenseKey classId) => ContextAlgebraSite store ctx classId analysis | store -> ctx classId analysis where
  type ContextSiteOwner store :: Type
  contextPreparedSite :: store -> PreparedContextSite (ContextSiteOwner store) ctx
  contextCachedContexts :: store -> [ctx]
  contextGlobalRepresentative :: classId -> store -> classId
  contextClassSupportIndex :: store -> ClassSupportIndex (ContextSiteOwner store) ctx
  classesFor :: ctx -> store -> Either (PreparedContextSupportError ctx) (IntMap classId)
  contextAnalysisFor :: ctx -> store -> Either (PreparedContextSupportError ctx) (IntMap analysis)
  contextAnalysisJoin :: store -> analysis -> analysis -> analysis
  contextEnumerableContexts :: store -> [ctx]
  contextEnumerableContexts = contextCachedContexts

type ContextClassLookupFailure :: Type -> Type -> Type
data ContextClassLookupFailure ctx classId
  = ContextClassMissing !ctx !classId
  | ContextClassSupportFailed !(PreparedContextSupportError ctx)
  deriving stock (Eq, Ord, Show)

contextClassAt ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  classId ->
  store ->
  Either (ContextClassLookupFailure ctx classId) classId
contextClassAt contextValue classId store =
  do
    contextClasses <-
      either
        (Left . ContextClassSupportFailed)
        Right
        (classesFor contextValue store)
    let
      resolvedClass =
        case IntMap.lookup (encodeDenseKey classId) contextClasses of
          Just representative ->
            Just representative
          Nothing ->
            IntMap.lookup
              (encodeDenseKey (contextGlobalRepresentative classId store))
              contextClasses
    maybe
        (Left (ContextClassMissing contextValue classId))
        Right
        resolvedClass

contextEquivalentAt ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  classId ->
  classId ->
  store ->
  Either (ContextClassLookupFailure ctx classId) Bool
contextEquivalentAt contextValue leftClassId rightClassId store = do
  leftRepresentative <- contextClassAt contextValue leftClassId store
  rightRepresentative <- contextClassAt contextValue rightClassId store
  pure (leftRepresentative == rightRepresentative)

restrictionMap ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  ctx ->
  store ->
  Either (PreparedContextSupportError ctx) (IntMap classId)
restrictionMap sourceContext targetContext store =
  case preparedContextRestrictsTo (contextPreparedSite store) sourceContext targetContext of
    Right True -> do
      sourceClasses <- classesFor sourceContext store
      targetClasses <- classesFor targetContext store
      pure (IntMap.map (restrictClassIdWith targetClasses) sourceClasses)
    Right False ->
      Left (PreparedContextRestrictionUnavailable sourceContext targetContext)
    Left failureValue ->
      Left failureValue

classSupportFor :: ContextAlgebraSite store ctx classId analysis => classId -> store -> Either (PreparedContextSupportError ctx) (ClassSiteSupport ctx)
classSupportFor classId store =
  let site =
        contextPreparedSite store
      supportIndex =
        contextClassSupportIndex store
      canonicalClassId =
        contextGlobalRepresentative classId store
      directCarrier =
        classSupportExplicitCarrierForKey supportIndex (encodeDenseKey classId)
      canonicalCarrier =
        classSupportExplicitCarrierForKey supportIndex (encodeDenseKey canonicalClassId)
   in case (directCarrier, canonicalCarrier) of
        (Nothing, Nothing) -> Right (defaultPreparedSupport site)
        (Just carrierValue, Nothing) -> supportCarrierToSupport site carrierValue
        (Nothing, Just carrierValue) -> supportCarrierToSupport site carrierValue
        (Just leftCarrier, Just rightCarrier) ->
          supportCarrierToSupport site (supportCarrierUnion site leftCarrier rightCarrier)

propagationTargets :: ContextAlgebraSite store ctx classId analysis => ctx -> classId -> classId -> store -> Either (PreparedContextSupportError ctx) [ctx]
propagationTargets contextValue leftClassId rightClassId store = do
  leftSupport <- classSupportFor leftClassId store
  rightSupport <- classSupportFor rightClassId store
  let site =
        contextPreparedSite store
  leftCarrier <- supportCarrierFromSupport site leftSupport
  rightCarrier <- supportCarrierFromSupport site rightSupport
  let visibleTargets =
        supportCarrierMeet site leftCarrier rightCarrier
  candidateTargets <- sitePropagationTargets contextValue store
  keyedCandidateTargets <-
    traverse
      (\targetContext -> fmap ((,) targetContext) (contextObjectKeyFor site targetContext))
      candidateTargets
  pure
    [ targetContext
      | (targetContext, targetKey) <- keyedCandidateTargets,
        supportCarrierContainsKey site visibleTargets targetKey
    ]

contextAnalysisAt :: ContextAlgebraSite store ctx classId analysis => ctx -> Int -> store -> Either (PreparedContextSupportError ctx) (Maybe analysis)
contextAnalysisAt contextValue classKey store =
  fmap (IntMap.lookup classKey) (contextAnalysisFor contextValue store)

restrictAnalysisToTarget :: DenseKey classId => (analysis -> analysis -> analysis) -> IntMap classId -> IntMap analysis -> IntMap analysis
restrictAnalysisToTarget joinFn targetClasses =
  IntMap.mapKeysWith joinFn
    (\key -> encodeDenseKey (IntMap.findWithDefault (decodeDenseKey key) key targetClasses))

restrictClassIdToTarget :: ContextAlgebraSite store ctx classId analysis => store -> ctx -> classId -> Either (PreparedContextSupportError ctx) classId
restrictClassIdToTarget store targetContext classId =
  fmap (\targetClasses -> restrictClassIdWith targetClasses classId) (classesFor targetContext store)

sitePropagationTargets :: ContextAlgebraSite store ctx classId analysis => ctx -> store -> Either (PreparedContextSupportError ctx) [ctx]
sitePropagationTargets =
  smallSidePropagationTargets

smallSidePropagationTargets :: ContextAlgebraSite store ctx classId analysis => ctx -> store -> Either (PreparedContextSupportError ctx) [ctx]
smallSidePropagationTargets contextValue store =
  fmap
    (fmap fst . filter snd)
    ( traverse
        ( \candidateContext ->
            fmap
              ((,) candidateContext)
              (preparedContextRestrictsTo (contextPreparedSite store) candidateContext contextValue)
        )
        (Set.toAscList (Set.insert contextValue (Set.fromList (contextCachedContexts store))))
    )
