{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Context.Core
  ( ContextEGraph,
    ContextRuntimeState (..),
    cegBase,
    cegSite,
    cegLattice,
    ContextFiber (..),
    cegContextFibers,
    contextAuthoredUnionPairs,
    cegClassSupportIndex,
    cegContextAnalysisDeltas,
    cegContextRevision,
    cegRuntimeState,
    contextPreparedObjects,
    contextCachedObjectsForExecution,
    activateContext,
    ContextEGraphObstructionFailure (..),
    ContextIncidenceMaterializationError (..),
    ContextPayloadLookupError (..),
    ContextRestrictionMismatchError (..),
    cachedContextPayloadFor,
    requireCachedContextPayloadFor,
    contextVisibleClassKeys,
    contextRepresentativeAt,
    contextAnalysisValueAt,
    deriveContextAnalysisDeltaAtKey,
    ambientRepresentativeAnalysisFor,
    ambientRepresentativeAnalysisValuesFor,
    materializeAmbientPayloadFor,
    materializeContextPayloadFor,
    lookupContextPayload,
    checkedContextRestrictionMismatchesAt,
    materializeIncidenceCategoryFromSnapshot,
    materializeIncidenceSiteFromSnapshot,
    graphLiveCanonicalClassMap,
    baseCanonicalClassMap,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (asJoin, asJoinChanged))
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( annotatedRepresentativeKeyAt,
    annotatedRepresentativeMapAt,
    contextAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedView
  ( annotatedContextViewAtKey,
    annotatedContextViewFromRepresentativeMapAtKey,
    annotatedRowsChildrenByRepresentativeWithin,
    annotatedViewCanonicalize,
    annotatedViewRowsByRepresentative,
    annotatedViewRowsByRepresentativeWithin,
  )
import Moonlight.EGraph.Pure.Context.Internal.Store
  ( ContextEGraph,
    ContextFiber (..),
    ContextPayloadLookupError (..),
    ContextRuntimeState (..),
  )
import Moonlight.EGraph.Pure.Context.Internal.Store qualified as Store
import Moonlight.EGraph.Pure.Rebuild (repairAnalysisFromRows)
import Moonlight.EGraph.Pure.Structural.Store (structuralRepairClosure)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphAnalysisSpec,
    eGraphStore,
    eGraphUnionFind,
  )
import Moonlight.EGraph.Sheaf.IncidenceSite
  ( EGraphIncidenceCategory,
    EGraphIncidenceCategoryError,
    EGraphIncidenceTag,
    defaultEGraphIncidenceNerveDepth,
    egraphIncidenceCategoryFromSnapshot,
    egraphIncidenceNerveSite,
  )
import Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite (..),
    ContextClassLookupFailure (..),
    contextEquivalentAt,
  )
import Moonlight.Sheaf.Context.Core
  ( SectionMismatch,
  )
import Moonlight.Sheaf.Context.Site
  ( ClassSupportIndex,
    ContextObjectKey,
    PreparedContextSite,
    PreparedContextSupportError,
    classKeysVisibleAtKey,
    classSupportIndexEntries,
    classSupportIndexExplicitClassKeys,
    contextFragmentLattice,
    contextObjectKeyFor,
    preparedContextFragment,
    preparedDefaultContext,
    preparedContextRestrictsTo,
    preparedJoinClosureOver,
  )
import Moonlight.Sheaf.Obstruction (ContextualObstructionStore (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( equivalencePairs,
  )
import Moonlight.Sheaf.Section.Context.Payload
  ( ContextClassPayload (..),
    payloadMapFromSections,
    payloadMapToAnalysisMap,
    payloadMapToRepresentativeMap,
    payloadRestrictionMismatchesToTarget,
  )
import Moonlight.Sheaf.Site (NerveSite)
import Moonlight.FiniteLattice
  ( ContextLattice,
    supportGenerators
  )

cegBase :: ContextEGraph owner f a c -> EGraph f a
cegBase =
  Store.cegBase

cegSite :: ContextEGraph owner f a c -> PreparedContextSite owner c
cegSite =
  Store.cegSite

cegLattice :: ContextEGraph owner f a c -> ContextLattice c
cegLattice =
  contextFragmentLattice . preparedContextFragment . Store.cegSite

cegContextFibers :: ContextEGraph owner f a c -> Map c Store.ContextFiber
cegContextFibers =
  Store.cegContextFibers

-- | Union pairs authored AT exactly this context; each pair relates a member
-- to its delta leader. This is the raw stored delta, not the observable
-- relation; observable closure is derived by the regional forest.
contextAuthoredUnionPairs :: Ord c => c -> ContextEGraph owner f a c -> [(ClassId, ClassId)]
contextAuthoredUnionPairs contextValue contextGraph =
  maybe
    []
    (equivalencePairs . Store.cfRelation)
    (Map.lookup contextValue (Store.cegContextFibers contextGraph))

cegClassSupportIndex :: ContextEGraph owner f a c -> ClassSupportIndex owner c
cegClassSupportIndex =
  Store.cegClassSupport

cegContextAnalysisDeltas :: ContextEGraph owner f a c -> Map c (IntMap a)
cegContextAnalysisDeltas =
  Store.cegContextAnalysisDeltas

cegContextRevision :: ContextEGraph owner f a c -> Natural
cegContextRevision =
  Store.cegContextRevision

cegRuntimeState :: ContextEGraph owner f a c -> Store.ContextRuntimeState c
cegRuntimeState =
  Store.cegRuntimeState

contextPreparedObjects :: Ord c => ContextEGraph owner f a c -> [c]
contextPreparedObjects =
  contextEnumerableObjectsForExecution
{-# INLINE contextPreparedObjects #-}

contextEnumerableObjectsForExecution :: Ord c => ContextEGraph owner f a c -> [c]
contextEnumerableObjectsForExecution contextGraph =
  fst
    ( preparedJoinClosureOver
        (cegSite contextGraph)
        (Set.toAscList (contextEnumerableSeedObjects contextGraph))
    )
{-# INLINE contextEnumerableObjectsForExecution #-}

contextEnumerableSeedObjects :: Ord c => ContextEGraph owner f a c -> Set.Set c
contextEnumerableSeedObjects contextGraph =
  Set.unions
    [ Set.singleton (preparedDefaultContext (cegSite contextGraph)),
      Map.keysSet (cegContextFibers contextGraph),
      Map.keysSet (cegContextAnalysisDeltas contextGraph),
      Store.crsDirtyContexts (cegRuntimeState contextGraph),
      contextSupportGeneratorObjects contextGraph
    ]
{-# INLINE contextEnumerableSeedObjects #-}

contextSupportGeneratorObjects :: Ord c => ContextEGraph owner f a c -> Set.Set c
contextSupportGeneratorObjects contextGraph =
  either
    (const Set.empty)
    (Set.fromList . foldMap supportGenerators . IntMap.elems)
    (classSupportIndexEntries (cegSite contextGraph) (cegClassSupportIndex contextGraph))
{-# INLINE contextSupportGeneratorObjects #-}

contextCachedObjectsForExecution :: ContextEGraph owner f a c -> [c]
contextCachedObjectsForExecution =
  Set.toAscList . Map.keysSet . cegContextAnalysisDeltas
{-# INLINE contextCachedObjectsForExecution #-}

activateContext ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (ContextEGraph owner f a c)
activateContext contextValue contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  pure
    contextGraph
      { Store.cegContextAnalysisDeltas =
          Map.insert
            contextValue
            (deriveContextAnalysisDeltaAtKey contextKey contextGraph)
            (cegContextAnalysisDeltas contextGraph)
      }
{-# INLINE activateContext #-}

graphLiveCanonicalClassMap :: ContextEGraph owner f a c -> EGraph f a -> IntMap ClassId
graphLiveCanonicalClassMap contextGraph graph =
  let rawKeys =
        IntSet.unions
          [ IntMap.keysSet (eGraphAnalysis (cegBase contextGraph)),
            IntMap.keysSet (eGraphAnalysis graph),
            classSupportIndexExplicitClassKeys (cegClassSupportIndex contextGraph)
          ]
      representativeKeys =
        IntSet.map
          (classIdKey . canonicalizeClassKey graph)
          rawKeys
      domainKeys =
        IntSet.union rawKeys representativeKeys
   in IntMap.fromSet
        (canonicalizeClassKey graph)
        domainKeys
  where
    canonicalizeClassKey :: EGraph f a -> Int -> ClassId
    canonicalizeClassKey graphValue classKey =
      canonicalizeClassId graphValue (ClassId classKey)
{-# INLINE graphLiveCanonicalClassMap #-}

baseCanonicalClassMap :: ContextEGraph owner f a c -> IntMap ClassId
baseCanonicalClassMap contextGraph =
  graphLiveCanonicalClassMap contextGraph (cegBase contextGraph)
{-# INLINE baseCanonicalClassMap #-}

contextVisibleClassKeys ::
  Ord c =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) IntSet.IntSet
contextVisibleClassKeys contextValue contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  let representatives =
        ambientRepresentativeMapAtKeyWith
          (baseCanonicalClassMap contextGraph)
          contextKey
          contextGraph
  pure (contextVisibleClassKeysAtKeyWith representatives contextKey contextGraph)
{-# INLINE contextVisibleClassKeys #-}

contextVisibleClassKeysAtKeyWith ::
  IntMap ClassId ->
  ContextObjectKey owner ->
  ContextEGraph owner f a c ->
  IntSet.IntSet
contextVisibleClassKeysAtKeyWith canonicalClasses contextKey contextGraph =
  let supportIndex =
        cegClassSupportIndex contextGraph
      site =
        cegSite contextGraph
      supportedClassKeys =
        classSupportIndexExplicitClassKeys supportIndex
      implicitGlobalKeys =
        IntSet.difference (IntMap.keysSet canonicalClasses) supportedClassKeys
      visibleSeedKeys =
        IntSet.union
          (classKeysVisibleAtKey site supportIndex contextKey)
          implicitGlobalKeys
      visibleRepresentatives =
        IntSet.map
          (classIdKey . baseRepresentativeAtKey canonicalClasses)
          visibleSeedKeys
   in IntSet.filter
        (\classKey -> IntSet.member (classIdKey (baseRepresentativeAtKey canonicalClasses classKey)) visibleRepresentatives)
        (IntMap.keysSet canonicalClasses)
{-# INLINE contextVisibleClassKeysAtKeyWith #-}

baseRepresentativeAtKey :: IntMap ClassId -> Int -> ClassId
baseRepresentativeAtKey representatives classKey =
  IntMap.findWithDefault (ClassId classKey) classKey representatives
{-# INLINE baseRepresentativeAtKey #-}

contextRepresentativeAt ::
  Ord c =>
  c ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) ClassId
contextRepresentativeAt contextValue classId contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  pure (contextRepresentativeAtKey contextKey classId contextGraph)
{-# INLINE contextRepresentativeAt #-}

contextRepresentativeAtKey :: ContextObjectKey owner -> ClassId -> ContextEGraph owner f a c -> ClassId
contextRepresentativeAtKey contextKey classId contextGraph =
  let baseRepresentative =
        canonicalizeClassId (cegBase contextGraph) classId
   in ClassId
        ( annotatedRepresentativeKeyAt
            contextKey
            (contextAnnotatedDeltaBuckets contextGraph)
            (classIdKey baseRepresentative)
        )
{-# INLINE contextRepresentativeAtKey #-}

contextAnalysisValueAt ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (Maybe a)
contextAnalysisValueAt contextValue classId contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  let baseRepresentative = canonicalizeClassId (cegBase contextGraph) classId
      baseRepresentativeKey = classIdKey baseRepresentative
      (regionalRepresentatives, contextAnalysis) =
        ambientRepresentativeAnalysisValuesAtKey
          contextValue
          contextKey
          (IntSet.singleton baseRepresentativeKey)
          contextGraph
      representativeKey =
        IntMap.findWithDefault
          baseRepresentativeKey
          baseRepresentativeKey
          regionalRepresentatives
  pure (IntMap.lookup representativeKey contextAnalysis)
{-# INLINE contextAnalysisValueAt #-}

ambientRepresentativeMapAtKeyWith ::
  IntMap ClassId ->
  ContextObjectKey owner ->
  ContextEGraph owner f a c ->
  IntMap ClassId
ambientRepresentativeMapAtKeyWith baseRepresentatives contextKey contextGraph =
  ambientRepresentativeMapWithRegional
    baseRepresentatives
    ( annotatedRepresentativeMapAt
        contextKey
        (contextAnnotatedDeltaBuckets contextGraph)
    )
{-# INLINE ambientRepresentativeMapAtKeyWith #-}

ambientRepresentativeMapWithRegional ::
  IntMap ClassId ->
  IntMap Int ->
  IntMap ClassId
ambientRepresentativeMapWithRegional baseRepresentatives regionalRepresentatives =
  let contextualRepresentative baseRepresentative =
        let baseRepresentativeKey = classIdKey baseRepresentative
         in ClassId
              ( IntMap.findWithDefault
                  baseRepresentativeKey
                  baseRepresentativeKey
                  regionalRepresentatives
              )
      contextualRepresentatives =
        fmap contextualRepresentative baseRepresentatives
      contextualRepresentativeKeys =
        IntSet.fromList (fmap classIdKey (IntMap.elems contextualRepresentatives))
      domainKeys =
        IntSet.union (IntMap.keysSet baseRepresentatives) contextualRepresentativeKeys
   in IntMap.fromSet
        (contextualRepresentative . baseRepresentativeAtKey baseRepresentatives)
        domainKeys
{-# INLINE ambientRepresentativeMapWithRegional #-}

contextualBaseAnalysisOverridesWith ::
  IntMap Int ->
  EGraph f a ->
  IntMap a
contextualBaseAnalysisOverridesWith regionalRepresentatives baseGraph =
  IntMap.foldlWithKey'
    mergeAbsorbedAnalysis
    representativeBaseAnalysis
    regionalRepresentatives
  where
    baseAnalysis = eGraphAnalysis baseGraph
    analysisSpec = eGraphAnalysisSpec baseGraph
    representativeBaseAnalysis =
      IntMap.restrictKeys
        baseAnalysis
        (IntSet.fromList (IntMap.elems regionalRepresentatives))

    mergeAbsorbedAnalysis contextualAnalysis absorbedKey representativeKey =
      case IntMap.lookup absorbedKey baseAnalysis of
        Nothing -> contextualAnalysis
        Just absorbedAnalysis ->
          IntMap.insertWith
            (flip (asJoin analysisSpec))
            representativeKey
            absorbedAnalysis
            contextualAnalysis
{-# INLINE contextualBaseAnalysisOverridesWith #-}

deriveContextAnalysisDeltaAtKey ::
  Language f =>
  ContextObjectKey owner ->
  ContextEGraph owner f a c ->
  IntMap a
deriveContextAnalysisDeltaAtKey contextKey contextGraph =
  contextAnalysisDelta
    (deriveContextAnalysisSectionsAtKey contextKey contextGraph)
{-# INLINE deriveContextAnalysisDeltaAtKey #-}

data ContextAnalysisSections a = ContextAnalysisSections
  { contextAnalysisRegionalRepresentatives :: ~(IntMap Int),
    contextAnalysisDelta :: ~(IntMap a),
    contextAnalysisCanonicalBase :: ~(IntMap a)
  }

deriveContextAnalysisSectionsAtKey ::
  Language f =>
  ContextObjectKey owner ->
  ContextEGraph owner f a c ->
  ContextAnalysisSections a
deriveContextAnalysisSectionsAtKey contextKey contextGraph =
  ContextAnalysisSections
    { contextAnalysisRegionalRepresentatives = regionalRepresentatives,
      contextAnalysisDelta = analysisDelta,
      contextAnalysisCanonicalBase = canonicalBaseAnalysis
    }
  where
    analysisDelta =
      IntMap.filterWithKey
        differsFromBaseAnalysis
        (IntMap.restrictKeys repairedAnalysis repairKeys)

    differsFromBaseAnalysis classKey analysisValue =
      maybe
        True
        (not . analysisValuesEquivalent analysisSpec analysisValue)
        (IntMap.lookup classKey (eGraphAnalysis baseGraph))

    baseGraph = cegBase contextGraph
    analysisSpec = eGraphAnalysisSpec baseGraph
    buckets = contextAnnotatedDeltaBuckets contextGraph
    annotatedView =
      annotatedContextViewFromRepresentativeMapAtKey
        contextKey
        regionalRepresentatives
        buckets
    regionalRepresentatives =
      annotatedRepresentativeMapAt contextKey buckets
    regionalRepresentativeRoots =
      IntSet.fromList (IntMap.elems regionalRepresentatives)
    regionallyTouchedKeys =
      IntSet.union
        (IntMap.keysSet regionalRepresentatives)
        regionalRepresentativeRoots
    repairKeys =
      IntSet.map
        (\classKey -> IntMap.findWithDefault classKey classKey regionalRepresentatives)
        (structuralRepairClosure (eGraphStore baseGraph) regionallyTouchedKeys)
    rowsByRepresentative =
      annotatedViewRowsByRepresentativeWithin repairKeys annotatedView baseGraph
    canonicalBaseAnalysis =
      IntMap.union canonicalBaseAnalysisOverrides retainedBaseAnalysis
    canonicalBaseAnalysisOverrides =
      contextualBaseAnalysisOverridesWith regionalRepresentatives baseGraph
    retainedBaseAnalysis =
      IntMap.withoutKeys
        (eGraphAnalysis baseGraph)
        (IntMap.keysSet regionalRepresentatives)
    childrenWithin =
      annotatedRowsChildrenByRepresentativeWithin repairKeys rowsByRepresentative
    repairedAnalysis =
      repairAnalysisFromRows
        analysisSpec
        id
        (\representativeKey -> IntMap.findWithDefault [] representativeKey rowsByRepresentative)
        childrenWithin
        regionalRepresentativeRoots
        repairKeys
        canonicalBaseAnalysis

{-# INLINE deriveContextAnalysisSectionsAtKey #-}

analysisValuesEquivalent :: AnalysisSpec f a -> a -> a -> Bool
analysisValuesEquivalent analysisSpec leftValue rightValue =
  not leftChanges && not rightChanges
  where
    (_, leftChanges) = asJoinChanged analysisSpec leftValue rightValue
    (_, rightChanges) = asJoinChanged analysisSpec rightValue leftValue
{-# INLINE analysisValuesEquivalent #-}

ambientRepresentativeAnalysisAtKey ::
  (Language f, Ord c) =>
  c ->
  ContextObjectKey owner ->
  ContextEGraph owner f a c ->
  (IntMap Int, IntMap a)
ambientRepresentativeAnalysisAtKey contextValue contextKey contextGraph =
  ( contextAnalysisRegionalRepresentatives sections,
    IntMap.union selectedAnalysisDelta (contextAnalysisCanonicalBase sections)
  )
  where
    sections = deriveContextAnalysisSectionsAtKey contextKey contextGraph
    selectedAnalysisDelta =
      Map.findWithDefault
        (contextAnalysisDelta sections)
        contextValue
        (cegContextAnalysisDeltas contextGraph)
{-# INLINE ambientRepresentativeAnalysisAtKey #-}

ambientRepresentativeAnalysisFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap Int, IntMap a)
ambientRepresentativeAnalysisFor contextValue contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  pure (ambientRepresentativeAnalysisAtKey contextValue contextKey contextGraph)
{-# INLINE ambientRepresentativeAnalysisFor #-}

ambientRepresentativeAnalysisValuesAtKey ::
  (Language f, Ord c) =>
  c ->
  ContextObjectKey owner ->
  IntSet.IntSet ->
  ContextEGraph owner f a c ->
  (IntMap Int, IntMap a)
ambientRepresentativeAnalysisValuesAtKey contextValue contextKey requestedBaseKeys contextGraph =
  (regionalRepresentatives, requestedAnalysis)
  where
    sections = deriveContextAnalysisSectionsAtKey contextKey contextGraph
    cachedAnalysisDelta = Map.lookup contextValue (cegContextAnalysisDeltas contextGraph)
    regionalRepresentatives =
      case cachedAnalysisDelta of
        Just _ ->
          annotatedRepresentativeMapAt
            contextKey
            (contextAnnotatedDeltaBuckets contextGraph)
        Nothing -> contextAnalysisRegionalRepresentatives sections
    requestedRepresentativeKeys =
      IntSet.union
        (IntSet.difference requestedBaseKeys (IntMap.keysSet regionalRepresentatives))
        ( IntSet.fromList
            ( IntMap.elems
                (IntMap.restrictKeys regionalRepresentatives requestedBaseKeys)
            )
        )
    selectedAnalysisDelta =
      maybe (contextAnalysisDelta sections) id cachedAnalysisDelta
    requestedAnalysis =
      IntMap.unions
        [ IntMap.restrictKeys selectedAnalysisDelta requestedRepresentativeKeys,
          IntMap.restrictKeys
            (eGraphAnalysis (cegBase contextGraph))
            requestedRepresentativeKeys
        ]
{-# INLINE ambientRepresentativeAnalysisValuesAtKey #-}

ambientRepresentativeAnalysisValuesFor ::
  (Language f, Ord c) =>
  c ->
  IntSet.IntSet ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap Int, IntMap a)
ambientRepresentativeAnalysisValuesFor contextValue requestedBaseKeys contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  pure
    ( ambientRepresentativeAnalysisValuesAtKey
        contextValue
        contextKey
        requestedBaseKeys
        contextGraph
    )
{-# INLINE ambientRepresentativeAnalysisValuesFor #-}

ambientAnalysisMapFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap a)
ambientAnalysisMapFor contextValue contextGraph =
  fmap snd (ambientRepresentativeAnalysisFor contextValue contextGraph)
{-# INLINE ambientAnalysisMapFor #-}

materializeAmbientPayloadFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap (ContextClassPayload ClassId a))
materializeAmbientPayloadFor contextValue contextGraph =
  IntMap.mapWithKey
    ( \representativeKey analysisValue ->
        ContextClassPayload
          { ccpRepresentative = ClassId representativeKey,
            ccpAnalysis = analysisValue
          }
    )
    <$> ambientAnalysisMapFor contextValue contextGraph
{-# INLINE materializeAmbientPayloadFor #-}

materializeContextPayloadFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap (ContextClassPayload ClassId a))
materializeContextPayloadFor contextValue contextGraph =
  materializeContextPayloadForWith (baseCanonicalClassMap contextGraph) contextValue contextGraph
{-# INLINE materializeContextPayloadFor #-}

materializeContextPayloadForWith ::
  (Language f, Ord c) =>
  IntMap ClassId ->
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap (ContextClassPayload ClassId a))
materializeContextPayloadForWith baseRepresentatives contextValue contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  let (regionalRepresentatives, contextualAnalysis) =
        ambientRepresentativeAnalysisAtKey contextValue contextKey contextGraph
      representatives =
        ambientRepresentativeMapWithRegional baseRepresentatives regionalRepresentatives
      visibleKeys =
        contextVisibleClassKeysAtKeyWith representatives contextKey contextGraph
      visibleRepresentatives =
        IntMap.restrictKeys representatives visibleKeys
      visibleAnalysisKeys =
        IntSet.union
          visibleKeys
          (IntSet.fromList (fmap classIdKey (IntMap.elems visibleRepresentatives)))
  let visibleAnalysis =
        IntMap.restrictKeys contextualAnalysis visibleAnalysisKeys
  pure (payloadMapFromSections visibleRepresentatives visibleAnalysis)
{-# INLINE materializeContextPayloadForWith #-}

cachedContextPayloadFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either
    (PreparedContextSupportError c)
    (Maybe (IntMap (ContextClassPayload ClassId a)))
cachedContextPayloadFor contextValue contextGraph =
  if Map.member contextValue (cegContextAnalysisDeltas contextGraph)
    then fmap Just (materializeAmbientPayloadFor contextValue contextGraph)
    else Nothing <$ contextObjectKeyFor (cegSite contextGraph) contextValue
{-# INLINE cachedContextPayloadFor #-}

requireCachedContextPayloadFor ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either
    (ContextRestrictionMismatchError c (ContextPayloadLookupError c))
    (IntMap (ContextClassPayload ClassId a))
requireCachedContextPayloadFor contextValue contextGraph =
  first ContextRestrictionSupportLookup
    (cachedContextPayloadFor contextValue contextGraph)
    >>= maybe
      (Left (ContextRestrictionPayloadLookup (MissingContextPayload contextValue)))
      Right
{-# INLINE requireCachedContextPayloadFor #-}

lookupContextPayload ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either
    (ContextRestrictionMismatchError c (ContextPayloadLookupError c))
    (Maybe (ContextClassPayload ClassId a))
lookupContextPayload contextValue classId contextGraph =
  fmap
    (IntMap.lookup (classIdKey classId))
    (requireCachedContextPayloadFor contextValue contextGraph)
{-# INLINE lookupContextPayload #-}

instance (Language f, Ord c) => ContextAlgebraSite (ContextEGraph owner f a c) c ClassId a where
  type ContextSiteOwner (ContextEGraph owner f a c) = owner

  contextPreparedSite = cegSite

  contextCachedContexts = contextCachedObjectsForExecution

  contextEnumerableContexts =
    contextPreparedObjects

  contextGlobalRepresentative classId contextGraph =
    fst (UnionFind.find classId (eGraphUnionFind (cegBase contextGraph)))

  contextClassSupportIndex = cegClassSupportIndex

  classesFor =
    contextClassesFor

  contextAnalysisFor contextValue contextGraph =
    fmap
      payloadMapToAnalysisMap
      (materializeContextPayloadFor contextValue contextGraph)

  contextAnalysisJoin contextGraph =
    asJoin (eGraphAnalysisSpec (cegBase contextGraph))

contextClassesFor ::
  Ord c =>
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) (IntMap ClassId)
contextClassesFor contextValue contextGraph = do
  contextKey <- contextObjectKeyFor (cegSite contextGraph) contextValue
  let contextRepresentatives =
        ambientRepresentativeMapAtKeyWith
          (baseCanonicalClassMap contextGraph)
          contextKey
          contextGraph
  pure
    ( IntMap.restrictKeys
        contextRepresentatives
        (contextVisibleClassKeysAtKeyWith contextRepresentatives contextKey contextGraph)
    )
{-# INLINE contextClassesFor #-}

type ContextEGraphObstructionFailure :: Type -> Type
data ContextEGraphObstructionFailure c
  = ContextEGraphObstructionClassLookup !(ContextClassLookupFailure c ClassId)
  | ContextEGraphObstructionRestrictionLookup
      !(ContextRestrictionMismatchError c (ContextPayloadLookupError c))
  deriving stock (Eq, Ord, Show)

type ContextRestrictionMismatchError :: Type -> Type -> Type
data ContextRestrictionMismatchError c payloadError
  = ContextRestrictionPayloadLookup !payloadError
  | ContextRestrictionSupportLookup !(PreparedContextSupportError c)
  deriving stock (Eq, Ord, Show)

type ContextIncidenceMaterializationError :: (Type -> Type) -> Type -> Type
data ContextIncidenceMaterializationError f c
  = ContextIncidenceSupportLookup !(PreparedContextSupportError c)
  | ContextIncidenceCategoryFailed !(EGraphIncidenceCategoryError f)
  deriving stock (Eq, Show)

instance (Language f, Ord c, Eq a) =>
  ContextualObstructionStore
    (ContextEGraph owner f a c)
    c
    ClassId
    (ENode f)
    (SectionMismatch ClassId a)
    (ContextEGraphObstructionFailure c)
  where
  obstructionContexts =
    contextPreparedObjects

  obstructionEquivalentAt contextValue leftClassId rightClassId =
    first ContextEGraphObstructionClassLookup
      . contextEquivalentAt contextValue leftClassId rightClassId

  obstructionStructuralPairsAt leftClassId rightClassId contextValue contextGraph =
    case contextObjectKeyFor (cegSite contextGraph) contextValue of
      Left _ ->
        -- The obstruction protocol asks equivalence first and reports that
        -- typed lookup failure before requesting structural pairs.
        []
      Right contextKey ->
        let annotatedView =
              annotatedContextViewAtKey
                contextKey
                (contextAnnotatedDeltaBuckets contextGraph)
            rowsByRepresentative =
              annotatedViewRowsByRepresentative annotatedView (cegBase contextGraph)
            nodesOf classId =
              IntMap.findWithDefault
                []
                (classIdKey (annotatedViewCanonicalize annotatedView (cegBase contextGraph) classId))
                rowsByRepresentative
            leftNodes = nodesOf leftClassId
            rightNodes = nodesOf rightClassId
         in [(leftNode, rightNode) | leftNode <- leftNodes, rightNode <- rightNodes]

  obstructionRestrictionStatsAt contextValue contextGraph =
    first
      ContextEGraphObstructionRestrictionLookup
      (contextRestrictionMismatchesAt contextValue contextGraph)

  obstructionPropagationFailure =
    const Nothing

checkedContextRestrictionMismatchesAt ::
  (Language f, Ord c, Eq a) =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextRestrictionMismatchError c (ContextPayloadLookupError c)) [SectionMismatch ClassId a]
checkedContextRestrictionMismatchesAt =
  contextRestrictionMismatchesWith
    requireCachedContextPayloadFor
    cachedRestrictionNeighborContexts
{-# INLINE checkedContextRestrictionMismatchesAt #-}

contextRestrictionMismatchesAt ::
  (Language f, Ord c, Eq a) =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextRestrictionMismatchError c (ContextPayloadLookupError c)) [SectionMismatch ClassId a]
contextRestrictionMismatchesAt contextValue contextGraph =
  let sharedBaseRepresentatives =
        baseCanonicalClassMap contextGraph
      visiblePayloadFor sourceContext sourceGraph =
        first
          ContextRestrictionSupportLookup
          (materializeContextPayloadForWith sharedBaseRepresentatives sourceContext sourceGraph)
   in contextRestrictionMismatchesWith
        visiblePayloadFor
        totalRestrictionNeighborContexts
        contextValue
        contextGraph
{-# INLINE contextRestrictionMismatchesAt #-}

contextRestrictionMismatchesWith ::
  Eq a =>
  (c -> ContextEGraph owner f a c -> Either errorValue (IntMap (ContextClassPayload ClassId a))) ->
  (c -> ContextEGraph owner f a c -> Either errorValue [c]) ->
  c ->
  ContextEGraph owner f a c ->
  Either errorValue [SectionMismatch ClassId a]
contextRestrictionMismatchesWith payloadSource neighborSource contextValue contextGraph = do
  targetPayloads <- payloadSource contextValue contextGraph
  neighborContexts <- neighborSource contextValue contextGraph
  let targetClasses = payloadMapToRepresentativeMap targetPayloads
      mismatchesFrom sourceContext = do
        sourcePayloads <- payloadSource sourceContext contextGraph
        Right
          ( payloadRestrictionMismatchesToTarget
              (asJoin (eGraphAnalysisSpec (cegBase contextGraph)))
              targetClasses
              sourceContext
              contextValue
              sourcePayloads
              targetPayloads
          )
  fold <$> traverse mismatchesFrom neighborContexts
{-# INLINE contextRestrictionMismatchesWith #-}

cachedRestrictionNeighborContexts ::
  Ord c =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextRestrictionMismatchError c payloadError) [c]
cachedRestrictionNeighborContexts contextValue contextGraph =
  first
    ContextRestrictionSupportLookup
    (restrictionNeighborContextsFrom (contextCachedObjectsForExecution contextGraph) contextValue contextGraph)
{-# INLINE cachedRestrictionNeighborContexts #-}

totalRestrictionNeighborContexts ::
  Ord c =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextRestrictionMismatchError c payloadError) [c]
totalRestrictionNeighborContexts contextValue contextGraph =
  first
    ContextRestrictionSupportLookup
    (restrictionNeighborContextsFrom (contextPreparedObjects contextGraph) contextValue contextGraph)
{-# INLINE totalRestrictionNeighborContexts #-}

restrictionNeighborContextsFrom ::
  Ord c =>
  [c] ->
  c ->
  ContextEGraph owner f a c ->
  Either (PreparedContextSupportError c) [c]
restrictionNeighborContextsFrom candidates contextValue contextGraph =
  fmap
    (fmap fst . filter snd)
    ( traverse
        ( \sourceContext ->
            fmap
              ((,) sourceContext)
              (preparedContextRestrictsTo (cegSite contextGraph) sourceContext contextValue)
        )
        (filter (/= contextValue) candidates)
    )
{-# INLINE restrictionNeighborContextsFrom #-}

materializeIncidenceCategoryFromSnapshot ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextIncidenceMaterializationError f c) (EGraphIncidenceCategory f)
materializeIncidenceCategoryFromSnapshot contextValue contextGraph = do
  contextKey <-
    first ContextIncidenceSupportLookup
      (contextObjectKeyFor (cegSite contextGraph) contextValue)
  visibleClassKeySet <-
    first ContextIncidenceSupportLookup (contextVisibleClassKeys contextValue contextGraph)
  let annotatedView =
        annotatedContextViewAtKey
          contextKey
          (contextAnnotatedDeltaBuckets contextGraph)
      rowsByRepresentative =
        annotatedViewRowsByRepresentative annotatedView (cegBase contextGraph)
      visibleClasses =
        IntMap.restrictKeys
          ( ambientRepresentativeMapAtKeyWith
              (baseCanonicalClassMap contextGraph)
              contextKey
              contextGraph
          )
          visibleClassKeySet
      visibleCanonicalClasses =
        IntMap.fromList
          (fmap (\representative -> (classIdKey representative, representative)) (IntMap.elems visibleClasses))
      visibleClassKeys = IntMap.keysSet visibleCanonicalClasses
      membership =
        IntMap.mapWithKey
          (\classKey _ -> visibleENodes visibleClassKeys (IntMap.lookup classKey rowsByRepresentative))
          visibleCanonicalClasses
      incidenceUnionFind =
        IntMap.foldlWithKey'
          (\unionFind classKey representative ->
              UnionFind.union (ClassId classKey) representative unionFind
          )
          UnionFind.emptyUnionFind
          visibleClasses
  first
    ContextIncidenceCategoryFailed
    (egraphIncidenceCategoryFromSnapshot incidenceUnionFind membership)

materializeIncidenceSiteFromSnapshot ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either
    (ContextIncidenceMaterializationError f c)
    (NerveSite (EGraphIncidenceTag f))
materializeIncidenceSiteFromSnapshot contextValue contextGraph =
  fmap
    (`egraphIncidenceNerveSite` defaultEGraphIncidenceNerveDepth)
    (materializeIncidenceCategoryFromSnapshot contextValue contextGraph)

visibleENodes :: Language f => IntSet.IntSet -> Maybe [ENode f] -> [ENode f]
visibleENodes visibleClassKeys =
  maybe
    []
    (filter (enodeArgumentsVisible visibleClassKeys))

enodeArgumentsVisible :: Language f => IntSet.IntSet -> ENode f -> Bool
enodeArgumentsVisible visibleClassKeys (ENode nodeValue) =
  all
    (\classId -> IntSet.member (classIdKey classId) visibleClassKeys)
    (toList nodeValue)
