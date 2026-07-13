-- | Canonical digests for timed graphs, contexts, terminations, and verdicts.
module Moonlight.EGraph.Bench.Harness.Digest
  ( graphDigest,
    contextGraphDigest,
    SemanticQuery (..),
    SemanticDigestObstruction (..),
    QuotientObservation (..),
    ContextRowObservation (..),
    RegionalStructureObservation (..),
    ContextSemanticObservation (..),
    semanticQueriesForClasses,
    deriveContextSemanticQueries,
    contextQuotientObservations,
    productQuotientObservations,
    contextQuotientDigest,
    productQuotientDigest,
    contextSemanticObservation,
    contextRegionalStructureObservation,
    semanticObservationDigest,
    contextSemanticDigest,
    productSemanticDigest,
    saturationTerminationDigest,
    searchVerdictDigest,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (ClassId (..), Language, classIdKey)
import Moonlight.EGraph.Pure.Context qualified as ContextGraph
import Moonlight.EGraph.Pure.Context.AnnotatedDelta qualified as Annotated
import Moonlight.EGraph.Pure.Context.Core qualified as Context
import Moonlight.EGraph.Pure.Types qualified as Graph
import Moonlight.Saturation.Core qualified as Saturation
import Moonlight.Sheaf.Context.Site qualified as Site
import Moonlight.Sheaf.Verdict qualified as Verdict

graphDigest :: Graph.EGraph f analysis -> Int
graphDigest graph =
  Graph.eGraphClassCount graph
    + Graph.eGraphNodeCount graph
    + Graph.eGraphRevisionValue (Graph.eGraphRevision graph)
    + length (Graph.eGraphPendingClassUnions graph)

contextGraphDigest :: ContextGraph.ContextEGraph f analysis context -> Int
contextGraphDigest graph =
  List.foldl'
    mixDigest
    (graphDigest (Context.cegBase graph))
    [ fromIntegral (Context.cegContextRevision graph),
      Map.size (Context.cegContextFibers graph),
      Annotated.annotatedDeltaParentChildCount metrics,
      Annotated.annotatedDeltaParentEdgeCount metrics,
      Annotated.annotatedDeltaParentRegionCubeCount metrics,
      Annotated.annotatedDeltaVariantRowCount metrics,
      Annotated.annotatedDeltaAbsorbedRowCount metrics,
      Map.size analysisDeltas,
      Map.foldl' (\entryCount delta -> entryCount + IntMap.size delta) 0 analysisDeltas
    ]
  where
    metrics =
      Annotated.annotatedDeltaMetrics
        (Site.preparedRegionTable (Context.cegSite graph))
        (Annotated.contextAnnotatedDeltaBuckets graph)
    analysisDeltas =
      Context.cegContextAnalysisDeltas graph

-- | One point of the semantic cover together with the exact classes that
-- must be queried there. Query domains are explicit because a shared
-- context graph and a product of independent graphs assign class keys to
-- context-authored terms differently.
data SemanticQuery context = SemanticQuery
  { semanticQueryContextKey :: !Site.ContextObjectKey,
    semanticQueryContext :: !context,
    semanticQueryClasses :: ![ClassId]
  }
  deriving stock (Eq, Show)

-- | Every semantic lookup failure is data. Missing product sections and
-- invalid support queries are benchmark obstructions, never magic digests.
data SemanticDigestObstruction context
  = SemanticContextSupportObstruction !(Site.PreparedContextSupportError context)
  | SemanticContextPayloadRepresentativeMissing !Site.ContextObjectKey !Int
  | SemanticProductContextMissing !Site.ContextObjectKey !context
  deriving stock (Eq, Show)

-- | Quotient-normalized observations use the least queried base-canonical
-- key as the representative. Raw union-find roots are deliberately absent:
-- rank choices are implementation detail, while the quotient is semantic.
data QuotientObservation = QuotientObservation
  !Site.ContextObjectKey
  ![(Int, Int)]
  ![(Int, Maybe Int)]
  deriving stock (Eq, Show)

-- | A row visible at one typed context key. These observations deliberately
-- consume the semantic row API rather than inspecting bucket storage.
data ContextRowObservation = ContextRowObservation
  { contextRowContextKey :: !Site.ContextObjectKey,
    contextRowTagOrdinal :: !Int,
    contextRowRootKey :: !Int,
    contextRowChildKeys :: ![Int]
  }
  deriving stock (Eq, Show)

data RegionalStructureObservation = RegionalStructureObservation
  { regionalParentChildCount :: !Int,
    regionalParentEdgeCount :: !Int,
    regionalParentRegionCubeCount :: !Int,
    regionalVariantRowCount :: !Int,
    regionalAbsorbedRowCount :: !Int,
    regionalFingerprint :: !Int,
    regionalActiveAnalysisDeltaCount :: !Int,
    regionalAnalysisDeltaEntryCount :: !Int
  }
  deriving stock (Eq, Show)

data ContextSemanticObservation = ContextSemanticObservation
  { contextObservedQuotients :: ![QuotientObservation],
    contextObservedVariantRows :: ![ContextRowObservation],
    contextObservedAbsorbedRows :: ![ContextRowObservation],
    contextObservedRegionalStructure :: !RegionalStructureObservation
  }
  deriving stock (Eq, Show)

semanticQueriesForClasses :: [(Site.ContextObjectKey, context)] -> [ClassId] -> [SemanticQuery context]
semanticQueriesForClasses keyedContexts classIds =
  [ SemanticQuery contextKey contextValue classIds
    | (contextKey, contextValue) <- keyedContexts
  ]

-- | Derive the complete pointwise query cover from the authoritative support
-- index. This is used for the context arm's annotated-versus-eager proof.
deriveContextSemanticQueries ::
  Ord context =>
  [(Site.ContextObjectKey, context)] ->
  ContextGraph.ContextEGraph f analysis context ->
  Either (SemanticDigestObstruction context) [SemanticQuery context]
deriveContextSemanticQueries keyedContexts contextGraph =
  traverse queryFor keyedContexts
  where
    queryFor (contextKey, contextValue) = do
      visibleClassKeys <-
        first SemanticContextSupportObstruction
          (Context.contextVisibleClassKeys contextValue contextGraph)
      pure
        ( SemanticQuery
            contextKey
            contextValue
            (ClassId <$> IntSet.toAscList visibleClassKeys)
        )

contextQuotientObservations ::
  (Language f, Ord context) =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  ContextGraph.ContextEGraph f analysis context ->
  Either (SemanticDigestObstruction context) [QuotientObservation]
contextQuotientObservations analysisDigest queries contextGraph =
  traverse
    (contextQuotientObservationAt analysisDigest contextGraph)
    queries

productQuotientObservations ::
  Ord context =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  Map.Map context (Graph.EGraph f analysis) ->
  Either (SemanticDigestObstruction context) [QuotientObservation]
productQuotientObservations analysisDigest queries graphs =
  traverse observeProductQuery queries
  where
    observeProductQuery query =
      maybe
        ( Left
            ( SemanticProductContextMissing
                (semanticQueryContextKey query)
                (semanticQueryContext query)
            )
        )
        ( Right
            . quotientObservationAt
              analysisDigest
              (semanticQueryContextKey query)
              (semanticQueryClasses query)
        )
        (Map.lookup (semanticQueryContext query) graphs)

contextQuotientDigest ::
  (Language f, Ord context) =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  ContextGraph.ContextEGraph f analysis context ->
  Either (SemanticDigestObstruction context) Int
contextQuotientDigest analysisDigest queries =
  fmap (digestList quotientObservationDigest)
    . contextQuotientObservations analysisDigest queries

productQuotientDigest ::
  Ord context =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  Map.Map context (Graph.EGraph f analysis) ->
  Either (SemanticDigestObstruction context) Int
productQuotientDigest analysisDigest queries =
  fmap (digestList quotientObservationDigest)
    . productQuotientObservations analysisDigest queries

contextSemanticObservation ::
  (Language f, Ord context, Show (f ())) =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  ContextGraph.ContextEGraph f analysis context ->
  Either (SemanticDigestObstruction context) ContextSemanticObservation
contextSemanticObservation analysisDigest queries contextGraph = do
  quotientObservations <-
    contextQuotientObservations analysisDigest queries contextGraph
  pure
    ContextSemanticObservation
      { contextObservedQuotients = quotientObservations,
        contextObservedVariantRows =
          contextRowObservations Annotated.annotatedRowsByTagAt buckets queries,
        contextObservedAbsorbedRows =
          contextRowObservations Annotated.absorbedRowsByTagAt buckets queries,
        contextObservedRegionalStructure =
          contextRegionalStructureObservation contextGraph
      }
  where
    buckets =
      Annotated.contextAnnotatedDeltaBuckets contextGraph

contextSemanticDigest ::
  (Language f, Ord context, Show (f ())) =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  ContextGraph.ContextEGraph f analysis context ->
  Either (SemanticDigestObstruction context) Int
contextSemanticDigest analysisDigest queries contextGraph =
  fmap
    (mixDigest (contextGraphDigest contextGraph) . semanticObservationDigest)
    (contextSemanticObservation analysisDigest queries contextGraph)

productSemanticDigest ::
  (Language f, Ord context) =>
  (analysis -> Int) ->
  [SemanticQuery context] ->
  Map.Map context (Graph.EGraph f analysis) ->
  Either (SemanticDigestObstruction context) Int
productSemanticDigest analysisDigest queries graphs =
  fmap
    ( mixDigest
        (digestList (graphSemanticDigest analysisDigest) (Map.elems graphs))
        . digestList quotientObservationDigest
    )
    (productQuotientObservations analysisDigest queries graphs)

semanticObservationDigest :: ContextSemanticObservation -> Int
semanticObservationDigest observation =
  List.foldl'
    mixDigest
    146959810
    [ digestList quotientObservationDigest (contextObservedQuotients observation),
      digestList contextRowDigest (contextObservedVariantRows observation),
      digestList contextRowDigest (contextObservedAbsorbedRows observation),
      regionalStructureDigest (contextObservedRegionalStructure observation)
    ]

contextQuotientObservationAt ::
  (Language f, Ord context) =>
  (analysis -> Int) ->
  ContextGraph.ContextEGraph f analysis context ->
  SemanticQuery context ->
  Either (SemanticDigestObstruction context) QuotientObservation
contextQuotientObservationAt analysisDigest contextGraph query = do
  (regionalRepresentatives, contextualAnalysis) <-
    first SemanticContextSupportObstruction
      ( Context.ambientRepresentativeAnalysisValuesFor
          (semanticQueryContext query)
          baseQueryKeySet
          contextGraph
      )
  let representativeAt classKey =
        ( classKey,
          IntMap.findWithDefault classKey classKey regionalRepresentatives
        )
      representatives = fmap representativeAt baseQueryKeys
      representativeKeys =
        IntSet.union
          (IntSet.difference baseQueryKeySet (IntMap.keysSet regionalRepresentatives))
          ( IntSet.fromList
              ( IntMap.elems
                  (IntMap.restrictKeys regionalRepresentatives baseQueryKeySet)
              )
          )
  analysisValues <-
    traverse (analysisAt contextualAnalysis)
      (IntSet.toAscList representativeKeys)
  let observation =
        QuotientObservation
          (semanticQueryContextKey query)
          representatives
          analysisValues
  -- Force each local section before traversing to the next context. Otherwise
  -- lazy analysis observations retain every ambient analysis section until
  -- the final global digest, turning a pointwise semantic sweep into
  -- accidental O(KN) live memory.
  quotientObservationDigest observation `seq` pure observation
  where
    baseGraph = Context.cegBase contextGraph
    baseQueryKeys = IntSet.toAscList baseQueryKeySet
    baseQueryKeySet =
      IntSet.fromList
        ( fmap
            (classIdKey . Graph.canonicalizeClassId baseGraph)
            (semanticQueryClasses query)
        )
    analysisAt contextualAnalysis representativeKey =
      maybe
        ( Left
            ( SemanticContextPayloadRepresentativeMissing
                (semanticQueryContextKey query)
                representativeKey
            )
        )
        (\analysisValue -> Right (representativeKey, Just (analysisDigest analysisValue)))
        (IntMap.lookup representativeKey contextualAnalysis)

quotientObservationAt ::
  (analysis -> Int) ->
  Site.ContextObjectKey ->
  [ClassId] ->
  Graph.EGraph f analysis ->
  QuotientObservation
quotientObservationAt analysisDigest contextKey classIds graph =
  QuotientObservation
    contextKey
    [ (classKey, IntMap.findWithDefault classKey rawRootKey leastKeyByRawRoot)
      | (classKey, rawRootKey) <- rawRoots
    ]
    ( List.sortOn fst
        [ ( leastKey,
            analysisDigest <$> IntMap.lookup rawRootKey (Graph.eGraphAnalysis graph)
          )
          | (rawRootKey, leastKey) <- IntMap.toAscList leastKeyByRawRoot
        ]
    )
  where
    queryKeys =
      IntSet.toAscList (IntSet.fromList (fmap classIdKey classIds))
    rawRoots =
      [ (classKey, classIdKey (Graph.canonicalizeClassId graph (ClassId classKey)))
        | classKey <- queryKeys
      ]
    leastKeyByRawRoot =
      IntMap.fromListWith min
        [(rawRootKey, classKey) | (classKey, rawRootKey) <- rawRoots]

contextRowObservations ::
  (Site.ContextObjectKey -> Annotated.AnnotatedDeltaBuckets f -> Map.Map (f ()) [(Int, [Int])]) ->
  Annotated.AnnotatedDeltaBuckets f ->
  [SemanticQuery context] ->
  [ContextRowObservation]
contextRowObservations rowsAt buckets queries =
  concatMap rowsForQuery queries
  where
    rowsForQuery query =
      [ ContextRowObservation
          (semanticQueryContextKey query)
          tagOrdinal
          rootKey
          childKeys
        | (tagOrdinal, (_, rows)) <-
            zip [0 ..] (Map.toAscList (rowsAt (semanticQueryContextKey query) buckets)),
          (rootKey, childKeys) <- rows
      ]

contextRegionalStructureObservation ::
  Show (f ()) =>
  ContextGraph.ContextEGraph f analysis context ->
  RegionalStructureObservation
contextRegionalStructureObservation contextGraph =
  RegionalStructureObservation
    { regionalParentChildCount = Annotated.annotatedDeltaParentChildCount metrics,
      regionalParentEdgeCount = Annotated.annotatedDeltaParentEdgeCount metrics,
      regionalParentRegionCubeCount = Annotated.annotatedDeltaParentRegionCubeCount metrics,
      regionalVariantRowCount = Annotated.annotatedDeltaVariantRowCount metrics,
      regionalAbsorbedRowCount = Annotated.annotatedDeltaAbsorbedRowCount metrics,
      regionalFingerprint = Annotated.annotatedDeltaFingerprint regionTable buckets,
      regionalActiveAnalysisDeltaCount = Map.size analysisDeltas,
      regionalAnalysisDeltaEntryCount =
        Map.foldl' (\entryCount delta -> entryCount + IntMap.size delta) 0 analysisDeltas
    }
  where
    buckets = Annotated.contextAnnotatedDeltaBuckets contextGraph
    regionTable = Site.preparedRegionTable (Context.cegSite contextGraph)
    metrics = Annotated.annotatedDeltaMetrics regionTable buckets
    analysisDeltas = Context.cegContextAnalysisDeltas contextGraph

graphSemanticDigest :: Language f => (analysis -> Int) -> Graph.EGraph f analysis -> Int
graphSemanticDigest analysisDigest graph =
  List.foldl'
    mixDigest
    (graphDigest graph)
    [ digestList
        (\(classKey, analysisValue) -> mixDigest classKey (analysisDigest analysisValue))
        (IntMap.toAscList (Graph.eGraphAnalysis graph)),
      digestList taggedRowsDigest (zip [0 ..] (Map.toAscList taggedRows))
    ]
  where
    taggedRows =
      Map.fromListWith (<>)
        [ ( void nodeShape,
            [(rootKey, fmap classIdKey (toList nodeShape))]
          )
          | (rootKey, graphClass) <- IntMap.toAscList (Graph.eGraphClasses graph),
            Graph.ENode nodeShape <- Set.toAscList (Graph.eClassNodes graphClass)
        ]
    taggedRowsDigest :: (Int, (tag, [(Int, [Int])])) -> Int
    taggedRowsDigest (tagOrdinal, (_, rows)) =
      mixDigest
        tagOrdinal
        ( digestList
            (\(rootKey, childKeys) -> mixDigest rootKey (digestList id childKeys))
            (List.sort rows)
        )

quotientObservationDigest :: QuotientObservation -> Int
quotientObservationDigest (QuotientObservation contextKey representatives analysisValues) =
  List.foldl'
    mixDigest
    37
    [ Site.contextObjectKeyValue contextKey,
      digestList (uncurry mixDigest) representatives,
      digestList analysisObservationDigest analysisValues
    ]

analysisObservationDigest :: (Int, Maybe Int) -> Int
analysisObservationDigest (classKey, maybeValue) =
  mixDigest
    classKey
    (maybe 41 (mixDigest 43) maybeValue)

contextRowDigest :: ContextRowObservation -> Int
contextRowDigest row =
  List.foldl'
    mixDigest
    47
    [ Site.contextObjectKeyValue (contextRowContextKey row),
      contextRowTagOrdinal row,
      contextRowRootKey row,
      digestList id (contextRowChildKeys row)
    ]

regionalStructureDigest :: RegionalStructureObservation -> Int
regionalStructureDigest structure =
  List.foldl'
    mixDigest
    59
    [ regionalParentChildCount structure,
      regionalParentEdgeCount structure,
      regionalParentRegionCubeCount structure,
      regionalVariantRowCount structure,
      regionalAbsorbedRowCount structure,
      regionalFingerprint structure,
      regionalActiveAnalysisDeltaCount structure,
      regionalAnalysisDeltaEntryCount structure
    ]

digestList :: (value -> Int) -> [value] -> Int
digestList digestValue values =
  List.foldl'
    (\digestAcc value -> mixDigest digestAcc (digestValue value))
    (mixDigest 53 (length values))
    values

mixDigest :: Int -> Int -> Int
mixDigest leftValue rightValue =
  leftValue * 16777619 + rightValue + 97

saturationTerminationDigest :: Saturation.SaturationTermination -> Int
saturationTerminationDigest Saturation.ReachedFixedPoint = 1
saturationTerminationDigest Saturation.ReachedGoal = 2
saturationTerminationDigest Saturation.HitIterationLimit = 3
saturationTerminationDigest Saturation.HitNodeLimit = 4

searchVerdictDigest :: Verdict.SearchVerdict void obstruction -> Int
searchVerdictDigest Verdict.SearchAccepted = 1
searchVerdictDigest (Verdict.SearchRejected obstructions) = 10 + length obstructions
searchVerdictDigest (Verdict.SearchUndecided refusals partials) =
  100 + length refusals + length partials
