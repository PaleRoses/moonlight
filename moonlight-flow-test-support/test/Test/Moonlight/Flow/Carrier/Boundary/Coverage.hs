module Test.Moonlight.Flow.Carrier.Boundary.Coverage
  ( RootCoverage (..),
    ResultToken (..),
    MatchOrigin (..),
    rootCoverageFromFrontier,
    rootCoverageSatisfies,
    restrictRootCoverage,
    mergeRootCoverage,
    mergeOrigin,
    atomSchemasOfPlan,
    sensitiveSlotsFromSchemas,
    restrictionCoverageForAtomSchemas,
    relationsRestrictExactly,
    restrictionInjectiveOnSensitiveSlots,
    slotDomainsFromRelations,
    rowCells,
    atomRowRepKeys,
    noCollision,
    coverCoverageForAtomRows,
    coverRowsFromChildren,
    relationProduct,
  )
where

import Control.Monad (foldM)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (DenseKey, encodeDenseKey)
import Moonlight.Core
  ( duplicateValuesOn,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Flow.Storage.Relation

type RootCoverage :: Type
data RootCoverage
  = CoverageAllRoots
  | CoverageRoots !IntSet
  deriving stock (Eq, Ord, Show)

type ResultToken :: Type
data ResultToken = ResultToken
  { rtBaseRevision :: !Int,
    rtLiveEpoch :: !Int
  }
  deriving stock (Eq, Ord, Show)

type MatchOrigin :: Type -> Type
data MatchOrigin c
  = OriginExactWCOJ
  | OriginRestrictedFrom !c
  | OriginAmalgamatedFrom !(Set c)
  | OriginMerged ![MatchOrigin c]
  deriving stock (Eq, Show)

rootCoverageFromFrontier :: Maybe IntSet -> RootCoverage
rootCoverageFromFrontier Nothing = CoverageAllRoots
rootCoverageFromFrontier (Just roots) = CoverageRoots roots

rootCoverageSatisfies :: RootCoverage -> Maybe IntSet -> Bool
rootCoverageSatisfies coverage maybeWanted =
  case maybeWanted of
    Nothing -> coverage == CoverageAllRoots
    Just wanted ->
      case coverage of
        CoverageAllRoots -> True
        CoverageRoots roots -> IntSet.isSubsetOf wanted roots

restrictRootCoverage ::
  DenseKey key =>
  IntMap key ->
  RootCoverage ->
  RootCoverage
restrictRootCoverage targetClasses coverage =
  case coverage of
    CoverageAllRoots ->
      CoverageAllRoots
    CoverageRoots roots ->
      CoverageRoots
        ( IntSet.fromList
            [ encodeDenseKey target
              | sourceRoot <- IntSet.toList roots,
                Just target <- [IntMap.lookup sourceRoot targetClasses]
            ]
        )

mergeRootCoverage :: RootCoverage -> RootCoverage -> RootCoverage
mergeRootCoverage CoverageAllRoots _ = CoverageAllRoots
mergeRootCoverage _ CoverageAllRoots = CoverageAllRoots
mergeRootCoverage (CoverageRoots leftRoots) (CoverageRoots rightRoots) =
  CoverageRoots (IntSet.union leftRoots rightRoots)

mergeOrigin :: MatchOrigin c -> MatchOrigin c -> MatchOrigin c
mergeOrigin (OriginMerged leftOrigins) (OriginMerged rightOrigins) = OriginMerged (leftOrigins <> rightOrigins)
mergeOrigin (OriginMerged origins) origin = OriginMerged (origins <> [origin])
mergeOrigin origin (OriginMerged origins) = OriginMerged (origin : origins)
mergeOrigin leftOrigin rightOrigin = OriginMerged [leftOrigin, rightOrigin]

atomSchemasOfPlan ::
  QueryPlan compiled output guard tag tuple key ->
  IntMap (Vector SlotId)
atomSchemasOfPlan plan =
  IntMap.fromList
    [ (RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec), RelPlan.asColumns atomSpec)
      | atomSpec <- Vector.toList (qpAtoms plan)
    ]

sensitiveSlotsFromSchemas :: IntMap (Vector SlotId) -> IntSet
sensitiveSlotsFromSchemas atomSchemas =
  IntMap.keysSet (IntMap.filter (> 1) slotCounts)
  where
    slotCounts =
      IntMap.fromListWith (+)
        [ (slotIdKey slotId, 1 :: Int)
          | schema <- IntMap.elems atomSchemas,
            slotId <- Vector.toList schema
        ]

restrictionCoverageForAtomSchemas ::
  DenseKey key =>
  IntMap (Vector SlotId) ->
  IntMap key ->
  IntMap (RowBlock 'Canonical) ->
  IntMap (RowBlock 'Canonical) ->
  CoverageFact
restrictionCoverageForAtomSchemas atomSchemas targetClasses sourceRelations targetRelations
  | not (relationsRestrictExactly atomSchemas targetClasses sourceRelations targetRelations) = LowerBound
  | not (restrictionInjectiveOnSensitiveSlots atomSchemas targetClasses sourceRelations) = LowerBound
  | otherwise = ExactRestricted

relationsRestrictExactly ::
  DenseKey key =>
  IntMap (Vector SlotId) ->
  IntMap key ->
  IntMap (RowBlock 'Canonical) ->
  IntMap (RowBlock 'Canonical) ->
  Bool
relationsRestrictExactly atomSchemas targetClasses sourceRelations targetRelations =
  all step (IntMap.toAscList atomSchemas)
  where
    step (atomId, expectedSchema) =
      case (IntMap.lookup atomId sourceRelations, IntMap.lookup atomId targetRelations) of
        (Just sourceRelation, Just targetRelation) ->
          rowBlockLayout sourceRelation == expectedSchema
            && rowBlockLayout targetRelation == expectedSchema
            && restrictedAtomRows (rowBlockIdentity targetRelation) targetClasses sourceRelation == Right targetRelation
        _ -> False


restrictedAtomRows ::
  DenseKey key =>
  RowBlockIdentity ->
  IntMap key ->
  RowBlock 'Canonical ->
  Either RowBuildError (RowBlock 'Canonical)
restrictedAtomRows outputIdentity targetClasses relation =
  atomRowsFromTupleKeys
    outputIdentity
    (rowBlockLayout relation)
    ( foldRowBlock
        (\acc desc -> restrictTupleKey targetClasses (materializeAtomRow relation desc) : acc)
        []
        relation
    )
{-# INLINE restrictedAtomRows #-}

restrictionInjectiveOnSensitiveSlots ::
  DenseKey key =>
  IntMap (Vector SlotId) ->
  IntMap key ->
  IntMap (RowBlock 'Canonical) ->
  Bool
restrictionInjectiveOnSensitiveSlots atomSchemas targetClasses sourceRelations =
  all slotInjective (IntSet.toList (sensitiveSlotsFromSchemas atomSchemas))
  where
    slotDomains = slotDomainsFromRelations atomSchemas sourceRelations

    slotInjective slotKey =
      let domain = IntSet.toList (IntMap.findWithDefault IntSet.empty slotKey slotDomains)

          projectKey classKey =
            maybe
              classKey
              encodeDenseKey
              (IntMap.lookup classKey targetClasses)
       in noCollision projectKey domain

slotDomainsFromRelations ::
  IntMap (Vector SlotId) ->
  IntMap (RowBlock 'Canonical) ->
  IntMap IntSet
slotDomainsFromRelations atomSchemas relations =
  IntMap.foldlWithKey' collect IntMap.empty atomSchemas
  where
    collect acc atomId expectedSchema =
      case IntMap.lookup atomId relations of
        Nothing -> acc
        Just relation ->
          if rowBlockLayout relation /= expectedSchema
            then acc
            else
              foldRowBlock
                (\rowsBySlot desc -> collectRow rowsBySlot (materializeAtomRow relation desc))
                acc
                relation
          where
            collectRow rowsBySlot row =
              IntMap.unionWith
                IntSet.union
                rowsBySlot
                ( IntMap.fromListWith
                    IntSet.union
                    [ (slotIdKey slotId, IntSet.singleton repKey)
                    | (slotId, repKey) <- rowCells relation row
                    ]
                )

rowCells :: RowBlock 'Canonical -> RowTupleKey -> [(SlotId, Int)]
rowCells relation row =
  [ (slotId, repKey)
    | (slotId, repKey) <- zip (Vector.toList (rowBlockLayout relation)) (atomRowRepKeys row)
  ]

atomRowRepKeys :: RowTupleKey -> [Int]
atomRowRepKeys =
  tupleKeyToInts
{-# INLINE atomRowRepKeys #-}

noCollision :: (Int -> Int) -> [Int] -> Bool
noCollision projectKey =
  null . duplicateValuesOn projectKey

coverCoverageForAtomRows ::
  (Ord c, DenseKey key) =>
  Int ->
  IntMap key ->
  Map (c, c) (IntMap key) ->
  Vector SlotId ->
  RowBlock 'Canonical ->
  [(c, RowBlock 'Canonical)] ->
  CoverageFact
coverCoverageForAtomRows maxProduct parentClasses meetMaps expectedSchema parentRelation childRelations
  | rowBlockLayout parentRelation /= expectedSchema = LowerBound
  | any ((/= expectedSchema) . rowBlockLayout . snd) childRelations = LowerBound
  | relationProduct childRelations > maxProduct = LowerBound
  | otherwise =
      let parentRowsFromChildren =
            coverRowsFromChildren parentClasses meetMaps childRelations
       in if parentRowsFromChildren == relationRowSet parentRelation
            then ExactAmalgamated
            else LowerBound

coverRowsFromChildren ::
  (Ord c, DenseKey key) =>
  IntMap key ->
  Map (c, c) (IntMap key) ->
  [(c, RowBlock 'Canonical)] ->
  HashSet RowTupleKey
coverRowsFromChildren parentClasses meetMaps childRelations =
  let domains =
        fmap
          (\(childContext, relation) -> (childContext, relationRowList relation))
          (List.sortOn (rowBlockCount . snd) childRelations)
      coherentFamilies = enumerateCoherentFamilies meetMaps domains
   in HashSet.fromList
        [ restrictTupleKey parentClasses row
          | ((_, row) : _) <- coherentFamilies
        ]

relationProduct :: [(c, RowBlock 'Canonical)] -> Int
relationProduct =
  product . fmap (max 1 . rowBlockCount . snd)

coherentOnMeet :: DenseKey key => IntMap key -> RowTupleKey -> RowTupleKey -> Bool
coherentOnMeet meetClasses leftRow rightRow =
  restrictTupleKey meetClasses leftRow == restrictTupleKey meetClasses rightRow

enumerateCoherentFamilies ::
  (Ord c, DenseKey key) =>
  Map (c, c) (IntMap key) ->
  [(c, [RowTupleKey])] ->
  [[(c, RowTupleKey)]]
enumerateCoherentFamilies meetMaps domains =
  fmap reverse (foldM extendFamily [] domains)
  where
    extendFamily partial (contextValue, rows) =
      [ (contextValue, row) : partial
        | row <- rows,
          familyCoherent contextValue row partial
      ]

    familyCoherent contextValue row =
      all
        ( \(otherContext, otherRow) ->
            let meetClasses = Map.findWithDefault mempty (orderedPair contextValue otherContext) meetMaps
             in coherentOnMeet meetClasses row otherRow
        )

orderedPair :: Ord c => c -> c -> (c, c)
orderedPair leftContext rightContext =
  if leftContext <= rightContext
    then (leftContext, rightContext)
    else (rightContext, leftContext)

relationRowList :: RowBlock 'Canonical -> [RowTupleKey]
relationRowList relation =
  foldRowBlock
    (\rows desc -> materializeAtomRow relation desc : rows)
    []
    relation

relationRowSet :: RowBlock 'Canonical -> HashSet RowTupleKey
relationRowSet =
  HashSet.fromList . relationRowList
