{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Query
  ( Query,
    Match,
    Projection,
    AtomRef (..),
    QueryError (..),
    QueryProjection,
    atomRef,
    match,
    select,
    selectAll,
    query,
    rootQuery,
    queryPlan,
    queryFullSchema,
    queryOutputSlots,
    queryProjection,
    queryPlanProjection,
    queryProjectionFromSlots,
    projectRows,
    projectRowsWithSlots,
    projectRowsWithProjection,
    projectRowsFoldWithSlots,
    projectRowsFoldWithProjection,
    projectRowWithSlots,
    projectRowWithProjection,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( AtomId,
    SlotId,
    atomIdKey,
    slotIdKey,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    addMultiplicity,
    zeroMultiplicity
  )
import Moonlight.Flow.Internal.Digest
  ( digestWordsLow,
    mix64,
    wordOfInt,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey,
    tupleKeyFoldlSlotRepKeys',
    tupleKeyFromRepKeys,
    tupleKeyWidth,
  )
import Moonlight.Flow.Plan.Compile.Build
  ( PlanOutputBinding (..),
    QueryPlanError,
    QueryPlanInput (..),
    mkQueryPlan,
  )
import Moonlight.Flow.Plan.Query.Core
  ( AtomSpec,
    OutputProjectionObstruction (..),
    QueryOutput (..),
    QueryPlan,
    QueryPlanDomain (..),
    QueryPlanResidual (..),
    StalkRecipe,
    mkQueryAtomId,
    mkSourceAtomId,
    mkAtomSpec,
    mkStalkRecipe,
    orderedSlotNub,
    qpFullSchema,
    qpOutputSlots,
  )

type PublicQueryPlan =
  QueryPlan QuerySource OutputRow () PublicAtomTag () RepKey

newtype Query = Query
  { unQueryPlan :: PublicQueryPlan
  }

data QuerySource = QuerySource
  { qsDomain :: !QueryPlanDomain,
    qsMatches :: ![Match],
    qsOutputSlots :: ![SlotId]
  }
  deriving stock (Eq, Ord, Show)

data AtomRef = AtomRef
  { arAtomId :: !AtomId,
    arColumns :: ![SlotId]
  }
  deriving stock (Eq, Ord, Show, Read)

newtype Match = Match
  { matchAtomRef :: AtomRef
  }
  deriving stock (Eq, Ord, Show, Read)

data Projection
  = SelectSlots ![SlotId]
  | SelectAllSlots
  deriving stock (Eq, Ord, Show, Read)

data QueryError
  = QueryEmpty
  | QueryStructuralSchemaEmpty
  | QueryDuplicateAtomSlot !AtomId !SlotId
  | QueryOutputSlotOutsideSchema !SlotId
  | QueryResultRowWidthMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      !RowTupleKey
  | QueryResultSlotMissing !SlotId !RowTupleKey
  | QueryPlanRejected ![QueryPlanError]
  deriving stock (Eq, Ord, Show)

data QueryProjection = QueryProjection
  { qprFullSchema :: ![SlotId],
    qprOutputSlots :: ![SlotId]
  }
  deriving stock (Eq, Ord, Show, Read)

newtype PublicAtomTag = PublicAtomTag
  { unPublicAtomTag :: AtomId
  }
  deriving stock (Eq, Ord, Show)

newtype OutputRow = OutputRow
  { unOutputRow :: RowTupleKey
  }
  deriving stock (Eq, Ord, Show)

instance QueryOutput OutputRow RepKey where
  type OutputVar OutputRow RepKey = ()
  newtype OutputRecipe OutputRow RepKey = OutputRowRecipe Int

  mkOutputRecipe vars =
    OutputRowRecipe (length vars)
  {-# INLINE mkOutputRecipe #-}

  projectOutputRecipe (OutputRowRecipe expected) _rootKey values =
    let actual =
          Vector.length values
     in if actual == expected
          then Right (OutputRow (tupleKeyFromRepKeys (Vector.toList values)))
          else
            Left
              OutputBindingArityMismatch
                { opoExpectedArity = expected,
                  opoActualArity = actual
                }
  {-# INLINE projectOutputRecipe #-}

atomRef ::
  AtomId ->
  [SlotId] ->
  AtomRef
atomRef atomIdValue columns =
  AtomRef
    { arAtomId = atomIdValue,
      arColumns = columns
    }
{-# INLINE atomRef #-}

match ::
  AtomRef ->
  Match
match =
  Match
{-# INLINE match #-}

select ::
  [SlotId] ->
  Projection
select =
  SelectSlots
{-# INLINE select #-}

selectAll :: Projection
selectAll =
  SelectAllSlots
{-# INLINE selectAll #-}

query ::
  [Match] ->
  Projection ->
  Either QueryError Query
query matches projection = do
  case matches of
    [] ->
      Left QueryEmpty
    _ ->
      Right ()
  validateMatches matches
  let fullSchema =
        fullSchemaOfMatches matches
  rootSlot <-
    case fullSchema of
      [] ->
        Left QueryStructuralSchemaEmpty
      slot : _ ->
        Right slot
  outputSlots <-
    resolveProjection fullSchema projection
  compileQueryPlan
    StructuralQueryPlan
    matches
    fullSchema
    rootSlot
    outputSlots
{-# INLINE query #-}

rootQuery ::
  SlotId ->
  Projection ->
  Either QueryError Query
rootQuery rootSlot projection = do
  outputSlots <-
    resolveProjection [rootSlot] projection
  compileQueryPlan
    RootDomainQueryPlan
    []
    [rootSlot]
    rootSlot
    outputSlots
{-# INLINE rootQuery #-}

queryPlan ::
  Query ->
  PublicQueryPlan
queryPlan =
  unQueryPlan
{-# INLINE queryPlan #-}

queryFullSchema ::
  Query ->
  [SlotId]
queryFullSchema =
  qprFullSchema . queryProjection
{-# INLINE queryFullSchema #-}

queryOutputSlots ::
  Query ->
  [SlotId]
queryOutputSlots =
  qprOutputSlots . queryProjection
{-# INLINE queryOutputSlots #-}

queryProjection ::
  Query ->
  QueryProjection
queryProjection =
  queryPlanProjection . queryPlan
{-# INLINE queryProjection #-}

queryPlanProjection ::
  QueryPlan compiled output guard tag tuple key ->
  QueryProjection
queryPlanProjection planValue =
  queryProjectionFromSlots
    (Vector.toList (qpFullSchema planValue))
    (Vector.toList (qpOutputSlots planValue))
{-# INLINE queryPlanProjection #-}

queryProjectionFromSlots ::
  [SlotId] ->
  [SlotId] ->
  QueryProjection
queryProjectionFromSlots fullSchema outputSlots =
  QueryProjection
    { qprFullSchema = fullSchema,
      qprOutputSlots = outputSlots
    }
{-# INLINE queryProjectionFromSlots #-}

projectRows ::
  Query ->
  Map RowTupleKey Multiplicity ->
  Either QueryError (Map RowTupleKey Multiplicity)
projectRows queryValue =
  projectRowsWithProjection (queryProjection queryValue)
{-# INLINE projectRows #-}

projectRowsWithSlots ::
  [SlotId] ->
  [SlotId] ->
  Map RowTupleKey Multiplicity ->
  Either QueryError (Map RowTupleKey Multiplicity)
projectRowsWithSlots fullSchema outputSlots =
  projectRowsWithProjection (queryProjectionFromSlots fullSchema outputSlots)
{-# INLINE projectRowsWithSlots #-}

projectRowsWithProjection ::
  QueryProjection ->
  Map RowTupleKey Multiplicity ->
  Either QueryError (Map RowTupleKey Multiplicity)
projectRowsWithProjection projection rowsValue =
  normalizeMultiplicityMap
    <$> projectRowsFoldWithProjection
      projection
      Map.empty
      ( \rowValue multiplicity !acc ->
          Map.insertWith addMultiplicity rowValue multiplicity acc
      )
      rowsValue
{-# INLINE projectRowsWithProjection #-}

projectRowsFoldWithSlots ::
  [SlotId] ->
  [SlotId] ->
  r ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Map RowTupleKey Multiplicity ->
  Either QueryError r
projectRowsFoldWithSlots fullSchema outputSlots =
  projectRowsFoldWithProjection
    (queryProjectionFromSlots fullSchema outputSlots)
{-# INLINE projectRowsFoldWithSlots #-}

projectRowsFoldWithProjection ::
  QueryProjection ->
  r ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Map RowTupleKey Multiplicity ->
  Either QueryError r
projectRowsFoldWithProjection projection initial step =
  Map.foldlWithKey'
    (projectOneRowFold projection step)
    (Right initial)
{-# INLINE projectRowsFoldWithProjection #-}

projectOneRowFold ::
  QueryProjection ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Either QueryError r ->
  RowTupleKey ->
  Multiplicity ->
  Either QueryError r
projectOneRowFold _projection _step (Left err) _rowValue _multiplicity =
  Left err
projectOneRowFold projection step (Right acc0) rowValue multiplicity
  | multiplicity == zeroMultiplicity =
      Right acc0
  | otherwise = do
      projectedRow <-
        projectRowWithProjection projection rowValue
      let !acc1 =
            step projectedRow multiplicity acc0
      pure acc1
{-# INLINE projectOneRowFold #-}

projectRowWithSlots ::
  [SlotId] ->
  [SlotId] ->
  RowTupleKey ->
  Either QueryError RowTupleKey
projectRowWithSlots fullSchema outputSlots =
  projectRowWithProjection (queryProjectionFromSlots fullSchema outputSlots)
{-# INLINE projectRowWithSlots #-}

projectRowWithProjection ::
  QueryProjection ->
  RowTupleKey ->
  Either QueryError RowTupleKey
projectRowWithProjection =
  projectRow
{-# INLINE projectRowWithProjection #-}

projectRow ::
  QueryProjection ->
  RowTupleKey ->
  Either QueryError RowTupleKey
projectRow projection rowValue = do
  env <-
    rowEnv (qprFullSchema projection) rowValue
  values <-
    traverse
      (lookupProjectedSlot env rowValue)
      (qprOutputSlots projection)
  pure (tupleKeyFromRepKeys values)
{-# INLINE projectRow #-}

rowEnv ::
  [SlotId] ->
  RowTupleKey ->
  Either QueryError (IntMap RepKey)
rowEnv schema rowValue =
  case
    tupleKeyFoldlSlotRepKeys'
      (\env slot rep -> IntMap.insert (slotIdKey slot) rep env)
      IntMap.empty
      schema
      rowValue
    of
      Just env ->
        Right env
      Nothing ->
        Left
          ( QueryResultRowWidthMismatch
              (length schema)
              (tupleKeyWidth rowValue)
              rowValue
          )
{-# INLINE rowEnv #-}

lookupProjectedSlot ::
  IntMap RepKey ->
  RowTupleKey ->
  SlotId ->
  Either QueryError RepKey
lookupProjectedSlot env rowValue slot =
  case IntMap.lookup (slotIdKey slot) env of
    Just value ->
      Right value
    Nothing ->
      Left (QueryResultSlotMissing slot rowValue)
{-# INLINE lookupProjectedSlot #-}

validateMatches ::
  [Match] ->
  Either QueryError ()
validateMatches =
  traverse_ validateMatch
{-# INLINE validateMatches #-}

validateMatch ::
  Match ->
  Either QueryError ()
validateMatch (Match ref) =
  case firstDuplicate (arColumns ref) of
    Nothing ->
      Right ()
    Just slot ->
      Left (QueryDuplicateAtomSlot (arAtomId ref) slot)
{-# INLINE validateMatch #-}

resolveProjection ::
  [SlotId] ->
  Projection ->
  Either QueryError [SlotId]
resolveProjection fullSchema = \case
  SelectAllSlots ->
    Right fullSchema
  SelectSlots slots -> do
    traverse_ ensureOutputSlotKnown slots
    Right slots
  where
    schemaKeys =
      IntSet.fromList (fmap slotIdKey fullSchema)

    ensureOutputSlotKnown slot =
      if IntSet.member (slotIdKey slot) schemaKeys
        then Right ()
        else Left (QueryOutputSlotOutsideSchema slot)
{-# INLINE resolveProjection #-}

compileQueryPlan ::
  QueryPlanDomain ->
  [Match] ->
  [SlotId] ->
  SlotId ->
  [SlotId] ->
  Either QueryError Query
compileQueryPlan domain matches fullSchema rootSlot outputSlots = do
  let sourceAtomKeys =
        fmap (atomIdKey . arAtomId . matchAtomRef) matches
      queryAtomBase =
        Foldable.foldl'
          (\ceilingKey sourceAtomKey -> max ceilingKey (sourceAtomKey + 1))
          0
          sourceAtomKeys
      hasRepeatedSourceAtoms =
        case firstDuplicate sourceAtomKeys of
          Nothing ->
            False
          Just _ ->
            True
  atomSpecs <-
    traverse
      (uncurry (matchAtomSpec queryAtomBase hasRepeatedSourceAtoms))
      (zip [0 :: Int ..] matches)
  let source =
        QuerySource
          { qsDomain = domain,
            qsMatches = matches,
            qsOutputSlots = outputSlots
          }
  first QueryPlanRejected $
    Query
      <$> mkQueryPlan
        QueryPlanInput
          { qpiDomain = domain,
            qpiCompiled = source,
            qpiDigest = queryDigest domain rootSlot matches outputSlots,
            qpiAtoms = Vector.fromList atomSpecs,
            qpiSchemaOrder = Just (Vector.fromList fullSchema),
            qpiRootSlot = rootSlot,
            qpiOutputs =
              [ PlanOutputBinding slot ()
              | slot <- outputSlots
              ],
            qpiResidual = NoQueryPlanResidual
          }
{-# INLINE compileQueryPlan #-}

matchAtomSpec ::
  Int ->
  Bool ->
  Int ->
  Match ->
  Either QueryError (AtomSpec PublicAtomTag () RepKey)
matchAtomSpec queryAtomBase hasRepeatedSourceAtoms occurrenceIndex (Match ref) =
  let sourceAtomKey =
        atomIdKey (arAtomId ref)
      queryAtomKey
        | hasRepeatedSourceAtoms =
            queryAtomBase + occurrenceIndex
        | otherwise =
            sourceAtomKey
   in
  Right
    ( mkAtomSpec
        (mkQueryAtomId queryAtomKey)
        (mkSourceAtomId (arAtomId ref))
        (PublicAtomTag (arAtomId ref))
        (publicAtomTagDigest (arAtomId ref))
        (Vector.fromList (arColumns ref))
        (publicStalkRecipe (arColumns ref))
    )
{-# INLINE matchAtomSpec #-}

publicStalkRecipe ::
  [SlotId] ->
  StalkRecipe
publicStalkRecipe columns =
  mkStalkRecipe (Vector.replicate (length columns) [])
{-# INLINE publicStalkRecipe #-}

publicAtomTagDigest ::
  AtomId ->
  Word64
publicAtomTagDigest atomIdValue =
  mix64 0x70756261746f6d (wordOfInt (atomIdKey atomIdValue))
{-# INLINE publicAtomTagDigest #-}

fullSchemaOfMatches ::
  [Match] ->
  [SlotId]
fullSchemaOfMatches =
  orderedSlotNub . foldMap (arColumns . matchAtomRef)
{-# INLINE fullSchemaOfMatches #-}

queryDigest ::
  QueryPlanDomain ->
  SlotId ->
  [Match] ->
  [SlotId] ->
  Word64
queryDigest domain rootSlot matches outputSlots =
  digestWordsLow
    ( [ 0x7175657279,
        queryPlanDomainWord domain,
        wordOfInt (slotIdKey rootSlot)
      ]
        <> listWords 0x61746f6d matchDigestWords matches
        <> listWords 0x6f757470 slotDigestWords outputSlots
    )
{-# INLINE queryDigest #-}

queryPlanDomainWord ::
  QueryPlanDomain ->
  Word64
queryPlanDomainWord domain =
  case domain of
    StructuralQueryPlan ->
      0x01
    RootDomainQueryPlan ->
      0x02
{-# INLINE queryPlanDomainWord #-}

matchDigestWords ::
  Match ->
  [Word64]
matchDigestWords (Match ref) =
  [ 0x10,
    wordOfInt (atomIdKey (arAtomId ref))
  ]
    <> listWords 0x11 slotDigestWords (arColumns ref)
{-# INLINE matchDigestWords #-}

slotDigestWords ::
  SlotId ->
  [Word64]
slotDigestWords slot =
  [wordOfInt (slotIdKey slot)]
{-# INLINE slotDigestWords #-}

listWords ::
  Word64 ->
  (a -> [Word64]) ->
  [a] ->
  [Word64]
listWords tag encode values =
  tag : wordOfInt (length values) : foldMap encode values
{-# INLINE listWords #-}

normalizeMultiplicityMap ::
  Map RowTupleKey Multiplicity ->
  Map RowTupleKey Multiplicity
normalizeMultiplicityMap =
  Map.filter (/= zeroMultiplicity)
{-# INLINE normalizeMultiplicityMap #-}

data DuplicateScan a = DuplicateScan
  { dsSeen :: !(Set a),
    dsDuplicate :: !(Maybe a)
  }

firstDuplicate ::
  Ord a =>
  [a] ->
  Maybe a
firstDuplicate =
  dsDuplicate . Foldable.foldl' step (DuplicateScan Set.empty Nothing)
  where
    step ::
      Ord a =>
      DuplicateScan a ->
      a ->
      DuplicateScan a
    step scan@DuplicateScan {dsDuplicate = Just _} _value =
      scan
    step DuplicateScan {dsSeen = seen} value
      | Set.member value seen =
          DuplicateScan seen (Just value)
      | otherwise =
          DuplicateScan (Set.insert value seen) Nothing
{-# INLINE firstDuplicate #-}
