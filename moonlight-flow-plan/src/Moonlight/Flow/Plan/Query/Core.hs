{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Plan.Query.Core
  ( QueryId,
    mkQueryId,
    queryIdKey,
    AtomId,
    mkAtomId,
    atomIdKey,
    QueryAtomId,
    mkQueryAtomId,
    queryAtomKey,
    queryAtomAsAtomId,
    SourceAtomId,
    mkSourceAtomId,
    sourceAtomId,
    sourceAtomKey,
    SlotId,
    mkSlotId,
    slotIdKey,
    orderedSlotNub,
    SlotSource (..),
    StalkRecipe,
    mkStalkRecipe,
    stalkRecipeColumns,
    QueryOutput (..),
    OutputProjectionObstruction (..),
    projectQueryPlanRootKey,
    projectQueryPlanOutput,
    projectQueryPlanOutputs,
    AtomSpec,
    asQueryAtomId,
    asSourceAtomId,
    asTag,
    asTagDigest,
    asColumns,
    asStalkRecipe,
    mkAtomSpec,
    JoinShape,
    exactJoinShape,
    acyclicJoinShape,
    factorizedJoinShape,
    foldJoinShape,
    JoinForest,
    jfRoot,
    jfParent,
    jfChildren,
    jfSeparator,
    mkJoinForest,
    JoinMeta,
    jmAtomSchemas,
    jmAtomsBySlot,
    jmNeighborsBySlot,
    jmStaticRank,
    jmIncidence,
    jmShape,
    mkJoinMeta,
    withJoinMetaShape,
    QueryPlanDomain (..),
    queryPlanDomainDigestWord,
    QueryPlanResidual (..),
    QueryPlan,
    qpDomain,
    qpId,
    qpCompiled,
    qpFingerprint,
    qpAtoms,
    qpFullSchema,
    qpRootSlot,
    qpOutputSlots,
    qpOutputRecipe,
    qpResidual,
    qpJoinMeta,
    unsafeMkQueryPlan,
    withQueryPlanJoinMeta,
    AtomPlanDescriptor,
    apdQueryAtomId,
    apdSourceAtomId,
    apdTagDigest,
    apdColumns,
    apdStalkRecipe,
    QueryPlanCacheDescriptor,
    qpcdQueryId,
    qpcdFingerprint,
    qpcdDomain,
    qpcdAtoms,
    qpcdFullSchema,
    qpcdRootSlot,
    qpcdOutputSlots,
    qpcdResidualDigest,
    qpcdJoinMeta,
    PlanCacheKey,
    pckDigestHigh,
    pckDigestLow,
    pckDescriptor,
    queryPlanCacheKey,
    BagId (..),
    FactorNode (..),
    factorNodeIsBag,
    factorNodeIsSeparator,
    factorNodeIsBagBelief,
    factorNodeIsRoot,
    DecompBag,
    dbBagId,
    dbSlots,
    dbAtoms,
    mkDecompBag,
    DecompPlan,
    dpRoot,
    dpBags,
    dpParent,
    dpChildren,
    dpSeparator,
    dpAtomOwner,
    mkDecompPlan,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core (DenseKey (..))
import Moonlight.Core
  ( dedupStableOn,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
    atomIdKey,
    mkAtomId,
    mkQueryId,
    mkSlotId,
    queryIdKey,
    slotIdKey,
  )
import Moonlight.Flow.Plan.Residual
  ( QueryPlanResidual (..),
    queryPlanResidualIdentityDigest,
  )
import Moonlight.Flow.Internal.Digest
  ( digestWordsHigh,
    digestWordsLow,
    maybeWord64DigestWords,
    wordOfInt,
  )
import Moonlight.Flow.Model.Id
import Moonlight.Differential.Row.Tuple

type QueryAtomId :: Type
newtype QueryAtomId = QueryAtomId
  { queryAtomKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

mkQueryAtomId :: Int -> QueryAtomId
mkQueryAtomId =
  QueryAtomId
{-# INLINE mkQueryAtomId #-}

queryAtomAsAtomId :: QueryAtomId -> AtomId
queryAtomAsAtomId =
  mkAtomId . queryAtomKey
{-# INLINE queryAtomAsAtomId #-}

type SourceAtomId :: Type
newtype SourceAtomId = SourceAtomId
  { sourceAtomId :: AtomId
  }
  deriving stock (Eq, Ord, Show, Read)

mkSourceAtomId :: AtomId -> SourceAtomId
mkSourceAtomId =
  SourceAtomId
{-# INLINE mkSourceAtomId #-}

sourceAtomKey :: SourceAtomId -> Int
sourceAtomKey =
  atomIdKey . sourceAtomId
{-# INLINE sourceAtomKey #-}

type SlotSource :: Type
data SlotSource
  = SourceResult
  | SourceChild {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

type StalkRecipe :: Type
newtype StalkRecipe = StalkRecipe
  { srColumns :: Vector [SlotSource]
  }
  deriving stock (Eq, Ord, Show)

mkStalkRecipe :: Vector [SlotSource] -> StalkRecipe
mkStalkRecipe =
  StalkRecipe

orderedSlotNub ::
  [SlotId] ->
  [SlotId]
orderedSlotNub =
  dedupStableOn slotIdKey
{-# INLINE orderedSlotNub #-}

stalkRecipeColumns :: StalkRecipe -> Vector [SlotSource]
stalkRecipeColumns =
  srColumns

type QueryOutput :: Type -> Type -> Constraint
class DenseKey key => QueryOutput output key where
  type OutputVar output key
  data OutputRecipe output key
  mkOutputRecipe :: [OutputVar output key] -> OutputRecipe output key
  projectOutputRecipe :: OutputRecipe output key -> key -> Vector key -> Either OutputProjectionObstruction output

type OutputProjectionObstruction :: Type
data OutputProjectionObstruction
  = OutputBindingArityMismatch
      { opoExpectedArity :: {-# UNPACK #-} !Int,
        opoActualArity :: {-# UNPACK #-} !Int
      }
  deriving stock (Eq, Ord, Show)

type AtomSpec :: Type -> Type -> Type -> Type
data AtomSpec tag tuple key = AtomSpec
  { asQueryAtomId :: {-# UNPACK #-} !QueryAtomId,
    asSourceAtomId :: {-# UNPACK #-} !SourceAtomId,
    asTag :: !tag,
    asTagDigest :: {-# UNPACK #-} !Word64,
    asColumns :: !(Vector SlotId),
    asStalkRecipe :: !StalkRecipe
  }

mkAtomSpec ::
  QueryAtomId ->
  SourceAtomId ->
  tag ->
  Word64 ->
  Vector SlotId ->
  StalkRecipe ->
  AtomSpec tag tuple key
mkAtomSpec queryAtomIdValue sourceAtomIdValue tagValue tagDigestValue columns recipe =
  AtomSpec
    { asQueryAtomId = queryAtomIdValue,
      asSourceAtomId = sourceAtomIdValue,
      asTag = tagValue,
      asTagDigest = tagDigestValue,
      asColumns = columns,
      asStalkRecipe = recipe
    }

type JoinShape :: Type
data JoinShape
  = ExactJoin
  | AcyclicJoin !JoinForest
  | FactorizedJoin !DecompPlan
  deriving stock (Eq, Ord, Show)

exactJoinShape :: JoinShape
exactJoinShape =
  ExactJoin

acyclicJoinShape :: JoinForest -> JoinShape
acyclicJoinShape =
  AcyclicJoin

factorizedJoinShape :: DecompPlan -> JoinShape
factorizedJoinShape =
  FactorizedJoin

foldJoinShape :: r -> (JoinForest -> r) -> (DecompPlan -> r) -> JoinShape -> r
foldJoinShape exact acyclic factorized shape =
  case shape of
    ExactJoin ->
      exact
    AcyclicJoin forest ->
      acyclic forest
    FactorizedJoin decomp ->
      factorized decomp

type JoinForest :: Type
data JoinForest = JoinForest
  { jfRoot :: {-# UNPACK #-} !AtomId,
    jfParent :: !(IntMap AtomId),
    jfChildren :: !(IntMap [AtomId]),
    jfSeparator :: !(Map (AtomId, AtomId) [SlotId])
  }
  deriving stock (Eq, Ord, Show)

mkJoinForest ::
  AtomId ->
  IntMap AtomId ->
  IntMap [AtomId] ->
  Map (AtomId, AtomId) [SlotId] ->
  JoinForest
mkJoinForest root parent children separator =
  JoinForest
    { jfRoot = root,
      jfParent = parent,
      jfChildren = children,
      jfSeparator = separator
    }

type FactorNode :: Type
data FactorNode
  = FactorNodeBag !BagId
  | FactorNodeSeparator !BagId !BagId
  | FactorNodeBagBelief !BagId
  | FactorNodeRoot
  deriving stock (Eq, Ord, Show, Read)

factorNodeIsBag :: FactorNode -> Bool
factorNodeIsBag node =
  case node of
    FactorNodeBag {} ->
      True
    _ ->
      False
{-# INLINE factorNodeIsBag #-}

factorNodeIsSeparator :: FactorNode -> Bool
factorNodeIsSeparator node =
  case node of
    FactorNodeSeparator {} ->
      True
    _ ->
      False
{-# INLINE factorNodeIsSeparator #-}

factorNodeIsBagBelief :: FactorNode -> Bool
factorNodeIsBagBelief node =
  case node of
    FactorNodeBagBelief {} ->
      True
    _ ->
      False
{-# INLINE factorNodeIsBagBelief #-}

factorNodeIsRoot :: FactorNode -> Bool
factorNodeIsRoot node =
  case node of
    FactorNodeRoot ->
      True
    _ ->
      False
{-# INLINE factorNodeIsRoot #-}

type DecompBag :: Type
data DecompBag = DecompBag
  { dbBagId :: {-# UNPACK #-} !BagId,
    dbSlots :: ![SlotId],
    dbAtoms :: !IntSet
  }
  deriving stock (Eq, Ord, Show)

mkDecompBag :: BagId -> [SlotId] -> IntSet -> DecompBag
mkDecompBag bagId slots atoms =
  DecompBag
    { dbBagId = bagId,
      dbSlots = slots,
      dbAtoms = atoms
    }

type DecompPlan :: Type
data DecompPlan = DecompPlan
  { dpRoot :: {-# UNPACK #-} !BagId,
    dpBags :: !(IntMap DecompBag),
    dpParent :: !(IntMap BagId),
    dpChildren :: !(IntMap [BagId]),
    dpSeparator :: !(Map (BagId, BagId) [SlotId]),
    dpAtomOwner :: !(IntMap BagId)
  }
  deriving stock (Eq, Ord, Show)

mkDecompPlan ::
  BagId ->
  IntMap DecompBag ->
  IntMap BagId ->
  IntMap [BagId] ->
  Map (BagId, BagId) [SlotId] ->
  IntMap BagId ->
  DecompPlan
mkDecompPlan root bags parent children separator atomOwner =
  DecompPlan
    { dpRoot = root,
      dpBags = bags,
      dpParent = parent,
      dpChildren = children,
      dpSeparator = separator,
      dpAtomOwner = atomOwner
    }

type JoinMeta :: Type
data JoinMeta = JoinMeta
  { jmAtomSchemas :: !(IntMap [SlotId]),
    jmAtomsBySlot :: !(IntMap IntSet),
    jmNeighborsBySlot :: !(IntMap IntSet),
    jmStaticRank :: !(IntMap Int),
    jmIncidence :: !(IntMap Int),
    jmShape :: !JoinShape
  }
  deriving stock (Eq, Ord, Show)

mkJoinMeta ::
  IntMap [SlotId] ->
  IntMap IntSet ->
  IntMap IntSet ->
  IntMap Int ->
  IntMap Int ->
  JoinShape ->
  JoinMeta
mkJoinMeta atomSchemas atomsBySlot neighborsBySlot staticRank incidence shape =
  JoinMeta
    { jmAtomSchemas = atomSchemas,
      jmAtomsBySlot = atomsBySlot,
      jmNeighborsBySlot = neighborsBySlot,
      jmStaticRank = staticRank,
      jmIncidence = incidence,
      jmShape = shape
    }

withJoinMetaShape :: JoinShape -> JoinMeta -> JoinMeta
withJoinMetaShape shape meta =
  meta {jmShape = shape}

type QueryPlanDomain :: Type
data QueryPlanDomain
  = StructuralQueryPlan
  | RootDomainQueryPlan
  deriving stock (Eq, Ord, Show, Read)

type QueryPlan :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data QueryPlan compiled output guard tag tuple key = QueryPlan
  { qpDomain :: !QueryPlanDomain,
    qpId :: {-# UNPACK #-} !QueryId,
    qpCompiled :: !compiled,
    qpFingerprint :: {-# UNPACK #-} !Int,
    qpAtoms :: !(Vector (AtomSpec tag tuple key)),
    qpFullSchema :: !(Vector SlotId),
    qpRootSlot :: {-# UNPACK #-} !SlotId,
    qpOutputSlots :: !(Vector SlotId),
    qpProjection :: !(Maybe QueryPlanProjection),
    qpOutputRecipe :: !(OutputRecipe output key),
    qpResidual :: !(QueryPlanResidual guard),
    qpJoinMeta :: !JoinMeta
  }

type QueryPlanProjection :: Type
data QueryPlanProjection = QueryPlanProjection
  { qppRootColumn :: {-# UNPACK #-} !Int,
    qppOutputColumns :: !(Vector Int)
  }
  deriving stock (Eq, Ord, Show, Read)

unsafeMkQueryPlan ::
  QueryPlanDomain ->
  QueryId ->
  compiled ->
  Int ->
  Vector (AtomSpec tag tuple key) ->
  Vector SlotId ->
  SlotId ->
  Vector SlotId ->
  OutputRecipe output key ->
  QueryPlanResidual guard ->
  JoinMeta ->
  QueryPlan compiled output guard tag tuple key
unsafeMkQueryPlan domain queryId compiled fingerprint atoms fullSchema rootSlot outputSlots outputRecipe residual joinMeta =
  QueryPlan
    { qpDomain = domain,
      qpId = queryId,
      qpCompiled = compiled,
      qpFingerprint = fingerprint,
      qpAtoms = atoms,
      qpFullSchema = fullSchema,
      qpRootSlot = rootSlot,
      qpOutputSlots = outputSlots,
      qpProjection = compileQueryPlanProjection fullSchema rootSlot outputSlots,
      qpOutputRecipe = outputRecipe,
      qpResidual = residual,
      qpJoinMeta = joinMeta
    }
{-# INLINE unsafeMkQueryPlan #-}

compileQueryPlanProjection ::
  Vector SlotId ->
  SlotId ->
  Vector SlotId ->
  Maybe QueryPlanProjection
compileQueryPlanProjection fullSchema rootSlot outputSlots = do
  rootColumn <- IntMap.lookup (slotIdKey rootSlot) slotColumns
  outputColumns <-
    traverse
      (\slotValue -> IntMap.lookup (slotIdKey slotValue) slotColumns)
      outputSlots
  pure
    QueryPlanProjection
      { qppRootColumn = rootColumn,
        qppOutputColumns = outputColumns
      }
  where
    slotColumns =
      Vector.ifoldl'
        ( \columns columnIndex slotValue ->
            IntMap.insert (slotIdKey slotValue) columnIndex columns
        )
        IntMap.empty
        fullSchema
{-# INLINE compileQueryPlanProjection #-}

withQueryPlanJoinMeta ::
  JoinMeta ->
  QueryPlan compiled output guard tag tuple key ->
  QueryPlan compiled output guard tag tuple key
withQueryPlanJoinMeta joinMeta plan =
  plan {qpJoinMeta = joinMeta}

type AtomPlanDescriptor :: Type
data AtomPlanDescriptor = AtomPlanDescriptor
  { apdQueryAtomId :: {-# UNPACK #-} !QueryAtomId,
    apdSourceAtomId :: {-# UNPACK #-} !SourceAtomId,
    apdTagDigest :: {-# UNPACK #-} !Word64,
    apdColumns :: ![SlotId],
    apdStalkRecipe :: !StalkRecipe
  }
  deriving stock (Eq, Ord, Show)

type QueryPlanCacheDescriptor :: Type
data QueryPlanCacheDescriptor = QueryPlanCacheDescriptor
  { qpcdQueryId :: {-# UNPACK #-} !QueryId,
    qpcdFingerprint :: {-# UNPACK #-} !Int,
    qpcdDomain :: !QueryPlanDomain,
    qpcdAtoms :: ![AtomPlanDescriptor],
    qpcdFullSchema :: ![SlotId],
    qpcdRootSlot :: {-# UNPACK #-} !SlotId,
    qpcdOutputSlots :: ![SlotId],
    qpcdResidualDigest :: !(Maybe Word64),
    qpcdJoinMeta :: !JoinMeta
  }
  deriving stock (Eq, Ord, Show)

type PlanCacheKey :: Type
data PlanCacheKey = PlanCacheKey
  { pckDigestHigh :: {-# UNPACK #-} !Word64,
    pckDigestLow :: {-# UNPACK #-} !Word64,
    pckDescriptor :: !QueryPlanCacheDescriptor
  }
  deriving stock (Eq, Ord, Show)

queryPlanCacheKey :: QueryPlan compiled output guard tag tuple key -> PlanCacheKey
queryPlanCacheKey plan =
  let descriptor =
        QueryPlanCacheDescriptor
          { qpcdQueryId = qpId plan,
            qpcdFingerprint = qpFingerprint plan,
            qpcdDomain = qpDomain plan,
            qpcdAtoms =
              fmap
                ( \atomSpec ->
                    AtomPlanDescriptor
                      { apdQueryAtomId = asQueryAtomId atomSpec,
                        apdSourceAtomId = asSourceAtomId atomSpec,
                        apdTagDigest = asTagDigest atomSpec,
                        apdColumns = Vector.toList (asColumns atomSpec),
                        apdStalkRecipe = asStalkRecipe atomSpec
                      }
                )
                (Vector.toList (qpAtoms plan)),
            qpcdFullSchema = Vector.toList (qpFullSchema plan),
            qpcdRootSlot = qpRootSlot plan,
            qpcdOutputSlots = Vector.toList (qpOutputSlots plan),
            qpcdResidualDigest = queryPlanResidualIdentityDigest (qpResidual plan),
            qpcdJoinMeta = qpJoinMeta plan
          }
      digestInts =
        descriptorDigestInts descriptor
   in PlanCacheKey
        { pckDigestHigh = digestWordsHigh digestInts,
          pckDigestLow = digestWordsLow digestInts,
          pckDescriptor = descriptor
        }

descriptorDigestInts :: QueryPlanCacheDescriptor -> [Word64]
descriptorDigestInts descriptor =
  [ 0x01,
    wordOfInt (queryIdKey (qpcdQueryId descriptor)),
    wordOfInt (qpcdFingerprint descriptor),
    queryPlanDomainDigestWord (qpcdDomain descriptor),
    wordOfInt (slotIdKey (qpcdRootSlot descriptor))
  ]
    <> maybeWord64DigestWords (qpcdResidualDigest descriptor)
    <> listDigestInts 0x10 atomDescriptorDigestInts (qpcdAtoms descriptor)
    <> listDigestInts 0x11 slotDigestSingleton (qpcdFullSchema descriptor)
    <> listDigestInts 0x12 slotDigestSingleton (qpcdOutputSlots descriptor)
    <> joinMetaDigestInts (qpcdJoinMeta descriptor)

queryPlanDomainDigestWord :: QueryPlanDomain -> Word64
queryPlanDomainDigestWord domain =
  case domain of
    StructuralQueryPlan ->
      0x01
    RootDomainQueryPlan ->
      0x02
{-# INLINE queryPlanDomainDigestWord #-}

atomDescriptorDigestInts :: AtomPlanDescriptor -> [Word64]
atomDescriptorDigestInts descriptor =
  [ 0x20,
    wordOfInt (queryAtomKey (apdQueryAtomId descriptor)),
    wordOfInt (sourceAtomKey (apdSourceAtomId descriptor)),
    apdTagDigest descriptor
  ]
    <> listDigestInts 0x21 slotDigestSingleton (apdColumns descriptor)
    <> stalkRecipeDigestInts (apdStalkRecipe descriptor)

stalkRecipeDigestInts :: StalkRecipe -> [Word64]
stalkRecipeDigestInts (StalkRecipe columns) =
  listDigestInts
    0x30
    (listDigestInts 0x31 slotSourceDigestInts)
    (Vector.toList columns)

slotSourceDigestInts :: SlotSource -> [Word64]
slotSourceDigestInts SourceResult =
  [0x40]
slotSourceDigestInts (SourceChild childIndex) =
  [0x41, wordOfInt childIndex]

joinMetaDigestInts :: JoinMeta -> [Word64]
joinMetaDigestInts meta =
  [0x50]
    <> listDigestInts 0x51 atomSchemaDigestInts (IntMap.toAscList (jmAtomSchemas meta))
    <> intSetMapDigestInts 0x52 (jmAtomsBySlot meta)
    <> intSetMapDigestInts 0x53 (jmNeighborsBySlot meta)
    <> intMapDigestInts 0x54 (jmStaticRank meta)
    <> intMapDigestInts 0x55 (jmIncidence meta)
    <> joinShapeDigestInts (jmShape meta)

atomSchemaDigestInts :: (Int, [SlotId]) -> [Word64]
atomSchemaDigestInts (atomKey, slots) =
  [0x60, wordOfInt atomKey]
    <> listDigestInts 0x61 slotDigestSingleton slots

intSetMapDigestInts :: Word64 -> IntMap IntSet -> [Word64]
intSetMapDigestInts tag =
  listDigestInts
    tag
    ( \(key, values) ->
        [wordOfInt key]
          <> listDigestInts
            (tag + 1)
            (\value -> [wordOfInt value])
            (IntSet.toAscList values)
    )
    . IntMap.toAscList

intMapDigestInts :: Word64 -> IntMap Int -> [Word64]
intMapDigestInts tag =
  listDigestInts
    tag
    (\(key, value) -> [wordOfInt key, wordOfInt value])
    . IntMap.toAscList

joinShapeDigestInts :: JoinShape -> [Word64]
joinShapeDigestInts ExactJoin =
  [0x70]
joinShapeDigestInts (AcyclicJoin forest) =
  0x71 : joinForestDigestInts forest
joinShapeDigestInts (FactorizedJoin decomp) =
  0x72 : decompPlanDigestInts decomp

joinForestDigestInts :: JoinForest -> [Word64]
joinForestDigestInts forest =
  [0x80, wordOfInt (atomIdKey (jfRoot forest))]
    <> intAtomMapDigestInts 0x81 (jfParent forest)
    <> intAtomListMapDigestInts 0x82 (jfChildren forest)
    <> listDigestInts 0x83 atomPairSlotsDigestInts (Map.toAscList (jfSeparator forest))

intAtomMapDigestInts :: Word64 -> IntMap AtomId -> [Word64]
intAtomMapDigestInts tag =
  listDigestInts
    tag
    (\(key, atomIdValue) -> [wordOfInt key, wordOfInt (atomIdKey atomIdValue)])
    . IntMap.toAscList

intAtomListMapDigestInts :: Word64 -> IntMap [AtomId] -> [Word64]
intAtomListMapDigestInts tag =
  listDigestInts
    tag
    ( \(key, atomIds) ->
        [wordOfInt key]
          <> listDigestInts
            (tag + 1)
            (\atomIdValue -> [wordOfInt (atomIdKey atomIdValue)])
            atomIds
    )
    . IntMap.toAscList

atomPairSlotsDigestInts :: ((AtomId, AtomId), [SlotId]) -> [Word64]
atomPairSlotsDigestInts ((leftAtom, rightAtom), slots) =
  [0x90, wordOfInt (atomIdKey leftAtom), wordOfInt (atomIdKey rightAtom)]
    <> listDigestInts 0x91 slotDigestSingleton slots

decompPlanDigestInts :: DecompPlan -> [Word64]
decompPlanDigestInts decomp =
  [0xa0, wordOfInt (unBagId (dpRoot decomp))]
    <> listDigestInts 0xa1 bagDigestInts (IntMap.toAscList (dpBags decomp))
    <> intBagMapDigestInts 0xa2 (dpParent decomp)
    <> intBagListMapDigestInts 0xa3 (dpChildren decomp)
    <> listDigestInts 0xa4 bagPairSlotsDigestInts (Map.toAscList (dpSeparator decomp))
    <> intBagMapDigestInts 0xa5 (dpAtomOwner decomp)

bagDigestInts :: (Int, DecompBag) -> [Word64]
bagDigestInts (key, bag) =
  [0xb0, wordOfInt key, wordOfInt (unBagId (dbBagId bag))]
    <> listDigestInts 0xb1 slotDigestSingleton (dbSlots bag)
    <> listDigestInts 0xb2 (\atomKey -> [wordOfInt atomKey]) (IntSet.toAscList (dbAtoms bag))

intBagMapDigestInts :: Word64 -> IntMap BagId -> [Word64]
intBagMapDigestInts tag =
  listDigestInts
    tag
    (\(key, bagIdValue) -> [wordOfInt key, wordOfInt (unBagId bagIdValue)])
    . IntMap.toAscList

intBagListMapDigestInts :: Word64 -> IntMap [BagId] -> [Word64]
intBagListMapDigestInts tag =
  listDigestInts
    tag
    ( \(key, bagIds) ->
        [wordOfInt key]
          <> listDigestInts
            (tag + 1)
            (\bagIdValue -> [wordOfInt (unBagId bagIdValue)])
            bagIds
    )
    . IntMap.toAscList

bagPairSlotsDigestInts :: ((BagId, BagId), [SlotId]) -> [Word64]
bagPairSlotsDigestInts ((leftBag, rightBag), slots) =
  [0xc0, wordOfInt (unBagId leftBag), wordOfInt (unBagId rightBag)]
    <> listDigestInts 0xc1 slotDigestSingleton slots

slotDigestSingleton :: SlotId -> [Word64]
slotDigestSingleton slotIdValue =
  [slotDigestInt slotIdValue]

slotDigestInt :: SlotId -> Word64
slotDigestInt =
  wordOfInt . slotIdKey

listDigestInts :: Word64 -> (a -> [Word64]) -> [a] -> [Word64]
listDigestInts tag encode values =
  tag : wordOfInt (length values) : foldMap encode values
{-# INLINE listDigestInts #-}

projectQueryPlanRootKey ::
  QueryOutput output key =>
  QueryPlan compiled output guard tag tuple key ->
  RowTupleKey ->
  Maybe key
projectQueryPlanRootKey plan row = do
  projection <- qpProjection plan
  rowSlotValue row (qppRootColumn projection)

projectQueryPlanOutput ::
  QueryOutput output key =>
  QueryPlan compiled output guard tag tuple key ->
  RowTupleKey ->
  Either OutputProjectionObstruction (Maybe output)
projectQueryPlanOutput plan row =
  case qpProjection plan of
    Nothing ->
      Right Nothing
    Just projection ->
      case ( rowSlotValue row (qppRootColumn projection),
             traverse (rowSlotValue row) (qppOutputColumns projection)
           ) of
        (Just rootKey, Just outputValues) ->
          fmap Just (projectOutputRecipe (qpOutputRecipe plan) rootKey outputValues)
        _ ->
          Right Nothing

projectQueryPlanOutputs ::
  QueryOutput output key =>
  QueryPlan compiled output guard tag tuple key ->
  [RowTupleKey] ->
  Either OutputProjectionObstruction [output]
projectQueryPlanOutputs plan rows =
  fmap catMaybes (traverse (projectQueryPlanOutput plan) rows)

rowSlotValue :: DenseKey key => RowTupleKey -> Int -> Maybe key
rowSlotValue row column = do
  RepKey key <- tupleKeyIndex row column
  pure (decodeDenseKey key)
{-# INLINE rowSlotValue #-}
