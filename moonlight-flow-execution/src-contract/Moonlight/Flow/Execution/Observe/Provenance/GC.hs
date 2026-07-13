{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvGCMode (..),
    ProvGCConfig (..),
    ProvGCStats (..),
    defaultProvGCConfig,
    collectProvArena,
    shouldRunMinorGC,
    shouldRunMajorGC,
    shouldCompactArena,
    nurseryCount,
    liveRatio,
    markProvRoots,
    provChildren,
    validateProvArenaClosed,
    validateProvRoot,
    ProvIdRemap,
    identityRemap,
    provIdRemapIsIdentity,
    provIdRemapSize,
    remapProvIdKey,
    compactProvArena,
    remapProvVal,
  )
where

import Control.Monad (foldM)
import Data.Foldable (find, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Flow.Execution.Observe.Provenance.Args
  ( ProvArgs,
    provArgsFoldl',
    provArgsFromSet,
    provArgsToIds,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Arena (rebuildCons)
import Moonlight.Flow.Execution.Observe.Provenance.Types.Internal
  ( ProvArena,
    ProvEntry (..),
    ProvGen (..),
    ProvId (..),
    ProvNode (..),
    ProvVal (..),
    ProvenanceObstruction (..),
    nextProvArenaScope,
    paCons,
    paEpoch,
    paNext,
    paNodes,
    paScope,
  )

-- ---------------------------------------------------------------------------
-- GC machinery.
-- ---------------------------------------------------------------------------

type ProvGCMode :: Type
data ProvGCMode
  = MinorGC
  | MajorGC
  | MovingCompactGC
  deriving stock (Eq, Ord, Show)

type ProvGCConfig :: Type
data ProvGCConfig = ProvGCConfig
  { pgcMinorNurseryLimit :: {-# UNPACK #-} !Int,
    pgcMajorNodeLimit :: {-# UNPACK #-} !Int,
    pgcMajorDeadRatio :: {-# UNPACK #-} !Double,
    pgcStableSurvivals :: {-# UNPACK #-} !Int,
    -- | Compact when the ratio of @paNext@ to live-node count exceeds
    --   this threshold.  High 'paNext' with few live nodes means the id
    --   space is sparse and the arena will pay cache-unfriendly costs.
    pgcCompactionSparsity :: {-# UNPACK #-} !Double,
    -- | Minimum @paNext@ before compaction is considered.  Keeps tiny
    --   arenas from bouncing in and out of compaction.
    pgcCompactionMinPaNext :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

type ProvGCStats :: Type
data ProvGCStats = ProvGCStats
  { pgsMode :: !ProvGCMode,
    pgsBeforeNodes :: {-# UNPACK #-} !Int,
    pgsAfterNodes :: {-# UNPACK #-} !Int,
    pgsCollected :: {-# UNPACK #-} !Int,
    pgsReachable :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

defaultProvGCConfig :: ProvGCConfig
defaultProvGCConfig =
  ProvGCConfig
    { pgcMinorNurseryLimit = 50000,
      pgcMajorNodeLimit = 500000,
      pgcMajorDeadRatio = 0.35,
      pgcStableSurvivals = 3,
      pgcCompactionSparsity = 4.0,
      pgcCompactionMinPaNext = 100000
    }

provChildren :: ProvNode -> [ProvId]
provChildren = \case
  PNAtom _ _ -> []
  PNSum args -> provArgsToIds args
  PNProd args -> provArgsToIds args

markProvVal :: ProvArena -> ProvVal -> IntSet -> Either ProvenanceObstruction IntSet
markProvVal arena value marked0 =
  case value of
    PVZero -> Right marked0
    PVOne -> Right marked0
    PVObstructed obstruction -> Left obstruction
    PVRef pid -> markProvId arena pid marked0

markProvId :: ProvArena -> ProvId -> IntSet -> Either ProvenanceObstruction IntSet
markProvId arena pid marked0
  | IntSet.member key marked0 = Right marked0
  | otherwise =
      case IntMap.lookup key (paNodes arena) of
        Nothing -> Left (DanglingProvId pid)
        Just entry ->
          foldM
            (\m child -> markProvId arena child m)
            (IntSet.insert key marked0)
            (provChildren (peNode entry))
  where
    key = unProvId pid

markProvRoots :: ProvArena -> [ProvVal] -> Either ProvenanceObstruction IntSet
markProvRoots arena =
  foldM (\marked root -> markProvVal arena root marked) IntSet.empty

-- | Non-moving sweep.  Minor mode drops unreachable nursery nodes only;
-- major mode drops every unreachable node and promotes repeated
-- survivors to stable.  'ProvId's are never reused.
collectProvArena ::
  ProvGCConfig ->
  ProvGCMode ->
  [ProvVal] ->
  ProvArena ->
  Either ProvenanceObstruction (ProvArena, ProvGCStats)
collectProvArena _ MovingCompactGC _ _ =
  Left MovingCompactionRequiresRemapTransaction
collectProvArena cfg mode roots arena0 =
  let beforeCount = IntMap.size (paNodes arena0)
   in do
        reachable <- markProvRoots arena0 roots
        let nodes1 = IntMap.mapMaybeWithKey (sweepEntry cfg mode reachable) (paNodes arena0)
            cons1 = rebuildCons nodes1
            arena1 =
              arena0
                { paEpoch = paEpoch arena0 + 1,
                  paNodes = nodes1,
                  paCons = cons1
                }
            afterCount = IntMap.size nodes1
            stats =
              ProvGCStats
                { pgsMode = mode,
                  pgsBeforeNodes = beforeCount,
                  pgsAfterNodes = afterCount,
                  pgsCollected = beforeCount - afterCount,
                  pgsReachable = IntSet.size reachable
                }
        Right (arena1, stats)

sweepEntry ::
  ProvGCConfig ->
  ProvGCMode ->
  IntSet ->
  Int ->
  ProvEntry ->
  Maybe ProvEntry
sweepEntry cfg mode reachable key entry =
  let isLive = IntSet.member key reachable
   in case mode of
        MinorGC ->
          case (isLive, peGen entry) of
            (False, GenNursery) -> Nothing
            (True, GenNursery) ->
              Just
                entry
                  { peGen = GenCached,
                    peSurvivals = peSurvivals entry + 1
                  }
            _ -> Just entry
        MajorGC -> majorSweep
        MovingCompactGC -> majorSweep
  where
    majorSweep
      | not (IntSet.member key reachable) = Nothing
      | otherwise =
          let survivals = peSurvivals entry + 1
              gen' = case peGen entry of
                GenStable -> GenStable
                _
                  | survivals >= pgcStableSurvivals cfg -> GenStable
                  | otherwise -> GenCached
           in Just entry {peGen = gen', peSurvivals = survivals}

nurseryCount :: ProvArena -> Int
nurseryCount arena =
  length
    [ ()
      | entry <- IntMap.elems (paNodes arena),
        peGen entry == GenNursery
    ]
{-# INLINE nurseryCount #-}

shouldRunMinorGC :: ProvGCConfig -> ProvArena -> Bool
shouldRunMinorGC cfg arena =
  nurseryCount arena >= pgcMinorNurseryLimit cfg
{-# INLINE shouldRunMinorGC #-}

shouldRunMajorGC :: ProvGCConfig -> [ProvVal] -> ProvArena -> Either ProvenanceObstruction Bool
shouldRunMajorGC cfg roots arena =
  let total = IntMap.size (paNodes arena)
   in if total < pgcMajorNodeLimit cfg
        then Right False
        else do
          reachableSet <- markProvRoots arena roots
          let reachable = IntSet.size reachableSet
              dead = total - reachable
              ratio :: Double
              ratio =
                if total == 0
                  then 0
                  else fromIntegral dead / fromIntegral total
          Right (ratio >= pgcMajorDeadRatio cfg)
{-# INLINE shouldRunMajorGC #-}

-- ---------------------------------------------------------------------------
-- Validation.
-- ---------------------------------------------------------------------------

validateProvArenaClosed :: ProvArena -> Either ProvenanceObstruction ()
validateProvArenaClosed arena =
  traverse_ validateEntry (IntMap.elems (paNodes arena))
  where
    validateEntry entry =
      maybe
        (Right ())
        (Left . DanglingProvId)
        (find missingChild (provChildren (peNode entry)))

    missingChild child =
      IntMap.notMember (unProvId child) (paNodes arena)

validateProvRoot :: ProvArena -> ProvVal -> Either ProvenanceObstruction ()
validateProvRoot arena = \case
  PVZero -> Right ()
  PVOne -> Right ()
  PVObstructed obstruction -> Left obstruction
  PVRef pid
    | IntMap.member (unProvId pid) (paNodes arena) -> Right ()
    | otherwise -> Left (DanglingProvId pid)

-- ---------------------------------------------------------------------------
-- Moving collector (compaction).
-- ---------------------------------------------------------------------------

-- | Old-'ProvId' → new-'ProvId' remap produced by 'compactProvArena'.
-- Callers must walk every cached 'ProvVal' and apply 'remapProvVal'
-- atomically before the new arena is observable anywhere else.
type ProvIdRemap :: Type
data ProvIdRemap
  = IdentityProvIdRemap
  | CompactProvIdRemap !(IntMap Int)
  deriving stock (Eq, Show)

identityRemap :: ProvIdRemap
identityRemap = IdentityProvIdRemap

provIdRemapIsIdentity :: ProvIdRemap -> Bool
provIdRemapIsIdentity = \case
  IdentityProvIdRemap -> True
  CompactProvIdRemap _remap -> False
{-# INLINE provIdRemapIsIdentity #-}

provIdRemapSize :: ProvIdRemap -> Int
provIdRemapSize = \case
  IdentityProvIdRemap -> 0
  CompactProvIdRemap remap -> IntMap.size remap
{-# INLINE provIdRemapSize #-}

remapProvIdKey :: ProvIdRemap -> Int -> Maybe Int
remapProvIdKey IdentityProvIdRemap oldKey =
  Just oldKey
remapProvIdKey (CompactProvIdRemap remap) oldKey =
  IntMap.lookup oldKey remap
{-# INLINE remapProvIdKey #-}

-- | Fraction of live nodes among total arena nodes.  Range @[0, 1]@.
liveRatio :: [ProvVal] -> ProvArena -> Either ProvenanceObstruction Double
liveRatio roots arena =
  let total = IntMap.size (paNodes arena)
   in do
        reachable <- IntSet.size <$> markProvRoots arena roots
        Right $
          if total == 0
            then 1
            else fromIntegral reachable / fromIntegral total

-- | Compaction triggers when 'paNext' grows far beyond the live-node
-- count.  High 'paNext' with few live nodes indicates a sparse id space
-- that degrades 'IntMap' locality and wastes memory on retired ids.
-- 'pgcCompactionMinPaNext' prevents bouncing on small arenas.
shouldCompactArena :: ProvGCConfig -> [ProvVal] -> ProvArena -> Either ProvenanceObstruction Bool
shouldCompactArena cfg roots arena =
  let next = paNext arena
   in if next < pgcCompactionMinPaNext cfg
        then Right False
        else do
          reachable <- IntSet.size <$> markProvRoots arena roots
          Right
            ( reachable == 0
                || fromIntegral next
                  >= pgcCompactionSparsity cfg * fromIntegral reachable
            )
{-# INLINE shouldCompactArena #-}

remapProvVal :: ProvIdRemap -> ProvVal -> Either ProvenanceObstruction ProvVal
remapProvVal IdentityProvIdRemap value =
  Right value
remapProvVal (CompactProvIdRemap remap) value =
  case value of
    PVZero -> Right PVZero
    PVOne -> Right PVOne
    PVObstructed obstruction -> Left obstruction
    PVRef oldPid@(ProvId oldI) ->
      case IntMap.lookup oldI remap of
        Just newI -> Right (PVRef (ProvId newI))
        Nothing -> Left (StaleProvIdRemap oldPid)
{-# INLINE remapProvVal #-}

-- | Moving collector: re-densify live 'ProvId's to @[0, N)@ and return
-- the remap alongside the new arena.  After this, 'paNext' equals the
-- number of live nodes and 'paCons' has been rebuilt with the new ids.
--
-- Every downstream 'ProvVal' holder (factor cells, delta cells, cached
-- root factor, etc.) must apply 'remapProvVal' with the returned
-- 'ProvIdRemap' before the new arena is exposed anywhere.
compactProvArena ::
  [ProvVal] ->
  ProvArena ->
  Either ProvenanceObstruction (ProvArena, ProvIdRemap, ProvGCStats)
compactProvArena roots arena0 =
  let beforeCount = IntMap.size (paNodes arena0)
   in do
        reachable <- markProvRoots arena0 roots
        let oldIds = IntSet.toAscList reachable
            remapTable = IntMap.fromList (zip oldIds [0 ..])
            remap = CompactProvIdRemap remapTable

        let remapPid :: ProvId -> Either ProvenanceObstruction ProvId
            remapPid pid@(ProvId old) =
              case IntMap.lookup old remapTable of
                Just new -> Right (ProvId new)
                Nothing -> Left (MissingProvIdRemap pid)

            remapArgs :: ProvArgs -> Either ProvenanceObstruction ProvArgs
            remapArgs args =
              provArgsFromSet
                <$> provArgsFoldl'
                  ( \acc pid -> do
                      newPid <- remapPid pid
                      let ProvId newId = newPid
                      IntSet.insert newId <$> acc
                  )
                  (Right IntSet.empty)
                  args

            remapNode :: ProvNode -> Either ProvenanceObstruction ProvNode
            remapNode = \case
              PNAtom a r -> Right (PNAtom a r)
              PNSum xs -> PNSum <$> remapArgs xs
              PNProd xs -> PNProd <$> remapArgs xs

            remapEntry oldI = do
              entry <-
                case IntMap.lookup oldI (paNodes arena0) of
                  Just entryValue -> Right entryValue
                  Nothing -> Left (MissingReachableProvId (ProvId oldI))
              newKey <-
                case IntMap.lookup oldI remapTable of
                  Just keyValue -> Right keyValue
                  Nothing -> Left (MissingProvIdRemap (ProvId oldI))
              node <- remapNode (peNode entry)
              Right (newKey, entry {peNode = node})

        remappedEntries <- traverse remapEntry oldIds
        let newNodes = IntMap.fromList remappedEntries
            newCons = rebuildCons newNodes

            arena1 =
              arena0
                { paNext = IntMap.size newNodes,
                  paEpoch = paEpoch arena0 + 1,
                  paScope = nextProvArenaScope (paScope arena0),
                  paNodes = newNodes,
                  paCons = newCons
                }

            afterCount = IntMap.size newNodes
            stats =
              ProvGCStats
                { pgsMode = MovingCompactGC,
                  pgsBeforeNodes = beforeCount,
                  pgsAfterNodes = afterCount,
                  pgsCollected = beforeCount - afterCount,
                  pgsReachable = IntSet.size reachable
                }
        Right (arena1, remap, stats)
