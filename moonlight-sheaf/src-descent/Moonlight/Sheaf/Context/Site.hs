{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Prepared context sites in two arms behind one abstract type: materialized finite lattices, and implicit powerset sites that refuse full-object enumeration in the type.
module Moonlight.Sheaf.Context.Site
  ( ContextObjectKey (..),
    PreparedContextSite,
    PreparedContextSiteError (..),
    PreparedContextSupportError (..),
    PowersetSitePreparationError (..),
    SiteEnumerability (..),
    SiteEnumerationRefusal (..),
    RefusedSiteEnumeration (..),
    SupportCarrier,
    supportCarrierFromSupport,
    supportCarrierToSupport,
    supportCarrierContainsKey,
    supportCarrierReachableObjects,
    supportCarrierGeneratorCount,
    supportCarrierUnion,
    supportCarrierMeet,
    ClassSupportIndex,
    ClassSupportDelta,
    fromFiniteLattice,
    fromPowersetAtoms,
    preparedSiteEnumerability,
    preparedDefaultContext,
    preparedContextLattice,
    preparedRegionTable,
    preparedRegionAt,
    supportCarrierRegion,
    preparedSupportFromContexts,
    preparedContextObjects,
    preparedContextKeyedObjects,
    preparedContextAtKey,
    preparedContextObjectSet,
    preparedContextRestrictsTo,
    preparedContextKeyRestrictsTo,
    preparedContextUpperCovers,
    preparedContextJoin,
    preparedContextMeet,
    preparedJoinClosureContexts,
    preparedJoinFailures,
    joinClosureOverContexts,
    extendJoinClosureOverContexts,
    extendPreparedJoinClosureOver,
    preparedJoinClosureOver,
    preparedMaterializedObjects,
    preparedMaterializedKeyedObjects,
    preparedMaterializedObjectSet,
    preparedMaterializedRestrictionPairs,
    preparedMaterializedJoinClosure,
    preparedMaterializedLattice,
    contextObjectKeyFor,
    classKeysVisibleAtKey,
    preparedRestrictionSources,
    preparedRestrictionTargets,
    preparedRestrictionSourcesAmong,
    preparedRestrictionTargetsAmong,
    preparedRestrictionPairs,
    contextRestrictionRegistryForObjects,
    emptyClassSupportIndex,
    classSupportIndexFromEntries,
    classSupportIndexEntries,
    classSupportIndexExplicitClassKeys,
    classSupportIndexSupportEntryCount,
    classSupportIndexCarrierGeneratorCount,
    classSupportIndexGeneratorBucketCount,
    classSupportExplicitCarrierForKey,
    defaultPreparedSupport,
    preparedSupportObjects,
    preparedSupportReachableObjects,
    normalizePreparedSupport,
    unionPreparedSupport,
    meetPreparedSupport,
    classSupportIndexInsert,
    classSupportIndexInsertMany,
    classSupportIndexMergeInto,
    emptyClassSupportDelta,
    appendClassSupportDelta,
    classSupportDeltaEmpty,
    classSupportDeltaTouchedClassKeys,
    classSupportDeltaTouchedCarriers,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Bits
  ( bit,
    testBit,
    (.&.),
    (.|.),
  )
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
    ContextRestrictionRegistry,
    ContextRestrictionRegistryError (..),
    mkContextRestrictionRegistry,
  )
import Moonlight.FiniteLattice
  ( ContextLattice (clBottom),
    ContextLatticeLookupError (..),
    contextLatticeElements,
    joinContext,
    meetContext,
    leqContext,
    singletonContextLattice,
    strictOrderPairs,
    upperCovers
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    principalSupport,
    supportBasis,
    supportBasisWithOrder,
    supportGenerators
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    fromGeneratorKeys,
    powersetRegionTable,
    regionTableFromUpsets,
  )
import Moonlight.Sheaf.Context.Region qualified as Region


type ContextObjectKey :: Type
newtype ContextObjectKey = ContextObjectKey
  { contextObjectKeyValue :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type MaterializedContextSite :: Type -> Type
data MaterializedContextSite c = MaterializedContextSite
  { pcsObjects :: !(Set c),
    pcsObjectKeys :: !(Map c ContextObjectKey),
    pcsObjectsByKey :: !(IntMap c),
    pcsDefaultObject :: !c,
    pcsRestrictionOutKeys :: !(IntMap IntSet),
    pcsRestrictionInKeys :: !(IntMap IntSet),
    pcsUpsetByKey :: !(IntMap IntSet),
    pcsStrictLowerByKey :: !(IntMap IntSet),
    pcsContextLattice :: !(ContextLattice c),
    pcsJoinClosureContexts :: ![c],
    pcsJoinFailures :: ![(c, c, ContextLatticeLookupError c)],
    pcsRegionTable :: !RegionTable
  }

type PowersetContextSite :: Type -> Type
data PowersetContextSite c = PowersetContextSite
  { pwsAtomCount :: !Int,
    pwsEncodeObject :: !(c -> Maybe Int),
    pwsDecodeObject :: !(Int -> c),
    pwsBottomObject :: !c
  }

type PreparedContextSite :: Type -> Type
data PreparedContextSite c
  = SiteMaterialized !(MaterializedContextSite c)
  | SitePowerset !(PowersetContextSite c)

type PreparedContextSiteError :: Type -> Type
data PreparedContextSiteError c
  = PreparedContextSiteObjectMissing !c
  deriving stock (Eq, Ord, Show, Read)

type RefusedSiteEnumeration :: Type
data RefusedSiteEnumeration
  = RefusedContextObjects
  | RefusedContextKeyedObjects
  | RefusedContextObjectSet
  | RefusedRestrictionPairs
  | RefusedRestrictionSources
  | RefusedRestrictionTargets
  | RefusedJoinClosure
  | RefusedContextLattice
  | RefusedSupportObjects
  deriving stock (Eq, Ord, Show, Read)

type SiteEnumerationRefusal :: Type
data SiteEnumerationRefusal = SiteEnumerationRefusal
  { serAtomCount :: !Int,
    serRefusedEnumeration :: !RefusedSiteEnumeration
  }
  deriving stock (Eq, Ord, Show, Read)

type PreparedContextSupportError :: Type -> Type
data PreparedContextSupportError c
  = PreparedContextSupportObjectMissing !c
  | PreparedContextSupportDefaultMissing
  | PreparedContextRestrictionUnavailable !c !c
  | PreparedContextSymbolicEnumerationRefused !SiteEnumerationRefusal
  deriving stock (Eq, Ord, Show, Read)

type PowersetSitePreparationError :: Type -> Type
data PowersetSitePreparationError a
  = PowersetAtomBudgetExceeded !Int
  | PowersetAtomDuplicated !a
  deriving stock (Eq, Ord, Show, Read)

type SiteEnumerability :: Type
data SiteEnumerability
  = SiteFullyMaterialized !Int
  | SiteImplicitPowerset !Int
  deriving stock (Eq, Ord, Show, Read)

type SupportCarrier :: Type -> Type
newtype SupportCarrier c = SupportCarrier
  { supportCarrierGeneratorKeys :: IntSet
  }
  deriving stock (Eq, Ord, Show)

type ClassSupportIndex :: Type -> Type
data ClassSupportIndex c = ClassSupportIndex
  { csiCarrierByClassKey :: !(IntMap (SupportCarrier c)),
    csiClassesByGeneratorKey :: !(IntMap IntSet)
  }
  deriving stock (Eq, Show)

type ClassSupportDelta :: Type -> Type
data ClassSupportDelta c = ClassSupportDelta
  { csdTouchedClassKeys :: !IntSet,
    csdTouchedCarriers :: !(IntMap (SupportCarrier c)),
    csdTouchedGeneratorKeys :: !IntSet
  }
  deriving stock (Eq, Show)

instance Semigroup (ClassSupportDelta c) where
  (<>) = appendClassSupportDelta

instance Monoid (ClassSupportDelta c) where
  mempty = emptyClassSupportDelta

fromFiniteLattice ::
  Ord c =>
  ContextLattice c ->
  PreparedContextSite c
fromFiniteLattice lattice =
  SiteMaterialized
    MaterializedContextSite
      { pcsObjects = objectSet,
        pcsObjectKeys = objectKeys,
        pcsObjectsByKey = objectsByKey,
        pcsDefaultObject = clBottom lattice,
        pcsRestrictionOutKeys = restrictionOutKeys,
        pcsRestrictionInKeys = restrictionInKeys,
        pcsUpsetByKey = upsetRows,
        pcsStrictLowerByKey = strictLowerRows,
        pcsContextLattice = lattice,
        pcsJoinClosureContexts = joinClosureContexts,
        pcsJoinFailures = joinFailures,
        pcsRegionTable = regionTableFromUpsets objectCount upsetRows strictLowerRows
      }
  where
    strictLowerRows =
      strictLowerRowsFromUpsets upsetRows
    (joinClosureContexts, joinFailures) =
      joinClosureOverContexts lattice (IntMap.elems objectsByKey)
    objectKeyEntries =
      objectKeyEntriesFromLattice lattice

    objectKeys =
      Map.fromList objectKeyEntries

    objectsByKey =
      objectsByKeyFromEntries objectKeyEntries

    objectCount =
      IntMap.size objectsByKey

    objectSet =
      Set.fromList (fmap fst objectKeyEntries)

    restrictionKeyPairs =
      [ (sourceKey, targetKey)
      | (targetContext, sourceContext) <- strictOrderPairs lattice,
        Just sourceKey <- [Map.lookup sourceContext objectKeys],
        Just targetKey <- [Map.lookup targetContext objectKeys]
      ]

    restrictionOutKeys =
      restrictionRowsFromObjectKeyPairs objectCount restrictionKeyPairs

    restrictionInKeys =
      restrictionRowsFromObjectKeyPairs objectCount (fmap swapObjectKeyPair restrictionKeyPairs)

    upsetRows =
      latticeUpsetsByKey lattice
{-# INLINE fromFiniteLattice #-}

powersetAtomBudget :: Int
powersetAtomBudget =
  62

-- | Implicit powerset site over distinct atoms (at most 62): objects are atom
-- subsets, object keys are atom bitmasks in ascending-atom rank order, and
-- preparation stores only the atom dictionary — nothing of size 2^n exists.
fromPowersetAtoms ::
  Ord a =>
  [a] ->
  Either (PowersetSitePreparationError a) (PreparedContextSite (Set a))
fromPowersetAtoms atomValues =
  case duplicatedAtoms of
    duplicateValue : _ ->
      Left (PowersetAtomDuplicated duplicateValue)
    []
      | atomCount > powersetAtomBudget ->
          Left (PowersetAtomBudgetExceeded atomCount)
      | otherwise ->
          Right
            ( SitePowerset
                PowersetContextSite
                  { pwsAtomCount = atomCount,
                    pwsEncodeObject = encodeObject,
                    pwsDecodeObject = decodeObject,
                    pwsBottomObject = Set.empty
                  }
            )
  where
    duplicatedAtoms =
      [ atomValue
        | (atomValue, occurrenceCount) <-
            Map.toAscList
              (Map.fromListWith (+) (fmap (\seenAtom -> (seenAtom, 1 :: Int)) atomValues)),
          occurrenceCount > 1
      ]

    sortedAtoms =
      Set.toAscList (Set.fromList atomValues)

    atomCount =
      length sortedAtoms

    indexByAtom =
      Map.fromList (zip sortedAtoms [0 ..])

    atomByIndex =
      IntMap.fromList (zip [0 ..] sortedAtoms)

    encodeObject atomSet =
      fmap
        (foldl (.|.) 0)
        (traverse (fmap bit . (`Map.lookup` indexByAtom)) (Set.toAscList atomSet))

    decodeObject maskValue =
      Set.fromDistinctAscList
        [ atomValue
          | (bitIndex, atomValue) <- IntMap.toAscList atomByIndex,
            testBit maskValue bitIndex
        ]

preparedSiteEnumerability :: PreparedContextSite c -> SiteEnumerability
preparedSiteEnumerability site =
  case site of
    SiteMaterialized denseSite -> SiteFullyMaterialized (IntMap.size (pcsObjectsByKey denseSite))
    SitePowerset powersetSite -> SiteImplicitPowerset (pwsAtomCount powersetSite)
{-# INLINE preparedSiteEnumerability #-}

preparedDefaultContext :: PreparedContextSite c -> c
preparedDefaultContext site =
  case site of
    SiteMaterialized denseSite -> pcsDefaultObject denseSite
    SitePowerset powersetSite -> pwsBottomObject powersetSite
{-# INLINE preparedDefaultContext #-}

-- | Materialized lattice on materialized sites; the bottom-fragment singleton
-- on implicit sites, whose lookups fail typed for every non-bottom context
-- (see 'preparedMaterializedLattice' for the refusing spelling).
preparedContextLattice :: PreparedContextSite c -> ContextLattice c
preparedContextLattice site =
  case site of
    SiteMaterialized denseSite -> pcsContextLattice denseSite
    SitePowerset powersetSite -> singletonContextLattice (pwsBottomObject powersetSite)
{-# INLINE preparedContextLattice #-}

preparedRegionTable :: PreparedContextSite c -> RegionTable
preparedRegionTable site =
  case site of
    SiteMaterialized denseSite -> pcsRegionTable denseSite
    SitePowerset powersetSite -> powersetRegionTable (pwsAtomCount powersetSite)
{-# INLINE preparedRegionTable #-}

-- | Principal open of a context: the bit-packed up-set of its object key on
-- materialized sites, a single subcube on implicit sites.
preparedRegionAt ::
  Ord c =>
  PreparedContextSite c ->
  c ->
  Either (PreparedContextSupportError c) ContextRegion
preparedRegionAt site contextValue =
  fmap
    (\(ContextObjectKey keyValue) -> Region.regionAtKey (preparedRegionTable site) keyValue)
    (contextObjectKeyFor site contextValue)
{-# INLINE preparedRegionAt #-}

-- | Compile a support carrier's generator antichain into its open region —
-- the Birkhoff crossing between the law-level and region representations.
supportCarrierRegion :: PreparedContextSite c -> SupportCarrier c -> ContextRegion
supportCarrierRegion site carrier =
  fromGeneratorKeys
    (preparedRegionTable site)
    (IntSet.toAscList (supportCarrierGeneratorKeys carrier))
{-# INLINE supportCarrierRegion #-}

preparedSupportFromContexts ::
  Ord c =>
  PreparedContextSite c ->
  [c] ->
  Either (PreparedContextSupportError c) (SupportBasis c)
preparedSupportFromContexts site contexts =
  case site of
    SiteMaterialized denseSite ->
      first contextLookupToPreparedSupport (supportBasis (pcsContextLattice denseSite) contexts)
    SitePowerset powersetSite -> do
      traverse_ (contextObjectKeyFor site) contexts
      pure (supportBasisWithOrder (powersetLeqObject powersetSite) contexts)
{-# INLINE preparedSupportFromContexts #-}

contextLookupToPreparedSupport :: ContextLatticeLookupError c -> PreparedContextSupportError c
contextLookupToPreparedSupport (ContextLatticeUnknownContext contextValue) =
  PreparedContextSupportObjectMissing contextValue
{-# INLINE contextLookupToPreparedSupport #-}

-- | Objects materialized at preparation: every object on materialized sites,
-- only the bottom fragment on implicit sites ('preparedMaterializedObjects'
-- is the spelling that refuses instead).
preparedContextObjects :: PreparedContextSite c -> [c]
preparedContextObjects site =
  case site of
    SiteMaterialized denseSite -> IntMap.elems (pcsObjectsByKey denseSite)
    SitePowerset powersetSite -> [pwsBottomObject powersetSite]
{-# INLINE preparedContextObjects #-}

preparedContextKeyedObjects :: PreparedContextSite c -> [(ContextObjectKey, c)]
preparedContextKeyedObjects site =
  case site of
    SiteMaterialized denseSite ->
      fmap
        (\(keyValue, contextValue) -> (ContextObjectKey keyValue, contextValue))
        (IntMap.toAscList (pcsObjectsByKey denseSite))
    SitePowerset powersetSite ->
      [(ContextObjectKey 0, pwsBottomObject powersetSite)]
{-# INLINE preparedContextKeyedObjects #-}

-- | Decode one already-known object key without enumerating the site.  This
-- is the point-query dual of 'contextObjectKeyFor' and remains lawful for an
-- implicit powerset site.
preparedContextAtKey :: PreparedContextSite c -> ContextObjectKey -> Maybe c
preparedContextAtKey site (ContextObjectKey keyValue) =
  case site of
    SiteMaterialized denseSite ->
      IntMap.lookup keyValue (pcsObjectsByKey denseSite)
    SitePowerset powersetSite ->
      if keyValue >= 0 && keyValue <= powersetUniverseMask powersetSite
        then Just (pwsDecodeObject powersetSite keyValue)
        else Nothing
{-# INLINE preparedContextAtKey #-}

preparedContextObjectSet :: PreparedContextSite c -> Set c
preparedContextObjectSet site =
  case site of
    SiteMaterialized denseSite -> pcsObjects denseSite
    SitePowerset powersetSite -> Set.singleton (pwsBottomObject powersetSite)
{-# INLINE preparedContextObjectSet #-}

-- | Join closure of the materialized object fragment, computed once at site
-- preparation; the bottom singleton on implicit sites.
preparedJoinClosureContexts :: PreparedContextSite c -> [c]
preparedJoinClosureContexts site =
  case site of
    SiteMaterialized denseSite -> pcsJoinClosureContexts denseSite
    SitePowerset powersetSite -> [pwsBottomObject powersetSite]
{-# INLINE preparedJoinClosureContexts #-}

-- | Join lookup failures observed while closing the materialized fragment.
-- Empty for a lawful finite lattice and always empty on implicit sites, whose
-- joins are total bitmask unions.
preparedJoinFailures :: PreparedContextSite c -> [(c, c, ContextLatticeLookupError c)]
preparedJoinFailures site =
  case site of
    SiteMaterialized denseSite -> pcsJoinFailures denseSite
    SitePowerset _ -> []
{-# INLINE preparedJoinFailures #-}

-- | Close a context set under pairwise joins in the given lattice, reporting
-- the closure in ascending order together with any join lookup failures.
joinClosureOverContexts ::
  Ord c =>
  ContextLattice c ->
  [c] ->
  ([c], [(c, c, ContextLatticeLookupError c)])
joinClosureOverContexts lattice base =
  let joinResults =
        [ (left, right, joinContext lattice left right)
        | left <- base,
          right <- base,
          left /= right
        ]
      (joinedContexts, joinFailures) =
        foldMap partitionContextJoinOutcome joinResults
   in (Set.toAscList (Set.fromList (base <> joinedContexts)), joinFailures)

extendJoinClosureOverContexts ::
  Ord c =>
  ContextLattice c ->
  [c] ->
  [c] ->
  [c] ->
  ([c], [(c, c, ContextLatticeLookupError c)])
extendJoinClosureOverContexts lattice priorBase priorClosure base =
  let priorBaseSet =
        Set.fromList priorBase
      freshPair left right =
        left /= right
          && (Set.notMember left priorBaseSet || Set.notMember right priorBaseSet)
      joinResults =
        [ (left, right, joinContext lattice left right)
        | left <- base,
          right <- base,
          freshPair left right
        ]
      (joinedContexts, joinFailures) =
        foldMap partitionContextJoinOutcome joinResults
   in ( Set.toAscList
          ( Set.unions
              [ Set.fromList priorClosure,
                Set.fromList base,
                Set.fromList joinedContexts
              ]
          ),
        joinFailures
      )

partitionContextJoinOutcome ::
  (c, c, Either errorValue c) ->
  ([c], [(c, c, errorValue)])
partitionContextJoinOutcome (left, right, result) =
  case result of
    Right joinedContext ->
      ([joinedContext], [])
    Left lookupError ->
      ([], [(left, right, lookupError)])

extendPreparedJoinClosureOver ::
  Ord c =>
  PreparedContextSite c ->
  [c] ->
  [c] ->
  [c] ->
  ([c], [(c, c, ContextLatticeLookupError c)])
extendPreparedJoinClosureOver site priorBase priorClosure base =
  case site of
    SiteMaterialized denseSite ->
      extendJoinClosureOverContexts (pcsContextLattice denseSite) priorBase priorClosure base
    SitePowerset powersetSite ->
      let priorBaseSet = Set.fromList priorBase
          freshPair left right =
            left /= right
              && (Set.notMember left priorBaseSet || Set.notMember right priorBaseSet)
          joinOutcome (leftValue, rightValue) =
            case (pwsEncodeObject powersetSite leftValue, pwsEncodeObject powersetSite rightValue) of
              (Just leftKey, Just rightKey) ->
                ([pwsDecodeObject powersetSite (leftKey .|. rightKey)], [])
              (Nothing, _) ->
                ([], [(leftValue, rightValue, ContextLatticeUnknownContext leftValue)])
              (_, Nothing) ->
                ([], [(leftValue, rightValue, ContextLatticeUnknownContext rightValue)])
          (joinedContexts, joinFailures) =
            foldMap
              joinOutcome
              [ (leftValue, rightValue)
                | leftValue <- base,
                  rightValue <- base,
                  freshPair leftValue rightValue
              ]
       in ( Set.toAscList
              ( Set.unions
                  [ Set.fromList priorClosure,
                    Set.fromList base,
                    Set.fromList joinedContexts
                  ]
              ),
            joinFailures
          )

-- | Pairwise-join closure of a caller-scoped context set on either arm: the
-- inhabited-scoped replacement for full-object closures on implicit sites.
preparedJoinClosureOver ::
  Ord c =>
  PreparedContextSite c ->
  [c] ->
  ([c], [(c, c, ContextLatticeLookupError c)])
preparedJoinClosureOver site base =
  case site of
    SiteMaterialized denseSite ->
      joinClosureOverContexts (pcsContextLattice denseSite) base
    SitePowerset powersetSite ->
      let joinOutcome (leftValue, rightValue) =
            case (pwsEncodeObject powersetSite leftValue, pwsEncodeObject powersetSite rightValue) of
              (Just leftKey, Just rightKey) ->
                ([pwsDecodeObject powersetSite (leftKey .|. rightKey)], [])
              (Nothing, _) ->
                ([], [(leftValue, rightValue, ContextLatticeUnknownContext leftValue)])
              (_, Nothing) ->
                ([], [(leftValue, rightValue, ContextLatticeUnknownContext rightValue)])
          (joinedContexts, joinFailures) =
            foldMap
              joinOutcome
              [ (leftValue, rightValue)
                | leftValue <- base,
                  rightValue <- base,
                  leftValue /= rightValue
              ]
       in (Set.toAscList (Set.fromList (base <> joinedContexts)), joinFailures)

preparedContextUpperCovers ::
  Ord c =>
  PreparedContextSite c ->
  c ->
  Either (ContextLatticeLookupError c) [c]
preparedContextUpperCovers site contextValue =
  case site of
    SiteMaterialized denseSite ->
      upperCovers (pcsContextLattice denseSite) contextValue
    SitePowerset powersetSite ->
      case pwsEncodeObject powersetSite contextValue of
        Nothing ->
          Left (ContextLatticeUnknownContext contextValue)
        Just keyValue ->
          Right
            [ pwsDecodeObject powersetSite (keyValue .|. bit atomIndex)
            | atomIndex <- [0 .. pwsAtomCount powersetSite - 1],
              not (testBit keyValue atomIndex)
            ]
{-# INLINE preparedContextUpperCovers #-}

preparedContextJoin ::
  Ord c =>
  PreparedContextSite c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) c
preparedContextJoin site leftValue rightValue =
  case site of
    SiteMaterialized denseSite ->
      joinContext (pcsContextLattice denseSite) leftValue rightValue
    SitePowerset powersetSite ->
      case (pwsEncodeObject powersetSite leftValue, pwsEncodeObject powersetSite rightValue) of
        (Just leftKey, Just rightKey) ->
          Right (pwsDecodeObject powersetSite (leftKey .|. rightKey))
        (Nothing, _) ->
          Left (ContextLatticeUnknownContext leftValue)
        (_, Nothing) ->
          Left (ContextLatticeUnknownContext rightValue)
{-# INLINE preparedContextJoin #-}

preparedContextMeet ::
  Ord c =>
  PreparedContextSite c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) c
preparedContextMeet site leftValue rightValue =
  case site of
    SiteMaterialized denseSite ->
      meetContext (pcsContextLattice denseSite) leftValue rightValue
    SitePowerset powersetSite ->
      case (pwsEncodeObject powersetSite leftValue, pwsEncodeObject powersetSite rightValue) of
        (Just leftKey, Just rightKey) ->
          Right (pwsDecodeObject powersetSite (leftKey .&. rightKey))
        (Nothing, _) ->
          Left (ContextLatticeUnknownContext leftValue)
        (_, Nothing) ->
          Left (ContextLatticeUnknownContext rightValue)
{-# INLINE preparedContextMeet #-}

preparedMaterializedObjects :: PreparedContextSite c -> Either SiteEnumerationRefusal [c]
preparedMaterializedObjects site =
  case site of
    SiteMaterialized denseSite -> Right (IntMap.elems (pcsObjectsByKey denseSite))
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedContextObjects)
{-# INLINE preparedMaterializedObjects #-}

preparedMaterializedKeyedObjects :: PreparedContextSite c -> Either SiteEnumerationRefusal [(ContextObjectKey, c)]
preparedMaterializedKeyedObjects site =
  case site of
    SiteMaterialized _ -> Right (preparedContextKeyedObjects site)
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedContextKeyedObjects)
{-# INLINE preparedMaterializedKeyedObjects #-}

preparedMaterializedObjectSet :: PreparedContextSite c -> Either SiteEnumerationRefusal (Set c)
preparedMaterializedObjectSet site =
  case site of
    SiteMaterialized denseSite -> Right (pcsObjects denseSite)
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedContextObjectSet)
{-# INLINE preparedMaterializedObjectSet #-}

preparedMaterializedRestrictionPairs :: PreparedContextSite c -> Either SiteEnumerationRefusal [(c, c)]
preparedMaterializedRestrictionPairs site =
  case site of
    SiteMaterialized _ -> Right (preparedRestrictionPairs site)
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedRestrictionPairs)
{-# INLINE preparedMaterializedRestrictionPairs #-}

preparedMaterializedJoinClosure ::
  PreparedContextSite c ->
  Either SiteEnumerationRefusal ([c], [(c, c, ContextLatticeLookupError c)])
preparedMaterializedJoinClosure site =
  case site of
    SiteMaterialized denseSite -> Right (pcsJoinClosureContexts denseSite, pcsJoinFailures denseSite)
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedJoinClosure)
{-# INLINE preparedMaterializedJoinClosure #-}

preparedMaterializedLattice :: PreparedContextSite c -> Either SiteEnumerationRefusal (ContextLattice c)
preparedMaterializedLattice site =
  case site of
    SiteMaterialized denseSite -> Right (pcsContextLattice denseSite)
    SitePowerset powersetSite -> Left (powersetRefusal powersetSite RefusedContextLattice)
{-# INLINE preparedMaterializedLattice #-}

preparedContextRestrictsTo :: Ord c => PreparedContextSite c -> c -> c -> Either (PreparedContextSupportError c) Bool
preparedContextRestrictsTo site sourceContext targetContext = do
  sourceKey <- contextObjectKeyFor site sourceContext
  targetKey <- contextObjectKeyFor site targetContext
  pure (preparedContextKeyRestrictsTo site sourceKey targetKey)
{-# INLINE preparedContextRestrictsTo #-}

preparedContextKeyRestrictsTo :: PreparedContextSite c -> ContextObjectKey -> ContextObjectKey -> Bool
preparedContextKeyRestrictsTo site (ContextObjectKey sourceKeyValue) (ContextObjectKey targetKeyValue) =
  case site of
    SiteMaterialized denseSite ->
      sourceKeyValue == targetKeyValue
        || IntSet.member
          targetKeyValue
          (IntMap.findWithDefault IntSet.empty sourceKeyValue (pcsRestrictionOutKeys denseSite))
    SitePowerset powersetSite ->
      maskWithin
        (targetKeyValue .&. powersetUniverseMask powersetSite)
        (sourceKeyValue .&. powersetUniverseMask powersetSite)
{-# INLINE preparedContextKeyRestrictsTo #-}

preparedRestrictionSources :: Ord c => c -> PreparedContextSite c -> Either (PreparedContextSupportError c) [c]
preparedRestrictionSources contextValue site =
  case site of
    SiteMaterialized denseSite ->
      fmap
        ( \(ContextObjectKey keyValue) ->
            decodeObjectKeyList denseSite (IntMap.findWithDefault IntSet.empty keyValue (pcsRestrictionInKeys denseSite))
        )
        (contextObjectKeyFor site contextValue)
    SitePowerset powersetSite ->
      Left
        ( PreparedContextSymbolicEnumerationRefused
            (powersetRefusal powersetSite RefusedRestrictionSources)
        )
{-# INLINE preparedRestrictionSources #-}

preparedRestrictionTargets :: Ord c => c -> PreparedContextSite c -> Either (PreparedContextSupportError c) [c]
preparedRestrictionTargets contextValue site =
  case site of
    SiteMaterialized denseSite ->
      fmap
        ( \(ContextObjectKey keyValue) ->
            decodeObjectKeyList denseSite (IntMap.findWithDefault IntSet.empty keyValue (pcsRestrictionOutKeys denseSite))
        )
        (contextObjectKeyFor site contextValue)
    SitePowerset powersetSite ->
      Left
        ( PreparedContextSymbolicEnumerationRefused
            (powersetRefusal powersetSite RefusedRestrictionTargets)
        )
{-# INLINE preparedRestrictionTargets #-}

preparedRestrictionSourcesAmong ::
  Ord c =>
  [c] ->
  c ->
  PreparedContextSite c ->
  Either (PreparedContextSupportError c) [c]
preparedRestrictionSourcesAmong scope contextValue site =
  fmap
    ( \targetKey ->
        [ sourceValue
        | sourceValue <- scope,
          sourceValue /= contextValue,
          Right sourceKey <- [contextObjectKeyFor site sourceValue],
          preparedContextKeyRestrictsTo site sourceKey targetKey
        ]
    )
    (contextObjectKeyFor site contextValue)
{-# INLINE preparedRestrictionSourcesAmong #-}

preparedRestrictionTargetsAmong ::
  Ord c =>
  [c] ->
  c ->
  PreparedContextSite c ->
  Either (PreparedContextSupportError c) [c]
preparedRestrictionTargetsAmong scope contextValue site =
  fmap
    ( \sourceKey ->
        [ targetValue
        | targetValue <- scope,
          targetValue /= contextValue,
          Right targetKey <- [contextObjectKeyFor site targetValue],
          preparedContextKeyRestrictsTo site sourceKey targetKey
        ]
    )
    (contextObjectKeyFor site contextValue)
{-# INLINE preparedRestrictionTargetsAmong #-}

preparedRestrictionPairs :: PreparedContextSite c -> [(c, c)]
preparedRestrictionPairs site =
  case site of
    SiteMaterialized denseSite ->
      [ (sourceValue, targetValue)
        | (sourceKey, targetKeys) <- IntMap.toAscList (pcsRestrictionOutKeys denseSite),
          Just sourceValue <- [IntMap.lookup sourceKey (pcsObjectsByKey denseSite)],
          targetKey <- IntSet.toAscList targetKeys,
          Just targetValue <- [IntMap.lookup targetKey (pcsObjectsByKey denseSite)]
      ]
    SitePowerset _ ->
      []
{-# INLINE preparedRestrictionPairs #-}

contextRestrictionRegistryForObjects ::
  Ord c =>
  Set c ->
  PreparedContextSite c ->
  Either (PreparedContextSiteError c) (ContextRestrictionRegistry c)
contextRestrictionRegistryForObjects objects site =
  case site of
    SiteMaterialized denseSite -> do
      traverse_ (validateMaterializedContextObject denseSite) (Set.toAscList objects)
      registryFromScopedEdges
        objects
        [ ContextRestrictionEdge sourceValue targetValue
          | (sourceValue, targetValue) <- preparedRestrictionPairs site,
            Set.member sourceValue objects,
            Set.member targetValue objects
        ]
    SitePowerset powersetSite -> do
      keyedObjects <-
        traverse
          ( \objectValue ->
              maybe
                (Left (PreparedContextSiteObjectMissing objectValue))
                (\keyValue -> Right (objectValue, keyValue))
                (pwsEncodeObject powersetSite objectValue)
          )
          (Set.toAscList objects)
      registryFromScopedEdges
        objects
        [ ContextRestrictionEdge sourceValue targetValue
          | (sourceValue, sourceKey) <- keyedObjects,
            (targetValue, targetKey) <- keyedObjects,
            sourceKey /= targetKey,
            maskWithin targetKey sourceKey
        ]
{-# INLINE contextRestrictionRegistryForObjects #-}

validateMaterializedContextObject ::
  Ord c =>
  MaterializedContextSite c ->
  c ->
  Either (PreparedContextSiteError c) ()
validateMaterializedContextObject denseSite objectValue
  | Set.member objectValue (pcsObjects denseSite) = Right ()
  | otherwise = Left (PreparedContextSiteObjectMissing objectValue)

registryFromScopedEdges ::
  Ord c =>
  Set c ->
  [ContextRestrictionEdge c] ->
  Either (PreparedContextSiteError c) (ContextRestrictionRegistry c)
registryFromScopedEdges objects edges =
  case mkContextRestrictionRegistry objects edges of
    Left (ContextRestrictionEdgeEndpointUnknown edge) ->
      Left
        ( PreparedContextSiteObjectMissing
            ( if Set.member (creSourceContext edge) objects
                then creTargetContext edge
                else creSourceContext edge
            )
        )
    Right registry ->
      Right registry
{-# INLINE registryFromScopedEdges #-}

emptyClassSupportIndex :: ClassSupportIndex c
emptyClassSupportIndex =
  ClassSupportIndex
    { csiCarrierByClassKey = IntMap.empty,
      csiClassesByGeneratorKey = IntMap.empty
    }
{-# INLINE emptyClassSupportIndex #-}

classSupportIndexFromEntries ::
  Ord c =>
  PreparedContextSite c ->
  IntMap (SupportBasis c) ->
  Either (PreparedContextSupportError c) (ClassSupportIndex c)
classSupportIndexFromEntries site supportByClass =
  IntMap.foldlWithKey'
    ( \accumulated classKey supportValue ->
        accumulated
          >>= fmap fst . classSupportIndexInsert site supportValue classKey
    )
    (Right emptyClassSupportIndex)
    supportByClass
{-# INLINE classSupportIndexFromEntries #-}

contextObjectKeyFor ::
  Ord c =>
  PreparedContextSite c ->
  c ->
  Either (PreparedContextSupportError c) ContextObjectKey
contextObjectKeyFor site objectValue =
  case site of
    SiteMaterialized denseSite ->
      maybe
        (Left (PreparedContextSupportObjectMissing objectValue))
        Right
        (Map.lookup objectValue (pcsObjectKeys denseSite))
    SitePowerset powersetSite ->
      maybe
        (Left (PreparedContextSupportObjectMissing objectValue))
        (Right . ContextObjectKey)
        (pwsEncodeObject powersetSite objectValue)
{-# INLINE contextObjectKeyFor #-}

supportCarrierFromSupport ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (SupportCarrier c)
supportCarrierFromSupport site supportValue = do
  generatorKeys <-
    traverse
      contextObjectKeyValueFromObject
      (supportGenerators supportValue)
  pure (SupportCarrier (normalizeSupportCarrierKeys site (IntSet.fromList generatorKeys)))
  where
    contextObjectKeyValueFromObject objectValue = do
      ContextObjectKey keyValue <- contextObjectKeyFor site objectValue
      pure keyValue
{-# INLINE supportCarrierFromSupport #-}

supportCarrierToSupport ::
  Ord c =>
  PreparedContextSite c ->
  SupportCarrier c ->
  Either (PreparedContextSupportError c) (SupportBasis c)
supportCarrierToSupport site carrier =
  preparedSupportFromContexts site (Set.toAscList (decodeObjectKeys site (supportCarrierGeneratorKeys carrier)))
{-# INLINE supportCarrierToSupport #-}

supportCarrierContainsKey :: PreparedContextSite c -> SupportCarrier c -> ContextObjectKey -> Bool
supportCarrierContainsKey site carrier (ContextObjectKey keyValue) =
  case site of
    SiteMaterialized denseSite ->
      not
        ( IntSet.null
            ( IntSet.intersection
                (supportCarrierGeneratorKeys carrier)
                (visibleGeneratorKeysAtKey denseSite (ContextObjectKey keyValue))
            )
        )
    SitePowerset powersetSite ->
      let contextMask = keyValue .&. powersetUniverseMask powersetSite
       in any
            (`maskWithin` contextMask)
            (IntSet.toAscList (supportCarrierGeneratorKeys carrier))
{-# INLINE supportCarrierContainsKey #-}

supportCarrierReachableObjects ::
  Ord c =>
  PreparedContextSite c ->
  Set c ->
  SupportCarrier c ->
  Either (PreparedContextSupportError c) (Set c)
supportCarrierReachableObjects site candidateObjects carrier = do
  keyedCandidates <-
    traverse
      (\objectValue -> fmap ((,) objectValue) (contextObjectKeyFor site objectValue))
      (Set.toAscList candidateObjects)
  pure
    ( Set.fromList
        [ objectValue
        | (objectValue, objectKey) <- keyedCandidates,
          supportCarrierContainsKey site carrier objectKey
        ]
    )
{-# INLINE supportCarrierReachableObjects #-}

supportCarrierGeneratorCount :: SupportCarrier c -> Int
supportCarrierGeneratorCount =
  IntSet.size . supportCarrierGeneratorKeys
{-# INLINE supportCarrierGeneratorCount #-}

supportCarrierUnion :: PreparedContextSite c -> SupportCarrier c -> SupportCarrier c -> SupportCarrier c
supportCarrierUnion site leftCarrier rightCarrier =
  SupportCarrier
    ( normalizeSupportCarrierKeys
        site
        (IntSet.union (supportCarrierGeneratorKeys leftCarrier) (supportCarrierGeneratorKeys rightCarrier))
    )
{-# INLINE supportCarrierUnion #-}

supportCarrierMeet :: PreparedContextSite c -> SupportCarrier c -> SupportCarrier c -> SupportCarrier c
supportCarrierMeet site leftCarrier rightCarrier =
  case site of
    SiteMaterialized denseSite ->
      SupportCarrier
        ( normalizeSupportCarrierKeys
            site
            ( IntSet.intersection
                (supportCarrierReachableKeys denseSite leftCarrier)
                (supportCarrierReachableKeys denseSite rightCarrier)
            )
        )
    SitePowerset _ ->
      SupportCarrier
        ( normalizeSupportCarrierKeys
            site
            ( IntSet.fromList
                [ leftKey .|. rightKey
                  | leftKey <- IntSet.toAscList (supportCarrierGeneratorKeys leftCarrier),
                    rightKey <- IntSet.toAscList (supportCarrierGeneratorKeys rightCarrier)
                ]
            )
        )
{-# INLINE supportCarrierMeet #-}

defaultSupportCarrier ::
  Ord c =>
  PreparedContextSite c ->
  Either (PreparedContextSupportError c) (SupportCarrier c)
defaultSupportCarrier site =
  defaultPreparedSupport site >>= supportCarrierFromSupport site
{-# INLINE defaultSupportCarrier #-}

classKeysVisibleAtKey :: PreparedContextSite c -> ClassSupportIndex c -> ContextObjectKey -> IntSet
classKeysVisibleAtKey site supportIndex (ContextObjectKey keyValue) =
  case site of
    SiteMaterialized denseSite ->
      IntSet.foldr
        (\generatorKey visibleKeys -> IntSet.union (classesForGeneratorKey generatorKey) visibleKeys)
        IntSet.empty
        (visibleGeneratorKeysAtKey denseSite (ContextObjectKey keyValue))
    SitePowerset powersetSite ->
      let contextMask = keyValue .&. powersetUniverseMask powersetSite
       in IntMap.foldlWithKey'
            ( \visibleKeys generatorKey classKeys ->
                if maskWithin generatorKey contextMask
                  then IntSet.union classKeys visibleKeys
                  else visibleKeys
            )
            IntSet.empty
            (csiClassesByGeneratorKey supportIndex)
  where
    classesForGeneratorKey generatorKey =
      IntMap.findWithDefault IntSet.empty generatorKey (csiClassesByGeneratorKey supportIndex)
{-# INLINE classKeysVisibleAtKey #-}

classSupportIndexEntries ::
  Ord c =>
  PreparedContextSite c ->
  ClassSupportIndex c ->
  Either (PreparedContextSupportError c) (IntMap (SupportBasis c))
classSupportIndexEntries site supportIndex =
  traverse (supportCarrierToSupport site) (csiCarrierByClassKey supportIndex)
{-# INLINE classSupportIndexEntries #-}

classSupportIndexExplicitClassKeys :: ClassSupportIndex c -> IntSet
classSupportIndexExplicitClassKeys =
  IntMap.keysSet . csiCarrierByClassKey
{-# INLINE classSupportIndexExplicitClassKeys #-}

classSupportIndexSupportEntryCount :: ClassSupportIndex c -> Int
classSupportIndexSupportEntryCount =
  IntMap.size . csiCarrierByClassKey
{-# INLINE classSupportIndexSupportEntryCount #-}

classSupportIndexCarrierGeneratorCount :: ClassSupportIndex c -> Int
classSupportIndexCarrierGeneratorCount =
  sum . fmap supportCarrierGeneratorCount . IntMap.elems . csiCarrierByClassKey
{-# INLINE classSupportIndexCarrierGeneratorCount #-}

classSupportIndexGeneratorBucketCount :: ClassSupportIndex c -> Int
classSupportIndexGeneratorBucketCount =
  IntMap.size . csiClassesByGeneratorKey
{-# INLINE classSupportIndexGeneratorBucketCount #-}

classSupportExplicitCarrierForKey :: ClassSupportIndex c -> Int -> Maybe (SupportCarrier c)
classSupportExplicitCarrierForKey supportIndex classKey =
  IntMap.lookup classKey (csiCarrierByClassKey supportIndex)
{-# INLINE classSupportExplicitCarrierForKey #-}

defaultPreparedSupport :: PreparedContextSite c -> Either (PreparedContextSupportError c) (SupportBasis c)
defaultPreparedSupport site =
  case site of
    SiteMaterialized denseSite ->
      Right (principalSupport (pcsDefaultObject denseSite))
    SitePowerset powersetSite ->
      Right (principalSupport (pwsBottomObject powersetSite))
{-# INLINE defaultPreparedSupport #-}

preparedSupportObjects ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (Set c)
preparedSupportObjects site supportValue =
  case site of
    SiteMaterialized denseSite -> do
      carrier <- supportCarrierFromSupport site supportValue
      pure (decodeObjectKeys site (supportCarrierReachableKeys denseSite carrier))
    SitePowerset powersetSite ->
      Left
        ( PreparedContextSymbolicEnumerationRefused
            (powersetRefusal powersetSite RefusedSupportObjects)
        )
{-# INLINE preparedSupportObjects #-}

preparedSupportReachableObjects ::
  Ord c =>
  PreparedContextSite c ->
  Set c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (Set c)
preparedSupportReachableObjects site candidateObjects supportValue = do
  carrier <- supportCarrierFromSupport site supportValue
  supportCarrierReachableObjects site candidateObjects carrier
{-# INLINE preparedSupportReachableObjects #-}

normalizePreparedSupport ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (SupportBasis c)
normalizePreparedSupport site supportValue =
  supportCarrierFromSupport site supportValue >>= supportCarrierToSupport site
{-# INLINE normalizePreparedSupport #-}

unionPreparedSupport ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (SupportBasis c)
unionPreparedSupport site leftSupport rightSupport = do
  leftCarrier <- supportCarrierFromSupport site leftSupport
  rightCarrier <- supportCarrierFromSupport site rightSupport
  supportCarrierToSupport site (supportCarrierUnion site leftCarrier rightCarrier)
{-# INLINE unionPreparedSupport #-}

meetPreparedSupport ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  SupportBasis c ->
  Either (PreparedContextSupportError c) (SupportBasis c)
meetPreparedSupport site leftSupport rightSupport = do
  leftCarrier <- supportCarrierFromSupport site leftSupport
  rightCarrier <- supportCarrierFromSupport site rightSupport
  supportCarrierToSupport site (supportCarrierMeet site leftCarrier rightCarrier)
{-# INLINE meetPreparedSupport #-}

classSupportIndexInsert ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  Int ->
  ClassSupportIndex c ->
  Either (PreparedContextSupportError c) (ClassSupportIndex c, ClassSupportDelta c)
classSupportIndexInsert site supportValue classKey supportIndex =
  classSupportIndexInsertMany site supportValue (IntSet.singleton classKey) supportIndex
{-# INLINE classSupportIndexInsert #-}

classSupportIndexInsertMany ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  IntSet ->
  ClassSupportIndex c ->
  Either (PreparedContextSupportError c) (ClassSupportIndex c, ClassSupportDelta c)
classSupportIndexInsertMany site supportValue classKeys supportIndex = do
  normalizedCarrier <- supportCarrierFromSupport site supportValue
  defaultCarrierValue <- defaultSupportCarrier site
  IntSet.foldl'
    (accumulateSupportUpdate defaultCarrierValue normalizedCarrier)
    (Right (supportIndex, emptyClassSupportDelta))
    classKeys
  where
    accumulateSupportUpdate defaultCarrierValue normalizedCarrier currentResult classKey = do
      (currentIndex, currentDelta) <- currentResult
      let maybeOriginalCarrier =
            IntMap.lookup classKey (csiCarrierByClassKey currentIndex)
          effectiveOriginalCarrier =
            maybe defaultCarrierValue id maybeOriginalCarrier
          updatedCarrier =
            maybe
              normalizedCarrier
              (supportCarrierUnion site normalizedCarrier)
              maybeOriginalCarrier
          changed =
            updatedCarrier /= effectiveOriginalCarrier
          updatedIndex
            | not changed && maybeOriginalCarrier /= Nothing =
                currentIndex
            | otherwise =
                insertClassCarrier classKey maybeOriginalCarrier updatedCarrier currentIndex
          changedGeneratorKeys
            | changed =
                IntSet.union
                  (supportCarrierGeneratorKeys effectiveOriginalCarrier)
                  (supportCarrierGeneratorKeys updatedCarrier)
            | otherwise =
                IntSet.empty
          updatedDelta
            | changed =
                appendClassSupportDelta
                  currentDelta
                  ClassSupportDelta
                    { csdTouchedClassKeys = IntSet.singleton classKey,
                      csdTouchedCarriers = IntMap.singleton classKey updatedCarrier,
                      csdTouchedGeneratorKeys = changedGeneratorKeys
                    }
            | otherwise =
                currentDelta
      pure (updatedIndex, updatedDelta)
{-# INLINE classSupportIndexInsertMany #-}

-- | Push class support forward along a quotient collapse: the absorbed class
-- keys surrender their explicit entries, whose carriers fold into the
-- surviving canonical key together with the supplied merged support. The
-- canonical key is materialized only when its effective carrier departs from
-- the site default or it already held an explicit entry. The returned delta is
-- authoritative: it is empty exactly when the index is unchanged.
classSupportIndexMergeInto ::
  Ord c =>
  PreparedContextSite c ->
  SupportBasis c ->
  Int ->
  IntSet ->
  ClassSupportIndex c ->
  Either (PreparedContextSupportError c) (ClassSupportIndex c, ClassSupportDelta c)
classSupportIndexMergeInto site supportValue canonicalKey absorbedKeys supportIndex = do
  normalizedCarrier <- supportCarrierFromSupport site supportValue
  defaultCarrierValue <- defaultSupportCarrier site
  let absorbedKeySet =
        IntSet.delete canonicalKey absorbedKeys
      absorbedEntries =
        [ (classKey, carrier)
          | classKey <- IntSet.toAscList absorbedKeySet,
            Just carrier <- [IntMap.lookup classKey (csiCarrierByClassKey supportIndex)]
        ]
      maybeCanonicalCarrier =
        IntMap.lookup canonicalKey (csiCarrierByClassKey supportIndex)
      mergedCarrier =
        foldl
          (supportCarrierUnion site)
          (maybe normalizedCarrier (supportCarrierUnion site normalizedCarrier) maybeCanonicalCarrier)
          (fmap snd absorbedEntries)
      canonicalUnchanged =
        case maybeCanonicalCarrier of
          Just existingCarrier ->
            mergedCarrier == existingCarrier
          Nothing ->
            mergedCarrier == defaultCarrierValue
  if null absorbedEntries && canonicalUnchanged
    then pure (supportIndex, emptyClassSupportDelta)
    else do
      let scrubbedIndex =
            foldl
              ( \currentIndex (classKey, carrier) ->
                  ClassSupportIndex
                    { csiCarrierByClassKey =
                        IntMap.delete classKey (csiCarrierByClassKey currentIndex),
                      csiClassesByGeneratorKey =
                        removeClassFromGeneratorBuckets
                          classKey
                          (supportCarrierGeneratorKeys carrier)
                          (csiClassesByGeneratorKey currentIndex)
                    }
              )
              supportIndex
              absorbedEntries
          updatedIndex
            | maybeCanonicalCarrier == Nothing && mergedCarrier == defaultCarrierValue =
                scrubbedIndex
            | otherwise =
                insertClassCarrier canonicalKey maybeCanonicalCarrier mergedCarrier scrubbedIndex
          touchedClassKeys =
            IntSet.insert canonicalKey (IntSet.fromList (fmap fst absorbedEntries))
          touchedGeneratorKeys =
            IntSet.unions
              ( supportCarrierGeneratorKeys mergedCarrier
                  : supportCarrierGeneratorKeys (maybe defaultCarrierValue id maybeCanonicalCarrier)
                  : fmap (supportCarrierGeneratorKeys . snd) absorbedEntries
              )
      pure
        ( updatedIndex,
          ClassSupportDelta
            { csdTouchedClassKeys = touchedClassKeys,
              csdTouchedCarriers = IntMap.fromSet (const mergedCarrier) touchedClassKeys,
              csdTouchedGeneratorKeys = touchedGeneratorKeys
            }
        )
{-# INLINEABLE classSupportIndexMergeInto #-}

emptyClassSupportDelta :: ClassSupportDelta c
emptyClassSupportDelta =
  ClassSupportDelta
    { csdTouchedClassKeys = IntSet.empty,
      csdTouchedCarriers = IntMap.empty,
      csdTouchedGeneratorKeys = IntSet.empty
    }
{-# INLINE emptyClassSupportDelta #-}

appendClassSupportDelta :: ClassSupportDelta c -> ClassSupportDelta c -> ClassSupportDelta c
appendClassSupportDelta leftDelta rightDelta =
  ClassSupportDelta
    { csdTouchedClassKeys =
        IntSet.union
          (csdTouchedClassKeys leftDelta)
          (csdTouchedClassKeys rightDelta),
      csdTouchedCarriers =
        IntMap.union
          (csdTouchedCarriers rightDelta)
          (csdTouchedCarriers leftDelta),
      csdTouchedGeneratorKeys =
        IntSet.union
          (csdTouchedGeneratorKeys leftDelta)
          (csdTouchedGeneratorKeys rightDelta)
    }
{-# INLINE appendClassSupportDelta #-}

classSupportDeltaEmpty :: ClassSupportDelta c -> Bool
classSupportDeltaEmpty delta =
  IntSet.null (csdTouchedClassKeys delta)
    && IntMap.null (csdTouchedCarriers delta)
    && IntSet.null (csdTouchedGeneratorKeys delta)
{-# INLINE classSupportDeltaEmpty #-}

classSupportDeltaTouchedClassKeys :: ClassSupportDelta c -> IntSet
classSupportDeltaTouchedClassKeys =
  csdTouchedClassKeys
{-# INLINE classSupportDeltaTouchedClassKeys #-}

classSupportDeltaTouchedCarriers :: ClassSupportDelta c -> IntMap (SupportCarrier c)
classSupportDeltaTouchedCarriers =
  csdTouchedCarriers
{-# INLINE classSupportDeltaTouchedCarriers #-}

insertClassCarrier :: Int -> Maybe (SupportCarrier c) -> SupportCarrier c -> ClassSupportIndex c -> ClassSupportIndex c
insertClassCarrier classKey maybeOriginalCarrier updatedCarrier supportIndex =
  ClassSupportIndex
    { csiCarrierByClassKey =
        IntMap.insert classKey updatedCarrier (csiCarrierByClassKey supportIndex),
      csiClassesByGeneratorKey =
        insertClassIntoGeneratorBuckets
          classKey
          (supportCarrierGeneratorKeys updatedCarrier)
          (foldMap supportCarrierGeneratorKeys maybeOriginalCarrier)
          (csiClassesByGeneratorKey supportIndex)
    }
{-# INLINE insertClassCarrier #-}

insertClassIntoGeneratorBuckets :: Int -> IntSet -> IntSet -> IntMap IntSet -> IntMap IntSet
insertClassIntoGeneratorBuckets classKey updatedGeneratorKeys originalGeneratorKeys buckets =
  IntSet.foldl'
    (\currentBuckets generatorKey ->
      IntMap.insertWith
        IntSet.union
        generatorKey
        (IntSet.singleton classKey)
        currentBuckets
    )
    (removeClassFromGeneratorBuckets classKey (IntSet.difference originalGeneratorKeys updatedGeneratorKeys) buckets)
    updatedGeneratorKeys
{-# INLINE insertClassIntoGeneratorBuckets #-}

removeClassFromGeneratorBuckets :: Int -> IntSet -> IntMap IntSet -> IntMap IntSet
removeClassFromGeneratorBuckets classKey generatorKeys buckets =
  IntSet.foldl'
    (\currentBuckets generatorKey ->
      IntMap.update
        (keepNonEmptyIntSet . IntSet.delete classKey)
        generatorKey
        currentBuckets
    )
    buckets
    generatorKeys
{-# INLINE removeClassFromGeneratorBuckets #-}

keepNonEmptyIntSet :: IntSet -> Maybe IntSet
keepNonEmptyIntSet values
  | IntSet.null values = Nothing
  | otherwise = Just values
{-# INLINE keepNonEmptyIntSet #-}

supportCarrierReachableKeys :: MaterializedContextSite c -> SupportCarrier c -> IntSet
supportCarrierReachableKeys denseSite carrier =
  IntSet.foldr
    (\generatorKey reachableKeys -> IntSet.union (upsetKeysFor denseSite (ContextObjectKey generatorKey)) reachableKeys)
    IntSet.empty
    (supportCarrierGeneratorKeys carrier)
{-# INLINE supportCarrierReachableKeys #-}

visibleGeneratorKeysAtKey :: MaterializedContextSite c -> ContextObjectKey -> IntSet
visibleGeneratorKeysAtKey denseSite (ContextObjectKey keyValue) =
  IntSet.insert
    keyValue
    (IntMap.findWithDefault IntSet.empty keyValue (pcsRestrictionOutKeys denseSite))
{-# INLINE visibleGeneratorKeysAtKey #-}

normalizeSupportCarrierKeys :: PreparedContextSite c -> IntSet -> IntSet
normalizeSupportCarrierKeys site generatorKeys =
  case site of
    SiteMaterialized denseSite ->
      let dominatedByCarrier candidateKey =
            not
              ( IntSet.null
                  ( IntSet.intersection
                      generatorKeys
                      (IntMap.findWithDefault IntSet.empty candidateKey (pcsStrictLowerByKey denseSite))
                  )
              )
       in IntSet.filter (not . dominatedByCarrier) generatorKeys
    SitePowerset _ ->
      let dominatedByCarrier candidateKey =
            any
              (\otherKey -> otherKey /= candidateKey && maskWithin otherKey candidateKey)
              (IntSet.toAscList generatorKeys)
       in IntSet.filter (not . dominatedByCarrier) generatorKeys
{-# INLINE normalizeSupportCarrierKeys #-}

strictLowerRowsFromUpsets :: IntMap IntSet -> IntMap IntSet
strictLowerRowsFromUpsets upsetRows =
  IntMap.foldlWithKey'
    insertDominatedKeys
    emptyRows
    upsetRows
  where
    allKeys =
      IntSet.union (IntMap.keysSet upsetRows) (foldMap id upsetRows)

    emptyRows =
      IntSet.foldl'
        (\rows keyValue -> IntMap.insert keyValue IntSet.empty rows)
        IntMap.empty
        allKeys

    insertDominatedKeys rows lowerKey upperKeys =
      IntSet.foldl'
        (insertDominance lowerKey)
        rows
        upperKeys

    insertDominance lowerKey rows upperKey
      | lowerKey == upperKey = rows
      | otherwise =
          IntMap.insertWith
            IntSet.union
            upperKey
            (IntSet.singleton lowerKey)
            rows
{-# INLINE strictLowerRowsFromUpsets #-}

upsetKeysFor :: MaterializedContextSite c -> ContextObjectKey -> IntSet
upsetKeysFor denseSite (ContextObjectKey keyValue) =
  IntMap.findWithDefault (IntSet.singleton keyValue) keyValue (pcsUpsetByKey denseSite)
{-# INLINE upsetKeysFor #-}

decodeObjectKeys :: Ord c => PreparedContextSite c -> IntSet -> Set c
decodeObjectKeys site keyValues =
  case site of
    SiteMaterialized denseSite ->
      IntSet.foldr
        ( \keyValue decoded ->
            maybe
              decoded
              (`Set.insert` decoded)
              (IntMap.lookup keyValue (pcsObjectsByKey denseSite))
        )
        Set.empty
        keyValues
    SitePowerset powersetSite ->
      IntSet.foldr
        (\keyValue decoded -> Set.insert (pwsDecodeObject powersetSite keyValue) decoded)
        Set.empty
        keyValues
{-# INLINE decodeObjectKeys #-}

decodeObjectKeyList :: MaterializedContextSite c -> IntSet -> [c]
decodeObjectKeyList denseSite keyValues =
  [ objectValue
    | keyValue <- IntSet.toAscList keyValues,
      Just objectValue <- [IntMap.lookup keyValue (pcsObjectsByKey denseSite)]
  ]
{-# INLINE decodeObjectKeyList #-}

objectKeyEntriesFromLattice :: ContextLattice c -> [(c, ContextObjectKey)]
objectKeyEntriesFromLattice lattice =
  [ (objectValue, ContextObjectKey keyValue)
    | (keyValue, objectValue) <- zip [0 ..] (contextLatticeElements lattice)
  ]
{-# INLINE objectKeyEntriesFromLattice #-}

objectsByKeyFromEntries :: [(c, ContextObjectKey)] -> IntMap c
objectsByKeyFromEntries entries =
  IntMap.fromList
    ( fmap
        (\(objectValue, ContextObjectKey keyValue) -> (keyValue, objectValue))
        entries
    )
{-# INLINE objectsByKeyFromEntries #-}

restrictionRowsFromObjectKeyPairs :: Int -> [(ContextObjectKey, ContextObjectKey)] -> IntMap IntSet
restrictionRowsFromObjectKeyPairs objectCount keyPairs =
  foldr insertKeyPair emptyRows keyPairs
  where
    emptyRows =
      IntMap.fromAscList
        [ (keyValue, IntSet.empty)
        | keyValue <- [0 .. objectCount - 1]
        ]

    insertKeyPair (ContextObjectKey sourceKey, ContextObjectKey targetKey) rowsValue =
      IntMap.insertWith
        IntSet.union
        sourceKey
        (IntSet.singleton targetKey)
        rowsValue
{-# INLINE restrictionRowsFromObjectKeyPairs #-}

latticeUpsetsByKey :: Ord c => ContextLattice c -> IntMap IntSet
latticeUpsetsByKey lattice =
  IntMap.fromList
    [ (keyValue, upperKeyValues objectValue)
      | (keyValue, objectValue) <- keyedObjects
    ]
  where
    keyedObjects =
      zip [0 ..] (contextLatticeElements lattice)

    keyByObject =
      Map.fromList
        [ (objectValue, ContextObjectKey keyValue)
        | (keyValue, objectValue) <- keyedObjects
        ]

    upperKeyValues objectValue =
      IntSet.fromList
        [ upperKeyValue
        | (upperKeyValue, upperObject) <- keyedObjects,
          leqContext lattice objectValue upperObject == Right True,
          Map.member upperObject keyByObject
        ]
{-# INLINE latticeUpsetsByKey #-}

swapObjectKeyPair :: (ContextObjectKey, ContextObjectKey) -> (ContextObjectKey, ContextObjectKey)
swapObjectKeyPair (leftKey, rightKey) =
  (rightKey, leftKey)
{-# INLINE swapObjectKeyPair #-}

powersetRefusal :: PowersetContextSite c -> RefusedSiteEnumeration -> SiteEnumerationRefusal
powersetRefusal powersetSite =
  SiteEnumerationRefusal (pwsAtomCount powersetSite)
{-# INLINE powersetRefusal #-}

powersetUniverseMask :: PowersetContextSite c -> Int
powersetUniverseMask powersetSite =
  bit (pwsAtomCount powersetSite) - 1
{-# INLINE powersetUniverseMask #-}

powersetLeqObject :: PowersetContextSite c -> c -> c -> Bool
powersetLeqObject powersetSite leftValue rightValue =
  case (pwsEncodeObject powersetSite leftValue, pwsEncodeObject powersetSite rightValue) of
    (Just leftMask, Just rightMask) -> maskWithin leftMask rightMask
    _ -> False
{-# INLINE powersetLeqObject #-}

maskWithin :: Int -> Int -> Bool
maskWithin narrowMask wideMask =
  narrowMask .&. wideMask == narrowMask
{-# INLINE maskWithin #-}
