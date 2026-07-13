{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | View-free graph capabilities at a context key, served entirely by the
-- base store and the annotated delta buckets: the three consumers guard
-- evaluation needs (canonicalization, least-node lookup, child projection)
-- without ever materializing the fiber view. Absorbed rows need no masking
-- here: their canonical forms coincide with variant or surviving base rows
-- and collapse under canonical set semantics.
module Moonlight.EGraph.Pure.Context.AnnotatedView
  ( AnnotatedContextView,
    annotatedContextViewAtKey,
    annotatedContextViewFromRepresentativeMapAtKey,
    annotatedViewCanonicalize,
    annotatedRowsChildrenByRepresentativeWithin,
    annotatedViewLookupLeastENode,
    annotatedViewProjectChildAt,
    annotatedViewRepresentativePreimages,
    annotatedViewRowsByRepresentative,
    annotatedViewRowsByRepresentativeWithin,
  )
where

import Data.Foldable (toList)
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import Numeric.Natural (Natural)
import Moonlight.Core (ClassId (..), Language, classIdKey, safeIndexNatural)
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    annotatedRepresentativeMapAt,
    annotatedRowsByTagAt,
    absorbedRowsByTagAt,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralLookup (..),
    structuralLookupTupleAll,
    structuralResultKeys,
    structuralTuplesForResultKey,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphStore,
  )
import Moonlight.Sheaf.Context.Site (ContextObjectKey (..))

type AnnotatedContextView :: (Type -> Type) -> Type
data AnnotatedContextView f = AnnotatedContextView
  { acvReps :: !(IntMap Int),
    acvRepPreimages :: !(IntMap [Int]),
    acvVariantRowsByTag :: !(Map (f ()) [(Int, [Int])]),
    acvVariantRootsByForm :: Map (f ()) (Map [Int] [Int]),
    acvAbsorbedRowsByTag :: !(Map (f ()) (Set.Set (Int, [Int])))
  }

annotatedContextViewAtKey ::
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  AnnotatedContextView f
annotatedContextViewAtKey contextKey buckets =
  annotatedContextViewFromRepresentativeMapAtKey
    contextKey
    (annotatedRepresentativeMapAt contextKey buckets)
    buckets

-- | Build the point view from a representative section already projected by
-- the caller. Analysis descent and structural lookup then share one regional
-- projection instead of independently re-reading the same forest.
annotatedContextViewFromRepresentativeMapAtKey ::
  ContextObjectKey ->
  IntMap Int ->
  AnnotatedDeltaBuckets f ->
  AnnotatedContextView f
annotatedContextViewFromRepresentativeMapAtKey contextKey repsAtKey buckets =
  AnnotatedContextView
    { acvReps = repsAtKey,
      acvRepPreimages =
        IntMap.fromListWith
          (<>)
          [(repKey, [classKey]) | (classKey, repKey) <- IntMap.toAscList repsAtKey],
      acvVariantRowsByTag = variantRows,
      acvVariantRootsByForm =
        fmap
          ( Map.fromListWith
              (<>)
              . fmap (\(rootKey, childKeys) -> (childKeys, [rootKey]))
          )
          variantRows,
      acvAbsorbedRowsByTag = fmap Set.fromList (absorbedRowsByTagAt contextKey buckets)
    }
  where
    variantRows = annotatedRowsByTagAt contextKey buckets

annotatedViewCanonicalize ::
  AnnotatedContextView f ->
  EGraph f a ->
  ClassId ->
  ClassId
annotatedViewCanonicalize view graph classId =
  let baseKey = classIdKey (canonicalizeClassId graph classId)
   in ClassId (IntMap.findWithDefault baseKey baseKey (acvReps view))

-- | The complete preimage of one contextual representative.  The regional
-- map is sparse, so the representative itself is supplied explicitly and
-- changed base representatives are appended from the compiled section.
annotatedViewRepresentativePreimages ::
  AnnotatedContextView f ->
  Int ->
  [Int]
annotatedViewRepresentativePreimages view representativeKey =
  representativeKey : IntMap.findWithDefault [] representativeKey (acvRepPreimages view)
{-# INLINE annotatedViewRepresentativePreimages #-}

-- | Exact contextual rows grouped by contextual representative.
--
-- A base row whose original form is absorbed at this context is omitted;
-- regional variants are then inserted.  This is the row-source projection
-- consumed by analysis repair and structural readers, rather than an eager
-- contextual 'EGraph'.
annotatedViewRowsByRepresentative ::
  Language f =>
  AnnotatedContextView f ->
  EGraph f a ->
  IntMap [ENode f]
annotatedViewRowsByRepresentative view graph =
  annotatedViewRowsByRepresentativeWithin domainKeys view graph
  where
    domainKeys =
      IntSet.union
        (IntSet.map contextualKey (structuralResultKeys (eGraphStore graph)))
        ( IntSet.fromList
            [ contextualKey rootKey
              | rows <- Map.elems (acvVariantRowsByTag view),
                (rootKey, _) <- rows
            ]
        )

    contextualKey classKey =
      IntMap.findWithDefault classKey classKey (acvReps view)

-- | Exact contextual rows for only the dependency-closed analysis frontier.
-- Point analysis must not allocate the complete contextual row section when a
-- sparse regional union disturbs only a handful of representatives.
annotatedViewRowsByRepresentativeWithin ::
  Language f =>
  IntSet ->
  AnnotatedContextView f ->
  EGraph f a ->
  IntMap [ENode f]
annotatedViewRowsByRepresentativeWithin representativeKeys view graph =
  IntMap.mapMaybe rowsAtRepresentative (IntMap.fromSet id representativeKeys)
  where
    structuralStore = eGraphStore graph

    contextualKey classKey =
      IntMap.findWithDefault classKey classKey (acvReps view)

    rowsAtRepresentative representativeKey =
      nonEmptyRows
        ( Set.toAscList
            ( Set.fromList
                (survivingBaseRows representativeKey <> regionalVariantRows representativeKey)
            )
        )

    survivingBaseRows representativeKey =
      [ ENode (fmap (ClassId . contextualKey . classIdKey) nodeShape)
        | preimageKey <- annotatedViewRepresentativePreimages view representativeKey,
          ENode nodeShape <- structuralTuplesForResultKey preimageKey structuralStore,
          let tag = void nodeShape
              baseChildren = fmap classIdKey (toList nodeShape),
          not
            ( Set.member
                (preimageKey, baseChildren)
                (Map.findWithDefault Set.empty tag (acvAbsorbedRowsByTag view))
            )
      ]

    regionalVariantRows representativeKey =
      [ ENode contextualNodeShape
        | (tag, rows) <- Map.toAscList (acvVariantRowsByTag view),
          (rootKey, childKeys) <- rows,
          contextualKey rootKey == representativeKey,
          Just contextualNodeShape <-
            [ replaceTaggedChildren
                tag
                (fmap contextualKey childKeys)
            ]
      ]

    nonEmptyRows :: [a] -> Maybe [a]
    nonEmptyRows rows =
      case rows of
        [] -> Nothing
        _ -> Just rows

-- | Dependency edges for the exact contextual row source, restricted to the
-- requested repair domain just like the base structural-store projection.
annotatedRowsChildrenByRepresentativeWithin ::
  Language f =>
  IntSet ->
  IntMap [ENode f] ->
  IntMap (IntMap Int)
annotatedRowsChildrenByRepresentativeWithin repairKeys rowsByRepresentative =
  IntMap.mapMaybe nonEmptyChildren (IntMap.fromSet childrenAt repairKeys)
  where
    childrenAt representativeKey =
      IntMap.restrictKeys
        ( foldl'
            (\children (ENode nodeShape) ->
                foldl'
                  (\counts childClass -> IntMap.insertWith (+) (classIdKey childClass) 1 counts)
                  children
                  nodeShape
            )
            IntMap.empty
            (IntMap.findWithDefault [] representativeKey rowsByRepresentative)
        )
        repairKeys

    nonEmptyChildren :: IntMap a -> Maybe (IntMap a)
    nonEmptyChildren children
      | IntMap.null children = Nothing
      | otherwise = Just children

replaceTaggedChildren :: Traversable f => f () -> [Int] -> Maybe (f ClassId)
replaceTaggedChildren tag childKeys =
  case mapAccumL replaceChild childKeys tag of
    ([], taggedShape) -> either (const Nothing) Just (sequenceA taggedShape)
    (_ : _, _) -> Nothing
  where
    replaceChild :: [Int] -> () -> ([Int], Either () ClassId)
    replaceChild remainingKeys _ =
      case remainingKeys of
        [] -> ([], Left ())
        nextKey : restKeys -> (restKeys, Right (ClassId nextKey))

annotatedViewLookupLeastENode ::
  Language f =>
  AnnotatedContextView f ->
  EGraph f a ->
  ENode f ->
  Maybe ClassId
annotatedViewLookupLeastENode view graph (ENode nodeShape) =
  let canonicalShape =
        fmap (annotatedViewCanonicalize view graph) nodeShape
      canonicalChildren =
        fmap classIdKey (toList canonicalShape)
      baseOwners =
        case structuralLookupTupleAll (ENode canonicalShape) (eGraphStore graph) of
          StructuralMissing -> []
          StructuralUnique ownerClass -> [ownerClass]
          StructuralAmbiguous ownerClasses -> toList ownerClasses
      variantOwners =
        Map.findWithDefault
          []
          canonicalChildren
          (Map.findWithDefault Map.empty (void nodeShape) (acvVariantRootsByForm view))
      candidateKeys =
        fmap (classIdKey . annotatedViewCanonicalize view graph) baseOwners
          <> variantOwners
   in case candidateKeys of
        [] -> Nothing
        _ -> Just (ClassId (minimum candidateKeys))

annotatedViewProjectChildAt ::
  Language f =>
  AnnotatedContextView f ->
  EGraph f a ->
  ClassId ->
  Natural ->
  Maybe ClassId
annotatedViewProjectChildAt view graph classId childIndex =
  let viewKey =
        classIdKey (annotatedViewCanonicalize view graph classId)
      preimageKeys =
        viewKey : IntMap.findWithDefault [] viewKey (acvRepPreimages view)
      baseChildLists =
        [ fmap classIdKey (toList nodeShape)
          | preimageKey <- preimageKeys,
            ENode nodeShape <- structuralTuplesForResultKey preimageKey (eGraphStore graph)
        ]
      variantChildLists =
        [ childKeys
          | rows <- Map.elems (acvVariantRowsByTag view),
            (rootKey, childKeys) <- rows,
            rootKey == viewKey
        ]
      canonicalChildKeys =
        Set.fromList
          ( mapMaybe
              (fmap canonicalKey . safeIndexNatural childIndex)
              (baseChildLists <> variantChildLists)
          )
      canonicalKey childKey =
        classIdKey (annotatedViewCanonicalize view graph (ClassId childKey))
   in case Set.toAscList canonicalChildKeys of
        [uniqueChildKey] -> Just (ClassId uniqueChildKey)
        _ -> Nothing
