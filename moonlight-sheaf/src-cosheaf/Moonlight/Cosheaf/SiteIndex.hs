{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey (..),
    IndexedCosheafMorphism (..),
    CosheafSiteIndex,
    CosheafSiteIndexFailure (..),
    buildCosheafSiteIndex,
    cosheafSiteObjectIndex,
    cosheafIndexedMorphisms,
    cosheafComposableMorphismPairs,
    cosheafCompositionValidationBasis,
    cosheafMorphismKeyOf,
  )
where

import Data.Foldable (foldl', traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (DenseKey (..))
import Moonlight.Core (duplicatesOrd)
import Moonlight.Sheaf.Index.Dense
  ( denseIndexCount,
    denseIndexKeyOf,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    ObjectKey (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteMorphismUniverse,
  )

type CosheafMorphismKey :: Type
newtype CosheafMorphismKey = CosheafMorphismKey
  { unCosheafMorphismKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey CosheafMorphismKey where
  encodeDenseKey =
    unCosheafMorphismKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    CosheafMorphismKey
  {-# INLINE decodeDenseKey #-}

type IndexedCosheafMorphism :: Type -> Type -> Type
data IndexedCosheafMorphism obj mor = IndexedCosheafMorphism
  { icmKey :: !CosheafMorphismKey,
    icmMorphism :: !(CheckedMorphism obj mor),
    icmSourceObjectKey :: !ObjectKey,
    icmTargetObjectKey :: !ObjectKey
  }
  deriving stock (Eq, Show)

type CosheafSiteIndex :: Type -> Type
data CosheafSiteIndex site = CosheafSiteIndex
  { csiObjectIndex :: !(ObjectIndex (SiteObject site)),
    csiIndexedMorphisms :: ![IndexedCosheafMorphism (SiteObject site) (SiteMorphism site)],
    csiMorphismKeysByValue :: !(Map (CheckedMorphism (SiteObject site) (SiteMorphism site)) CosheafMorphismKey),
    csiComposableMorphismPairs ::
      ![ ( IndexedCosheafMorphism (SiteObject site) (SiteMorphism site),
           IndexedCosheafMorphism (SiteObject site) (SiteMorphism site)
         )
       ],
    csiCompositionValidationBasis ::
      ![ ( IndexedCosheafMorphism (SiteObject site) (SiteMorphism site),
           IndexedCosheafMorphism (SiteObject site) (SiteMorphism site)
         )
       ]
  }

deriving stock instance
  (Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (CosheafSiteIndex site)

deriving stock instance
  (Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (CosheafSiteIndex site)

type CosheafSiteIndexFailure :: Type -> Type -> Type
data CosheafSiteIndexFailure obj mor
  = CosheafDuplicateObject !obj
  | CosheafDuplicateMorphism !(CheckedMorphism obj mor)
  | CosheafMorphismSourceUnknown !(CheckedMorphism obj mor)
  | CosheafMorphismTargetUnknown !(CheckedMorphism obj mor)
  deriving stock (Eq, Show)

buildCosheafSiteIndex ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Either
    (CosheafSiteIndexFailure (SiteObject site) (SiteMorphism site))
    (CosheafSiteIndex site)
buildCosheafSiteIndex site = do
  traverse_ (Left . CosheafDuplicateObject) (duplicatesOrd (siteObjects site))
  traverse_ (Left . CosheafDuplicateMorphism) (duplicatesOrd (siteMorphisms site))
  indexedMorphisms <-
    traverse indexMorphism keyedMorphisms
  let morphismKeysByValue =
        Map.fromList (fmap morphismKeyed indexedMorphisms)
      composablePairs =
        composableIndexedMorphismPairs indexedMorphisms
      compositionValidationBasis =
        compositionValidationBasisFor
          (denseIndexCount objectIndex)
          indexedMorphisms
          composablePairs
  pure
    CosheafSiteIndex
      { csiObjectIndex = objectIndex,
        csiIndexedMorphisms = indexedMorphisms,
        csiMorphismKeysByValue = morphismKeysByValue,
        csiComposableMorphismPairs = composablePairs,
        csiCompositionValidationBasis = compositionValidationBasis
      }
  where
    objectIndex =
      mkObjectIndex (siteObjects site)

    keyedMorphisms =
      zip (fmap CosheafMorphismKey [0 ..]) (siteMorphismUniverse site)

    indexMorphism (morphismKey, morphismValue) = do
      sourceKey <-
        maybe
          (Left (CosheafMorphismSourceUnknown morphismValue))
          Right
          (denseIndexKeyOf (cmSource morphismValue) objectIndex)
      targetKey <-
        maybe
          (Left (CosheafMorphismTargetUnknown morphismValue))
          Right
          (denseIndexKeyOf (cmTarget morphismValue) objectIndex)
      pure
        IndexedCosheafMorphism
          { icmKey = morphismKey,
            icmMorphism = morphismValue,
            icmSourceObjectKey = sourceKey,
            icmTargetObjectKey = targetKey
          }

    morphismKeyed :: IndexedCosheafMorphism obj mor -> (CheckedMorphism obj mor, CosheafMorphismKey)
    morphismKeyed indexedMorphism =
      (icmMorphism indexedMorphism, icmKey indexedMorphism)

composableIndexedMorphismPairs ::
  [IndexedCosheafMorphism obj mor] ->
  [(IndexedCosheafMorphism obj mor, IndexedCosheafMorphism obj mor)]
composableIndexedMorphismPairs indexedMorphisms =
  foldMap pairsWithInner indexedMorphisms
  where
    outersBySource =
      IntMap.fromListWith
        (<>)
        [ (unObjectKey (icmSourceObjectKey outerIndexed), [outerIndexed])
        | outerIndexed <- indexedMorphisms
        ]

    pairsWithInner innerIndexed =
      fmap
        (,innerIndexed)
        (IntMap.findWithDefault [] (unObjectKey (icmTargetObjectKey innerIndexed)) outersBySource)

compositionValidationBasisFor ::
  Int ->
  [IndexedCosheafMorphism obj mor] ->
  [(IndexedCosheafMorphism obj mor, IndexedCosheafMorphism obj mor)] ->
  [(IndexedCosheafMorphism obj mor, IndexedCosheafMorphism obj mor)]
compositionValidationBasisFor objectCount indexedMorphisms composablePairs =
  if isThinMorphismUniverse objectCount indexedMorphisms
    && isClosedAcyclicEndpointRelation indexedMorphisms
    then filter (innerMorphismInThinValidationBasis generatorKeys) composablePairs
    else composablePairs
  where
    generatorKeys =
      hasseGeneratorMorphismKeys indexedMorphisms

isClosedAcyclicEndpointRelation ::
  [IndexedCosheafMorphism obj mor] ->
  Bool
isClosedAcyclicEndpointRelation indexedMorphisms =
  all edgeDominatesSuccessors nonIdentityEdges
  where
    nonIdentityEdges =
      [ ( unObjectKey (icmSourceObjectKey indexedMorphism),
          unObjectKey (icmTargetObjectKey indexedMorphism)
        )
      | indexedMorphism <- indexedMorphisms,
        not (isIdentityEndpoint indexedMorphism)
      ]

    outNeighborsBySource =
      IntMap.fromListWith
        IntSet.union
        [ (sourceKey, IntSet.singleton targetKey)
        | (sourceKey, targetKey) <- nonIdentityEdges
        ]

    edgeDominatesSuccessors (sourceKey, targetKey) =
      IntSet.isSubsetOf
        (IntMap.findWithDefault IntSet.empty targetKey outNeighborsBySource)
        (IntMap.findWithDefault IntSet.empty sourceKey outNeighborsBySource)

isThinMorphismUniverse ::
  Int ->
  [IndexedCosheafMorphism obj mor] ->
  Bool
isThinMorphismUniverse objectCount =
  maybe False (const True) . foldl' insertEndpointKey (Just IntSet.empty)
  where
    insertEndpointKey Nothing _ =
      Nothing
    insertEndpointKey (Just endpointKeys) indexedMorphism =
      let encodedKey =
            encodedEndpointKey objectCount indexedMorphism
       in if IntSet.member encodedKey endpointKeys
            then Nothing
            else Just (IntSet.insert encodedKey endpointKeys)

hasseGeneratorMorphismKeys ::
  [IndexedCosheafMorphism obj mor] ->
  IntSet
hasseGeneratorMorphismKeys indexedMorphisms =
  IntSet.fromList
    [ unCosheafMorphismKey (icmKey indexedMorphism)
    | indexedMorphism <- indexedMorphisms,
      isHasseGenerator indexedMorphism
    ]
  where
    outNeighborsBySource =
      IntMap.fromListWith
        IntSet.union
        [ (sourceKey, IntSet.singleton targetKey)
        | indexedMorphism <- indexedMorphisms,
          let sourceKey = unObjectKey (icmSourceObjectKey indexedMorphism),
          let targetKey = unObjectKey (icmTargetObjectKey indexedMorphism)
        ]

    inNeighborsByTarget =
      IntMap.fromListWith
        IntSet.union
        [ (targetKey, IntSet.singleton sourceKey)
        | indexedMorphism <- indexedMorphisms,
          let sourceKey = unObjectKey (icmSourceObjectKey indexedMorphism),
          let targetKey = unObjectKey (icmTargetObjectKey indexedMorphism)
        ]

    isHasseGenerator indexedMorphism =
      not (isIdentityEndpoint indexedMorphism)
        && not (isCompositeEndpoint indexedMorphism)

    isCompositeEndpoint indexedMorphism =
      not . IntSet.null $
        IntSet.delete sourceKey
          ( IntSet.delete targetKey
              ( IntSet.intersection
                  (IntMap.findWithDefault IntSet.empty sourceKey outNeighborsBySource)
                  (IntMap.findWithDefault IntSet.empty targetKey inNeighborsByTarget)
              )
          )
      where
        sourceKey =
          unObjectKey (icmSourceObjectKey indexedMorphism)

        targetKey =
          unObjectKey (icmTargetObjectKey indexedMorphism)

innerMorphismInThinValidationBasis ::
  IntSet ->
  (IndexedCosheafMorphism obj mor, IndexedCosheafMorphism obj mor) ->
  Bool
innerMorphismInThinValidationBasis generatorKeys (_, innerIndexed) =
  isIdentityEndpoint innerIndexed
    || IntSet.member (unCosheafMorphismKey (icmKey innerIndexed)) generatorKeys

isIdentityEndpoint ::
  IndexedCosheafMorphism obj mor ->
  Bool
isIdentityEndpoint indexedMorphism =
  icmSourceObjectKey indexedMorphism == icmTargetObjectKey indexedMorphism

encodedEndpointKey ::
  Int ->
  IndexedCosheafMorphism obj mor ->
  Int
encodedEndpointKey objectCount indexedMorphism =
  unObjectKey (icmSourceObjectKey indexedMorphism) * objectCount
    + unObjectKey (icmTargetObjectKey indexedMorphism)

cosheafSiteObjectIndex ::
  CosheafSiteIndex site ->
  ObjectIndex (SiteObject site)
cosheafSiteObjectIndex =
  csiObjectIndex
{-# INLINE cosheafSiteObjectIndex #-}

cosheafIndexedMorphisms ::
  CosheafSiteIndex site ->
  [IndexedCosheafMorphism (SiteObject site) (SiteMorphism site)]
cosheafIndexedMorphisms =
  csiIndexedMorphisms
{-# INLINE cosheafIndexedMorphisms #-}

cosheafComposableMorphismPairs ::
  CosheafSiteIndex site ->
  [(IndexedCosheafMorphism (SiteObject site) (SiteMorphism site), IndexedCosheafMorphism (SiteObject site) (SiteMorphism site))]
cosheafComposableMorphismPairs =
  csiComposableMorphismPairs
{-# INLINE cosheafComposableMorphismPairs #-}

cosheafCompositionValidationBasis ::
  CosheafSiteIndex site ->
  [(IndexedCosheafMorphism (SiteObject site) (SiteMorphism site), IndexedCosheafMorphism (SiteObject site) (SiteMorphism site))]
cosheafCompositionValidationBasis =
  csiCompositionValidationBasis
{-# INLINE cosheafCompositionValidationBasis #-}

cosheafMorphismKeyOf ::
  (Ord (SiteObject site), Ord (SiteMorphism site)) =>
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CosheafSiteIndex site ->
  Maybe CosheafMorphismKey
cosheafMorphismKeyOf morphismValue =
  Map.lookup morphismValue . csiMorphismKeysByValue
{-# INLINE cosheafMorphismKeyOf #-}
