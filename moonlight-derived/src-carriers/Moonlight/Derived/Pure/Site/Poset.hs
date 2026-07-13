{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , FinObjectId (..)
  , mkDerivedPosetFromOrderEdges
  , derivedPosetFromFinCat
  , derivedPosetFromSiteManifest
  , categoryFromOrderClosure
  , mkDerivedPosetFunctor
  , derivedPosetFunctorFromFinThinFunctor
  , identityDerivedPosetFunctor
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  , applyDerivedPosetFunctor
  , derivedPosetFunctorObjectPairs
  , star
  , leq
  , leqChecked
  , closureOf
  , mkDerivedPosetFromCovers
  , memberOfDerivedPoset
  , starChecked
  , starValidated
  , closureOfChecked
  , closureOfValidated
  ) where

import Control.Monad ((>=>))
import Data.Bits (setBit)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Category
  ( FinCat
  , FinThinFunctor
  , FinObjectId (..)
  , SiteManifest
  , finCatExplicitMorphismMapView
  , finCatObjects
  , finThinFunctorSource
  , finThinFunctorTarget
  , finThinFunctorObjectMap
  , mkFinThinFunctor
  , siteImportsAsFinCat
  )
import Moonlight.Category.Pure.FinCat
  ( FinGeneratorId (..)
  , FinMorphismId (..)
  , trustedDenseThinFinCatFromReachabilityRows
  , trustedThinFinCatFromTransitiveEndpoints
  )
import Moonlight.Core (MoonlightError (..))
import Moonlight.Core (worklistFold)
import Moonlight.Core (queueFromList)
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureToMoonlightError
  )

type DerivedPoset :: Type
data DerivedPoset = DerivedPoset
  { derivedPosetCategory :: !FinCat
  , derivedPosetNodes    :: !(Vector FinObjectId)
  , derivedPosetUpper    :: !(IntMap IntSet)
  , derivedPosetLower    :: !(IntMap IntSet)
  , derivedPosetCoversUp :: !(IntMap IntSet)
  , derivedPosetTopoDesc :: !(Vector FinObjectId)
  , derivedPosetTopoAsc  :: !(Vector FinObjectId)
  } deriving stock (Show)

-- The category handle is an exact token of the finite order.  Comparing the
-- derived caches again would merely replay equality of views already sealed by
-- that token.
instance Eq DerivedPoset where
  leftPoset == rightPoset =
    derivedPosetCategory leftPoset == derivedPosetCategory rightPoset

type DerivedPosetFunctor :: Type
-- | The derived-site section of a finite thin functor.  Category validation
-- and finite-category descent happen once at construction; derived operations
-- consume the glued sites and total object action without reconstructing them.
data DerivedPosetFunctor = DerivedPosetFunctor
  { derivedPosetFunctorSource :: !DerivedPoset
  , derivedPosetFunctorTarget :: !DerivedPoset
  , derivedPosetFunctorAction :: !DerivedPosetFunctorAction
  }
  deriving stock (Show)

type DerivedPosetFunctorAction :: Type
data DerivedPosetFunctorAction
  = IdentityDerivedPosetFunctorAction
  | DenseDerivedPosetFunctorAction !(Vector FinObjectId) ![(FinObjectId, FinObjectId)]
  | SparseDerivedPosetFunctorAction !(IntMap FinObjectId) ![(FinObjectId, FinObjectId)]
  deriving stock (Show)

star :: DerivedPoset -> FinObjectId -> IntSet
star DerivedPoset{derivedPosetUpper} (FinObjectId objectKey) =
  IM.findWithDefault (IS.singleton objectKey) objectKey derivedPosetUpper

leq :: DerivedPoset -> FinObjectId -> FinObjectId -> Bool
leq posetValue leftObject (FinObjectId rightKey) =
  IS.member rightKey (star posetValue leftObject)

leqChecked :: DerivedPoset -> FinObjectId -> FinObjectId -> Either DerivedFailure Bool
leqChecked posetValue leftObject rightObject@(FinObjectId rightKey) = do
  upperSet <- starChecked posetValue leftObject
  if memberOfDerivedPoset posetValue rightObject
    then Right (IS.member rightKey upperSet)
    else Left (DerivedPosetUnknownNode rightKey)

closureOf :: DerivedPoset -> IntSet -> IntSet
closureOf DerivedPoset{derivedPosetLower} objectKeys =
  IS.unions
    [ IM.findWithDefault (IS.singleton objectKey) objectKey derivedPosetLower
    | objectKey <- IS.toList objectKeys
    ]

memberOfDerivedPoset :: DerivedPoset -> FinObjectId -> Bool
memberOfDerivedPoset DerivedPoset{derivedPosetUpper} (FinObjectId objectKey) =
  IM.member objectKey derivedPosetUpper

starChecked :: DerivedPoset -> FinObjectId -> Either DerivedFailure IntSet
starChecked DerivedPoset{derivedPosetUpper} (FinObjectId objectKey) =
  case IM.lookup objectKey derivedPosetUpper of
    Just s -> Right s
    Nothing -> Left (DerivedPosetUnknownNode objectKey)

starValidated :: DerivedPoset -> FinObjectId -> Either MoonlightError IntSet
starValidated posetValue =
  firstMoonlight . starChecked posetValue

closureOfChecked :: DerivedPoset -> IntSet -> Either DerivedFailure IntSet
closureOfChecked posetValue@DerivedPoset{derivedPosetLower} objectKeys =
  let unknown = IS.filter (not . (`IM.member` derivedPosetLower)) objectKeys
  in if IS.null unknown
       then Right (closureOf posetValue objectKeys)
       else Left (DerivedPosetUnknownNode (IS.findMin unknown))

closureOfValidated :: DerivedPoset -> IntSet -> Either MoonlightError IntSet
closureOfValidated posetValue =
  firstMoonlight . closureOfChecked posetValue

mkDerivedPosetFromOrderEdges :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> Either DerivedFailure DerivedPoset
mkDerivedPosetFromOrderEdges =
  mkPosetFromCoversChecked

derivedPosetFromFinCat :: FinCat -> Either DerivedFailure DerivedPoset
derivedPosetFromFinCat categoryValue =
  validateThinEndpointMultiplicity morphismEntries
    *> validateNoNonIdentityEndomorphism morphismEntries
    *> mkDerivedPosetFromOrderEdges objectIds orderEdges
  where
    objectIds =
      Set.toAscList (finCatObjects categoryValue)
    morphismEntries =
      Map.toAscList (finCatExplicitMorphismMapView categoryValue)
    orderEdges =
      [ (FinObjectId sourceKey, FinObjectId targetKey)
      | ((FinObjectId sourceKey, FinObjectId targetKey), morphisms) <- morphismEntries
      , sourceKey /= targetKey
      , not (null morphisms)
      ]

derivedPosetFromSiteManifest :: Ord obj => SiteManifest obj -> Either DerivedFailure DerivedPoset
derivedPosetFromSiteManifest =
  firstSite . siteImportsAsFinCat >=> derivedPosetFromFinCat

mkDerivedPosetFunctor ::
  DerivedPoset ->
  DerivedPoset ->
  Map.Map FinObjectId FinObjectId ->
  Either DerivedFailure DerivedPosetFunctor
mkDerivedPosetFunctor sourcePoset targetPoset objectMap = do
  finiteFunctor <-
    either
      (Left . DerivedFunctorInvalidProjection . show)
      Right
      (mkFinThinFunctor (derivedPosetCategory sourcePoset) (derivedPosetCategory targetPoset) objectMap)
  Right (derivedPosetFunctorFromValidated sourcePoset targetPoset finiteFunctor)

derivedPosetFunctorFromFinThinFunctor :: FinThinFunctor -> Either DerivedFailure DerivedPosetFunctor
derivedPosetFunctorFromFinThinFunctor finiteFunctor = do
  sourcePoset <- derivedPosetFromFinCat (finThinFunctorSource finiteFunctor)
  targetPoset <- derivedPosetFromFinCat (finThinFunctorTarget finiteFunctor)
  Right (derivedPosetFunctorFromValidated sourcePoset targetPoset finiteFunctor)

identityDerivedPosetFunctor :: DerivedPoset -> DerivedPosetFunctor
identityDerivedPosetFunctor posetValue =
  DerivedPosetFunctor
    { derivedPosetFunctorSource = posetValue
    , derivedPosetFunctorTarget = posetValue
    , derivedPosetFunctorAction = IdentityDerivedPosetFunctorAction
    }

applyDerivedPosetFunctor :: DerivedPosetFunctor -> FinObjectId -> Either DerivedFailure FinObjectId
applyDerivedPosetFunctor DerivedPosetFunctor {derivedPosetFunctorSource, derivedPosetFunctorAction} sourceObject@(FinObjectId sourceKey)
  | not (memberOfDerivedPoset derivedPosetFunctorSource sourceObject) =
      Left (DerivedFunctorApplicationFailed ("unknown source object " <> show sourceObject))
  | otherwise =
      case derivedPosetFunctorAction of
        IdentityDerivedPosetFunctorAction -> Right sourceObject
        DenseDerivedPosetFunctorAction targetObjects _ ->
          maybe
            (Left (DerivedFunctorApplicationFailed ("missing validated dense source object " <> show sourceObject)))
            Right
            (targetObjects V.!? sourceKey)
        SparseDerivedPosetFunctorAction objectMap _ ->
          maybe
            (Left (DerivedFunctorApplicationFailed ("missing validated source object " <> show sourceObject)))
            Right
            (IM.lookup sourceKey objectMap)

derivedPosetFunctorObjectPairs :: DerivedPosetFunctor -> Either DerivedFailure [(FinObjectId, FinObjectId)]
derivedPosetFunctorObjectPairs DerivedPosetFunctor {derivedPosetFunctorSource, derivedPosetFunctorAction} =
  case derivedPosetFunctorAction of
    IdentityDerivedPosetFunctorAction ->
      Right (fmap (\sourceObject -> (sourceObject, sourceObject)) (V.toList sourceObjects))
    DenseDerivedPosetFunctorAction _ objectPairs -> Right objectPairs
    SparseDerivedPosetFunctorAction _ objectPairs -> Right objectPairs
  where
    sourceObjects = derivedPosetNodes derivedPosetFunctorSource

derivedPosetFunctorFromValidated :: DerivedPoset -> DerivedPoset -> FinThinFunctor -> DerivedPosetFunctor
derivedPosetFunctorFromValidated sourcePoset targetPoset finiteFunctor =
  DerivedPosetFunctor
    { derivedPosetFunctorSource = sourcePoset
    , derivedPosetFunctorTarget = targetPoset
    , derivedPosetFunctorAction =
        if derivedPosetNodes sourcePoset == V.generate (V.length sourceObjects) FinObjectId
          then DenseDerivedPosetFunctorAction (V.fromList (fmap snd objectEntries)) objectEntries
          else
            SparseDerivedPosetFunctorAction
              ( IM.fromList
                  [ (sourceKey, targetObject)
                  | (FinObjectId sourceKey, targetObject) <- objectEntries
                  ]
              )
              objectEntries
    }
  where
    objectEntries = Map.toAscList (finThinFunctorObjectMap finiteFunctor)
    sourceObjects = derivedPosetNodes sourcePoset

mkDerivedPosetFromCovers :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> Either MoonlightError DerivedPoset
mkDerivedPosetFromCovers rawObjects rawCovers =
  either (Left . derivedFailureToMoonlightError) Right (mkPosetFromCoversChecked rawObjects rawCovers)

mkPosetFromCoversChecked :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> Either DerivedFailure DerivedPoset
mkPosetFromCoversChecked rawObjects rawCovers
  | Just (FinObjectId selfLoopKey) <- firstSelfLoop rawCovers =
      Left (DerivedPosetSelfLoop selfLoopKey)
  | otherwise =
      let topoAscList = kahnTopo objects coversUp indeg0
       in case topoAscList of
            Left err -> Left err
            Right ascList ->
              let topoDescList = reverse ascList
                  upper = buildUpper topoDescList coversUp
                  lower = buildLower objects upper
                  canonicalCovers = transitiveReduction objects upper
                  categoryValue = categoryFromOrderClosure objects upper
              in Right DerivedPoset
                    { derivedPosetCategory = categoryValue
                    , derivedPosetNodes = V.fromList objects
                    , derivedPosetUpper = upper
                    , derivedPosetLower = lower
                    , derivedPosetCoversUp = canonicalCovers
                    , derivedPosetTopoDesc = V.fromList topoDescList
                    , derivedPosetTopoAsc = V.fromList ascList
                    }
  where
    objects =
      fmap FinObjectId
        (IS.toAscList (IS.fromList (fmap unFinObjectId (rawObjects ++ concatMap (\(leftObject, rightObject) -> [leftObject, rightObject]) rawCovers))))
    objectKeys = map unFinObjectId objects
    (coversUp, indeg0) =
      foldl'
        insertCanonicalEdge
        (zeroAdj objectKeys, IM.fromList [ (objectKey, (0 :: Int)) | objectKey <- objectKeys ])
        rawCovers

    zeroAdj ks = IM.fromList [ (k, IS.empty) | k <- ks ]
    insertCanonicalEdge :: (IntMap IntSet, IntMap Int) -> (FinObjectId, FinObjectId) -> (IntMap IntSet, IntMap Int)
    insertCanonicalEdge stateValue@(adjacency, indegrees) (FinObjectId sourceKey, FinObjectId targetKey)
      | IS.member targetKey (IM.findWithDefault IS.empty sourceKey adjacency) = stateValue
      | otherwise =
          ( IM.insertWith IS.union sourceKey (IS.singleton targetKey) adjacency
          , IM.insertWith (+) targetKey 1 indegrees
          )

    kahnTopo :: [FinObjectId] -> IntMap IntSet -> IntMap Int -> Either DerivedFailure [FinObjectId]
    kahnTopo objectValues adj indegInit =
      let (_, revOut) =
            worklistFold
              step
              (indegInit, [])
              (queueFromList start)
       in if length revOut == length objectValues
            then Right (reverse revOut)
            else Left DerivedPosetCycle
      where
        start = [ objectValue | objectValue@(FinObjectId objectKey) <- objectValues, IM.findWithDefault 0 objectKey indegInit == 0 ]
        step :: (IntMap Int, [FinObjectId]) -> FinObjectId -> ((IntMap Int, [FinObjectId]), [FinObjectId])
        step (indeg, revOut) objectValue@(FinObjectId objectKey) =
          let succs = IS.toList (IM.findWithDefault IS.empty objectKey adj)
              (indeg', newZerosRev) = foldl' dec (indeg, []) succs
           in ((indeg', objectValue : revOut), fmap FinObjectId (reverse newZerosRev))
        dec :: (IntMap Int, [Int]) -> Int -> (IntMap Int, [Int])
        dec (m, zs) y =
          let d = IM.findWithDefault 0 y m - 1
              m' = IM.insert y d m
          in if d == 0 then (m', y : zs) else (m', zs)

    buildUpper :: [FinObjectId] -> IntMap IntSet -> IntMap IntSet
    buildUpper desc adj = foldl' step IM.empty desc
      where
        step acc (FinObjectId objectKey) =
          let kids = IM.findWithDefault IS.empty objectKey adj
              up = IS.insert objectKey $ foldl'
                     (\s child -> IS.union s (IM.findWithDefault (IS.singleton child) child acc))
                     IS.empty (IS.toList kids)
          in IM.insert objectKey up acc

    buildLower ns upperMap = foldl' step initLower ns
      where
        initLower = IM.fromList [ (unFinObjectId objectValue, IS.singleton (unFinObjectId objectValue)) | objectValue <- ns ]
        step acc (FinObjectId objectKey) =
          let ups = IM.findWithDefault (IS.singleton objectKey) objectKey upperMap
          in foldl' (\m y -> IM.insertWith IS.union y (IS.singleton objectKey) m) acc (IS.toList ups)

    transitiveReduction :: [FinObjectId] -> IntMap IntSet -> IntMap IntSet
    transitiveReduction ns upperMap =
      IM.fromList
        [ (sourceKey, IS.filter (isCover sourceKey) strictSuccessors)
        | FinObjectId sourceKey <- ns
        , let strictSuccessors = IS.delete sourceKey (IM.findWithDefault IS.empty sourceKey upperMap)
        ]
      where
        isCover sourceKey targetKey =
          IS.null
            ( IS.filter
                (\middleKey ->
                   middleKey /= targetKey
                     && IS.member targetKey (IM.findWithDefault IS.empty middleKey upperMap)
                )
                (IS.delete sourceKey (IM.findWithDefault IS.empty sourceKey upperMap))
            )

firstMoonlight :: Either DerivedFailure value -> Either MoonlightError value
firstMoonlight =
  either (Left . derivedFailureToMoonlightError) Right

firstSite :: Either err value -> Either DerivedFailure value
firstSite =
  either (const (Left (DerivedPosetSiteLoweringFailed "invalid site manifest"))) Right

categoryFromOrderClosure :: [FinObjectId] -> IntMap IntSet -> FinCat
categoryFromOrderClosure objectValues upperMap =
  if objectValues == fmap FinObjectId [0 .. length objectValues - 1]
    then
      trustedDenseThinFinCatFromReachabilityRows
        objectSet
        ( V.fromList
            [ IS.foldl'
                (\reachableBits targetKey -> if sourceKey == targetKey then reachableBits else setBit reachableBits targetKey)
                0
                (IM.findWithDefault IS.empty sourceKey upperMap)
            | FinObjectId sourceKey <- objectValues
            ]
        )
    else
      trustedThinFinCatFromTransitiveEndpoints
        objectSet
        ( Map.fromAscList
            ( zipWith
                (\endpointValue generatorIndex -> (endpointValue, FinGeneratorMorphismId (FinGeneratorId generatorIndex)))
                [ (FinObjectId sourceKey, FinObjectId targetKey)
                | FinObjectId sourceKey <- objectValues
                , targetKey <- IS.toAscList (IM.findWithDefault IS.empty sourceKey upperMap)
                , sourceKey /= targetKey
                ]
                [0 ..]
            )
        )
  where
    objectSet =
      Set.fromDistinctAscList objectValues

validateThinEndpointMultiplicity ::
  [((FinObjectId, FinObjectId), [morphism])] ->
  Either DerivedFailure ()
validateThinEndpointMultiplicity morphismEntries =
  case filter ((> 1) . length . snd) morphismEntries of
    [] -> Right ()
    multiple : _ -> Left (DerivedPosetNonThinCategory (showEndpoint (fst multiple)))

validateNoNonIdentityEndomorphism ::
  [((FinObjectId, FinObjectId), [morphism])] ->
  Either DerivedFailure ()
validateNoNonIdentityEndomorphism morphismEntries =
  case filter diagonalNonIdentity morphismEntries of
    [] -> Right ()
    diagonal : _ -> Left (DerivedPosetNonPosetalCategory (showEndpoint (fst diagonal)))
  where
    diagonalNonIdentity :: ((FinObjectId, FinObjectId), [morphism]) -> Bool
    diagonalNonIdentity ((sourceId, targetId), morphisms) =
      sourceId == targetId && not (null morphisms)

showEndpoint :: (FinObjectId, FinObjectId) -> String
showEndpoint (FinObjectId sourceKey, FinObjectId targetKey) =
  show (sourceKey, targetKey)

firstSelfLoop :: [(FinObjectId, FinObjectId)] -> Maybe FinObjectId
firstSelfLoop =
  fmap fst . safeFirst . filter (uncurry (==))

safeFirst :: [value] -> Maybe value
safeFirst values =
  case values of
    [] -> Nothing
    firstValue : _ -> Just firstValue
