{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Site.Skeleton.RowSource
  ( NerveRowSource (..),
    SkeletonRowPlan (..),
    DenseNerveArrangement,
    DenseNerveMorphismOption,
    DenseNerveArrangementError (..),
    nerveSimplexFace,
    skeletonRowPlan,
    truncatedSkeletonRowPlan,
    prepareDenseNerveArrangement,
    denseNerveArrangementObjectCount,
    denseNerveArrangementObjectAt,
    denseNerveArrangementMorphismsFromOrdinal,
    denseNerveMorphismOptionValue,
    denseNerveMorphismOptionTargetOrdinal,
    denseOrdinalNerveRowSource,
    denseOrdinalSkeletonRowPlan,
    denseOrdinalSkeletonRowPlanWithDepth,
    simplicialNerveRowSource,
    wcojNerveRowSource,
    rowSourceToTruncatedNerve,
    denseArrangementCategory,
  )
where

import Control.Monad (guard)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Category
  ( Category (..),
    ComposableChain,
    FiniteComposableCategory (..),
    appendComposableMorphism,
    chainMorphisms,
    chainStartObject,
    mkComposableChain,
    singletonComposableChain,
  )
import Moonlight.Differential.Join.WCOJ
  ( Domain,
    Env,
    JoinAlgebra (..),
    Slot,
    adaptiveJoin,
    domainEmpty,
    domainFromListPreservingOrder,
    domainSize,
    domainSingleton,
  )
import Moonlight.Sheaf.Site.Skeleton.Window
  ( SiteSkeletonWindow (..),
    siteWindowDepth,
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerveChainVertices,
    nerveSimplexDegeneracy,
    nerveSimplexDimension,
    nerveSimplexFace,
    nerveSimplexFromChain,
  )
import Moonlight.Category.Simplicial
  ( TruncatedNormalizedSSet,
    TruncatedSSetObstruction,
    applyDegeneracyAtDimension,
    applyFaceAtDimension,
    mkTruncatedSSet,
    simplicesAtDimension,
  )
import Moonlight.Category.Simplicial (finValue)
import Numeric.Natural (Natural)

data NerveRowSource category = NerveRowSource
  { nerveRowsAtDimension :: Natural -> [NerveSimplex category],
    nerveFaceAt :: Natural -> Natural -> NerveSimplex category -> Maybe (NerveSimplex category),
    nerveDegeneracyAt :: Natural -> Natural -> NerveSimplex category -> Maybe (NerveSimplex category)
  }

simplexFaceRowSource ::
  (Category category, Eq (Ob category)) =>
  category ->
  (Natural -> [NerveSimplex category]) ->
  NerveRowSource category
simplexFaceRowSource categoryValue rowsAt =
  NerveRowSource
    { nerveRowsAtDimension = rowsAt,
      nerveFaceAt = const (nerveSimplexFace categoryValue),
      nerveDegeneracyAt = const (nerveSimplexDegeneracy categoryValue)
    }

data SkeletonRowPlan category = SkeletonRowPlan
  { skeletonRowPlanDepth :: Natural,
    skeletonRowPlanCellDimensions :: Set Natural,
    skeletonRowPlanFaceSourceDimensions :: Set Natural,
    skeletonRowPlanSource :: NerveRowSource category
  }

skeletonRowPlan ::
  Set Natural ->
  Set Natural ->
  NerveRowSource category ->
  SkeletonRowPlan category
skeletonRowPlan cellDimensions faceSourceDimensions rowSource =
  mkSkeletonRowPlan
    (rowPlanDepth cellDimensions faceSourceDimensions)
    cellDimensions
    faceSourceDimensions
    rowSource

mkSkeletonRowPlan ::
  Natural ->
  Set Natural ->
  Set Natural ->
  NerveRowSource category ->
  SkeletonRowPlan category
mkSkeletonRowPlan depthValue cellDimensions faceSourceDimensions rowSource =
  SkeletonRowPlan
    { skeletonRowPlanDepth = depthValue,
      skeletonRowPlanCellDimensions = cellDimensions,
      skeletonRowPlanFaceSourceDimensions = faceSourceDimensions,
      skeletonRowPlanSource = rowSource
    }

rowPlanDepth :: Set Natural -> Set Natural -> Natural
rowPlanDepth cellDimensions faceSourceDimensions =
  siteWindowDepth
    ( SiteSkeletonWindow
        { sswCellDimensions = cellDimensions,
          sswFaceSourceDimensions = faceSourceDimensions
        }
    )

truncatedSkeletonRowPlan ::
  Natural ->
  NerveRowSource category ->
  SkeletonRowPlan category
truncatedSkeletonRowPlan depthValue rowSource =
  mkSkeletonRowPlan
    depthValue
    (Set.fromAscList [0 .. depthValue])
    (Set.fromAscList [1 .. depthValue])
    rowSource

data DenseNerveArrangement category = DenseNerveArrangement
  { denseArrangementCategoryInternal :: !category,
    denseArrangementObjects :: !(Vector (Ob category)),
    denseArrangementMorphismsBySource :: !(IntMap.IntMap [DenseNerveMorphismOption category])
  }

data DenseNerveMorphismOption category = DenseNerveMorphismOption
  { denseMorphismValue :: !(Mor category),
    denseMorphismTargetOrdinal :: {-# UNPACK #-} !Int
  }

data DenseNerveChainRow category = DenseNerveChainRow
  { denseChainValue :: !(ComposableChain category),
    denseChainTerminalOrdinal :: {-# UNPACK #-} !Int
  }

data DenseNerveArrangementError category
  = DenseNerveTargetObjectAbsent !(Mor category) !(Ob category)
  | DenseNerveTargetUnavailable !(Mor category)

deriving stock instance
  (Show (Mor category), Show (Ob category)) =>
  Show (DenseNerveArrangementError category)

deriving stock instance
  (Eq (Mor category), Eq (Ob category)) =>
  Eq (DenseNerveArrangementError category)

prepareDenseNerveArrangement ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Eq (Mor category)
  ) =>
  category ->
  Either (DenseNerveArrangementError category) (DenseNerveArrangement category)
prepareDenseNerveArrangement categoryValue =
  let objectValues = enumerateObjects categoryValue
      objectOrdinals = Map.fromList (zip objectValues [0 :: Int ..])
   in fmap
        ( \morphismsBySource ->
            DenseNerveArrangement
              { denseArrangementCategoryInternal = categoryValue,
                denseArrangementObjects = Vector.fromList objectValues,
                denseArrangementMorphismsBySource = IntMap.fromList morphismsBySource
              }
        )
        ( traverse
            (denseMorphismRowForSource categoryValue objectOrdinals)
            (zip [0 :: Int ..] objectValues)
        )

denseArrangementCategory :: DenseNerveArrangement category -> category
denseArrangementCategory = denseArrangementCategoryInternal

denseMorphismRowForSource ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Eq (Mor category)
  ) =>
  category ->
  Map.Map (Ob category) Int ->
  (Int, Ob category) ->
  Either (DenseNerveArrangementError category) (Int, [DenseNerveMorphismOption category])
denseMorphismRowForSource categoryValue objectOrdinals (sourceOrdinal, sourceObject) =
  fmap
    (\morphismOptions -> (sourceOrdinal, morphismOptions))
    ( traverse
        (denseMorphismOption categoryValue objectOrdinals)
        (filter (nondegenerateMorphism categoryValue) (enumerateMorphismsFrom categoryValue sourceObject))
    )

denseMorphismOption ::
  ( Category category,
    Ord (Ob category)
  ) =>
  category ->
  Map.Map (Ob category) Int ->
  Mor category ->
  Either (DenseNerveArrangementError category) (DenseNerveMorphismOption category)
denseMorphismOption categoryValue objectOrdinals morphismValue =
  case target categoryValue morphismValue of
    Right targetObject ->
      case Map.lookup targetObject objectOrdinals of
        Just targetOrdinal ->
          Right
            DenseNerveMorphismOption
              { denseMorphismValue = morphismValue,
                denseMorphismTargetOrdinal = targetOrdinal
              }
        Nothing ->
          Left (DenseNerveTargetObjectAbsent morphismValue targetObject)
    Left _ ->
      Left (DenseNerveTargetUnavailable morphismValue)

denseNerveArrangementObjectCount :: DenseNerveArrangement category -> Int
denseNerveArrangementObjectCount =
  Vector.length . denseArrangementObjects

denseNerveArrangementObjectAt ::
  Int ->
  DenseNerveArrangement category ->
  Maybe (Ob category)
denseNerveArrangementObjectAt objectOrdinal arrangement =
  if objectOrdinal < 0
    then Nothing
    else denseArrangementObjects arrangement Vector.!? objectOrdinal

denseNerveArrangementMorphismsFromOrdinal ::
  Int ->
  DenseNerveArrangement category ->
  [DenseNerveMorphismOption category]
denseNerveArrangementMorphismsFromOrdinal objectOrdinal arrangement =
  IntMap.findWithDefault [] objectOrdinal (denseArrangementMorphismsBySource arrangement)

denseNerveMorphismOptionValue ::
  DenseNerveMorphismOption category ->
  Mor category
denseNerveMorphismOptionValue =
  denseMorphismValue

denseNerveMorphismOptionTargetOrdinal ::
  DenseNerveMorphismOption category ->
  Int
denseNerveMorphismOptionTargetOrdinal =
  denseMorphismTargetOrdinal

denseOrdinalNerveRowSource ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  NerveRowSource category
denseOrdinalNerveRowSource arrangement =
  simplexFaceRowSource (denseArrangementCategory arrangement) (denseNerveSimplicesAt arrangement)

denseOrdinalSkeletonRowPlan ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Set Natural ->
  Set Natural ->
  SkeletonRowPlan category
denseOrdinalSkeletonRowPlan arrangement cellDimensions faceSourceDimensions =
  denseOrdinalSkeletonRowPlanWithDepth
    arrangement
    (rowPlanDepth cellDimensions faceSourceDimensions)
    cellDimensions
    faceSourceDimensions

denseOrdinalSkeletonRowPlanWithDepth ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Natural ->
  Set Natural ->
  Set Natural ->
  SkeletonRowPlan category
denseOrdinalSkeletonRowPlanWithDepth arrangement depthValue cellDimensions faceSourceDimensions =
  mkSkeletonRowPlan
    depthValue
    cellDimensions
    faceSourceDimensions
    (cachedDenseNerveRowSource (denseArrangementCategory arrangement) (denseNerveRowsUpTo arrangement depthValue))

cachedDenseNerveRowSource ::
  (Category category, Eq (Ob category)) =>
  category ->
  Map.Map Natural [NerveSimplex category] ->
  NerveRowSource category
cachedDenseNerveRowSource categoryValue rowsByDimension =
  simplexFaceRowSource categoryValue (\dimensionValue -> Map.findWithDefault [] dimensionValue rowsByDimension)

denseNerveRowsUpTo ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Natural ->
  Map.Map Natural [NerveSimplex category]
denseNerveRowsUpTo arrangement depthValue =
  case naturalToBoundedInt depthValue of
    Nothing ->
      Map.empty
    Just depthInt ->
      Map.fromAscList
        [ (fromIntegral dimensionInt, denseRowsToSimplices arrangement dimensionInt rowValues)
        | (dimensionInt, rowValues) <-
            zip [0 .. depthInt] (denseChainRowsUpTo arrangement depthInt)
        ]

denseNerveSimplicesAt ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Natural ->
  [NerveSimplex category]
denseNerveSimplicesAt arrangement dimensionValue =
  maybe
    []
    ( \dimensionInt ->
        denseRowsToSimplices
          arrangement
          dimensionInt
          (denseChainRowsAt arrangement dimensionInt)
    )
    (naturalToBoundedInt dimensionValue)

denseRowsToSimplices ::
  DenseNerveArrangement category ->
  Int ->
  [DenseNerveChainRow category] ->
  [NerveSimplex category]
denseRowsToSimplices _ _dimensionInt =
  fmap (nerveSimplexFromChain . denseChainValue)

denseChainRowsUpTo ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Int ->
  [[DenseNerveChainRow category]]
denseChainRowsUpTo arrangement depthInt =
  take (depthInt + 1) (iterate (extendDenseChainRows arrangement) (denseSeedRows arrangement))

denseChainRowsAt ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  Int ->
  [DenseNerveChainRow category]
denseChainRowsAt arrangement dimensionInt =
  fromMaybe [] (indexByInt dimensionInt (denseChainRowsUpTo arrangement dimensionInt))

indexByInt :: Int -> [value] -> Maybe value
indexByInt indexValue values
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue values of
        value : _ -> Just value
        [] -> Nothing

denseSeedRows ::
  DenseNerveArrangement category ->
  [DenseNerveChainRow category]
denseSeedRows arrangement =
  Vector.toList
    ( Vector.imap
        ( \objectOrdinal objectValue ->
            DenseNerveChainRow
              { denseChainValue = singletonComposableChain objectValue,
                denseChainTerminalOrdinal = objectOrdinal
              }
        )
        (denseArrangementObjects arrangement)
    )

extendDenseChainRows ::
  (Category category, Eq (Ob category)) =>
  DenseNerveArrangement category ->
  [DenseNerveChainRow category] ->
  [DenseNerveChainRow category]
extendDenseChainRows arrangement =
  foldMap
    ( \rowValue ->
        foldMap
          (extendDenseChainRow (denseArrangementCategory arrangement) rowValue)
          (IntMap.findWithDefault [] (denseChainTerminalOrdinal rowValue) (denseArrangementMorphismsBySource arrangement))
    )

extendDenseChainRow ::
  (Category category, Eq (Ob category)) =>
  category ->
  DenseNerveChainRow category ->
  DenseNerveMorphismOption category ->
  [DenseNerveChainRow category]
extendDenseChainRow categoryValue rowValue morphismOption =
  either
    (const [])
    ( \chainValue ->
        [ DenseNerveChainRow
            { denseChainValue = chainValue,
              denseChainTerminalOrdinal = denseMorphismTargetOrdinal morphismOption
            }
        ]
    )
    (appendComposableMorphism categoryValue (denseChainValue rowValue) (denseMorphismValue morphismOption))

simplicialNerveRowSource ::
  TruncatedNormalizedSSet (NerveSimplex category) ->
  NerveRowSource category
simplicialNerveRowSource nerveValue =
  NerveRowSource
    { nerveRowsAtDimension = simplicesAtDimension nerveValue,
      nerveFaceAt = applyFaceAtDimension nerveValue,
      nerveDegeneracyAt = applyDegeneracyAtDimension nerveValue
    }

rowSourceToTruncatedNerve ::
  (Eq (Ob category), Eq (Mor category)) =>
  SkeletonRowPlan category ->
  Either (NonEmpty (TruncatedSSetObstruction (NerveSimplex category))) (TruncatedNormalizedSSet (NerveSimplex category))
rowSourceToTruncatedNerve plan =
  mkTruncatedSSet
    (skeletonRowPlanDepth plan)
    [ (dimensionValue, rowValues)
    | dimensionValue <- [0 .. skeletonRowPlanDepth plan],
      let rowValues = nerveRowsAtDimension (skeletonRowPlanSource plan) dimensionValue,
      not (null rowValues)
    ]
    ( \_ faceIndex simplexValue ->
        nerveFaceAt
          (skeletonRowPlanSource plan)
          (nerveSimplexDimension simplexValue)
          (finValue faceIndex)
          simplexValue
    )
    ( \_ degeneracyIndex simplexValue ->
        nerveDegeneracyAt
          (skeletonRowPlanSource plan)
          (nerveSimplexDimension simplexValue)
          (finValue degeneracyIndex)
          simplexValue
    )
    (\_ _ -> False)

wcojNerveRowSource ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  NerveRowSource category
wcojNerveRowSource categoryValue =
  simplexFaceRowSource categoryValue (wcojNerveSimplicesAtWith categoryValue (prepareWCOJMorphismDomainIndex categoryValue))

wcojNerveSimplicesAt ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  Natural ->
  [NerveSimplex category]
wcojNerveSimplicesAt categoryValue dimensionValue =
  wcojNerveSimplicesAtWith categoryValue (prepareWCOJMorphismDomainIndex categoryValue) dimensionValue

wcojNerveSimplicesAtWith ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  WCOJMorphismDomainIndex category ->
  Natural ->
  [NerveSimplex category]
wcojNerveSimplicesAtWith categoryValue domainIndex dimensionValue =
  fmap
    nerveSimplexFromChain
    (wcojComposableChainsAtWith categoryValue domainIndex dimensionValue)

wcojComposableChainsAt ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  Natural ->
  [ComposableChain category]
wcojComposableChainsAt categoryValue dimensionValue =
  wcojComposableChainsAtWith categoryValue (prepareWCOJMorphismDomainIndex categoryValue) dimensionValue

wcojComposableChainsAtWith ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  WCOJMorphismDomainIndex category ->
  Natural ->
  [ComposableChain category]
wcojComposableChainsAtWith categoryValue domainIndex dimensionValue =
  case naturalToBoundedInt dimensionValue of
    Nothing ->
      []
    Just dimensionInt ->
      let context = WCOJChainContext categoryValue dimensionInt domainIndex
       in canonicalChains
            ( mapMaybe
                (chainJoinEnvToChain context)
                ( adaptiveJoin
                    chainJoinAlgebra
                    context
                    (chainJoinSlots dimensionInt)
                    IntMap.empty
                )
            )

data ChainJoinValue category
  = ChainJoinObject (Ob category)
  | ChainJoinMorphism (Mor category)

data WCOJChainContext category = WCOJChainContext
  { wcCategory :: category,
    wcDimension :: Int,
    wcDomainIndex :: WCOJMorphismDomainIndex category
  }

data WCOJMorphismDomainIndex category = WCOJMorphismDomainIndex
  { wmdiObjects :: ![Ob category],
    wmdiMorphismValues :: ![Mor category],
    wmdiMorphismsBySource :: !(Map.Map (Ob category) [Mor category]),
    wmdiMorphismsByTarget :: !(Map.Map (Ob category) [Mor category]),
    wmdiMorphismsByEndpoint :: !(Map.Map (Ob category, Ob category) [Mor category])
  }

data WCOJMorphismFact category = WCOJMorphismFact
  { wmfMorphism :: !(Mor category),
    wmfSource :: !(Ob category),
    wmfTarget :: !(Ob category)
  }

prepareWCOJMorphismDomainIndex ::
  ( FiniteComposableCategory category,
    Ord (Ob category),
    Ord (Mor category)
  ) =>
  category ->
  WCOJMorphismDomainIndex category
prepareWCOJMorphismDomainIndex categoryValue =
  let morphismFacts = wcojMorphismFacts categoryValue
   in WCOJMorphismDomainIndex
        { wmdiObjects = enumerateObjects categoryValue,
          wmdiMorphismValues = fmap wmfMorphism morphismFacts,
          wmdiMorphismsBySource = bucketWCOJMorphismsBy wmfSource morphismFacts,
          wmdiMorphismsByTarget = bucketWCOJMorphismsBy wmfTarget morphismFacts,
          wmdiMorphismsByEndpoint = bucketWCOJMorphismsByEndpoint morphismFacts
        }

wcojMorphismFacts ::
  (FiniteComposableCategory category, Eq (Mor category)) =>
  category ->
  [WCOJMorphismFact category]
wcojMorphismFacts categoryValue =
  mapMaybe (wcojMorphismFact categoryValue) (enumerateMorphisms categoryValue)

wcojMorphismFact ::
  (Category category, Eq (Mor category)) =>
  category ->
  Mor category ->
  Maybe (WCOJMorphismFact category)
wcojMorphismFact categoryValue morphismValue
  | nondegenerateMorphism categoryValue morphismValue =
      case (source categoryValue morphismValue, target categoryValue morphismValue) of
        (Right sourceObject, Right targetObject) ->
          Just
            WCOJMorphismFact
              { wmfMorphism = morphismValue,
                wmfSource = sourceObject,
                wmfTarget = targetObject
              }
        _ ->
          Nothing
  | otherwise =
      Nothing

bucketWCOJMorphismsBy ::
  Ord key =>
  (WCOJMorphismFact category -> key) ->
  [WCOJMorphismFact category] ->
  Map.Map key [Mor category]
bucketWCOJMorphismsBy keyOf =
  foldr
    ( \morphismFact ->
        Map.insertWith (<>) (keyOf morphismFact) [wmfMorphism morphismFact]
    )
    Map.empty

bucketWCOJMorphismsByEndpoint ::
  Ord (Ob category) =>
  [WCOJMorphismFact category] ->
  Map.Map (Ob category, Ob category) [Mor category]
bucketWCOJMorphismsByEndpoint =
  bucketWCOJMorphismsBy (\morphismFact -> (wmfSource morphismFact, wmfTarget morphismFact))

chainJoinAlgebra ::
  ( FiniteComposableCategory category,
    Ord (Ob category)
  ) =>
  JoinAlgebra (WCOJChainContext category) (ChainJoinValue category)
chainJoinAlgebra =
  JoinAlgebra
    { joinCount = \context env slot -> domainSize (chainJoinDomain context env slot),
      joinPropose = chainJoinDomain,
      joinValidate = chainJoinWitness
    }

chainJoinDomain ::
  ( Category category,
    Ord (Ob category)
  ) =>
  WCOJChainContext category ->
  Env (ChainJoinValue category) ->
  Slot ->
  Domain (ChainJoinValue category)
chainJoinDomain context env slot
  | even slot =
      vertexDomain context env (slotVertexIndex slot)
  | otherwise =
      morphismDomain context env (slotMorphismIndex slot)

vertexDomain ::
  (Category category, Eq (Ob category)) =>
  WCOJChainContext category ->
  Env (ChainJoinValue category) ->
  Int ->
  Domain (ChainJoinValue category)
vertexDomain context env vertexIndex =
  case (morphismAt vertexIndex env, morphismAt (vertexIndex + 1) env) of
    (Just leftMorphism, Just rightMorphism)
      | Right targetObject <- target (wcCategory context) leftMorphism,
        Right sourceObject <- source (wcCategory context) rightMorphism,
        targetObject == sourceObject ->
          domainSingleton (ChainJoinObject targetObject)
      | otherwise ->
          domainEmpty
    (Just leftMorphism, Nothing) ->
      either (const domainEmpty) (domainSingleton . ChainJoinObject) (target (wcCategory context) leftMorphism)
    (Nothing, Just rightMorphism) ->
      either (const domainEmpty) (domainSingleton . ChainJoinObject) (source (wcCategory context) rightMorphism)
    (Nothing, Nothing) ->
      domainFromListPreservingOrder
        (fmap ChainJoinObject (wmdiObjects (wcDomainIndex context)))

morphismDomain ::
  Ord (Ob category) =>
  WCOJChainContext category ->
  Env (ChainJoinValue category) ->
  Int ->
  Domain (ChainJoinValue category)
morphismDomain context env morphismIndex
  | morphismIndex <= 0 || morphismIndex > wcDimension context =
      domainEmpty
  | otherwise =
      domainFromListPreservingOrder
        (fmap ChainJoinMorphism (indexedMorphismDomain context (objectAt (morphismIndex - 1) env) (objectAt morphismIndex env)))

indexedMorphismDomain ::
  Ord (Ob category) =>
  WCOJChainContext category ->
  Maybe (Ob category) ->
  Maybe (Ob category) ->
  [Mor category]
indexedMorphismDomain context maybeSourceObject maybeTargetObject =
  case (maybeSourceObject, maybeTargetObject) of
    (Just sourceObject, Just targetObject) ->
      Map.findWithDefault [] (sourceObject, targetObject) (wmdiMorphismsByEndpoint domainIndex)
    (Just sourceObject, Nothing) ->
      Map.findWithDefault [] sourceObject (wmdiMorphismsBySource domainIndex)
    (Nothing, Just targetObject) ->
      Map.findWithDefault [] targetObject (wmdiMorphismsByTarget domainIndex)
    (Nothing, Nothing) ->
      wmdiMorphismValues domainIndex
  where
    domainIndex =
      wcDomainIndex context

nondegenerateMorphism ::
  (Category category, Eq (Mor category)) =>
  category ->
  Mor category ->
  Bool
nondegenerateMorphism categoryValue morphismValue =
  case source categoryValue morphismValue >>= identity categoryValue of
    Right identityMorphism -> morphismValue /= identityMorphism
    Left _ -> False

chainJoinWitness ::
  FiniteComposableCategory category =>
  WCOJChainContext category ->
  Env (ChainJoinValue category) ->
  Bool
chainJoinWitness context =
  isJust . chainJoinEnvToChain context

chainJoinEnvToChain ::
  FiniteComposableCategory category =>
  WCOJChainContext category ->
  Env (ChainJoinValue category) ->
  Maybe (ComposableChain category)
chainJoinEnvToChain context env = do
  startObject <- objectAt 0 env
  morphismValues <- traverse (`morphismAt` env) [1 .. wcDimension context]
  chainValue <- either (const Nothing) Just (mkComposableChain (wcCategory context) startObject morphismValues)
  let expectedVertices = fmap (`objectAt` env) [0 .. wcDimension context]
  actualVertices <- either (const Nothing) (Just . fmap Just) (nerveChainVertices (wcCategory context) chainValue)
  guard (expectedVertices == actualVertices)
  pure chainValue

objectAt :: Int -> Env (ChainJoinValue category) -> Maybe (Ob category)
objectAt indexValue env =
  case IntMap.lookup (vertexSlot indexValue) env of
    Just (ChainJoinObject objectValue) -> Just objectValue
    _ -> Nothing

morphismAt :: Int -> Env (ChainJoinValue category) -> Maybe (Mor category)
morphismAt indexValue env =
  if indexValue <= 0
    then Nothing
    else
      case IntMap.lookup (morphismSlot indexValue) env of
        Just (ChainJoinMorphism morphismValue) -> Just morphismValue
        _ -> Nothing

chainJoinSlots :: Int -> [Slot]
chainJoinSlots dimensionInt =
  fmap vertexSlot [0 .. dimensionInt]
    <> fmap morphismSlot [1 .. dimensionInt]

vertexSlot :: Int -> Slot
vertexSlot indexValue =
  indexValue * 2

morphismSlot :: Int -> Slot
morphismSlot indexValue =
  indexValue * 2 - 1

slotVertexIndex :: Slot -> Int
slotVertexIndex slot =
  slot `div` 2

slotMorphismIndex :: Slot -> Int
slotMorphismIndex slot =
  (slot + 1) `div` 2

canonicalChains ::
  (Ord (Ob category), Ord (Mor category)) =>
  [ComposableChain category] ->
  [ComposableChain category]
canonicalChains =
  Map.elems
    . Map.fromList
    . fmap (\chainValue -> (chainKey chainValue, chainValue))

chainKey ::
  ComposableChain category ->
  (Ob category, [Mor category])
chainKey chainValue =
  (chainStartObject chainValue, chainMorphisms chainValue)

naturalToBoundedInt :: Natural -> Maybe Int
naturalToBoundedInt value
  | value <= fromIntegral (maxBound :: Int) = Just (fromIntegral value)
  | otherwise = Nothing
