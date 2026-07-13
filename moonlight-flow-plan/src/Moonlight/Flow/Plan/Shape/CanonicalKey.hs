{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Plan.Shape.CanonicalKey
  ( PlanCanonicalizationError (..),

    PlanCanonicalizationCacheKey (..),
    PlanCanonicalizationMemo (..),
    emptyPlanCanonicalizationMemo,
    canonicalizationResultFromPlanShapeMemoized,

    canonicalizationResultFromPlanShape,
    canonicalizationResultFromQueryPlan,
    canonicalizationResultFromQueryPlanOutputErased,
    extractCanonicalPlanShape,
    extractCanonicalPlanKey,
    planShapeInputDigest,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( safeIndex,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlan,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonAtom (..),
    CanonicalizationResult (..),
    LogicalQueryShape (..),
    insertCanonAtom,
  )
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.Encode qualified as ShapeEncode
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    CanonSlotSource,
    CanonStalkRecipe (..),
    LogicalPlanTerm (..),
    PlanShape (..),
    PlanStage (..),
    RawAtomTerm (..),
    RawSlot (..),
    rawSlotKey,
  )

data PlanCanonicalizationError
  = PlanCanonicalizationNoCandidate
  | PlanCanonicalizationMissingSlot !RawSlot
  | PlanCanonicalizationDigestMismatch !StableDigest128 !StableDigest128
  deriving stock (Eq, Ord, Show, Read)

newtype PlanCanonicalizationCacheKey = PlanCanonicalizationCacheKey
  { pcckShapeDigest :: StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

data PlanCanonicalizationMemo = PlanCanonicalizationMemo
  { pcmResults :: !(Map PlanCanonicalizationCacheKey CanonicalizationResult)
  }
  deriving stock (Eq, Show, Read)

emptyPlanCanonicalizationMemo :: PlanCanonicalizationMemo
emptyPlanCanonicalizationMemo =
  PlanCanonicalizationMemo
    { pcmResults = Map.empty
    }
{-# INLINE emptyPlanCanonicalizationMemo #-}

data SlotInitialSignature = SlotInitialSignature
  { sisRoot :: !Bool,
    sisOutputPositions :: ![Int],
    sisRawIncidence :: ![(Word64, [(Int, Maybe [CanonSlotSource])])]
  }
  deriving stock (Eq, Ord, Show, Read)

data SlotRefinementSignature = SlotRefinementSignature
  { srsCurrentColor :: {-# UNPACK #-} !Int,
    srsRoot :: !Bool,
    srsOutputPositions :: ![Int],
    srsNeighborhood :: ![SlotAtomNeighborhood]
  }
  deriving stock (Eq, Ord, Show, Read)

data SlotAtomNeighborhood = SlotAtomNeighborhood
  { sanTagDigest :: {-# UNPACK #-} !Word64,
    sanColumnColors :: ![Int],
    sanOccurrenceColumns :: ![Int],
    sanRecipe :: !CanonStalkRecipe
  }
  deriving stock (Eq, Ord, Show, Read)

data CanonicalCandidate = CanonicalCandidate
  { ccShape :: !LogicalQueryShape,
    ccSlotMap :: !(IntMap CanonSlot),
    ccAtomMap :: !(IntMap CanonAtom)
  }
  deriving stock (Eq, Ord, Show, Read)

data SearchAccumulator = SearchAccumulator
  { saSeen :: !(Set [(Int, Int)]),
    saBest :: !(Maybe CanonicalCandidate)
  }

canonicalizationResultFromPlanShape ::
  PlanShape 'RawLogical ->
  Either PlanCanonicalizationError CanonicalizationResult
canonicalizationResultFromPlanShape shape =
  snd <$> canonicalizationResultFromPlanShapeMemoized emptyPlanCanonicalizationMemo shape
{-# INLINE canonicalizationResultFromPlanShape #-}

canonicalizationResultFromQueryPlan ::
  QueryPlan compiled output guard tag tuple key ->
  Either PlanCanonicalizationError CanonicalizationResult
canonicalizationResultFromQueryPlan =
  canonicalizationResultFromPlanShape . ShapeBuild.queryPlanToPlanShape
{-# INLINE canonicalizationResultFromQueryPlan #-}

canonicalizationResultFromQueryPlanOutputErased ::
  QueryPlan compiled output guard tag tuple key ->
  Either PlanCanonicalizationError CanonicalizationResult
canonicalizationResultFromQueryPlanOutputErased =
  canonicalizationResultFromPlanShape . ShapeBuild.queryPlanToOutputErasedPlanShape
{-# INLINE canonicalizationResultFromQueryPlanOutputErased #-}

canonicalizationResultFromPlanShapeMemoized ::
  PlanCanonicalizationMemo ->
  PlanShape 'RawLogical ->
  Either PlanCanonicalizationError (PlanCanonicalizationMemo, CanonicalizationResult)
canonicalizationResultFromPlanShapeMemoized memo shape =
  canonicalizationResultFromLogicalPayloadMemoized memo (psDigest shape) (psPayload shape)
{-# INLINE canonicalizationResultFromPlanShapeMemoized #-}

canonicalizationResultFromLogicalPayloadMemoized ::
  PlanCanonicalizationMemo ->
  StableDigest128 ->
  LogicalPlanTerm ->
  Either PlanCanonicalizationError (PlanCanonicalizationMemo, CanonicalizationResult)
canonicalizationResultFromLogicalPayloadMemoized memo shapeDigest logical =
  case Map.lookup cacheKey (pcmResults memo) of
    Just result ->
      Right (memo, result)
    Nothing -> do
      candidate <- bestCanonicalCandidate logical
      let shapeValue =
            ccShape candidate
          planShape =
            ShapeBuild.mkPlanShape ShapeEncode.logicalQueryShapeWords shapeValue
          result =
            CanonicalizationResult
              { crPlan = planShape,
                crSlotMap = ccSlotMap candidate,
                crAtomShapes = ccAtomMap candidate,
                crResidual = lqsResidual shapeValue
              }
          memo' =
            memo
              { pcmResults =
                  Map.insert cacheKey result (pcmResults memo)
              }
      pure (memo', result)
  where
    cacheKey =
      PlanCanonicalizationCacheKey shapeDigest
{-# INLINE canonicalizationResultFromLogicalPayloadMemoized #-}

extractCanonicalPlanShape ::
  PlanShape 'RawLogical ->
  Either PlanCanonicalizationError (PlanShape 'Canonical)
extractCanonicalPlanShape =
  fmap crPlan . canonicalizationResultFromPlanShape
{-# INLINE extractCanonicalPlanShape #-}

extractCanonicalPlanKey ::
  PlanShape 'RawLogical ->
  Either PlanCanonicalizationError StableDigest128
extractCanonicalPlanKey =
  fmap psDigest . extractCanonicalPlanShape
{-# INLINE extractCanonicalPlanKey #-}

planShapeInputDigest :: PlanShape stage -> StableDigest128
planShapeInputDigest =
  psDigest
{-# INLINE planShapeInputDigest #-}

bestCanonicalCandidate ::
  LogicalPlanTerm ->
  Either PlanCanonicalizationError CanonicalCandidate
bestCanonicalCandidate logical =
  case saBest (searchCanonical logical initialColors emptySearchAccumulator) of
    Nothing ->
      Left PlanCanonicalizationNoCandidate
    Just candidate ->
      Right candidate
  where
    initialColors =
      refineSlotColors logical (initialSlotColors logical)

emptySearchAccumulator :: SearchAccumulator
emptySearchAccumulator =
  SearchAccumulator
    { saSeen = Set.empty,
      saBest = Nothing
    }
{-# INLINE emptySearchAccumulator #-}

searchCanonical ::
  LogicalPlanTerm ->
  IntMap Int ->
  SearchAccumulator ->
  SearchAccumulator
searchCanonical logical colors0 acc0 =
  let !colors =
        refineSlotColors logical colors0
      !memoKey =
        IntMap.toAscList colors
   in if Set.member memoKey (saSeen acc0)
        then acc0
        else
          let !acc1 =
                acc0 {saSeen = Set.insert memoKey (saSeen acc0)}
           in case chooseBranchClass colors of
                Nothing ->
                  insertLeafCandidate logical colors acc1
                Just branchClass ->
                  List.foldl'
                    ( \acc slot ->
                        searchCanonical logical (individualizeSlot slot colors) acc
                    )
                    acc1
                    branchClass
{-# INLINE searchCanonical #-}

insertLeafCandidate ::
  LogicalPlanTerm ->
  IntMap Int ->
  SearchAccumulator ->
  SearchAccumulator
insertLeafCandidate logical colors acc =
  case canonicalCandidateWithSlotMap logical (slotMapFromDiscreteColors colors) of
    Left _err ->
      acc
    Right candidate ->
      acc
        { saBest =
            case saBest acc of
              Nothing ->
                Just candidate
              Just best
                | ccShape candidate < ccShape best ->
                    Just candidate
                | otherwise ->
                    Just best
        }
{-# INLINE insertLeafCandidate #-}

rawSlotsOfLogicalPlan ::
  LogicalPlanTerm ->
  [RawSlot]
rawSlotsOfLogicalPlan logical =
  List.sort . IntMap.elems $
    List.foldl'
      insertSlot
      IntMap.empty
      allSlots
  where
    allSlots =
      lptRoot logical
        : lptOutputs logical
          <> foldMap ratColumns (lptAtoms logical)

    insertSlot acc slot =
      IntMap.insert (rawSlotKey slot) slot acc
{-# INLINE rawSlotsOfLogicalPlan #-}

initialSlotColors ::
  LogicalPlanTerm ->
  IntMap Int
initialSlotColors logical =
  recolorBy
    (initialSlotSignature logical)
    (rawSlotsOfLogicalPlan logical)
{-# INLINE initialSlotColors #-}

initialSlotSignature ::
  LogicalPlanTerm ->
  RawSlot ->
  SlotInitialSignature
initialSlotSignature logical slot =
  SlotInitialSignature
    { sisRoot = slot == lptRoot logical,
      sisOutputPositions =
        [ outputIndex
        | (outputIndex, outputSlot) <- zip [0 :: Int ..] (lptOutputs logical),
          outputSlot == slot
        ],
      sisRawIncidence =
        List.sort
          [ (ratTagDigest atomValue, atomSlotIncidence slot atomValue)
          | atomValue <- lptAtoms logical,
            slot `elem` ratColumns atomValue
          ]
    }
{-# INLINE initialSlotSignature #-}

atomSlotIncidence ::
  RawSlot ->
  RawAtomTerm ->
  [(Int, Maybe [CanonSlotSource])]
atomSlotIncidence slot atomValue =
  [ (columnIndex, safeIndex columnIndex recipeColumns)
  | (columnIndex, columnSlot) <- zip [0 :: Int ..] (ratColumns atomValue),
    columnSlot == slot
  ]
  where
    CanonStalkRecipe recipeColumns =
      ratRecipe atomValue
{-# INLINE atomSlotIncidence #-}

refineSlotColors ::
  LogicalPlanTerm ->
  IntMap Int ->
  IntMap Int
refineSlotColors logical colors0 =
  let !colors1 =
        recolorBy
          (slotRefinementSignature logical colors0)
          (rawSlotsOfLogicalPlan logical)
   in if colors1 == colors0
        then colors1
        else refineSlotColors logical colors1
{-# INLINE refineSlotColors #-}

slotRefinementSignature ::
  LogicalPlanTerm ->
  IntMap Int ->
  RawSlot ->
  SlotRefinementSignature
slotRefinementSignature logical colors slot =
  SlotRefinementSignature
    { srsCurrentColor =
        IntMap.findWithDefault maxBound (rawSlotKey slot) colors,
      srsRoot =
        slot == lptRoot logical,
      srsOutputPositions =
        [ outputIndex
        | (outputIndex, outputSlot) <- zip [0 :: Int ..] (lptOutputs logical),
          outputSlot == slot
        ],
      srsNeighborhood =
        List.sort
          [ atomNeighborhood colors slot atomValue
          | atomValue <- lptAtoms logical,
            slot `elem` ratColumns atomValue
          ]
    }
{-# INLINE slotRefinementSignature #-}

atomNeighborhood ::
  IntMap Int ->
  RawSlot ->
  RawAtomTerm ->
  SlotAtomNeighborhood
atomNeighborhood colors slot atomValue =
  SlotAtomNeighborhood
    { sanTagDigest = ratTagDigest atomValue,
      sanColumnColors =
        fmap
          (\rawSlot -> IntMap.findWithDefault maxBound (rawSlotKey rawSlot) colors)
          (ratColumns atomValue),
      sanOccurrenceColumns =
        [ columnIndex
        | (columnIndex, columnSlot) <- zip [0 :: Int ..] (ratColumns atomValue),
          columnSlot == slot
        ],
      sanRecipe = ratRecipe atomValue
    }
{-# INLINE atomNeighborhood #-}

recolorBy ::
  Ord signature =>
  (RawSlot -> signature) ->
  [RawSlot] ->
  IntMap Int
recolorBy signatureOf slots =
  snd $
    List.foldl'
      assignGroup
      (0 :: Int, IntMap.empty)
      grouped
  where
    grouped =
      Map.elems $
        Map.fromListWith
          (<>)
          [ (signatureOf slot, [slot])
          | slot <- slots
          ]

    assignGroup ::
      (Int, IntMap Int) ->
      [RawSlot] ->
      (Int, IntMap Int)
    assignGroup (!nextColor, !acc) rawGroup =
      let sortedGroup =
            List.sort rawGroup
          acc' =
            List.foldl'
              (\m rawSlot -> IntMap.insert (rawSlotKey rawSlot) nextColor m)
              acc
              sortedGroup
       in (nextColor + 1, acc')
{-# INLINE recolorBy #-}

chooseBranchClass ::
  IntMap Int ->
  Maybe [RawSlot]
chooseBranchClass colors =
  List.foldl' choose Nothing nonSingletonClasses
  where
    classes =
      colorClasses colors

    nonSingletonClasses =
      filter hasMultiple classes

    hasMultiple :: [value] -> Bool
    hasMultiple values =
      case values of
        _first : _second : _rest ->
          True
        _ ->
          False

    choose :: Maybe [RawSlot] -> [RawSlot] -> Maybe [RawSlot]
    choose Nothing values =
      Just values
    choose (Just best) values =
      case compare (length values, values) (length best, best) of
        LT ->
          Just values
        _ ->
          Just best
{-# INLINE chooseBranchClass #-}

individualizeSlot ::
  RawSlot ->
  IntMap Int ->
  IntMap Int
individualizeSlot selected colors =
  case IntMap.lookup (rawSlotKey selected) colors of
    Nothing ->
      colors
    Just selectedColor ->
      snd $
        List.foldl'
          rebuildClass
          (0 :: Int, IntMap.empty)
          (colorClassesWithColor colors)
      where
        selectedKey =
          rawSlotKey selected

        rebuildClass (!nextColor, !acc) (!classColor, !rawGroup)
          | classColor /= selectedColor =
              (nextColor + 1, writeColor nextColor rawGroup acc)
          | otherwise =
              let withoutSelected =
                    filter ((/= selectedKey) . rawSlotKey) rawGroup
                  accWithSelected =
                    IntMap.insert selectedKey nextColor acc
               in case withoutSelected of
                    [] ->
                      (nextColor + 1, accWithSelected)
                    _ ->
                      (nextColor + 2, writeColor (nextColor + 1) withoutSelected accWithSelected)
{-# INLINE individualizeSlot #-}

slotMapFromDiscreteColors ::
  IntMap Int ->
  IntMap CanonSlot
slotMapFromDiscreteColors colors =
  IntMap.fromList
    [ (rawSlotKeyValue, CanonSlot canonKey)
    | (canonKey, (rawSlotKeyValue, _color)) <-
        zip [0 :: Int ..] $
          List.sortOn
            (\(rawSlotKeyValue, color) -> (color, rawSlotKeyValue))
            (IntMap.toAscList colors)
    ]
{-# INLINE slotMapFromDiscreteColors #-}

colorClasses ::
  IntMap Int ->
  [[RawSlot]]
colorClasses =
  fmap snd . colorClassesWithColor
{-# INLINE colorClasses #-}

colorClassesWithColor ::
  IntMap Int ->
  [(Int, [RawSlot])]
colorClassesWithColor colors =
  [ (color, List.sort rawSlots)
  | (color, rawSlots) <-
      Map.toAscList $
        IntMap.foldlWithKey'
          ( \acc rawSlotKeyValue color ->
              Map.insertWith
                (<>)
                color
                [RawSlot rawSlotKeyValue]
                acc
          )
          Map.empty
          colors
  ]
{-# INLINE colorClassesWithColor #-}

writeColor ::
  Int ->
  [RawSlot] ->
  IntMap Int ->
  IntMap Int
writeColor colorValue rawSlots acc0 =
  List.foldl'
    (\acc rawSlot -> IntMap.insert (rawSlotKey rawSlot) colorValue acc)
    acc0
    rawSlots
{-# INLINE writeColor #-}

canonicalCandidateWithSlotMap ::
  LogicalPlanTerm ->
  IntMap CanonSlot ->
  Either PlanCanonicalizationError CanonicalCandidate
canonicalCandidateWithSlotMap logical slotMap = do
  rootSlot <- lookupCanonSlot slotMap (lptRoot logical)
  outputSlots <- traverse (lookupCanonSlot slotMap) (lptOutputs logical)
  atomPairs <- traverse (canonicalAtomPairWithSlotMap slotMap) (lptAtoms logical)
  let atomMultiset =
        List.foldl'
          (\acc (_rawKey, atomValue) -> insertCanonAtom atomValue acc)
          Map.empty
          atomPairs
      atomMap =
        IntMap.fromList atomPairs
      shapeValue =
        LogicalQueryShape
          { lqsDomain = lptDomain logical,
            lqsAtoms = atomMultiset,
            lqsRoot = rootSlot,
            lqsOutputs = outputSlots,
            lqsResidual = lptResidual logical
          }
  pure
    CanonicalCandidate
      { ccShape = shapeValue,
        ccSlotMap = slotMap,
        ccAtomMap = atomMap
      }
{-# INLINE canonicalCandidateWithSlotMap #-}

canonicalAtomPairWithSlotMap ::
  IntMap CanonSlot ->
  RawAtomTerm ->
  Either PlanCanonicalizationError (Int, CanonAtom)
canonicalAtomPairWithSlotMap slotMap atomValue = do
  columns <- traverse (lookupCanonSlot slotMap) (ratColumns atomValue)
  pure
    ( ratRawAtomKey atomValue,
      CanonAtom
        { caTagDigest = ratTagDigest atomValue,
          caColumns = columns,
          caRecipe = ratRecipe atomValue
        }
    )
{-# INLINE canonicalAtomPairWithSlotMap #-}

lookupCanonSlot ::
  IntMap CanonSlot ->
  RawSlot ->
  Either PlanCanonicalizationError CanonSlot
lookupCanonSlot slotMap rawSlot =
  maybe
    (Left (PlanCanonicalizationMissingSlot rawSlot))
    Right
    (IntMap.lookup (rawSlotKey rawSlot) slotMap)
{-# INLINE lookupCanonSlot #-}
