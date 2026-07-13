{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Differential.Index.RowSet
  ( RowSet,
    RowSetRestriction (..),
    RowSetError (..),
    rowSetSmallLimit,
    rowSetDenseMinUniverse,
    emptyRowSet,
    singletonRowSet,
    rowSetFromList,
    rowSetFromIntSetCanonical,
    rowSetFromIntSetWithUniverse,
    rowSetFullRange,
    rowSetToList,
    rowSetToIntSet,
    rowSetDigest,
    rowSetNull,
    rowSetSize,
    rowSetMember,
    rowSetInsert,
    rowSetDelete,
    rowSetUnion,
    rowSetIntersection,
    rowSetDifference,
    rowSetIntersects,
    rowSetIntersectionWithRowIdSet,
    rowSetIntersectionWithRowIdSetChanged,
    rowSetIntersectsRowIdSet,
    rowSetFoldl',
    validateRowSet,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bits
  ( Bits (..),
    FiniteBits (..),
    popCount,
  )
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Word (Word64)
import Moonlight.Differential.Internal.Index.RowId
  ( RowId (..),
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetAny,
    rowIdSetFoldl',
    rowIdSetIntersectionWithIntSet,
    rowIdSetIntersectsIntSet,
    rowIdSetMember,
    rowIdSetNull,
    rowIdSetSize,
    rowIdSetSmallLimit,
  )
import Moonlight.Differential.Index.SmallIntArray
  ( smallIntArrayFoldl',
    smallIntArrayFromAscList,
    smallIntArrayMember,
    smallIntArrayToAscList,
    validateSmallIntArrayAscending,
  )

rowSetSmallLimit :: Int
rowSetSmallLimit = 32

rowSetDenseMinUniverse :: Int
rowSetDenseMinUniverse = 1024

denseDensityDenominator :: Int
denseDensityDenominator = 8

wordBits :: Int
wordBits =
  finiteBitSize (0 :: Word)

type RowSet :: Type
data RowSet
  = RowSetEmpty
  | RowSetSingleton {-# UNPACK #-} !Int
  | RowSetSmall !(PrimArray Int)
  | RowSetDense
      {-# UNPACK #-} !Int
      !(PrimArray Word)
  | RowSetSparse !IntSet
  deriving stock (Eq, Ord, Show)

type RowSetRestriction :: Type
data RowSetRestriction
  = RowSetRestrictionEmpty
  | RowSetRestrictionUnchanged
  | RowSetRestrictionChanged !RowSet
  deriving stock (Eq, Ord, Show)

type RowSetError :: Type
data RowSetError
  = RowSetSingletonNegative !Int
  | RowSetSmallTooLarge !Int
  | RowSetNegativeMember !Int
  | RowSetSmallNotStrictlyAscending !Int !Int !Int
  | RowSetDenseNegativeUniverse !Int
  | RowSetDenseWrongWordCount !Int !Int
  | RowSetSparseTooSmall !Int
  deriving stock (Eq, Ord, Show)

emptyRowSet :: RowSet
emptyRowSet =
  RowSetEmpty
{-# INLINE emptyRowSet #-}

singletonRowSet :: RowId -> RowSet
singletonRowSet rowId
  = singletonRowSetInt (rowIdInt rowId)
{-# INLINE singletonRowSet #-}

singletonRowSetInt :: Int -> RowSet
singletonRowSetInt rowId
  | rowId < 0 = RowSetEmpty
  | otherwise = RowSetSingleton rowId
{-# INLINE singletonRowSetInt #-}

rowSetFromList :: [RowId] -> RowSet
rowSetFromList =
  rowSetFromIntSetCanonical . IntSet.fromList . fmap rowIdInt
{-# INLINE rowSetFromList #-}

rowSetFromIntSetCanonical :: IntSet -> RowSet
rowSetFromIntSetCanonical rows =
  case IntSet.lookupMax nonnegativeRows of
    Nothing ->
      RowSetEmpty
    Just maxRow ->
      rowSetFromNonnegativeIntSetWithUniverse (maxRow + 1) nonnegativeRows
  where
    !nonnegativeRows =
      nonnegativeIntSet rows
{-# INLINE rowSetFromIntSetCanonical #-}

rowSetFromIntSetWithUniverse :: Int -> IntSet -> RowSet
rowSetFromIntSetWithUniverse universe rows =
  rowSetFromNonnegativeIntSetWithUniverse canonicalUniverse nonnegativeRows
  where
    !nonnegativeRows =
      nonnegativeIntSet rows
    !canonicalUniverse =
      max (max 0 universe) (maybe 0 (+ 1) (IntSet.lookupMax nonnegativeRows))
{-# INLINE rowSetFromIntSetWithUniverse #-}

rowSetFromNonnegativeIntSetWithUniverse :: Int -> IntSet -> RowSet
rowSetFromNonnegativeIntSetWithUniverse universe rows
  | IntSet.null rows =
      RowSetEmpty
  | IntSet.size rows == 1 =
      case IntSet.lookupMin rows of
        Nothing -> RowSetEmpty
        Just rowId -> singletonRowSetInt rowId
  | IntSet.size rows <= rowSetSmallLimit =
      RowSetSmall (smallIntArrayFromAscList (IntSet.toAscList rows))
  | shouldUseDense universe rows =
      RowSetDense universe (denseWordsFromIntSet universe rows)
  | otherwise =
      RowSetSparse rows
{-# INLINE rowSetFromNonnegativeIntSetWithUniverse #-}

rowSetFullRange :: Int -> RowSet
rowSetFullRange count
  | count <= 0 =
      RowSetEmpty
  | count == 1 =
      RowSetSingleton 0
  | count <= rowSetSmallLimit =
      RowSetSmall (smallIntArrayFromAscList [0 .. count - 1])
  | count >= rowSetDenseMinUniverse =
      RowSetDense count (denseFullRangeWords count)
  | otherwise =
      RowSetSparse (IntSet.fromDistinctAscList [0 .. count - 1])
{-# INLINE rowSetFullRange #-}

shouldUseDense :: Int -> IntSet -> Bool
shouldUseDense universe rows =
  universe >= rowSetDenseMinUniverse
    && IntSet.size rows * denseDensityDenominator >= universe
{-# INLINE shouldUseDense #-}

rowSetToList :: RowSet -> [RowId]
rowSetToList =
  fmap RowId . rowSetToIntList
{-# INLINE rowSetToList #-}

rowSetToIntList :: RowSet -> [Int]
rowSetToIntList = \case
  RowSetEmpty ->
    []
  RowSetSingleton rowId ->
    [rowId]
  RowSetSmall values ->
    smallIntArrayToAscList values
  RowSetDense universe denseWords ->
    denseToList universe denseWords
  RowSetSparse rows ->
    IntSet.toAscList rows
{-# INLINE rowSetToIntList #-}

rowSetToIntSet :: RowSet -> IntSet
rowSetToIntSet = \case
  RowSetEmpty ->
    IntSet.empty
  RowSetSingleton rowId ->
    IntSet.singleton rowId
  RowSetSmall values ->
    IntSet.fromDistinctAscList (smallIntArrayToAscList values)
  RowSetDense universe denseWords ->
    IntSet.fromDistinctAscList (denseToList universe denseWords)
  RowSetSparse rows ->
    rows
{-# INLINE rowSetToIntSet #-}

rowSetNull :: RowSet -> Bool
rowSetNull = \case
  RowSetEmpty -> True
  RowSetSingleton _ -> False
  RowSetSmall values -> PrimArray.sizeofPrimArray values == 0
  RowSetDense _ denseWords -> not (anyWordNonZero denseWords)
  RowSetSparse rows -> IntSet.null rows
{-# INLINE rowSetNull #-}

rowSetSize :: RowSet -> Int
rowSetSize = \case
  RowSetEmpty ->
    0
  RowSetSingleton _ ->
    1
  RowSetSmall values ->
    PrimArray.sizeofPrimArray values
  RowSetDense _ denseWords ->
    foldPrimArrayWords' (\acc word -> acc + popCount word) 0 denseWords
  RowSetSparse rows ->
    IntSet.size rows
{-# INLINE rowSetSize #-}

rowSetDigest :: RowSet -> Word64
rowSetDigest rows =
  rowSetFoldInts'
    (\hashValue rowId -> mix64 hashValue (wordOfInt rowId))
    (mix64 0x6a09e667f3bcc909 (wordOfInt (rowSetSize rows)))
    rows
{-# INLINE rowSetDigest #-}

mix64 :: Word64 -> Word64 -> Word64
mix64 hashValue value =
  let !x0 = hashValue `xor` value
      !x1 = (x0 `xor` (x0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !x2 = (x1 `xor` (x1 `shiftR` 27)) * 0x94d049bb133111eb
   in x2 `xor` (x2 `shiftR` 31)

wordOfInt :: Int -> Word64
wordOfInt =
  fromIntegral

rowSetMember :: RowId -> RowSet -> Bool
rowSetMember rowId =
  rowSetMemberInt (rowIdInt rowId)
{-# INLINE rowSetMember #-}

rowSetMemberInt :: Int -> RowSet -> Bool
rowSetMemberInt rowId set
  | rowId < 0 = False
  | otherwise =
      case set of
        RowSetEmpty ->
          False
        RowSetSingleton only ->
          rowId == only
        RowSetSmall values ->
          smallIntArrayMember rowId values
        RowSetDense universe denseWords ->
          rowId < universe && denseMember rowId denseWords
        RowSetSparse rows ->
          IntSet.member rowId rows
{-# INLINE rowSetMemberInt #-}

rowSetInsert :: RowId -> RowSet -> RowSet
rowSetInsert rowId set
  = rowSetInsertInt (rowIdInt rowId) set
{-# INLINE rowSetInsert #-}

rowSetInsertInt :: Int -> RowSet -> RowSet
rowSetInsertInt rowId set
  | rowId < 0 = set
  | rowSetMemberInt rowId set = set
  | otherwise =
      case set of
        RowSetEmpty ->
          RowSetSingleton rowId
        RowSetSingleton only ->
          rowSetFromIntSetWithUniverse
            (max (rowId + 1) (only + 1))
            (IntSet.fromList [only, rowId])
        RowSetSmall values ->
          rowSetFromIntSetWithUniverse
            (max (rowId + 1) (smallUniverse values))
            (IntSet.insert rowId (IntSet.fromDistinctAscList (smallIntArrayToAscList values)))
        RowSetDense universe denseWords ->
          rowSetInsertDense rowId universe denseWords
        RowSetSparse rows ->
          rowSetFromIntSetWithUniverse
            (max (rowId + 1) (sparseUniverse rows))
            (IntSet.insert rowId rows)
{-# INLINE rowSetInsertInt #-}

smallUniverse :: PrimArray Int -> Int
smallUniverse values =
  case PrimArray.sizeofPrimArray values of
    0 -> 0
    count -> PrimArray.indexPrimArray values (count - 1) + 1
{-# INLINE smallUniverse #-}

sparseUniverse :: IntSet -> Int
sparseUniverse =
  maybe 0 (+ 1) . IntSet.lookupMax
{-# INLINE sparseUniverse #-}

rowSetInsertDense :: Int -> Int -> PrimArray Word -> RowSet
rowSetInsertDense rowId universe denseWords
  | shouldUseDenseCount newUniverse newCount =
      RowSetDense newUniverse (denseInsertWord rowId newUniverse denseWords)
  | otherwise =
      rowSetFromIntSetWithUniverse
        newUniverse
        (IntSet.insert rowId (IntSet.fromDistinctAscList (denseToList universe denseWords)))
  where
    !newUniverse =
      max universe (rowId + 1)
    !newCount =
      foldPrimArrayWords' (\acc word -> acc + popCount word) 1 denseWords
{-# INLINE rowSetInsertDense #-}

denseInsertWord :: Int -> Int -> PrimArray Word -> PrimArray Word
denseInsertWord rowId universe denseWords =
  PrimArray.generatePrimArray (wordCount universe) wordAt
  where
    !targetWord =
      rowId `quot` wordBits
    !targetBit =
      rowId `rem` wordBits
    !sourceWords =
      PrimArray.sizeofPrimArray denseWords

    wordAt wordIx =
      maskDenseWord universe wordIx $
        if wordIx == targetWord
          then setBit sourceWord targetBit
          else sourceWord
      where
        !sourceWord =
          if wordIx < sourceWords
            then PrimArray.indexPrimArray denseWords wordIx
            else zeroBits
{-# INLINE denseInsertWord #-}

rowSetDelete :: RowId -> RowSet -> RowSet
rowSetDelete rowId set
  = rowSetDeleteInt (rowIdInt rowId) set
{-# INLINE rowSetDelete #-}

rowSetDeleteInt :: Int -> RowSet -> RowSet
rowSetDeleteInt rowId set
  | rowId < 0 = set
  | otherwise =
      case set of
        RowSetEmpty ->
          RowSetEmpty
        RowSetSingleton only
          | rowId == only -> RowSetEmpty
          | otherwise -> set
        RowSetSmall values ->
          rowSetFromIntSetWithUniverse
            (smallUniverse values)
            (IntSet.delete rowId (IntSet.fromDistinctAscList (smallIntArrayToAscList values)))
        RowSetDense universe denseWords ->
          rowSetDeleteDense rowId universe denseWords
        RowSetSparse rows ->
          rowSetFromIntSetWithUniverse
            (sparseUniverse rows)
            (IntSet.delete rowId rows)
{-# INLINE rowSetDeleteInt #-}

rowSetDeleteDense :: Int -> Int -> PrimArray Word -> RowSet
rowSetDeleteDense rowId universe denseWords
  | rowId >= universe =
      RowSetDense universe denseWords
  | not (denseMember rowId denseWords) =
      RowSetDense universe denseWords
  | newCount <= rowSetSmallLimit =
      rowSetFromIntSetWithUniverse
        universe
        (IntSet.fromDistinctAscList (denseToList universe deletedWords))
  | shouldUseDenseCount universe newCount =
      RowSetDense universe deletedWords
  | otherwise =
      RowSetSparse (IntSet.fromDistinctAscList (denseToList universe deletedWords))
  where
    !deletedWords =
      denseDeleteWord rowId universe denseWords
    !newCount =
      foldPrimArrayWords' (\acc word -> acc + popCount word) 0 denseWords - 1
{-# INLINE rowSetDeleteDense #-}

denseDeleteWord :: Int -> Int -> PrimArray Word -> PrimArray Word
denseDeleteWord rowId universe denseWords =
  PrimArray.generatePrimArray (PrimArray.sizeofPrimArray denseWords) wordAt
  where
    !targetWord =
      rowId `quot` wordBits
    !targetBit =
      rowId `rem` wordBits

    wordAt wordIx =
      maskDenseWord universe wordIx $
        if wordIx == targetWord
          then clearBit sourceWord targetBit
          else sourceWord
      where
        !sourceWord =
          PrimArray.indexPrimArray denseWords wordIx
{-# INLINE denseDeleteWord #-}

rowSetUnion :: RowSet -> RowSet -> RowSet
rowSetUnion left right =
  case (left, right) of
    (RowSetEmpty, _) -> right
    (_, RowSetEmpty) -> left
    (RowSetDense lu lw, RowSetDense ru rw)
      | lu == ru ->
          denseBinary lu (.|.) lw rw
    _ ->
      rowSetFromIntSetCanonical (IntSet.union (rowSetToIntSet left) (rowSetToIntSet right))
{-# INLINE rowSetUnion #-}

rowSetIntersection :: RowSet -> RowSet -> RowSet
rowSetIntersection left right =
  case (left, right) of
    (RowSetEmpty, _) -> RowSetEmpty
    (_, RowSetEmpty) -> RowSetEmpty
    (RowSetSingleton rowId, other) ->
      if rowSetMemberInt rowId other then RowSetSingleton rowId else RowSetEmpty
    (other, RowSetSingleton rowId) ->
      if rowSetMemberInt rowId other then RowSetSingleton rowId else RowSetEmpty
    (RowSetDense lu lw, RowSetDense ru rw)
      | lu == ru ->
          denseBinary lu (.&.) lw rw
    _ ->
      let !universe = maxUniverse left right
          (smaller, larger) =
            if rowSetSize left <= rowSetSize right
              then (left, right)
              else (right, left)
       in rowSetFromIntSetWithUniverse universe $
            rowSetFoldInts'
              ( \acc rowId ->
                  if rowSetMemberInt rowId larger
                    then IntSet.insert rowId acc
                    else acc
              )
              IntSet.empty
              smaller
{-# INLINE rowSetIntersection #-}

rowSetDifference :: RowSet -> RowSet -> RowSet
rowSetDifference left right =
  case (left, right) of
    (RowSetEmpty, _) ->
      RowSetEmpty
    (_, RowSetEmpty) ->
      left
    (RowSetDense lu lw, RowSetDense ru rw)
      | lu == ru ->
          denseBinary lu (\a b -> a .&. complement b) lw rw
    _ ->
      rowSetFromIntSetCanonical (IntSet.difference (rowSetToIntSet left) (rowSetToIntSet right))
{-# INLINE rowSetDifference #-}

rowSetIntersects :: RowSet -> RowSet -> Bool
rowSetIntersects left right =
  case (left, right) of
    (RowSetEmpty, _) -> False
    (_, RowSetEmpty) -> False
    (RowSetSingleton rowId, other) -> rowSetMemberInt rowId other
    (other, RowSetSingleton rowId) -> rowSetMemberInt rowId other
    (RowSetSmall values, other) ->
      any (\rowId -> rowSetMemberInt rowId other) (smallIntArrayToAscList values)
    (other, RowSetSmall values) ->
      any (\rowId -> rowSetMemberInt rowId other) (smallIntArrayToAscList values)
    (RowSetSparse rows, other) ->
      IntSet.foldr (\rowId rest -> rowSetMemberInt rowId other || rest) False rows
    (other, RowSetSparse rows) ->
      IntSet.foldr (\rowId rest -> rowSetMemberInt rowId other || rest) False rows
    (RowSetDense _ leftWords, RowSetDense _ rightWords) ->
      denseIntersects leftWords rightWords
{-# INLINE rowSetIntersects #-}

rowSetIntersectionWithRowIdSet :: RowIdSet -> RowSet -> RowSet
rowSetIntersectionWithRowIdSet rowIds active =
  case rowSetIntersectionWithRowIdSetChanged rowIds active of
    RowSetRestrictionEmpty ->
      RowSetEmpty
    RowSetRestrictionUnchanged ->
      active
    RowSetRestrictionChanged restricted ->
      restricted
{-# INLINE rowSetIntersectionWithRowIdSet #-}

rowSetIntersectionWithRowIdSetChanged ::
  RowIdSet ->
  RowSet ->
  RowSetRestriction
rowSetIntersectionWithRowIdSetChanged rowIds active
  | rowSetNull active =
      RowSetRestrictionEmpty
  | rowIdSetNull rowIds =
      RowSetRestrictionEmpty
  | rowIdSetSize rowIds <= rowIdSetSmallLimit =
      rowSetRestrictionFromRestricted active $
        rowSetIntersectionWithRowIdSetByRows rowIds active
  | rowSetSize active <= rowIdSetSize rowIds
      && rowSetSubsetOfRowIdSet rowIds active =
      RowSetRestrictionUnchanged
  | otherwise =
      let !restricted =
            rowSetIntersectionWithRowIdSetStrict rowIds active
       in if rowSetNull restricted
            then RowSetRestrictionEmpty
            else
              if rowSetSize restricted == rowSetSize active
                then RowSetRestrictionUnchanged
                else RowSetRestrictionChanged restricted
{-# INLINE rowSetIntersectionWithRowIdSetChanged #-}

rowSetRestrictionFromRestricted ::
  RowSet ->
  RowSet ->
  RowSetRestriction
rowSetRestrictionFromRestricted active restricted
  | rowSetNull restricted =
      RowSetRestrictionEmpty
  | restricted == active =
      RowSetRestrictionUnchanged
  | otherwise =
      RowSetRestrictionChanged restricted
{-# INLINE rowSetRestrictionFromRestricted #-}

rowSetIntersectionWithRowIdSetByRows ::
  RowIdSet ->
  RowSet ->
  RowSet
rowSetIntersectionWithRowIdSetByRows rowIds active =
  rowSetFromIntSetWithUniverse
    (universeOf active)
    ( rowIdSetFoldl'
        ( \acc rowId ->
            let rowKey = rowIdInt rowId
             in if rowSetMemberInt rowKey active
              then IntSet.insert rowKey acc
              else acc
        )
        IntSet.empty
        rowIds
    )
{-# INLINE rowSetIntersectionWithRowIdSetByRows #-}

rowSetIntersectionWithRowIdSetStrict ::
  RowIdSet ->
  RowSet ->
  RowSet
rowSetIntersectionWithRowIdSetStrict rowIds active
  | rowIdSetSize rowIds <= rowIdSetSmallLimit =
      rowSetIntersectionWithRowIdSetByRows rowIds active
  | rowIdSetSize rowIds <= rowSetSize active =
      rowSetFromIntSetWithUniverse
        (universeOf active)
        ( rowIdSetFoldl'
            ( \acc rowId ->
                let rowKey = rowIdInt rowId
                 in if rowSetMemberInt rowKey active
                  then IntSet.insert rowKey acc
                  else acc
            )
            IntSet.empty
            rowIds
        )
  | otherwise =
      rowSetFromIntSetWithUniverse
        (universeOf active)
        (rowIdSetIntersectionWithIntSet rowIds (rowSetToIntSet active))
{-# INLINE rowSetIntersectionWithRowIdSetStrict #-}

rowSetSubsetOfRowIdSet ::
  RowIdSet ->
  RowSet ->
  Bool
rowSetSubsetOfRowIdSet rowIds =
  \case
    RowSetEmpty ->
      True
    RowSetSingleton rowId ->
      rowIdSetMember (RowId rowId) rowIds
    RowSetSmall values ->
      all (\rowId -> rowIdSetMember (RowId rowId) rowIds) (smallIntArrayToAscList values)
    RowSetDense universe denseWords ->
      all (\rowId -> rowIdSetMember (RowId rowId) rowIds) (denseToList universe denseWords)
    RowSetSparse rows ->
      IntSet.foldr
        (\rowId rest -> rowIdSetMember (RowId rowId) rowIds && rest)
        True
        rows
{-# INLINE rowSetSubsetOfRowIdSet #-}

rowSetIntersectsRowIdSet :: RowIdSet -> RowSet -> Bool
rowSetIntersectsRowIdSet rowIds active =
  case active of
    RowSetEmpty ->
      False
    RowSetSparse rows ->
      rowIdSetIntersectsIntSet rowIds rows
    _ ->
      rowIdSetAny (\rowId -> rowSetMemberInt (rowIdInt rowId) active) rowIds
{-# INLINE rowSetIntersectsRowIdSet #-}

rowSetFoldl' :: (acc -> RowId -> acc) -> acc -> RowSet -> acc
rowSetFoldl' step =
  rowSetFoldInts' (\acc rowId -> step acc (RowId rowId))
{-# INLINE rowSetFoldl' #-}

rowSetFoldInts' :: (acc -> Int -> acc) -> acc -> RowSet -> acc
rowSetFoldInts' step initial = \case
  RowSetEmpty ->
    initial
  RowSetSingleton rowId ->
    step initial rowId
  RowSetSmall values ->
    smallIntArrayFoldl' step initial values
  RowSetDense universe denseWords ->
    foldDense' universe denseWords step initial
  RowSetSparse rows ->
    IntSet.foldl' step initial rows
{-# INLINE rowSetFoldInts' #-}

validateRowSet :: RowSet -> Either RowSetError ()
validateRowSet = \case
  RowSetEmpty ->
    Right ()
  RowSetSingleton rowId
    | rowId < 0 -> Left (RowSetSingletonNegative rowId)
    | otherwise -> Right ()
  RowSetSmall values ->
    let !count = PrimArray.sizeofPrimArray values
     in if count > rowSetSmallLimit
          then Left (RowSetSmallTooLarge count)
          else validateSmall values
  RowSetDense universe denseWords
    | universe < 0 ->
        Left (RowSetDenseNegativeUniverse universe)
    | PrimArray.sizeofPrimArray denseWords /= wordCount universe ->
        Left (RowSetDenseWrongWordCount (PrimArray.sizeofPrimArray denseWords) (wordCount universe))
    | otherwise ->
        Right ()
  RowSetSparse rows ->
    let !count = IntSet.size rows
     in case negativeIntSetMinimum rows of
          Just negativeMember ->
            Left (RowSetNegativeMember negativeMember)
          Nothing ->
            if count <= rowSetSmallLimit
              then Left (RowSetSparseTooSmall count)
              else Right ()
{-# INLINE validateRowSet #-}

maxUniverse :: RowSet -> RowSet -> Int
maxUniverse left right =
  max (universeOf left) (universeOf right)
{-# INLINE maxUniverse #-}

universeOf :: RowSet -> Int
universeOf = \case
  RowSetEmpty -> 0
  RowSetSingleton rowId -> rowId + 1
  RowSetSmall values ->
    case PrimArray.sizeofPrimArray values of
      0 -> 0
      n -> PrimArray.indexPrimArray values (n - 1) + 1
  RowSetDense universe _ -> universe
  RowSetSparse rows ->
    maybe 0 (+ 1) (IntSet.lookupMax rows)
{-# INLINE universeOf #-}

wordCount :: Int -> Int
wordCount universe =
  (max 0 universe + wordBits - 1) `quot` wordBits
{-# INLINE wordCount #-}

denseWordsFromIntSet :: Int -> IntSet -> PrimArray Word
denseWordsFromIntSet universe rows =
  runST $ do
    let !wc = wordCount universe
    mutable <- PrimArray.newPrimArray wc
    fillWords mutable wc 0

    let setOne rowId
          | rowId < 0 = pure ()
          | rowId >= universe = pure ()
          | otherwise = do
              let !wi = rowId `quot` wordBits
                  !bi = rowId `rem` wordBits
              word0 <- PrimArray.readPrimArray mutable wi
              PrimArray.writePrimArray mutable wi (setBit word0 bi)

    IntSet.foldl'
      (\action rowId -> action *> setOne rowId)
      (pure ())
      rows

    PrimArray.unsafeFreezePrimArray mutable
{-# INLINE denseWordsFromIntSet #-}

denseFullRangeWords :: Int -> PrimArray Word
denseFullRangeWords universe =
  PrimArray.generatePrimArray wc wordAt
  where
    !wc = wordCount universe
    !lastIndex = wc - 1
    !fullWord = complement zeroBits
    !lastBits = universe `rem` wordBits
    !lastWord =
      if lastBits == 0
        then fullWord
        else bit lastBits - 1

    wordAt ix
      | ix == lastIndex = lastWord
      | otherwise = fullWord
{-# INLINE denseFullRangeWords #-}

fillWords :: PrimArray.MutablePrimArray s Word -> Int -> Word -> ST s ()
fillWords mutable count value =
  PrimArray.setPrimArray mutable 0 count value
{-# INLINE fillWords #-}

denseMember :: Int -> PrimArray Word -> Bool
denseMember rowId denseWords =
  let !wi = rowId `quot` wordBits
      !bi = rowId `rem` wordBits
   in wi >= 0
        && wi < PrimArray.sizeofPrimArray denseWords
        && testBit (PrimArray.indexPrimArray denseWords wi) bi
{-# INLINE denseMember #-}

denseToList :: Int -> PrimArray Word -> [Int]
denseToList universe denseWords =
  buildWord 0
  where
    !wc = PrimArray.sizeofPrimArray denseWords

    buildWord !wi
      | wi == wc = []
      | otherwise =
          let !word = PrimArray.indexPrimArray denseWords wi
              !base = wi * wordBits
           in buildBits base word (buildWord (wi + 1))

    buildBits !base !word rest
      | word == 0 = rest
      | rowId >= universe = rest
      | otherwise = rowId : buildBits base wordRest rest
      where
        !bitIx = countTrailingZeros word
        !rowId = base + bitIx
        !wordRest = word .&. (word - 1)
{-# INLINE denseToList #-}

denseBinary :: Int -> (Word -> Word -> Word) -> PrimArray Word -> PrimArray Word -> RowSet
denseBinary universe op left right =
  case count of
    0 ->
      RowSetEmpty
    1 ->
      case denseFirstSetBit universe denseWords of
        Nothing -> RowSetEmpty
        Just rowId -> RowSetSingleton rowId
    _
      | count <= rowSetSmallLimit ->
          RowSetSmall (smallIntArrayFromAscList (denseToList universe denseWords))
      | shouldUseDenseCount universe count ->
          RowSetDense universe denseWords
      | otherwise ->
          RowSetSparse (IntSet.fromDistinctAscList (denseToList universe denseWords))
  where
    !n = min (PrimArray.sizeofPrimArray left) (PrimArray.sizeofPrimArray right)

    (!denseWords, !count) =
      runST $ do
        mutable <- PrimArray.newPrimArray n
        let go !ix !total
              | ix == n = do
                  frozen <- PrimArray.unsafeFreezePrimArray mutable
                  pure (frozen, total)
              | otherwise = do
                  let !w =
                        maskDenseWord universe ix $
                          op
                            (PrimArray.indexPrimArray left ix)
                            (PrimArray.indexPrimArray right ix)
                      !total' = total + popCount w
                  PrimArray.writePrimArray mutable ix w
                  go (ix + 1) total'
        go 0 0
{-# INLINE denseBinary #-}

shouldUseDenseCount :: Int -> Int -> Bool
shouldUseDenseCount universe count =
  universe >= rowSetDenseMinUniverse
    && count * denseDensityDenominator >= universe
{-# INLINE shouldUseDenseCount #-}

maskDenseWord :: Int -> Int -> Word -> Word
maskDenseWord universe wordIx word
  | wordIx == wordCount universe - 1 = word .&. denseLastWordMask universe
  | otherwise = word
{-# INLINE maskDenseWord #-}

denseLastWordMask :: Int -> Word
denseLastWordMask universe =
  case universe `rem` wordBits of
    0 -> complement zeroBits
    bits -> bit bits - 1
{-# INLINE denseLastWordMask #-}

denseFirstSetBit :: Int -> PrimArray Word -> Maybe Int
denseFirstSetBit universe denseWords =
  go 0
  where
    !wc = PrimArray.sizeofPrimArray denseWords

    go !wi
      | wi == wc = Nothing
      | word == 0 = go (wi + 1)
      | rowId >= universe = Nothing
      | otherwise = Just rowId
      where
        !word = PrimArray.indexPrimArray denseWords wi
        !rowId = wi * wordBits + countTrailingZeros word
{-# INLINE denseFirstSetBit #-}

denseIntersects :: PrimArray Word -> PrimArray Word -> Bool
denseIntersects left right =
  let !n = min (PrimArray.sizeofPrimArray left) (PrimArray.sizeofPrimArray right)

      go !ix
        | ix == n = False
        | otherwise =
            (PrimArray.indexPrimArray left ix .&. PrimArray.indexPrimArray right ix) /= 0
              || go (ix + 1)
   in go 0
{-# INLINE denseIntersects #-}

anyWordNonZero :: PrimArray Word -> Bool
anyWordNonZero denseWords =
  let !n = PrimArray.sizeofPrimArray denseWords

      go !ix
        | ix == n = False
        | otherwise =
            PrimArray.indexPrimArray denseWords ix /= 0 || go (ix + 1)
   in go 0
{-# INLINE anyWordNonZero #-}

foldPrimArrayWords' :: (acc -> Word -> acc) -> acc -> PrimArray Word -> acc
foldPrimArrayWords' step initial values =
  let !n = PrimArray.sizeofPrimArray values

      go !ix !acc
        | ix == n = acc
        | otherwise =
            let !value = PrimArray.indexPrimArray values ix
                !acc' = step acc value
             in go (ix + 1) acc'
   in go 0 initial
{-# INLINE foldPrimArrayWords' #-}

foldDense' :: Int -> PrimArray Word -> (acc -> Int -> acc) -> acc -> acc
foldDense' universe denseWords step initial =
  let !wc = PrimArray.sizeofPrimArray denseWords

      goWord !wi !acc
        | wi == wc = acc
        | otherwise =
            let !word = PrimArray.indexPrimArray denseWords wi
                !base = wi * wordBits
                !acc' = goBits base word acc
             in goWord (wi + 1) acc'

      goBits !base !word !acc
        | word == 0 = acc
        | rowId >= universe = acc
        | otherwise =
            let !acc' = step acc rowId
             in goBits base wordRest acc'
        where
          !bitIx = countTrailingZeros word
          !rowId = base + bitIx
          !wordRest = word .&. (word - 1)
   in goWord 0 initial
{-# INLINE foldDense' #-}

validateSmall :: PrimArray Int -> Either RowSetError ()
validateSmall values =
  case negativeSmallValue values of
    Just negativeMember ->
      Left (RowSetNegativeMember negativeMember)
    Nothing ->
      validateSmallIntArrayAscending RowSetSmallNotStrictlyAscending values
{-# INLINE validateSmall #-}

negativeSmallValue :: PrimArray Int -> Maybe Int
negativeSmallValue =
  smallIntArrayFoldl'
    ( \found value ->
        case found of
          Just negativeMember ->
            Just negativeMember
          Nothing
            | value < 0 -> Just value
            | otherwise -> Nothing
    )
    Nothing
{-# INLINE negativeSmallValue #-}

negativeIntSetMinimum :: IntSet -> Maybe Int
negativeIntSetMinimum values =
  case IntSet.lookupMin values of
    Just minimumValue
      | minimumValue < 0 -> Just minimumValue
    _ -> Nothing
{-# INLINE negativeIntSetMinimum #-}

nonnegativeIntSet :: IntSet -> IntSet
nonnegativeIntSet =
  IntSet.filter (>= 0)
{-# INLINE nonnegativeIntSet #-}
