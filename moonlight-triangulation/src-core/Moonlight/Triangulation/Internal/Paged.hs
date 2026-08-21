{-# LANGUAGE BangPatterns #-}

module Moonlight.Triangulation.Internal.Paged
  ( Paged
  , MutablePaged
  , FlatMutablePaged
  , emptyPaged
  , fromVector
  , fromLocalVector
  , toVector
  , pagedLength
  , pagedUnsafeIndex
  , pagedFoldl'
  , newMutablePaged
  , newLocalMutablePaged
  , TransactionShape (..)
  , thawPaged
  , thawPagedDense
  , thawPagedShaped
  , flatMutableSection
  , readFlatMutable
  , writeFlatMutable
  , readPaged
  , writePaged
  , freezePaged
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV

-- | One physical store with two lawful local sections. A bulk constructor
-- publishes one contiguous plane for hot traversal. A persistent edit derives
-- fixed-size shared pages from that plane without copying and republishes only
-- pages it wrote. Both constructors denote exactly the same indexed sequence;
-- equality, serialization, and compaction descend through that sequence.
data Paged a
  = FlatPaged {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(U.Vector a)
  | SharedPaged {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector (U.Vector a))

-- | A fresh build owns a contiguous arena. A transaction over a published
-- value owns only its mutable page table and clones a page before its first
-- write. No mutable page or state token can escape this module.
data MutablePaged s a
  = MutableFlatPaged {-# UNPACK #-} !Int !(MUV.MVector s a)
  | MutableSharedPaged
      {-# UNPACK #-} !Int
      !(Paged a)
      !(V.Vector (U.Vector a))
      !(MV.MVector s (MUV.MVector s a))
      !(MUV.MVector s Bool)
      !(STRef s Int)

-- | A proof that one mutable sequence is in its contiguous physical section.
-- The constructor stays private: callers can only obtain it by descending
-- through 'flatMutableSection', after which hot indexed programs no longer
-- re-test the storage sum at every cell.
newtype FlatMutablePaged s a = FlatMutablePaged (MUV.MVector s a)

localPageBits :: Int
localPageBits = 8
{-# INLINE localPageBits #-}

-- A published plane's page geometry is the clone quantum of every
-- copy-on-write transaction opened on it. Full ladder 2026-08-06 on the
-- star-local excise (persistent-removal 10k/2.5k, min of 4 interleaved
-- rounds): 62.6ms at 12 bits, 57.2ms at 10, 83.2ms at 8 — reproducing the
-- 2026-08-05 finding that at 8 bits the per-transaction table build and
-- freeze scan outweigh the smaller clones (profiled there at 61% of the
-- lane), while at 12 the scattered-touch clones dominate instead. 10 is the
-- measured saddle between the two taxes.
traversalPageBits :: Int
traversalPageBits = 10
{-# INLINE traversalPageBits #-}

pageSize :: Int -> Int
pageSize bits = 1 `shiftL` bits
{-# INLINE pageSize #-}

pageOf :: Int -> Int -> Int
pageOf bits index = index `shiftR` bits
{-# INLINE pageOf #-}

offsetOf :: Int -> Int -> Int
offsetOf bits index = index .&. (pageSize bits - 1)
{-# INLINE offsetOf #-}

pagesFor :: Int -> Int -> Int
pagesFor bits count
  | count <= 0 = 0
  | otherwise = pageOf bits (count + pageSize bits - 1)
{-# INLINE pagesFor #-}

pagedLength :: Paged a -> Int
pagedLength paged =
  case paged of
    FlatPaged count _ _ -> count
    SharedPaged count _ _ -> count
{-# INLINE pagedLength #-}

pagedBits :: Paged a -> Int
pagedBits paged = case paged of
  FlatPaged _ bits _ -> bits
  SharedPaged _ bits _ -> bits
{-# INLINE pagedBits #-}

pageVector :: U.Unbox a => Paged a -> V.Vector (U.Vector a)
pageVector source = case source of
  FlatPaged count bits values ->
    V.generate
      (pagesFor bits count)
      (\index ->
         let !offset = index `shiftL` bits
             !width = min (pageSize bits) (count - offset)
          in U.unsafeSlice offset width values
      )
  SharedPaged _ _ pages -> pages
{-# INLINE pageVector #-}

instance (U.Unbox a, Eq a) => Eq (Paged a) where
  left == right =
    pagedLength left == pagedLength right
      && case (left, right) of
           (FlatPaged _ _ leftValues, FlatPaged _ _ rightValues) ->
             leftValues == rightValues
           _ -> pagedChunksEqual left right

-- Structural equality is a join shortcut and must not materialize either
-- operand. Chunks are compared over the overlap of the two page geometries,
-- so operands with different page widths still compare slice against slice.
pagedChunksEqual :: (U.Unbox a, Eq a) => Paged a -> Paged a -> Bool
pagedChunksEqual left right = walk 0
 where
  !count = pagedLength left
  !leftBits = pagedBits left
  !rightBits = pagedBits right
  leftPages = pageVector left
  rightPages = pageVector right
  walk !offset
    | offset >= count = True
    | otherwise =
        let !leftPage = V.unsafeIndex leftPages (pageOf leftBits offset)
            !leftOffset = offsetOf leftBits offset
            !rightPage = V.unsafeIndex rightPages (pageOf rightBits offset)
            !rightOffset = offsetOf rightBits offset
            !width =
              min
                (count - offset)
                ( min
                    (U.length leftPage - leftOffset)
                    (U.length rightPage - rightOffset)
                )
         in U.unsafeSlice leftOffset width leftPage
              == U.unsafeSlice rightOffset width rightPage
              && walk (offset + width)

instance (U.Unbox a, Show a) => Show (Paged a) where
  showsPrec precedence = showsPrec precedence . U.toList . toVector

instance NFData (Paged a) where
  rnf paged =
    case paged of
      FlatPaged count bits values -> count `seq` bits `seq` values `seq` ()
      SharedPaged count bits pages ->
        count `seq` bits `seq` V.foldl' (\() page -> page `seq` ()) () pages

emptyPaged :: Paged a
emptyPaged = SharedPaged 0 traversalPageBits V.empty

-- The padding belongs to page publication rather than dense ingress. A dense
-- plane stores exactly its live sequence and therefore needs no tail value.
fromVector :: U.Unbox a => a -> U.Vector a -> Paged a
fromVector _padding values = FlatPaged (U.length values) traversalPageBits values

-- | Dense ingress whose future edits are expected to be local. The value is
-- still one flat plane until a transaction publishes a changed leaf.
fromLocalVector :: U.Unbox a => a -> U.Vector a -> Paged a
fromLocalVector _padding values = FlatPaged (U.length values) localPageBits values

toVector :: U.Unbox a => Paged a -> U.Vector a
toVector paged =
  case paged of
    FlatPaged _ _ values -> values
    SharedPaged count bits pages
      | count <= 0 -> U.empty
      | otherwise -> runST $ do
          output <- MUV.new count
          let copyPage !pageIndex !offset
                | offset >= count = pure ()
                | otherwise = do
                    let !width = min (pageSize bits) (count - offset)
                    U.copy
                      (MUV.unsafeSlice offset width output)
                      (U.unsafeSlice 0 width (V.unsafeIndex pages pageIndex))
                    copyPage (pageIndex + 1) (offset + width)
          copyPage 0 0
          U.unsafeFreeze output

pagedUnsafeIndex :: U.Unbox a => Paged a -> Int -> a
pagedUnsafeIndex paged index =
  case paged of
    FlatPaged _ _ values -> U.unsafeIndex values index
    SharedPaged _ bits pages ->
      U.unsafeIndex (V.unsafeIndex pages (pageOf bits index)) (offsetOf bits index)
{-# INLINE pagedUnsafeIndex #-}

pagedFoldl' :: U.Unbox a => (b -> a -> b) -> b -> Paged a -> b
pagedFoldl' step initial paged =
  case paged of
    FlatPaged _ _ values -> U.foldl' step initial values
    SharedPaged count bits pages -> foldPages 0 0 initial
     where
      foldPages !pageIndex !offset !accumulated
        | offset >= count = accumulated
        | otherwise =
            let !width = min (pageSize bits) (count - offset)
                !next =
                  U.foldl'
                    step
                    accumulated
                    (U.unsafeSlice 0 width (V.unsafeIndex pages pageIndex))
             in foldPages (pageIndex + 1) (offset + width) next
{-# INLINE pagedFoldl' #-}

-- | Allocate the one contiguous mutable arena owned by a fresh constructor.
newMutablePaged :: U.Unbox a => Int -> ST s (MutablePaged s a)
newMutablePaged capacity = MutableFlatPaged traversalPageBits <$> MUV.new (max 0 capacity)

newLocalMutablePaged :: U.Unbox a => Int -> ST s (MutablePaged s a)
newLocalMutablePaged capacity = MutableFlatPaged localPageBits <$> MUV.new (max 0 capacity)

-- | Derive a copy-on-write page table from a published plane. Flat bases
-- become zero-copy slices of their one ByteArray; already shared bases reuse
-- their page vectors. Capacity-only tail pages all point to one empty sentinel
-- and acquire storage only when an append first writes them. Reads stay one
-- direct page-table access — 2026-08-05, measured: routing reads through a
-- dirty-map overlay instead taxed every walk and regressed the local verbs by
-- 2x, so the table is built eagerly and only publication consults ownership.
thawPaged :: U.Unbox a => Int -> Paged a -> ST s (MutablePaged s a)
thawPaged requestedCapacity paged = do
  let !count = pagedLength paged
      !capacity = max requestedCapacity count
      !bits = pagedBits paged
      !basePages = pageVector paged
      !basePageCount = V.length basePages
      !pageCount = pagesFor bits capacity
  pages <- MV.new pageCount
  owned <- MUV.replicate pageCount False
  ownedCount <- newSTRef 0
  sentinel <- MUV.new 0
  V.imapM_
    (\index immutablePage ->
       U.unsafeThaw immutablePage >>= MV.unsafeWrite pages index
    )
    basePages
  MV.set
    (MV.unsafeSlice basePageCount (pageCount - basePageCount) pages)
    sentinel
  pure (MutableSharedPaged bits paged basePages pages owned ownedCount)

-- | The two lawful physical sections of one transaction boundary. Both
-- publish the same canonical sequence; they differ in what the transaction
-- pays for. The choice belongs to the operation that knows its own edit
-- volume, never to a public caller.
data TransactionShape = DenseTransaction | LocalTransaction

thawPagedShaped :: U.Unbox a => TransactionShape -> Int -> Paged a -> ST s (MutablePaged s a)
thawPagedShaped shape = case shape of
  DenseTransaction -> thawPagedDense
  LocalTransaction -> thawPaged
{-# INLINE thawPagedShaped #-}

-- | Materialize one dense mutable arena from a published value. A batch
-- transaction amortizes this single copy over many edits and then reads and
-- writes flat storage with no page bookkeeping; the copy-on-write 'thawPaged'
-- remains the local-edit section whose publication is proportional to dirty
-- pages. Both freeze to the same canonical sequence.
thawPagedDense :: U.Unbox a => Int -> Paged a -> ST s (MutablePaged s a)
thawPagedDense requestedCapacity paged = do
  let !count = pagedLength paged
      !capacity = max requestedCapacity count
  values <- MUV.new capacity
  case paged of
    FlatPaged _ _ source ->
      U.copy (MUV.unsafeSlice 0 count values) source
    SharedPaged _ bits pages ->
      let copyPage !pageIndex !offset
            | offset >= count = pure ()
            | otherwise = do
                let !width = min (pageSize bits) (count - offset)
                U.copy
                  (MUV.unsafeSlice offset width values)
                  (U.unsafeSlice 0 width (V.unsafeIndex pages pageIndex))
                copyPage (pageIndex + 1) (offset + width)
       in copyPage 0 0
  pure (MutableFlatPaged (pagedBits paged) values)

flatMutableSection :: MutablePaged s a -> Maybe (FlatMutablePaged s a)
flatMutableSection mutable =
  case mutable of
    MutableFlatPaged _ values -> Just (FlatMutablePaged values)
    MutableSharedPaged{} -> Nothing
{-# INLINE flatMutableSection #-}

readFlatMutable :: U.Unbox a => FlatMutablePaged s a -> Int -> ST s a
readFlatMutable (FlatMutablePaged values) = MUV.unsafeRead values
{-# INLINE readFlatMutable #-}

writeFlatMutable :: U.Unbox a => FlatMutablePaged s a -> Int -> a -> ST s ()
writeFlatMutable (FlatMutablePaged values) = MUV.unsafeWrite values
{-# INLINE writeFlatMutable #-}

readPaged :: U.Unbox a => MutablePaged s a -> Int -> ST s a
readPaged mutable index =
  case mutable of
    MutableFlatPaged _ values -> MUV.unsafeRead values index
    MutableSharedPaged bits _ _ pages _ _ -> do
      page <- MV.unsafeRead pages (pageOf bits index)
      MUV.unsafeRead page (offsetOf bits index)
{-# INLINE readPaged #-}

writePaged :: U.Unbox a => MutablePaged s a -> Int -> a -> ST s ()
writePaged mutable index value =
  case mutable of
    MutableFlatPaged _ values -> MUV.unsafeWrite values index value
    MutableSharedPaged bits _ _ pages owned ownedCount -> do
      let !slot = pageOf bits index
      mine <- MUV.unsafeRead owned slot
      page <-
        if mine
          then MV.unsafeRead pages slot
          else acquireSharedWritablePage (pageSize bits) pages owned ownedCount slot
      MUV.unsafeWrite page (offsetOf bits index) value
{-# INLINE writePaged #-}

-- Keep the once-per-page copy-on-write transition behind one compiled
-- boundary. Both steady-state write paths remain inline; dependent modules do
-- not repeatedly simplify page cloning into every element write.
acquireSharedWritablePage
  :: U.Unbox a
  => Int
  -> MV.MVector s (MUV.MVector s a)
  -> MUV.MVector s Bool
  -> STRef s Int
  -> Int
  -> ST s (MUV.MVector s a)
acquireSharedWritablePage width pages owned ownedCount slot = do
  shared <- MV.unsafeRead pages slot
  copy <-
    if MUV.length shared == width
      then MUV.clone shared
      else do
        copy <- MUV.new width
        MUV.copy (MUV.unsafeSlice 0 (MUV.length shared) copy) shared
        pure copy
  MV.unsafeWrite pages slot copy
  MUV.unsafeWrite owned slot True
  modifySTRef' ownedCount (+ 1)
  pure copy
{-# INLINE[0] acquireSharedWritablePage #-}

freezePaged :: U.Unbox a => Int -> MutablePaged s a -> ST s (Paged a)
freezePaged count mutable
  | count <= 0 = pure emptyPaged
  | otherwise =
      case mutable of
        -- 'U.take' is an O(1) slice, so freezing that way republishes the whole
        -- growth reservation and holds it for the lifetime of the value. The
        -- arena is deliberately loose during construction; carrying that slack
        -- past publication is not the same decision. Above an eighth the copy
        -- is paid once and the dead tail is released, which is also what the
        -- shared branch below already does by generating exactly its pages.
        MutableFlatPaged bits values
          | MUV.length values <= count + (count `quot` 8) ->
              FlatPaged count bits . U.take count <$> U.unsafeFreeze values
          | otherwise ->
              FlatPaged count bits <$> U.freeze (MUV.unsafeSlice 0 count values)
        MutableSharedPaged bits base basePages pages owned ownedCount -> do
          dirtyPages <- readSTRef ownedCount
          if dirtyPages == 0 && count == pagedLength base
            -- An untouched plane republishes as the value it opened on.
            then pure base
            else
              SharedPaged count bits
                <$> V.generateM
                  (pagesFor bits count)
                  (\index -> do
                     mine <- MUV.unsafeRead owned index
                     if mine
                       then MV.unsafeRead pages index >>= U.unsafeFreeze
                       else
                         if index >= V.length basePages
                           -- A clean slot past the base geometry still holds
                           -- the zero-length sentinel; publish allocated
                           -- storage instead, matching the flat arena's
                           -- answer to an unwritten tail.
                           then MUV.new (pageSize bits) >>= U.unsafeFreeze
                           else pure (V.unsafeIndex basePages index)
                  )
