{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Delta.Patch.Internal.Cursor
  ( AscCursor (..),
    ascCursor,
    ascAdvance,
    Cursor (..),
    cursor,
    fromAsc,
    advanceRow,
    advancePage,
    currentKey,
    currentRow,
    beforeMaybe,
    afterMaybe,
  )
where

import Data.Bits (testBit)
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict (Map)
import Data.Word (Word64)
import Moonlight.Delta.Patch.Internal.Page
  ( ColumnView (..),
    columnView,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( Endpoint (..),
    Page (..),
    ValueColumn,
    pageKeyAt,
    valueColumnAt,
  )
import Prelude

data AscStack key value
  = AscStackEnd
  | AscStackNode !key value !(Map key value) !(AscStack key value)

data AscCursor key value
  = AscEnd
  | AscCursor !key value !(Map key value) !(AscStack key value)

ascCursor :: forall key value. Map key value -> AscCursor key value
ascCursor tree =
  descend tree AscStackEnd
  where
    descend :: Map key value -> AscStack key value -> AscCursor key value
    descend entries stack =
      case entries of
        MapInternal.Tip ->
          popStack stack
        MapInternal.Bin _ key value left right ->
          descend left (AscStackNode key value right stack)

    popStack :: AscStack key value -> AscCursor key value
    popStack stack =
      case stack of
        AscStackEnd ->
          AscEnd
        AscStackNode key value right rest ->
          AscCursor key value right rest
{-# INLINE ascCursor #-}

ascAdvance :: forall key value. AscCursor key value -> AscCursor key value
ascAdvance cursor =
  case cursor of
    AscEnd ->
      AscEnd
    AscCursor _key _value right stack ->
      descend right stack
  where
    descend :: Map key value -> AscStack key value -> AscCursor key value
    descend entries remaining =
      case entries of
        MapInternal.Tip ->
          popStack remaining
        MapInternal.Bin _ key value left nextRight ->
          descend left (AscStackNode key value nextRight remaining)

    popStack :: AscStack key value -> AscCursor key value
    popStack remaining =
      case remaining of
        AscStackEnd ->
          AscEnd
        AscStackNode key value nextRight rest ->
          AscCursor key value nextRight rest
{-# INLINE ascAdvance #-}

data Cursor key value
  = CursorEnd
  | Cursor
      !key
      !(Page key value)
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Word64
      !(ValueColumn value)
      {-# UNPACK #-} !Word64
      !(ValueColumn value)
      !(AscCursor key (Page key value))

cursor :: Map key (Page key value) -> Cursor key value
cursor =
  fromAsc . ascCursor
{-# INLINE cursor #-}

fromAsc :: AscCursor key (Page key value) -> Cursor key value
fromAsc cursor =
  case cursor of
    AscEnd ->
      CursorEnd
    AscCursor maximumKey page _right _stack ->
      case
          ( columnView (pageCount page) (pageBeforeColumn page),
            columnView (pageCount page) (pageAfterColumn page)
          )
        of
          (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
            Cursor
              maximumKey
              page
              0
              0
              0
              beforeMask
              beforeValues
              afterMask
              afterValues
              (ascAdvance cursor)
{-# INLINE fromAsc #-}

advancePage :: Cursor key value -> Cursor key value
advancePage cursor =
  case cursor of
    CursorEnd ->
      CursorEnd
    Cursor _ _ _ _ _ _ _ _ _ remainingPages ->
      fromAsc remainingPages
{-# INLINE advancePage #-}

advanceRow :: Cursor key value -> Cursor key value
advanceRow cursor =
  case cursor of
    CursorEnd ->
      CursorEnd
    Cursor maximumKey page logicalIndex beforePackedIndex afterPackedIndex beforeMask beforeValues afterMask afterValues remainingPages
      | logicalIndex + 1 == pageCount page ->
          fromAsc remainingPages
      | otherwise ->
          let !nextBeforePackedIndex =
                if testBit beforeMask logicalIndex
                  then beforePackedIndex + 1
                  else beforePackedIndex
              !nextAfterPackedIndex =
                if testBit afterMask logicalIndex
                  then afterPackedIndex + 1
                  else afterPackedIndex
           in Cursor
                maximumKey
                page
                (logicalIndex + 1)
                nextBeforePackedIndex
                nextAfterPackedIndex
                beforeMask
                beforeValues
                afterMask
                afterValues
                remainingPages
{-# INLINE advanceRow #-}


currentKey :: Cursor key value -> Maybe key
currentKey cursor =
  case cursor of
    CursorEnd ->
      Nothing
    Cursor maximumKey page logicalIndex _ _ _ _ _ _ _ ->
      Just (pageKeyAt maximumKey page logicalIndex)
{-# INLINE currentKey #-}

currentRow :: Cursor key value -> Maybe (key, Endpoint value, Endpoint value)
currentRow cursor =
  case cursor of
    CursorEnd ->
      Nothing
    Cursor maximumKey page logicalIndex beforePackedIndex afterPackedIndex beforeMask beforeValues afterMask afterValues _ ->
      Just
        ( pageKeyAt maximumKey page logicalIndex,
          if testBit beforeMask logicalIndex
            then EndpointPresent (valueColumnAt beforeValues beforePackedIndex)
            else EndpointAbsent,
          if testBit afterMask logicalIndex
            then EndpointPresent (valueColumnAt afterValues afterPackedIndex)
            else EndpointAbsent
        )
{-# INLINE currentRow #-}


beforeMaybe :: Cursor key value -> Maybe value
beforeMaybe cursor =
  case cursor of
    CursorEnd ->
      Nothing
    Cursor _ _ logicalIndex packedIndex _ mask values _ _ _ ->
      if testBit mask logicalIndex
        then Just (valueColumnAt values packedIndex)
        else Nothing
{-# INLINE beforeMaybe #-}

afterMaybe :: Cursor key value -> Maybe value
afterMaybe cursor =
  case cursor of
    CursorEnd ->
      Nothing
    Cursor _ _ logicalIndex _ packedIndex _ _ mask values _ ->
      if testBit mask logicalIndex
        then Just (valueColumnAt values packedIndex)
        else Nothing
{-# INLINE afterMaybe #-}
