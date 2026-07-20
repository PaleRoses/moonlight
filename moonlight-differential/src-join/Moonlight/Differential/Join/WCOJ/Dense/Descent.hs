{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Differential.Join.WCOJ.Dense.Descent
  ( descendSlots,
  )
where

import Control.Monad.ST (ST)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

descendSlots ::
  frame ->
  IntSet ->
  acc ->
  (frame -> IntSet -> ST s (Maybe (Int, IntSet))) ->
  (frame -> ST s Int) ->
  (frame -> Int -> Int -> Int -> ST s Bool) ->
  (frame -> Int -> Int -> ST s ()) ->
  (frame -> acc -> ST s acc) ->
  ST s acc
descendSlots frame unbound initial choose markFrame bind rollback leaf =
  go unbound initial
  where
    go unboundKeys !acc =
      choose frame unboundKeys >>= \case
        Nothing ->
          leaf frame acc
        Just (_, dom)
          | IntSet.null dom ->
              pure acc
        Just (slotKey, dom) ->
          let !unboundNext = IntSet.delete slotKey unboundKeys
           in IntSet.foldl'
                ( \action repKey ->
                    action >>= \ !acc0 -> do
                      mark <- markFrame frame
                      ok <- bind frame slotKey repKey mark
                      if not ok
                        then pure acc0
                        else do
                          acc1 <- go unboundNext acc0
                          rollback frame slotKey mark
                          pure acc1
                )
                (pure acc)
                dom
{-# INLINE descendSlots #-}
