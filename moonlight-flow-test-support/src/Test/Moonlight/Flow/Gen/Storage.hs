{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Gen.Storage
  ( IndexedRowsOp (..),
    genAtomRow,
    genIndexedRowsOpStream,
    genRowIdList,
  )
where

import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
  )
import Test.QuickCheck
  ( Gen,
    chooseInt,
    listOf,
    vectorOf,
  )

data IndexedRowsOp
  = InsertRow !RowTupleKey !Int
  | DeleteRow !RowTupleKey
  | SetPayload !RowTupleKey !Int
  deriving stock (Eq, Show)

genAtomRow :: Int -> Gen RowTupleKey
genAtomRow width =
  tupleKeyFromInts <$> vectorOf (max 0 width) (chooseInt (0, 32))

genIndexedRowsOpStream :: Int -> Int -> Gen [IndexedRowsOp]
genIndexedRowsOpStream width maxOps = do
  opCount <- chooseInt (0, max 0 maxOps)
  vectorOf opCount genOp
  where
    genOp = do
      tag <- chooseInt (0, 2 :: Int)
      rowValue <- genAtomRow width
      payload <- chooseInt (-1024, 1024)
      pure $ case tag of
        0 -> InsertRow rowValue payload
        1 -> DeleteRow rowValue
        _ -> SetPayload rowValue payload

genRowIdList :: Gen [Int]
genRowIdList =
  fmap (fmap (`mod` 8192) . filter (>= 0)) (listOf (chooseInt (-1024, 16384)))
