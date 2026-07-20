module Test.Moonlight.Flow.Property.Subsumption
  ( reuseMaterializationIndexBalance,
    subsumptionProperties,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
    plainRowPatchFromList,
    positivePlainRowPatchRows,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Test.QuickCheck
  ( Property,
    (===),
  )
import Test.Moonlight.Flow.Property.Runtime.BranchSharing qualified as RuntimeBranchSharing
import Test.Moonlight.Flow.Property.Runtime.FactorReuse qualified as RuntimeFactorReuse
import Test.Moonlight.Flow.Property.Runtime.PlanReuse qualified as RuntimePlanReuse
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- Proves the materialization-index algebra in miniature: summed installed
-- patches equal the final materialized rows. Runtime-specific index tests plug
-- into this same invariant instead of asserting incidental insertion state.
reuseMaterializationIndexBalance :: Property
reuseMaterializationIndexBalance =
  let rowA :: RowTupleKey
      rowA = tupleKeyFromRepKeys [RepKey 1]
      rowB :: RowTupleKey
      rowB = tupleKeyFromRepKeys [RepKey 2]
      installedMaterializations :: [RowDelta]
      installedMaterializations =
        [ plainRowPatchFromList [(rowA, MultiplicityChange 2)],
          plainRowPatchFromList [(rowA, MultiplicityChange (-1)), (rowB, MultiplicityChange 4)]
        ]
      materialized :: RowDelta
      materialized = foldr composePlainRowPatch (plainRowPatchFromList []) installedMaterializations
   in positivePlainRowPatchRows materialized
        === Map.fromList [(rowA, Multiplicity 1), (rowB, Multiplicity 4)]

subsumptionProperties :: TestTree
subsumptionProperties =
  testGroup
    "subsumption"
    [ testProperty "reuse materialization index balance" reuseMaterializationIndexBalance,
      RuntimeFactorReuse.spec,
      RuntimePlanReuse.spec,
      RuntimeBranchSharing.spec
    ]
