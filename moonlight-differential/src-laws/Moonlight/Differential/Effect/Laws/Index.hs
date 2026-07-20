module Moonlight.Differential.Effect.Laws.Index
  ( lawBundles,
  )
where

import Moonlight.Differential.Effect.Harness.Index qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..), lawName)
import Moonlight.Pale.Test.LawSuite (LawBundle, hUnitLaw, renderedLawBundle)

lawBundles :: [LawBundle String]
lawBundles =
  [ renderedLawBundle
      "index"
      [ hUnitLaw (lawName RowSetsAgreeWithIntSet) Harness.rowSubstrateSetAlgebra,
        hUnitLaw (lawName RowSubstratesCanonicalizeNonnegative) Harness.rowSubstratesCanonicalizeNegativeRawIds,
        hUnitLaw (lawName TupleWordConversionRejectsNegatives) Harness.tupleWordConversionRejectsNegativeRepresentatives,
        hUnitLaw (lawName RowSetDenseInsertDenotesIntSetInsert) Harness.rowSetDenseInsertDenotesIntSetInsert,
        hUnitLaw (lawName IndexedRowsValueBucketsDenoteLiveBindings) Harness.indexedRowsValueBucketsDenoteBindings,
        hUnitLaw (lawName IndexedRegistryReverseAxesGlued) Harness.indexedRegistryUpsertDeleteLaws,
        hUnitLaw (lawName IndexedRegistryTypedCorruptionObstructions) Harness.indexedRegistryValidationReportsObstructions,
        hUnitLaw (lawName BatchProjectsIntoIndexedRows) Harness.batchProjectsIntoIndexedRows,
        hUnitLaw (lawName TraceProjectionAccumulatesDuplicateRowsAlgebraically) Harness.traceProjectionAccumulatesDuplicatePhysicalRows,
        hUnitLaw (lawName ProjectedRowDeltasMatchSnapshotProjection) Harness.projectedRowsDeltaMaintainsSnapshot,
        hUnitLaw (lawName RelationAdvanceMaintainsViewsAtomically) Harness.relationAdvanceMaintainsViews
      ]
  ]
