module Test.Moonlight.Flow.Property.Model
  ( tupleCanonicalizationLaws,
    rowPatchCompositionLaws,
    rowDeltaPositiveNegativeSplitLaws,
    restrictTupleKeyDistributes,
    restrictionChainFunctorial,
    scopeMonoidLaws,
    modelProperties,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Row.Delta
  ( RowDelta,
    dropEmptyRowDeltas,
    positiveMultiplicityValue,
    rowDeltaNegativePart,
    rowDeltaNull,
    rowDeltaPositivePart,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
    mapPlainRowPatchRows,
    plainRowPatchFromList,
    plainRowPatchChangeMap
  )
import Moonlight.Flow.Model.Scope
  ( relationalScopeFromSets,
    relationalScopeNull,
    scopeDeps,
    scopeImpacted,
    scopeResults,
    scopeRoots,
    scopeTopo,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    coerceTupleKey,
    restrictTupleKey,
    tupleKeyClassKeys,
    tupleKeyFromInts,
    tupleKeyToInts,
    tupleKeyWidth,
  )
import Test.Moonlight.Flow.Gen.Model
  ( genRestrictionMap,
    genRowPatch,
  )
import Test.Moonlight.Flow.Oracle.Model
  ( oracleComposeRows,
    oracleRestrictRows,
  )
import Test.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    listOf,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck (testProperty)

-- Proves semantic-surface invariant: canonical construction chooses exactly
-- one representation for tuple contents, and role coercion preserves content.
tupleCanonicalizationLaws :: Property
tupleCanonicalizationLaws =
  forAll (listOf (chooseInt (0, 16))) $ \values ->
    let row =
          tupleKeyFromInts values :: RowTupleKey
        coerced =
          coerceTupleKey row :: RowTupleKey
     in conjoin
          [ tupleKeyToInts row === values,
            tupleKeyWidth row === length values,
            tupleKeyToInts coerced === tupleKeyToInts row
          ]

-- Proves semantic-surface invariant: rowPatch composition has empty identity
-- and agrees with a direct Map union-by-multiplicity oracle.
rowPatchCompositionLaws :: Property
rowPatchCompositionLaws =
  forAll genRowPatch $ \leftRows ->
    forAll genRowPatch $ \middleRows ->
      forAll genRowPatch $ \rightRows ->
        let emptyPatch = plainRowPatchFromList []
            composed = composePlainRowPatch (composePlainRowPatch leftRows middleRows) rightRows
            recomposed = composePlainRowPatch leftRows (composePlainRowPatch middleRows rightRows)
            oracle = oracleComposeRows (plainRowPatchChangeMap leftRows) (plainRowPatchChangeMap middleRows)
            expectedNonEmptyRows =
              if rowDeltaNull leftRows
                then IntMap.empty
                else IntMap.singleton 1 leftRows
         in conjoin
              [ counterexample "associative" (composed === recomposed),
                counterexample "left identity" (composePlainRowPatch emptyPatch leftRows === leftRows),
                counterexample "right identity" (composePlainRowPatch leftRows emptyPatch === leftRows),
                counterexample "Map oracle" (plainRowPatchChangeMap (composePlainRowPatch leftRows middleRows) === oracle),
                counterexample
                  "empty row deltas pruned"
                  ( dropEmptyRowDeltas (IntMap.fromList [(0, emptyPatch), (1, leftRows)])
                      === expectedNonEmptyRows
                  )
              ]

rowDeltaPositiveNegativeSplitLaws :: Assertion
rowDeltaPositiveNegativeSplitLaws =
  let rowA =
        tupleKeyFromInts [1, 2] :: RowTupleKey
      rowB =
        tupleKeyFromInts [3, 4] :: RowTupleKey
      rowC =
        tupleKeyFromInts [5, 6] :: RowTupleKey
      delta =
        plainRowPatchFromList
          [ (rowA, MultiplicityChange 2),
            (rowB, MultiplicityChange (-3)),
            (rowC, MultiplicityChange 0)
          ]
   in do
        fmap positiveMultiplicityValue (rowDeltaPositivePart delta)
          @?= Map.fromList [(rowA, Multiplicity 2)]
        fmap positiveMultiplicityValue (rowDeltaNegativePart delta)
          @?= Map.fromList [(rowB, Multiplicity 3)]

-- Proves semantic-surface invariant: restrictTupleKeyPatch distributes over
-- composition and matches a direct tuple-by-tuple materialization oracle.
restrictTupleKeyDistributes :: Property
restrictTupleKeyDistributes =
  forAll genRowPatch $ \leftRows ->
    forAll genRowPatch $ \rightRows ->
      forAll genRestrictionMap $ \restriction ->
        let composed = composePlainRowPatch leftRows rightRows
            restrictedComposed = restrictRowDelta restriction composed
            composedRestricted =
              composePlainRowPatch
                (restrictRowDelta restriction leftRows)
                (restrictRowDelta restriction rightRows)
            oracle = oracleRestrictRows restriction (plainRowPatchChangeMap composed)
         in conjoin
              [ restrictedComposed === composedRestricted,
                plainRowPatchChangeMap restrictedComposed === oracle
              ]

-- Proves semantic-surface invariant: a chain of restrictions is functorial;
-- stepwise local restriction glues to the composed restriction on the observed
-- row-value cover.
restrictionChainFunctorial :: Property
restrictionChainFunctorial =
  forAll genRowPatch $ \rows ->
    forAll genRestrictionMap $ \first ->
      forAll genRestrictionMap $ \second ->
        let stepwise = restrictRowDelta second (restrictRowDelta first rows)
            composed = restrictRowDelta (composeRestrictions (rowValueDomain rows first second) first second) rows
         in plainRowPatchChangeMap stepwise === plainRowPatchChangeMap composed

scopeMonoidLaws :: Assertion
scopeMonoidLaws =
  let scope =
        relationalScopeFromSets
          (IntSet.fromList [1])
          (IntSet.fromList [2])
          (IntSet.fromList [3])
          (IntSet.fromList [4])
          (IntSet.fromList [5])
      sameScope =
        scope <> scope
   in do
        relationalScopeNull mempty @?= True
        mempty <> scope @?= scope
        scope <> mempty @?= scope
        scopeDeps sameScope @?= scopeDeps scope
        scopeTopo sameScope @?= scopeTopo scope
        scopeRoots sameScope @?= scopeRoots scope
        scopeResults sameScope @?= scopeResults scope
        scopeImpacted sameScope @?= scopeImpacted scope

modelProperties :: TestTree
modelProperties =
  testGroup
    "model"
    [ testProperty "tuple canonicalization round trips" tupleCanonicalizationLaws,
      testProperty "rowPatch composition associative with identity" rowPatchCompositionLaws,
      testCase "rowDelta positive/negative split" rowDeltaPositiveNegativeSplitLaws,
      testProperty "restrictTupleKeyPatch distributes over compose" restrictTupleKeyDistributes,
      testProperty "restriction chain functoriality" restrictionChainFunctorial,
      testCase "scope monoid identity and idempotent channels" scopeMonoidLaws
    ]

rowValueDomain :: RowDelta -> IntMap RepKey -> IntMap RepKey -> IntSet.IntSet
rowValueDomain rows first second =
  Map.foldlWithKey'
    (\acc rowValue _multiplicity -> IntSet.union acc (tupleKeyClassKeys rowValue))
    (IntSet.union (IntMap.keysSet first) (IntMap.keysSet second))
    (plainRowPatchChangeMap rows)

restrictRowDelta :: IntMap RepKey -> RowDelta -> RowDelta
restrictRowDelta targetClasses =
  mapPlainRowPatchRows (restrictTupleKey targetClasses)
{-# INLINE restrictRowDelta #-}

composeRestrictions :: IntSet.IntSet -> IntMap RepKey -> IntMap RepKey -> IntMap RepKey
composeRestrictions domain first second =
  IntMap.fromList
    [ (source, target)
    | source <- IntSet.toAscList domain,
      let target = restrictRepKey second (restrictRepKey first (RepKey source)),
      target /= RepKey source
    ]

restrictRepKey :: IntMap RepKey -> RepKey -> RepKey
restrictRepKey restriction key@(RepKey source) =
  IntMap.findWithDefault key source restriction
