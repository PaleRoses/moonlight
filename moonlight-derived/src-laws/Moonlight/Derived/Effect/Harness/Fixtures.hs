-- | Fixtures, generators, and comparators backing the moonlight-derived law harness.
module Moonlight.Derived.Effect.Harness.Fixtures
  ( WedgeComplexCase (..)
  , LawfulMapFamily (..)
  , SparseDigestCacheCoherenceReport (..)
  , sampleChainPoset
  , sampleDiamondPoset
  , singletonPosetFixture
  , twoPointChainPosetFixture
  , sampleTwoPointDiscrete
  , sampleSingleton
  , scalarIdentityFixture
  , nonCommutingWitness
  , zeroSourceFixture
  , zeroTargetFixture
  , sampleZeroComplex
  , singletonInjectiveAt
  , injectiveOnPosetAt
  , posetFunctor
  , nodes
  , inflatedWedgeComplexCaseGen
  , shiftedWedgeComplexCaseGen
  , lawfulMapFamilyGen
  , plainWedgeComplexCases
  , trailingContractiblePointCase
  , wedgeComplex
  , coupledContractibleDifferential
  , scalarIdentityComponents
  , derivedMapForFamily
  , acceptsMapFamily
  , rejectsPerturbedNonCommutingComponent
  , compareDims
  , compareScalar
  , reindexedDims
  , eulerCharacteristic
  , vanishOutside
  , zeroPaddedHypercohomology
  , microsupportResult
  , withSingletonPoset
  , withDiamondPoset
  , degreeWindow
  , axisFromMultiplicities
  , seededBlockedMatrix
  , seededDenseBlock
  , sparseShape
  , sparseCoordinates
  , sparseDigestCacheCoherenceReport
  , restrictedSparseDifferentials
  , restrictedSupports
  , restrictedComplex
  , restrictedNodeParameters
  ) where

import Data.Either (isRight)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Core (MoonlightError)
import Moonlight.Category (FinObjectId (..))
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureToMoonlightError
  )
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDims
  , hypercohomologyVanishesWith
  )
import Moonlight.Derived.Pure.Morse.Support
  ( fiberSubsets
  , microSupportBangOn
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( gf2PackedRankBackend
  )
import Moonlight.Derived.Pure.LinAlg.Rank
  ( precomputeStableSparseRankCache
  , rankSparseWith
  , stableSparseDigestRankBackend
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..)
  , Derived (..)
  , complexObjectAxes
  , getDerived
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.DerivedMap
  ( DerivedMap
  , derivedMapComponents
  , identityMap
  , mkDerivedMapChecked
  , zeroMap
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , GroupedAxis
  , SparseMat
  , SparseMatrixEntry (..)
  , axisMultiplicity
  , blockedToSparseMat
  , canonicalSparseMatEntries
  , groupedAxisOrder
  , setBlock
  , zeroBlocked
  , sparseMatCols
  , sparseMatRows
  , fromLabels
  )
import Moonlight.Derived.Pure.Site.Microsupport (LocalClosed, mkLocalClosed)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , categoryFromOrderClosure
  , mkDerivedPosetFunctor
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.LinAlg (GF2)
import Test.Tasty.QuickCheck qualified as QC

data WedgeComplexCase = WedgeComplexCase
  { wccCircleCount :: Int
  , wccContractibleCount :: Int
  } deriving stock (Eq, Show)

data SparseDigestCacheCoherenceReport = SparseDigestCacheCoherenceReport
  { sdccRawRanks :: [Int]
  , sdccCachedRanks :: [Int]
  , sdccRawVanishes :: [Bool]
  , sdccCachedVanishes :: [Bool]
  } deriving stock (Eq, Show)

data LawfulMapFamily
  = IdentityMapFamily
  | ZeroMapFamily
  | ScalarIdentityMapFamily [Bool]
  deriving stock (Eq, Show)

sampleChainPoset :: Either DerivedFailure DerivedPoset
sampleChainPoset =
  mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]

sampleDiamondPoset :: Either DerivedFailure DerivedPoset
sampleDiamondPoset =
  mkDerivedPosetFromOrderEdges
    [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3]
    [(FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 2), (FinObjectId 1, FinObjectId 3), (FinObjectId 2, FinObjectId 3)]

posetFunctor :: DerivedPoset -> DerivedPoset -> (FinObjectId -> FinObjectId) -> Either DerivedFailure DerivedPosetFunctor
posetFunctor sourcePoset targetPoset mapNode =
  case
      mkDerivedPosetFunctor
        sourcePoset
        targetPoset
        ( Map.fromList
            [ (FinObjectId sourceKey, FinObjectId targetKey)
            | sourceNode@(FinObjectId sourceKey) <- Vector.toList (derivedPosetNodes sourcePoset)
            , let FinObjectId targetKey = mapNode sourceNode
            ]
        )
    of
      Left validationFailure -> Left (DerivedFunctorApplicationFailed (show validationFailure))
      Right functorValue -> Right functorValue

singletonPosetFixture :: DerivedPoset
singletonPosetFixture =
  DerivedPoset
    { derivedPosetCategory = categoryFromOrderClosure [FinObjectId 0] (IntMap.singleton 0 (IntSet.singleton 0))
    , derivedPosetNodes = Vector.singleton (FinObjectId 0)
    , derivedPosetUpper = IntMap.singleton 0 (IntSet.singleton 0)
    , derivedPosetLower = IntMap.singleton 0 (IntSet.singleton 0)
    , derivedPosetCoversUp = IntMap.singleton 0 IntSet.empty
    , derivedPosetTopoDesc = Vector.singleton (FinObjectId 0)
    , derivedPosetTopoAsc = Vector.singleton (FinObjectId 0)
    }

twoPointChainPosetFixture :: DerivedPoset
twoPointChainPosetFixture =
  DerivedPoset
    { derivedPosetCategory = categoryFromOrderClosure [FinObjectId 0, FinObjectId 1] (IntMap.fromList [(0, IntSet.fromList [0, 1]), (1, IntSet.singleton 1)])
    , derivedPosetNodes = Vector.fromList [FinObjectId 0, FinObjectId 1]
    , derivedPosetUpper = IntMap.fromList [(0, IntSet.fromList [0, 1]), (1, IntSet.singleton 1)]
    , derivedPosetLower = IntMap.fromList [(0, IntSet.singleton 0), (1, IntSet.fromList [0, 1])]
    , derivedPosetCoversUp = IntMap.fromList [(0, IntSet.singleton 1), (1, IntSet.empty)]
    , derivedPosetTopoDesc = Vector.fromList [FinObjectId 1, FinObjectId 0]
    , derivedPosetTopoAsc = Vector.fromList [FinObjectId 0, FinObjectId 1]
    }

shiftedWedgeComplexCaseGen :: QC.Gen (Int, WedgeComplexCase)
shiftedWedgeComplexCaseGen =
  (,)
    <$> QC.chooseInt (-3, 3)
    <*> inflatedWedgeComplexCaseGen

lawfulMapFamilyGen :: QC.Gen LawfulMapFamily
lawfulMapFamilyGen =
  QC.oneof
    [ pure IdentityMapFamily
    , pure ZeroMapFamily
    , fmap (ScalarIdentityMapFamily . Vector.toList) (Vector.replicateM 3 QC.arbitrary)
    ]

acceptsMapFamily :: LawfulMapFamily -> Bool
acceptsMapFamily =
  isRight . derivedMapForFamily

derivedMapForFamily :: LawfulMapFamily -> Either DerivedFailure (DerivedMap GF2)
derivedMapForFamily familyValue =
  case familyValue of
    IdentityMapFamily ->
      mkDerivedMapChecked scalarIdentityFixture scalarIdentityFixture (derivedMapComponents (identityMap scalarIdentityFixture))
    ZeroMapFamily ->
      mkDerivedMapChecked zeroSourceFixture zeroTargetFixture (derivedMapComponents (zeroMap zeroSourceFixture zeroTargetFixture))
    ScalarIdentityMapFamily switches ->
      mkDerivedMapChecked scalarIdentityFixture scalarIdentityFixture (scalarIdentityComponents switches)

scalarIdentityComponents :: [Bool] -> IntMap.IntMap (BlockedMat GF2)
scalarIdentityComponents switches =
  IntMap.fromList
    ( zipWith3
        scalarComponent
        [0 ..]
        (complexObjectAxes (getDerived scalarIdentityFixture))
        (take 3 (switches <> repeat False))
    )
  where
    identityComponents =
      derivedMapComponents (identityMap scalarIdentityFixture)

    scalarComponent degreeValue axisValue enabled =
      ( degreeValue
      , if enabled
          then IntMap.findWithDefault (zeroBlocked axisValue axisValue) degreeValue identityComponents
          else zeroBlocked axisValue axisValue
      )

rejectsPerturbedNonCommutingComponent :: Bool
rejectsPerturbedNonCommutingComponent =
  case mkDerivedMapChecked nonCommutingWitness nonCommutingWitness perturbedComponents of
    Left _ -> True
    Right _ -> False
  where
    identityComponents =
      derivedMapComponents (identityMap nonCommutingWitness)

    initialAxis =
      case complexObjectAxes (getDerived nonCommutingWitness) of
        axisValue : _ -> axisValue
        [] -> fromLabels Vector.empty

    perturbedComponents =
      IntMap.insert 0 (zeroBlocked initialAxis initialAxis) identityComponents

zeroSourceFixture :: Derived GF2
zeroSourceFixture =
  either (const scalarIdentityFixture) id (singletonInjectiveAt 0 1)

zeroTargetFixture :: Derived GF2
zeroTargetFixture =
  either (const scalarIdentityFixture) id (singletonInjectiveAt 0 2)

scalarIdentityFixture :: Derived GF2
scalarIdentityFixture =
  Derived
    singletonPosetFixture
    ( InjectiveComplex
        0
        ( Vector.fromList
            [ zeroBlocked scalarAxis scalarAxis
            , zeroBlocked scalarAxis scalarAxis
            ]
        )
    )
  where
    scalarAxis =
      axisFromMultiplicities [(FinObjectId 0, 1)]

nonCommutingWitness :: Derived GF2
nonCommutingWitness =
  Derived
    twoPointChainPosetFixture
    ( InjectiveComplex
        0
        ( Vector.singleton
            ( setBlock
                (FinObjectId 0)
                (FinObjectId 1)
                (DenseMat 1 1 (Vector.singleton (Vector.singleton 1)))
                (zeroBlocked targetAxis sourceAxis)
            )
        )
    )
  where
    sourceAxis =
      axisFromMultiplicities [(FinObjectId 1, 1)]

    targetAxis =
      axisFromMultiplicities [(FinObjectId 0, 1)]

singletonInjectiveAt :: Int -> Int -> Either MoonlightError (Derived GF2)
singletonInjectiveAt startDegree multiplicityValue =
  mkNormalizedDerived
    singletonPosetFixture
    InjectiveComplex
      { icStart = startDegree
      , icDiffs =
          Vector.singleton
            ( zeroBlocked
                (fromLabels Vector.empty)
                (axisFromMultiplicities [(FinObjectId 0, multiplicityValue)])
            )
      }

compareDims ::
  String ->
  Either MoonlightError (IntMap.IntMap Int) ->
  Either MoonlightError (IntMap.IntMap Int) ->
  QC.Property
compareDims context leftResult rightResult =
  case (leftResult, rightResult) of
    (Right leftDims, Right rightDims) ->
      QC.counterexample (show (context, leftDims, rightDims)) (leftDims == rightDims)
    results ->
      QC.counterexample (show (context, results)) False

compareScalar ::
  (Eq a, Show a) =>
  String ->
  Either MoonlightError a ->
  Either MoonlightError a ->
  QC.Property
compareScalar context leftResult rightResult =
  case (leftResult, rightResult) of
    (Right leftValue, Right rightValue) ->
      QC.counterexample (show (context, leftValue, rightValue)) (leftValue == rightValue)
    results ->
      QC.counterexample (show (context, results)) False

reindexedDims :: Int -> IntMap.IntMap Int -> IntMap.IntMap Int
reindexedDims shiftAmount =
  IntMap.mapKeys (+ shiftAmount)

eulerCharacteristic :: Derived GF2 -> Either MoonlightError Int
eulerCharacteristic =
  fmap
    ( sum
        . fmap
          (\(degreeValue, dimensionValue) -> if even degreeValue then dimensionValue else negate dimensionValue)
        . IntMap.toList
    )
    . hypercohomologyDims

vanishOutside :: String -> (Int -> Bool) -> Derived GF2 -> QC.Property
vanishOutside context outsideWindow derivedValue =
  case hypercohomologyDims derivedValue of
    Right dims ->
      QC.counterexample
        (show (context, dims))
        (all (== 0) [dimensionValue | (degreeValue, dimensionValue) <- IntMap.toList dims, outsideWindow degreeValue])
    Left err ->
      QC.counterexample (show (context, err)) False

inflatedWedgeComplexCaseGen :: QC.Gen WedgeComplexCase
inflatedWedgeComplexCaseGen =
  WedgeComplexCase
    <$> QC.chooseInt (1, 5)
    <*> QC.chooseInt (1, 5)

plainWedgeComplexCases :: [WedgeComplexCase]
plainWedgeComplexCases =
  fmap (`WedgeComplexCase` 0) [1 .. 5]

trailingContractiblePointCase :: WedgeComplexCase
trailingContractiblePointCase =
  WedgeComplexCase 0 1

withSingletonPoset :: String -> (DerivedPoset -> QC.Property) -> QC.Property
withSingletonPoset context continue =
  case sampleSingleton of
    Left err ->
      QC.counterexample (show (context, err)) False
    Right posetValue ->
      continue posetValue

withDiamondPoset :: String -> (DerivedPoset -> QC.Property) -> QC.Property
withDiamondPoset context continue =
  case sampleDiamondPoset of
    Left err ->
      QC.counterexample (show (context, err)) False
    Right posetValue ->
      continue posetValue

-- | A lawful one-object complex over an arbitrary poset: multiplicities on
-- the given nodes, zero differential (trivially site-lawful), normalized at
-- construction. This is the multi-node counterpart of 'singletonInjectiveAt'
-- that lets the triangulated/Verdier/tensor laws leave the one-point site.
injectiveOnPosetAt :: DerivedPoset -> Int -> [(FinObjectId, Int)] -> Either MoonlightError (Derived GF2)
injectiveOnPosetAt posetValue startDegree multiplicities =
  mkNormalizedDerived
    posetValue
    InjectiveComplex
      { icStart = startDegree
      , icDiffs =
          Vector.singleton
            ( zeroBlocked
                (fromLabels Vector.empty)
                (axisFromMultiplicities multiplicities)
            )
      }

wedgeComplex :: WedgeComplexCase -> InjectiveComplex GF2
wedgeComplex WedgeComplexCase {wccCircleCount, wccContractibleCount}
  | wccContractibleCount <= 0 =
      InjectiveComplex
        0
        ( Vector.singleton
            (zeroBlocked middleAxis baseAxis)
        )
  | otherwise =
      InjectiveComplex
        0
        ( Vector.fromList
            [ zeroBlocked middleAxis baseAxis
            , setBlock
                (FinObjectId 0)
                (FinObjectId 0)
                (coupledContractibleDifferential wccCircleCount wccContractibleCount)
                (zeroBlocked topAxis middleAxis)
            ]
        )
  where
    baseAxis =
      axisFromMultiplicities [(FinObjectId 0, 1)]

    middleAxis =
      axisFromMultiplicities [(FinObjectId 0, wccCircleCount + wccContractibleCount)]

    topAxis =
      axisFromMultiplicities [(FinObjectId 0, wccContractibleCount)]

coupledContractibleDifferential :: Int -> Int -> DenseMat GF2
coupledContractibleDifferential circleCount contractibleCount =
  DenseMat
    contractibleCount
    (circleCount + contractibleCount)
    ( Vector.generate
        contractibleCount
        ( \rowIndex ->
            Vector.generate
              (circleCount + contractibleCount)
              (contractibleCoefficient rowIndex)
        )
    )
  where
    contractibleCoefficient rowIndex columnIndex
      | columnIndex < circleCount =
          if even (rowIndex + columnIndex) then 1 else 0
      | columnIndex == circleCount + rowIndex =
          1
      | otherwise =
          0

zeroPaddedHypercohomology ::
  DerivedPoset ->
  [Int] ->
  InjectiveComplex GF2 ->
  Either MoonlightError [(Int, Int)]
zeroPaddedHypercohomology posetValue degrees complexValue =
  fmap
    ( \dims ->
        fmap
          (\degreeValue -> (degreeValue, IntMap.findWithDefault 0 degreeValue dims))
          degrees
    )
    (hypercohomologyDims (Derived posetValue complexValue))

microsupportResult ::
  DerivedPoset ->
  InjectiveComplex GF2 ->
  Either MoonlightError [LocalClosed]
microsupportResult posetValue complexValue = do
  let derivedValue = Derived posetValue complexValue
  identityFunctor <-
    first derivedFailureToMoonlightError (posetFunctor posetValue posetValue id)
  supports <-
    first derivedFailureToMoonlightError
      (fiberSubsets identityFunctor)
  preparedPullbacks <-
    first derivedFailureToMoonlightError
      (traverse (`prepareProperPullback` derivedValue) supports)
  microSupportBangOn preparedPullbacks

degreeWindow :: InjectiveComplex a -> [Int]
degreeWindow InjectiveComplex {icStart, icDiffs} =
  [icStart .. icStart + Vector.length icDiffs]

seededBlockedMatrix :: Int -> BlockedMat GF2
seededBlockedMatrix seedValue =
  foldr
    insertBlock
    (zeroBlocked rowAxis columnAxis)
    [ (rowNode, columnNode)
    | rowNode <- Vector.toList (groupedAxisOrder rowAxis)
    , columnNode <- Vector.toList (groupedAxisOrder columnAxis)
    ]
  where
    rowAxis =
      axisFromMultiplicities
        [ (FinObjectId 0, 1 + seedValue `mod` 3)
        , (FinObjectId 1, 1 + (seedValue `div` 3) `mod` 3)
        , (FinObjectId 2, 1 + (seedValue `div` 9) `mod` 2)
        ]

    columnAxis =
      axisFromMultiplicities
        [ (FinObjectId 0, 1 + (seedValue `div` 5) `mod` 3)
        , (FinObjectId 1, 1 + (seedValue `div` 7) `mod` 3)
        , (FinObjectId 2, 1 + (seedValue `div` 11) `mod` 2)
        ]

    insertBlock (rowNode, columnNode) matrixValue =
      setBlock
        rowNode
        columnNode
        (seededDenseBlock seedValue rowAxis columnAxis rowNode columnNode)
        matrixValue

seededDenseBlock ::
  Int ->
  GroupedAxis ->
  GroupedAxis ->
  FinObjectId ->
  FinObjectId ->
  DenseMat GF2
seededDenseBlock seedValue rowAxis columnAxis rowNode columnNode =
  DenseMat
    rowCount
    columnCount
    ( Vector.generate
        rowCount
        ( \rowIndex ->
            Vector.generate
              columnCount
              ( \columnIndex ->
                  if odd (seedValue + 17 * unFinObjectId rowNode + 31 * unFinObjectId columnNode + 5 * rowIndex + 11 * columnIndex)
                    then 1
                    else 0
              )
        )
    )
  where
    rowCount =
      axisMultiplicity rowAxis rowNode

    columnCount =
      axisMultiplicity columnAxis columnNode

sparseShape :: SparseMat a -> (Int, Int)
sparseShape sparseMatrix =
  (sparseMatRows sparseMatrix, sparseMatCols sparseMatrix)

sparseCoordinates :: (Eq a, Num a) => SparseMat a -> [(Int, Int)]
sparseCoordinates =
  fmap
    (\SparseMatrixEntry {smeRow, smeColumn} -> (smeRow, smeColumn))
    . canonicalSparseMatEntries

sparseDigestCacheCoherenceReport ::
  Int ->
  Either MoonlightError SparseDigestCacheCoherenceReport
sparseDigestCacheCoherenceReport seedValue = do
  supportPoset <-
    first derivedFailureToMoonlightError sampleTwoPointDiscrete
  supports <-
    first derivedFailureToMoonlightError
      (traverse (mkLocalClosed supportPoset) (restrictedSupports seedValue))
  minimizedComplex <-
    minimizeComplex (restrictedComplex seedValue)
  derivedValue <-
    mkNormalizedDerived supportPoset minimizedComplex
  preparedPullbacks <-
    first derivedFailureToMoonlightError
      (traverse (`prepareProperPullback` derivedValue) supports)
  let restrictedDerivedValues =
        fmap properPullback preparedPullbacks
      sparseDifferentials =
        concatMap restrictedSparseDifferentials restrictedDerivedValues
  rankCache <-
    precomputeStableSparseRankCache
      gf2PackedRankBackend
      sparseDifferentials
  let cachedBackend =
        stableSparseDigestRankBackend rankCache gf2PackedRankBackend
  rawRanks <-
    traverse
      (rankSparseWith gf2PackedRankBackend)
      sparseDifferentials
  cachedRanks <-
    traverse
      (rankSparseWith cachedBackend)
      sparseDifferentials
  rawVanishes <-
    traverse
      (hypercohomologyVanishesWith gf2PackedRankBackend)
      restrictedDerivedValues
  cachedVanishes <-
    traverse
      (hypercohomologyVanishesWith cachedBackend)
      restrictedDerivedValues
  Right
    SparseDigestCacheCoherenceReport
      { sdccRawRanks = rawRanks
      , sdccCachedRanks = cachedRanks
      , sdccRawVanishes = rawVanishes
      , sdccCachedVanishes = cachedVanishes
      }

restrictedSparseDifferentials :: Derived GF2 -> [SparseMat GF2]
restrictedSparseDifferentials Derived
  { getDerived = InjectiveComplex {icDiffs}
  } =
  fmap blockedToSparseMat (Vector.toList icDiffs)

restrictedSupports :: Int -> [IntSet.IntSet]
restrictedSupports seedValue =
  [ IntSet.singleton (seedValue `mod` 2)
  , IntSet.singleton ((seedValue + 1) `mod` 2)
  , IntSet.fromList [0, 1]
  ]

restrictedComplex :: Int -> InjectiveComplex GF2
restrictedComplex seedValue =
  InjectiveComplex
    0
    ( Vector.fromList
        [ zeroBlocked middleAxis baseAxis
        , foldr
            insertDifferentialBlock
            (zeroBlocked topAxis middleAxis)
            nodeParameters
        ]
    )
  where
    nodeParameters =
      restrictedNodeParameters seedValue

    baseAxis =
      axisFromMultiplicities
        (fmap (\(nodeValue, _, _) -> (nodeValue, 1)) nodeParameters)

    middleAxis =
      axisFromMultiplicities
        (fmap (\(nodeValue, circleCount, contractibleCount) -> (nodeValue, circleCount + contractibleCount)) nodeParameters)

    topAxis =
      axisFromMultiplicities
        (fmap (\(nodeValue, _, contractibleCount) -> (nodeValue, contractibleCount)) nodeParameters)

    insertDifferentialBlock (nodeValue, circleCount, contractibleCount) differentialValue =
      setBlock
        nodeValue
        nodeValue
        (coupledContractibleDifferential circleCount contractibleCount)
        differentialValue

restrictedNodeParameters :: Int -> [(FinObjectId, Int, Int)]
restrictedNodeParameters seedValue =
  [ (FinObjectId 0, 1 + seedValue `mod` 3, 1 + (seedValue `div` 3) `mod` 3)
  , (FinObjectId 1, 1 + (seedValue `div` 5) `mod` 3, 1 + (seedValue `div` 7) `mod` 3)
  ]

axisFromMultiplicities :: [(FinObjectId, Int)] -> GroupedAxis
axisFromMultiplicities =
  fromLabels
    . Vector.fromList
    . concatMap
      ( \(nodeValue, multiplicityValue) ->
          replicate multiplicityValue nodeValue
      )

sampleZeroComplex :: InjectiveComplex GF2
sampleZeroComplex =
  let axis = fromLabels (Vector.fromList [FinObjectId 0])
   in InjectiveComplex 0 (Vector.singleton (zeroBlocked axis axis))

sampleTwoPointDiscrete :: Either DerivedFailure DerivedPoset
sampleTwoPointDiscrete =
  mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] []

sampleSingleton :: Either DerivedFailure DerivedPoset
sampleSingleton =
  mkDerivedPosetFromOrderEdges [FinObjectId 0] []

nodes :: DerivedPoset -> [FinObjectId]
nodes = Vector.toList . derivedPosetNodes
