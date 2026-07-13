module Moonlight.Derived.Effect.Harness
  ( sampleChainPoset
  , sampleDiamondPoset
  , posetReflexiveLaw
  , posetAntisymmetricLaw
  , posetTransitiveLaw
  , posetUpperLowerDualLaw
  , posetTopoRespectsEdgesLaw
  , matrixIdentityLaw
  , matrixTransposeInvolutionLaw
  , matrixRestrictIdempotentLaw
  , matrixBlockedSparseRepresentationAgreementLaw
  , complexDifferentialSquaresZeroLaw
  , complexNormalizationIdempotentLaw
  , complexMinimizationHypercohomologyInvariantLaw
  , complexMinimizationMicrosupportInvariantLaw
  , complexMinimizationDegreeWindowStableLaw
  , -- | Shifting by n reindexes hypercohomology dimensions by k+n.
    shiftReindexesHypercohomologyLaw
  , -- | Checked derived maps accept commuting families and reject a perturbed square.
    mapSquaresCommuteLaw
  , -- | Mapping cones satisfy Euler additivity at hypercohomology level.
    coneEulerAdditiveLaw
  , -- | Rotating a distinguished triangle preserves the cone invariant data.
    triangleRotationInvariantLaw
  , -- | The cone criterion agrees with hypercohomological acyclicity.
    quasiIsoConeAcyclicLaw
  , -- | Verdier duality squared preserves hypercohomology dimensions over GF2.
    verdierInvolutionInvariantsLaw
  , -- | Tensor-Hom adjunction preserves hypercohomology dimensions on singleton fixtures.
    rHomTensorAdjunctionDimsLaw
  , -- | Truncations vanish outside their half-window; canonical truncations
    -- additionally agree with the input inside it.
    truncationTriangleExactLaw
  , quillenARejectsBadFiberLaw
  , morseSparseDigestCacheCoherenceLaw
  , deterministicFixtureLaw
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
import Moonlight.Derived.Pure.Functor.Tensor
  ( internalHom
  , tensorProduct
  )
import Moonlight.Derived.Pure.Functor.VerdierDual
  ( verdierDualComplex
  )
import Moonlight.Derived.Pure.Gluing.Cone
  ( cone
  , mkTriangleOf
  , quasiIsoCone
  , rotateTriangle
  , triA
  , triC
  )
import Moonlight.Derived.Pure.Gluing.Truncation
  ( canonicalTruncateAtLeast
  , canonicalTruncateAtMost
  , canonicalTruncationPair
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDims
  , hypercohomologyDimsWith
  , hypercohomologyVanishes
  , hypercohomologyVanishesWith
  )
import Moonlight.Derived.Pure.Morse.Support
  ( fiberSubsets
  , microSupportBangOn
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.Functor.QuillenA
  ( QuillenACertificate (..)
  , quillenAMaximumCertificate
  )
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
  , composesToZero
  , firstNonMinimal
  , getDerived
  , mkDerivedChecked
  , mkNormalizedDerived
  , normalizeComplexPresentation
  )
import Moonlight.Derived.Pure.Site.DerivedMap
  ( DerivedMap
  , derivedMapSource
  , derivedMapTarget
  , derivedMapComponents
  , identityMap
  , mkDerivedMapChecked
  , shift
  , stupidTruncateAbove
  , stupidTruncateBelow
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
  , blockAt
  , canonicalSparseMatEntries
  , collapseBlockedDense
  , denseToSparseMat
  , groupedAxisOrder
  , identMat
  , matMulChecked
  , restrictBlocked
  , setBlock
  , zeroBlocked
  , sparseMatCols
  , sparseMatRows
  , transposeMatChecked
  , fromLabels
  )
import Moonlight.Derived.Pure.Site.Microsupport (LocalClosed, mkLocalClosed)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , categoryFromOrderClosure
  , leq
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

posetReflexiveLaw :: Bool
posetReflexiveLaw =
  either (const False) (\posetValue -> all (\nodeValue -> leq posetValue nodeValue nodeValue) (Vector.toList (derivedPosetNodes posetValue))) sampleDiamondPoset

posetAntisymmetricLaw :: Bool
posetAntisymmetricLaw =
  either (const False) check sampleDiamondPoset
  where
    check posetValue =
      and
        [ leftNode == rightNode || not (leq posetValue rightNode leftNode)
        | leftNode <- Vector.toList (derivedPosetNodes posetValue)
        , rightNode <- Vector.toList (derivedPosetNodes posetValue)
        , leq posetValue leftNode rightNode
        ]

posetTransitiveLaw :: Bool
posetTransitiveLaw =
  either (const False) check sampleDiamondPoset
  where
    check posetValue =
      and
        [ (leq posetValue leftNode rightNode && leq posetValue rightNode topNode) ==> leq posetValue leftNode topNode
        | leftNode <- nodes posetValue
        , rightNode <- nodes posetValue
        , topNode <- nodes posetValue
        ]

posetUpperLowerDualLaw :: Bool
posetUpperLowerDualLaw =
  either (const False) check sampleDiamondPoset
  where
    check DerivedPoset {derivedPosetNodes, derivedPosetUpper, derivedPosetLower} =
      and
        [ IntSet.member rightKey (IntMap.findWithDefault IntSet.empty leftKey derivedPosetUpper)
            == IntSet.member leftKey (IntMap.findWithDefault IntSet.empty rightKey derivedPosetLower)
        | FinObjectId leftKey <- Vector.toList derivedPosetNodes
        , FinObjectId rightKey <- Vector.toList derivedPosetNodes
        ]

posetTopoRespectsEdgesLaw :: Bool
posetTopoRespectsEdgesLaw =
  either (const False) check sampleDiamondPoset
  where
    check DerivedPoset {derivedPosetCoversUp, derivedPosetTopoAsc} =
      let topoIndex = IntMap.fromList (zip (fmap unFinObjectId (Vector.toList derivedPosetTopoAsc)) [0 :: Int ..])
       in and
            [ IntMap.findWithDefault 0 sourceKey topoIndex < IntMap.findWithDefault 0 targetKey topoIndex
            | (sourceKey, targets) <- IntMap.toList derivedPosetCoversUp
            , targetKey <- IntSet.toList targets
            ]

matrixIdentityLaw :: Bool
matrixIdentityLaw =
  let left = DenseMat 2 2 (Vector.fromList [Vector.fromList [1, 2], Vector.fromList [3, 4]]) :: DenseMat Int
      identity = identMat 2
   in matMulChecked identity left == Right left && matMulChecked left identity == Right left

matrixTransposeInvolutionLaw :: Bool
matrixTransposeInvolutionLaw =
  let matrixValue = DenseMat 2 3 (Vector.fromList [Vector.fromList [1, 2, 3], Vector.fromList [4, 5, 6]]) :: DenseMat Int
   in (transposeMatChecked matrixValue >>= transposeMatChecked) == Right matrixValue

matrixRestrictIdempotentLaw :: Bool
matrixRestrictIdempotentLaw =
  let axis = fromLabels (Vector.fromList [FinObjectId 0, FinObjectId 1])
      matrixValue = zeroBlocked axis axis :: BlockedMat GF2
      keep = IntSet.singleton 0
   in restrictBlocked keep (restrictBlocked keep matrixValue) == restrictBlocked keep matrixValue
        && blockAt (FinObjectId 0) (FinObjectId 0) (restrictBlocked keep matrixValue) == blockAt (FinObjectId 0) (FinObjectId 0) matrixValue

matrixBlockedSparseRepresentationAgreementLaw :: QC.Property
matrixBlockedSparseRepresentationAgreementLaw =
  QC.withNumTests 400
    ( QC.forAll
        (QC.chooseInt (0, 100000))
        ( \seedValue ->
            let blockedMatrix = seededBlockedMatrix seedValue
                denseMatrix = collapseBlockedDense blockedMatrix
                sparseMatrix = blockedToSparseMat blockedMatrix
                denseSparseMatrix = denseToSparseMat denseMatrix
             in QC.counterexample
                  (show (seedValue, sparseMatrix, denseSparseMatrix))
                  ( sparseShape sparseMatrix == sparseShape denseSparseMatrix
                      && sparseCoordinates sparseMatrix == sparseCoordinates denseSparseMatrix
                  )
        )
    )

complexDifferentialSquaresZeroLaw :: Bool
complexDifferentialSquaresZeroLaw =
  composesToZero sampleZeroComplex && isRight (mkDerivedChecked singletonPosetFixture sampleZeroComplex)

complexNormalizationIdempotentLaw :: Bool
complexNormalizationIdempotentLaw =
  let canonical = Vector.fromList [FinObjectId 0]
      once = normalizeComplexPresentation canonical sampleZeroComplex
      twice = normalizeComplexPresentation canonical once
   in once == twice

complexMinimizationHypercohomologyInvariantLaw :: QC.Property
complexMinimizationHypercohomologyInvariantLaw =
  QC.withNumTests 400
    ( QC.forAll
        inflatedWedgeComplexCaseGen
        ( \inflatedCase ->
            QC.conjoin
              ( fmap
                  minimizationHypercohomologyInvariantCase
                  (plainWedgeComplexCases <> [inflatedCase])
                  <> [forcedPeelingCase inflatedCase]
              )
        )
    )

complexMinimizationMicrosupportInvariantLaw :: QC.Property
complexMinimizationMicrosupportInvariantLaw =
  QC.withNumTests 400
    ( QC.forAll
        inflatedWedgeComplexCaseGen
        ( \inflatedCase ->
            QC.conjoin
              ( fmap
                  minimizationMicrosupportInvariantCase
                  (plainWedgeComplexCases <> [inflatedCase])
                  <> [forcedPeelingCase inflatedCase]
              )
        )
    )

complexMinimizationDegreeWindowStableLaw :: QC.Property
complexMinimizationDegreeWindowStableLaw =
  QC.withNumTests 400
    ( QC.forAll
        inflatedWedgeComplexCaseGen
        ( \inflatedCase ->
            QC.conjoin
              ( fmap
                  minimizationDegreeWindowStableCase
                  (trailingContractiblePointCase : plainWedgeComplexCases <> [inflatedCase])
                  <> [forcedPeelingCase inflatedCase]
              )
        )
    )

quillenARejectsBadFiberLaw :: Bool
quillenARejectsBadFiberLaw =
  case (sampleTwoPointDiscrete, sampleSingleton) of
    (Right sourcePoset, Right targetPoset) ->
      fmap (== QuillenAInconclusive (FinObjectId 0))
        (posetFunctor sourcePoset targetPoset (const (FinObjectId 0)) >>= quillenAMaximumCertificate)
        == Right True
    _ -> False

morseSparseDigestCacheCoherenceLaw :: QC.Property
morseSparseDigestCacheCoherenceLaw =
  QC.withNumTests 400
    ( QC.forAll
        (QC.chooseInt (0, 100000))
        ( \seedValue ->
            case sparseDigestCacheCoherenceReport seedValue of
              Left err ->
                QC.counterexample (show (seedValue, err)) False
              Right report ->
                QC.counterexample
                  (show (seedValue, report))
                  ( sdccRawRanks report == sdccCachedRanks report
                      && sdccRawVanishes report == sdccCachedVanishes report
                  )
        )
    )

deterministicFixtureLaw :: Bool
deterministicFixtureLaw =
  case
    ( sampleChainPoset
    , mkDerivedPosetFromOrderEdges
        [FinObjectId 2, FinObjectId 1, FinObjectId 0]
        [(FinObjectId 1, FinObjectId 2), (FinObjectId 0, FinObjectId 1)]
    )
  of
    (Right forwardPoset, Right reversedInputPoset) ->
      derivedPosetUpper forwardPoset == derivedPosetUpper reversedInputPoset
        && derivedPosetLower forwardPoset == derivedPosetLower reversedInputPoset
        && derivedPosetCoversUp forwardPoset == derivedPosetCoversUp reversedInputPoset
    _ -> False

shiftReindexesHypercohomologyLaw :: QC.Property
shiftReindexesHypercohomologyLaw =
  QC.withNumTests 400
    ( QC.forAll
        shiftedWedgeComplexCaseGen
        ( \(shiftAmount, caseValue) ->
            let derivedValue = Derived singletonPosetFixture (wedgeComplex caseValue)
             in compareDims
                  (show ("shift reindexes", shiftAmount, caseValue))
                  (reindexedDims shiftAmount <$> hypercohomologyDims (shift shiftAmount derivedValue))
                  (hypercohomologyDims derivedValue)
        )
    )

mapSquaresCommuteLaw :: QC.Property
mapSquaresCommuteLaw =
  QC.withNumTests 400
    ( QC.forAll
        lawfulMapFamilyGen
        ( \familyValue ->
            QC.counterexample
              (show familyValue)
              ( acceptsMapFamily familyValue
                  && rejectsPerturbedNonCommutingComponent
              )
        )
    )

coneEulerAdditiveLaw :: QC.Property
coneEulerAdditiveLaw =
  QC.withNumTests 400
    ( QC.forAll
        lawfulMapFamilyGen
        ( \familyValue ->
            withSingletonPoset "cone Euler additive" $ \posetValue ->
              case derivedMapForFamily familyValue of
                Left err ->
                  QC.counterexample (show (familyValue, err)) False
                Right mapValue ->
                  compareScalar
                    (show ("cone Euler additive", familyValue))
                    (cone mapValue >>= eulerCharacteristic)
                    (liftA2 (-) (eulerCharacteristic (derivedMapTarget mapValue)) (eulerCharacteristic (derivedMapSource mapValue)))
        )
    )

triangleRotationInvariantLaw :: QC.Property
triangleRotationInvariantLaw =
  QC.withNumTests 400
    ( QC.forAll
        lawfulMapFamilyGen
        ( \familyValue ->
            withSingletonPoset "triangle rotation invariant" $ \posetValue ->
              case derivedMapForFamily familyValue of
                Left err ->
                  QC.counterexample (show (familyValue, err)) False
                Right mapValue ->
                  case mkTriangleOf mapValue of
                    Left err ->
                      QC.counterexample (show (familyValue, err)) False
                    Right triangleValue ->
                      case rotateTriangle triangleValue of
                        Left err ->
                          QC.counterexample (show (familyValue, err)) False
                        Right rotatedTriangle ->
                          compareScalar
                            (show ("triangle rotation invariant", familyValue))
                            (eulerCharacteristic (triC rotatedTriangle))
                            (eulerCharacteristic (shift 1 (triA triangleValue)))
        )
    )

quasiIsoConeAcyclicLaw :: QC.Property
quasiIsoConeAcyclicLaw =
  withSingletonPoset "quasi-iso cone acyclic" $ \posetValue ->
    QC.conjoin
      [ quasiIsoCriterionCase posetValue "identity" (identityMap (Derived posetValue sampleZeroComplex)) True
      , quasiIsoCriterionCase posetValue "zero differing hypercohomology" differingZeroMap False
      ]

verdierInvolutionInvariantsLaw :: QC.Property
verdierInvolutionInvariantsLaw =
  QC.withNumTests 400
    ( QC.forAll
        inflatedWedgeComplexCaseGen
        ( \caseValue ->
            withSingletonPoset "Verdier involution invariants" $ \posetValue ->
              let derivedValue = Derived posetValue (wedgeComplex caseValue)
               in compareDims
                    (show ("Verdier involution", caseValue))
                    (hypercohomologyDims derivedValue)
                    (verdierDualComplex derivedValue >>= verdierDualComplex >>= hypercohomologyDims)
        )
    )

rHomTensorAdjunctionDimsLaw :: QC.Property
rHomTensorAdjunctionDimsLaw =
  withSingletonPoset "RHom tensor adjunction dimensions" $ \posetValue ->
    case (singletonInjectiveAt 0 1, singletonInjectiveAt 1 2, singletonInjectiveAt 2 1) of
      (Right leftValue, Right middleValue, Right rightValue) ->
        compareDims
          "RHom tensor adjunction dimensions"
          (tensorProduct leftValue middleValue >>= \tensorValue -> internalHom tensorValue rightValue >>= hypercohomologyDims)
          (internalHom middleValue rightValue >>= \homValue -> internalHom leftValue homValue >>= hypercohomologyDims)
      results ->
        QC.counterexample (show results) False

truncationTriangleExactLaw :: QC.Property
truncationTriangleExactLaw =
  QC.withNumTests 400
    ( QC.forAll
        inflatedWedgeComplexCaseGen
        ( \caseValue ->
            let cutoffDegree = 0
                derivedValue = Derived singletonPosetFixture (wedgeComplex caseValue)
             in QC.conjoin
                  [ vanishOutside
                      (show ("stupidTruncateBelow", caseValue))
                      (> cutoffDegree + 1)
                      (stupidTruncateBelow cutoffDegree derivedValue)
                  , vanishOutside
                      (show ("stupidTruncateAbove", caseValue))
                      (< cutoffDegree)
                      (stupidTruncateAbove cutoffDegree derivedValue)
                  , withSingletonPoset "canonical truncation triangle" $ \posetValue ->
                      canonicalTruncationAgrees posetValue cutoffDegree derivedValue
                  ]
        )
    )

canonicalTruncationAgrees :: DerivedPoset -> Int -> Derived GF2 -> QC.Property
canonicalTruncationAgrees posetValue cutoffDegree derivedValue =
  case ( hypercohomologyDims derivedValue
       , canonicalTruncateAtMost cutoffDegree derivedValue
       , canonicalTruncateAtLeast (cutoffDegree + 1) derivedValue
       , canonicalTruncationPair cutoffDegree derivedValue
       ) of
    (Right fullDims, Right lowerValue, Right upperValue, Right pairValue) ->
      case (hypercohomologyDims lowerValue, hypercohomologyDims upperValue) of
        (Right lowerDims, Right upperDims) ->
          QC.counterexample
            (show ("canonical truncation", cutoffDegree, fullDims, lowerDims, upperDims))
            ( matchesHalf (<= cutoffDegree) fullDims lowerDims
                && matchesHalf (>= cutoffDegree + 1) fullDims upperDims
                && pairValue == (lowerValue, upperValue)
            )
        dimResults ->
          QC.counterexample (show ("canonical truncation dims", cutoffDegree, dimResults)) False
    results ->
      QC.counterexample (show ("canonical truncation", cutoffDegree, results)) False
  where
    matchesHalf :: (Int -> Bool) -> IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
    matchesHalf keepDegree fullDims halfDims =
      all
        ( \degreeValue ->
            IntMap.findWithDefault 0 degreeValue halfDims
              == ( if keepDegree degreeValue
                     then IntMap.findWithDefault 0 degreeValue fullDims
                     else 0
                 )
        )
        (IntMap.keys fullDims <> IntMap.keys halfDims)

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

quasiIsoCriterionCase :: DerivedPoset -> String -> DerivedMap GF2 -> Bool -> QC.Property
quasiIsoCriterionCase posetValue context mapValue expectedValue =
  compareScalar
    context
    (quasiIsoCone mapValue >>= hypercohomologyVanishes)
    (Right expectedValue)

differingZeroMap :: DerivedMap GF2
differingZeroMap =
  zeroMap zeroSourceFixture zeroTargetFixture

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

forcedPeelingCase :: WedgeComplexCase -> QC.Property
forcedPeelingCase caseValue =
  QC.counterexample
    (show ("expected forced peeling", caseValue))
    ( firstNonMinimal (wedgeComplex caseValue) /= Nothing
        && wccContractibleCount caseValue > 0
    )

minimizationHypercohomologyInvariantCase :: WedgeComplexCase -> QC.Property
minimizationHypercohomologyInvariantCase caseValue =
  withSingletonPoset "hypercohomology minimization invariant" $ \posetValue ->
    let complexValue = wedgeComplex caseValue
        degreeWindowValue = degreeWindow complexValue
     in case minimizeComplex complexValue of
          Left err ->
            QC.counterexample (show (caseValue, err)) False
          Right minimizedComplex ->
            case
              ( zeroPaddedHypercohomology posetValue degreeWindowValue complexValue
              , zeroPaddedHypercohomology posetValue degreeWindowValue minimizedComplex
              ) of
              (Right sourceDims, Right minimizedDims) ->
                QC.counterexample
                  (show (caseValue, sourceDims, minimizedDims))
                  (sourceDims == minimizedDims)
              (leftResult, rightResult) ->
                QC.counterexample (show (caseValue, leftResult, rightResult)) False

minimizationMicrosupportInvariantCase :: WedgeComplexCase -> QC.Property
minimizationMicrosupportInvariantCase caseValue =
  withSingletonPoset "microsupport minimization invariant" $ \posetValue ->
    let complexValue = wedgeComplex caseValue
     in case minimizeComplex complexValue of
          Left err ->
            QC.counterexample (show (caseValue, err)) False
          Right minimizedComplex ->
            case
              ( microsupportResult posetValue complexValue
              , microsupportResult posetValue minimizedComplex
              ) of
              (Right sourceMicrosupport, Right minimizedMicrosupport) ->
                QC.counterexample
                  (show (caseValue, sourceMicrosupport, minimizedMicrosupport))
                  (sourceMicrosupport == minimizedMicrosupport)
              (leftResult, rightResult) ->
                QC.counterexample (show (caseValue, leftResult, rightResult)) False

minimizationDegreeWindowStableCase :: WedgeComplexCase -> QC.Property
minimizationDegreeWindowStableCase caseValue =
  withSingletonPoset "degree-window minimization invariant" $ \posetValue ->
    let complexValue = wedgeComplex caseValue
        sourceWindow = degreeWindow complexValue
     in case minimizeComplex complexValue of
          Left err ->
            QC.counterexample (show (caseValue, err)) False
          Right minimizedComplex ->
            case
              ( hypercohomologyDimsWith gf2PackedRankBackend (Derived posetValue complexValue)
              , hypercohomologyDimsWith gf2PackedRankBackend (Derived posetValue minimizedComplex)
              ) of
              (Right sourceDims, Right minimizedDims) ->
                QC.counterexample
                  (show (caseValue, sourceWindow, degreeWindow minimizedComplex, sourceDims, minimizedDims))
                  ( degreeWindow minimizedComplex == sourceWindow
                      && IntMap.keys sourceDims == sourceWindow
                      && IntMap.keys minimizedDims == sourceWindow
                  )
              (leftResult, rightResult) ->
                QC.counterexample (show (caseValue, leftResult, rightResult)) False

withSingletonPoset :: String -> (DerivedPoset -> QC.Property) -> QC.Property
withSingletonPoset context continue =
  case sampleSingleton of
    Left err ->
      QC.counterexample (show (context, err)) False
    Right posetValue ->
      continue posetValue

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

infixr 1 ==>

(==>) :: Bool -> Bool -> Bool
antecedent ==> consequent =
  not antecedent || consequent
