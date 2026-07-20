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
  , -- | Rotating a distinguished triangle preserves hypercohomology
    -- dimensions degreewise (not merely the Euler characteristic), on the
    -- singleton and diamond posets.
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
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import Moonlight.Category (FinObjectId (..))
import Moonlight.Derived.Effect.Harness.Fixtures
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
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.Functor.QuillenA
  ( QuillenACertificate (..)
  , quillenAMaximumCertificate
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( gf2PackedRankBackend
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , composesToZero
  , firstNonMinimal
  , mkDerivedChecked
  , normalizeComplexPresentation
  )
import Moonlight.Derived.Pure.Site.DerivedMap
  ( DerivedMap
  , derivedMapSource
  , derivedMapTarget
  , identityMap
  , shift
  , stupidTruncateAbove
  , stupidTruncateBelow
  , zeroMap
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , blockedToSparseMat
  , blockAt
  , collapseBlockedDense
  , denseToSparseMat
  , identMat
  , matMulChecked
  , restrictBlocked
  , zeroBlocked
  , transposeMatChecked
  , fromLabels
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , leq
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.LinAlg (GF2)
import Test.Tasty.QuickCheck qualified as QC

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

-- | Both sides of the Quillen-A certificate: a discrete two-point fiber has
-- no maximum and stays inconclusive, while a chain fiber (which has a
-- maximum) is positively certified — the certifying branch is asserted, not
-- merely never reached.
quillenARejectsBadFiberLaw :: Bool
quillenARejectsBadFiberLaw =
  rejectsDiscreteFiber && certifiesChainFiber
  where
    rejectsDiscreteFiber =
      case (sampleTwoPointDiscrete, sampleSingleton) of
        (Right sourcePoset, Right targetPoset) ->
          fmap (== QuillenAInconclusive (FinObjectId 0))
            (posetFunctor sourcePoset targetPoset (const (FinObjectId 0)) >>= quillenAMaximumCertificate)
            == Right True
        _ -> False
    certifiesChainFiber =
      case (sampleChainPoset, sampleSingleton) of
        (Right sourcePoset, Right targetPoset) ->
          (posetFunctor sourcePoset targetPoset (const (FinObjectId 0)) >>= quillenAMaximumCertificate)
            == Right QuillenACertifiedByMaximum
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
            withSingletonPoset "cone Euler additive" $ \_posetValue ->
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
  QC.conjoin
    [ QC.withNumTests 400
        ( QC.forAll
            lawfulMapFamilyGen
            ( \familyValue ->
                withSingletonPoset "triangle rotation invariant" $ \_posetValue ->
                  case derivedMapForFamily familyValue of
                    Left err ->
                      QC.counterexample (show (familyValue, err)) False
                    Right mapValue ->
                      rotationPreservesDims (show ("triangle rotation invariant", familyValue)) mapValue
            )
        )
    , withDiamondPoset "triangle rotation invariant (diamond)" $ \posetValue ->
        case injectiveOnPosetAt posetValue 0 [(FinObjectId 0, 1), (FinObjectId 1, 2), (FinObjectId 3, 1)] of
          Left err ->
            QC.counterexample (show ("triangle rotation invariant (diamond)", err)) False
          Right derivedValue ->
            rotationPreservesDims "triangle rotation invariant (diamond)" (identityMap derivedValue)
    ]

-- | The rotated triangle's cone must match the shifted apex degreewise:
-- H^*(C(rot Δ)) ≡ H^*(A[1]), a strictly stronger comparison than the Euler
-- characteristic it replaces (Euler equality already follows from cone
-- additivity alone).
rotationPreservesDims :: String -> DerivedMap GF2 -> QC.Property
rotationPreservesDims context mapValue =
  case mkTriangleOf mapValue of
    Left err ->
      QC.counterexample (show (context, err)) False
    Right triangleValue ->
      case rotateTriangle triangleValue of
        Left err ->
          QC.counterexample (show (context, err)) False
        Right rotatedTriangle ->
          compareDims
            context
            (hypercohomologyDims (triC rotatedTriangle))
            (hypercohomologyDims (shift 1 (triA triangleValue)))

quasiIsoConeAcyclicLaw :: QC.Property
quasiIsoConeAcyclicLaw =
  withSingletonPoset "quasi-iso cone acyclic" $ \posetValue ->
    QC.conjoin
      [ quasiIsoCriterionCase posetValue "identity" (identityMap (Derived posetValue sampleZeroComplex)) True
      , quasiIsoCriterionCase posetValue "zero differing hypercohomology" differingZeroMap False
      ]

verdierInvolutionInvariantsLaw :: QC.Property
verdierInvolutionInvariantsLaw =
  QC.conjoin
    [ QC.withNumTests 400
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
    , -- Multi-node object on the diamond: duality mixes nodes through the
      -- dualizing complex, so this case exercises the poset structure the
      -- singleton fixtures cannot see.
      withDiamondPoset "Verdier involution invariants (diamond)" $ \posetValue ->
        case injectiveOnPosetAt posetValue 0 [(FinObjectId 0, 1), (FinObjectId 1, 2), (FinObjectId 2, 1), (FinObjectId 3, 1)] of
          Left err ->
            QC.counterexample (show ("Verdier involution (diamond)", err)) False
          Right derivedValue ->
            compareDims
              "Verdier involution (diamond)"
              (hypercohomologyDims derivedValue)
              (verdierDualComplex derivedValue >>= verdierDualComplex >>= hypercohomologyDims)
    ]

rHomTensorAdjunctionDimsLaw :: QC.Property
rHomTensorAdjunctionDimsLaw =
  QC.conjoin
    [ withSingletonPoset "RHom tensor adjunction dimensions" $ \_posetValue ->
        case (singletonInjectiveAt 0 1, singletonInjectiveAt 1 2, singletonInjectiveAt 2 1) of
          (Right leftValue, Right middleValue, Right rightValue) ->
            adjunctionDimsAgree "RHom tensor adjunction dimensions" leftValue middleValue rightValue
          results ->
            QC.counterexample (show results) False
    , withDiamondPoset "RHom tensor adjunction dimensions (diamond)" $ \posetValue ->
        case
          ( injectiveOnPosetAt posetValue 0 [(FinObjectId 0, 1), (FinObjectId 3, 1)]
          , injectiveOnPosetAt posetValue 1 [(FinObjectId 1, 2), (FinObjectId 2, 1)]
          , injectiveOnPosetAt posetValue 2 [(FinObjectId 3, 1)]
          ) of
          (Right leftValue, Right middleValue, Right rightValue) ->
            adjunctionDimsAgree "RHom tensor adjunction dimensions (diamond)" leftValue middleValue rightValue
          results ->
            QC.counterexample (show results) False
    ]

adjunctionDimsAgree :: String -> Derived GF2 -> Derived GF2 -> Derived GF2 -> QC.Property
adjunctionDimsAgree context leftValue middleValue rightValue =
  compareDims
    context
    (tensorProduct leftValue middleValue >>= \tensorValue -> internalHom tensorValue rightValue >>= hypercohomologyDims)
    (internalHom middleValue rightValue >>= \homValue -> internalHom leftValue homValue >>= hypercohomologyDims)

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
canonicalTruncationAgrees _posetValue cutoffDegree derivedValue =
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

quasiIsoCriterionCase :: DerivedPoset -> String -> DerivedMap GF2 -> Bool -> QC.Property
quasiIsoCriterionCase _posetValue context mapValue expectedValue =
  compareScalar
    context
    (quasiIsoCone mapValue >>= hypercohomologyVanishes)
    (Right expectedValue)

differingZeroMap :: DerivedMap GF2
differingZeroMap =
  zeroMap zeroSourceFixture zeroTargetFixture

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

infixr 1 ==>

(==>) :: Bool -> Bool -> Bool
antecedent ==> consequent =
  not antecedent || consequent
