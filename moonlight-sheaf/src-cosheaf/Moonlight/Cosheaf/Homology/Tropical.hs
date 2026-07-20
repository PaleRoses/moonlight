{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Homology.Tropical
  ( TropicalPDegree (..),
    TropicalBidegree (..),
    TropicalCellKey (..),
    TropicalCell (..),
    TropicalTangentBasis (..),
    TropicalFace (..),
    TropicalCellularComplex (..),
    TropicalCoefficientWitness (..),
    TropicalBoundaryProvenance,
    TropicalCoefficientFailure (..),
    TropicalHomologyFailure (..),
    TropicalHomologyArtifact (..),
    tropicalTangentRank,
    tropicalCellularBoundaryAlgebra,
    tropicalCoefficientChain,
    tropicalHomology,
    tropicalHomologyWithBackend,
    tropicalHomologyGF2,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Vector qualified as Box
import Moonlight.Algebra
  ( AdditiveMonoid (..), Semiring,
  )
import Moonlight.Cosheaf.Chain.Linear
  ( CosheafBoundaryProvenance,
    LinearCosheafChainFailure (..),
    LinearCosheafChainSpec (..),
    prepareLinearCosheafChainFromSupportPlan,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps,
    gf2CoefficientOps,
    rationalCoefficientOps,
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( PreparedCosheafChain,
    pccChainComplex,
  )
import Moonlight.Cosheaf.Support.Linear
  ( fullLinearCosheafSupportPlan,
  )
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError,
    HomologicalDegree (..),
    HomologyBackend (..),
    HomologyBackendTag,
    HomologyFailure,
    HomologyGroup,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    runHomologyBackend,
    homologyBackendTag,
  )
import Moonlight.LinAlg
  ( ExteriorBasis (..),
    ExteriorPowerFailure (..),
    GF2,
    exteriorBasis,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
  )
import Numeric.Natural (Natural)

newtype TropicalPDegree = TropicalPDegree
  { unTropicalPDegree :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type TropicalBidegree :: Type
data TropicalBidegree = TropicalBidegree
  { tropicalBidegreeP :: !TropicalPDegree,
    tropicalBidegreeQ :: !HomologicalDegree
  }
  deriving stock (Eq, Ord, Show, Read)

newtype TropicalCellKey = TropicalCellKey
  { unTropicalCellKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type TropicalCell :: Type
data TropicalCell = TropicalCell
  { tropicalCellKey :: !TropicalCellKey,
    tropicalCellDimension :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Integer tangent lattice basis. V1 stores explicit lattice basis vectors;
-- coefficient cosheaves use their finite rank plus validated integer tangent
-- maps on faces.
type TropicalTangentBasis :: Type
data TropicalTangentBasis = TropicalTangentBasis
  { tropicalTangentBasisVectors :: ![[Integer]]
  }
  deriving stock (Eq, Ord, Show, Read)

type TropicalFace :: Type
data TropicalFace = TropicalFace
  { tropicalFaceSource :: !TropicalCell,
    tropicalFaceTarget :: !TropicalCell,
    tropicalFaceOrientation :: !Int,
    tropicalFaceTangentMap :: ![[Integer]]
  }
  deriving stock (Eq, Ord, Show, Read)

type TropicalCellularComplex :: Type
data TropicalCellularComplex = TropicalCellularComplex
  { tropicalCells :: !(Map TropicalCellKey TropicalCell),
    tropicalFaces :: ![TropicalFace],
    tropicalTangentBases :: !(Map TropicalCellKey TropicalTangentBasis)
  }
  deriving stock (Eq, Show, Read)

type TropicalCoefficientWitness :: Type -> Type
data TropicalCoefficientWitness coefficient = TropicalCoefficientWitness
  { tcwPDegree :: !TropicalPDegree,
    tcwFace :: !TropicalFace,
    tcwSourceExteriorIndex :: !Int,
    tcwTargetExteriorIndex :: !Int,
    tcwCoefficient :: !coefficient
  }
  deriving stock (Eq, Show)

type TropicalBoundaryProvenance coefficient =
  CosheafBoundaryProvenance TropicalCell TropicalFace coefficient (TropicalCoefficientWitness coefficient)

type TropicalCoefficientFailure :: Type
data TropicalCoefficientFailure
  = TropicalCoefficientTangentBasisMissing !TropicalCellKey
  | TropicalCoefficientExteriorFailed !ExteriorPowerFailure
  | TropicalCoefficientBoundaryShapeFailed !BoundaryIncidenceShapeError
  deriving stock (Eq, Show)

type TropicalHomologyFailure :: Type -> Type
data TropicalHomologyFailure chainCoeff
  = TropicalNegativePDegree !TropicalPDegree
  | TropicalNegativeCellDimension !TropicalCell
  | TropicalFaceSourceMissing !TropicalFace
  | TropicalFaceTargetMissing !TropicalFace
  | TropicalTangentBasisMissing !TropicalCellKey
  | TropicalTangentBasisMalformed !TropicalCellKey ![Int]
  | TropicalFaceOrientationZero !TropicalFace
  | TropicalFaceDimensionMismatch !TropicalFace !Int !Int
  | TropicalFaceTangentMapShapeMismatch !TropicalFace !Int !Int !Int ![Int]
  | TropicalChainFailed !TropicalPDegree !(LinearCosheafChainFailure TropicalCell TropicalFace chainCoeff TropicalCoefficientFailure)
  | TropicalBackendFailed !TropicalPDegree !HomologyBackendTag !HomologyFailure
  deriving stock (Eq, Show)

type TropicalHomologyArtifact :: Type -> Type -> Type
data TropicalHomologyArtifact chainCoeff groupCoeff = TropicalHomologyArtifact
  { thaBackend :: !HomologyBackendTag,
    thaComplex :: !TropicalCellularComplex,
    thaChainsByP :: !(Map TropicalPDegree (PreparedCosheafChain TropicalCellularComplex TropicalCell chainCoeff (TropicalBoundaryProvenance chainCoeff))),
    thaGroupsByBidegree :: !(Map TropicalBidegree (HomologyGroup groupCoeff))
  }

type TropicalCoefficientRuntime :: Type -> Type
data TropicalCoefficientRuntime chainCoeff = TropicalCoefficientRuntime
  { tcrComplex :: !TropicalCellularComplex,
    tcrExteriorBases :: !(Map (TropicalPDegree, Int) ExteriorBasis),
    tcrFaceBlocks :: !(Map (TropicalPDegree, TropicalFace) (BoundaryIncidence chainCoeff))
  }

tropicalTangentRank :: TropicalTangentBasis -> Int
tropicalTangentRank =
  length . tropicalTangentBasisVectors
{-# INLINE tropicalTangentRank #-}

tropicalCellularBoundaryAlgebra :: SiteBoundaryAlgebra TropicalCellularComplex TropicalCell TropicalFace
tropicalCellularBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = complexDepth,
      sbaCellsAtDimension = \complex degreeValue ->
        filter ((== degreeValue) . tropicalCellDimension) (Map.elems (tropicalCells complex)),
      sbaFaceMorphisms = tropicalFaces,
      sbaFaceSource = tropicalFaceSource,
      sbaFaceTarget = tropicalFaceTarget,
      sbaFaceOrientation = tropicalFaceOrientation,
      sbaCellDimension = tropicalCellDimension
    }

tropicalCoefficientChain ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  CoefficientOps chainCoeff ->
  TropicalPDegree ->
  TropicalCellularComplex ->
  Either
    (TropicalHomologyFailure chainCoeff)
    (PreparedCosheafChain TropicalCellularComplex TropicalCell chainCoeff (TropicalBoundaryProvenance chainCoeff))
tropicalCoefficientChain coefficientOps pDegree complex = do
  runtime <- prepareTropicalCoefficientRuntime [pDegree] complex
  tropicalCoefficientChainFromRuntime coefficientOps runtime pDegree

tropicalCoefficientChainFromRuntime ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  CoefficientOps chainCoeff ->
  TropicalCoefficientRuntime chainCoeff ->
  TropicalPDegree ->
  Either
    (TropicalHomologyFailure chainCoeff)
    (PreparedCosheafChain TropicalCellularComplex TropicalCell chainCoeff (TropicalBoundaryProvenance chainCoeff))
tropicalCoefficientChainFromRuntime coefficientOps runtime pDegree = do
  supportPlan <-
    first (TropicalChainFailed pDegree . LinearCosheafChainSupportFailed) $
      fullLinearCosheafSupportPlan complex tropicalCellularBoundaryAlgebra (fpCostalkDimensionFromRuntime runtime pDegree)
  first (TropicalChainFailed pDegree) $
    prepareLinearCosheafChainFromSupportPlan
      coefficientOps
      supportPlan
      (tropicalLinearCosheafChainSpecFromRuntime pDegree runtime)
  where
    complex =
      tcrComplex runtime

tropicalHomology ::
  [TropicalPDegree] ->
  TropicalCellularComplex ->
  Either (TropicalHomologyFailure Rational) (TropicalHomologyArtifact Rational Rational)
tropicalHomology =
  tropicalHomologyWithBackend RationalRankBackend rationalCoefficientOps
{-# INLINEABLE tropicalHomology #-}

tropicalHomologyGF2 ::
  [TropicalPDegree] ->
  TropicalCellularComplex ->
  Either (TropicalHomologyFailure GF2) (TropicalHomologyArtifact GF2 GF2)
tropicalHomologyGF2 =
  tropicalHomologyWithBackend GF2RankBackend gf2CoefficientOps
{-# INLINEABLE tropicalHomologyGF2 #-}

tropicalHomologyWithBackend ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  HomologyBackend chainCoeff groupCoeff ->
  CoefficientOps chainCoeff ->
  [TropicalPDegree] ->
  TropicalCellularComplex ->
  Either (TropicalHomologyFailure chainCoeff) (TropicalHomologyArtifact chainCoeff groupCoeff)
tropicalHomologyWithBackend backend coefficientOps pDegrees complex = do
  runtime <- prepareTropicalCoefficientRuntime pDegrees complex
  perPDegree <- traverse (homologyAtP runtime) pDegrees
  pure
    TropicalHomologyArtifact
      { thaBackend = backendTag,
        thaComplex = complex,
        thaChainsByP = Map.fromList (fmap (\(pDegree, chain, _groups) -> (pDegree, chain)) perPDegree),
        thaGroupsByBidegree = Map.fromList (foldMap tropicalGroupsByBidegree perPDegree)
      }
  where
    backendTag = homologyBackendTag backend

    homologyAtP runtime pDegree = do
      chain <- tropicalCoefficientChainFromRuntime coefficientOps runtime pDegree
      groups <-
        first (TropicalBackendFailed pDegree backendTag) $
          runHomologyBackend backend (pccChainComplex chain)
      pure (pDegree, chain, groups)

prepareTropicalCoefficientRuntime ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  [TropicalPDegree] ->
  TropicalCellularComplex ->
  Either (TropicalHomologyFailure chainCoeff) (TropicalCoefficientRuntime chainCoeff)
prepareTropicalCoefficientRuntime pDegrees complex = do
  validateTropicalInputs pDegrees complex
  exteriorBases <-
    exteriorBasisCache pDegrees complex
  faceBlocks <-
    Map.fromList
      <$> traverse
        (faceBlockEntry exteriorBases)
        [ (pDegree, face)
          | pDegree <- pDegrees,
            face <- tropicalBoundaryFaces complex
        ]
  pure
    TropicalCoefficientRuntime
      { tcrComplex = complex,
        tcrExteriorBases = exteriorBases,
        tcrFaceBlocks = faceBlocks
      }
  where
    faceBlockEntry exteriorBases (pDegree, face) =
      first
        (TropicalChainFailed pDegree . LinearCosheafChainCorestrictionFailed face)
        (fpCorestrictionBlockFromCache exteriorBases pDegree complex face)
        >>= \blockValue -> Right ((pDegree, face), blockValue)

validateTropicalInputs :: [TropicalPDegree] -> TropicalCellularComplex -> Either (TropicalHomologyFailure chainCoeff) ()
validateTropicalInputs pDegrees complex =
  case pDegrees of
    [] -> Right ()
    firstPDegree : remainingPDegrees ->
      validatePDegree firstPDegree
        *> validateTropicalComplex complex
        *> traverse_ validatePDegree remainingPDegrees

exteriorBasisCache ::
  [TropicalPDegree] ->
  TropicalCellularComplex ->
  Either (TropicalHomologyFailure chainCoeff) (Map (TropicalPDegree, Int) ExteriorBasis)
exteriorBasisCache pDegrees complex =
  Map.traverseWithKey
    basisEntry
    basisRequests
  where
    basisRequests =
      Map.fromList
        [ ((pDegree, tropicalTangentRank tangentBasis), cell)
          | pDegree <- pDegrees,
            cell <- Map.elems (tropicalCells complex),
            tangentBasis <- maybe [] (: []) (Map.lookup (tropicalCellKey cell) (tropicalTangentBases complex))
        ]

    basisEntry (pDegree@(TropicalPDegree pValue), rankValue) cell =
      first
        (TropicalChainFailed pDegree . LinearCosheafChainCostalkDimensionFailed cell . TropicalCoefficientExteriorFailed)
        (exteriorBasis pValue rankValue)

tropicalBoundaryFaces :: TropicalCellularComplex -> [TropicalFace]
tropicalBoundaryFaces complex =
  foldMap facesAtDegree [1 .. complexDepthInt complex]
  where
    facesAtDegree degreeValue =
      filter
        ((== degreeValue) . tropicalCellDimension . tropicalFaceSource)
        (tropicalFaces complex)

complexDepthInt :: TropicalCellularComplex -> Int
complexDepthInt =
  fromIntegral . complexDepth

tropicalGroupsByBidegree ::
  (TropicalPDegree, chain, [HomologyGroup groupCoeff]) ->
  [(TropicalBidegree, HomologyGroup groupCoeff)]
tropicalGroupsByBidegree (pDegree, _chain, groups) =
  fmap
    (\(degreeInt, groupValue) -> (TropicalBidegree pDegree (HomologicalDegree degreeInt), groupValue))
    (zip [0 ..] groups)

tropicalLinearCosheafChainSpecFromRuntime ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  TropicalPDegree ->
  TropicalCoefficientRuntime chainCoeff ->
  LinearCosheafChainSpec
    TropicalCellularComplex
    TropicalCell
    TropicalFace
    chainCoeff
    (TropicalCoefficientWitness chainCoeff)
    TropicalCoefficientFailure
tropicalLinearCosheafChainSpecFromRuntime pDegree runtime =
  LinearCosheafChainSpec
    { lccsSite = complex,
      lccsBoundaryAlgebra = tropicalCellularBoundaryAlgebra,
      lccsCostalkDimension = fpCostalkDimensionFromRuntime runtime pDegree,
      lccsCorestrictionBlock = fpCorestrictionBlockFromRuntime pDegree runtime,
      lccsEntryProvenance = \face sourceLocalIndex targetLocalIndex coefficient ->
        TropicalCoefficientWitness
          { tcwPDegree = pDegree,
            tcwFace = face,
            tcwSourceExteriorIndex = sourceLocalIndex,
            tcwTargetExteriorIndex = targetLocalIndex,
            tcwCoefficient = coefficient
          }
    }
  where
    complex =
      tcrComplex runtime

fpCostalkDimensionFromRuntime :: TropicalCoefficientRuntime chainCoeff -> TropicalPDegree -> TropicalCell -> Int
fpCostalkDimensionFromRuntime runtime pDegree cell =
  maybe 0 (length . ebBasisVectors) $ do
    tangentBasis <- Map.lookup (tropicalCellKey cell) (tropicalTangentBases complex)
    Map.lookup (pDegree, tropicalTangentRank tangentBasis) (tcrExteriorBases runtime)
  where
    complex =
      tcrComplex runtime

fpCorestrictionBlockFromRuntime ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  TropicalPDegree ->
  TropicalCoefficientRuntime chainCoeff ->
  TropicalFace ->
  Either TropicalCoefficientFailure (BoundaryIncidence chainCoeff)
fpCorestrictionBlockFromRuntime pDegree runtime face =
  maybe
    (fpCorestrictionBlockFromCache (tcrExteriorBases runtime) pDegree (tcrComplex runtime) face)
    Right
    (Map.lookup (pDegree, face) (tcrFaceBlocks runtime))

fpCorestrictionBlockFromCache ::
  (Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  Map (TropicalPDegree, Int) ExteriorBasis ->
  TropicalPDegree ->
  TropicalCellularComplex ->
  TropicalFace ->
  Either TropicalCoefficientFailure (BoundaryIncidence chainCoeff)
fpCorestrictionBlockFromCache exteriorBases pDegree@(TropicalPDegree pValue) complex face = do
  sourceBasis <- tangentBasisFor (tropicalFaceSource face)
  targetBasis <- tangentBasisFor (tropicalFaceTarget face)
  sourceExteriorBasis <- cachedExteriorBasis exteriorBases pDegree (tropicalTangentRank sourceBasis)
  targetExteriorBasis <- cachedExteriorBasis exteriorBases pDegree (tropicalTangentRank targetBasis)
  sparseExteriorMatrixToIncidence
    pValue
    (tropicalTangentRank targetBasis)
    (tropicalTangentRank sourceBasis)
    targetExteriorBasis
    sourceExteriorBasis
    (fmap (fmap fromInteger) (tropicalFaceTangentMap face))
  where
    tangentBasisFor cell =
      maybe
        (Left (TropicalCoefficientTangentBasisMissing (tropicalCellKey cell)))
        Right
        (Map.lookup (tropicalCellKey cell) (tropicalTangentBases complex))

cachedExteriorBasis ::
  Map (TropicalPDegree, Int) ExteriorBasis ->
  TropicalPDegree ->
  Int ->
  Either TropicalCoefficientFailure ExteriorBasis
cachedExteriorBasis exteriorBases pDegree@(TropicalPDegree pValue) rankValue =
  maybe
    (first TropicalCoefficientExteriorFailed (exteriorBasis pValue rankValue))
    Right
    (Map.lookup (pDegree, rankValue) exteriorBases)

sparseExteriorMatrixToIncidence ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  Int ->
  Int ->
  Int ->
  ExteriorBasis ->
  ExteriorBasis ->
  [[coefficient]] ->
  Either TropicalCoefficientFailure (BoundaryIncidence coefficient)
sparseExteriorMatrixToIncidence degree targetRank sourceRank targetBasis sourceBasis matrix
  | degree < 0 = Left (TropicalCoefficientExteriorFailed (ExteriorNegativeDegree degree))
  | sourceRank < 0 = Left (TropicalCoefficientExteriorFailed (ExteriorNegativeSourceRank sourceRank))
  | targetRank < 0 = Left (TropicalCoefficientExteriorFailed (ExteriorNegativeTargetRank targetRank))
  | actualShapeRows matrix /= targetRank || any (/= sourceRank) (actualShapeColumns matrix) =
      Left (TropicalCoefficientExteriorFailed shapeFailure)
  | otherwise = do
      sparseEntries <-
        first TropicalCoefficientExteriorFailed $
          sparseExteriorBoundaryEntries shapeFailure sourceRank entries targetBasis sourceBasis
      first TropicalCoefficientBoundaryShapeFailed $
        mkBoundaryIncidenceFromOrderedEntries
          (fromIntegral (length (ebBasisVectors sourceBasis)))
          (fromIntegral (length (ebBasisVectors targetBasis)))
          sparseEntries
  where
    shapeFailure =
      ExteriorMatrixShapeMismatch
        targetRank
        sourceRank
        (actualShapeRows matrix)
        (actualShapeColumns matrix)

    entries =
      Box.fromList (concat matrix)

sparseExteriorBoundaryEntries ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  ExteriorPowerFailure ->
  Int ->
  Box.Vector coefficient ->
  ExteriorBasis ->
  ExteriorBasis ->
  Either ExteriorPowerFailure [BoundaryEntry coefficient]
sparseExteriorBoundaryEntries shapeFailure sourceRank entries targetBasis sourceBasis =
  fmap catMaybes (traverse sparseEntry indexedMinors)
  where
    indexedMinors =
      ( \(sourceIndexValue, sourceVector) ->
          fmap
            ( \(targetIndexValue, targetVector) ->
                (targetIndexValue, targetVector, sourceIndexValue, sourceVector)
            )
            (zip [0 :: Natural ..] (ebBasisVectors targetBasis))
      )
        =<< zip [0 :: Natural ..] (ebBasisVectors sourceBasis)

    sparseEntry (targetIndexValue, targetVector, sourceIndexValue, sourceVector) =
      fmap
        ( \coefficientValue ->
            if coefficientValue == zero
              then Nothing
              else Just (mkBoundaryEntry sourceIndexValue targetIndexValue coefficientValue)
        )
        (minorDeterminant shapeFailure sourceRank entries targetVector sourceVector)

actualShapeRows :: [[coefficient]] -> Int
actualShapeRows =
  length
{-# INLINE actualShapeRows #-}

actualShapeColumns :: [[coefficient]] -> [Int]
actualShapeColumns =
  fmap length
{-# INLINE actualShapeColumns #-}

entryAt ::
  ExteriorPowerFailure ->
  Int ->
  Box.Vector coefficient ->
  Int ->
  Int ->
  Either ExteriorPowerFailure coefficient
entryAt failure sourceRank entries targetIndex sourceIndex =
  maybe (Left failure) Right (entries Box.!? (targetIndex * sourceRank + sourceIndex))
{-# INLINE entryAt #-}

minorDeterminant ::
  Num coefficient =>
  ExteriorPowerFailure ->
  Int ->
  Box.Vector coefficient ->
  [Int] ->
  [Int] ->
  Either ExteriorPowerFailure coefficient
minorDeterminant failure sourceRank entries targetVector sourceVector =
  case (targetVector, sourceVector) of
    ([], []) -> Right 1
    ([row0], [column0]) ->
      entryAt failure sourceRank entries row0 column0
    ([row0, row1], [column0, column1]) -> do
      a <- entryAt failure sourceRank entries row0 column0
      b <- entryAt failure sourceRank entries row0 column1
      c <- entryAt failure sourceRank entries row1 column0
      d <- entryAt failure sourceRank entries row1 column1
      Right ((a * d) - (b * c))
    ([row0, row1, row2], [column0, column1, column2]) -> do
      a <- entryAt failure sourceRank entries row0 column0
      b <- entryAt failure sourceRank entries row0 column1
      c <- entryAt failure sourceRank entries row0 column2
      d <- entryAt failure sourceRank entries row1 column0
      e <- entryAt failure sourceRank entries row1 column1
      f <- entryAt failure sourceRank entries row1 column2
      g <- entryAt failure sourceRank entries row2 column0
      h <- entryAt failure sourceRank entries row2 column1
      i <- entryAt failure sourceRank entries row2 column2
      Right ((a * e * i) + (b * f * g) + (c * d * h) - (c * e * g) - (b * d * i) - (a * f * h))
    _ ->
      fmap determinant
        ( traverse
            (\targetIndex -> traverse (entryAt failure sourceRank entries targetIndex) sourceVector)
            targetVector
        )
{-# INLINE minorDeterminant #-}

determinant :: Num coefficient => [[coefficient]] -> coefficient
determinant matrix =
  case matrix of
    [] -> 1
    [singleRow] ->
      case singleRow of
        [value] -> value
        _ -> 0
    firstRow : remainingRows ->
      sum
        ( fmap
            (\(columnIndex, value) -> signFor columnIndex * value * determinant (removeColumn columnIndex remainingRows))
            (zip [0 ..] firstRow)
        )
{-# INLINEABLE determinant #-}

removeColumn :: Int -> [[coefficient]] -> [[coefficient]]
removeColumn columnIndex =
  fmap (fmap snd . filter ((/= columnIndex) . fst) . zip [0 :: Int ..])
{-# INLINE removeColumn #-}

signFor :: Num coefficient => Int -> coefficient
signFor columnIndex =
  if even columnIndex then 1 else (-1)
{-# INLINE signFor #-}

validatePDegree :: TropicalPDegree -> Either (TropicalHomologyFailure chainCoeff) ()
validatePDegree pDegree@(TropicalPDegree pValue) =
  if pValue < 0
    then Left (TropicalNegativePDegree pDegree)
    else Right ()

validateTropicalComplex :: TropicalCellularComplex -> Either (TropicalHomologyFailure chainCoeff) ()
validateTropicalComplex complex =
  traverse_ validateCell (Map.elems (tropicalCells complex))
    *> traverse_ validateFace (tropicalFaces complex)
  where
    validateCell cell
      | tropicalCellDimension cell < 0 = Left (TropicalNegativeCellDimension cell)
      | otherwise = validateTangentBasis (tropicalCellKey cell)

    validateFace face = do
      requireCell TropicalFaceSourceMissing face (tropicalFaceSource face)
      requireCell TropicalFaceTargetMissing face (tropicalFaceTarget face)
      validateOrientation face
      validateFaceDimension face
      sourceBasis <- validateTangentBasis (tropicalCellKey (tropicalFaceSource face))
      targetBasis <- validateTangentBasis (tropicalCellKey (tropicalFaceTarget face))
      validateTangentMapShape face (tropicalTangentRank targetBasis) (tropicalTangentRank sourceBasis)

    requireCell failureConstructor face cell =
      case Map.lookup (tropicalCellKey cell) (tropicalCells complex) of
        Just knownCell | knownCell == cell -> Right ()
        _ -> Left (failureConstructor face)

    validateOrientation :: TropicalFace -> Either (TropicalHomologyFailure chainCoeff) ()
    validateOrientation face =
      if tropicalFaceOrientation face == 0
        then Left (TropicalFaceOrientationZero face)
        else Right ()

    validateFaceDimension :: TropicalFace -> Either (TropicalHomologyFailure chainCoeff) ()
    validateFaceDimension face =
      let sourceDimension = tropicalCellDimension (tropicalFaceSource face)
          targetDimension = tropicalCellDimension (tropicalFaceTarget face)
       in if sourceDimension == targetDimension + 1
            then Right ()
            else Left (TropicalFaceDimensionMismatch face sourceDimension targetDimension)

    validateTangentBasis key =
      case Map.lookup key (tropicalTangentBases complex) of
        Nothing -> Left (TropicalTangentBasisMissing key)
        Just basis ->
          if tangentBasisRectangular basis
            then Right basis
            else Left (TropicalTangentBasisMalformed key (fmap length (tropicalTangentBasisVectors basis)))

    validateTangentMapShape :: TropicalFace -> Int -> Int -> Either (TropicalHomologyFailure chainCoeff) ()
    validateTangentMapShape face expectedRows expectedColumns =
      let actualRows = length (tropicalFaceTangentMap face)
          actualColumns = fmap length (tropicalFaceTangentMap face)
       in if actualRows == expectedRows && all (== expectedColumns) actualColumns
            then Right ()
            else Left (TropicalFaceTangentMapShapeMismatch face expectedRows expectedColumns actualRows actualColumns)

tangentBasisRectangular :: TropicalTangentBasis -> Bool
tangentBasisRectangular basis =
  case fmap length (tropicalTangentBasisVectors basis) of
    [] -> True
    firstWidth : widths -> all (== firstWidth) widths

complexDepth :: TropicalCellularComplex -> Natural
complexDepth complex =
  fromIntegral
    ( foldr
        max
        0
        (fmap (max 0 . tropicalCellDimension) (Map.elems (tropicalCells complex)))
    )
