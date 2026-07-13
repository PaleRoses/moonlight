{-# LANGUAGE DerivingStrategies #-}

-- |
-- The mapping cone with tracked homotopy equivalence, and the distinguished
-- triangle it generates.
--
-- 'minimizeComplex' discards the equivalence between input and minimal
-- model, which is fine for objects but fatal for the triangle: the maps
-- @g : B → cone f@ and @h : cone f → A⟦1⟧@ live against the /minimized/
-- cone, so the equivalence must be carried through every cancellation. This
-- module therefore runs its own reduction — the Gaussian elimination lemma,
-- applied only to label-diagonal invertible entries — threading the
-- projection @φ@ and inclusion @ψ@ of the homotopy equivalence through each
-- step. Cancelling an invertible entry @α@ of @d_i@ in block form
-- @[[α,β],[γ,δ]]@ replaces @d_i@ by the Schur complement @δ − γα⁻¹β@,
-- composes @φ@ with @[−γα⁻¹, id]@ at the target and the coordinate
-- projection at the source, and @ψ@ with @[−α⁻¹β; id]@ at the source and
-- the coordinate inclusion at the target; the adjacent differentials lose
-- the cancelled row and column with no correction. Each step removes one
-- source and one target coordinate, so the loop terminates, and over a
-- field it exits only when every same-label entry is zero — which is
-- exactly 'isMinimal'.
--
-- The machinery consumes 'RawComplex' presentations — expanded labels and
-- dense differentials with zero padding outside the window — so it serves
-- both sealed endpoints (via 'rawFromDerived') and unsealed presentations
-- such as canonical-truncation tails. Nothing here is trusted: every result
-- crosses the composable-complex gate once, then the final derived seal checks
-- only site membership, order variance, and minimality. Both triangle maps
-- re-enter through the 'DerivedMap' gate, so a fabricated sign or a wrong
-- Schur formula is rejected by the carriers, not shipped.
module Moonlight.Derived.Pure.Gluing.Cone
  ( cone
  , coneWithWitness
  , coneOfRaw
  , quasiIsoCone
  , Triangle (..)
  , mkTriangleOf
  , rotateTriangle
  , RawComplex (..)
  , rawFromDerived
  , rawLabelsAt
  , rawDiffAt
  ) where

import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IM
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Core (Field (..), MoonlightError (..))
import Moonlight.Derived.Pure.Failure (derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.Site.DerivedMap
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  )

type Triangle :: Type -> Type
data Triangle a = Triangle
  { triA :: !(Derived a)
  , triB :: !(Derived a)
  , triC :: !(Derived a)
  , triF :: !(DerivedMap a)
  , triG :: !(DerivedMap a)
  , triH :: !(DerivedMap a)
  } deriving stock (Eq, Show)

-- | An unsealed complex presentation: object degrees run from 'rcStart',
-- one expanded label vector per object, one dense differential per adjacent
-- pair. Input data for the cone machinery, never a carrier.
type RawComplex :: Type -> Type
data RawComplex a = RawComplex
  { rcStart :: !Int
  , rcLabels :: !(Vector (Vector FinObjectId))
  , rcDiffs :: !(Vector (DenseMat a))
  }

rawFromDerived :: Num a => Derived a -> RawComplex a
rawFromDerived derivedValue =
  RawComplex
    { rcStart = fst (derivedObjectWindow derivedValue)
    , rcLabels = V.fromList (fmap axisLabelsExpanded (derivedObjectAxes derivedValue))
    , rcDiffs = fmap collapseBlockedDense (injectiveComplexDiffs (derivedInjectiveComplex derivedValue))
    }

rawLabelsAt :: RawComplex a -> Int -> Vector FinObjectId
rawLabelsAt RawComplex {rcStart, rcLabels} degreeValue =
  case rcLabels V.!? (degreeValue - rcStart) of
    Just labelsValue -> labelsValue
    Nothing -> V.empty

rawDiffAt :: Num a => RawComplex a -> Int -> DenseMat a
rawDiffAt rawValue@RawComplex {rcStart, rcDiffs} degreeValue =
  case rcDiffs V.!? (degreeValue - rcStart) of
    Just diffValue -> diffValue
    Nothing ->
      zeroMat
        (V.length (rawLabelsAt rawValue (degreeValue + 1)))
        (V.length (rawLabelsAt rawValue degreeValue))

type Reduction :: Type -> Type
data Reduction a = Reduction
  { redLabels :: !(Vector (Vector FinObjectId))
  , redDiffs :: !(Vector (DenseMat a))
  , redPhi :: !(Vector (DenseMat a))
  , redPsi :: !(Vector (DenseMat a))
  }

-- | The minimal mapping cone of a chain map, sealed and normalized over the
-- given site.
cone :: (Eq a, Field a, Num a) => DerivedMap a -> Either MoonlightError (Derived a)
cone mapValue =
  fmap (\(coneValue, _, _) -> coneValue) (coneWithWitness mapValue)

-- | The cone as the quasi-isomorphism witness: @f@ is a quasi-isomorphism
-- iff this complex is acyclic. Provided at the gluing tier so consumers with
-- their own vanishing test can state the criterion without the morse tier.
quasiIsoCone :: (Eq a, Field a, Num a) => DerivedMap a -> Either MoonlightError (Derived a)
quasiIsoCone = cone

-- | The minimal cone of a raw presentation pair joined by raw components
-- indexed by absolute degree. The components must have shape
-- @|target labels at n| × |source labels at n|@; degrees absent from the
-- map contribute zero blocks via the padding accessors.
coneOfRaw ::
  (Eq a, Field a, Num a) =>
  DerivedPoset -> RawComplex a -> RawComplex a -> (Int -> DenseMat a) -> Either MoonlightError (Derived a)
coneOfRaw posetValue sourceRaw targetRaw componentAt =
  reduceCone posetValue sourceRaw targetRaw componentAt
    >>= sealReduction posetValue coneLow
  where
    coneLow = coneLowOf sourceRaw targetRaw

-- | The minimal cone together with the two connecting maps of its
-- distinguished triangle: @(cone f, g : B → cone f, h : cone f → A⟦1⟧)@.
coneWithWitness ::
  (Eq a, Field a, Num a) =>
  DerivedMap a -> Either MoonlightError (Derived a, DerivedMap a, DerivedMap a)
coneWithWitness mapValue = do
  reducedValue <-
    reduceCone
      posetValue
      sourceRaw
      targetRaw
      (collapseBlockedDense . derivedMapComponentAt mapValue)
  coneValue <- sealReduction posetValue coneLow reducedValue
  inclusionParts <- first derivedFailureToMoonlightError (inclusionComponents reducedValue coneValue)
  projectionParts <- first derivedFailureToMoonlightError (projectionComponents reducedValue coneValue)
  inclusionMap <- mkDerivedMap targetValue coneValue inclusionParts
  projectionMap <- mkDerivedMap coneValue (shift 1 sourceValue) projectionParts
  pure (coneValue, inclusionMap, projectionMap)
  where
    sourceValue = derivedMapSource mapValue
    targetValue = derivedMapTarget mapValue
    posetValue = derivedPoset sourceValue
    sourceRaw = rawFromDerived sourceValue
    targetRaw = rawFromDerived targetValue
    (sourceLow, sourceHigh) = derivedObjectWindow sourceValue
    (targetLow, targetHigh) = derivedObjectWindow targetValue
    coneLow = coneLowOf sourceRaw targetRaw
    coneHigh = coneHighOf sourceRaw targetRaw

    survivingDegrees coneValue lowDegree highDegree =
      let (coneWindowLow, coneWindowHigh) = derivedObjectWindow coneValue
       in [max lowDegree (max coneLow coneWindowLow) .. min highDegree (min coneHigh coneWindowHigh)]

    inclusionComponents reducedValue coneValue =
      fmap IM.fromList $
        traverse
          ( \degreeValue ->
              fmap ((,) degreeValue) $
                fromExpandedChecked
                  (redLabels reducedValue V.! (degreeValue - coneLow))
                  (rawLabelsAt targetRaw degreeValue)
                  ( let phiValue = redPhi reducedValue V.! (degreeValue - coneLow)
                        topColCount = V.length (rawLabelsAt sourceRaw (degreeValue + 1))
                     in mkDense
                          (dmRows phiValue)
                          (V.length (rawLabelsAt targetRaw degreeValue))
                          (\rowIndex colIndex -> matIndex phiValue rowIndex (topColCount + colIndex))
                  )
          )
          (survivingDegrees coneValue targetLow targetHigh)

    projectionComponents reducedValue coneValue =
      fmap IM.fromList $
        traverse
          ( \degreeValue ->
              fmap ((,) degreeValue) $
                fromExpandedChecked
                  (rawLabelsAt sourceRaw (degreeValue + 1))
                  (redLabels reducedValue V.! (degreeValue - coneLow))
                  ( let psiValue = redPsi reducedValue V.! (degreeValue - coneLow)
                     in mkDense
                          (V.length (rawLabelsAt sourceRaw (degreeValue + 1)))
                          (dmCols psiValue)
                          (matIndex psiValue)
                  )
          )
          (survivingDegrees coneValue (sourceLow - 1) (sourceHigh - 1))

-- | The distinguished triangle @A → B → cone f → A⟦1⟧@ of a chain map, with
-- all three maps validated against the sealed carriers.
mkTriangleOf :: (Eq a, Field a, Num a) => DerivedMap a -> Either MoonlightError (Triangle a)
mkTriangleOf mapValue =
  fmap
    ( \(coneValue, inclusionMap, projectionMap) ->
        Triangle
          { triA = derivedMapSource mapValue
          , triB = derivedMapTarget mapValue
          , triC = coneValue
          , triF = mapValue
          , triG = inclusionMap
          , triH = projectionMap
          }
    )
    (coneWithWitness mapValue)

-- | The rotation @(B, C, cone g)@ of a distinguished triangle, rebuilt from
-- its second map.
rotateTriangle :: (Eq a, Field a, Num a) => Triangle a -> Either MoonlightError (Triangle a)
rotateTriangle triangleValue =
  mkTriangleOf (triG triangleValue)

coneLowOf :: RawComplex a -> RawComplex a -> Int
coneLowOf sourceRaw targetRaw =
  min (rcStart sourceRaw - 1) (rcStart targetRaw)

coneHighOf :: RawComplex a -> RawComplex a -> Int
coneHighOf sourceRaw targetRaw =
  max (rcStart sourceRaw + V.length (rcLabels sourceRaw) - 2) (rcStart targetRaw + V.length (rcLabels targetRaw) - 1)

reduceCone ::
  (Eq a, Field a, Num a) =>
  DerivedPoset -> RawComplex a -> RawComplex a -> (Int -> DenseMat a) -> Either MoonlightError (Reduction a)
reduceCone posetValue sourceRaw targetRaw componentAt =
  if squaresVanish
    then
      Right $
        canonicalOrder
          (derivedPosetTopoAsc posetValue)
          ( reduceFully
              Reduction
                { redLabels = initialLabels
                , redDiffs = initialDiffs
                , redPhi = fmap (identMat . V.length) initialLabels
                , redPsi = fmap (identMat . V.length) initialLabels
                }
          )
    else
      Left
        ( InvariantViolation
            "cone: assembled differential does not square to zero; the input complexes or components do not form a lawful chain map"
        )
  where
    coneLow = coneLowOf sourceRaw targetRaw
    coneHigh = coneHighOf sourceRaw targetRaw

    initialDiffs = V.fromList [ coneDiffAt degreeValue | degreeValue <- [coneLow .. coneHigh - 1] ]

    squaresVanish =
      all
        (\diffIndex -> isZeroMat (matMul (initialDiffs V.! (diffIndex + 1)) (initialDiffs V.! diffIndex)))
        [0 .. V.length initialDiffs - 2]

    initialLabels =
      V.fromList
        [ rawLabelsAt sourceRaw (degreeValue + 1) <> rawLabelsAt targetRaw degreeValue
        | degreeValue <- [coneLow .. coneHigh]
        ]

    coneDiffAt degreeValue =
      let topRowCount = V.length (rawLabelsAt sourceRaw (degreeValue + 2))
          botRowCount = V.length (rawLabelsAt targetRaw (degreeValue + 1))
          topColCount = V.length (rawLabelsAt sourceRaw (degreeValue + 1))
          botColCount = V.length (rawLabelsAt targetRaw degreeValue)
          sourceDiff = rawDiffAt sourceRaw (degreeValue + 1)
          targetDiff = rawDiffAt targetRaw degreeValue
          mapDense = componentAt (degreeValue + 1)
       in mkDense
            (topRowCount + botRowCount)
            (topColCount + botColCount)
            ( \rowIndex colIndex ->
                if rowIndex < topRowCount
                  then
                    if colIndex < topColCount
                      then negate (matIndex sourceDiff rowIndex colIndex)
                      else 0
                  else
                    if colIndex < topColCount
                      then matIndex mapDense (rowIndex - topRowCount) colIndex
                      else matIndex targetDiff (rowIndex - topRowCount) (colIndex - topColCount)
            )

sealReduction :: (Eq a, Num a) => DerivedPoset -> Int -> Reduction a -> Either MoonlightError (Derived a)
sealReduction posetValue startDegree reductionValue = do
  blockedDiffs <-
    first derivedFailureToMoonlightError $
      traverse
        ( \diffIndex ->
            fromExpandedChecked
              (redLabels reductionValue V.! (diffIndex + 1))
              (redLabels reductionValue V.! diffIndex)
              (redDiffs reductionValue V.! diffIndex)
        )
        (V.enumFromN 0 (V.length (redDiffs reductionValue)))
  composableComplex <-
    first derivedFailureToMoonlightError (mkComposableInjectiveComplex startDegree blockedDiffs)
  first derivedFailureToMoonlightError (mkNormalizedDerivedFromComposableChecked posetValue composableComplex)

reduceFully :: (Eq a, Field a, Num a) => Reduction a -> Reduction a
reduceFully reductionValue =
  case findPivot reductionValue of
    Nothing -> reductionValue
    Just pivotValue -> reduceFully (eliminate pivotValue reductionValue)

findPivot :: Field a => Reduction a -> Maybe (Int, Int, Int, a)
findPivot Reduction {redLabels, redDiffs} =
  listToMaybe
    [ (diffIndex, rowIndex, colIndex, inverseValue)
    | diffIndex <- [0 .. V.length redDiffs - 1]
    , let diffValue = redDiffs V.! diffIndex
          rowLabels = redLabels V.! (diffIndex + 1)
          colLabels = redLabels V.! diffIndex
    , rowIndex <- [0 .. dmRows diffValue - 1]
    , colIndex <- [0 .. dmCols diffValue - 1]
    , rowLabels V.! rowIndex == colLabels V.! colIndex
    , Just inverseValue <- [tryInv (matIndex diffValue rowIndex colIndex)]
    ]

eliminate :: Num a => (Int, Int, Int, a) -> Reduction a -> Reduction a
eliminate (diffIndex, pivotRow, pivotCol, inverseValue) Reduction {redLabels, redDiffs, redPhi, redPsi} =
  Reduction
    { redLabels =
        redLabels
          V.// [ (diffIndex, deleteAt pivotCol (redLabels V.! diffIndex))
               , (diffIndex + 1, deleteAt pivotRow (redLabels V.! (diffIndex + 1)))
               ]
    , redDiffs =
        redDiffs
          V.// ( [ (diffIndex, schurValue) ]
                  <> [ (diffIndex - 1, deleteDenseRow pivotCol (redDiffs V.! (diffIndex - 1))) | diffIndex > 0 ]
                  <> [ (diffIndex + 1, deleteDenseCol pivotRow (redDiffs V.! (diffIndex + 1))) | diffIndex + 1 < V.length redDiffs ]
               )
    , redPhi =
        redPhi
          V.// [ (diffIndex, deleteDenseRow pivotCol (redPhi V.! diffIndex))
               , (diffIndex + 1, correctedPhi)
               ]
    , redPsi =
        redPsi
          V.// [ (diffIndex, correctedPsi)
               , (diffIndex + 1, deleteDenseCol pivotRow (redPsi V.! (diffIndex + 1)))
               ]
    }
  where
    diffValue = redDiffs V.! diffIndex

    schurValue =
      mkDense
        (dmRows diffValue - 1)
        (dmCols diffValue - 1)
        ( \rowIndex colIndex ->
            matIndex diffValue (skipIndex pivotRow rowIndex) (skipIndex pivotCol colIndex)
              - matIndex diffValue (skipIndex pivotRow rowIndex) pivotCol
                * inverseValue
                * matIndex diffValue pivotRow (skipIndex pivotCol colIndex)
        )

    correctedPhi =
      let phiValue = redPhi V.! (diffIndex + 1)
       in mkDense
            (dmRows phiValue - 1)
            (dmCols phiValue)
            ( \rowIndex colIndex ->
                matIndex phiValue (skipIndex pivotRow rowIndex) colIndex
                  - matIndex diffValue (skipIndex pivotRow rowIndex) pivotCol
                    * inverseValue
                    * matIndex phiValue pivotRow colIndex
            )

    correctedPsi =
      let psiValue = redPsi V.! diffIndex
       in mkDense
            (dmRows psiValue)
            (dmCols psiValue - 1)
            ( \rowIndex colIndex ->
                matIndex psiValue rowIndex (skipIndex pivotCol colIndex)
                  - matIndex psiValue rowIndex pivotCol
                    * inverseValue
                    * matIndex diffValue pivotRow (skipIndex pivotCol colIndex)
            )

canonicalOrder :: Vector FinObjectId -> Reduction a -> Reduction a
canonicalOrder canonicalNodes Reduction {redLabels, redDiffs, redPhi, redPsi} =
  Reduction
    { redLabels = V.zipWith (\permValue labelsValue -> fmap (labelsValue V.!) permValue) permutations redLabels
    , redDiffs =
        V.imap
          ( \diffIndex diffValue ->
              mkDense
                (dmRows diffValue)
                (dmCols diffValue)
                ( \rowIndex colIndex ->
                    matIndex
                      diffValue
                      (permutations V.! (diffIndex + 1) V.! rowIndex)
                      (permutations V.! diffIndex V.! colIndex)
                )
          )
          redDiffs
    , redPhi =
        V.zipWith
          ( \permValue phiValue ->
              mkDense
                (dmRows phiValue)
                (dmCols phiValue)
                (\rowIndex colIndex -> matIndex phiValue (permValue V.! rowIndex) colIndex)
          )
          permutations
          redPhi
    , redPsi =
        V.zipWith
          ( \permValue psiValue ->
              mkDense
                (dmRows psiValue)
                (dmCols psiValue)
                (\rowIndex colIndex -> matIndex psiValue rowIndex (permValue V.! colIndex))
          )
          permutations
          redPsi
    }
  where
    positionMap =
      IM.fromList (zip (fmap unFinObjectId (V.toList canonicalNodes)) [0 :: Int ..])

    permutations =
      fmap
        ( \labelsValue ->
            V.fromList
              ( sortOn
                  (\labelIndex -> IM.findWithDefault maxBound (unFinObjectId (labelsValue V.! labelIndex)) positionMap)
                  [0 .. V.length labelsValue - 1]
              )
        )
        redLabels

mkDense :: Int -> Int -> (Int -> Int -> a) -> DenseMat a
mkDense rowCount colCount entryAt =
  DenseMat rowCount colCount (V.generate rowCount (\rowIndex -> V.generate colCount (entryAt rowIndex)))

deleteAt :: Int -> Vector b -> Vector b
deleteAt dropIndex vectorValue =
  V.generate (V.length vectorValue - 1) (\keptIndex -> vectorValue V.! skipIndex dropIndex keptIndex)

deleteDenseRow :: Int -> DenseMat a -> DenseMat a
deleteDenseRow dropIndex denseValue =
  mkDense (dmRows denseValue - 1) (dmCols denseValue) (\rowIndex -> matIndex denseValue (skipIndex dropIndex rowIndex))

deleteDenseCol :: Int -> DenseMat a -> DenseMat a
deleteDenseCol dropIndex denseValue =
  mkDense (dmRows denseValue) (dmCols denseValue - 1) (\rowIndex colIndex -> matIndex denseValue rowIndex (skipIndex dropIndex colIndex))

skipIndex :: Int -> Int -> Int
skipIndex dropIndex keptIndex =
  if keptIndex >= dropIndex then keptIndex + 1 else keptIndex
