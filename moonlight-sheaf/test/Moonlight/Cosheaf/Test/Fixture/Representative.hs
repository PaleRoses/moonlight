{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Test.Fixture.Representative
  ( RepresentativeBuildFailure (..),
    RepresentativeBoundaryTerm (..),
    representativeFromCells,
    representativeVector,
    findUniqueCellAtDegree,
    chainCellObjectPath,
    applyIntegralBoundary,
    boundaryOfRepresentative,
    assertRepresentativeCycle,
    liftedWitnessSupportKeys,
    liftedWitnessNerveSupport,
    liftedWitnessCellPaths,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf.Chain
  ( CosheafChainBasisKey,
    CosheafChainCell (..),
    CosheafNerveChain,
    PreparedFiniteCosheafChain,
    cosheafBoundaryIncidenceAt,
    cosheafChainBasisIndexOf,
    cosheafChainCellByBasisIndex,
    cosheafChainCellsAtDegree,
    cosheafChainCellKey,
    cosheafChainCellNerveChain,
    cosheafNerveChainMorphisms,
    cosheafNerveChainSourceObject,
  )
import Moonlight.Cosheaf.Homology
  ( CosheafHomologyWitness (..),
    LiftedCosheafChainTerm (..),
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree (..),
    RepresentativeChain (..),
    boundaryCoefficient,
    boundaryEntries,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type RepresentativeBuildFailure :: Type -> Type -> Type -> Type
data RepresentativeBuildFailure obj mor value
  = RepresentativeBasisKeyMissing !HomologicalDegree !CosheafChainBasisKey
  | RepresentativeCellNotFound !HomologicalDegree !String
  | RepresentativeCellAmbiguous !HomologicalDegree !String ![CosheafChainCell obj mor value]
  | RepresentativeBoundaryCellMissing !HomologicalDegree !Int
  | RepresentativeBoundaryNonZero !HomologicalDegree ![RepresentativeBoundaryTerm obj mor value]

type RepresentativeBoundaryTerm :: Type -> Type -> Type -> Type
data RepresentativeBoundaryTerm obj mor value = RepresentativeBoundaryTerm
  { rbtCoefficient :: !Integer,
    rbtBasisIndex :: !Int,
    rbtCell :: !(CosheafChainCell obj mor value)
  }
  deriving stock (Eq, Show)

deriving stock instance
  (Eq obj, Eq mor, Eq value) =>
  Eq (RepresentativeBuildFailure obj mor value)

deriving stock instance
  (Show obj, Show mor, Show value) =>
  Show (RepresentativeBuildFailure obj mor value)

representativeFromCells ::
  HomologicalDegree ->
  [(Integer, CosheafChainCell (SiteObject site) (SiteMorphism site) value)] ->
  PreparedFiniteCosheafChain site value ->
  Either
    (RepresentativeBuildFailure (SiteObject site) (SiteMorphism site) value)
    (RepresentativeChain Integer Int)
representativeFromCells degreeValue terms plan = do
  indexedTerms <-
    foldr
      insertTerm
      (Right Map.empty)
      terms
  Right
    RepresentativeChain
      { representativeDegree = degreeValue,
        representativeTerms =
          [ (coefficientValue, basisIndexValue)
          | (basisIndexValue, coefficientValue) <- Map.toAscList indexedTerms,
            coefficientValue /= 0
          ]
      }
  where
    insertTerm (coefficientValue, cellValue) accumulated
      | coefficientValue == 0 =
          accumulated
      | otherwise = do
          current <- accumulated
          basisIndexValue <-
            maybe
              (Left (RepresentativeBasisKeyMissing degreeValue (cosheafChainCellKey cellValue)))
              Right
              (cosheafChainBasisIndexOf degreeValue (cosheafChainCellKey cellValue) plan)
          Right (Map.insertWith (+) basisIndexValue coefficientValue current)

representativeVector ::
  (Eq coefficient, Num coefficient) =>
  RepresentativeChain coefficient Int ->
  Map Int coefficient
representativeVector representativeValue =
  Map.filter (/= 0) $
    Map.fromListWith
      (+)
      [ (basisIndexValue, coefficientValue)
      | (coefficientValue, basisIndexValue) <- representativeTerms representativeValue
      ]
{-# INLINE representativeVector #-}

findUniqueCellAtDegree ::
  HomologicalDegree ->
  String ->
  (CosheafChainCell (SiteObject site) (SiteMorphism site) value -> Bool) ->
  PreparedFiniteCosheafChain site value ->
  Either
    (RepresentativeBuildFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafChainCell (SiteObject site) (SiteMorphism site) value)
findUniqueCellAtDegree degreeValue label predicate plan =
  case filter predicate (cosheafChainCellsAtDegree degreeValue plan) of
    [] ->
      Left (RepresentativeCellNotFound degreeValue label)
    [cellValue] ->
      Right cellValue
    cells ->
      Left (RepresentativeCellAmbiguous degreeValue label cells)

chainCellObjectPath :: CosheafChainCell obj mor value -> [obj]
chainCellObjectPath cellValue =
  cosheafNerveChainSourceObject nerveValue
    : fmap cmTarget (cosheafNerveChainMorphisms nerveValue)
  where
    nerveValue =
      cosheafChainCellNerveChain cellValue
{-# INLINE chainCellObjectPath #-}

applyIntegralBoundary ::
  BoundaryIncidence Int ->
  Map Int Integer ->
  Map Int Integer
applyIntegralBoundary incidenceValue sourceVector =
  Map.filter (/= 0) $
    Map.fromListWith
      (+)
      [ (targetIndex entry, targetValue)
      | entry <- boundaryEntries incidenceValue,
        Just sourceValue <- [Map.lookup (sourceIndex entry) sourceVector],
        let targetValue =
              fromIntegral (boundaryCoefficient entry) * sourceValue,
        targetValue /= 0
      ]
{-# INLINE applyIntegralBoundary #-}

boundaryOfRepresentative ::
  PreparedFiniteCosheafChain site value ->
  RepresentativeChain Integer Int ->
  Map Int Integer
boundaryOfRepresentative plan representativeValue =
  applyIntegralBoundary
    (cosheafBoundaryIncidenceAt (representativeDegree representativeValue) plan)
    (representativeVector representativeValue)
{-# INLINE boundaryOfRepresentative #-}

assertRepresentativeCycle ::
  PreparedFiniteCosheafChain site value ->
  RepresentativeChain Integer Int ->
  Either
    (RepresentativeBuildFailure (SiteObject site) (SiteMorphism site) value)
    ()
assertRepresentativeCycle plan representativeValue =
  let boundaryValue =
        boundaryOfRepresentative plan representativeValue
   in if Map.null boundaryValue
        then Right ()
        else do
          diagnosticTerms <-
            traverse
              (boundaryDiagnosticTerm plan (boundaryTargetDegree (representativeDegree representativeValue)))
              (Map.toAscList boundaryValue)
          Left (RepresentativeBoundaryNonZero (representativeDegree representativeValue) diagnosticTerms)

boundaryTargetDegree :: HomologicalDegree -> HomologicalDegree
boundaryTargetDegree (HomologicalDegree degreeInt) =
  HomologicalDegree (degreeInt - 1)
{-# INLINE boundaryTargetDegree #-}

boundaryDiagnosticTerm ::
  PreparedFiniteCosheafChain site value ->
  HomologicalDegree ->
  (Int, Integer) ->
  Either
    (RepresentativeBuildFailure (SiteObject site) (SiteMorphism site) value)
    (RepresentativeBoundaryTerm (SiteObject site) (SiteMorphism site) value)
boundaryDiagnosticTerm plan targetDegree (basisIndexValue, coefficientValue) =
  case cosheafChainCellByBasisIndex targetDegree basisIndexValue plan of
    Right cellValue ->
      Right
        RepresentativeBoundaryTerm
          { rbtCoefficient = coefficientValue,
            rbtBasisIndex = basisIndexValue,
            rbtCell = cellValue
          }
    Left _failure ->
      Left (RepresentativeBoundaryCellMissing targetDegree basisIndexValue)

liftedWitnessSupportKeys ::
  CosheafHomologyWitness site value Integer ->
  [(Integer, CosheafChainBasisKey)]
liftedWitnessSupportKeys witnessValue =
  [ (lcctCoefficient termValue, cosheafChainCellKey (lcctCell termValue))
  | termValue <- chwRepresentativeTerms witnessValue
  ]
{-# INLINE liftedWitnessSupportKeys #-}

liftedWitnessNerveSupport ::
  CosheafHomologyWitness site value Integer ->
  [(Integer, CosheafNerveChain (SiteObject site) (SiteMorphism site))]
liftedWitnessNerveSupport witnessValue =
  [ (lcctCoefficient termValue, cosheafChainCellNerveChain (lcctCell termValue))
  | termValue <- chwRepresentativeTerms witnessValue
  ]
{-# INLINE liftedWitnessNerveSupport #-}

liftedWitnessCellPaths ::
  CosheafHomologyWitness site value Integer ->
  [(Integer, [SiteObject site])]
liftedWitnessCellPaths witnessValue =
  [ (lcctCoefficient termValue, chainCellObjectPath (lcctCell termValue))
  | termValue <- chwRepresentativeTerms witnessValue
  ]
{-# INLINE liftedWitnessCellPaths #-}
