{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.H0
  ( CosheafH0CellKey (..),
    CosheafH0Agreement (..),
    CosheafH0ClassAgreement (..),
    CosheafH0AgreementReport (..),
    CosheafH0Failure (..),
    verifyCosheafH0RankAgreement,
    verifyCosheafH0ClassAgreement,
    verifyPreparedCosheafH0ClassAgreement,
    degreeZeroBoundaryEquivalence,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Cosheaf.Chain
  ( CosheafChainCell (..),
    CosheafChainFailure,
    PreparedFiniteCosheafChain,
    cosheafBoundaryIncidenceAt,
    cosheafChainCellsAtDegree,
    prepareFiniteCosheafChainFromPreparedSupport,
  )
import Moonlight.Cosheaf.Colimit
  ( CosheafColimit,
    CosheafColimitFailure,
    cosheafColimitClassKeys,
    cosheafColimitClassOf,
    finiteCosheafColimitFromPreparedSupport,
  )
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey,
  )
import Moonlight.Cosheaf.Finite
  ( FiniteCosheaf,
  )
import Moonlight.Cosheaf.Homology
  ( CosheafHomologyFailure,
    cosheafIntegralHomology,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportFailure (..),
    h0PreparedSupport,
  )
import Moonlight.Homology
  ( BoundaryEntry,
    HomologicalDegree (..),
    HomologyGroup (..),
    boundaryCoefficient,
    boundaryEntries,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )

type CosheafH0CellKey :: Type
newtype CosheafH0CellKey = CosheafH0CellKey
  { unCosheafH0CellKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey CosheafH0CellKey where
  encodeDenseKey =
    unCosheafH0CellKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    CosheafH0CellKey
  {-# INLINE decodeDenseKey #-}

-- | Rank/torsion summary only. This is useful, but it is not the class theorem.
type CosheafH0Agreement :: Type
data CosheafH0Agreement = CosheafH0Agreement
  { chaColimitClassCount :: !Int,
    chaHomologyFreeRank :: !Int,
    chaHomologyTorsionInvariants :: ![Integer]
  }
  deriving stock (Eq, Show)

-- | One H₀ class comparison, carrying the actual C₀ cells rather than a bare count.
type CosheafH0ClassAgreement :: Type -> Type -> Type -> Type
data CosheafH0ClassAgreement obj mor value = CosheafH0ClassAgreement
  { ch0ColimitClass :: !CosectionClassKey,
    ch0ChainClass :: !CosheafH0CellKey,
    ch0DegreeZeroCells :: ![CosheafChainCell obj mor value]
  }
  deriving stock (Eq, Show)

-- | The real H₀ agreement report: rank summary plus class-membership gluing.
type CosheafH0AgreementReport :: Type -> Type -> Type -> Type
data CosheafH0AgreementReport obj mor value = CosheafH0AgreementReport
  { ch0RankAgreement :: !CosheafH0Agreement,
    ch0ClassAgreements :: ![CosheafH0ClassAgreement obj mor value]
  }
  deriving stock (Eq, Show)

type CosheafH0Failure :: Type -> Type -> Type -> Type
data CosheafH0Failure obj mor value
  = CosheafH0SupportFailed !(CosheafSupportFailure obj mor value)
  | CosheafH0ChainFailed !(CosheafChainFailure obj mor value)
  | CosheafH0ColimitFailed !(CosheafColimitFailure obj mor value)
  | CosheafH0HomologyFailed !(CosheafHomologyFailure obj mor value)
  | CosheafH0GroupMissing
  | CosheafH0RankMismatch !Int !(HomologyGroup Integer)
  | CosheafH0BoundaryTargetOutsideC0 !Int !Int !Int
  | CosheafH0BoundaryColumnMalformed !Int ![(Int, Int)]
  | CosheafH0EquivalenceInvalid !EquivalenceRelationError
  | CosheafH0CellClassMissing !Int
  | CosheafH0ColimitClassSplit !CosectionClassKey ![CosheafH0CellKey]
  | CosheafH0ChainClassMergesColimitClasses !CosheafH0CellKey ![CosectionClassKey]
  deriving stock (Eq, Show)

verifyCosheafH0RankAgreement ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteCosheaf site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    CosheafH0Agreement
verifyCosheafH0RankAgreement cosheaf = do
  preparedSupport <-
    first CosheafH0SupportFailed (h0PreparedSupport cosheaf)
  plan <-
    first
      supportFailureToH0Failure
      (prepareFiniteCosheafChainFromPreparedSupport preparedSupport)
  colimit <-
    first
      CosheafH0ColimitFailed
      (finiteCosheafColimitFromPreparedSupport preparedSupport)
  verifyCosheafH0RankAgreementForPrepared colimit plan

verifyCosheafH0ClassAgreement ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteCosheaf site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    (CosheafH0AgreementReport (SiteObject site) (SiteMorphism site) value)
verifyCosheafH0ClassAgreement cosheaf = do
  preparedSupport <-
    first CosheafH0SupportFailed (h0PreparedSupport cosheaf)
  plan <-
    first
      supportFailureToH0Failure
      (prepareFiniteCosheafChainFromPreparedSupport preparedSupport)
  colimit <-
    first
      CosheafH0ColimitFailed
      (finiteCosheafColimitFromPreparedSupport preparedSupport)
  verifyPreparedCosheafH0ClassAgreement plan colimit

verifyPreparedCosheafH0ClassAgreement ::
  (Site site, Ord value) =>
  PreparedFiniteCosheafChain site value ->
  CosheafColimit site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    (CosheafH0AgreementReport (SiteObject site) (SiteMorphism site) value)
verifyPreparedCosheafH0ClassAgreement plan colimit = do
  rankAgreement <- verifyCosheafH0RankAgreementForPrepared colimit plan
  degreeZeroClasses <- degreeZeroColimitClasses plan colimit
  boundaryRelation <- degreeZeroBoundaryEquivalence plan
  classAgreements <- buildH0ClassAgreements degreeZeroClasses boundaryRelation
  Right
    CosheafH0AgreementReport
      { ch0RankAgreement = rankAgreement,
        ch0ClassAgreements = classAgreements
      }

verifyCosheafH0RankAgreementForPrepared ::
  CosheafColimit site value ->
  PreparedFiniteCosheafChain site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    CosheafH0Agreement
verifyCosheafH0RankAgreementForPrepared colimit plan = do
  homologyGroups <-
    first
      CosheafH0HomologyFailed
      (cosheafIntegralHomology plan)
  h0Group <-
    maybe
      (Left CosheafH0GroupMissing)
      Right
      (safeHead homologyGroups)
  let classCount =
        length (cosheafColimitClassKeys colimit)
      agreement =
        CosheafH0Agreement
          { chaColimitClassCount = classCount,
            chaHomologyFreeRank = freeRank h0Group,
            chaHomologyTorsionInvariants = torsionInvariants h0Group
          }
  if chaColimitClassCount agreement == chaHomologyFreeRank agreement
    && null (chaHomologyTorsionInvariants agreement)
    then Right agreement
    else Left (CosheafH0RankMismatch classCount h0Group)

degreeZeroColimitClasses ::
  (Site site, Ord value) =>
  PreparedFiniteCosheafChain site value ->
  CosheafColimit site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    (IntMap (CosheafChainCell (SiteObject site) (SiteMorphism site) value, CosectionClassKey))
degreeZeroColimitClasses plan colimit =
  IntMap.fromAscList
    <$> traverse classifyCell (zip [0 :: Int ..] (cosheafChainCellsAtDegree zeroDegree plan))
  where
    classifyCell (cellIndexValue, cell) = do
      classKey <-
        first
          CosheafH0ColimitFailed
          (cosheafColimitClassOf (cosheafChainCellRepresentative cell) colimit)
      Right (cellIndexValue, (cell, classKey))

degreeZeroBoundaryEquivalence ::
  PreparedFiniteCosheafChain site value ->
  Either
    (CosheafH0Failure (SiteObject site) (SiteMorphism site) value)
    (EquivalenceRelation CosheafH0CellKey)
degreeZeroBoundaryEquivalence plan = do
  let c0Count =
        length (cosheafChainCellsAtDegree zeroDegree plan)
      domain =
        IntSet.fromAscList [0 .. c0Count - 1]
      groupedColumns =
        boundaryEntriesBySource (boundaryEntries (cosheafBoundaryIncidenceAt oneDegree plan))
  pairs <-
    concat
      <$> traverse
        (boundaryColumnPairs c0Count)
        (IntMap.toAscList groupedColumns)
  first CosheafH0EquivalenceInvalid $
    equivalenceFromPairs domain pairs

buildH0ClassAgreements ::
  IntMap (CosheafChainCell obj mor value, CosectionClassKey) ->
  EquivalenceRelation CosheafH0CellKey ->
  Either
    (CosheafH0Failure obj mor value)
    [CosheafH0ClassAgreement obj mor value]
buildH0ClassAgreements cellClasses boundaryRelation = do
  rows <- traverse classifyRow (IntMap.toAscList cellClasses)
  let colimitClassToChainClasses =
        Map.fromListWith
          IntSet.union
          [ (colimitClass, IntSet.singleton (unCosheafH0CellKey chainClass))
          | (_cellIndexValue, _cell, colimitClass, chainClass) <- rows
          ]
      chainClassToColimitClasses =
        IntMap.fromListWith
          Set.union
          [ (unCosheafH0CellKey chainClass, Set.singleton colimitClass)
          | (_cellIndexValue, _cell, colimitClass, chainClass) <- rows
          ]
  case colimitClassSplits colimitClassToChainClasses of
    (classKey, chainClasses) : _ ->
      Left (CosheafH0ColimitClassSplit classKey chainClasses)
    [] ->
      Right ()
  case chainClassMerges chainClassToColimitClasses of
    (chainClass, classKeys) : _ ->
      Left (CosheafH0ChainClassMergesColimitClasses chainClass classKeys)
    [] ->
      Right ()
  Right (reportsFromRows rows)
  where
    classifyRow (cellIndexValue, (cell, colimitClass)) = do
      chainClass <-
        maybe
          (Left (CosheafH0CellClassMissing cellIndexValue))
          Right
          (equivalenceRepresentative boundaryRelation (CosheafH0CellKey cellIndexValue))
      Right (cellIndexValue, cell, colimitClass, chainClass)

boundaryEntriesBySource ::
  [BoundaryEntry Int] ->
  IntMap [(Int, Int)]
boundaryEntriesBySource =
  IntMap.fromListWith (<>)
    . fmap
      ( \entry ->
          ( sourceIndex entry,
            [(targetIndex entry, boundaryCoefficient entry)]
          )
      )

boundaryColumnPairs ::
  Int ->
  (Int, [(Int, Int)]) ->
  Either (CosheafH0Failure obj mor value) [(CosheafH0CellKey, CosheafH0CellKey)]
boundaryColumnPairs c0Count (columnIndexValue, rawTerms) = do
  let terms = normalizeColumnTerms rawTerms
  traverse_ (validateTargetIndex columnIndexValue c0Count . fst) terms
  case terms of
    [] ->
      Right []
    [(targetValue, coefficientValue)] ->
      Left (CosheafH0BoundaryColumnMalformed columnIndexValue [(targetValue, coefficientValue)])
    [(leftTarget, leftCoefficient), (rightTarget, rightCoefficient)]
      | leftTarget == rightTarget ->
          Right []
      | abs leftCoefficient == 1
          && abs rightCoefficient == 1
          && leftCoefficient + rightCoefficient == 0 ->
          Right [(CosheafH0CellKey leftTarget, CosheafH0CellKey rightTarget)]
      | otherwise ->
          Left (CosheafH0BoundaryColumnMalformed columnIndexValue terms)
    _ ->
      Left (CosheafH0BoundaryColumnMalformed columnIndexValue terms)

normalizeColumnTerms :: [(Int, Int)] -> [(Int, Int)]
normalizeColumnTerms =
  Map.toAscList . Map.filter (/= 0) . Map.fromListWith (+)

validateTargetIndex ::
  Int ->
  Int ->
  Int ->
  Either (CosheafH0Failure obj mor value) ()
validateTargetIndex columnIndexValue c0Count targetIndexValue =
  if targetIndexValue >= 0 && targetIndexValue < c0Count
    then Right ()
    else Left (CosheafH0BoundaryTargetOutsideC0 columnIndexValue targetIndexValue c0Count)

colimitClassSplits ::
  Map CosectionClassKey IntSet ->
  [(CosectionClassKey, [CosheafH0CellKey])]
colimitClassSplits =
  mapMaybe
    (\(classKey, chainClasses) ->
      if IntSet.size chainClasses <= 1
        then Nothing
        else Just (classKey, fmap CosheafH0CellKey (IntSet.toAscList chainClasses))
    )
    . Map.toAscList

chainClassMerges ::
  IntMap (Set CosectionClassKey) ->
  [(CosheafH0CellKey, [CosectionClassKey])]
chainClassMerges =
  mapMaybe
    (\(chainClassKey, classKeys) ->
      if Set.size classKeys <= 1
        then Nothing
        else Just (CosheafH0CellKey chainClassKey, Set.toAscList classKeys)
    )
    . IntMap.toAscList

reportsFromRows ::
  [(Int, CosheafChainCell obj mor value, CosectionClassKey, CosheafH0CellKey)] ->
  [CosheafH0ClassAgreement obj mor value]
reportsFromRows rows =
  mapMaybe reportForClass (Map.toAscList rowsByColimitClass)
  where
    rowsByColimitClass =
      Map.fromListWith
        (<>)
        [ (colimitClass, [(chainClass, cell)])
        | (_cellIndexValue, cell, colimitClass, chainClass) <- rows
        ]

    reportForClass ::
      (CosectionClassKey, [(CosheafH0CellKey, CosheafChainCell obj mor value)]) ->
      Maybe (CosheafH0ClassAgreement obj mor value)
    reportForClass (colimitClass, classRows) =
      case classRows of
        [] ->
          Nothing
        (chainClass, _) : _ ->
          Just
            CosheafH0ClassAgreement
              { ch0ColimitClass = colimitClass,
                ch0ChainClass = chainClass,
                ch0DegreeZeroCells = fmap snd classRows
              }

zeroDegree :: HomologicalDegree
zeroDegree =
  HomologicalDegree 0

oneDegree :: HomologicalDegree
oneDegree =
  HomologicalDegree 1

safeHead :: [a] -> Maybe a
safeHead values =
  case values of
    firstValue : _ ->
      Just firstValue
    [] ->
      Nothing

supportFailureToH0Failure ::
  CosheafSupportFailure obj mor value ->
  CosheafH0Failure obj mor value
supportFailureToH0Failure supportFailure =
  case supportFailure of
    CosheafSupportChainFailed chainFailure ->
      CosheafH0ChainFailed chainFailure
    _otherSupportFailure ->
      CosheafH0SupportFailed supportFailure
