{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Homology
  ( LiftedCosheafChainTerm (..),
    CosheafHomologyWitness (..),
    CosheafHomologyResult (..),
    CosheafHomologyFailure (..),
    cosheafIntegralHomology,
    cosheafIntegralHomologyResult,
    cosheafIntegralHomologyResultWithRepresentatives,
    liftCosheafRepresentative,
    liftCosheafRepresentatives,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Cosheaf.Chain
  ( CosheafChainCell,
    CosheafChainFailure,
    PreparedFiniteCosheafChain,
    cosheafChainCellByBasisIndex,
    pfccChainComplex,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyFailure,
    HomologyGroup,
    RepresentativeChain (..),
    integralHomologyGroupsOf,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )

-- | One homology representative term lifted from a raw finite-chain basis index
-- back to the cosheaf chain cell that owns the domain meaning.
type LiftedCosheafChainTerm :: Type -> Type -> Type -> Type
data LiftedCosheafChainTerm site value coefficient = LiftedCosheafChainTerm
  { lcctCoefficient :: !coefficient,
    lcctBasisIndex :: !Int,
    lcctCell :: !(CosheafChainCell (SiteObject site) (SiteMorphism site) value)
  }

deriving stock instance
  (Eq coefficient, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (LiftedCosheafChainTerm site value coefficient)

deriving stock instance
  (Show coefficient, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (LiftedCosheafChainTerm site value coefficient)

-- | A lifted homology witness. Raw integer basis coordinates do not cross this
-- boundary without being glued back through the prepared cosheaf basis table.
type CosheafHomologyWitness :: Type -> Type -> Type -> Type
data CosheafHomologyWitness site value coefficient = CosheafHomologyWitness
  { chwDegree :: !HomologicalDegree,
    chwRepresentativeTerms :: ![LiftedCosheafChainTerm site value coefficient]
  }

deriving stock instance
  (Eq coefficient, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (CosheafHomologyWitness site value coefficient)

deriving stock instance
  (Show coefficient, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (CosheafHomologyWitness site value coefficient)

-- | Derived cosheaf homology result. The prepared chain plan remains the source
-- of truth; homology groups and lifted witnesses are views over that plan.
type CosheafHomologyResult :: Type -> Type -> Type -> Type
data CosheafHomologyResult site value coefficient = CosheafHomologyResult
  { chrPlan :: !(PreparedFiniteCosheafChain site value),
    chrGroups :: ![HomologyGroup Integer],
    chrWitnessesByDegree :: !(IntMap [CosheafHomologyWitness site value coefficient])
  }

type CosheafHomologyFailure :: Type -> Type -> Type -> Type
data CosheafHomologyFailure obj mor value
  = CosheafHomologyChainFailed !(CosheafChainFailure obj mor value)
  | CosheafHomologyComputationFailed !HomologyFailure
  | CosheafHomologyRepresentativeDegreeMismatch !HomologicalDegree !HomologicalDegree
  deriving stock (Eq, Show)

cosheafIntegralHomology ::
  PreparedFiniteCosheafChain site value ->
  Either
    (CosheafHomologyFailure (SiteObject site) (SiteMorphism site) value)
    [HomologyGroup Integer]
cosheafIntegralHomology =
  first CosheafHomologyComputationFailed . integralHomologyGroupsOf . pfccChainComplex
{-# INLINE cosheafIntegralHomology #-}

cosheafIntegralHomologyResult ::
  PreparedFiniteCosheafChain site value ->
  Either
    (CosheafHomologyFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafHomologyResult site value Integer)
cosheafIntegralHomologyResult plan = do
  groups <- cosheafIntegralHomology plan
  Right
    CosheafHomologyResult
      { chrPlan = plan,
        chrGroups = groups,
        chrWitnessesByDegree = IntMap.empty
      }

cosheafIntegralHomologyResultWithRepresentatives ::
  PreparedFiniteCosheafChain site value ->
  [RepresentativeChain coefficient Int] ->
  Either
    (CosheafHomologyFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafHomologyResult site value coefficient)
cosheafIntegralHomologyResultWithRepresentatives plan representatives = do
  groups <- cosheafIntegralHomology plan
  witnesses <- liftCosheafRepresentatives plan representatives
  Right
    CosheafHomologyResult
      { chrPlan = plan,
        chrGroups = groups,
        chrWitnessesByDegree = witnesses
      }

liftCosheafRepresentatives ::
  PreparedFiniteCosheafChain site value ->
  [RepresentativeChain coefficient Int] ->
  Either
    (CosheafHomologyFailure (SiteObject site) (SiteMorphism site) value)
    (IntMap [CosheafHomologyWitness site value coefficient])
liftCosheafRepresentatives plan representatives =
  fmap (IntMap.fromListWith (<>)) $
    traverse liftOne representatives
  where
    liftOne representative = do
      let degreeValue@(HomologicalDegree degreeInt) =
            representativeDegree representative
      witness <- liftCosheafRepresentative degreeValue plan representative
      Right (degreeInt, [witness])

liftCosheafRepresentative ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  RepresentativeChain coefficient Int ->
  Either
    (CosheafHomologyFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafHomologyWitness site value coefficient)
liftCosheafRepresentative expectedDegree plan representative
  | representativeDegree representative /= expectedDegree =
      Left (CosheafHomologyRepresentativeDegreeMismatch expectedDegree (representativeDegree representative))
  | otherwise = do
      liftedTerms <- traverse liftTerm (representativeTerms representative)
      Right
        CosheafHomologyWitness
          { chwDegree = expectedDegree,
            chwRepresentativeTerms = liftedTerms
          }
  where
    liftTerm (coefficientValue, basisIndexValue) = do
      cellValue <-
        first CosheafHomologyChainFailed $
          cosheafChainCellByBasisIndex expectedDegree basisIndexValue plan
      Right
        LiftedCosheafChainTerm
          { lcctCoefficient = coefficientValue,
            lcctBasisIndex = basisIndexValue,
            lcctCell = cellValue
          }
