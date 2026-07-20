{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Homology.Cover
  ( CoverHomologyArtifact (..),
    CoverHomologyFailure (..),
    coverHomology,
    coverHomologyOfPreparedChain,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Cosheaf.Chain.Cover
  ( CoverBoundaryProvenance,
    CoverChainFailure,
    CoverChainSpec,
    CoverIntersectionCell,
    CoverNervePlan,
    ccsNervePlan,
    prepareCoverCosheafChain,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps,
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( PreparedCosheafChain,
    pccChainComplex,
  )
import Moonlight.Homology
  ( HomologyBackend,
    HomologyBackendTag,
    HomologyFailure,
    HomologyGroup,
    homologyBackendTag,
    runHomologyBackend,
  )

type CoverHomologyArtifact :: Type -> Type -> Type -> Type -> Type -> Type
data CoverHomologyArtifact obj mor chainCoeff groupCoeff provenance = CoverHomologyArtifact
  { chaBackend :: !HomologyBackendTag,
    chaNervePlan :: !(CoverNervePlan obj mor),
    chaChain :: !(PreparedCosheafChain (CoverNervePlan obj mor) (CoverIntersectionCell obj mor) chainCoeff provenance),
    chaGroupsByDegree :: !(IntMap (HomologyGroup groupCoeff))
  }

type CoverHomologyFailure :: Type -> Type -> Type -> Type -> Type
data CoverHomologyFailure obj mor chainCoeff coreFailure
  = CoverHomologyChainFailed !(CoverChainFailure obj mor chainCoeff coreFailure)
  | CoverHomologyBackendFailed !HomologyBackendTag !HomologyFailure
  deriving stock (Eq, Show)

coverHomology ::
  (Ord obj, Ord mor, Eq chainCoeff, Num chainCoeff, Semiring chainCoeff) =>
  HomologyBackend chainCoeff groupCoeff ->
  CoefficientOps chainCoeff ->
  CoverChainSpec obj mor chainCoeff provenance coreFailure ->
  Either
    (CoverHomologyFailure obj mor chainCoeff coreFailure)
    (CoverHomologyArtifact obj mor chainCoeff groupCoeff (CoverBoundaryProvenance obj mor chainCoeff provenance))
coverHomology backend coefficientOps spec = do
  chain <- first CoverHomologyChainFailed (prepareCoverCosheafChain coefficientOps spec)
  coverHomologyOfPreparedChain backend (ccsNervePlan spec) chain

coverHomologyOfPreparedChain ::
  HomologyBackend chainCoeff groupCoeff ->
  CoverNervePlan obj mor ->
  PreparedCosheafChain (CoverNervePlan obj mor) (CoverIntersectionCell obj mor) chainCoeff provenance ->
  Either
    (CoverHomologyFailure obj mor chainCoeff coreFailure)
    (CoverHomologyArtifact obj mor chainCoeff groupCoeff provenance)
coverHomologyOfPreparedChain backend nervePlan chain = do
  groups <-
    first (CoverHomologyBackendFailed backendTag) $
      runHomologyBackend backend (pccChainComplex chain)
  pure
    CoverHomologyArtifact
      { chaBackend = backendTag,
        chaNervePlan = nervePlan,
        chaChain = chain,
        chaGroupsByDegree = IntMap.fromAscList (zip [0 ..] groups)
      }
  where
    backendTag =
      homologyBackendTag backend
