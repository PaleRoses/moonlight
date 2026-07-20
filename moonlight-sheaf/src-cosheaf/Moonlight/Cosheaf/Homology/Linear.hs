{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Homology.Linear
  ( LinearCosheafHomologyArtifact (..),
    LinearCosheafHomologyFailure (..),
    linearCosheafHomology,
    homologyGroupsByDegree,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
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

type LinearCosheafHomologyArtifact :: Type -> Type -> Type -> Type -> Type -> Type
data LinearCosheafHomologyArtifact site cell chainCoeff groupCoeff provenance = LinearCosheafHomologyArtifact
  { lchaBackend :: !HomologyBackendTag,
    lchaChain :: !(PreparedCosheafChain site cell chainCoeff provenance),
    lchaGroupsByDegree :: !(IntMap (HomologyGroup groupCoeff))
  }

type LinearCosheafHomologyFailure :: Type
data LinearCosheafHomologyFailure
  = LinearCosheafHomologyBackendFailed !HomologyBackendTag !HomologyFailure
  deriving stock (Eq, Show)

linearCosheafHomology ::
  HomologyBackend chainCoeff groupCoeff ->
  PreparedCosheafChain site cell chainCoeff provenance ->
  Either
    LinearCosheafHomologyFailure
    (LinearCosheafHomologyArtifact site cell chainCoeff groupCoeff provenance)
linearCosheafHomology backend chain = do
  groups <-
    first (LinearCosheafHomologyBackendFailed backendTag) $
      runHomologyBackend backend (pccChainComplex chain)
  pure
    LinearCosheafHomologyArtifact
      { lchaBackend = backendTag,
        lchaChain = chain,
        lchaGroupsByDegree = homologyGroupsByDegree groups
      }
  where
    backendTag =
      homologyBackendTag backend
{-# INLINEABLE linearCosheafHomology #-}

homologyGroupsByDegree :: [HomologyGroup coefficient] -> IntMap (HomologyGroup coefficient)
homologyGroupsByDegree =
  IntMap.fromAscList . zip [0 ..]
{-# INLINE homologyGroupsByDegree #-}
