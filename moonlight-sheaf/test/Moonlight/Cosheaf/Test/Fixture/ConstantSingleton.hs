{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Fixture.ConstantSingleton
  ( SingletonCostalk (..),
    SingletonMismatch (..),
    constantSingletonCosheaf,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import Moonlight.Cosheaf.Finite
  ( FiniteCosheaf,
    FiniteCosheafAlgebra (..),
    FiniteCosheafFailure,
    mkFiniteCosheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )

type SingletonCostalk :: Type
data SingletonCostalk = SingletonCostalk
  deriving stock (Eq, Ord, Show, Read)

type SingletonMismatch :: Type
data SingletonMismatch = SingletonMismatch
  deriving stock (Eq, Ord, Show, Read)

constantSingletonCosheaf ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Either
    (FiniteCosheafFailure (SiteObject site) (SiteMorphism site) SingletonCostalk SingletonMismatch Void)
    (FiniteCosheaf site SingletonCostalk)
constantSingletonCosheaf site =
  mkFiniteCosheaf
    site
    singletonAlgebra
    (Map.fromList [(objectValue, [SingletonCostalk]) | objectValue <- siteObjects site])

singletonAlgebra ::
  FiniteCosheafAlgebra site SingletonCostalk SingletonMismatch Void
singletonAlgebra =
  FiniteCosheafAlgebra
    { fcaCorestrict = \_morphismValue SingletonCostalk -> Right SingletonCostalk,
      fcaMismatches =
        \_objectValue leftValue rightValue ->
          [SingletonMismatch | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }
