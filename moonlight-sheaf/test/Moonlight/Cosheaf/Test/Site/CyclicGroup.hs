{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Site.CyclicGroup
  ( CyclicGroupObject (..),
    CyclicGroupMorphism (..),
    CyclicGroupSite,
    CyclicGroupSiteFailure (..),
    cyclicGroupSite,
    cyclicGroupOrder,
    cyclicGroupMorphism,
    cyclicGroupNonidentityExponents,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    Site (..),
  )

type CyclicGroupObject :: Type
data CyclicGroupObject = CyclicGroupObject
  deriving stock (Eq, Ord, Show, Read)

type CyclicGroupMorphism :: Type
newtype CyclicGroupMorphism = CyclicGroupMorphism
  { cyclicGroupMorphismExponent :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type CyclicGroupSite :: Type
newtype CyclicGroupSite = CyclicGroupSite
  { cyclicGroupOrder :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type CyclicGroupSiteFailure :: Type
data CyclicGroupSiteFailure
  = CyclicGroupOrderTooSmall !Int
  deriving stock (Eq, Ord, Show, Read)

cyclicGroupSite :: Int -> Either CyclicGroupSiteFailure CyclicGroupSite
cyclicGroupSite orderValue
  | orderValue <= 0 =
      Left (CyclicGroupOrderTooSmall orderValue)
  | otherwise =
      Right (CyclicGroupSite orderValue)

cyclicGroupMorphism ::
  CyclicGroupSite ->
  Int ->
  CheckedMorphism CyclicGroupObject CyclicGroupMorphism
cyclicGroupMorphism site exponentValue =
  CheckedMorphism
    { cmSource = CyclicGroupObject,
      cmTarget = CyclicGroupObject,
      cmWitness = CyclicGroupMorphism (normalizeExponent site exponentValue)
    }

cyclicGroupNonidentityExponents :: CyclicGroupSite -> [Int]
cyclicGroupNonidentityExponents site =
  [1 .. cyclicGroupOrder site - 1]

instance Site CyclicGroupSite where
  type SiteObject CyclicGroupSite = CyclicGroupObject
  type SiteMorphism CyclicGroupSite = CyclicGroupMorphism

  siteObjects _site =
    [CyclicGroupObject]

  siteMorphisms site =
    fmap (cyclicGroupMorphism site) (cyclicGroupNonidentityExponents site)

  identityAt site _objectValue =
    cyclicGroupMorphism site 0

  coversAt _site _objectValue =
    []

  composeChecked site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | not (cyclicGroupMorphismBelongs site outerMorphism) =
        Nothing
    | not (cyclicGroupMorphismBelongs site innerMorphism) =
        Nothing
    | otherwise =
        Just
          ( cyclicGroupMorphism
              site
              ( cyclicGroupMorphismExponent (cmWitness outerMorphism)
                  + cyclicGroupMorphismExponent (cmWitness innerMorphism)
              )
          )

  pullbackPair site leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | not (cyclicGroupMorphismBelongs site leftMorphism) =
        Nothing
    | not (cyclicGroupMorphismBelongs site rightMorphism) =
        Nothing
    | otherwise =
        Just
          PullbackSquare
            { psLeftBase = leftMorphism,
              psRightBase = rightMorphism,
              psApex = CyclicGroupObject,
              psToLeft = rightMorphism,
              psToRight = leftMorphism
            }

normalizeExponent :: CyclicGroupSite -> Int -> Int
normalizeExponent site exponentValue =
  exponentValue `mod` cyclicGroupOrder site
{-# INLINE normalizeExponent #-}

cyclicGroupMorphismBelongs ::
  CyclicGroupSite ->
  CheckedMorphism CyclicGroupObject CyclicGroupMorphism ->
  Bool
cyclicGroupMorphismBelongs site morphismValue =
  cmSource morphismValue == CyclicGroupObject
    && cmTarget morphismValue == CyclicGroupObject
    && let exponentValue =
             cyclicGroupMorphismExponent (cmWitness morphismValue)
        in exponentValue >= 0
             && exponentValue < cyclicGroupOrder site
             && exponentValue == normalizeExponent site exponentValue
{-# INLINE cyclicGroupMorphismBelongs #-}
