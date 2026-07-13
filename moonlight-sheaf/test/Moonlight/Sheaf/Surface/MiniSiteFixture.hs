{-# LANGUAGE TypeFamilies #-}

-- | Shared mini-site fixtures for the public surface specs.
module Moonlight.Sheaf.Surface.MiniSiteFixture
  ( MiniCell (..),
    MiniMorphism (..),
    MiniSite (..),
    BadIdentitySite (..),
    OrderedMiniSite (..),
    MiniStalk (..),
    MiniMismatch,
    MiniRepair,
    miniArrow,
    miniIdentity,
    miniMeet,
    miniPullbackPair,
    miniAlgebra,
    orderedMiniAlgebra,
  )
where

import Data.Maybe (mapMaybe)
import Moonlight.Sheaf
import Moonlight.Sheaf.Stalk

data MiniCell
  = Parent
  | Child
  | Ghost
  deriving stock (Eq, Ord, Show)

data MiniMorphism = MiniMorphism MiniCell MiniCell
  deriving stock (Eq, Ord, Show)

data MiniSite = MiniSite
  deriving stock (Eq, Ord, Show)

data BadIdentitySite = BadIdentitySite
  deriving stock (Eq, Ord, Show)

data OrderedMiniSite
  = ParentFirstMiniSite
  | ChildFirstMiniSite
  deriving stock (Eq, Ord, Show)

newtype MiniStalk = MiniStalk Int
  deriving stock (Eq, Show)

type MiniMismatch = DiscreteMismatch MiniStalk

type MiniRepair = DiscreteRepairObstruction MiniStalk

instance Site MiniSite where
  type SiteObject MiniSite = MiniCell
  type SiteMorphism MiniSite = MiniMorphism

  siteObjects _ = [Parent, Child]

  siteMorphisms _ = mapMaybe (uncurry miniArrow) ((,) <$> [Parent, Child] <*> [Parent, Child])

  identityAt _ = miniIdentity

  coversAt _ _ = []

  composeChecked _ outer inner =
    if cmTarget inner == cmSource outer
      then miniArrow (cmSource inner) (cmTarget outer)
      else Nothing

  pullbackPair _ =
    miniPullbackPair

instance Site BadIdentitySite where
  type SiteObject BadIdentitySite = MiniCell
  type SiteMorphism BadIdentitySite = MiniMorphism

  siteObjects _ = [Parent]

  siteMorphisms _ = []

  identityAt _ _ =
    CheckedMorphism Child Parent (MiniMorphism Child Parent)

  coversAt _ _ = []

  composeChecked _ _ _ = Nothing

  pullbackPair _ _ _ = Nothing

instance Site OrderedMiniSite where
  type SiteObject OrderedMiniSite = MiniCell
  type SiteMorphism OrderedMiniSite = MiniMorphism

  siteObjects site =
    case site of
      ParentFirstMiniSite -> [Parent, Child]
      ChildFirstMiniSite -> [Child, Parent]

  siteMorphisms site =
    mapMaybe
      (uncurry miniArrow)
      ((,) <$> siteObjects site <*> siteObjects site)

  identityAt _ = miniIdentity

  coversAt _ _ = []

  composeChecked _ outer inner =
    if cmTarget inner == cmSource outer
      then miniArrow (cmSource inner) (cmTarget outer)
      else Nothing

  pullbackPair _ =
    miniPullbackPair

miniArrow :: MiniCell -> MiniCell -> Maybe (CheckedMorphism MiniCell MiniMorphism)
miniArrow source target =
  if source == target || (source == Parent && target == Child)
    then
      Just
        CheckedMorphism
          { cmSource = source,
            cmTarget = target,
            cmWitness = MiniMorphism source target
          }
    else Nothing

miniIdentity :: MiniCell -> CheckedMorphism MiniCell MiniMorphism
miniIdentity cell =
  CheckedMorphism cell cell (MiniMorphism cell cell)

miniPullbackPair ::
  CheckedMorphism MiniCell MiniMorphism ->
  CheckedMorphism MiniCell MiniMorphism ->
  Maybe (PullbackSquare MiniCell MiniMorphism)
miniPullbackPair leftMorphism rightMorphism
  | cmTarget leftMorphism /= cmTarget rightMorphism =
      Nothing
  | otherwise = do
      apexCell <- miniMeet (cmSource leftMorphism) (cmSource rightMorphism)
      leftLeg <- miniArrow apexCell (cmSource leftMorphism)
      rightLeg <- miniArrow apexCell (cmSource rightMorphism)
      pure
        PullbackSquare
          { psLeftBase = leftMorphism,
            psRightBase = rightMorphism,
            psApex = apexCell,
            psToLeft = leftLeg,
            psToRight = rightLeg
          }

miniMeet :: MiniCell -> MiniCell -> Maybe MiniCell
miniMeet leftCell rightCell =
  case (leftCell, rightCell) of
    (Parent, _) -> Just Parent
    (_, Parent) -> Just Parent
    (Child, Child) -> Just Child
    _ -> Nothing

miniAlgebra :: StalkAlgebra (CompiledRestriction MiniSite) MiniStalk MiniMismatch MiniRepair
miniAlgebra =
  discreteStalkAlgebra

orderedMiniAlgebra :: StalkAlgebra (CompiledRestriction OrderedMiniSite) MiniStalk MiniMismatch MiniRepair
orderedMiniAlgebra =
  discreteStalkAlgebra
