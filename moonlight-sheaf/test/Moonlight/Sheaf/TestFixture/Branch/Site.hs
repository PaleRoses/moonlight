{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.TestFixture.Branch.Site
  ( BranchMorphism (..),
    BranchSite (..),
    branchLeq,
    branchJoin,
    branchMeet,
    branchSite,
    branchArrow,
    branchRootCover,
    branchRestrictStalk,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchStalk (..),
    branchContexts,
  )

data BranchMorphism = BranchMorphism BranchContext BranchContext
  deriving stock (Eq, Ord, Show)

data BranchSite = BranchSite
  deriving stock (Eq, Ord, Show)

branchLeq :: BranchContext -> BranchContext -> Bool
branchLeq leftContext rightContext
  | leftContext == rightContext = True
  | leftContext == BranchBase = True
  | rightContext == BranchApex = True
  | otherwise = False

branchJoin :: BranchContext -> BranchContext -> BranchContext
branchJoin leftContext rightContext
  | branchLeq leftContext rightContext = rightContext
  | branchLeq rightContext leftContext = leftContext
  | otherwise = BranchApex

branchMeet :: BranchContext -> BranchContext -> BranchContext
branchMeet leftContext rightContext
  | branchLeq leftContext rightContext = leftContext
  | branchLeq rightContext leftContext = rightContext
  | otherwise = BranchBase

branchSite :: BranchSite
branchSite =
  BranchSite

branchArrow ::
  BranchContext ->
  BranchContext ->
  Maybe (CheckedMorphism BranchContext BranchMorphism)
branchArrow sourceContext targetContext =
  if branchLeq targetContext sourceContext
    then
      Just
        CheckedMorphism
          { cmSource = sourceContext,
            cmTarget = targetContext,
            cmWitness = BranchMorphism sourceContext targetContext
          }
    else Nothing

branchRootCover ::
  Either
    (CoverConstructionError BranchContext)
    (CoveringFamily BranchContext BranchMorphism)
branchRootCover =
  mkCoveringFamily
    BranchBase
    ( checkedBranchArrow BranchLeft BranchBase
        :| [checkedBranchArrow BranchRight BranchBase]
    )

branchRestrictStalk ::
  CheckedMorphism BranchContext BranchMorphism ->
  BranchStalk ->
  BranchStalk
branchRestrictStalk morphismValue (BranchStalk entries) =
  BranchStalk
    ( Map.filterWithKey
        (\contextValue _ -> branchLeq (cmSource morphismValue) contextValue)
        entries
    )

instance Site BranchSite where
  type SiteObject BranchSite = BranchContext
  type SiteMorphism BranchSite = BranchMorphism

  siteObjects _ =
    branchContexts

  siteMorphisms _ =
    mapMaybe
      (uncurry branchArrow)
      ((,) <$> branchContexts <*> branchContexts)

  identityAt _ contextValue =
    checkedBranchArrow contextValue contextValue

  coversAt _ contextValue =
    case contextValue of
      BranchBase ->
        either (const []) pure branchRootCover
      BranchLeft ->
        singletonCover BranchLeft BranchApex
      BranchRight ->
        singletonCover BranchRight BranchApex
      BranchApex ->
        []

  composeChecked _ outerMorphism innerMorphism =
    if cmTarget innerMorphism == cmSource outerMorphism
      then branchArrow (cmSource innerMorphism) (cmTarget outerMorphism)
      else Nothing

  pullbackPair _ leftMorphism rightMorphism =
    if cmTarget leftMorphism == cmTarget rightMorphism
      then
        let apexContext = branchJoin (cmSource leftMorphism) (cmSource rightMorphism)
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apexContext,
                  psToLeft = checkedBranchArrow apexContext (cmSource leftMorphism),
                  psToRight = checkedBranchArrow apexContext (cmSource rightMorphism)
                }
      else Nothing

checkedBranchArrow :: BranchContext -> BranchContext -> CheckedMorphism BranchContext BranchMorphism
checkedBranchArrow sourceContext targetContext =
  CheckedMorphism
    { cmSource = sourceContext,
      cmTarget = targetContext,
      cmWitness = BranchMorphism sourceContext targetContext
    }

singletonCover ::
  BranchContext ->
  BranchContext ->
  [CoveringFamily BranchContext BranchMorphism]
singletonCover targetContext sourceContext =
  either
    (const [])
    pure
    (mkCoveringFamily targetContext (checkedBranchArrow sourceContext targetContext :| []))
