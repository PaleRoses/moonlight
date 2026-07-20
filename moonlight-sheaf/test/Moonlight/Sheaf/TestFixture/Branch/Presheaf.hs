{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.Sheaf.TestFixture.Branch.Presheaf
  ( branchCompiledStalkAlgebra,
    branchGluingAlgebra,
  )
where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Void (Void)
import Moonlight.Sheaf.Presheaf.Core (CompiledRestriction (..), Presheaf (..))
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.Sheaf.Gluing
  ( GluingAlgebra (..),
    GluingObstruction (..),
    compatibleMatchingFamilyUnderlying,
    matchingFamilySections,
    matchingFamilyTarget,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchMismatch,
    BranchStalk (..),
    branchStalkAlgebra,
    branchStalkEntries,
  )
import Moonlight.Sheaf.TestFixture.Branch.Site
  ( BranchSite (..),
    branchRestrictStalk,
  )

instance Presheaf BranchSite BranchStalk where
  restrictAlong _ =
    branchRestrictStalk

branchCompiledStalkAlgebra :: StalkAlgebra (CompiledRestriction BranchSite) BranchStalk BranchMismatch ()
branchCompiledStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \restriction -> StalkRestrictionMap (restrictAlong (crSite restriction) (crMorphism restriction)),
      saMismatches = saMismatches branchStalkAlgebra,
      saMerge = saMerge branchStalkAlgebra,
      saRepair = const (Left ()),
      saNormalize = saNormalize branchStalkAlgebra
    }

branchGluingAlgebra :: GluingAlgebra BranchSite BranchStalk Void
branchGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_ compatibleFamily ->
        let matchingFamily = compatibleMatchingFamilyUnderlying compatibleFamily
         in case matchingFamilyTarget matchingFamily of
              BranchBase ->
                Right
                  ( BranchStalk
                      (Map.unions (fmap branchStalkEntries (Vector.toList (matchingFamilySections matchingFamily))))
                  )
              _ ->
                Left (GluingUnavailable (matchingFamilyTarget matchingFamily))
    }
