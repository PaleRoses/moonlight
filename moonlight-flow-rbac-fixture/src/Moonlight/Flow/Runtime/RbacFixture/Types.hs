{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.RbacFixture.Types
  ( RbacContext (..),
    RbacProp (..),
    RbacAtomName (..),
    RbacAtoms (..),
    RbacSize (..),
    RbacSeedCounts (..),
    RbacPatchShape (..),
    RbacLocalityScenario (..),
    RbacTruth (..),
    RbacRelationPatchSummary (..),
    RbacPatchSummary (..),
    RbacRowsDigest (..),
    RbacPlans (..),
    RbacModel (..),
    RbacFixtureError (..),
    RbacResourceScopeReproducerPlanSet (..),
    RbacResourceScopeReproducerCase (..),
    Rng (..),
    allAtomNames,
    rbacAtomKey,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Set
  ( Set,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Patch qualified as Patch
import Moonlight.Flow.Query qualified as Query
import Moonlight.Flow.Runtime.Spec.Schema qualified as RuntimeSpec
import Moonlight.Flow.Runtime.Types qualified as Runtime
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

-- | One public runtime context. RBAC tests intentionally exercise runtime joins,
-- patching, decomposition, and projection, not context lattice behavior.
data RbacContext
  = RbacGlobal
  deriving stock (Eq, Ord, Show, Read)

data RbacProp
  = RbacEntitlement
  deriving stock (Eq, Ord, Show, Read)

data RbacAtomName
  = Member
  | GroupScope
  | GroupRole
  | RoleAction
  | ResourceScope
  | RoleAttr
  | UserAttr
  | DenyMember
  | DenyGroupScope
  | DenyGroupAction
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

allAtomNames :: [RbacAtomName]
allAtomNames =
  [minBound .. maxBound]
{-# INLINE allAtomNames #-}

rbacAtomKey :: RbacAtomName -> Int
rbacAtomKey =
  fromEnum
{-# INLINE rbacAtomKey #-}

data RbacAtoms = RbacAtoms
  { rbaMember :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaGroupScope :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaGroupRole :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaRoleAction :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaResourceScope :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaRoleAttr :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaUserAttr :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaDenyMember :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaDenyGroupScope :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp),
    rbaDenyGroupAction :: !(RuntimeSpec.RuntimeAtom RbacContext RbacProp)
  }

data RbacSize = RbacSize
  { rbsTenants :: !Int,
    rbsUsersPerTenant :: !Int,
    rbsGroupsPerTenant :: !Int,
    rbsScopesPerTenant :: !Int,
    rbsResourcesPerTenant :: !Int,
    rbsRolesPerTenant :: !Int,
    rbsActions :: !Int,
    rbsAttrs :: !Int,
    rbsDenyGroupsPerTenant :: !Int
  }
  deriving stock (Eq, Show, Read)

data RbacSeedCounts = RbacSeedCounts
  { rscMember :: !Int,
    rscGroupScope :: !Int,
    rscGroupRole :: !Int,
    rscRoleAction :: !Int,
    rscResourceScope :: !Int,
    rscRoleAttr :: !Int,
    rscUserAttr :: !Int,
    rscDenyMember :: !Int,
    rscDenyGroupScope :: !Int,
    rscDenyGroupAction :: !Int
  }
  deriving stock (Eq, Show, Read)

data RbacPatchShape = RbacPatchShape
  { rpsMemberMoves :: !Int,
    rpsUserAttrMoves :: !Int,
    rpsResourceScopeMoves :: !Int,
    rpsRoleActionMoves :: !Int,
    rpsGroupRoleMoves :: !Int,
    rpsDenyMoves :: !Int,
    rpsGroupScopeMoves :: !Int
  }
  deriving stock (Eq, Show, Read)

data RbacLocalityScenario
  = RbacLocalityMemberChurn
  | RbacLocalityUserAttrChurn
  | RbacLocalityResourceScopeMove
  | RbacLocalityRoleActionToggle
  | RbacLocalityGroupRoleChange
  | RbacLocalityGroupScopeMove
  | RbacLocalityDenyChange
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data RbacTruth = RbacTruth
  { rbtRelations :: !(IntMap (Set RowTupleKey)),
    rbtNextOrdinals :: !(IntMap Int)
  }
  deriving stock (Eq, Show)

data RbacRelationPatchSummary = RbacRelationPatchSummary
  { rrpsDeleted :: !Int,
    rrpsInserted :: !Int
  }
  deriving stock (Eq, Show, Read)

data RbacPatchSummary = RbacPatchSummary
  { rpsDeletedRows :: !Int,
    rpsInsertedRows :: !Int,
    rpsRelations :: !(Map RbacAtomName RbacRelationPatchSummary)
  }
  deriving stock (Eq, Show, Read)

data RbacRowsDigest = RbacRowsDigest
  { rrdPositiveCount :: !Int,
    rrdDigest :: !(Word64, Word64)
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacPlans = RbacPlans
  { rbpGrant :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp),
    rbpConditionalGrant :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp),
    rbpDenied :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp),
    rbpGrantUserAction :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp),
    rbpGrantResourceSubject :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp),
    rbpGrantScopeAction :: !(RuntimeSpec.RuntimePlan RbacContext RbacProp)
  }

data RbacModel = RbacModel
  { rbmAtoms :: !RbacAtoms,
    rbmPlans :: !RbacPlans,
    rbmSchema :: !(RuntimeSpec.RuntimeSchema RbacContext RbacProp)
  }

data RbacFixtureError
  = RbacFixtureQueryError !Query.QueryError
  | RbacFixturePlanError !RuntimeSpec.RuntimePlanError
  | RbacFixtureDecompPlanError !String
  | RbacFixturePatchError !Patch.PatchError
  | RbacFixtureCreateError !(Runtime.RuntimeCreateError RbacContext RbacProp)
  | RbacFixtureFreshRowsExhausted !RbacAtomName !Int !Int
  deriving stock (Show)

data RbacResourceScopeReproducerPlanSet
  = RbacReproResourceScopeOnly
  | RbacReproGrantOnly
  | RbacReproConditionalOnlyDecomp
  | RbacReproDenyOnly
  | RbacReproAllSoakPlans
  deriving stock (Eq, Ord, Show, Read)

data RbacResourceScopeReproducerCase = RbacResourceScopeReproducerCase
  { rrscPlanSet :: !RbacResourceScopeReproducerPlanSet,
    rrscSeedAtoms :: ![RbacAtomName],
    rrscPlans :: RbacAtoms -> Either RbacFixtureError [RuntimeSpec.RuntimePlan RbacContext RbacProp]
  }

newtype Rng = Rng
  { unRng :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)
