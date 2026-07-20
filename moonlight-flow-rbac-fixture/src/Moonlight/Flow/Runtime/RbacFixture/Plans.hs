{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Runtime.RbacFixture.Plans
  ( slotT,
    slotU,
    slotG,
    slotR,
    slotA,
    slotS,
    slotRes,
    slotX,
    slotDg,
    entitlementProp,
    rbacAtoms,
    rbacSchema,
    atomList,
    atomByName,
    grantOnlyPlans,
    conditionalOnlyPlans,
    deniedOnlyPlans,
    fullSoakPlans,
    fullSoakPlanSet,
    planList,
    buildRbacModel,
    resourceScopeReproducerCases,
    resourceScopeOnlyPlans,
    conditionalReferencePlan,
    conditionalDecompPlan,
    conditionalSeedAtoms,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Core qualified as R
import Moonlight.Differential.Proposition qualified as R
import Moonlight.Flow.Query qualified as R
import Moonlight.Flow.Runtime.Spec.Schema qualified as R
import Moonlight.Flow.Plan.Query.Core qualified as Plan
import Moonlight.Flow.Runtime.RbacFixture.Types

slotT, slotU, slotG, slotR, slotA, slotS, slotRes, slotX, slotDg :: R.SlotId
slotT = R.mkSlotId 0
slotU = R.mkSlotId 1
slotG = R.mkSlotId 2
slotR = R.mkSlotId 3
slotA = R.mkSlotId 4
slotS = R.mkSlotId 5
slotRes = R.mkSlotId 6
slotX = R.mkSlotId 7
slotDg = R.mkSlotId 8

entitlementProp :: R.PropositionKey RbacProp
entitlementProp =
  R.PropositionKey RbacEntitlement

rbacAtoms :: RbacAtoms
rbacAtoms =
  RbacAtoms
    { rbaMember = R.runtimeAtom (R.mkAtomId (rbacAtomKey Member)) [slotT, slotU, slotG],
      rbaGroupScope = R.runtimeAtom (R.mkAtomId (rbacAtomKey GroupScope)) [slotT, slotG, slotS],
      rbaGroupRole = R.runtimeAtom (R.mkAtomId (rbacAtomKey GroupRole)) [slotT, slotG, slotR],
      rbaRoleAction = R.runtimeAtom (R.mkAtomId (rbacAtomKey RoleAction)) [slotT, slotR, slotA],
      rbaResourceScope = R.runtimeAtom (R.mkAtomId (rbacAtomKey ResourceScope)) [slotT, slotRes, slotS],
      rbaRoleAttr = R.runtimeAtom (R.mkAtomId (rbacAtomKey RoleAttr)) [slotT, slotR, slotX],
      rbaUserAttr = R.runtimeAtom (R.mkAtomId (rbacAtomKey UserAttr)) [slotT, slotU, slotX],
      rbaDenyMember = R.runtimeAtom (R.mkAtomId (rbacAtomKey DenyMember)) [slotT, slotU, slotDg],
      rbaDenyGroupScope = R.runtimeAtom (R.mkAtomId (rbacAtomKey DenyGroupScope)) [slotT, slotDg, slotS],
      rbaDenyGroupAction = R.runtimeAtom (R.mkAtomId (rbacAtomKey DenyGroupAction)) [slotT, slotDg, slotA]
    }

atomByName :: RbacAtoms -> RbacAtomName -> R.RuntimeAtom RbacContext RbacProp
atomByName atomsValue =
  \case
    Member -> rbaMember atomsValue
    GroupScope -> rbaGroupScope atomsValue
    GroupRole -> rbaGroupRole atomsValue
    RoleAction -> rbaRoleAction atomsValue
    ResourceScope -> rbaResourceScope atomsValue
    RoleAttr -> rbaRoleAttr atomsValue
    UserAttr -> rbaUserAttr atomsValue
    DenyMember -> rbaDenyMember atomsValue
    DenyGroupScope -> rbaDenyGroupScope atomsValue
    DenyGroupAction -> rbaDenyGroupAction atomsValue

atomList :: RbacAtoms -> [R.RuntimeAtom RbacContext RbacProp]
atomList atomsValue =
  fmap (atomByName atomsValue) allAtomNames

rbacSchema :: RbacAtoms -> R.RuntimeSchema RbacContext RbacProp
rbacSchema atomsValue =
  R.runtimeSchema
    [ ( RbacGlobal,
        R.runtimeContextSchema
          (atomList atomsValue)
          [entitlementProp]
      )
    ]

resourceScopeOnlyPlans :: RbacAtoms -> Either RbacFixtureError [R.RuntimePlan RbacContext RbacProp]
resourceScopeOnlyPlans atomsValue = do
  resourceScopeQuery <-
    first RbacFixtureQueryError $
      R.query
        [R.runtimeMatch (rbaResourceScope atomsValue)]
        (R.select [slotT, slotRes, slotS])
  resourceScopePlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp resourceScopeQuery)
  pure [resourceScopePlan]

grantOnlyPlans :: RbacAtoms -> Either RbacFixtureError [R.RuntimePlan RbacContext RbacProp]
grantOnlyPlans atomsValue = do
  grantQuery <- grantQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  grantPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp grantQuery)
  pure [grantPlan]

conditionalOnlyPlans :: RbacAtoms -> Either RbacFixtureError [R.RuntimePlan RbacContext RbacProp]
conditionalOnlyPlans atomsValue =
  fmap (: []) (conditionalDecompPlan atomsValue)

conditionalReferencePlan :: RbacAtoms -> Either RbacFixtureError (R.RuntimePlan RbacContext RbacProp)
conditionalReferencePlan atomsValue = do
  conditionalQuery <- conditionalGrantQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp conditionalQuery)

conditionalDecompPlan :: RbacAtoms -> Either RbacFixtureError (R.RuntimePlan RbacContext RbacProp)
conditionalDecompPlan atomsValue = do
  conditionalQuery <- conditionalGrantQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  first (RbacFixtureDecompPlanError . show) $
    R.runtimePlanWithDecompQuery RbacGlobal entitlementProp conditionalQuery conditionalGrantDecomp

conditionalSeedAtoms :: [RbacAtomName]
conditionalSeedAtoms =
  [Member, GroupScope, GroupRole, RoleAction, ResourceScope, RoleAttr, UserAttr]

deniedOnlyPlans :: RbacAtoms -> Either RbacFixtureError [R.RuntimePlan RbacContext RbacProp]
deniedOnlyPlans atomsValue = do
  deniedQuery <- deniedQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  deniedPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp deniedQuery)
  pure [deniedPlan]

fullSoakPlans :: RbacAtoms -> Either RbacFixtureError [R.RuntimePlan RbacContext RbacProp]
fullSoakPlans atomsValue =
  planList <$> fullSoakPlanSet atomsValue

resourceScopeReproducerCases :: [RbacResourceScopeReproducerCase]
resourceScopeReproducerCases =
  [ RbacResourceScopeReproducerCase
      { rrscPlanSet = RbacReproResourceScopeOnly,
        rrscSeedAtoms = [ResourceScope],
        rrscPlans = resourceScopeOnlyPlans
      },
    RbacResourceScopeReproducerCase
      { rrscPlanSet = RbacReproGrantOnly,
        rrscSeedAtoms = [Member, GroupScope, GroupRole, RoleAction, ResourceScope],
        rrscPlans = grantOnlyPlans
      },
    RbacResourceScopeReproducerCase
      { rrscPlanSet = RbacReproConditionalOnlyDecomp,
        rrscSeedAtoms = conditionalSeedAtoms,
        rrscPlans = conditionalOnlyPlans
      },
    RbacResourceScopeReproducerCase
      { rrscPlanSet = RbacReproDenyOnly,
        rrscSeedAtoms = [DenyMember, DenyGroupScope, DenyGroupAction, ResourceScope],
        rrscPlans = deniedOnlyPlans
      },
    RbacResourceScopeReproducerCase
      { rrscPlanSet = RbacReproAllSoakPlans,
        rrscSeedAtoms = allAtomNames,
        rrscPlans = fullSoakPlans
      }
  ]

fullSoakPlanSet :: RbacAtoms -> Either RbacFixtureError RbacPlans
fullSoakPlanSet atomsValue = do
  grantQuery <- grantQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  conditionalQuery <- conditionalGrantQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  deniedQuery <- deniedQueryOf atomsValue [slotT, slotU, slotRes, slotA]
  grantUserActionQuery <- grantQueryOf atomsValue [slotT, slotU, slotA]
  grantResourceSubjectQuery <- grantQueryOf atomsValue [slotT, slotRes, slotU]
  grantScopeActionQuery <- grantQueryOf atomsValue [slotT, slotS, slotA]
  grantPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp grantQuery)
  conditionalPlan <-
    first (RbacFixtureDecompPlanError . show) $
      R.runtimePlanWithDecompQuery RbacGlobal entitlementProp conditionalQuery conditionalGrantDecomp
  deniedPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp deniedQuery)
  grantUserActionPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp grantUserActionQuery)
  grantResourceSubjectPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp grantResourceSubjectQuery)
  grantScopeActionPlan <- first RbacFixturePlanError (R.runtimePlanQuery RbacGlobal entitlementProp grantScopeActionQuery)
  pure
    RbacPlans
      { rbpGrant = grantPlan,
        rbpConditionalGrant = conditionalPlan,
        rbpDenied = deniedPlan,
        rbpGrantUserAction = grantUserActionPlan,
        rbpGrantResourceSubject = grantResourceSubjectPlan,
        rbpGrantScopeAction = grantScopeActionPlan
      }

planList :: RbacPlans -> [R.RuntimePlan RbacContext RbacProp]
planList plans =
  [ rbpGrant plans,
    rbpConditionalGrant plans,
    rbpDenied plans,
    rbpGrantUserAction plans,
    rbpGrantResourceSubject plans,
    rbpGrantScopeAction plans
  ]

buildRbacModel :: Either RbacFixtureError RbacModel
buildRbacModel = do
  let atomsValue = rbacAtoms
  plansValue <- fullSoakPlanSet atomsValue
  pure
    RbacModel
      { rbmAtoms = atomsValue,
        rbmPlans = plansValue,
        rbmSchema = rbacSchema atomsValue
      }

grantQueryOf :: RbacAtoms -> [R.SlotId] -> Either RbacFixtureError R.Query
grantQueryOf atomsValue outputSlots =
  first RbacFixtureQueryError $
    R.query
      [ R.runtimeMatch (rbaMember atomsValue),
        R.runtimeMatch (rbaGroupScope atomsValue),
        R.runtimeMatch (rbaGroupRole atomsValue),
        R.runtimeMatch (rbaRoleAction atomsValue),
        R.runtimeMatch (rbaResourceScope atomsValue)
      ]
      (R.select outputSlots)

conditionalGrantQueryOf :: RbacAtoms -> [R.SlotId] -> Either RbacFixtureError R.Query
conditionalGrantQueryOf atomsValue outputSlots =
  first RbacFixtureQueryError $
    R.query
      [ R.runtimeMatch (rbaMember atomsValue),
        R.runtimeMatch (rbaGroupScope atomsValue),
        R.runtimeMatch (rbaGroupRole atomsValue),
        R.runtimeMatch (rbaRoleAction atomsValue),
        R.runtimeMatch (rbaResourceScope atomsValue),
        R.runtimeMatch (rbaRoleAttr atomsValue),
        R.runtimeMatch (rbaUserAttr atomsValue)
      ]
      (R.select outputSlots)

deniedQueryOf :: RbacAtoms -> [R.SlotId] -> Either RbacFixtureError R.Query
deniedQueryOf atomsValue outputSlots =
  first RbacFixtureQueryError $
    R.query
      [ R.runtimeMatch (rbaDenyMember atomsValue),
        R.runtimeMatch (rbaDenyGroupScope atomsValue),
        R.runtimeMatch (rbaDenyGroupAction atomsValue),
        R.runtimeMatch (rbaResourceScope atomsValue)
      ]
      (R.select outputSlots)

conditionalGrantDecomp :: Plan.DecompPlan
conditionalGrantDecomp =
  Plan.mkDecompPlan
    (Plan.BagId 0)
    bags
    parent
    children
    separator
    owner
  where
    bag0 =
      Plan.mkDecompBag
        (Plan.BagId 0)
        [slotT, slotU, slotG, slotR, slotX]
        (IntSet.fromList [rbacAtomKey Member, rbacAtomKey GroupRole, rbacAtomKey RoleAttr, rbacAtomKey UserAttr])
    bag1 =
      Plan.mkDecompBag
        (Plan.BagId 1)
        [slotT, slotG, slotS]
        (IntSet.singleton (rbacAtomKey GroupScope))
    bag2 =
      Plan.mkDecompBag
        (Plan.BagId 2)
        [slotT, slotS, slotRes]
        (IntSet.singleton (rbacAtomKey ResourceScope))
    bag3 =
      Plan.mkDecompBag
        (Plan.BagId 3)
        [slotT, slotR, slotA]
        (IntSet.singleton (rbacAtomKey RoleAction))
    bags =
      IntMap.fromList
        [ (0, bag0),
          (1, bag1),
          (2, bag2),
          (3, bag3)
        ]
    parent =
      IntMap.fromList
        [ (1, Plan.BagId 0),
          (2, Plan.BagId 1),
          (3, Plan.BagId 0)
        ]
    children =
      IntMap.fromList
        [ (0, [Plan.BagId 1, Plan.BagId 3]),
          (1, [Plan.BagId 2])
        ]
    separator =
      Map.fromList
        [ ((Plan.BagId 1, Plan.BagId 0), [slotT, slotG]),
          ((Plan.BagId 2, Plan.BagId 1), [slotT, slotS]),
          ((Plan.BagId 3, Plan.BagId 0), [slotT, slotR])
        ]
    owner =
      IntMap.fromList
        [ (rbacAtomKey Member, Plan.BagId 0),
          (rbacAtomKey GroupRole, Plan.BagId 0),
          (rbacAtomKey RoleAttr, Plan.BagId 0),
          (rbacAtomKey UserAttr, Plan.BagId 0),
          (rbacAtomKey GroupScope, Plan.BagId 1),
          (rbacAtomKey ResourceScope, Plan.BagId 2),
          (rbacAtomKey RoleAction, Plan.BagId 3)
        ]
