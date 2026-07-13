module Moonlight.Analysis.SheafArchitectureSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Moonlight.Core (splitModuleName)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Moonlight.Analysis.ModuleContract
  ( ModuleContract (..),
    ModuleLayerTag (..),
  )
import Moonlight.Analysis.SheafArchitecture
  ( BoundaryProjection (..),
    FreePolicyInput (..),
    LayerLattice (..),
    ModuleContractPolicyError (..),
    ModuleDescriptor (..),
    ModuleContractPolicy,
    SheafArchitectureViolation (..),
    buildModuleContractPolicyFromSiteManifest,
    forgetPolicy,
    freePolicy,
    policyDescriptors,
    policyInternalRoots,
    policyModuleByNameMap,
    policyModulesById,
    policySiteManifest,
    validateSheafArchitecture,
  )
import Moonlight.Category (SiteManifest (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

type TestModule :: Type
data TestModule
  = SiteModule
  | SectionModule
  | GlobalModule
  deriving stock (Eq, Ord, Show)

type TestLayer :: Type
data TestLayer
  = SiteLayer
  | SectionLayer
  | GlobalLayer
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "sheaf-architecture"
    [ testCase "valid contracts satisfy policy" testValidContracts,
      testCase "site constructor rejects missing descriptor" testMissingDescriptorPolicyError,
      testCase "site constructor rejects duplicate module names" testDuplicateModuleNamePolicyError,
      testCase "free-forget counit coherence holds" testFreeForgetCounitCoherence,
      testCase "external imports are monotone no-ops" testExternalImportMonotonicity,
      testCase "boundary separation violation is detected" testBoundarySeparationViolation,
      testCase "unknown internal import is detected" testUnknownInternalImportViolation
    ]

testValidContracts :: Assertion
testValidContracts = do
  policy <- loadPolicy
  validateSheafArchitecture policy validContracts @?= []

testMissingDescriptorPolicyError :: Assertion
testMissingDescriptorPolicyError =
  let partialDescriptors = Map.delete GlobalModule moduleDescriptors
      expected = [MissingModuleDescriptor GlobalModule]
   in case buildModuleContractPolicyFromSiteManifest siteManifest partialDescriptors layerLattice boundaryProjection of
        Left failures -> failures @?= expected
        Right _ -> fail "expected missing descriptor failure"

testDuplicateModuleNamePolicyError :: Assertion
testDuplicateModuleNamePolicyError =
  let duplicateDescriptors =
        Map.adjust
          (\descriptor -> descriptor {descriptorModuleName = "Pkg.Site.Cell"})
          SectionModule
          moduleDescriptors
      expected =
        [ DuplicatePolicyModuleName
            "Pkg.Site.Cell"
            (Set.fromList [SiteModule, SectionModule])
        ]
   in case buildModuleContractPolicyFromSiteManifest siteManifest duplicateDescriptors layerLattice boundaryProjection of
        Left failures -> failures @?= expected
        Right _ -> fail "expected duplicate module-name failure"

testFreeForgetCounitCoherence :: Assertion
testFreeForgetCounitCoherence =
  case freePolicy canonicalFreeInput of
    Left failures -> fail (show failures)
    Right policy ->
      case freePolicy (forgetPolicy policy) of
        Left failures -> fail (show failures)
        Right rebuilt -> do
          policySiteManifest rebuilt @?= policySiteManifest policy
          policyDescriptors rebuilt @?= policyDescriptors policy
          policyModulesById rebuilt @?= policyModulesById policy
          policyModuleByNameMap rebuilt @?= policyModuleByNameMap policy
          policyInternalRoots rebuilt @?= policyInternalRoots policy

testExternalImportMonotonicity :: Assertion
testExternalImportMonotonicity = do
  policy <- loadPolicy
  let baseViolations = validateSheafArchitecture policy validContracts
      expandedViolations = validateSheafArchitecture policy contractsWithExternalImports
  expandedViolations @?= baseViolations

testBoundarySeparationViolation :: Assertion
testBoundarySeparationViolation = do
  policy <- loadPolicy
  let violations =
        validateSheafArchitecture policy contractsWithDuplicateBoundary
      expected :: SheafArchitectureViolation TestModule TestLayer
      expected =
        BoundarySeparationViolation
          "shared_boundary_symbol"
          (Set.fromList [SiteModule, SectionModule])
  containsViolation expected violations @?= True

testUnknownInternalImportViolation :: Assertion
testUnknownInternalImportViolation = do
  policy <- loadPolicy
  let violations =
        validateSheafArchitecture policy contractsWithUnknownImport
      unknownExpected :: SheafArchitectureViolation TestModule TestLayer
      unknownExpected =
        UnknownInternalImportName SectionModule "Pkg.Site.Unknown"
      mismatchExpected :: SheafArchitectureViolation TestModule TestLayer
      mismatchExpected =
        DeclaredImportMismatch
          SectionModule
          (Set.singleton SiteModule)
          Set.empty
  containsViolation unknownExpected violations @?= True
  containsViolation mismatchExpected violations @?= True

loadPolicy :: IO (ModuleContractPolicy TestModule TestLayer)
loadPolicy =
  case buildModuleContractPolicyFromSiteManifest siteManifest moduleDescriptors layerLattice boundaryProjection of
    Left failures -> fail (show failures)
    Right policy -> pure policy

canonicalFreeInput :: FreePolicyInput TestModule TestLayer
canonicalFreeInput =
  FreePolicyInput
    { freePolicySiteManifest = siteManifest,
      freePolicyDescriptors = moduleDescriptors,
      freePolicyLayerLattice = layerLattice,
      freePolicyBoundaryProjection = boundaryProjection
    }

moduleDescriptors :: Map TestModule (ModuleDescriptor TestLayer)
moduleDescriptors =
  Map.fromList
    [ ( SiteModule,
        ModuleDescriptor
          { descriptorModuleName = "Pkg.Site.Cell",
            descriptorModuleLayer = SiteLayer
          }
      ),
      ( SectionModule,
        ModuleDescriptor
          { descriptorModuleName = "Pkg.Section.World",
            descriptorModuleLayer = SectionLayer
          }
      ),
      ( GlobalModule,
        ModuleDescriptor
          { descriptorModuleName = "Pkg.API",
            descriptorModuleLayer = GlobalLayer
          }
      )
    ]

siteManifest :: SiteManifest TestModule
siteManifest =
  SiteManifest
    { siteObjects = Set.fromList [SiteModule, SectionModule, GlobalModule],
      siteImports =
        Map.fromList
          [ (SiteModule, Set.empty),
            (SectionModule, Set.singleton SiteModule),
            (GlobalModule, Set.singleton SectionModule)
          ],
      siteCovers =
        Map.fromList
          [ (SiteModule, Set.empty),
            (SectionModule, Set.singleton SiteModule),
            (GlobalModule, Set.fromList [SiteModule, SectionModule])
          ]
    }

layerLattice :: LayerLattice TestLayer
layerLattice =
  LayerLattice
    { layerAllowsImport = \importer imported -> layerRank imported <= layerRank importer,
      layerTags = expectedLayerTags
    }

layerRank :: TestLayer -> Int
layerRank layerValue =
  case layerValue of
    SiteLayer -> 0
    SectionLayer -> 1
    GlobalLayer -> 2

expectedLayerTags :: TestLayer -> Set ModuleLayerTag
expectedLayerTags layerValue =
  case layerValue of
    SiteLayer -> Set.singleton (ModuleLayerTag "Site")
    SectionLayer -> Set.singleton (ModuleLayerTag "Section")
    GlobalLayer -> Set.empty

boundaryProjection :: BoundaryProjection
boundaryProjection = BoundaryProjection moduleContractExports

validContracts :: [ModuleContract]
validContracts =
  [ mkContract "Pkg.Site.Cell" Set.empty (Set.singleton "site_boundary_symbol"),
    mkContract "Pkg.Section.World" (Set.singleton "Pkg.Site.Cell") (Set.singleton "section_boundary_symbol"),
    mkContract "Pkg.API" (Set.singleton "Pkg.Section.World") (Set.singleton "global_boundary_symbol")
  ]

contractsWithDuplicateBoundary :: [ModuleContract]
contractsWithDuplicateBoundary =
  [ mkContract "Pkg.Site.Cell" Set.empty (Set.singleton "shared_boundary_symbol"),
    mkContract "Pkg.Section.World" (Set.singleton "Pkg.Site.Cell") (Set.singleton "shared_boundary_symbol"),
    mkContract "Pkg.API" (Set.singleton "Pkg.Section.World") (Set.singleton "global_boundary_symbol")
  ]

contractsWithUnknownImport :: [ModuleContract]
contractsWithUnknownImport =
  [ mkContract "Pkg.Site.Cell" Set.empty (Set.singleton "site_boundary_symbol"),
    mkContract "Pkg.Section.World" (Set.singleton "Pkg.Site.Unknown") (Set.singleton "section_boundary_symbol"),
    mkContract "Pkg.API" (Set.singleton "Pkg.Section.World") (Set.singleton "global_boundary_symbol")
  ]

contractsWithExternalImports :: [ModuleContract]
contractsWithExternalImports =
  validContracts
    & fmap addExternalImport

addExternalImport :: ModuleContract -> ModuleContract
addExternalImport contract =
  contract
    { moduleContractImports = Set.insert "Data.List" (moduleContractImports contract)
    }

mkContract :: String -> Set String -> Set String -> ModuleContract
mkContract contractName imports exports =
  ModuleContract
    { moduleContractName = Just contractName,
      moduleContractImports = imports,
      moduleContractExports = exports,
      moduleContractLayerTags = moduleLayerTags contractName
    }

moduleLayerTags :: String -> Set ModuleLayerTag
moduleLayerTags contractName =
  contractName
    & Text.pack
    & splitModuleName
    & filter (not . Text.null)
    & fmap (ModuleLayerTag . Text.unpack)
    & Set.fromList

containsViolation ::
  (Eq moduleId, Eq layer) =>
  SheafArchitectureViolation moduleId layer ->
  [SheafArchitectureViolation moduleId layer] ->
  Bool
containsViolation violation =
  any (== violation)
