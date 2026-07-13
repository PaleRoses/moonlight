{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.Analysis.SheafArchitecture
  ( PolicyModule (..),
    ModuleDescriptor (..),
    FreePolicyInput (..),
    LayerLattice (..),
    BoundaryProjection (..),
    ModuleContractPolicy,
    policySiteManifest,
    policyDescriptors,
    policyModulesById,
    policyModuleByNameMap,
    policyInternalRoots,
    freePolicy,
    forgetPolicy,
    ModuleContractPolicyError (..),
    SheafArchitectureViolation (..),
    buildModuleContractPolicy,
    buildModuleContractPolicyFromSiteManifest,
    validateSheafArchitecture,
  )
where

import Data.Kind (Type)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Analysis.ModuleContract
  ( ModuleContract (..),
    ModuleLayerTag,
  )
import Moonlight.Category (SiteManifest (..))
import Moonlight.Core (invertMapOfSets)
import Moonlight.Core (splitModuleName)
type PolicyModule :: Type -> Type -> Type
data PolicyModule moduleId layer = PolicyModule
  { policyModuleName :: String,
    policyModuleLayer :: layer,
    policyDeclaredImports :: Set moduleId,
    policyCover :: Set moduleId
  }
  deriving stock (Eq, Show)

type ModuleDescriptor :: Type -> Type
data ModuleDescriptor layer = ModuleDescriptor
  { descriptorModuleName :: String,
    descriptorModuleLayer :: layer
  }
  deriving stock (Eq, Show)

type FreePolicyInput :: Type -> Type -> Type
data FreePolicyInput moduleId layer = FreePolicyInput
  { freePolicySiteManifest :: SiteManifest moduleId,
    freePolicyDescriptors :: Map moduleId (ModuleDescriptor layer),
    freePolicyLayerLattice :: LayerLattice layer,
    freePolicyBoundaryProjection :: BoundaryProjection
  }

type LayerLattice :: Type -> Type
data LayerLattice layer = LayerLattice
  { layerAllowsImport :: layer -> layer -> Bool,
    layerTags :: layer -> Set ModuleLayerTag
  }

type BoundaryProjection :: Type
newtype BoundaryProjection = BoundaryProjection
  { projectBoundarySymbols :: ModuleContract -> Set String
  }

type ModuleContractPolicy :: Type -> Type -> Type
data ModuleContractPolicy moduleId layer = ModuleContractPolicy
  { contractPolicySiteManifest :: SiteManifest moduleId,
    contractPolicyDescriptors :: Map moduleId (ModuleDescriptor layer),
    contractPolicyModules :: Map moduleId (PolicyModule moduleId layer),
    contractPolicyModuleByName :: Map String moduleId,
    contractPolicyLayerLattice :: LayerLattice layer,
    contractPolicyIsInternalImport :: String -> Bool,
    contractPolicyInternalRoots :: Maybe (Set String),
    contractPolicyBoundaryProjection :: BoundaryProjection
  }

type ModuleContractPolicyError :: Type -> Type
data ModuleContractPolicyError moduleId
  = DuplicatePolicyModuleName String (Set moduleId)
  | MissingModuleDescriptor moduleId
  | DescriptorOutsideSiteManifest moduleId
  deriving stock (Eq, Show)

type SheafArchitectureViolation :: Type -> Type -> Type
data SheafArchitectureViolation moduleId layer
  = MissingModuleContract moduleId
  | ContractWithoutModuleName
  | UnknownContractModuleName String
  | DuplicateModuleContract moduleId Int
  | UnknownInternalImportName moduleId String
  | DeclaredImportMismatch moduleId (Set moduleId) (Set moduleId)
  | LayerImportViolation moduleId moduleId layer layer
  | LayerTagMismatch moduleId (Set ModuleLayerTag) (Set ModuleLayerTag)
  | BoundarySeparationViolation String (Set moduleId)
  | BoundaryGluingViolation moduleId String (Set moduleId)
  deriving stock (Eq, Show)

inferInternalRoots ::
  Map moduleId (ModuleDescriptor layer) ->
  Set String
inferInternalRoots =
  inferInternalRootsWith defaultRootPrefixStrategy

type RootPrefixStrategy :: Type
type RootPrefixStrategy = String -> String

inferInternalRootsWith ::
  RootPrefixStrategy ->
  Map moduleId (ModuleDescriptor layer) ->
  Set String
inferInternalRootsWith rootPrefixStrategy descriptors =
  descriptors
    & Map.elems
    & fmap (rootPrefixStrategy . descriptorModuleName)
    & Set.fromList

defaultRootPrefixStrategy :: RootPrefixStrategy
defaultRootPrefixStrategy =
  Text.unpack . Text.intercalate (Text.pack ".") . take 2 . splitModuleName . Text.pack

isInternalImportFromRoots :: Set String -> String -> Bool
isInternalImportFromRoots roots importName =
  roots
    & Set.toList
    & any
      ( \root ->
          importName == root
            || (root <> ".") `isPrefixOf` importName
      )

boundaryHomes ::
  Ord moduleId =>
  Map moduleId (Set String) ->
  Map String (Set moduleId)
boundaryHomes = invertMapOfSets

policySiteManifest :: ModuleContractPolicy moduleId layer -> SiteManifest moduleId
policySiteManifest = contractPolicySiteManifest

policyDescriptors ::
  ModuleContractPolicy moduleId layer ->
  Map moduleId (ModuleDescriptor layer)
policyDescriptors = contractPolicyDescriptors

policyModulesById ::
  ModuleContractPolicy moduleId layer ->
  Map moduleId (PolicyModule moduleId layer)
policyModulesById = contractPolicyModules

policyModuleByNameMap ::
  ModuleContractPolicy moduleId layer ->
  Map String moduleId
policyModuleByNameMap = contractPolicyModuleByName

policyInternalRoots ::
  ModuleContractPolicy moduleId layer ->
  Set String
policyInternalRoots policy =
  case contractPolicyInternalRoots policy of
    Just roots -> roots
    Nothing -> inferInternalRoots (contractPolicyDescriptors policy)

freePolicy ::
  Ord moduleId =>
  FreePolicyInput moduleId layer ->
  Either [ModuleContractPolicyError moduleId] (ModuleContractPolicy moduleId layer)
freePolicy input =
  buildModuleContractPolicyFromSiteManifest
    (freePolicySiteManifest input)
    (freePolicyDescriptors input)
    (freePolicyLayerLattice input)
    (freePolicyBoundaryProjection input)

forgetPolicy ::
  ModuleContractPolicy moduleId layer ->
  FreePolicyInput moduleId layer
forgetPolicy policy =
  FreePolicyInput
    { freePolicySiteManifest = policySiteManifest policy,
      freePolicyDescriptors = policyDescriptors policy,
      freePolicyLayerLattice = contractPolicyLayerLattice policy,
      freePolicyBoundaryProjection = contractPolicyBoundaryProjection policy
    }

buildModuleContractPolicy ::
  Ord moduleId =>
  Map moduleId (PolicyModule moduleId layer) ->
  LayerLattice layer ->
  (String -> Bool) ->
  BoundaryProjection ->
  Either [ModuleContractPolicyError moduleId] (ModuleContractPolicy moduleId layer)
buildModuleContractPolicy modules layerLattice isInternalImport boundaryProjection =
  let descriptors = descriptorsFromModules modules
      siteManifest = siteManifestFromModules modules
   in case checkedModuleNameIndex modules of
        Left duplicateNames -> Left duplicateNames
        Right moduleByName ->
          Right
            ( mkResolvedModuleContractPolicy
                siteManifest
                descriptors
                modules
                moduleByName
                layerLattice
                isInternalImport
                Nothing
                boundaryProjection
            )

buildModuleContractPolicyFromSiteManifest ::
  Ord moduleId =>
  SiteManifest moduleId ->
  Map moduleId (ModuleDescriptor layer) ->
  LayerLattice layer ->
  BoundaryProjection ->
  Either [ModuleContractPolicyError moduleId] (ModuleContractPolicy moduleId layer)
buildModuleContractPolicyFromSiteManifest siteManifest descriptors layerLattice boundaryProjection =
  let siteObjectsSet = siteObjects siteManifest
      descriptorKeys = Map.keysSet descriptors
      missingDescriptors =
        Set.difference siteObjectsSet descriptorKeys
          & Set.toList
          & fmap MissingModuleDescriptor
      outsideDescriptors =
        Set.difference descriptorKeys siteObjectsSet
          & Set.toList
          & fmap DescriptorOutsideSiteManifest
      policyModules = policyModulesFromSiteManifest siteManifest descriptors
      constructorViolations = missingDescriptors <> outsideDescriptors
      inferredRoots = inferInternalRootsWith defaultRootPrefixStrategy descriptors
      internalImportPredicate = isInternalImportFromRoots inferredRoots
   in case checkedModuleNameIndex policyModules of
        Left duplicateNames -> Left (constructorViolations <> duplicateNames)
        Right moduleByName
          | null constructorViolations ->
              Right
                ( mkResolvedModuleContractPolicy
                    siteManifest
                    descriptors
                    policyModules
                    moduleByName
                    layerLattice
                    internalImportPredicate
                    (Just inferredRoots)
                    boundaryProjection
                )
          | otherwise -> Left constructorViolations

descriptorsFromModules ::
  Map moduleId (PolicyModule moduleId layer) ->
  Map moduleId (ModuleDescriptor layer)
descriptorsFromModules =
  Map.map
    ( \policyModule ->
        ModuleDescriptor
          { descriptorModuleName = policyModuleName policyModule,
            descriptorModuleLayer = policyModuleLayer policyModule
          }
    )

siteManifestFromModules ::
  Map moduleId (PolicyModule moduleId layer) ->
  SiteManifest moduleId
siteManifestFromModules modules =
  SiteManifest
    { siteObjects = Map.keysSet modules,
      siteImports = modules & Map.map policyDeclaredImports,
      siteCovers = modules & Map.map policyCover
    }

policyModulesFromSiteManifest ::
  Ord moduleId =>
  SiteManifest moduleId ->
  Map moduleId (ModuleDescriptor layer) ->
  Map moduleId (PolicyModule moduleId layer)
policyModulesFromSiteManifest siteManifest descriptors =
  siteObjects siteManifest
    & Set.toList
    & mapMaybe
      ( \moduleId ->
          case Map.lookup moduleId descriptors of
            Nothing -> Nothing
            Just descriptor ->
              Just
                ( moduleId,
                  PolicyModule
                    { policyModuleName = descriptorModuleName descriptor,
                      policyModuleLayer = descriptorModuleLayer descriptor,
                      policyDeclaredImports = Map.findWithDefault Set.empty moduleId (siteImports siteManifest),
                      policyCover = Map.findWithDefault Set.empty moduleId (siteCovers siteManifest)
                    }
                )
      )
    & Map.fromList

checkedModuleNameIndex ::
  Ord moduleId =>
  Map moduleId (PolicyModule moduleId layer) ->
  Either [ModuleContractPolicyError moduleId] (Map String moduleId)
checkedModuleNameIndex modules =
  let groupedNames =
        policyModuleNameEntries modules
          & fmap (\(moduleName, moduleId) -> (moduleName, Set.singleton moduleId))
          & Map.fromListWith Set.union
      duplicateNames =
        groupedNames
          & Map.toAscList
          & foldMap
            ( \(moduleName, moduleIds) ->
                if Set.size moduleIds > 1
                  then [DuplicatePolicyModuleName moduleName moduleIds]
                  else []
            )
   in if null duplicateNames
        then Right (Map.mapMaybe (fmap fst . Set.minView) groupedNames)
        else Left duplicateNames

policyModuleNameEntries ::
  Map moduleId (PolicyModule moduleId layer) ->
  [(String, moduleId)]
policyModuleNameEntries modules =
  modules
    & Map.toList
    & fmap (\(moduleId, policyModule) -> (policyModuleName policyModule, moduleId))

mkResolvedModuleContractPolicy ::
  SiteManifest moduleId ->
  Map moduleId (ModuleDescriptor layer) ->
  Map moduleId (PolicyModule moduleId layer) ->
  Map String moduleId ->
  LayerLattice layer ->
  (String -> Bool) ->
  Maybe (Set String) ->
  BoundaryProjection ->
  ModuleContractPolicy moduleId layer
mkResolvedModuleContractPolicy siteManifest descriptors policyModules moduleByName layerLattice isInternalImport internalRoots boundaryProjection =
  ModuleContractPolicy
    { contractPolicySiteManifest = siteManifest,
      contractPolicyDescriptors = descriptors,
      contractPolicyModules = policyModules,
      contractPolicyModuleByName = moduleByName,
      contractPolicyLayerLattice = layerLattice,
      contractPolicyIsInternalImport = isInternalImport,
      contractPolicyInternalRoots = internalRoots,
      contractPolicyBoundaryProjection = boundaryProjection
    }

validateSheafArchitecture ::
  Ord moduleId =>
  ModuleContractPolicy moduleId layer ->
  [ModuleContract] ->
  [SheafArchitectureViolation moduleId layer]
validateSheafArchitecture policy contracts =
  let resolvedEntries = contracts & fmap (resolveContractEntry policy)
      (resolutionViolations, resolvedContracts) = partitionEithers resolvedEntries
      (duplicateViolations, contractsByModule) = deduplicateContracts resolvedContracts
      missingViolations = missingContracts policy contractsByModule
      moduleViolations = validateModules policy contractsByModule
      boundaryViolations = validateBoundary policy contractsByModule
   in resolutionViolations
        <> duplicateViolations
        <> missingViolations
        <> moduleViolations
        <> boundaryViolations

resolveContractEntry ::
  ModuleContractPolicy moduleId layer ->
  ModuleContract ->
  Either (SheafArchitectureViolation moduleId layer) (moduleId, ModuleContract)
resolveContractEntry policy contract =
  case moduleContractName contract of
    Nothing -> Left ContractWithoutModuleName
    Just moduleName ->
      case Map.lookup moduleName (contractPolicyModuleByName policy) of
        Nothing -> Left (UnknownContractModuleName moduleName)
        Just moduleId -> Right (moduleId, contract)

deduplicateContracts ::
  Ord moduleId =>
  [(moduleId, ModuleContract)] ->
  ([SheafArchitectureViolation moduleId layer], Map moduleId ModuleContract)
deduplicateContracts entries =
  let grouped =
        entries
          >>= (\(moduleId, contract) -> [(moduleId, [contract])])
          & Map.fromListWith (<>)
      duplicateViolations =
        grouped
          & Map.toList
          >>= ( \(moduleId, groupedContracts) ->
                  [DuplicateModuleContract moduleId (length groupedContracts) | length groupedContracts > 1]
              )
      contractsByModule =
        grouped
          & Map.mapMaybe
            ( \case
                [] -> Nothing
                contract : _ -> Just contract
            )
   in (duplicateViolations, contractsByModule)

missingContracts ::
  Ord moduleId =>
  ModuleContractPolicy moduleId layer ->
  Map moduleId ModuleContract ->
  [SheafArchitectureViolation moduleId layer]
missingContracts policy contractsByModule =
  contractPolicyModules policy
    & Map.keysSet
    & Set.toList
    >>= ( \moduleId ->
            [MissingModuleContract moduleId | not (Map.member moduleId contractsByModule)]
        )

validateModules ::
  Ord moduleId =>
  ModuleContractPolicy moduleId layer ->
  Map moduleId ModuleContract ->
  [SheafArchitectureViolation moduleId layer]
validateModules policy contractsByModule =
  contractPolicyModules policy
    & Map.toList
    >>= ( \(moduleId, policyModule) ->
            case Map.lookup moduleId contractsByModule of
              Nothing -> []
              Just contract ->
                let (resolvedImports, unknownImports) =
                      resolveInternalImports policy contract
                    unknownImportViolations =
                      unknownImports
                        & fmap (UnknownInternalImportName moduleId)
                    declaredImports = policyDeclaredImports policyModule
                    importMismatchViolations =
                      [DeclaredImportMismatch moduleId declaredImports resolvedImports | resolvedImports /= declaredImports]
                    layerViolations =
                      resolvedImports
                        & Set.toList
                        >>= ( \importedModule ->
                                case Map.lookup importedModule (contractPolicyModules policy) of
                                  Nothing -> []
                                  Just importedPolicyModule ->
                                    let importerLayer = policyModuleLayer policyModule
                                        importedLayer = policyModuleLayer importedPolicyModule
                                     in [LayerImportViolation moduleId importedModule importerLayer importedLayer | not (layerAllowsImport (contractPolicyLayerLattice policy) importerLayer importedLayer)]
                            )
                    expectedTags = layerTags (contractPolicyLayerLattice policy) (policyModuleLayer policyModule)
                    observedTags = moduleContractLayerTags contract
                    layerTagViolations =
                      [LayerTagMismatch moduleId expectedTags observedTags | not (Set.isSubsetOf expectedTags observedTags)]
                 in unknownImportViolations
                      <> importMismatchViolations
                      <> layerViolations
                      <> layerTagViolations
        )

resolveInternalImports ::
  Ord moduleId =>
  ModuleContractPolicy moduleId layer ->
  ModuleContract ->
  (Set moduleId, [String])
resolveInternalImports policy contract =
  moduleContractImports contract
    & Set.toList
    & foldr
      ( \moduleName (resolvedImports, unknownImports) ->
          case Map.lookup moduleName (contractPolicyModuleByName policy) of
            Just moduleId -> (Set.insert moduleId resolvedImports, unknownImports)
            Nothing ->
              if contractPolicyIsInternalImport policy moduleName
                then (resolvedImports, moduleName : unknownImports)
                else (resolvedImports, unknownImports)
      )
      (Set.empty, [])

validateBoundary ::
  Ord moduleId =>
  ModuleContractPolicy moduleId layer ->
  Map moduleId ModuleContract ->
  [SheafArchitectureViolation moduleId layer]
validateBoundary policy contractsByModule =
  let exportsByModule =
        contractPolicyModules policy
          & Map.mapWithKey
            ( \moduleId _ ->
                case Map.lookup moduleId contractsByModule of
                  Nothing -> Set.empty
                  Just contract ->
                    projectBoundarySymbols (contractPolicyBoundaryProjection policy) contract
            )
      homes = boundaryHomes exportsByModule
      separationViolations =
        homes
          & Map.toList
          >>= ( \(symbol, producers) ->
                  [BoundarySeparationViolation symbol producers | Set.size producers > 1]
              )
      gluingViolations =
        contractPolicyModules policy
          & Map.toList
          >>= ( \(moduleId, policyModule) ->
                  let importedSymbols =
                        policyDeclaredImports policyModule
                          & Set.toList
                          & fmap (\importedModule -> Map.findWithDefault Set.empty importedModule exportsByModule)
                          & Set.unions
                   in importedSymbols
                        & Set.toList
                        >>= ( \symbol ->
                                let producers = Map.findWithDefault Set.empty symbol homes
                                    compatible producer =
                                      producer == moduleId
                                        || Set.member producer (policyCover policyModule)
                                 in [BoundaryGluingViolation moduleId symbol producers | Set.null producers || not (all compatible (Set.toList producers))]
                            )
              )
   in separationViolations <> gluingViolations
