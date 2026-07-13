{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Pale.Ghc.Hie.Read
  ( HieReadError (..),
    readModuleOracle,
    indexHieRoots,
  )
where

import Control.Exception (SomeException, try)
import Data.Array (Array)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
  ( ContextInfo (..),
    HieAST (..),
    HieASTs (..),
    HieFile (..),
    HieTypeFlat,
    Identifier,
    IdentifierDetails (..),
    NodeInfo (..),
    SourcedNodeInfo (..),
    TypeIndex,
  )
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName)
import GHC.Types.Name.Cache (NameCache, newNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc
  ( RealSrcSpan,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
  )
import GHC.Unit.Module (moduleName, moduleNameString, moduleUnit, unitString)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), ResolvedOrigin (..), mkPackageUnit)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieOracleIndex, buildHieOracleIndex)
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWords, hieTypeIndexTypeWords, typeWordsList)
import Moonlight.Pale.Ghc.Expr (SourceRegion (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (normalise, takeExtension, (</>))

type HieReadError :: Type
data HieReadError
  = HieReadError !FilePath !String
  | HieRootError !FilePath !String
  deriving stock (Eq, Show)

readModuleOracle :: NameCache -> FilePath -> IO (Either HieReadError ModuleNameOracle)
readModuleOracle nameCache hiePath =
  fmap (first (HieReadError hiePath . show)) $
    fmap hieFileOracle <$> tryReadHieFile nameCache hiePath

indexHieRoots :: [FilePath] -> IO ([HieReadError], HieOracleIndex)
indexHieRoots [] =
  pure ([], buildHieOracleIndex [])
indexHieRoots roots = do
  nameCache <- newNameCache
  collected <- traverse collectHieFiles roots
  let (rootErrors, hiePaths) =
        partitionEithers collected
  readResults <- traverse (readModuleOracle nameCache) (concat hiePaths)
  let (readErrors, oracles) =
        partitionEithers readResults
  pure
    ( rootErrors <> readErrors,
      buildHieOracleIndex oracles
    )

tryReadHieFile :: NameCache -> FilePath -> IO (Either SomeException HieFileResult)
tryReadHieFile nameCache hiePath =
  try (readHieFile nameCache hiePath)

hieFileOracle :: HieFileResult -> ModuleNameOracle
hieFileOracle result =
  let hieFile = hie_file_result result
   in ModuleNameOracle
        { mnoSourcePath = normalise (hie_hs_file hieFile),
          mnoGlobalUses = globalUsesByOcc (hie_asts hieFile),
          mnoEvidenceAtSpan = evidenceBySpan (hie_asts hieFile),
          mnoTypeAtSpan = typeWordsBySpan (hie_types hieFile) (hie_asts hieFile)
        }

globalUsesByOcc :: HieASTs a -> Map String (Set.Set ResolvedOrigin)
globalUsesByOcc (HieASTs astsByPath) =
  Map.fromListWith
    Set.union
    [ (roOcc origin, Set.singleton origin)
    | ast <- Map.elems astsByPath,
      origin <- astGlobalUses ast
    ]

astGlobalUses :: HieAST a -> [ResolvedOrigin]
astGlobalUses ast =
  nodeGlobalUses ast <> foldMap astGlobalUses (nodeChildren ast)

nodeGlobalUses :: HieAST a -> [ResolvedOrigin]
nodeGlobalUses ast =
  [ origin
  | nodeInfo <- Map.elems (getSourcedNodeInfo (sourcedNodeInfo ast)),
    (identifier, details) <- Map.toList (nodeIdentifiers nodeInfo),
    Set.member Use (identInfo details),
    Just origin <- [identifierOrigin identifier]
  ]

evidenceBySpan :: HieASTs a -> Map.Map SourceRegion (Set.Set ResolvedOrigin)
evidenceBySpan (HieASTs astsByPath) =
  Map.fromListWith
    Set.union
    [ (region, Set.singleton origin)
    | ast <- Map.elems astsByPath,
      (region, origin) <- astEvidence ast
    ]

astEvidence :: HieAST a -> [(SourceRegion, ResolvedOrigin)]
astEvidence ast =
  nodeEvidence ast <> foldMap astEvidence (nodeChildren ast)

nodeEvidence :: HieAST a -> [(SourceRegion, ResolvedOrigin)]
nodeEvidence ast =
  [ (sourceRegionFromRealSpan (nodeSpan ast), origin)
  | nodeInfo <- Map.elems (getSourcedNodeInfo (sourcedNodeInfo ast)),
    (identifier, details) <- Map.toList (nodeIdentifiers nodeInfo),
    any evidenceContext (Set.toAscList (identInfo details)),
    Just origin <- [identifierOrigin identifier]
  ]

evidenceContext :: ContextInfo -> Bool
evidenceContext = \case
  EvidenceVarBind {} ->
    True
  EvidenceVarUse ->
    True
  _ ->
    False

typeWordsBySpan :: Array TypeIndex HieTypeFlat -> HieASTs TypeIndex -> Map.Map SourceRegion (Set.Set TypeWords)
typeWordsBySpan typeTable (HieASTs astsByPath) =
  Map.fromListWith
    Set.union
    [ (region, wordsValue)
    | ast <- Map.elems astsByPath,
      (region, wordsValue) <- astTypeWords typeTable ast
    ]

astTypeWords :: Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> [(SourceRegion, Set.Set TypeWords)]
astTypeWords typeTable ast =
  nodeTypeWordRows typeTable ast <> foldMap (astTypeWords typeTable) (nodeChildren ast)

nodeTypeWordRows :: Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> [(SourceRegion, Set.Set TypeWords)]
nodeTypeWordRows typeTable ast =
  [ (sourceRegionFromRealSpan (nodeSpan ast), Set.singleton typeWords)
  | nodeInfo <- Map.elems (getSourcedNodeInfo (sourcedNodeInfo ast)),
    typeIndex <- nodeType nodeInfo,
    let typeWords = hieTypeIndexTypeWords typeTable typeIndex,
    not (null (typeWordsList typeWords))
  ]

sourceRegionFromRealSpan :: RealSrcSpan -> SourceRegion
sourceRegionFromRealSpan realSrcSpan =
  SourceRegion
    { srStartLine = srcSpanStartLine realSrcSpan,
      srStartCol = srcSpanStartCol realSrcSpan,
      srEndLine = srcSpanEndLine realSrcSpan,
      srEndCol = srcSpanEndCol realSrcSpan
    }

identifierOrigin :: Identifier -> Maybe ResolvedOrigin
identifierOrigin = \case
  Left _ ->
    Nothing
  Right name ->
    nameOrigin name

nameOrigin :: Name -> Maybe ResolvedOrigin
nameOrigin name =
  if isExternalName name
    then
      let nameModuleValue = nameModule name
          unitText = unitString (moduleUnit nameModuleValue)
       in case mkPackageUnit unitText of
            Left _ ->
              Nothing
            Right unitValue ->
              Just
                ResolvedOrigin
                  { roUnit = unitValue,
                    roModule = moduleNameString (moduleName nameModuleValue),
                    roOcc = occNameString (nameOccName name)
                  }
    else Nothing

collectHieFiles :: FilePath -> IO (Either HieReadError [FilePath])
collectHieFiles root = do
  rootIsDirectory <- doesDirectoryExist root
  rootIsFile <- doesFileExist root
  if rootIsDirectory
    then first (HieRootError root . show) <$> tryCollectDirectory root
    else
      if rootIsFile
        then pure (Right [root | hieFilePath root])
        else pure (Left (HieRootError root "no such file or directory"))

tryCollectDirectory :: FilePath -> IO (Either SomeException [FilePath])
tryCollectDirectory = try . collectDirectory

collectDirectory :: FilePath -> IO [FilePath]
collectDirectory directory = do
  entries <- listDirectory directory
  let visiblePaths =
        fmap (directory </>) (sort (filter (not . isPrefixOf ".") entries))
  concat <$> traverse expandPath visiblePaths

expandPath :: FilePath -> IO [FilePath]
expandPath path = do
  pathIsDirectory <- doesDirectoryExist path
  if pathIsDirectory
    then collectDirectory path
    else pure [path | hieFilePath path]

hieFilePath :: FilePath -> Bool
hieFilePath =
  (== ".hie") . takeExtension
