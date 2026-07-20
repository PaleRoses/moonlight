module Moonlight.Analysis.ModuleContract
  ( ModuleLayerTag (..),
    ModuleContract (..),
    ModuleParseError (..),
    hasCppDirectives,
    parseModuleContract,
    parseModuleContractFromFile,
  )
where

import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Hs (GhcPs, HsModule)
import Moonlight.Core (splitModuleName)
import Moonlight.Pale.Ghc.ModuleSurface
  ( ModuleSurface (..),
    moduleSurfaceFromGhcPs,
    parseHsModule,
    unParsedModuleName,
    unParsedName,
  )
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

type ModuleContract :: Type
data ModuleContract = ModuleContract
  { moduleContractName :: Maybe String,
    moduleContractImports :: Set String,
    moduleContractExports :: Set String,
    moduleContractLayerTags :: Set ModuleLayerTag
  }
  deriving stock (Eq, Show)

type ModuleLayerTag :: Type
newtype ModuleLayerTag = ModuleLayerTag
  { unModuleLayerTag :: String
  }
  deriving stock (Eq, Ord, Show, Read)

type ModuleParseError :: Type
data ModuleParseError
  = CppPreprocessFailure FilePath String
  | ParserFailure FilePath String
  deriving stock (Eq, Show)

parseModuleContractFromFile :: FilePath -> IO (Either ModuleParseError ModuleContract)
parseModuleContractFromFile sourcePath = do
  moduleContents <- readFile sourcePath
  preprocessed <- preprocessIfNeeded sourcePath moduleContents
  pure (preprocessed >>= parseModuleContract sourcePath)

parseModuleContract :: FilePath -> String -> Either ModuleParseError ModuleContract
parseModuleContract sourcePath moduleContents =
  case parseHsModule sourcePath moduleContents of
    Left parserError -> Left (ParserFailure sourcePath parserError)
    Right moduleAst -> Right (moduleContractFromGhcPs moduleAst)

hasCppDirectives :: String -> Bool
hasCppDirectives contents =
  contents
    & lines
    & any (startsWithDirective . dropWhile isSpace)
  where
    startsWithDirective line =
      case line of
        '#' : _ -> True
        _ -> False

preprocessIfNeeded :: FilePath -> String -> IO (Either ModuleParseError String)
preprocessIfNeeded sourcePath moduleContents =
  if hasCppDirectives moduleContents
    then do
      (exitCode, stdoutOutput, stderrOutput) <-
        readProcessWithExitCode "cpp" ["-traditional-cpp", "-P", sourcePath] ""
      pure
        (case exitCode of
          ExitSuccess -> Right stdoutOutput
          ExitFailure _ -> Left (CppPreprocessFailure sourcePath stderrOutput)
        )
    else pure (Right moduleContents)

moduleContractFromGhcPs :: HsModule GhcPs -> ModuleContract
moduleContractFromGhcPs moduleAst =
  let moduleSurface = moduleSurfaceFromGhcPs moduleAst
      contractName = surfaceModuleName moduleSurface & fmap unParsedModuleName
   in ModuleContract
        { moduleContractName = contractName,
          moduleContractImports = surfaceImportedModules moduleSurface & Set.map unParsedModuleName,
          moduleContractExports = surfaceExportedNames moduleSurface & Set.map unParsedName,
          moduleContractLayerTags = moduleLayerTagsFromName contractName
        }

moduleLayerTagsFromName :: Maybe String -> Set ModuleLayerTag
moduleLayerTagsFromName moduleName =
  moduleName
    & maybe [] (splitModuleName . Text.pack)
    & fmap (ModuleLayerTag . Text.unpack)
    & Set.fromList
