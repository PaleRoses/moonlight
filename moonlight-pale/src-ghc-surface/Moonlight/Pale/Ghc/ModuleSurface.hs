
module Moonlight.Pale.Ghc.ModuleSurface
  ( ParsedModuleName,
    mkParsedModuleName,
    unParsedModuleName,
    ParsedName,
    mkParsedName,
    unParsedName,
    ModuleSurface (..),
    parseWithGhcParser,
    parseHsModule,
    moduleIdentity,
    moduleImportNames,
    moduleExportNames,
    moduleExportIdentifiers,
    exportedIdentifier,
    wrappedNameIdentifier,
    rdrNameIdentifier,
    moduleSurfaceFromGhcPs,
  )
where

import Data.Kind (Type)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.DynFlags (Language (..), languageExtensions)
import GHC.Driver.Flags (OnOff (..), WarningFlag, impliedXFlags)
import GHC.Driver.Session (flagSpecFlag, flagSpecName, xFlags)
import GHC.Hs
  ( GhcPs,
    HsModule (..),
    IE (..),
    IEWrappedName (..),
    ImportDecl (..),
    LIE,
    LIEWrappedName,
  )
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer
  ( P (..),
    ParseResult (..),
    PState,
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc)
import GHC.Types.Error (defaultOpts)
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error
  ( DiagOpts (..),
    pprMessages,
  )
import GHC.Utils.Outputable (defaultSDocContext, showSDocUnsafe)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import Moonlight.Core (IdentifierToken, mkIdentifierTokenWith, renderIdentifierToken)
import Moonlight.Core (isCompactName, isQualifiedModuleName)

type ParsedModuleNameNamespace :: Type
data ParsedModuleNameNamespace

type ParsedModuleName :: Type
newtype ParsedModuleName = ParsedModuleName (IdentifierToken ParsedModuleNameNamespace)
  deriving stock (Eq, Ord, Show)

type ParsedNameNamespace :: Type
data ParsedNameNamespace

type ParsedName :: Type
newtype ParsedName = ParsedName (IdentifierToken ParsedNameNamespace)
  deriving stock (Eq, Ord, Show)

type ModuleSurface :: Type
data ModuleSurface = ModuleSurface
  { surfaceModuleName :: Maybe ParsedModuleName,
    surfaceImportedModules :: Set ParsedModuleName,
    surfaceExportedNames :: Set ParsedName
  }
  deriving stock (Eq, Show)

mkParsedModuleName :: String -> Maybe ParsedModuleName
mkParsedModuleName =
  fmap ParsedModuleName . mkIdentifierTokenWith isQualifiedModuleName . Text.pack

unParsedModuleName :: ParsedModuleName -> String
unParsedModuleName (ParsedModuleName identifierToken) =
  Text.unpack (renderIdentifierToken identifierToken)

mkParsedName :: String -> Maybe ParsedName
mkParsedName =
  fmap ParsedName . mkIdentifierTokenWith isCompactName . Text.pack

unParsedName :: ParsedName -> String
unParsedName (ParsedName identifierToken) =
  Text.unpack (renderIdentifierToken identifierToken)

parseHsModule :: FilePath -> String -> Either String (HsModule GhcPs)
parseHsModule sourcePath moduleContents =
  unLoc <$> parseWithGhcParser sourcePath moduleContents parseModule

parseWithGhcParser :: FilePath -> String -> P a -> Either String a
parseWithGhcParser sourcePath sourceContents parser =
  let parserState =
        initParserState
          (mkParserOpts (parserExtensions sourceContents) parserDiagOpts False False False False)
          (stringToStringBuffer sourceContents)
          (mkRealSrcLoc (mkFastString sourcePath) 1 1)
   in case unP parser parserState of
        POk _ parsedValue -> Right parsedValue
        PFailed parserStateValue -> Left (renderParseFailure parserStateValue)

moduleIdentity :: HsModule GhcPs -> Maybe String
moduleIdentity =
  fmap unParsedModuleName
    . surfaceModuleName
    . moduleSurfaceFromGhcPs

moduleImportNames :: HsModule GhcPs -> Set String
moduleImportNames =
  Set.map unParsedModuleName
    . surfaceImportedModules
    . moduleSurfaceFromGhcPs

moduleExportNames :: HsModule GhcPs -> Set String
moduleExportNames =
  Set.map unParsedName
    . surfaceExportedNames
    . moduleSurfaceFromGhcPs

moduleExportIdentifiers :: Maybe (GenLocated l [LIE GhcPs]) -> Set String
moduleExportIdentifiers maybeExports =
  case maybeExports of
    Nothing -> Set.empty
    Just exports ->
      exports
        & unLoc
        & mapMaybe exportedIdentifier
        & Set.fromList

moduleSurfaceFromGhcPs :: HsModule GhcPs -> ModuleSurface
moduleSurfaceFromGhcPs moduleAst =
  ModuleSurface
    { surfaceModuleName =
        hsmodName moduleAst
          & fmap (moduleNameString . unLoc)
          >>= mkParsedModuleName,
      surfaceImportedModules =
        hsmodImports moduleAst
          & map (moduleNameString . unLoc . ideclName . unLoc)
          & mapMaybe mkParsedModuleName
          & Set.fromList,
      surfaceExportedNames =
        case hsmodExports moduleAst of
          Nothing -> Set.empty
          Just exports ->
            exports
              & unLoc
              & mapMaybe exportedName
              & Set.fromList
    }

exportedIdentifier :: LIE GhcPs -> Maybe String
exportedIdentifier =
  fmap unParsedName . exportedName

exportedName :: LIE GhcPs -> Maybe ParsedName
exportedName exportEntry =
  case unLoc exportEntry of
    IEVar _ wrappedNameValue _ -> wrappedSurfaceName wrappedNameValue
    IEThingAbs _ wrappedNameValue _ -> wrappedSurfaceName wrappedNameValue
    IEThingAll _ wrappedNameValue _ -> wrappedSurfaceName wrappedNameValue
    IEThingWith _ wrappedNameValue _ _ _ -> wrappedSurfaceName wrappedNameValue
    IEModuleContents {} -> Nothing
    IEGroup {} -> Nothing
    IEDoc {} -> Nothing
    IEDocNamed {} -> Nothing

wrappedNameIdentifier :: LIEWrappedName GhcPs -> Maybe String
wrappedNameIdentifier =
  fmap unParsedName . wrappedSurfaceName

wrappedSurfaceName :: LIEWrappedName GhcPs -> Maybe ParsedName
wrappedSurfaceName wrappedNameValue =
  case unLoc wrappedNameValue of
    IEName _ name -> mkParsedName (rdrNameIdentifier (unLoc name))
    IEPattern _ name -> mkParsedName (rdrNameIdentifier (unLoc name))
    IEType _ name -> mkParsedName (rdrNameIdentifier (unLoc name))
    IEDefault _ name -> mkParsedName (rdrNameIdentifier (unLoc name))
    IEData _ name -> mkParsedName (rdrNameIdentifier (unLoc name))

rdrNameIdentifier :: RdrName -> String
rdrNameIdentifier =
  occNameString . rdrNameOcc

renderParseFailure :: PState -> String
renderParseFailure parserStateValue =
  parserStateValue
    & getPsErrorMessages
    & pprMessages defaultOpts
    & showSDocUnsafe

type LanguagePragmaDirective :: Type
data LanguagePragmaDirective
  = UseLanguage !Language
  | EnableExtension !Extension
  | DisableExtension !Extension
  deriving stock (Eq, Show)

type ParserExtensionState :: Type
data ParserExtensionState = ParserExtensionState
  { pesEnabled :: !(EnumSet.EnumSet Extension),
    pesExplicitlyDisabled :: !(EnumSet.EnumSet Extension)
  }

parserExtensions :: String -> EnumSet.EnumSet Extension
parserExtensions moduleContents =
  pesEnabled
    ( foldl'
        applyLanguagePragmaDirective
        ghc2024ParserExtensionState
        (languagePragmaDirectives moduleContents)
    )

ghc2024ParserExtensionState :: ParserExtensionState
ghc2024ParserExtensionState =
  closeImpliedExtensions
    ParserExtensionState
      { pesEnabled = EnumSet.fromList (languageExtensions (Just GHC2024)),
        pesExplicitlyDisabled = EnumSet.empty
      }

applyLanguagePragmaDirective :: ParserExtensionState -> LanguagePragmaDirective -> ParserExtensionState
applyLanguagePragmaDirective _ (UseLanguage languageValue) =
  closeImpliedExtensions
    ParserExtensionState
      { pesEnabled = EnumSet.fromList (languageExtensions (Just languageValue)),
        pesExplicitlyDisabled = EnumSet.empty
      }
applyLanguagePragmaDirective parserExtensionState (EnableExtension extensionValue) =
  closeImpliedExtensions
    parserExtensionState
      { pesEnabled = EnumSet.insert extensionValue (pesEnabled parserExtensionState),
        pesExplicitlyDisabled = EnumSet.delete extensionValue (pesExplicitlyDisabled parserExtensionState)
      }
applyLanguagePragmaDirective parserExtensionState (DisableExtension extensionValue) =
  closeImpliedExtensions
    parserExtensionState
      { pesEnabled = EnumSet.delete extensionValue (pesEnabled parserExtensionState),
        pesExplicitlyDisabled = EnumSet.insert extensionValue (pesExplicitlyDisabled parserExtensionState)
      }

closeImpliedExtensions :: ParserExtensionState -> ParserExtensionState
closeImpliedExtensions parserExtensionState =
  let nextState =
        foldl'
          applyImpliedExtension
          parserExtensionState
          impliedXFlags
   in if sameParserExtensionState nextState parserExtensionState
        then parserExtensionState
        else closeImpliedExtensions nextState

sameParserExtensionState :: ParserExtensionState -> ParserExtensionState -> Bool
sameParserExtensionState leftState rightState =
  EnumSet.toList (pesEnabled leftState) == EnumSet.toList (pesEnabled rightState)
    && EnumSet.toList (pesExplicitlyDisabled leftState) == EnumSet.toList (pesExplicitlyDisabled rightState)

applyImpliedExtension :: ParserExtensionState -> (Extension, OnOff Extension) -> ParserExtensionState
applyImpliedExtension parserExtensionState (triggerExtension, impliedDirective)
  | EnumSet.member triggerExtension (pesEnabled parserExtensionState) =
      applyImpliedDirective parserExtensionState impliedDirective
  | otherwise =
      parserExtensionState

applyImpliedDirective :: ParserExtensionState -> OnOff Extension -> ParserExtensionState
applyImpliedDirective parserExtensionState (On extensionValue)
  | EnumSet.member extensionValue (pesExplicitlyDisabled parserExtensionState) =
      parserExtensionState
  | otherwise =
      parserExtensionState
        { pesEnabled = EnumSet.insert extensionValue (pesEnabled parserExtensionState)
        }
applyImpliedDirective parserExtensionState (Off extensionValue) =
  parserExtensionState
    { pesEnabled = EnumSet.delete extensionValue (pesEnabled parserExtensionState)
    }

languagePragmaDirectives :: String -> [LanguagePragmaDirective]
languagePragmaDirectives =
  foldMap languagePragmaLine . Text.lines . Text.pack

languagePragmaLine :: Text.Text -> [LanguagePragmaDirective]
languagePragmaLine rawLine =
  case Text.stripPrefix (Text.pack "{-# LANGUAGE") (Text.strip rawLine) >>= Text.stripSuffix (Text.pack "#-}") of
    Nothing -> []
    Just pragmaBody ->
      mapMaybe
        (languagePragmaDirective . compactPragmaToken)
        (Text.splitOn (Text.pack ",") pragmaBody)

compactPragmaToken :: Text.Text -> String
compactPragmaToken =
  Text.unpack . Text.filter (not . isSpace)

languagePragmaDirective :: String -> Maybe LanguagePragmaDirective
languagePragmaDirective token =
  case languageName token of
    Just languageValue ->
      Just (UseLanguage languageValue)
    Nothing ->
      case Map.lookup token extensionNameIndex of
        Just extensionValue ->
          Just (EnableExtension extensionValue)
        Nothing ->
          DisableExtension <$> noExtension token

languageName :: String -> Maybe Language
languageName = \case
  "GHC2024" -> Just GHC2024
  "GHC2021" -> Just GHC2021
  "Haskell2010" -> Just Haskell2010
  "Haskell98" -> Just Haskell98
  _ -> Nothing

noExtension :: String -> Maybe Extension
noExtension token =
  case Text.stripPrefix (Text.pack "No") (Text.pack token) of
    Nothing -> Nothing
    Just extensionName -> Map.lookup (Text.unpack extensionName) extensionNameIndex

extensionNameIndex :: Map String Extension
extensionNameIndex =
  Map.fromList
    [ (flagSpecName flagSpec, flagSpecFlag flagSpec)
    | flagSpec <- xFlags
    ]

parserDiagOpts :: DiagOpts
parserDiagOpts =
  DiagOpts
    { diag_warning_flags = EnumSet.empty :: EnumSet.EnumSet WarningFlag,
      diag_fatal_warning_flags = EnumSet.empty :: EnumSet.EnumSet WarningFlag,
      diag_custom_warning_categories = emptyWarningCategorySet,
      diag_fatal_custom_warning_categories = emptyWarningCategorySet,
      diag_warn_is_error = False,
      diag_reverse_errors = False,
      diag_max_errors = Nothing,
      diag_ppr_ctx = defaultSDocContext
    }
