{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieSourceKeyKind (..),
    PathSuffix (..),
    TriedKey (..),
    OracleLookup (..),
    OracleAttachFailure (..),
    HieOracleIndex (..),
    OracleQuery (..),
    buildHieOracleIndex,
    lookupModuleOracle,
    oracleLookupOracle,
    oracleAttachFailure,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import System.FilePath (isPathSeparator, joinPath, normalise, splitDirectories)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))

type HieSourceKeyKind :: Type
data HieSourceKeyKind
  = GivenPathKey
  | AbsolutePathKey
  | RootRelativeKey
  | ModuleSuffixKey
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type PathSuffix :: Type
newtype PathSuffix = PathSuffix (NonEmpty FilePath)
  deriving stock (Eq, Ord, Show)

type TriedKey :: Type
data TriedKey = TriedKey !HieSourceKeyKind !FilePath
  deriving stock (Eq, Ord, Show)

type OracleLookup :: Type
data OracleLookup
  = OracleFound !HieSourceKeyKind !FilePath !ModuleNameOracle
  | OracleMissing ![TriedKey]
  | OracleAmbiguous !HieSourceKeyKind !FilePath ![FilePath]
  deriving stock (Eq, Show)

type OracleAttachFailure :: Type
data OracleAttachFailure
  = OracleLookupMissing ![TriedKey]
  | OracleLookupAmbiguous !HieSourceKeyKind !FilePath ![FilePath]
  deriving stock (Eq, Ord, Show)

type HieOracleIndex :: Type
data HieOracleIndex = HieOracleIndex
  { hoiExact :: !(Map FilePath (NonEmpty ModuleNameOracle)),
    hoiBySuffix :: !(Map PathSuffix (NonEmpty ModuleNameOracle))
  }
  deriving stock (Eq, Show)

type OracleQuery :: Type
data OracleQuery = OracleQuery
  { oqGivenPath :: !FilePath,
    oqAbsolutePath :: !(Maybe FilePath),
    oqSourceRoots :: ![FilePath]
  }
  deriving stock (Eq, Show)

buildHieOracleIndex :: [ModuleNameOracle] -> HieOracleIndex
buildHieOracleIndex oracles =
  HieOracleIndex
    { hoiExact =
        Map.fromListWith
          (<>)
          [(normalise (mnoSourcePath oracle), oracle :| []) | oracle <- oracles],
      hoiBySuffix =
        Map.fromListWith
          (<>)
          [ (pathSuffix, oracle :| [])
          | oracle <- oracles,
            pathSuffix <- pathSuffixes (mnoSourcePath oracle)
          ]
    }

lookupModuleOracle :: HieOracleIndex -> OracleQuery -> OracleLookup
lookupModuleOracle oracleIndex query =
  case firstExactHit exactCandidates of
    Just found ->
      found
    Nothing ->
      lookupSuffixes (fmap fst exactCandidates >>= maybeTriedKey) (querySuffixes query)
  where
    exactCandidates =
      exactQueryKeys query

    maybeTriedKey (TriedKey _ "") =
      []
    maybeTriedKey triedKey =
      [triedKey]

    firstExactHit =
      foldr firstPresent Nothing

    firstPresent (triedKey, keyValue) next =
      case Map.lookup keyValue (hoiExact oracleIndex) of
        Nothing ->
          next
        Just candidates ->
          Just (lookupOutcome triedKey keyValue candidates)

    lookupSuffixes triedBeforeSuffixes =
      appendTriedPrefix triedBeforeSuffixes
        . foldr
          ( \suffixValue next ->
              let suffixKey = suffixFilePath suffixValue
                  triedKey = TriedKey ModuleSuffixKey suffixKey
               in case Map.lookup suffixValue (hoiBySuffix oracleIndex) of
                    Nothing ->
                      appendMissing triedKey next
                    Just candidates ->
                      lookupOutcome triedKey suffixKey candidates
          )
          (OracleMissing [])

    appendMissing triedKey = \case
      OracleMissing triedKeys ->
        OracleMissing (triedKey : triedKeys)
      found ->
        found

    appendTriedPrefix triedPrefix = \case
      OracleMissing triedSuffixes ->
        OracleMissing (triedPrefix <> triedSuffixes)
      found ->
        found

lookupOutcome :: TriedKey -> FilePath -> NonEmpty ModuleNameOracle -> OracleLookup
lookupOutcome (TriedKey keyKind _) matchedKey candidates =
  case NonEmpty.toList candidates of
    [oracle] ->
      OracleFound keyKind (mnoSourcePath oracle) oracle
    ambiguous ->
      OracleAmbiguous keyKind matchedKey (fmap mnoSourcePath ambiguous)

oracleLookupOracle :: OracleLookup -> Maybe ModuleNameOracle
oracleLookupOracle = \case
  OracleFound _ _ oracle ->
    Just oracle
  OracleMissing _ ->
    Nothing
  OracleAmbiguous _ _ _ ->
    Nothing

oracleAttachFailure :: OracleLookup -> Maybe OracleAttachFailure
oracleAttachFailure = \case
  OracleFound {} ->
    Nothing
  OracleMissing triedKeys ->
    Just (OracleLookupMissing triedKeys)
  OracleAmbiguous keyKind keyValue candidates ->
    Just (OracleLookupAmbiguous keyKind keyValue candidates)

exactQueryKeys :: OracleQuery -> [(TriedKey, FilePath)]
exactQueryKeys query =
  [ (TriedKey GivenPathKey givenPath, givenPath)
  ]
    <> maybe [] (\absolutePath -> [(TriedKey AbsolutePathKey absolutePath, absolutePath)]) (normalisedMaybe (oqAbsolutePath query))
    <> [ (TriedKey RootRelativeKey rootRelativePath, rootRelativePath)
       | root <- oqSourceRoots query,
         rootRelativePath <- rootRelativePaths root query
       ]
  where
    givenPath =
      normalise (oqGivenPath query)

normalisedMaybe :: Maybe FilePath -> Maybe FilePath
normalisedMaybe =
  fmap normalise

rootRelativePaths :: FilePath -> OracleQuery -> [FilePath]
rootRelativePaths root query =
  mapMaybe
    (stripRoot root)
    (oqGivenPath query : maybe [] pure (oqAbsolutePath query))

stripRoot :: FilePath -> FilePath -> Maybe FilePath
stripRoot root path =
  fmap joinPath (stripPrefixComponents (pathComponents root) (pathComponents path))

stripPrefixComponents :: [FilePath] -> [FilePath] -> Maybe [FilePath]
stripPrefixComponents prefixComponents pathValue =
  case (prefixComponents, pathValue) of
    ([], []) ->
      Nothing
    ([], remaining@(_ : _)) ->
      Just remaining
    (prefixComponent : remainingPrefix, pathComponent : remainingPath)
      | prefixComponent == pathComponent ->
          stripPrefixComponents remainingPrefix remainingPath
    _ ->
      Nothing

querySuffixes :: OracleQuery -> [PathSuffix]
querySuffixes = pathSuffixes . oqGivenPath

pathSuffixes :: FilePath -> [PathSuffix]
pathSuffixes path =
  maybe [] suffixesFromComponents (NonEmpty.nonEmpty (pathComponents path))

suffixesFromComponents :: NonEmpty FilePath -> [PathSuffix]
suffixesFromComponents components =
  case components of
    component :| [] ->
      [PathSuffix (component :| [])]
    component :| next : remaining ->
      PathSuffix (component :| next : remaining) : suffixesFromComponents (next :| remaining)

suffixFilePath :: PathSuffix -> FilePath
suffixFilePath (PathSuffix components) =
  joinPath (NonEmpty.toList components)

pathComponents :: FilePath -> [FilePath]
pathComponents =
  filter (not . rootOrEmpty) . splitDirectories . normalise

rootOrEmpty :: FilePath -> Bool
rootOrEmpty component =
  null component || all isPathSeparator component
