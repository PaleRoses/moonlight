{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Write.Declaration
  ( RecordDeclaration (..),
    RecordFieldRow (..),
    RecordSelectorRewrite (..),
    DeclarationPatch (..),
    DeclarationSealObligation (..),
    recordDeclarations,
    recordDeclarationsFromParsedModule,
    planRecordFieldDeletion,
    planRecordOwnershipRewrite,
    patchedDeclarationSource,
    sealDeclarationPatch,
    sealDeclarationObligations,
    sealDeclarationObligationsFromParsedModule,
  )
where

import Data.Char (isSpace)
import Data.Foldable (fold, toList, traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List (find)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs qualified as Ghc
import GHC.Hs
  ( ConDecl (..),
    HsDecl (..),
    HsConDeclRecField (..),
    HsConDetails (..),
    HsDataDefn (..),
    TyClDecl (..),
    cdrf_names,
    con_args,
    con_name,
    dd_cons,
    foLabel,
    hsmodDecls,
    tcdDataDefn,
    tcdLName,
  )
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)
import Melusine.Nebula.Core (NebulaError (..))
import Melusine.Nebula.Source.Ast
  ( RecordConstruction (..),
    RecordConstructionField (..),
    SelectorApplication (..),
    locatedRecordConstructions,
    locatedRecordConstructionsFromParsedModule,
    locatedSelectorApplications,
    locatedSelectorApplicationsFromParsedModule,
    sourceRegionFromSrcSpan,
    sourceRegionText,
    wholeLineRegion,
  )
import Melusine.Nebula.Write.Patch (SourceSplice (..), applySplices)
import Moonlight.Pale.Ghc.Expr (SourceRegion (..))
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

-- | A source-backed view of one H98 record constructor declaration.
type RecordDeclaration :: Type
data RecordDeclaration = RecordDeclaration
  { rdTypeName :: !String,
    rdConstructorName :: !String,
    rdRegion :: !SourceRegion,
    rdFieldRows :: ![RecordFieldRow]
  }
  deriving stock (Eq, Show)

-- | One syntactic record-field row.  Multiple names in one row share a single
-- source span, so deleting only some of them is a typed refusal.
type RecordFieldRow :: Type
data RecordFieldRow = RecordFieldRow
  { rfrNames :: !(Set String),
    rfrRegion :: !SourceRegion
  }
  deriving stock (Eq, Show)

type RecordSelectorRewrite :: Type
data RecordSelectorRewrite = RecordSelectorRewrite
  { rsrDeletedSelector :: !String,
    rsrOwnerSelector :: !String,
    rsrProjectedSelector :: !String
  }
  deriving stock (Eq, Show)

type DeclarationPatch :: Type
data DeclarationPatch = DeclarationPatch
  { dpSplices :: ![SourceSplice],
    dpObligations :: ![DeclarationSealObligation]
  }
  deriving stock (Eq, Show)

type DeclarationSealObligation :: Type
data DeclarationSealObligation
  = RecordFieldDeletionObligation !String !(Set String) !(Set String)
  | RecordConstructorFieldDeletionObligation !String !(Set String)
  | RecordSelectorRewriteObligation !(Set String)
  deriving stock (Eq, Show)

recordDeclarations :: FilePath -> String -> Either NebulaError [RecordDeclaration]
recordDeclarations path source =
  recordDeclarationsFromParsedModule
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

recordDeclarationsFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [RecordDeclaration]
recordDeclarationsFromParsedModule =
  foldMap locatedDeclRecordDeclarations . hsmodDecls

recordConstructorDeletionSplices :: FilePath -> String -> String -> Set String -> Either NebulaError [SourceSplice]
recordConstructorDeletionSplices path source constructorName deletedFields = do
  constructions <- locatedRecordConstructions path source
  let lineIndex = sourceLineIndex source
  fold
    <$> traverse
      (fieldDeletionSplices lineIndex ((`Set.member` deletedFields) . rcfName) rcfRegion . rcFieldRows)
      (filter ((== constructorName) . rcConstructorName) constructions)

selectorRewriteSplices :: FilePath -> String -> [RecordSelectorRewrite] -> Either NebulaError [SourceSplice]
selectorRewriteSplices path source selectorRewrites = do
  selectorApplications <- locatedSelectorApplications path source
  traverse
    (selectorApplicationSplice source selectorRewrites)
    [ application
    | application <- selectorApplications,
      any ((== saSelectorName application) . rsrDeletedSelector) selectorRewrites
    ]

selectorApplicationSplice :: String -> [RecordSelectorRewrite] -> SelectorApplication -> Either NebulaError SourceSplice
selectorApplicationSplice source selectorRewrites application =
  case find ((== saSelectorName application) . rsrDeletedSelector) selectorRewrites of
    Nothing ->
      Left (NebulaWriteBackError ("selector rewrite missing for " <> saSelectorName application))
    Just selectorRewrite -> do
      argumentText <- sourceRegionText source (saArgumentRegion application)
      pure
        SourceSplice
          { ssRegion = saApplicationRegion application,
            ssReplacement =
              rsrProjectedSelector selectorRewrite
                <> " ("
                <> rsrOwnerSelector selectorRewrite
                <> " ("
                <> argumentText
                <> "))"
          }

planRecordFieldDeletion :: FilePath -> String -> String -> Set String -> Either NebulaError DeclarationPatch
planRecordFieldDeletion path source recordName deletedFields = do
  declarations <- recordDeclarations path source
  recordDeclaration <-
    maybe
      (Left (NebulaWriteBackError ("record declaration not found: " <> recordName)))
      Right
      (findRecordDeclaration recordName declarations)
  let originalFields = recordDeclarationFieldNames recordDeclaration
      missingFields = deletedFields Set.\\ originalFields
      partiallyDeletedRows = filter (rowPartiallyDeleted deletedFields) (rdFieldRows recordDeclaration)
  if not (Set.null missingFields)
    then Left (NebulaWriteBackError ("record fields not found on " <> recordName <> ": " <> show (Set.toAscList missingFields)))
    else
      case partiallyDeletedRows of
        _ : _ ->
          Left (NebulaWriteBackError ("record field deletion would split a multi-name row on " <> recordName))
        [] ->
          ( \deletionSplices ->
              DeclarationPatch
                { dpSplices = deletionSplices,
                  dpObligations =
                    [ RecordFieldDeletionObligation
                        recordName
                        originalFields
                        deletedFields
                    ]
                }
          )
            <$> fieldDeletionSplices (sourceLineIndex source) (rowFullyDeleted deletedFields) rfrRegion (rdFieldRows recordDeclaration)

planRecordOwnershipRewrite :: FilePath -> String -> String -> Set String -> [RecordSelectorRewrite] -> Either NebulaError DeclarationPatch
planRecordOwnershipRewrite path source recordName deletedFields selectorRewrites = do
  declarationPatch <- planRecordFieldDeletion path source recordName deletedFields
  declarations <- recordDeclarations path source
  recordDeclaration <-
    maybe
      (Left (NebulaWriteBackError ("record declaration not found: " <> recordName)))
      Right
      (findRecordDeclaration recordName declarations)
  constructorSplices <- recordConstructorDeletionSplices path source (rdConstructorName recordDeclaration) deletedFields
  selectorSplices <- selectorRewriteSplices path source selectorRewrites
  pure
    DeclarationPatch
      { dpSplices = dpSplices declarationPatch <> constructorSplices <> selectorSplices,
        dpObligations =
          dpObligations declarationPatch
            <> [ RecordConstructorFieldDeletionObligation
                   (rdConstructorName recordDeclaration)
                   deletedFields
               | not (null constructorSplices)
               ]
            <> [ RecordSelectorRewriteObligation
                   (Set.fromList (fmap rsrDeletedSelector selectorRewrites))
               | not (null selectorSplices)
               ]
      }

findRecordDeclaration :: String -> [RecordDeclaration] -> Maybe RecordDeclaration
findRecordDeclaration recordName =
  find (\declaration -> rdTypeName declaration == recordName || rdConstructorName declaration == recordName)

patchedDeclarationSource :: DeclarationPatch -> String -> Either NebulaError String
patchedDeclarationSource patch =
  applySplices (dpSplices patch)

sealDeclarationPatch :: FilePath -> String -> DeclarationPatch -> Either NebulaError String
sealDeclarationPatch path source patch = do
  patched <- patchedDeclarationSource patch source
  sealDeclarationObligations path patched (dpObligations patch)
  pure patched

sealDeclarationObligations :: FilePath -> String -> [DeclarationSealObligation] -> Either NebulaError ()
sealDeclarationObligations path patched obligations = do
  parsedModule <-
    either
      (Left . NebulaParseError)
      Right
      (parseHsModule path patched)
  sealDeclarationObligationsFromParsedModule parsedModule obligations

sealDeclarationObligationsFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [DeclarationSealObligation] -> Either NebulaError ()
sealDeclarationObligationsFromParsedModule parsedModule obligations =
  let declarations =
        recordDeclarationsFromParsedModule parsedModule
      constructorFields =
        locatedRecordConstructionsFromParsedModule parsedModule
      selectorApplications =
        locatedSelectorApplicationsFromParsedModule parsedModule
   in traverse_ (sealDeclarationObligation declarations constructorFields selectorApplications) obligations

sealDeclarationObligation :: [RecordDeclaration] -> [RecordConstruction] -> [SelectorApplication] -> DeclarationSealObligation -> Either NebulaError ()
sealDeclarationObligation declarations constructorFields selectorApplications obligation =
  case obligation of
    RecordFieldDeletionObligation recordName originalFields deletedFields -> do
      recordDeclaration <-
        maybe
          (Left (NebulaSealError recordName "record declaration is missing after declaration patch"))
          Right
          (find ((== recordName) . rdTypeName) declarations)
      let expectedFields = originalFields Set.\\ deletedFields
          actualFields = recordDeclarationFieldNames recordDeclaration
      if actualFields == expectedFields
        then Right ()
        else Left (NebulaSealError recordName ("record field set mismatch after declaration patch: expected " <> show (Set.toAscList expectedFields) <> ", got " <> show (Set.toAscList actualFields)))
    RecordConstructorFieldDeletionObligation constructorName deletedFields ->
      let remainingDeletedFields =
            foldMap
              ((`Set.intersection` deletedFields) . rcFields)
              (filter ((== constructorName) . rcConstructorName) constructorFields)
       in if Set.null remainingDeletedFields
            then Right ()
            else Left (NebulaSealError constructorName ("deleted constructor fields remain after declaration patch: " <> show (Set.toAscList remainingDeletedFields)))
    RecordSelectorRewriteObligation deletedSelectors ->
      let remainingDeletedSelectors =
            Set.fromList
              [ saSelectorName application
              | application <- selectorApplications,
                saSelectorName application `Set.member` deletedSelectors
              ]
       in if Set.null remainingDeletedSelectors
            then Right ()
            else Left (NebulaSealError "record selectors" ("deleted selector applications remain after declaration patch: " <> show (Set.toAscList remainingDeletedSelectors)))

locatedDeclRecordDeclarations :: Ghc.LHsDecl Ghc.GhcPs -> [RecordDeclaration]
locatedDeclRecordDeclarations locatedDecl =
  case unLoc locatedDecl of
    TyClD _ DataDecl {tcdLName = typeName, tcdDataDefn = dataDefn} ->
      maybe [] (dataRecordDeclarations (occNameString (rdrNameOcc (unLoc typeName))) dataDefn) (sourceRegionFromSrcSpan (getLocA locatedDecl))
    _ ->
      []

dataRecordDeclarations :: String -> HsDataDefn Ghc.GhcPs -> SourceRegion -> [RecordDeclaration]
dataRecordDeclarations typeName dataDefn declarationRegion =
  foldMap (constructorRecordDeclaration typeName declarationRegion) (dataDefinitionConstructors dataDefn)

constructorRecordDeclaration :: String -> SourceRegion -> Ghc.LConDecl Ghc.GhcPs -> [RecordDeclaration]
constructorRecordDeclaration typeName declarationRegion locatedConstructor =
  case unLoc locatedConstructor of
    ConDeclH98 {con_name = constructorName, con_args = RecCon fields} ->
      [ RecordDeclaration
          { rdTypeName = typeName,
            rdConstructorName = occNameString (rdrNameOcc (unLoc constructorName)),
            rdRegion = declarationRegion,
            rdFieldRows = mapMaybeRecordFieldRows (unLoc fields)
          }
      ]
    _ ->
      []

dataDefinitionConstructors :: HsDataDefn Ghc.GhcPs -> [Ghc.LConDecl Ghc.GhcPs]
dataDefinitionConstructors =
  toList . dd_cons

mapMaybeRecordFieldRows :: [Ghc.LHsConDeclRecField Ghc.GhcPs] -> [RecordFieldRow]
mapMaybeRecordFieldRows =
  foldMap (maybe [] pure . recordFieldRow)

recordFieldRow :: Ghc.LHsConDeclRecField Ghc.GhcPs -> Maybe RecordFieldRow
recordFieldRow locatedField = do
  region <- sourceRegionFromSrcSpan (getLocA locatedField)
  case unLoc locatedField of
    HsConDeclRecField {cdrf_names = names} ->
      Just
        RecordFieldRow
          { rfrNames = Set.fromList (fmap fieldOccName names),
            rfrRegion = region
          }

fieldOccName :: Ghc.LFieldOcc Ghc.GhcPs -> String
fieldOccName fieldOcc =
  occNameString (rdrNameOcc (unLoc (foLabel (unLoc fieldOcc))))

recordDeclarationFieldNames :: RecordDeclaration -> Set String
recordDeclarationFieldNames =
  foldMap rfrNames . rdFieldRows

rowFullyDeleted :: Set String -> RecordFieldRow -> Bool
rowFullyDeleted deletedFields row =
  rfrNames row `Set.isSubsetOf` deletedFields

rowPartiallyDeleted :: Set String -> RecordFieldRow -> Bool
rowPartiallyDeleted deletedFields row =
  let matchingFields = rfrNames row `Set.intersection` deletedFields
   in not (Set.null matchingFields) && not (rfrNames row `Set.isSubsetOf` deletedFields)

type SourceLineIndex = IntMap.IntMap String

sourceLineIndex :: String -> SourceLineIndex
sourceLineIndex =
  IntMap.fromDistinctAscList . zip [1 ..] . lines

fieldDeletionSplices :: SourceLineIndex -> (row -> Bool) -> (row -> SourceRegion) -> [row] -> Either NebulaError [SourceSplice]
fieldDeletionSplices lineIndex isDeleted regionOf rows =
  (fmap (fieldRegionLineSplice . regionOf) deletedRows <>)
    <$> maybe
      (Right [])
      (fmap maybeToList . trailingCommaSplice lineIndex . regionOf)
      (retainedRowBeforeDeletedSuffix isDeleted rows)
  where
    deletedRows =
      filter isDeleted rows

retainedRowBeforeDeletedSuffix :: (row -> Bool) -> [row] -> Maybe row
retainedRowBeforeDeletedSuffix isDeleted rows =
  case reverse rows of
    finalRow : reversedRows
      | isDeleted finalRow ->
          listToMaybe (dropWhile isDeleted reversedRows)
    _ ->
      Nothing

trailingCommaSplice :: SourceLineIndex -> SourceRegion -> Either NebulaError (Maybe SourceSplice)
trailingCommaSplice lineIndex region =
  case IntMap.lookup (srEndLine region) lineIndex of
    Nothing ->
      Left (NebulaSpliceError ("record field line is outside the source: " <> show region))
    Just lineText ->
      let suffix = drop (srEndCol region - 1) lineText
          (spacing, remainder) = span isSpace suffix
       in case remainder of
            ',' : _ ->
              Right
                ( Just
                    SourceSplice
                      { ssRegion =
                          SourceRegion
                            { srStartLine = srEndLine region,
                              srStartCol = srEndCol region,
                              srEndLine = srEndLine region,
                              srEndCol = srEndCol region + length spacing + 1
                            },
                        ssReplacement = ""
                      }
                )
            _ ->
              Right Nothing

fieldRegionLineSplice :: SourceRegion -> SourceSplice
fieldRegionLineSplice region =
  SourceSplice
    { ssRegion = wholeLineRegion region,
      ssReplacement = ""
    }
