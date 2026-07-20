{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Source.Ast
  ( LocatedBinding (..),
    RecordConstruction (..),
    RecordConstructionField (..),
    RecordExpressionShape (..),
    RecordFieldValue (..),
    RecordUpdate (..),
    SelectorApplication (..),
    locatedRecordConstructions,
    locatedRecordConstructionsFromParsedModule,
    locatedRecordUpdates,
    locatedRecordUpdatesFromParsedModule,
    locatedSelectorApplications,
    locatedSelectorApplicationsFromParsedModule,
    locatedValueBindings,
    locatedValueBindingsFromParsedModule,
    bindingGlobalNames,
    bindingRecordConstructions,
    bindingRecordUpdates,
    bindingSelectorApplications,
    bindGlobalNames,
    bindRecordConstructions,
    bindRecordUpdates,
    bindSelectorApplications,
    expressionChildren,
    locatedExprVarName,
    recordExpressionFieldName,
    sourceRegionFromSrcSpan,
    sourceRegionText,
    wholeLineRegion,
  )
where

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs qualified as Ghc
import GHC.Hs
  ( ClsInstDecl (..),
    FieldOcc (..),
    GRHS (..),
    GRHSs (..),
    HsBind,
    HsDecl (..),
    HsExpr (..),
    HsFieldBind (..),
    HsLocalBindsLR (..),
    HsTupArg (..),
    HsValBindsLR (..),
    InstDecl (..),
    Match (..),
    MatchGroup (..),
    Sig (..),
    StmtLR (..),
    foLabel,
    fun_id,
    fun_matches,
    grhssGRHSs,
    grhssLocalBinds,
    hfbLHS,
    hfbRHS,
    hsmodDecls,
    m_grhss,
    mg_alts,
    pat_rhs,
    rec_flds,
    rcon_con,
    rcon_flds,
    rupd_expr,
    rupd_flds,
  )
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc
  ( SrcSpan (..),
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )
import Melusine.Nebula.Core (NebulaError (..))
import Moonlight.Pale.Ghc.Expr (SourceRegion (..), renderRdrName)
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

type LocatedBinding :: Type
data LocatedBinding = LocatedBinding
  { lbName :: !String,
    lbRegion :: !SourceRegion,
    lbBind :: !(HsBind Ghc.GhcPs)
  }

type RecordConstruction :: Type
data RecordConstruction = RecordConstruction
  { rcConstructorName :: !String,
    rcRegion :: !SourceRegion,
    rcFields :: !(Set String),
    rcFieldRows :: ![RecordConstructionField]
  }
  deriving stock (Eq, Show)

type RecordConstructionField :: Type
data RecordConstructionField = RecordConstructionField
  { rcfName :: !String,
    rcfRegion :: !SourceRegion,
    rcfValue :: !RecordFieldValue
  }
  deriving stock (Eq, Show)

type RecordFieldValue :: Type
data RecordFieldValue
  = RecordFieldDirect !String
  | RecordFieldProjection !String !String
  | RecordFieldOther !RecordExpressionShape
  deriving stock (Eq, Ord, Show)

type RecordExpressionShape :: Type
data RecordExpressionShape
  = RecordShapeVar
  | RecordShapeApplication !(Maybe String) !Int
  | RecordShapeRecordConstruction !String !(Set String)
  | RecordShapeLet
  | RecordShapeLambda
  | RecordShapeCase
  | RecordShapeDo
  | RecordShapeOther
  deriving stock (Eq, Ord, Show)

type RecordUpdate :: Type
data RecordUpdate = RecordUpdate
  { ruRegion :: !SourceRegion
  }
  deriving stock (Eq, Show)

type SelectorApplication :: Type
data SelectorApplication = SelectorApplication
  { saSelectorName :: !String,
    saArgumentRegion :: !SourceRegion,
    saApplicationRegion :: !SourceRegion
  }
  deriving stock (Eq, Show)

type TypeSignatureRegion :: Type
data TypeSignatureRegion = TypeSignatureRegion
  { tsrName :: !String,
    tsrRegion :: !SourceRegion
  }

locatedValueBindings :: FilePath -> String -> Either NebulaError [LocatedBinding]
locatedValueBindings path source =
  locatedValueBindingsFromParsedModule
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

locatedValueBindingsFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [LocatedBinding]
locatedValueBindingsFromParsedModule parsedModule =
  let declarations = hsmodDecls parsedModule
      signatures = foldMap locatedSignatureRegions declarations
   in foldMap (locatedDeclarationBindings signatures) declarations

locatedRecordConstructions :: FilePath -> String -> Either NebulaError [RecordConstruction]
locatedRecordConstructions path source =
  locatedRecordConstructionsFromParsedModule
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

locatedRecordConstructionsFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [RecordConstruction]
locatedRecordConstructionsFromParsedModule =
  foldMap bindingRecordConstructions . locatedValueBindingsFromParsedModule

locatedRecordUpdates :: FilePath -> String -> Either NebulaError [RecordUpdate]
locatedRecordUpdates path source =
  locatedRecordUpdatesFromParsedModule
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

locatedRecordUpdatesFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [RecordUpdate]
locatedRecordUpdatesFromParsedModule =
  foldMap bindingRecordUpdates . locatedValueBindingsFromParsedModule

locatedSelectorApplications :: FilePath -> String -> Either NebulaError [SelectorApplication]
locatedSelectorApplications path source =
  locatedSelectorApplicationsFromParsedModule
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

locatedSelectorApplicationsFromParsedModule :: Ghc.HsModule Ghc.GhcPs -> [SelectorApplication]
locatedSelectorApplicationsFromParsedModule =
  foldMap bindingSelectorApplications . locatedValueBindingsFromParsedModule

locatedSignatureRegions :: Ghc.LHsDecl Ghc.GhcPs -> [TypeSignatureRegion]
locatedSignatureRegions locatedDecl =
  case (unLoc locatedDecl, sourceRegionFromSrcSpan (getLocA locatedDecl)) of
    (SigD _ (TypeSig _ names _), Just region) ->
      [ TypeSignatureRegion
          { tsrName = occNameString (rdrNameOcc (unLoc name)),
            tsrRegion = region
          }
      | name <- names
      ]
    _ ->
      []

locatedDeclarationBindings :: [TypeSignatureRegion] -> Ghc.LHsDecl Ghc.GhcPs -> [LocatedBinding]
locatedDeclarationBindings signatures locatedDecl =
  case unLoc locatedDecl of
    ValD _ binding ->
      maybeToList (locatedBindingAt signatures (getLocA locatedDecl) binding)
    InstD _ ClsInstD {cid_inst = ClsInstDecl {cid_binds = bindings}} ->
      mapMaybe locatedInstanceBinding (toList bindings)
    _ ->
      []

locatedInstanceBinding :: Ghc.LHsBind Ghc.GhcPs -> Maybe LocatedBinding
locatedInstanceBinding locatedBinding =
  locatedBindingAt [] (getLocA locatedBinding) (unLoc locatedBinding)

locatedBindingAt :: [TypeSignatureRegion] -> SrcSpan -> HsBind Ghc.GhcPs -> Maybe LocatedBinding
locatedBindingAt signatures bindingSpan binding = do
  bindingRegion <- sourceRegionFromSrcSpan bindingSpan
  bindingName <- bindName binding
  pure
    LocatedBinding
      { lbName = bindingName,
        lbRegion = bindingRegionWithSignature signatures bindingName bindingRegion,
        lbBind = binding
      }

bindName :: HsBind Ghc.GhcPs -> Maybe String
bindName = \case
  Ghc.FunBind {fun_id = name} ->
    Just (occNameString (rdrNameOcc (unLoc name)))
  _ ->
    Nothing

bindingRegionWithSignature :: [TypeSignatureRegion] -> String -> SourceRegion -> SourceRegion
bindingRegionWithSignature signatures bindingName bindingRegion =
  maybe bindingRegion (`combinedSourceRegion` bindingRegion) (nearestSignatureRegion signatures bindingName bindingRegion)

nearestSignatureRegion :: [TypeSignatureRegion] -> String -> SourceRegion -> Maybe SourceRegion
nearestSignatureRegion signatures bindingName bindingRegion =
  case eligibleSignatures of
    [] ->
      Nothing
    rows ->
      Just (tsrRegion (maximumBy (comparing (regionStartKey . tsrRegion)) rows))
  where
    eligibleSignatures =
      filter
        (\signature -> tsrName signature == bindingName && regionStartsBefore (tsrRegion signature) bindingRegion)
        signatures

combinedSourceRegion :: SourceRegion -> SourceRegion -> SourceRegion
combinedSourceRegion startRegion endRegion =
  SourceRegion
    { srStartLine = srStartLine startRegion,
      srStartCol = srStartCol startRegion,
      srEndLine = srEndLine endRegion,
      srEndCol = srEndCol endRegion
    }

regionStartsBefore :: SourceRegion -> SourceRegion -> Bool
regionStartsBefore leftRegion rightRegion =
  regionStartKey leftRegion < regionStartKey rightRegion

regionStartKey :: SourceRegion -> (Int, Int)
regionStartKey region =
  (srStartLine region, srStartCol region)

bindingGlobalNames :: LocatedBinding -> Set String
bindingGlobalNames =
  bindGlobalNames . lbBind

bindGlobalNames :: HsBind Ghc.GhcPs -> Set String
bindGlobalNames = \case
  Ghc.FunBind {fun_matches = matches} ->
    matchGroupGlobalNames matches
  Ghc.PatBind {pat_rhs = rhs} ->
    grhssGlobalNames rhs
  _ ->
    Set.empty

matchGroupGlobalNames :: MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Set String
matchGroupGlobalNames matches =
  foldMap locatedMatchGlobalNames (unLoc (mg_alts matches))

locatedMatchGlobalNames :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Set String
locatedMatchGlobalNames locatedMatch =
  grhssGlobalNames (m_grhss (unLoc locatedMatch))

grhssGlobalNames :: GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Set String
grhssGlobalNames grhss =
  localBindsGlobalNames (grhssLocalBinds grhss)
    <> foldMap locatedGrhsGlobalNames (grhssGRHSs grhss)

locatedGrhsGlobalNames :: Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Set String
locatedGrhsGlobalNames locatedGrhs =
  case unLoc locatedGrhs of
    GRHS _ _ body ->
      locatedExprGlobalNames body

locatedExprGlobalNames :: Ghc.LHsExpr Ghc.GhcPs -> Set String
locatedExprGlobalNames locatedExpr =
  case unLoc locatedExpr of
    HsVar _ variableName ->
      Set.singleton (occNameString (rdrNameOcc (unLoc variableName)))
    HsLet _ localBinds body ->
      localBindsGlobalNames localBinds <> locatedExprGlobalNames body
    HsLam _ _ matches ->
      matchGroupGlobalNames matches
    HsCase _ scrutinee matches ->
      locatedExprGlobalNames scrutinee <> matchGroupGlobalNames matches
    HsDo _ _ statements ->
      foldMap locatedStmtGlobalNames (unLoc statements)
    RecordCon {rcon_con = constructorName, rcon_flds = fields} ->
      Set.singleton (occNameString (rdrNameOcc (unLoc constructorName)))
        <> foldMap (locatedExprGlobalNames . hfbRHS . unLoc) (rec_flds fields)
    expressionValue ->
      foldMap locatedExprGlobalNames (expressionChildren expressionValue)

localBindsGlobalNames :: HsLocalBindsLR Ghc.GhcPs Ghc.GhcPs -> Set String
localBindsGlobalNames = \case
  HsValBinds _ valueBinds ->
    valueBindsGlobalNames valueBinds
  _ ->
    Set.empty

valueBindsGlobalNames :: HsValBindsLR Ghc.GhcPs Ghc.GhcPs -> Set String
valueBindsGlobalNames = \case
  ValBinds _ binds _ ->
    foldMap (bindGlobalNames . unLoc) (toList binds)
  _ ->
    Set.empty

locatedStmtGlobalNames :: Ghc.ExprLStmt Ghc.GhcPs -> Set String
locatedStmtGlobalNames locatedStmt =
  case unLoc locatedStmt of
    LastStmt _ body _ _ ->
      locatedExprGlobalNames body
    BindStmt _ _ body ->
      locatedExprGlobalNames body
    BodyStmt _ body _ _ ->
      locatedExprGlobalNames body
    LetStmt _ localBinds ->
      localBindsGlobalNames localBinds
    RecStmt {Ghc.recS_stmts = statements} ->
      foldMap locatedStmtGlobalNames (unLoc statements)
    _ ->
      Set.empty

bindingRecordConstructions :: LocatedBinding -> [RecordConstruction]
bindingRecordConstructions =
  bindRecordConstructions . lbBind

bindingRecordUpdates :: LocatedBinding -> [RecordUpdate]
bindingRecordUpdates =
  bindRecordUpdates . lbBind

bindingSelectorApplications :: LocatedBinding -> [SelectorApplication]
bindingSelectorApplications =
  bindSelectorApplications . lbBind

bindRecordConstructions :: HsBind Ghc.GhcPs -> [RecordConstruction]
bindRecordConstructions = \case
  Ghc.FunBind {fun_matches = matches} ->
    matchGroupRecordConstructions matches
  Ghc.PatBind {pat_rhs = rhs} ->
    grhssRecordConstructions rhs
  _ ->
    []

bindRecordUpdates :: HsBind Ghc.GhcPs -> [RecordUpdate]
bindRecordUpdates = \case
  Ghc.FunBind {fun_matches = matches} ->
    matchGroupRecordUpdates matches
  Ghc.PatBind {pat_rhs = rhs} ->
    grhssRecordUpdates rhs
  _ ->
    []

bindSelectorApplications :: HsBind Ghc.GhcPs -> [SelectorApplication]
bindSelectorApplications = \case
  Ghc.FunBind {fun_matches = matches} ->
    matchGroupSelectorApplications matches
  Ghc.PatBind {pat_rhs = rhs} ->
    grhssSelectorApplications rhs
  _ ->
    []

matchGroupRecordConstructions :: MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordConstruction]
matchGroupRecordConstructions matches =
  foldMap locatedMatchRecordConstructions (unLoc (mg_alts matches))

locatedMatchRecordConstructions :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordConstruction]
locatedMatchRecordConstructions locatedMatch =
  grhssRecordConstructions (m_grhss (unLoc locatedMatch))

grhssRecordConstructions :: GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordConstruction]
grhssRecordConstructions grhss =
  localBindsRecordConstructions (grhssLocalBinds grhss)
    <> foldMap locatedGrhsRecordConstructions (grhssGRHSs grhss)

locatedGrhsRecordConstructions :: Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordConstruction]
locatedGrhsRecordConstructions locatedGrhs =
  case unLoc locatedGrhs of
    GRHS _ _ body ->
      locatedExprRecordConstructions body

locatedExprRecordConstructions :: Ghc.LHsExpr Ghc.GhcPs -> [RecordConstruction]
locatedExprRecordConstructions locatedExpr =
  case unLoc locatedExpr of
    HsLet _ localBinds body ->
      localBindsRecordConstructions localBinds <> locatedExprRecordConstructions body
    HsLam _ _ matches ->
      matchGroupRecordConstructions matches
    HsCase _ scrutinee matches ->
      locatedExprRecordConstructions scrutinee <> matchGroupRecordConstructions matches
    HsDo _ _ statements ->
      foldMap locatedStmtRecordConstructions (unLoc statements)
    RecordCon {rcon_con = constructorName, rcon_flds = fields} ->
      maybeToList
        ( RecordConstruction
            (occNameString (rdrNameOcc (unLoc constructorName)))
            <$> sourceRegionFromSrcSpan (getLocA locatedExpr)
            <*> pure (recordFieldNames fields)
            <*> pure (recordConstructionFields fields)
        )
        <> foldMap (locatedExprRecordConstructions . hfbRHS . unLoc) (rec_flds fields)
    RecordUpd {rupd_expr = recordExpr, rupd_flds = fields} ->
      locatedExprRecordConstructions recordExpr
        <> foldMap (locatedExprRecordConstructions . hfbRHS . unLoc) (recUpdFieldBinds fields)
    expressionValue ->
      foldMap locatedExprRecordConstructions (expressionChildren expressionValue)

localBindsRecordConstructions :: HsLocalBindsLR Ghc.GhcPs Ghc.GhcPs -> [RecordConstruction]
localBindsRecordConstructions = \case
  HsValBinds _ valueBinds ->
    valueBindsRecordConstructions valueBinds
  _ ->
    []

valueBindsRecordConstructions :: HsValBindsLR Ghc.GhcPs Ghc.GhcPs -> [RecordConstruction]
valueBindsRecordConstructions = \case
  ValBinds _ binds _ ->
    foldMap (bindRecordConstructions . unLoc) (toList binds)
  _ ->
    []

locatedStmtRecordConstructions :: Ghc.ExprLStmt Ghc.GhcPs -> [RecordConstruction]
locatedStmtRecordConstructions locatedStmt =
  case unLoc locatedStmt of
    LastStmt _ body _ _ ->
      locatedExprRecordConstructions body
    BindStmt _ _ body ->
      locatedExprRecordConstructions body
    BodyStmt _ body _ _ ->
      locatedExprRecordConstructions body
    LetStmt _ localBinds ->
      localBindsRecordConstructions localBinds
    RecStmt {Ghc.recS_stmts = statements} ->
      foldMap locatedStmtRecordConstructions (unLoc statements)
    _ ->
      []

matchGroupRecordUpdates :: MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordUpdate]
matchGroupRecordUpdates matches =
  foldMap locatedMatchRecordUpdates (unLoc (mg_alts matches))

locatedMatchRecordUpdates :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordUpdate]
locatedMatchRecordUpdates locatedMatch =
  grhssRecordUpdates (m_grhss (unLoc locatedMatch))

grhssRecordUpdates :: GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordUpdate]
grhssRecordUpdates grhss =
  localBindsRecordUpdates (grhssLocalBinds grhss)
    <> foldMap locatedGrhsRecordUpdates (grhssGRHSs grhss)

locatedGrhsRecordUpdates :: Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [RecordUpdate]
locatedGrhsRecordUpdates locatedGrhs =
  case unLoc locatedGrhs of
    GRHS _ _ body ->
      locatedExprRecordUpdates body

locatedExprRecordUpdates :: Ghc.LHsExpr Ghc.GhcPs -> [RecordUpdate]
locatedExprRecordUpdates locatedExpr =
  case unLoc locatedExpr of
    HsLet _ localBinds body ->
      localBindsRecordUpdates localBinds <> locatedExprRecordUpdates body
    HsLam _ _ matches ->
      matchGroupRecordUpdates matches
    HsCase _ scrutinee matches ->
      locatedExprRecordUpdates scrutinee <> matchGroupRecordUpdates matches
    HsDo _ _ statements ->
      foldMap locatedStmtRecordUpdates (unLoc statements)
    RecordCon {rcon_flds = fields} ->
      foldMap (locatedExprRecordUpdates . hfbRHS . unLoc) (rec_flds fields)
    RecordUpd {rupd_expr = recordExpr, rupd_flds = fields} ->
      maybeToList (RecordUpdate <$> sourceRegionFromSrcSpan (getLocA locatedExpr))
        <> locatedExprRecordUpdates recordExpr
        <> foldMap (locatedExprRecordUpdates . hfbRHS . unLoc) (recUpdFieldBinds fields)
    expressionValue ->
      foldMap locatedExprRecordUpdates (expressionChildren expressionValue)

localBindsRecordUpdates :: HsLocalBindsLR Ghc.GhcPs Ghc.GhcPs -> [RecordUpdate]
localBindsRecordUpdates = \case
  HsValBinds _ valueBinds ->
    valueBindsRecordUpdates valueBinds
  _ ->
    []

valueBindsRecordUpdates :: HsValBindsLR Ghc.GhcPs Ghc.GhcPs -> [RecordUpdate]
valueBindsRecordUpdates = \case
  ValBinds _ binds _ ->
    foldMap (bindRecordUpdates . unLoc) (toList binds)
  _ ->
    []

locatedStmtRecordUpdates :: Ghc.ExprLStmt Ghc.GhcPs -> [RecordUpdate]
locatedStmtRecordUpdates locatedStmt =
  case unLoc locatedStmt of
    LastStmt _ body _ _ ->
      locatedExprRecordUpdates body
    BindStmt _ _ body ->
      locatedExprRecordUpdates body
    BodyStmt _ body _ _ ->
      locatedExprRecordUpdates body
    LetStmt _ localBinds ->
      localBindsRecordUpdates localBinds
    RecStmt {Ghc.recS_stmts = statements} ->
      foldMap locatedStmtRecordUpdates (unLoc statements)
    _ ->
      []

matchGroupSelectorApplications :: MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [SelectorApplication]
matchGroupSelectorApplications matches =
  foldMap locatedMatchSelectorApplications (unLoc (mg_alts matches))

locatedMatchSelectorApplications :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [SelectorApplication]
locatedMatchSelectorApplications locatedMatch =
  grhssSelectorApplications (m_grhss (unLoc locatedMatch))

grhssSelectorApplications :: GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [SelectorApplication]
grhssSelectorApplications grhss =
  localBindsSelectorApplications (grhssLocalBinds grhss)
    <> foldMap locatedGrhsSelectorApplications (grhssGRHSs grhss)

locatedGrhsSelectorApplications :: Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> [SelectorApplication]
locatedGrhsSelectorApplications locatedGrhs =
  case unLoc locatedGrhs of
    GRHS _ _ body ->
      locatedExprSelectorApplications body

locatedExprSelectorApplications :: Ghc.LHsExpr Ghc.GhcPs -> [SelectorApplication]
locatedExprSelectorApplications locatedExpr =
  case unLoc locatedExpr of
    HsLam _ _ matches ->
      matchGroupSelectorApplications matches
    HsCase _ scrutinee matches ->
      locatedExprSelectorApplications scrutinee <> matchGroupSelectorApplications matches
    HsLet _ localBinds body ->
      localBindsSelectorApplications localBinds <> locatedExprSelectorApplications body
    HsDo _ _ statements ->
      foldMap locatedStmtSelectorApplications (unLoc statements)
    HsApp _ functionExpr argumentExpr
      | Just selectorName <- locatedExprVarName functionExpr ->
          maybeToList
            ( SelectorApplication selectorName
                <$> sourceRegionFromSrcSpan (getLocA argumentExpr)
                <*> sourceRegionFromSrcSpan (getLocA locatedExpr)
            )
            <> foldMap locatedExprSelectorApplications (expressionChildren (unLoc locatedExpr))
    RecordUpd {rupd_expr = recordExpr, rupd_flds = fields} ->
      locatedExprSelectorApplications recordExpr
        <> foldMap (locatedExprSelectorApplications . hfbRHS . unLoc) (recUpdFieldBinds fields)
    expressionValue ->
      foldMap locatedExprSelectorApplications (expressionChildren expressionValue)

localBindsSelectorApplications :: HsLocalBindsLR Ghc.GhcPs Ghc.GhcPs -> [SelectorApplication]
localBindsSelectorApplications = \case
  HsValBinds _ valueBinds ->
    valueBindsSelectorApplications valueBinds
  _ ->
    []

valueBindsSelectorApplications :: HsValBindsLR Ghc.GhcPs Ghc.GhcPs -> [SelectorApplication]
valueBindsSelectorApplications = \case
  ValBinds _ binds _ ->
    foldMap (bindSelectorApplications . unLoc) (toList binds)
  _ ->
    []

locatedStmtSelectorApplications :: Ghc.ExprLStmt Ghc.GhcPs -> [SelectorApplication]
locatedStmtSelectorApplications locatedStmt =
  case unLoc locatedStmt of
    LastStmt _ body _ _ ->
      locatedExprSelectorApplications body
    BindStmt _ _ body ->
      locatedExprSelectorApplications body
    BodyStmt _ body _ _ ->
      locatedExprSelectorApplications body
    LetStmt _ localBinds ->
      localBindsSelectorApplications localBinds
    RecStmt {Ghc.recS_stmts = statements} ->
      foldMap locatedStmtSelectorApplications (unLoc statements)
    _ ->
      []

recordFieldNames :: Ghc.HsRecordBinds Ghc.GhcPs -> Set String
recordFieldNames fields =
  Set.fromList (mapMaybe recordExpressionFieldName (rec_flds fields))

recordConstructionFields :: Ghc.HsRecordBinds Ghc.GhcPs -> [RecordConstructionField]
recordConstructionFields fields =
  mapMaybe recordConstructionField (rec_flds fields)

recordConstructionField :: Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Maybe RecordConstructionField
recordConstructionField locatedField = do
  fieldRegion <- sourceRegionFromSrcSpan (getLocA locatedField)
  fieldName <- recordExpressionFieldName locatedField
  pure
    RecordConstructionField
      { rcfName = fieldName,
        rcfRegion = fieldRegion,
        rcfValue = recordFieldValue (hfbRHS (unLoc locatedField))
      }

recordFieldValue :: Ghc.LHsExpr Ghc.GhcPs -> RecordFieldValue
recordFieldValue locatedExpr =
  case locatedExprVarName locatedExpr of
    Just binderName ->
      RecordFieldDirect binderName
    Nothing ->
      case locatedExprProjection locatedExpr of
        Just (projectionName, binderName) ->
          RecordFieldProjection projectionName binderName
        Nothing ->
          RecordFieldOther (recordExpressionShape locatedExpr)

locatedExprProjection :: Ghc.LHsExpr Ghc.GhcPs -> Maybe (String, String)
locatedExprProjection locatedExpr =
  case sourceApplicationSpine locatedExpr of
    (functionExpr, [argumentExpr]) ->
      (,) <$> locatedExprRenderedName functionExpr <*> locatedExprVarName argumentExpr
    _ ->
      Nothing

recordExpressionShape :: Ghc.LHsExpr Ghc.GhcPs -> RecordExpressionShape
recordExpressionShape locatedExpr =
  case stripLocatedParens locatedExpr of
    HsVar {} ->
      RecordShapeVar
    HsApp {} ->
      let (functionExpr, arguments) = sourceApplicationSpine locatedExpr
       in RecordShapeApplication (locatedExprVarName functionExpr) (length arguments)
    RecordCon {rcon_con = constructorName, rcon_flds = fields} ->
      RecordShapeRecordConstruction
        (occNameString (rdrNameOcc (unLoc constructorName)))
        (recordFieldNames fields)
    HsLet {} ->
      RecordShapeLet
    HsLam {} ->
      RecordShapeLambda
    HsCase {} ->
      RecordShapeCase
    HsDo {} ->
      RecordShapeDo
    _ ->
      RecordShapeOther

sourceApplicationSpine :: Ghc.LHsExpr Ghc.GhcPs -> (Ghc.LHsExpr Ghc.GhcPs, [Ghc.LHsExpr Ghc.GhcPs])
sourceApplicationSpine locatedExpr =
  case stripLocatedParens locatedExpr of
    HsApp _ functionExpr argumentExpr ->
      let (headExpr, arguments) = sourceApplicationSpine functionExpr
       in (headExpr, arguments <> [argumentExpr])
    _ ->
      (locatedExpr, [])

stripLocatedParens :: Ghc.LHsExpr Ghc.GhcPs -> HsExpr Ghc.GhcPs
stripLocatedParens locatedExpr =
  case unLoc locatedExpr of
    HsPar _ innerExpr ->
      stripLocatedParens innerExpr
    expressionValue ->
      expressionValue

recUpdFieldBinds :: Ghc.LHsRecUpdFields Ghc.GhcPs -> [Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)]
recUpdFieldBinds fields =
  case fields of
    Ghc.RegularRecUpdFields {Ghc.recUpdFields = fieldBinds} ->
      fieldBinds
    Ghc.OverloadedRecUpdFields {} ->
      []

recordExpressionFieldName :: Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Maybe String
recordExpressionFieldName locatedField =
  case unLoc (hfbLHS (unLoc locatedField)) of
    FieldOcc {foLabel = labelValue} ->
      Just (occNameString (rdrNameOcc (unLoc labelValue)))

locatedExprVarName :: Ghc.LHsExpr Ghc.GhcPs -> Maybe String
locatedExprVarName locatedExpr =
  case unLoc locatedExpr of
    HsVar _ variableName ->
      Just (occNameString (rdrNameOcc (unLoc variableName)))
    HsPar _ innerExpr ->
      locatedExprVarName innerExpr
    _ ->
      Nothing

locatedExprRenderedName :: Ghc.LHsExpr Ghc.GhcPs -> Maybe String
locatedExprRenderedName locatedExpr =
  case unLoc locatedExpr of
    HsVar _ variableName ->
      Just (renderRdrName (unLoc variableName))
    HsPar _ innerExpr ->
      locatedExprRenderedName innerExpr
    _ ->
      Nothing

expressionChildren :: HsExpr Ghc.GhcPs -> [Ghc.LHsExpr Ghc.GhcPs]
expressionChildren = \case
  HsApp _ functionExpr argumentExpr ->
    [functionExpr, argumentExpr]
  HsAppType _ bodyExpr _ ->
    [bodyExpr]
  HsPar _ innerExpr ->
    [innerExpr]
  SectionL _ exprValue operatorValue ->
    [exprValue, operatorValue]
  SectionR _ operatorValue exprValue ->
    [operatorValue, exprValue]
  OpApp _ leftExpr operatorExpr rightExpr ->
    [leftExpr, operatorExpr, rightExpr]
  NegApp _ innerExpr _ ->
    [innerExpr]
  HsIf _ conditionExpr thenExpr elseExpr ->
    [conditionExpr, thenExpr, elseExpr]
  ExplicitList _ exprValues ->
    exprValues
  ExplicitTuple _ tupleArguments _ ->
    mapMaybe tupleArgumentExpr tupleArguments
  RecordCon {rcon_flds = fields} ->
    fmap (hfbRHS . unLoc) (rec_flds fields)
  ExprWithTySig _ bodyExpr _ ->
    [bodyExpr]
  _ ->
    []

tupleArgumentExpr :: HsTupArg Ghc.GhcPs -> Maybe (Ghc.LHsExpr Ghc.GhcPs)
tupleArgumentExpr = \case
  Present _ exprValue ->
    Just exprValue
  Missing _ ->
    Nothing

sourceRegionText :: String -> SourceRegion -> Either NebulaError String
sourceRegionText source region = do
  startOffset <- sourceOffset source (srStartLine region) (srStartCol region)
  endOffset <- sourceOffset source (srEndLine region) (srEndCol region)
  if startOffset <= endOffset
    then Right (take (endOffset - startOffset) (drop startOffset source))
    else Left (NebulaSpliceError ("source region ends before it starts: " <> show region))

sourceOffset :: String -> Int -> Int -> Either NebulaError Int
sourceOffset source lineNumber columnNumber =
  maybe
    (Left (NebulaSpliceError ("source position outside the source: line " <> show lineNumber <> ", column " <> show columnNumber)))
    Right
    boundedOffset
  where
    lineStarts =
      scanl (\startOffset lineText -> startOffset + length lineText + 1) 0 (lines source)
    boundedOffset = do
      lineStart <- listToMaybeDrop (lineNumber - 1) lineStarts
      let offset = lineStart + columnNumber - 1
      if lineNumber >= 1 && columnNumber >= 1 && offset <= length source
        then Just offset
        else Nothing

listToMaybeDrop :: Int -> [a] -> Maybe a
listToMaybeDrop count values =
  case drop count values of
    [] ->
      Nothing
    value : _ ->
      Just value

wholeLineRegion :: SourceRegion -> SourceRegion
wholeLineRegion region =
  SourceRegion
    { srStartLine = srStartLine region,
      srStartCol = 1,
      srEndLine = srEndLine region + 1,
      srEndCol = 1
    }

sourceRegionFromSrcSpan :: SrcSpan -> Maybe SourceRegion
sourceRegionFromSrcSpan = \case
  RealSrcSpan realSrcSpan _ ->
    Just
      SourceRegion
        { srStartLine = srcSpanStartLine realSrcSpan,
          srStartCol = srcSpanStartCol realSrcSpan,
          srEndLine = srcSpanEndLine realSrcSpan,
          srEndCol = srcSpanEndCol realSrcSpan
        }
  UnhelpfulSpan _ ->
    Nothing
