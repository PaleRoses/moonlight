{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Pale.Ghc.Expr.Syntax
  ( HsVarRef (..),
    BinderAnn (..),
    HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    HsPatF (..),
    patBinders,
    traversePatBinders,
    LetMode (..),
    LetRecursion (..),
    LetProvenance (..),
    NormalizedLit (..),
    normalizeHsLit,
    NormalizedOverLit (..),
    normalizeHsOverLit,
    NormalizedFieldLabel (..),
    normalizeFieldLabel,
    NormalizedTypeText (..),
    NormalizedArithSeq (..),
    SourceRegion (..),
    HsExprF (..),
    HsStmtF (..),
    HsGuardStmtF (..),
    GuardedAltF (..),
    ScopedExpr (..),
    eraseScopedExpr,
    SpannedExpr (..),
    eraseSpannedExpr,
    HsExprTag (..),
    TagSignature (..),
    tagSignatureFromTag,
    tagSignatureMember,
    matchesHsExprPattern,
    matchHsExprPatternSubstitution,
    instantiateHsExprPattern,
  )
where

import Control.Monad (foldM)
import Data.Bits (bit, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Data.FastString (unpackFS)
import GHC.Hs (GhcPs, HsLit (..), HsOverLit (..), OverLitVal (..))
import GHC.Types.FieldLabel
  ( DuplicateRecordFields (..),
    FieldLabel,
    FieldSelectors (..),
    flHasDuplicateRecordFields,
    flHasFieldSelector,
    flSelector,
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText (FractionalExponentBase (..), FractionalLit (..), IntegralLit (..))
import Moonlight.Core (BinderId, HasConstructorTag (..), Pattern (..), ZipMatch (..), zipSameNodeShape)
import Moonlight.Core qualified as EGraph
import Moonlight.Pale.Ghc.Expr.Opaque (HsOpaqueTag (..), HsPatOpaqueTag (..))
import Moonlight.Pale.Ghc.Expr.Scope (FreeScopeSummary, ScopeId)

type HsVarRef :: Type
data HsVarRef
  = GlobalName !RdrName
  | LocalName !BinderAnn
  deriving stock (Eq, Ord)

type BinderAnn :: Type
data BinderAnn = BinderAnn
  { baId :: !BinderId,
    baName :: !RdrName
  }
  deriving stock (Eq, Ord)

instance Show HsVarRef where
  show = \case
    GlobalName rdrName -> "GlobalName " <> occNameString (rdrNameOcc rdrName)
    LocalName binderAnn -> "LocalName " <> show binderAnn

instance Show BinderAnn where
  show binderAnn =
    "BinderAnn { baId = " <> show (baId binderAnn) <> ", baName = " <> occNameString (rdrNameOcc (baName binderAnn)) <> " }"

type HsPatF :: Type
data HsPatF
  = PVarP !BinderAnn
  | PWildP
  | PConP !RdrName ![HsPatF]
  | PTupleP ![HsPatF]
  | PListP ![HsPatF]
  | PLitP !NormalizedLit
  | POverLitP !NormalizedOverLit
  | PAsP !BinderAnn !HsPatF
  | PBangP !HsPatF
  | PLazyP !HsPatF
  | PParP !HsPatF
  | PRecP !RdrName ![(String, HsPatF)]
  | PLossyP !HsPatOpaqueTag ![BinderAnn]
  deriving stock (Eq, Ord)

instance Show HsPatF where
  show = \case
    PVarP binderAnn -> "PVarP (" <> show binderAnn <> ")"
    PWildP -> "PWildP"
    PConP conName subPatterns -> "PConP " <> occNameString (rdrNameOcc conName) <> " " <> show subPatterns
    PTupleP subPatterns -> "PTupleP " <> show subPatterns
    PListP subPatterns -> "PListP " <> show subPatterns
    PLitP literalValue -> "PLitP (" <> show literalValue <> ")"
    POverLitP literalValue -> "POverLitP (" <> show literalValue <> ")"
    PAsP binderAnn subPattern -> "PAsP (" <> show binderAnn <> ") (" <> show subPattern <> ")"
    PBangP subPattern -> "PBangP (" <> show subPattern <> ")"
    PLazyP subPattern -> "PLazyP (" <> show subPattern <> ")"
    PParP subPattern -> "PParP (" <> show subPattern <> ")"
    PRecP conName fieldPatterns -> "PRecP " <> occNameString (rdrNameOcc conName) <> " " <> show fieldPatterns
    PLossyP tagValue binderAnns -> "PLossyP " <> show tagValue <> " " <> show binderAnns

patBinders :: HsPatF -> [BinderAnn]
patBinders = \case
  PVarP binderAnn -> [binderAnn]
  PWildP -> []
  PConP _ subPatterns -> foldMap patBinders subPatterns
  PTupleP subPatterns -> foldMap patBinders subPatterns
  PListP subPatterns -> foldMap patBinders subPatterns
  PLitP _ -> []
  POverLitP _ -> []
  PAsP binderAnn subPattern -> binderAnn : patBinders subPattern
  PBangP subPattern -> patBinders subPattern
  PLazyP subPattern -> patBinders subPattern
  PParP subPattern -> patBinders subPattern
  PRecP _ fieldPatterns -> foldMap (patBinders . snd) fieldPatterns
  PLossyP _ binderAnns -> binderAnns

traversePatBinders :: Applicative f => (BinderAnn -> f BinderAnn) -> HsPatF -> f HsPatF
traversePatBinders onBinder = go
  where
    go = \case
      PVarP binderAnn -> PVarP <$> onBinder binderAnn
      PWildP -> pure PWildP
      PConP conName subPatterns -> PConP conName <$> traverse go subPatterns
      PTupleP subPatterns -> PTupleP <$> traverse go subPatterns
      PListP subPatterns -> PListP <$> traverse go subPatterns
      PLitP literalValue -> pure (PLitP literalValue)
      POverLitP literalValue -> pure (POverLitP literalValue)
      PAsP binderAnn subPattern -> PAsP <$> onBinder binderAnn <*> go subPattern
      PBangP subPattern -> PBangP <$> go subPattern
      PLazyP subPattern -> PLazyP <$> go subPattern
      PParP subPattern -> PParP <$> go subPattern
      PRecP conName fieldPatterns -> PRecP conName <$> traverse (traverse go) fieldPatterns
      PLossyP tagValue binderAnns -> PLossyP tagValue <$> traverse onBinder binderAnns

type LetRecursion :: Type
data LetRecursion
  = NonRecursiveBinds
  | RecursiveOpaqueBinds
  deriving stock (Eq, Ord, Show)

type LetProvenance :: Type
data LetProvenance
  = LetSyntax
  | WhereSyntax
  deriving stock (Eq, Ord, Show)

type LetMode :: Type
data LetMode = LetMode
  { lmRecursion :: !LetRecursion,
    lmProvenance :: !LetProvenance
  }
  deriving stock (Eq, Ord, Show)

type NormalizedLit :: Type
data NormalizedLit
  = NormalizedChar !Char
  | NormalizedCharPrim !Char
  | NormalizedString !String
  | NormalizedMultilineString !String
  | NormalizedStringPrim !ByteString
  | NormalizedInt !Integer
  | NormalizedIntPrim !Integer
  | NormalizedWordPrim !Integer
  | NormalizedInt8Prim !Integer
  | NormalizedInt16Prim !Integer
  | NormalizedInt32Prim !Integer
  | NormalizedInt64Prim !Integer
  | NormalizedWord8Prim !Integer
  | NormalizedWord16Prim !Integer
  | NormalizedWord32Prim !Integer
  | NormalizedWord64Prim !Integer
  | NormalizedFloatPrim !Rational
  | NormalizedDoublePrim !Rational
  deriving stock (Eq, Ord, Show)

type NormalizedOverLit :: Type
data NormalizedOverLit
  = NormalizedIntegralOverLit !Integer
  | NormalizedFractionalOverLit !Rational
  | NormalizedStringOverLit !String
  deriving stock (Eq, Ord, Show)

type NormalizedTypeText :: Type
newtype NormalizedTypeText = NormalizedTypeText
  { nttText :: String
  }
  deriving stock (Eq, Ord, Show)

type NormalizedFieldLabel :: Type
data NormalizedFieldLabel = NormalizedFieldLabel
  { nflSelector :: !String,
    nflAllowsDuplicateRecordFields :: !Bool,
    nflHasSelector :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type NormalizedArithSeq :: Type -> Type
data NormalizedArithSeq r
  = ArithSeqFrom !r
  | ArithSeqFromThen !r !r
  | ArithSeqFromTo !r !r
  | ArithSeqFromThenTo !r !r !r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type SourceRegion :: Type
data SourceRegion = SourceRegion
  { srStartLine :: !Int,
    srStartCol :: !Int,
    srEndLine :: !Int,
    srEndCol :: !Int
  }
  deriving stock (Eq, Ord, Show)

type HsExprF :: Type -> Type
data HsExprF r
  = VarF !HsVarRef
  | AppF !r !r
  | LamF !BinderAnn !r
  | LetF !LetMode ![(HsPatF, r)] !r
  | OpAppF !r !r !r
  | SectionLF !r !r
  | SectionRF !r !r
  | ParF !r
  | LitF !NormalizedLit
  | OverLitF !NormalizedOverLit
  | IfF !r !r !r
  | CaseF !r ![(HsPatF, r)]
  | DoF ![HsStmtF r]
  | NegF !r
  | ExplicitListF ![r]
  | ExplicitTupleF ![r]
  | RecordConF !r ![(NormalizedFieldLabel, r)]
  | RecordUpdF !r ![(NormalizedFieldLabel, r)]
  | ArithSeqF !(NormalizedArithSeq r)
  | GuardedF ![GuardedAltF r]
  | ClausesF ![([HsPatF], r)]
  | MultiIfF ![GuardedAltF r]
  | ExprWithTySigF !r !NormalizedTypeText
  | AppTypeF !r !NormalizedTypeText
  | OpaqueF !HsOpaqueTag
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type HsGuardStmtF :: Type -> Type
data HsGuardStmtF r
  = GuardBoolF !r
  | GuardPatF !HsPatF !r
  | GuardLetF !LetMode ![(HsPatF, r)]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type GuardedAltF :: Type -> Type
data GuardedAltF r = GuardedAltF
  { gaGuards :: ![HsGuardStmtF r],
    gaBody :: !r
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type HsStmtF :: Type -> Type
data HsStmtF r
  = BindStmtF !HsPatF !r
  | BodyStmtF !r
  | LetStmtF !LetMode ![(HsPatF, r)]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type ScopedExpr :: Type
data ScopedExpr = ScopedExpr
  { seOccScope :: !ScopeId,
    seFreeScopes :: !FreeScopeSummary,
    seNode :: !(HsExprF ScopedExpr)
  }
  deriving stock (Eq, Ord, Show)

type SpannedExpr :: Type
data SpannedExpr = SpannedExpr
  { sxRegion :: !(Maybe SourceRegion),
    sxNode :: !(HsExprF SpannedExpr)
  }
  deriving stock (Eq, Ord, Show)

type HsExprTag :: Type
data HsExprTag
  = VarTag
  | AppTag
  | LamTag
  | LetTag
  | OpAppTag
  | SectionLTag
  | SectionRTag
  | ParTag
  | LitTag
  | OverLitTag
  | IfTag
  | CaseTag
  | DoTag
  | NegTag
  | ExplicitListTag
  | ExplicitTupleTag
  | RecordConTag
  | RecordUpdTag
  | ArithSeqTag
  | GuardedTag
  | ClausesTag
  | MultiIfTag
  | ExprWithTySigTag
  | AppTypeTag
  | OpaqueTag
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TagSignature :: Type
newtype TagSignature = TagSignature Word64
  deriving stock (Eq, Ord, Show)

tagSignatureFromTag :: HsExprTag -> TagSignature
tagSignatureFromTag tag =
  TagSignature (bit (fromEnum tag))

tagSignatureMember :: HsExprTag -> TagSignature -> Bool
tagSignatureMember tag (TagSignature signature) =
  testBit signature (fromEnum tag)

instance Semigroup TagSignature where
  TagSignature left <> TagSignature right =
    TagSignature (left .|. right)

instance Monoid TagSignature where
  mempty =
    TagSignature 0

instance HasConstructorTag HsExprF where
  type ConstructorTag HsExprF = HsExprTag

  constructorTag = \case
    VarF {} -> VarTag
    AppF {} -> AppTag
    LamF {} -> LamTag
    LetF {} -> LetTag
    OpAppF {} -> OpAppTag
    SectionLF {} -> SectionLTag
    SectionRF {} -> SectionRTag
    ParF {} -> ParTag
    LitF {} -> LitTag
    OverLitF {} -> OverLitTag
    IfF {} -> IfTag
    CaseF {} -> CaseTag
    DoF {} -> DoTag
    NegF {} -> NegTag
    ExplicitListF {} -> ExplicitListTag
    ExplicitTupleF {} -> ExplicitTupleTag
    RecordConF {} -> RecordConTag
    RecordUpdF {} -> RecordUpdTag
    ArithSeqF {} -> ArithSeqTag
    GuardedF {} -> GuardedTag
    ClausesF {} -> ClausesTag
    MultiIfF {} -> MultiIfTag
    ExprWithTySigF {} -> ExprWithTySigTag
    AppTypeF {} -> AppTypeTag
    OpaqueF {} -> OpaqueTag

instance ZipMatch HsExprF where
  zipMatch =
    zipSameNodeShape

normalizeHsLit :: HsLit GhcPs -> NormalizedLit
normalizeHsLit = \case
  HsChar _ value -> NormalizedChar value
  HsCharPrim _ value -> NormalizedCharPrim value
  HsString _ value -> NormalizedString (unpackFS value)
  HsMultilineString _ value -> NormalizedMultilineString (unpackFS value)
  HsStringPrim _ value -> NormalizedStringPrim value
  HsInt _ value -> NormalizedInt (normalizedIntegralValue value)
  HsIntPrim _ value -> NormalizedIntPrim value
  HsWordPrim _ value -> NormalizedWordPrim value
  HsInt8Prim _ value -> NormalizedInt8Prim value
  HsInt16Prim _ value -> NormalizedInt16Prim value
  HsInt32Prim _ value -> NormalizedInt32Prim value
  HsInt64Prim _ value -> NormalizedInt64Prim value
  HsWord8Prim _ value -> NormalizedWord8Prim value
  HsWord16Prim _ value -> NormalizedWord16Prim value
  HsWord32Prim _ value -> NormalizedWord32Prim value
  HsWord64Prim _ value -> NormalizedWord64Prim value
  HsFloatPrim _ value -> NormalizedFloatPrim (normalizedFractionalValue value)
  HsDoublePrim _ value -> NormalizedDoublePrim (normalizedFractionalValue value)

normalizeHsOverLit :: HsOverLit GhcPs -> NormalizedOverLit
normalizeHsOverLit = \case
  OverLit {ol_val = value} -> normalizeOverLitVal value

normalizeFieldLabel :: FieldLabel -> NormalizedFieldLabel
normalizeFieldLabel fieldLabelValue =
  NormalizedFieldLabel
    { nflSelector = occNameString (nameOccName (flSelector fieldLabelValue)),
      nflAllowsDuplicateRecordFields = duplicateFieldFlag (flHasDuplicateRecordFields fieldLabelValue),
      nflHasSelector = selectorFieldFlag (flHasFieldSelector fieldLabelValue)
    }

eraseScopedExpr :: ScopedExpr -> Pattern HsExprF
eraseScopedExpr scopedExpr =
  PatternNode (fmap eraseScopedExpr (seNode scopedExpr))

eraseSpannedExpr :: SpannedExpr -> Pattern HsExprF
eraseSpannedExpr spannedExpr =
  PatternNode (fmap eraseSpannedExpr (sxNode spannedExpr))

matchesHsExprPattern :: Pattern HsExprF -> Pattern HsExprF -> Bool
matchesHsExprPattern patternValue =
  maybe False (const True) . matchHsExprPatternSubstitution patternValue

normalizeOverLitVal :: OverLitVal -> NormalizedOverLit
normalizeOverLitVal = \case
  HsIntegral value -> NormalizedIntegralOverLit (normalizedIntegralValue value)
  HsFractional value -> NormalizedFractionalOverLit (normalizedFractionalValue value)
  HsIsString _ value -> NormalizedStringOverLit (unpackFS value)

normalizedIntegralValue :: IntegralLit -> Integer
normalizedIntegralValue (IL _ isNegative value) =
  if isNegative then negate value else value

normalizedFractionalValue :: FractionalLit -> Rational
normalizedFractionalValue fractionalLit =
  let magnitude = fl_signi fractionalLit * exponentFactor (fl_exp_base fractionalLit) (fl_exp fractionalLit)
   in if fl_neg fractionalLit then negate magnitude else magnitude

exponentFactor :: FractionalExponentBase -> Integer -> Rational
exponentFactor exponentBase exponentValue =
  let baseValue =
        case exponentBase of
          Base2 -> 2
          Base10 -> 10
   in if exponentValue >= 0
        then baseValue ^^ exponentValue
        else recip (baseValue ^^ negate exponentValue)

duplicateFieldFlag :: DuplicateRecordFields -> Bool
duplicateFieldFlag = \case
  DuplicateRecordFields -> True
  NoDuplicateRecordFields -> False

selectorFieldFlag :: FieldSelectors -> Bool
selectorFieldFlag = \case
  FieldSelectors -> True
  NoFieldSelectors -> False

matchHsExprPatternSubstitution ::
  Pattern HsExprF ->
  Pattern HsExprF ->
  Maybe (Map EGraph.PatternVar (Pattern HsExprF))
matchHsExprPatternSubstitution =
  matchPatternWithSubstitution Map.empty

matchPatternWithSubstitution ::
  Map EGraph.PatternVar (Pattern HsExprF) ->
  Pattern HsExprF ->
  Pattern HsExprF ->
  Maybe (Map EGraph.PatternVar (Pattern HsExprF))
matchPatternWithSubstitution substitution patternValue termValue =
  case (patternValue, termValue) of
    (PatternVar patternVar, _) ->
      matchPatternVariable substitution patternVar termValue
    (PatternNode patternNode, PatternNode termNode) ->
      matchExprNode substitution patternNode termNode
    _ ->
      Nothing

matchPatternVariable ::
  Map EGraph.PatternVar (Pattern HsExprF) ->
  EGraph.PatternVar ->
  Pattern HsExprF ->
  Maybe (Map EGraph.PatternVar (Pattern HsExprF))
matchPatternVariable substitution patternVar termValue =
  case Map.lookup patternVar substitution of
    Nothing ->
      Just (Map.insert patternVar termValue substitution)
    Just existingValue
      | existingValue == termValue ->
          Just substitution
      | otherwise ->
          Nothing

matchExprNode ::
  Map EGraph.PatternVar (Pattern HsExprF) ->
  HsExprF (Pattern HsExprF) ->
  HsExprF (Pattern HsExprF) ->
  Maybe (Map EGraph.PatternVar (Pattern HsExprF))
matchExprNode substitution leftNode rightNode =
  zipSameNodeShape leftNode rightNode
    >>= foldM matchZippedChild substitution

matchZippedChild ::
  Map EGraph.PatternVar (Pattern HsExprF) ->
  (Pattern HsExprF, Pattern HsExprF) ->
  Maybe (Map EGraph.PatternVar (Pattern HsExprF))
matchZippedChild substitution (leftChild, rightChild) =
  matchPatternWithSubstitution substitution leftChild rightChild

instantiateHsExprPattern :: Map EGraph.PatternVar (Pattern HsExprF) -> Pattern HsExprF -> Pattern HsExprF
instantiateHsExprPattern substitution =
  \case
    PatternVar patternVar ->
      Map.findWithDefault (PatternVar patternVar) patternVar substitution
    PatternNode patternNode ->
      PatternNode (fmap (instantiateHsExprPattern substitution) patternNode)
