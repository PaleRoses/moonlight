{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell derivation for front DSL signatures.
-- Owns generation of tag types, 'HTraversable', 'RewriteSignature',
-- 'ZipMatch', and smart constructors from GADT signatures.
-- Contracts: only GADT data/newtype declarations of shape @sig "Sort" r@ are
-- accepted; recursive fields become children and payload fields stay in tags.
module Moonlight.Rewrite.Signature.TH
  ( deriveRewriteSignature,
  )
where

import Language.Haskell.TH
import Data.Char (toLower)
import Data.Foldable qualified as Foldable
import Data.Word (Word64)
import Moonlight.Core (ZipMatch (..))
import Moonlight.Rewrite.DSL.Signature (K (..), Node (..), SortWitness (..))

deriveRewriteSignature :: Name -> Q [Dec]
deriveRewriteSignature signatureName = do
  info <- reify signatureName
  case info of
    TyConI declaration ->
      deriveFromDeclaration signatureName declaration
    _ ->
      fail "deriveRewriteSignature expects a data type"

deriveFromDeclaration :: Name -> Dec -> Q [Dec]
deriveFromDeclaration signatureName declaration =
  case declaration of
    DataD _ _ _ _ constructors _ ->
      deriveFromConstructors signatureName constructors
    NewtypeD _ _ _ _ constructor _ ->
      deriveFromConstructors signatureName [constructor]
    _ ->
      fail "deriveRewriteSignature supports data/newtype declarations only"

deriveFromConstructors :: Name -> [Con] -> Q [Dec]
deriveFromConstructors signatureName constructors = do
  constructorInfos <-
    traverse (constructorInfo signatureName) constructors
  tagDeclaration <-
    tagDataDeclaration signatureName constructorInfos
  traversableInstance <-
    htraversableInstance signatureName constructorInfos
  signatureInstance <-
    rewriteSignatureInstance signatureName constructorInfos
  zipMatchInstance <-
    nodeZipMatchInstance signatureName constructorInfos
  smartConstructors <-
    foldMapM (smartConstructor signatureName) constructorInfos
  pure (tagDeclaration : traversableInstance : signatureInstance : zipMatchInstance : smartConstructors)

data ConstructorInfo = ConstructorInfo
  { ciName :: !Name,
    ciTagName :: !Name,
    ciResultSort :: !String,
    ciFields :: ![FieldInfo]
  }

data FieldInfo = FieldInfo
  { fiType :: !Type,
    fiRole :: !FieldRole
  }

data FieldRole
  = ChildField !String
  | PayloadField

constructorInfo :: Name -> Con -> Q ConstructorInfo
constructorInfo signatureName constructor =
  case constructor of
    GadtC [constructorName] fields resultType ->
      constructorInfoFromFields signatureName constructorName fields resultType
    RecGadtC [constructorName] fields resultType ->
      constructorInfoFromFields signatureName constructorName (fmap recFieldToBangType fields) resultType
    ForallC binders context nestedConstructor
      | not (null context) ->
          fail "deriveRewriteSignature does not support constrained constructors"
      | otherwise -> do
          resultType <- constructorResultType nestedConstructor
          (_resultSort, recursionName) <-
            resultSortAndRecursion signatureName resultType
          if all ((== recursionName) . boundTypeVariableName) binders
            then constructorInfo signatureName nestedConstructor
            else fail "deriveRewriteSignature does not support existential constructors"
    _ ->
      fail "deriveRewriteSignature requires GADT constructors"

constructorResultType :: Con -> Q Type
constructorResultType = \case
  GadtC _constructorNames _fields resultType ->
    pure resultType
  RecGadtC _constructorNames _fields resultType ->
    pure resultType
  ForallC _binders _context nestedConstructor ->
    constructorResultType nestedConstructor
  _ ->
    fail "deriveRewriteSignature requires GADT constructors"

boundTypeVariableName :: TyVarBndr flag -> Name
boundTypeVariableName = \case
  PlainTV name _flag ->
    name
  KindedTV name _flag _kind ->
    name

constructorInfoFromFields :: Name -> Name -> [BangType] -> Type -> Q ConstructorInfo
constructorInfoFromFields signatureName constructorName fields resultType = do
  (resultSort, recursionName) <-
    resultSortAndRecursion signatureName resultType
  fieldInfos <-
    traverse (fieldInfo recursionName) fields
  pure
    ConstructorInfo
      { ciName = constructorName,
        ciTagName = mkName (nameBase signatureName <> nameBase constructorName <> "Tag"),
        ciResultSort = resultSort,
        ciFields = fieldInfos
      }

recFieldToBangType :: VarBangType -> BangType
recFieldToBangType (_, fieldBang, fieldType) =
  (fieldBang, fieldType)

resultSortAndRecursion :: Name -> Type -> Q (String, Name)
resultSortAndRecursion signatureName resultType =
  case resultType of
    AppT (AppT (ConT observedSignature) (LitT (StrTyLit sortName))) (VarT recursionName)
      | observedSignature == signatureName ->
          pure (sortName, recursionName)
    _ ->
      fail "deriveRewriteSignature requires result type sig \"Sort\" r"

fieldInfo :: Name -> BangType -> Q FieldInfo
fieldInfo recursionName (_, fieldType) =
  pure
    FieldInfo
      { fiType = fieldType,
        fiRole = fieldRole recursionName fieldType
      }

fieldRole :: Name -> Type -> FieldRole
fieldRole recursionName fieldType =
  case fieldType of
    AppT (VarT observedRecursion) (LitT (StrTyLit sortName))
      | observedRecursion == recursionName ->
          ChildField sortName
    _ ->
      PayloadField

tagDataDeclaration :: Name -> [ConstructorInfo] -> Q Dec
tagDataDeclaration signatureName constructorInfos = do
  tagConstructors <-
    traverse tagConstructor constructorInfos
  pure
    ( DataD
        []
        (mkName (nameBase signatureName <> "Tag"))
        []
        Nothing
        tagConstructors
        [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord, ConT ''Show]]
    )

tagConstructor :: ConstructorInfo -> Q Con
tagConstructor constructorInfoValue =
  pure
    ( NormalC
        (ciTagName constructorInfoValue)
        [ (Bang NoSourceUnpackedness NoSourceStrictness, fiType fieldInfoValue)
          | fieldInfoValue <- ciFields constructorInfoValue,
            case fiRole fieldInfoValue of
              PayloadField -> True
              ChildField _ -> False
        ]
    )

htraversableInstance :: Name -> [ConstructorInfo] -> Q Dec
htraversableInstance signatureName constructorInfos = do
  traverseWithSortFunction <-
    funD (mkName "htraverseWithSort") (fmap htraverseWithSortClause constructorInfos)
  instanceD
    (pure [])
    (conT (mkName "HTraversable") `appT` conT signatureName)
    [pure traverseWithSortFunction]

htraverseWithSortClause :: ConstructorInfo -> Q Clause
htraverseWithSortClause constructorInfoValue = do
  transformName <- newName "transform"
  fieldNames <- fieldPatternNames constructorInfoValue
  let transformPattern =
        if any fieldIsChild (ciFields constructorInfoValue)
          then varP transformName
          else wildP
  clause
    [transformPattern, conP (ciName constructorInfoValue) (fmap varP fieldNames)]
    (normalB (applicativeConstructorExpressionWithSort transformName constructorInfoValue fieldNames))
    []
  where
    fieldIsChild fieldInfoValue =
      case fiRole fieldInfoValue of
        ChildField _ -> True
        PayloadField -> False

fieldPatternNames :: ConstructorInfo -> Q [Name]
fieldPatternNames constructorInfoValue =
  traverse
    (\index -> newName ("field" <> show index))
    [0 .. length (ciFields constructorInfoValue) - 1]

applicativeConstructorExpressionWithSort :: Name -> ConstructorInfo -> [Name] -> Q Exp
applicativeConstructorExpressionWithSort transformName constructorInfoValue fieldNames =
  Foldable.foldl'
    applyArgument
    (appE (varE 'pure) (conE (ciName constructorInfoValue)))
    (zip (ciFields constructorInfoValue) fieldNames)
  where
    applyArgument accumulated (fieldInfoValue, fieldName) =
      infixE
        (Just accumulated)
        (varE '(<*>))
        ( Just
            ( case fiRole fieldInfoValue of
                ChildField sortName ->
                  appE
                    (appE (varE transformName) (sortWitnessExpression sortName))
                    (varE fieldName)
                PayloadField ->
                  appE (varE 'pure) (varE fieldName)
            )
        )

rewriteSignatureInstance :: Name -> [ConstructorInfo] -> Q Dec
rewriteSignatureInstance signatureName constructorInfos = do
  nodeTagFamily <-
    tySynInstD
      ( tySynEqn
          Nothing
          (conT (mkName "NodeTag") `appT` conT signatureName)
          (conT (mkName (nameBase signatureName <> "Tag")))
      )
  nodeTagFunction <-
    funD (mkName "nodeTag") (fmap nodeTagClause constructorInfos)
  digestFunction <-
    funD (mkName "nodeTagDigest") [nodeTagDigestClause]
  resultSortFunction <-
    funD (mkName "nodeResultSort") (fmap nodeResultSortClause constructorInfos)
  instanceD
    (pure [])
    (conT (mkName "RewriteSignature") `appT` conT signatureName)
    [pure nodeTagFamily, pure nodeTagFunction, pure digestFunction, pure resultSortFunction]

nodeZipMatchInstance :: Name -> [ConstructorInfo] -> Q Dec
nodeZipMatchInstance signatureName constructorInfos = do
  zipMatchFunction <-
    funD 'zipMatch (fmap nodeZipMatchClause constructorInfos <> [nodeZipMatchFallbackClause])
  instanceD
    (pure [])
    (conT ''ZipMatch `appT` (conT ''Node `appT` conT signatureName))
    [pure zipMatchFunction]

nodeZipMatchClause :: ConstructorInfo -> Q Clause
nodeZipMatchClause constructorInfoValue = do
  leftFieldNames <- prefixedFieldPatternNames "left" constructorInfoValue
  rightFieldNames <- prefixedFieldPatternNames "right" constructorInfoValue
  clause
    [ conP 'Node [conP (ciName constructorInfoValue) (fmap varP leftFieldNames)],
      conP 'Node [conP (ciName constructorInfoValue) (fmap varP rightFieldNames)]
    ]
    ( guardedB
        [ (,)
            <$> normalG (payloadFieldsEqualExpression constructorInfoValue leftFieldNames rightFieldNames)
            <*> appE (conE 'Just) (appE (conE 'Node) (zippedConstructorExpression constructorInfoValue leftFieldNames rightFieldNames)),
          (,) <$> normalG [| True |] <*> conE 'Nothing
        ]
    )
    []

nodeZipMatchFallbackClause :: Q Clause
nodeZipMatchFallbackClause =
  clause
    [wildP, wildP]
    (normalB (conE 'Nothing))
    []

prefixedFieldPatternNames :: String -> ConstructorInfo -> Q [Name]
prefixedFieldPatternNames prefix constructorInfoValue =
  traverse
    (\index -> newName (prefix <> "Field" <> show index))
    [0 .. length (ciFields constructorInfoValue) - 1]

payloadFieldsEqualExpression :: ConstructorInfo -> [Name] -> [Name] -> Q Exp
payloadFieldsEqualExpression constructorInfoValue leftFieldNames rightFieldNames =
  case payloadComparisons of
    [] ->
      [| True |]
    comparison : comparisons ->
      foldr
        (\current rest -> [| $current && $rest |])
        comparison
        comparisons
  where
    payloadComparisons =
      [ [| $(varE leftFieldName) == $(varE rightFieldName) |]
        | (fieldInfoValue, leftFieldName, rightFieldName) <- zip3 (ciFields constructorInfoValue) leftFieldNames rightFieldNames,
          case fiRole fieldInfoValue of
            PayloadField -> True
            ChildField _ -> False
      ]

zippedConstructorExpression :: ConstructorInfo -> [Name] -> [Name] -> Q Exp
zippedConstructorExpression constructorInfoValue leftFieldNames rightFieldNames =
  foldl
    appE
    (conE (ciName constructorInfoValue))
    [ zippedFieldExpression fieldInfoValue leftFieldName rightFieldName
      | (fieldInfoValue, leftFieldName, rightFieldName) <- zip3 (ciFields constructorInfoValue) leftFieldNames rightFieldNames
    ]

zippedFieldExpression :: FieldInfo -> Name -> Name -> Q Exp
zippedFieldExpression fieldInfoValue leftFieldName rightFieldName =
  case fiRole fieldInfoValue of
    PayloadField ->
      varE leftFieldName
    ChildField _ ->
      [| K (unK $(varE leftFieldName), unK $(varE rightFieldName)) |]

nodeTagClause :: ConstructorInfo -> Q Clause
nodeTagClause constructorInfoValue = do
  fieldNames <- fieldPatternNames constructorInfoValue
  clause
    [ conP
        (ciName constructorInfoValue)
        ( zipWith
            tagFieldPattern
            (ciFields constructorInfoValue)
            fieldNames
        )
    ]
    ( normalB
        ( foldl
            appE
            (conE (ciTagName constructorInfoValue))
            [ varE fieldName
              | (fieldInfoValue, fieldName) <- zip (ciFields constructorInfoValue) fieldNames,
                case fiRole fieldInfoValue of
                  PayloadField -> True
                  ChildField _ -> False
            ]
        )
    )
    []
  where
    tagFieldPattern :: FieldInfo -> Name -> PatQ
    tagFieldPattern fieldInfoValue fieldName =
      case fiRole fieldInfoValue of
        PayloadField -> varP fieldName
        ChildField _ -> wildP

nodeResultSortClause :: ConstructorInfo -> Q Clause
nodeResultSortClause constructorInfoValue = do
  clause
    [ conP
        (ciName constructorInfoValue)
        (replicate (length (ciFields constructorInfoValue)) wildP)
    ]
    (normalB (sortWitnessExpression (ciResultSort constructorInfoValue)))
    []

sortWitnessExpression :: String -> Q Exp
sortWitnessExpression sortName =
  sigE
    (conE 'SortWitness)
    (conT ''SortWitness `appT` litT (strTyLit sortName))

nodeTagDigestClause :: Q Clause
nodeTagDigestClause = do
  tagName <- newName "tagValue"
  clause
    [wildP, varP tagName]
    (normalB (stableShowDigestExpression (varE tagName)))
    []

stableShowDigestExpression :: Q Exp -> Q Exp
stableShowDigestExpression shownValue =
  [|
    Foldable.foldl'
      (\acc character -> acc * 16777619 + fromIntegral (fromEnum character))
      (2166136261 :: Word64)
      (show $shownValue)
    |]

smartConstructor :: Name -> ConstructorInfo -> Q [Dec]
smartConstructor signatureName constructorInfoValue = do
  let functionName = mkName (lowerInitial (nameBase (ciName constructorInfoValue)))
  argumentNames <- fieldPatternNames constructorInfoValue
  signature <- smartConstructorSignature signatureName functionName constructorInfoValue
  implementation <-
    funD
      functionName
      [ clause
          (fmap varP argumentNames)
          ( normalB
              ( appE
                  (varE (mkName "node"))
                  ( foldl
                      appE
                      (conE (ciName constructorInfoValue))
                      (fmap varE argumentNames)
                  )
              )
          )
          []
      ]
  pure [signature, implementation]

smartConstructorSignature :: Name -> Name -> ConstructorInfo -> Q Dec
smartConstructorSignature signatureName functionName constructorInfoValue =
  sigD functionName
    ( foldr
        arrowTOf
        (termType signatureName (ciResultSort constructorInfoValue))
        (fmap (smartFieldType signatureName) (ciFields constructorInfoValue))
    )

smartFieldType :: Name -> FieldInfo -> Q Type
smartFieldType signatureName fieldInfoValue =
  case fiRole fieldInfoValue of
    ChildField sortName ->
      termType signatureName sortName
    PayloadField ->
      pure (fiType fieldInfoValue)

termType :: Name -> String -> Q Type
termType signatureName sortName =
  conT (mkName "Term") `appT` conT signatureName `appT` litT (strTyLit sortName)

arrowTOf :: Q Type -> Q Type -> Q Type
arrowTOf leftType rightType =
  arrowT `appT` leftType `appT` rightType

foldMapM :: Monoid m => (a -> Q m) -> [a] -> Q m
foldMapM transform =
  fmap Foldable.fold . traverse transform

lowerInitial :: String -> String
lowerInitial value =
  case value of
    [] ->
      []
    firstCharacter : remainingCharacters ->
      toLower firstCharacter : remainingCharacters
