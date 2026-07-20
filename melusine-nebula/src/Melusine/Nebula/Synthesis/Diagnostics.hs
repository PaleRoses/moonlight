{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Synthesis.Diagnostics
  ( structuralDiagnosticRejections,
  )
where

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import Melusine.Nebula.Discovery.Choose (CandidateSite (..), CandidateSiteKind (..))
import Melusine.Nebula.Source.Ast qualified as SourceAst
import Melusine.Nebula.Synthesis.Types (CandidateRejection (..), RecordOwnershipFinding (..), RecordOwnershipKind (..), RejectedCandidate (..), candidateSiteLabel)
import Moonlight.Core (binderIdKey)
import Moonlight.EGraph.Introspection.Core.HsExpr (BinderAnn (..), HsExprF (..), HsPatF (..), HsVarRef (..), NormalizedFieldLabel (..))
import Moonlight.Pale.Ghc.Expr (renderRdrName)
import Data.Fix (Fix (..))

type ProjectionAccess :: Type
newtype ProjectionAccess = ProjectionAccess
  { projectionAccessName :: String
  }
  deriving stock (Eq, Ord, Show)

type ProjectionVectorSignature :: Type
newtype ProjectionVectorSignature = ProjectionVectorSignature
  { projectionVectorAccesses :: [ProjectionAccess]
  }
  deriving stock (Eq, Ord, Show)

type FoldSkeletonSignature :: Type
data FoldSkeletonSignature
  = RowClearingFoldSkeleton
  deriving stock (Eq, Ord, Show)

type FoldSkeletonAtom :: Type
data FoldSkeletonAtom
  = FoldMAtom
  | DenseIndexAtom
  | IsZeroAtom
  | AddScaledDenseRowAtom
  deriving stock (Eq, Ord, Show)

type LetRowsProtocolSignature :: Type
data LetRowsProtocolSignature
  = LetRowsProtocolSignature
  deriving stock (Eq, Ord, Show)

type PatternBindRhsProtocolSignature :: Type
data PatternBindRhsProtocolSignature
  = PatternBindRhsProtocolSignature
  deriving stock (Eq, Ord, Show)

type KeyedRowAlignmentProtocolSignature :: Type
data KeyedRowAlignmentProtocolSignature
  = KeyedRowAlignmentProtocolSignature
  deriving stock (Eq, Ord, Show)

type ChildUnifierArity :: Type
data ChildUnifierArity
  = ChildUnifierUnary
  | ChildUnifierBinary
  | ChildUnifierTernary
  deriving stock (Eq, Ord, Show)

type ArityChildUnifierProtocolSignature :: Type
newtype ArityChildUnifierProtocolSignature = ArityChildUnifierProtocolSignature
  { arityChildUnifierArities :: Set.Set ChildUnifierArity
  }
  deriving stock (Eq, Ord, Show)

type RecordConstructionSkeletonSignature :: Type
data RecordConstructionSkeletonSignature = RecordConstructionSkeletonSignature
  { rcssConstructorName :: !String,
    rcssFieldShapes :: ![(String, RecordConstructionFieldShape)]
  }
  deriving stock (Eq, Ord, Show)

type RecordConstructionFieldShape :: Type
data RecordConstructionFieldShape
  = RecordConstructionDirectField
  | RecordConstructionProjectionField !String
  | RecordConstructionOtherField !SourceAst.RecordExpressionShape
  deriving stock (Eq, Ord, Show)

type SourceRecordOwnershipKey :: Type
data SourceRecordOwnershipKey = SourceRecordOwnershipKey
  { srokConstructorName :: !String,
    srokDerivedField :: !String,
    srokOwnerField :: !String,
    srokProjectionName :: !String
  }
  deriving stock (Eq, Ord, Show)

type SourceRecordOwnershipRow :: Type
data SourceRecordOwnershipRow = SourceRecordOwnershipRow
  { srorBindingName :: !String,
    srorOwnerBinder :: !String
  }
  deriving stock (Eq, Ord, Show)

type ScopedRegionExtractionProtocolSignature :: Type
data ScopedRegionExtractionProtocolSignature
  = ScopedRegionExtractionProtocolSignature
  deriving stock (Eq, Ord, Show)

type EitherValidationSignature :: Type
data EitherValidationSignature
  = EitherCarrierContinuationSignature
  deriving stock (Eq, Ord, Show)

type FiniteValidationSignature :: Type
data FiniteValidationSignature
  = FiniteValidationSignature
  deriving stock (Eq, Ord, Show)

type ThresholdRefinementSignature :: Type
newtype ThresholdRefinementSignature = ThresholdRefinementSignature
  { thresholdRefinementComparison :: ValidationComparison
  }
  deriving stock (Eq, Ord, Show)

type ValidationComparison :: Type
data ValidationComparison
  = RejectNonPositive
  | RejectNegative
  deriving stock (Eq, Ord, Show)

structuralDiagnosticRejections :: FilePath -> String -> [CandidateSite] -> [RejectedCandidate]
structuralDiagnosticRejections path source sites =
  projectionVectorDiagnosticRejections sites
    <> foldSkeletonDiagnosticRejections sites
    <> letRowsProtocolDiagnosticRejections sites
    <> patternBindRhsProtocolDiagnosticRejections sites
    <> keyedRowAlignmentProtocolDiagnosticRejections sites
    <> arityChildUnifierProtocolDiagnosticRejections sites
    <> recordOwnershipDiagnosticRejections path source sites
    <> recordConstructionSkeletonDiagnosticRejections path source sites
    <> redundantPatternClassCanonicalizationDiagnosticRejections sites
    <> scopedRegionExtractionProtocolDiagnosticRejections sites
    <> finiteValidationDiagnosticRejections sites
    <> thresholdRefinementDiagnosticRejections sites
    <> eitherValidationDiagnosticRejections sites

projectionVectorDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
projectionVectorDiagnosticRejections sites =
  diagnosticRejections
    RejectedProjectionVectorDiagnostic
    projectionVectorWeight
    [ (signature, site)
    | site <- sites,
      signature <- projectionVectorSignatures (csSourceTerm site)
    ]

foldSkeletonDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
foldSkeletonDiagnosticRejections sites =
  diagnosticRejections
    RejectedFoldSkeletonDiagnostic
    foldSkeletonWeight
    [ (signature, site)
    | site <- sites,
      signature <- foldSkeletonSignatures (csSourceTerm site)
    ]

letRowsProtocolDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
letRowsProtocolDiagnosticRejections sites =
  diagnosticRejections
    RejectedLetRowsProtocolDiagnostic
    letRowsProtocolWeight
    [ (signature, site)
    | site <- sites,
      signature <- letRowsProtocolSignatures (csSourceTerm site)
    ]

patternBindRhsProtocolDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
patternBindRhsProtocolDiagnosticRejections sites =
  diagnosticRejections
    RejectedPatternBindRhsProtocolDiagnostic
    patternBindRhsProtocolWeight
    [ (signature, site)
    | site <- sites,
      signature <- patternBindRhsProtocolSignatures (csSourceTerm site)
    ]

keyedRowAlignmentProtocolDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
keyedRowAlignmentProtocolDiagnosticRejections sites =
  diagnosticRejections
    RejectedKeyedRowAlignmentProtocolDiagnostic
    keyedRowAlignmentProtocolWeight
    [ (signature, site)
    | site <- sites,
      signature <- keyedRowAlignmentProtocolSignatures (csSourceTerm site)
    ]

arityChildUnifierProtocolDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
arityChildUnifierProtocolDiagnosticRejections sites =
  diagnosticRejections
    RejectedArityChildUnifierProtocolDiagnostic
    arityChildUnifierProtocolWeight
    [ (signature, site)
    | site <- sites,
      signature <- arityChildUnifierProtocolSignatures (csSourceTerm site)
    ]

recordOwnershipDiagnosticRejections :: FilePath -> String -> [CandidateSite] -> [RejectedCandidate]
recordOwnershipDiagnosticRejections path source sites =
  recordProjectionOwnershipDiagnosticRejections sites
    <> sourceRecordOwnershipDiagnosticRejections path source sites

recordProjectionOwnershipDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
recordProjectionOwnershipDiagnosticRejections sites =
  [ RejectedCandidate
      { rejSites = [candidateSiteLabel site],
        rejReason = RejectedRecordOwnershipDiagnostic findings,
        rejEstimatedWin = length findings,
        rejRealizedWin = Nothing
      }
  | site <- sites,
    csSiteKind site == BindingCandidateSite,
    let findings = recordProjectionOwnershipFindings (csSourceTerm site),
    not (null findings)
  ]

recordConstructionSkeletonDiagnosticRejections :: FilePath -> String -> [CandidateSite] -> [RejectedCandidate]
recordConstructionSkeletonDiagnosticRejections path source sites =
  diagnosticRejections
    RejectedRecordConstructionSkeletonDiagnostic
    recordConstructionSkeletonWeight
    [ (recordConstructionSkeletonSignature construction, site)
    | (bindingName, construction) <- sourceRecordConstructions path source,
      site <- maybe [] pure (Map.lookup bindingName bindingSites)
    ]
  where
    bindingSites =
      bindingSiteByName sites

redundantPatternClassCanonicalizationDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
redundantPatternClassCanonicalizationDiagnosticRejections sites =
  [ RejectedCandidate
      { rejSites = [candidateSiteLabel site],
        rejReason = RejectedRedundantPatternClassCanonicalizationDiagnostic,
        rejEstimatedWin = 1,
        rejRealizedWin = Nothing
      }
  | site <- sites,
    csSiteKind site == BindingCandidateSite,
    csBindingName site /= "resolvePatternClass",
    redundantPatternClassCanonicalizationTerm (csSourceTerm site)
  ]

scopedRegionExtractionProtocolDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
scopedRegionExtractionProtocolDiagnosticRejections sites =
  diagnosticRejections
    RejectedScopedRegionExtractionProtocolDiagnostic
    scopedRegionExtractionProtocolWeight
    [ (signature, site)
    | site <- sites,
      csSiteKind site == BindingCandidateSite,
      signature <- scopedRegionExtractionProtocolSignatures (csSourceTerm site)
    ]

finiteValidationDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
finiteValidationDiagnosticRejections sites =
  diagnosticRejections
    RejectedFiniteValidationDiagnostic
    finiteValidationWeight
    [ (signature, site)
    | site <- sites,
      signature <- finiteValidationSignatures (csSourceTerm site)
    ]

thresholdRefinementDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
thresholdRefinementDiagnosticRejections sites =
  diagnosticRejections
    RejectedThresholdRefinementDiagnostic
    thresholdRefinementWeight
    [ (signature, site)
    | site <- sites,
      signature <- thresholdRefinementSignatures (csSourceTerm site)
    ]

eitherValidationDiagnosticRejections :: [CandidateSite] -> [RejectedCandidate]
eitherValidationDiagnosticRejections sites =
  diagnosticRejections
    RejectedEitherValidationDiagnostic
    eitherValidationWeight
    [ (signature, site)
    | site <- sites,
      signature <- eitherValidationSignatures (csSourceTerm site)
    ]

diagnosticRejections ::
  Ord signature =>
  CandidateRejection ->
  (signature -> [CandidateSite] -> Int) ->
  [(signature, CandidateSite)] ->
  [RejectedCandidate]
diagnosticRejections rejection weight rows =
  mapMaybe groupRejection (Map.toList groupedRows)
  where
    groupedRows =
      Map.fromListWith mergeDiagnosticSites
        [ (signature, Map.singleton (csBindingName site) site)
        | (signature, site) <- rows
        ]
    mergeDiagnosticSites =
      Map.unionWith preferBindingSite
    groupRejection (signature, siteByBinding) =
      let sites = Map.elems siteByBinding
       in case sites of
            _ : _ : _ ->
              Just
                RejectedCandidate
                  { rejSites = fmap candidateSiteLabel sites,
                    rejReason = rejection,
                    rejEstimatedWin = weight signature sites,
                    rejRealizedWin = Nothing
                  }
            _ ->
              Nothing

preferBindingSite :: CandidateSite -> CandidateSite -> CandidateSite
preferBindingSite leftSite rightSite =
  case (csSiteKind leftSite, csSiteKind rightSite) of
    (BindingCandidateSite, RegionCandidateSite) ->
      leftSite
    (RegionCandidateSite, BindingCandidateSite) ->
      rightSite
    _ ->
      min leftSite rightSite

projectionVectorWeight :: ProjectionVectorSignature -> [CandidateSite] -> Int
projectionVectorWeight signature sites =
  length (projectionVectorAccesses signature) * length sites

foldSkeletonWeight :: FoldSkeletonSignature -> [CandidateSite] -> Int
foldSkeletonWeight _ sites =
  4 * length sites

letRowsProtocolWeight :: LetRowsProtocolSignature -> [CandidateSite] -> Int
letRowsProtocolWeight _ sites =
  8 * length sites

patternBindRhsProtocolWeight :: PatternBindRhsProtocolSignature -> [CandidateSite] -> Int
patternBindRhsProtocolWeight _ sites =
  5 * length sites

keyedRowAlignmentProtocolWeight :: KeyedRowAlignmentProtocolSignature -> [CandidateSite] -> Int
keyedRowAlignmentProtocolWeight _ sites =
  7 * length sites

arityChildUnifierProtocolWeight :: ArityChildUnifierProtocolSignature -> [CandidateSite] -> Int
arityChildUnifierProtocolWeight signature sites =
  Set.size (arityChildUnifierArities signature) * 4 * length sites

recordConstructionSkeletonWeight :: RecordConstructionSkeletonSignature -> [CandidateSite] -> Int
recordConstructionSkeletonWeight signature sites =
  length (rcssFieldShapes signature) * length sites

scopedRegionExtractionProtocolWeight :: ScopedRegionExtractionProtocolSignature -> [CandidateSite] -> Int
scopedRegionExtractionProtocolWeight _ sites =
  5 * length sites

finiteValidationWeight :: FiniteValidationSignature -> [CandidateSite] -> Int
finiteValidationWeight _ sites =
  3 * length sites

thresholdRefinementWeight :: ThresholdRefinementSignature -> [CandidateSite] -> Int
thresholdRefinementWeight _ sites =
  4 * length sites

eitherValidationWeight :: EitherValidationSignature -> [CandidateSite] -> Int
eitherValidationWeight signature sites =
  case signature of
    EitherCarrierContinuationSignature ->
      4 * length sites

projectionVectorSignatures :: Fix HsExprF -> [ProjectionVectorSignature]
projectionVectorSignatures termValue@(Fix nodeValue) =
  maybe [] pure (projectionVectorSignatureAt termValue)
    <> foldMap projectionVectorSignatures nodeValue

projectionVectorSignatureAt :: Fix HsExprF -> Maybe ProjectionVectorSignature
projectionVectorSignatureAt termValue =
  let (_, spineArgs) =
        applicationSpine termValue
      projectionSuffix =
        projectionArgumentSuffix spineArgs
   in case sameBinderProjectionAccesses projectionSuffix of
        Just accesses@(_ : _ : _) ->
          Just (ProjectionVectorSignature accesses)
        _ ->
          Nothing

type ProjectionArgument :: Type
data ProjectionArgument = ProjectionArgument
  { paBinderKey :: !Int,
    paAccess :: !ProjectionAccess
  }

projectionArgumentSuffix :: [Fix HsExprF] -> [ProjectionArgument]
projectionArgumentSuffix =
  snd . foldr collectProjection (True, [])
  where
    collectProjection termValue (stillSuffix, collected)
      | stillSuffix =
          case projectionArgument termValue of
            Just projectionValue ->
              (True, projectionValue : collected)
            Nothing ->
              (False, collected)
      | otherwise =
          (False, collected)

projectionArgument :: Fix HsExprF -> Maybe ProjectionArgument
projectionArgument termValue =
  case applicationSpine (stripParens termValue) of
    (Fix (VarF (GlobalName accessorName)), [Fix (VarF (LocalName binderAnn))]) ->
      Just
        ProjectionArgument
          { paBinderKey = binderIdKey (baId binderAnn),
            paAccess = ProjectionAccess (occNameString (rdrNameOcc accessorName))
          }
    _ ->
      Nothing

stripParens :: Fix HsExprF -> Fix HsExprF
stripParens = \case
  Fix (ParF innerTerm) ->
    stripParens innerTerm
  termValue ->
    termValue

sameBinderProjectionAccesses :: [ProjectionArgument] -> Maybe [ProjectionAccess]
sameBinderProjectionAccesses projections =
  let binderKeys =
        Set.fromList (fmap paBinderKey projections)
   in case Set.toList binderKeys of
        [_] ->
          Just (fmap paAccess projections)
        _ ->
          Nothing

foldSkeletonSignatures :: Fix HsExprF -> [FoldSkeletonSignature]
foldSkeletonSignatures termValue =
  [ RowClearingFoldSkeleton
  | rowClearingFoldAtoms (termAtoms termValue)
  ]

rowClearingFoldAtoms :: Set.Set FoldSkeletonAtom -> Bool
rowClearingFoldAtoms atoms =
  all (`Set.member` atoms) [FoldMAtom, DenseIndexAtom, IsZeroAtom, AddScaledDenseRowAtom]

termAtoms :: Fix HsExprF -> Set.Set FoldSkeletonAtom
termAtoms (Fix nodeValue) =
  nodeAtoms nodeValue <> foldMap termAtoms nodeValue

nodeAtoms :: HsExprF child -> Set.Set FoldSkeletonAtom
nodeAtoms = \case
  VarF (GlobalName nameValue) ->
    maybe Set.empty Set.singleton (foldSkeletonAtom (occNameString (rdrNameOcc nameValue)))
  _ ->
    Set.empty

foldSkeletonAtom :: String -> Maybe FoldSkeletonAtom
foldSkeletonAtom = \case
  "foldM" ->
    Just FoldMAtom
  "denseIndex" ->
    Just DenseIndexAtom
  "isZero" ->
    Just IsZeroAtom
  "addScaledDenseRow" ->
    Just AddScaledDenseRowAtom
  _ ->
    Nothing

letRowsProtocolSignatures :: Fix HsExprF -> [LetRowsProtocolSignature]
letRowsProtocolSignatures termValue =
  [ LetRowsProtocolSignature
  | letRowsProtocolNames `Set.isSubsetOf` termGlobalNames termValue,
    letRowsRecursionConstructors `Set.isSubsetOf` termConstructors termValue
  ]

letRowsProtocolNames :: Set.Set String
letRowsProtocolNames =
  Set.fromList
    [ "matchBindingRowPatterns",
      "lmRecursion",
      "alphaUnifyBindingRhsRows"
    ]

letRowsRecursionConstructors :: Set.Set String
letRowsRecursionConstructors =
  Set.fromList
    [ "NonRecursiveBinds",
      "RecursiveOpaqueBinds"
    ]

patternBindRhsProtocolSignatures :: Fix HsExprF -> [PatternBindRhsProtocolSignature]
patternBindRhsProtocolSignatures termValue =
  [ PatternBindRhsProtocolSignature
  | patternBindRhsProtocolNames `Set.isSubsetOf` globalNames,
    not (Set.null (patternBindRhsCarriers `Set.intersection` constructorNames))
  ]
  where
    globalNames =
      termGlobalNames termValue
    constructorNames =
      termConstructors termValue

patternBindRhsProtocolNames :: Set.Set String
patternBindRhsProtocolNames =
  Set.fromList
    [ "matchPattern",
      "alphaUnifyTerm"
    ]

patternBindRhsCarriers :: Set.Set String
patternBindRhsCarriers =
  Set.fromList
    [ "BindStmtF",
      "GuardPatF"
    ]

keyedRowAlignmentProtocolSignatures :: Fix HsExprF -> [KeyedRowAlignmentProtocolSignature]
keyedRowAlignmentProtocolSignatures termValue =
  [ KeyedRowAlignmentProtocolSignature
  | keyedRowAlignmentProtocolNames `Set.isSubsetOf` globalNames,
    Set.member "AlphaMismatch" constructorNames,
    not (Set.null (keyedRowAlignmentPayloadUnifiers `Set.intersection` globalNames))
  ]
  where
    globalNames =
      termGlobalNames termValue
    constructorNames =
      termConstructors termValue

keyedRowAlignmentProtocolNames :: Set.Set String
keyedRowAlignmentProtocolNames =
  Set.fromList
    [ "sortOn",
      "zipEqual",
      "mapAccumM",
      "swapAccumResult"
    ]

keyedRowAlignmentPayloadUnifiers :: Set.Set String
keyedRowAlignmentPayloadUnifiers =
  Set.fromList
    [ "alphaUnifyTerm",
      "matchPattern"
    ]

arityChildUnifierProtocolSignatures :: Fix HsExprF -> [ArityChildUnifierProtocolSignature]
arityChildUnifierProtocolSignatures termValue =
  [ ArityChildUnifierProtocolSignature arities
  | Set.member "alphaUnifyTerm" globalNames,
    Set.member "AlphaMatched" constructorNames,
    Set.size arities >= 2
  ]
  where
    globalNames =
      termGlobalNames termValue
    constructorNames =
      termConstructors termValue
    arities =
      childUnifierArities constructorNames

childUnifierArities :: Set.Set String -> Set.Set ChildUnifierArity
childUnifierArities constructorNames =
  Set.fromList
    [ arity
    | (arity, names) <- childUnifierArityConstructors,
      not (Set.null (names `Set.intersection` constructorNames))
    ]

childUnifierArityConstructors :: [(ChildUnifierArity, Set.Set String)]
childUnifierArityConstructors =
  [ ( ChildUnifierUnary,
      Set.fromList
        [ "ParF",
          "NegF",
          "ExprWithTySigF",
          "AppTypeF",
          "ArithSeqFrom"
        ]
    ),
    ( ChildUnifierBinary,
      Set.fromList
        [ "AppF",
          "SectionLF",
          "SectionRF",
          "ArithSeqFromThen",
          "ArithSeqFromTo"
        ]
    ),
    ( ChildUnifierTernary,
      Set.fromList
        [ "OpAppF",
          "IfF",
          "ArithSeqFromThenTo"
        ]
    )
  ]

sourceRecordConstructions :: FilePath -> String -> [(String, SourceAst.RecordConstruction)]
sourceRecordConstructions path source =
  either
    (const [])
    ( concatMap
        ( \binding ->
            fmap
              ((,) (SourceAst.lbName binding))
              (SourceAst.bindingRecordConstructions binding)
        )
    )
    (SourceAst.locatedValueBindings path source)

bindingSiteByName :: [CandidateSite] -> Map.Map String CandidateSite
bindingSiteByName sites =
  Map.fromList
    [ (csBindingName site, site)
    | site <- sites,
      csSiteKind site == BindingCandidateSite
    ]

recordConstructionSkeletonSignature :: SourceAst.RecordConstruction -> RecordConstructionSkeletonSignature
recordConstructionSkeletonSignature construction =
  RecordConstructionSkeletonSignature
    { rcssConstructorName = SourceAst.rcConstructorName construction,
      rcssFieldShapes =
        sortOn
          fst
          [ (SourceAst.rcfName field, recordConstructionFieldShape (SourceAst.rcfValue field))
          | field <- SourceAst.rcFieldRows construction
          ]
    }

recordConstructionFieldShape :: SourceAst.RecordFieldValue -> RecordConstructionFieldShape
recordConstructionFieldShape = \case
  SourceAst.RecordFieldDirect {} ->
    RecordConstructionDirectField
  SourceAst.RecordFieldProjection projectionName _ ->
    RecordConstructionProjectionField projectionName
  SourceAst.RecordFieldOther shape ->
    RecordConstructionOtherField shape

sourceRecordOwnershipDiagnosticRejections :: FilePath -> String -> [CandidateSite] -> [RejectedCandidate]
sourceRecordOwnershipDiagnosticRejections path source sites =
  either
    (const [])
    ( \bindings ->
        if null (foldMap SourceAst.bindingRecordUpdates bindings)
          then sourceRecordOwnershipRejectionsFromBindings (bindingSiteByName sites) bindings
          else []
    )
    (SourceAst.locatedValueBindings path source)

sourceRecordOwnershipRejectionsFromBindings :: Map.Map String CandidateSite -> [SourceAst.LocatedBinding] -> [RejectedCandidate]
sourceRecordOwnershipRejectionsFromBindings sites bindings =
  [ RejectedCandidate
      { rejSites = fmap candidateSiteLabel labelSites,
        rejReason = RejectedRecordOwnershipDiagnostic [sourceRecordOwnershipFinding key rows],
        rejEstimatedWin = 1,
        rejRealizedWin = Nothing
      }
  | (key, rows) <- Map.toList (sourceRecordOwnershipRowsByKey bindings),
    sourceRecordOwnershipKeyCoversConstructor key constructorSites,
    let labelSites = mapMaybe ((`Map.lookup` sites) . srorBindingName) rows,
    not (null labelSites)
  ]
  where
    constructionSites =
      [ construction
      | binding <- bindings,
        construction <- SourceAst.bindingRecordConstructions binding
      ]
    constructorSites =
      Map.fromListWith
        (<>)
        [ (SourceAst.rcConstructorName construction, [construction])
        | construction <- constructionSites
        ]

sourceRecordOwnershipRowsByKey :: [SourceAst.LocatedBinding] -> Map.Map SourceRecordOwnershipKey [SourceRecordOwnershipRow]
sourceRecordOwnershipRowsByKey bindings =
  Map.fromListWith
    (<>)
    [ (key, [row])
    | binding <- bindings,
      construction <- SourceAst.bindingRecordConstructions binding,
      (key, row) <- sourceRecordOwnershipRowsInConstruction (SourceAst.lbName binding) construction
    ]

sourceRecordOwnershipRowsInConstruction :: String -> SourceAst.RecordConstruction -> [(SourceRecordOwnershipKey, SourceRecordOwnershipRow)]
sourceRecordOwnershipRowsInConstruction bindingName construction =
  [ ( SourceRecordOwnershipKey
        { srokConstructorName = SourceAst.rcConstructorName construction,
          srokDerivedField = SourceAst.rcfName derivedField,
          srokOwnerField = SourceAst.rcfName ownerField,
          srokProjectionName = projectionName
        },
      SourceRecordOwnershipRow
        { srorBindingName = bindingName,
          srorOwnerBinder = ownerBinder
        }
    )
  | derivedField <- SourceAst.rcFieldRows construction,
    SourceAst.RecordFieldProjection projectionName ownerBinder <- [SourceAst.rcfValue derivedField],
    ownerField <- SourceAst.rcFieldRows construction,
    SourceAst.RecordFieldDirect directBinder <- [SourceAst.rcfValue ownerField],
    directBinder == ownerBinder,
    SourceAst.rcfName ownerField /= SourceAst.rcfName derivedField
  ]

sourceRecordOwnershipKeyCoversConstructor ::
  SourceRecordOwnershipKey ->
  Map.Map String [SourceAst.RecordConstruction] ->
  Bool
sourceRecordOwnershipKeyCoversConstructor key constructorSites =
  case Map.lookup (srokConstructorName key) constructorSites of
    Just (_ : _) ->
      all (constructionSatisfiesOwnershipKey key) (Map.findWithDefault [] (srokConstructorName key) constructorSites)
    _ ->
      False

constructionSatisfiesOwnershipKey :: SourceRecordOwnershipKey -> SourceAst.RecordConstruction -> Bool
constructionSatisfiesOwnershipKey key construction =
  case constructionOwnershipRow key construction of
    Nothing ->
      False
    Just ownershipRow ->
      any
        ( \ownerField ->
            SourceAst.rcfName ownerField == srokOwnerField key
              && SourceAst.rcfValue ownerField == SourceAst.RecordFieldDirect (srorOwnerBinder ownershipRow)
        )
        fields
  where
    fields =
      SourceAst.rcFieldRows construction

constructionOwnershipRow :: SourceRecordOwnershipKey -> SourceAst.RecordConstruction -> Maybe SourceRecordOwnershipRow
constructionOwnershipRow key construction =
  case
    [ SourceRecordOwnershipRow
        { srorBindingName = "",
          srorOwnerBinder = ownerBinder
        }
    | derivedField <- SourceAst.rcFieldRows construction,
      SourceAst.rcfName derivedField == srokDerivedField key,
      SourceAst.RecordFieldProjection projectionName ownerBinder <- [SourceAst.rcfValue derivedField],
      projectionName == srokProjectionName key
    ] of
    [] ->
      Nothing
    row : _ ->
      Just row

sourceRecordOwnershipFinding :: SourceRecordOwnershipKey -> [SourceRecordOwnershipRow] -> RecordOwnershipFinding
sourceRecordOwnershipFinding key rows =
  RecordOwnershipFinding
    { rofConstructorName = srokConstructorName key,
      rofDerivedField = srokDerivedField key,
      rofProjectionName = srokProjectionName key,
      rofOwnerField = srokOwnerField key,
      rofOwnerBinder =
        case rows of
          row : _ ->
            srorOwnerBinder row
          [] ->
            "",
      rofKind = StaleDerivedField
    }

redundantPatternClassCanonicalizationTerm :: Fix HsExprF -> Bool
redundantPatternClassCanonicalizationTerm termValue =
  Set.member "resolvePatternClass" globalNames
    && Set.member "canonicalizeClassId" globalNames
  where
    globalNames =
      termGlobalNames termValue

scopedRegionExtractionProtocolSignatures :: Fix HsExprF -> [ScopedRegionExtractionProtocolSignature]
scopedRegionExtractionProtocolSignatures termValue =
  [ ScopedRegionExtractionProtocolSignature
  | scopedRegionExtractionCoreNames `Set.isSubsetOf` globalNames
  ]
  where
    globalNames =
      termGlobalNames termValue

scopedRegionExtractionCoreNames :: Set.Set String
scopedRegionExtractionCoreNames =
  Set.fromList
    [ "sizeExtractAt",
      "ActualScope",
      "seOccScope",
      "erTerm"
    ]

recordProjectionOwnershipFindings :: Fix HsExprF -> [RecordOwnershipFinding]
recordProjectionOwnershipFindings termValue@(Fix nodeValue) =
  recordProjectionOwnershipAt termValue
    <> foldMap recordProjectionOwnershipFindings nodeValue

recordProjectionOwnershipAt :: Fix HsExprF -> [RecordOwnershipFinding]
recordProjectionOwnershipAt (Fix (RecordConF constructorTerm fieldRows)) =
  let ownedFieldsByBinder =
        Map.fromListWith
          (<>)
          [ (binderKey, [(fieldName, binderName)])
          | (fieldLabel, fieldTerm) <- fieldRows,
            let fieldName = nflSelector fieldLabel,
            (binderKey, binderName) <- maybe [] pure (directLocalBinder fieldTerm)
          ]
      projectionFindings =
        [ recordProjectionFinding constructorName duplicateField projectionName ownerField binderName
        | (fieldLabel, fieldTerm) <- fieldRows,
          let duplicateField = nflSelector fieldLabel,
          (binderKey, binderName, projectionName) <- maybe [] pure (projectedLocalBinder fieldTerm),
          (ownerField, _) <- Map.findWithDefault [] binderKey ownedFieldsByBinder,
          ownerField /= duplicateField
        ]
   in projectionFindings
  where
    constructorName =
      maybe "record" id (termHeadGlobalName constructorTerm)
recordProjectionOwnershipAt _ =
  []

recordProjectionFinding :: String -> String -> String -> String -> String -> RecordOwnershipFinding
recordProjectionFinding constructorName duplicateField projectionName ownerField binderName =
  RecordOwnershipFinding
    { rofConstructorName = constructorName,
      rofDerivedField = duplicateField,
      rofProjectionName = projectionName,
      rofOwnerField = ownerField,
      rofOwnerBinder = binderName,
      rofKind = ProjectionOwnedCachedField
    }

directLocalBinder :: Fix HsExprF -> Maybe (Int, String)
directLocalBinder termValue =
  case stripParens termValue of
    Fix (VarF (LocalName binderAnn)) ->
      Just (binderIdKey (baId binderAnn), occNameString (rdrNameOcc (baName binderAnn)))
    _ ->
      Nothing

projectedLocalBinder :: Fix HsExprF -> Maybe (Int, String, String)
projectedLocalBinder termValue =
  case applicationSpine (stripParens termValue) of
    (Fix (VarF (GlobalName projectionName)), [argumentTerm]) -> do
      (binderKey, binderName) <- directLocalBinder argumentTerm
      Just (binderKey, binderName, renderRdrName projectionName)
    _ ->
      Nothing

termHeadGlobalName :: Fix HsExprF -> Maybe String
termHeadGlobalName termValue =
  case stripParens termValue of
    Fix (VarF (GlobalName nameValue)) ->
      Just (occNameString (rdrNameOcc nameValue))
    _ ->
      Nothing

eitherValidationSignatures :: Fix HsExprF -> [EitherValidationSignature]
eitherValidationSignatures termValue =
  [ EitherCarrierContinuationSignature
  | termHasEitherCarrier termValue,
    termHasCase termValue || Set.member ">>=" (termGlobalNames termValue)
  ]

finiteValidationSignatures :: Fix HsExprF -> [FiniteValidationSignature]
finiteValidationSignatures termValue =
  [ FiniteValidationSignature
  | termHasEitherCarrier termValue,
    Set.member "isNaN" globalNames,
    Set.member "isInfinite" globalNames
  ]
  where
    globalNames =
      termGlobalNames termValue

thresholdRefinementSignatures :: Fix HsExprF -> [ThresholdRefinementSignature]
thresholdRefinementSignatures termValue =
  [ ThresholdRefinementSignature comparison
  | termHasEitherCarrier termValue,
    comparison <- Set.toList (termValidationComparisons termValue)
  ]

termHasEitherCarrier :: Fix HsExprF -> Bool
termHasEitherCarrier termValue =
  Set.member "Left" constructorNames
    && Set.member "Right" constructorNames
  where
    constructorNames =
      termConstructors termValue

termGlobalNames :: Fix HsExprF -> Set.Set String
termGlobalNames (Fix nodeValue) =
  nodeGlobalNames nodeValue <> foldMap termGlobalNames nodeValue

nodeGlobalNames :: HsExprF child -> Set.Set String
nodeGlobalNames = \case
  VarF (GlobalName nameValue) ->
    Set.singleton (occNameString (rdrNameOcc nameValue))
  _ ->
    Set.empty

termConstructors :: Fix HsExprF -> Set.Set String
termConstructors termValue =
  termPatternConstructors termValue <> termGlobalNames termValue

termPatternConstructors :: Fix HsExprF -> Set.Set String
termPatternConstructors (Fix nodeValue) =
  nodePatternConstructors nodeValue <> foldMap termPatternConstructors nodeValue

nodePatternConstructors :: HsExprF child -> Set.Set String
nodePatternConstructors = \case
  LetF _ bindingValues _ ->
    foldMap (patConstructorNames . fst) bindingValues
  CaseF _ alternatives ->
    foldMap (patConstructorNames . fst) alternatives
  ClausesF clauses ->
    foldMap (foldMap patConstructorNames . fst) clauses
  _ ->
    Set.empty

patConstructorNames :: HsPatF -> Set.Set String
patConstructorNames = \case
  PConP constructorName subPatterns ->
    Set.insert (occNameString (rdrNameOcc constructorName)) (foldMap patConstructorNames subPatterns)
  PTupleP subPatterns ->
    foldMap patConstructorNames subPatterns
  PListP subPatterns ->
    foldMap patConstructorNames subPatterns
  PAsP _ subPattern ->
    patConstructorNames subPattern
  PBangP subPattern ->
    patConstructorNames subPattern
  PLazyP subPattern ->
    patConstructorNames subPattern
  PParP subPattern ->
    patConstructorNames subPattern
  PRecP constructorName fieldPatterns ->
    Set.insert (occNameString (rdrNameOcc constructorName)) (foldMap (patConstructorNames . snd) fieldPatterns)
  _ ->
    Set.empty

termValidationComparisons :: Fix HsExprF -> Set.Set ValidationComparison
termValidationComparisons (Fix nodeValue) =
  nodeValidationComparisons nodeValue <> foldMap termValidationComparisons nodeValue

nodeValidationComparisons :: HsExprF (Fix HsExprF) -> Set.Set ValidationComparison
nodeValidationComparisons = \case
  OpAppF _ operatorTerm _ ->
    maybe Set.empty Set.singleton (validationComparisonOperator operatorTerm)
  _ ->
    Set.empty

validationComparisonOperator :: Fix HsExprF -> Maybe ValidationComparison
validationComparisonOperator operatorTerm =
  case stripParens operatorTerm of
    Fix (VarF (GlobalName operatorName)) ->
      validationComparisonName (occNameString (rdrNameOcc operatorName))
    _ ->
      Nothing

validationComparisonName :: String -> Maybe ValidationComparison
validationComparisonName = \case
  "<=" ->
    Just RejectNonPositive
  "<" ->
    Just RejectNegative
  _ ->
    Nothing

termHasCase :: Fix HsExprF -> Bool
termHasCase (Fix nodeValue) =
  nodeHasCase nodeValue || any termHasCase (toList nodeValue)

nodeHasCase :: HsExprF child -> Bool
nodeHasCase = \case
  CaseF {} ->
    True
  _ ->
    False

applicationSpine :: Fix HsExprF -> (Fix HsExprF, [Fix HsExprF])
applicationSpine =
  go []
  where
    go arguments = \case
      Fix (AppF functionTerm argumentTerm) ->
        go (argumentTerm : arguments) functionTerm
      headTerm ->
        (headTerm, arguments)
