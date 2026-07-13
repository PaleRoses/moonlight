{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceKind (..),
    FaceMorphism,
    NerveCell,
    NerveSite,
    NerveSiteAlgebra (..),
    NerveSiteConstructionError (..),
    SimplexKey (..),
    faceMorphismFaceIndex,
    faceMorphismKind,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    faceKindFor,
    mkNerveSite,
    mkNerveSiteDenseWindow,
    mkNerveSiteFromRowPlan,
    mkNerveSiteWCOJWindow,
    mkNerveSiteWindow,
    nerveCellKey,
    nerveCellSimplex,
    nerveSiteBasis,
    nerveSiteCategory,
    nerveSiteCells,
    nerveSiteDepth,
    nerveSiteSourceNerve,
    restrictNerveSiteToCellKeys,
    siteCellsAtDimension,
    siteFaceMorphisms,
  )
where

import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Kind (Constraint, Type)
import Moonlight.Category (Category (Mor, Ob), FiniteComposableCategory)
import Moonlight.Sheaf.Kernel.Basis (SheafBasis, mkSheafBasis)
import Moonlight.Sheaf.Site.Skeleton.Window
  ( SiteSkeletonWindow (..),
    siteWindowDepth,
  )
import Moonlight.Sheaf.Site.Internal.Skeleton
  ( TruncatedSiteSkeleton (..),
    buildTruncatedSiteSkeletonWithPlan,
  )
import Moonlight.Sheaf.Site.Skeleton.RowSource
  ( DenseNerveArrangementError (..),
    SkeletonRowPlan,
    denseOrdinalSkeletonRowPlan,
    prepareDenseNerveArrangement,
    rowSourceToTruncatedNerve,
    skeletonRowPlan,
    skeletonRowPlanDepth,
    simplicialNerveRowSource,
    truncatedSkeletonRowPlan,
    wcojNerveRowSource,
  )
import Moonlight.Category.Simplicial (NerveSimplex)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet, TruncatedSSetObstruction)
import Numeric.Natural (Natural)

type NerveSiteAlgebra :: Type -> Constraint
class NerveSiteAlgebra tag where
  type NerveCategory tag
  type NerveSource tag
  type NerveMorphism tag

  buildSiteNerve :: NerveCategory tag -> Natural -> TruncatedNormalizedSSet (NerveSimplex (NerveCategory tag))
  simplexSourceValue :: NerveSimplex (NerveCategory tag) -> NerveSource tag
  simplexMorphismChain :: NerveSimplex (NerveCategory tag) -> [NerveMorphism tag]

type SimplexKey :: Type -> Type
data SimplexKey tag = SimplexKey
  { skSourceValue :: NerveSource tag,
    skMorphismChain :: [NerveMorphism tag]
  }

deriving stock instance (Eq (NerveSource tag), Eq (NerveMorphism tag)) => Eq (SimplexKey tag)
deriving stock instance (Ord (NerveSource tag), Ord (NerveMorphism tag)) => Ord (SimplexKey tag)
deriving stock instance (Show (NerveSource tag), Show (NerveMorphism tag)) => Show (SimplexKey tag)

type CellKey :: Type
data CellKey = CellKey
  { ckDimension :: Natural,
    ckOrdinal :: Int
  }
  deriving stock (Eq, Ord, Show)

type FaceKind :: Type
data FaceKind
  = LeadingFace
  | TrailingFace
  | InnerFace Natural
  deriving stock (Eq, Ord, Show)

type NerveCell :: Type -> Type
data NerveCell tag = NerveCell
  { nerveCellKey :: CellKey,
    nerveCellSimplex :: NerveSimplex (NerveCategory tag)
  }

instance Eq (NerveCell tag) where
  (==) =
    (==) `on` nerveCellKey

instance Ord (NerveCell tag) where
  compare =
    compare `on` nerveCellKey

instance Show (NerveCell tag) where
  show = show . nerveCellKey

type FaceMorphism :: Type -> Type
data FaceMorphism tag = FaceMorphism
  { faceMorphismSource :: NerveCell tag,
    faceMorphismTarget :: NerveCell tag,
    faceMorphismKind :: FaceKind,
    faceMorphismFaceIndex :: Natural,
    faceMorphismOrientation :: Int
  }

faceMorphismOrderKey ::
  FaceMorphism tag ->
  (NerveCell tag, NerveCell tag, FaceKind, Natural)
faceMorphismOrderKey faceMorphism =
  ( faceMorphismSource faceMorphism,
    faceMorphismTarget faceMorphism,
    faceMorphismKind faceMorphism,
    faceMorphismFaceIndex faceMorphism
  )

instance Eq (FaceMorphism tag) where
  (==) =
    (==) `on` faceMorphismOrderKey

instance Ord (FaceMorphism tag) where
  compare =
    compare `on` faceMorphismOrderKey

instance Show (FaceMorphism tag) where
  show faceMorphism =
    show
      ( faceMorphismSource faceMorphism,
        faceMorphismTarget faceMorphism,
        faceMorphismKind faceMorphism,
        faceMorphismFaceIndex faceMorphism,
        faceMorphismOrientation faceMorphism
      )

type NerveSite :: Type -> Type
data NerveSite tag = NerveSite
  { nerveSiteDepth :: Natural,
    nerveSiteCategory :: NerveCategory tag,
    nerveSiteSourceNerve :: TruncatedNormalizedSSet (NerveSimplex (NerveCategory tag)),
    nerveSiteCells :: [NerveCell tag],
    nerveSiteCellsByDimension :: Map Natural [NerveCell tag],
    nerveSiteBasis :: SheafBasis (NerveCell tag),
    siteFaceMorphisms :: [FaceMorphism tag]
  }

type NerveSiteConstructionError :: Type -> Type
data NerveSiteConstructionError tag
  = NerveSiteDenseArrangementFailed !(DenseNerveArrangementError (NerveCategory tag))
  | NerveSiteTruncatedNerveInvalid !(NonEmpty (TruncatedSSetObstruction (NerveSimplex (NerveCategory tag))))

instance Show (NerveSiteConstructionError tag) where
  show errorValue =
    case errorValue of
      NerveSiteDenseArrangementFailed _ ->
        "NerveSiteDenseArrangementFailed"
      NerveSiteTruncatedNerveInvalid _ ->
        "NerveSiteTruncatedNerveInvalid"

mkNerveSite ::
  forall tag.
  ( NerveSiteAlgebra tag,
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  NerveCategory tag ->
  Natural ->
  NerveSite tag
mkNerveSite categoryValue depthValue =
  let nerveValue = buildSiteNerve @tag categoryValue depthValue
   in mkNerveSiteFromRowPlan
        categoryValue
        nerveValue
        (truncatedSkeletonRowPlan depthValue (simplicialNerveRowSource nerveValue))

mkNerveSiteFromRowPlan ::
  forall tag.
  ( NerveSiteAlgebra tag,
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  NerveCategory tag ->
  TruncatedNormalizedSSet (NerveSimplex (NerveCategory tag)) ->
  SkeletonRowPlan (NerveCategory tag) ->
  NerveSite tag
mkNerveSiteFromRowPlan categoryValue sourceNerve rowPlan =
  nerveSiteFromSkeleton
    (skeletonRowPlanDepth rowPlan)
    categoryValue
    sourceNerve
    ( buildTruncatedSiteSkeletonWithPlan
        rowPlan
        mkNerveCell
        nerveCellSimplex
        (simplexKey @tag)
        faceKindFor
        mkFaceMorphism
    )

nerveSiteFromSkeleton ::
  Natural ->
  NerveCategory tag ->
  TruncatedNormalizedSSet (NerveSimplex (NerveCategory tag)) ->
  TruncatedSiteSkeleton (SimplexKey tag) (NerveCell tag) (FaceMorphism tag) ->
  NerveSite tag
nerveSiteFromSkeleton depthValue categoryValue sourceNerve skeletonValue =
  let cells = tssCells skeletonValue
   in NerveSite
        { nerveSiteDepth = depthValue,
          nerveSiteCategory = categoryValue,
          nerveSiteSourceNerve = sourceNerve,
          nerveSiteCells = cells,
          nerveSiteCellsByDimension = tssCellsByDimension skeletonValue,
          nerveSiteBasis = mkSheafBasis cells,
          siteFaceMorphisms = tssFaceMorphisms skeletonValue
        }

mkNerveSiteWindow ::
  forall tag.
  ( NerveSiteAlgebra tag,
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  NerveCategory tag ->
  SiteSkeletonWindow ->
  NerveSite tag
mkNerveSiteWindow categoryValue windowValue =
  let depthValue = siteWindowDepth windowValue
      nerveValue = buildSiteNerve @tag categoryValue depthValue
   in mkNerveSiteFromRowPlan
        categoryValue
        nerveValue
        ( skeletonRowPlan
            (sswCellDimensions windowValue)
            (sswFaceSourceDimensions windowValue)
            (simplicialNerveRowSource nerveValue)
        )

mkNerveSiteWCOJWindow ::
  forall tag.
  ( NerveSiteAlgebra tag,
    FiniteComposableCategory (NerveCategory tag),
    Ord (Ob (NerveCategory tag)),
    Ord (Mor (NerveCategory tag)),
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  NerveCategory tag ->
  SiteSkeletonWindow ->
  Either (NerveSiteConstructionError tag) (NerveSite tag)
mkNerveSiteWCOJWindow categoryValue windowValue =
  let rowPlan =
        skeletonRowPlan
          (sswCellDimensions windowValue)
          (sswFaceSourceDimensions windowValue)
          (wcojNerveRowSource categoryValue)
   in do
        sourceNerve <-
          firstTruncatedNerveError (rowSourceToTruncatedNerve rowPlan)
        pure (mkNerveSiteFromRowPlan categoryValue sourceNerve rowPlan)

mkNerveSiteDenseWindow ::
  forall tag.
  ( NerveSiteAlgebra tag,
    FiniteComposableCategory (NerveCategory tag),
    Ord (Ob (NerveCategory tag)),
    Eq (Mor (NerveCategory tag)),
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  NerveCategory tag ->
  SiteSkeletonWindow ->
  Either (NerveSiteConstructionError tag) (NerveSite tag)
mkNerveSiteDenseWindow categoryValue windowValue = do
  arrangement <-
    case prepareDenseNerveArrangement categoryValue of
      Left arrangementError -> Left (NerveSiteDenseArrangementFailed arrangementError)
      Right arrangementValue -> Right arrangementValue
  let rowPlan =
        denseOrdinalSkeletonRowPlan
          arrangement
          (sswCellDimensions windowValue)
          (sswFaceSourceDimensions windowValue)
  sourceNerve <-
    firstTruncatedNerveError (rowSourceToTruncatedNerve rowPlan)
  pure (mkNerveSiteFromRowPlan categoryValue sourceNerve rowPlan)

firstTruncatedNerveError ::
  Either (NonEmpty (TruncatedSSetObstruction (NerveSimplex (NerveCategory tag)))) value ->
  Either (NerveSiteConstructionError tag) value
firstTruncatedNerveError =
  either (Left . NerveSiteTruncatedNerveInvalid) Right

mkNerveCell :: Natural -> Int -> NerveSimplex (NerveCategory tag) -> NerveCell tag
mkNerveCell dimensionValue ordinal simplexValue =
  NerveCell
    { nerveCellKey = CellKey dimensionValue ordinal,
      nerveCellSimplex = simplexValue
    }

mkFaceMorphism ::
  NerveCell tag ->
  NerveCell tag ->
  FaceKind ->
  Natural ->
  Int ->
  FaceMorphism tag
mkFaceMorphism sourceCellValue targetCellValue faceKindValue faceIndex orientationValue =
  FaceMorphism
    { faceMorphismSource = sourceCellValue,
      faceMorphismTarget = targetCellValue,
      faceMorphismKind = faceKindValue,
      faceMorphismFaceIndex = faceIndex,
      faceMorphismOrientation = orientationValue
    }

siteCellsAtDimension :: NerveSite tag -> Natural -> [NerveCell tag]
siteCellsAtDimension siteValue dimensionValue =
  Map.findWithDefault [] dimensionValue (nerveSiteCellsByDimension siteValue)

restrictNerveSiteToCellKeys :: Set.Set CellKey -> NerveSite tag -> NerveSite tag
restrictNerveSiteToCellKeys keptKeys siteValue =
  let keptCells =
        filter
          (\cellValue -> Set.member (nerveCellKey cellValue) keptKeys)
          (nerveSiteCells siteValue)
      keptFaceMorphisms =
        filter
          ( \faceMorphism ->
              Set.member (nerveCellKey (faceMorphismSource faceMorphism)) keptKeys
                && Set.member (nerveCellKey (faceMorphismTarget faceMorphism)) keptKeys
          )
          (siteFaceMorphisms siteValue)
   in siteValue
        { nerveSiteCells = keptCells,
          nerveSiteCellsByDimension = cellsByDimensionFromCells keptCells,
          nerveSiteBasis = mkSheafBasis keptCells,
          siteFaceMorphisms = keptFaceMorphisms
        }

faceKindFor :: Natural -> Natural -> FaceKind
faceKindFor dimensionValue faceIndex
  | faceIndex == 0 = LeadingFace
  | faceIndex == dimensionValue = TrailingFace
  | otherwise = InnerFace (faceIndex - 1)

simplexKey :: forall tag. NerveSiteAlgebra tag => NerveSimplex (NerveCategory tag) -> SimplexKey tag
simplexKey simplexValue =
  SimplexKey
    { skSourceValue = simplexSourceValue @tag simplexValue,
      skMorphismChain = simplexMorphismChain @tag simplexValue
    }

cellsByDimensionFromCells :: [NerveCell tag] -> Map Natural [NerveCell tag]
cellsByDimensionFromCells =
  fmap reverse . foldl' insertCell Map.empty
  where
    insertCell ::
      Map Natural [NerveCell tag] ->
      NerveCell tag ->
      Map Natural [NerveCell tag]
    insertCell cellsByDimension cell =
      Map.insertWith
        (++)
        (ckDimension (nerveCellKey cell))
        [cell]
        cellsByDimension
