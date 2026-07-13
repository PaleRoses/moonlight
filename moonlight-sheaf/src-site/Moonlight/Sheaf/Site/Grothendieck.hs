module Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCategory,
    GrothendieckCell,
    GrothendieckCellKey (..),
    GrothendieckFaceMorphism,
    GrothendieckMor (..),
    GrothendieckOb (..),
    GrothendieckSimplexKey (..),
    GrothendieckSite,
    grothendieckCategory,
    grothendieckCategoryFromPresentation,
    grothendieckCellDimension,
    grothendieckCellKey,
    grothendieckCellMorphisms,
    grothendieckCellSingleMorphism,
    grothendieckCellSimplex,
    grothendieckFaceMorphismFaceIndex,
    grothendieckFaceMorphismKind,
    grothendieckFaceMorphismOrientation,
    grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    baseGrothendieckMorphisms,
    grothendieckMorphisms,
    grothendieckNerve,
    grothendieckObjects,
    grothendieckSiteCellsAtDimension,
    grothendieckSiteBasis,
    grothendieckSiteCells,
    grothendieckSiteDepth,
    grothendieckSiteFaceMorphisms,
    grothendieckSiteSourceNerve,
    grothendieckZeroCellFromPresentation,
    grothendieckZeroCellByObject,
    grothendieckSimplexKey,
    mkGrothendieckSite,
    mkGrothendieckSiteFromCoverBasis,
    mkGrothendieckSiteFromCoverBasisWindow,
    mkGrothendieckSiteWindow,
  )
where

import Data.Function (on)
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Category (chainMorphisms, chainStartObject, singletonComposableChain)
import Moonlight.Sheaf.Kernel.Basis (SheafBasis, mkSheafBasis)
import Moonlight.Sheaf.Site.Skeleton.Window
  ( SiteSkeletonWindow (..),
    siteWindowDepth,
  )
import Moonlight.Sheaf.Site.Class
  ( coverSources,
    coverTarget,
  )
import Moonlight.Sheaf.Site.Context
  ( ContextCoverBasis (..),
  )
import Moonlight.Sheaf.Site.Context.Pairs (ContextPairStrategy (ExhaustivePairs))
import Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentationSystem (..),
    ContextPresentation,
    contextPresentationWith,
  )
import Moonlight.Sheaf.Site.Grothendieck.Category
  ( GrothendieckCategory,
    GrothendieckMor (..),
    GrothendieckOb (..),
    gmSourceContext,
    gmSourceObject,
    gmTargetObject,
    gmTargetContext,
    gmTargetMorphism,
    goContext,
    goValue,
    baseGrothendieckMorphisms,
    grothendieckCategory,
    grothendieckCategoryFromPresentation,
    grothendieckMorphisms,
    grothendieckNerve,
    grothendieckObjects,
  )
import Moonlight.Sheaf.Site.Internal.Skeleton
  ( TruncatedSiteSkeleton (..),
    buildTruncatedSiteSkeletonWithPlan,
  )
import Moonlight.Sheaf.Site.Skeleton.RowSource
  ( SkeletonRowPlan (..),
    simplicialNerveRowSource,
    skeletonRowPlanDepth,
    truncatedSkeletonRowPlan,
  )
import Moonlight.Sheaf.Site.Construction.Nerve (FaceKind, faceKindFor)
import Moonlight.Sheaf.Site.System (AnalyzableSystem (..), LatticeAnalyzableSystem, SystemCtx, SystemMor)
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerveSimplexChain,
    nerveSimplexDimension,
    nerveSimplexFromChain,
  )
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet)
import Numeric.Natural (Natural)

type GrothendieckCellKey :: Type
data GrothendieckCellKey = GrothendieckCellKey
  { gckDimension :: Natural,
    gckOrdinal :: Int
  }
  deriving stock (Eq, Ord, Show)

type GrothendieckCell :: Type -> Type
data GrothendieckCell system = GrothendieckCell
  { grothendieckCellKey :: GrothendieckCellKey,
    grothendieckCellSimplex :: NerveSimplex (GrothendieckCategory system)
  }

instance Eq (GrothendieckCell system) where
  (==) =
    (==) `on` grothendieckCellKey

instance Ord (GrothendieckCell system) where
  compare =
    compare `on` grothendieckCellKey

instance Show (GrothendieckCell system) where
  show = show . grothendieckCellKey

type GrothendieckSimplexKey :: Type -> Type
data GrothendieckSimplexKey system = GrothendieckSimplexKey
  { gskStartObject :: GrothendieckOb system,
    gskMorphisms :: [GrothendieckMor system]
  }
  deriving stock (Eq, Ord)

type GrothendieckFaceMorphism :: Type -> Type
data GrothendieckFaceMorphism system = GrothendieckFaceMorphism
  { grothendieckFaceMorphismSource :: GrothendieckCell system,
    grothendieckFaceMorphismTarget :: GrothendieckCell system,
    grothendieckFaceMorphismKind :: FaceKind,
    grothendieckFaceMorphismFaceIndex :: Natural,
    grothendieckFaceMorphismOrientation :: Int
  }

grothendieckFaceMorphismKey ::
  GrothendieckFaceMorphism system ->
  (GrothendieckCell system, GrothendieckCell system, FaceKind, Natural)
grothendieckFaceMorphismKey faceMorphism =
  ( grothendieckFaceMorphismSource faceMorphism,
    grothendieckFaceMorphismTarget faceMorphism,
    grothendieckFaceMorphismKind faceMorphism,
    grothendieckFaceMorphismFaceIndex faceMorphism
  )

instance Eq (GrothendieckFaceMorphism system) where
  (==) =
    (==) `on` grothendieckFaceMorphismKey

instance Ord (GrothendieckFaceMorphism system) where
  compare =
    compare `on` grothendieckFaceMorphismKey

instance Show (GrothendieckFaceMorphism system) where
  show faceMorphism =
    show
      ( grothendieckFaceMorphismSource faceMorphism,
        grothendieckFaceMorphismTarget faceMorphism,
        grothendieckFaceMorphismKind faceMorphism,
        grothendieckFaceMorphismFaceIndex faceMorphism,
        grothendieckFaceMorphismOrientation faceMorphism
      )

type GrothendieckSite :: Type -> Type
data GrothendieckSite system = GrothendieckSite
  { grothendieckSiteDepth :: Natural,
    grothendieckSiteSourceNerve :: TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system)),
    grothendieckSiteCells :: [GrothendieckCell system],
    grothendieckSiteCellsByDimension :: Map Natural [GrothendieckCell system],
    grothendieckSiteBasis :: SheafBasis (GrothendieckCell system),
    grothendieckSiteFaceMorphisms :: [GrothendieckFaceMorphism system]
  }

mkGrothendieckSite ::
  (ContextPresentationSystem system, LatticeAnalyzableSystem system) =>
  system ->
  Natural ->
  GrothendieckSite system
mkGrothendieckSite systemValue depthValue =
  let nerveValue =
        grothendieckNerve
          (systemContextPresentation systemValue)
          depthValue
   in grothendieckSiteFromNerve depthValue nerveValue

mkGrothendieckSiteWindow ::
  (ContextPresentationSystem system, LatticeAnalyzableSystem system) =>
  system ->
  SiteSkeletonWindow ->
  GrothendieckSite system
mkGrothendieckSiteWindow systemValue windowValue =
  let depthValue = siteWindowDepth windowValue
      nerveValue =
        grothendieckNerve
          (systemContextPresentation systemValue)
          depthValue
   in grothendieckSiteFromNerveWindow (sswCellDimensions windowValue) (sswFaceSourceDimensions windowValue) depthValue nerveValue

mkGrothendieckSiteFromCoverBasis ::
  (ContextCoverBasis system, LatticeAnalyzableSystem system) =>
  system ->
  Natural ->
  GrothendieckSite system
mkGrothendieckSiteFromCoverBasis systemValue depthValue =
  let nerveValue =
        grothendieckNerve
          (contextPresentationWith systemValue (coverBasisContexts systemValue) ExhaustivePairs)
          depthValue
   in grothendieckSiteFromNerve depthValue nerveValue

mkGrothendieckSiteFromCoverBasisWindow ::
  (ContextCoverBasis system, LatticeAnalyzableSystem system) =>
  system ->
  SiteSkeletonWindow ->
  GrothendieckSite system
mkGrothendieckSiteFromCoverBasisWindow systemValue windowValue =
  let depthValue = siteWindowDepth windowValue
      nerveValue =
        grothendieckNerve
          (contextPresentationWith systemValue (coverBasisContexts systemValue) ExhaustivePairs)
          depthValue
   in grothendieckSiteFromNerveWindow (sswCellDimensions windowValue) (sswFaceSourceDimensions windowValue) depthValue nerveValue

coverBasisContexts ::
  ContextCoverBasis system =>
  system ->
  [SystemCtx system]
coverBasisContexts systemValue =
  Set.toAscList $
    Set.fromList $
      allContexts systemValue
        <> [ contextValue
             | targetContext <- allContexts systemValue,
               coverValue <- contextCoversAt systemValue targetContext,
               contextValue <- coverTarget coverValue : coverSources coverValue
           ]

grothendieckSiteFromNerve ::
  AnalyzableSystem system =>
  Natural ->
  TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system)) ->
  GrothendieckSite system
grothendieckSiteFromNerve depthValue nerveValue =
  grothendieckSiteFromRowPlan
    nerveValue
    (truncatedSkeletonRowPlan depthValue (simplicialNerveRowSource nerveValue))

grothendieckSiteFromNerveWindow ::
  AnalyzableSystem system =>
  Set.Set Natural ->
  Set.Set Natural ->
  Natural ->
  TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system)) ->
  GrothendieckSite system
grothendieckSiteFromNerveWindow cellDimensions faceSourceDimensions depthValue nerveValue =
  grothendieckSiteFromRowPlan
    nerveValue
    SkeletonRowPlan
      { skeletonRowPlanDepth = depthValue,
        skeletonRowPlanCellDimensions = cellDimensions,
        skeletonRowPlanFaceSourceDimensions = faceSourceDimensions,
        skeletonRowPlanSource = simplicialNerveRowSource nerveValue
      }

grothendieckSiteFromRowPlan ::
  AnalyzableSystem system =>
  TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system)) ->
  SkeletonRowPlan (GrothendieckCategory system) ->
  GrothendieckSite system
grothendieckSiteFromRowPlan nerveValue rowPlan =
  grothendieckSiteFromSkeleton
    (skeletonRowPlanDepth rowPlan)
    nerveValue
    ( buildTruncatedSiteSkeletonWithPlan
        rowPlan
        mkGrothendieckCell
        grothendieckCellSimplex
        grothendieckSimplexKey
        faceKindFor
        mkGrothendieckFaceMorphism
    )

grothendieckSiteFromSkeleton ::
  Natural ->
  TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system)) ->
  TruncatedSiteSkeleton (GrothendieckSimplexKey system) (GrothendieckCell system) (GrothendieckFaceMorphism system) ->
  GrothendieckSite system
grothendieckSiteFromSkeleton depthValue nerveValue skeletonValue =
  let cells = tssCells skeletonValue
   in GrothendieckSite
        { grothendieckSiteDepth = depthValue,
          grothendieckSiteSourceNerve = nerveValue,
          grothendieckSiteCells = cells,
          grothendieckSiteCellsByDimension = tssCellsByDimension skeletonValue,
          grothendieckSiteBasis = mkSheafBasis cells,
          grothendieckSiteFaceMorphisms = tssFaceMorphisms skeletonValue
        }

mkGrothendieckCell :: Natural -> Int -> NerveSimplex (GrothendieckCategory system) -> GrothendieckCell system
mkGrothendieckCell _ ordinal simplexValue =
  GrothendieckCell
    { grothendieckCellKey =
        GrothendieckCellKey
          { gckDimension = nerveSimplexDimension simplexValue,
            gckOrdinal = ordinal
          },
      grothendieckCellSimplex = simplexValue
    }

mkGrothendieckFaceMorphism ::
  GrothendieckCell system ->
  GrothendieckCell system ->
  FaceKind ->
  Natural ->
  Int ->
  GrothendieckFaceMorphism system
mkGrothendieckFaceMorphism sourceCellValue targetCellValue faceKindValue faceIndex orientationValue =
  GrothendieckFaceMorphism
    { grothendieckFaceMorphismSource = sourceCellValue,
      grothendieckFaceMorphismTarget = targetCellValue,
      grothendieckFaceMorphismKind = faceKindValue,
      grothendieckFaceMorphismFaceIndex = faceIndex,
      grothendieckFaceMorphismOrientation = orientationValue
    }

grothendieckSiteCellsAtDimension :: GrothendieckSite system -> Natural -> [GrothendieckCell system]
grothendieckSiteCellsAtDimension siteValue dimensionValue =
  Map.findWithDefault [] dimensionValue (grothendieckSiteCellsByDimension siteValue)

grothendieckZeroCellByObject ::
  Eq (GrothendieckOb system) =>
  GrothendieckSite system ->
  GrothendieckOb system ->
  Maybe (GrothendieckCell system)
grothendieckZeroCellByObject siteValue objectValue =
  find
    ( \cellValue ->
        chainStartObject
          (nerveSimplexChain (grothendieckCellSimplex cellValue))
          == objectValue
    )
    (grothendieckSiteCellsAtDimension siteValue 0)

grothendieckZeroCellFromPresentation ::
  AnalyzableSystem system =>
  ContextPresentation system ->
  GrothendieckOb system ->
  Maybe (GrothendieckCell system)
grothendieckZeroCellFromPresentation contextPresentationValue objectValue =
  case matchingOrdinals of
    [ordinalValue] ->
      Just
        GrothendieckCell
          { grothendieckCellKey =
              GrothendieckCellKey
                { gckDimension = 0,
                  gckOrdinal = ordinalValue
                },
            grothendieckCellSimplex =
              nerveSimplexFromChain (singletonComposableChain objectValue)
          }
    _ ->
      Nothing
  where
    matchingOrdinals =
      [ ordinalValue
      | (ordinalValue, candidateObject) <-
          zip
            [0 :: Int ..]
            (reverse (grothendieckObjects contextPresentationValue)),
        candidateObject == objectValue
      ]

grothendieckCellDimension :: GrothendieckCell system -> Int
grothendieckCellDimension = fromIntegral . gckDimension . grothendieckCellKey

grothendieckCellMorphisms :: GrothendieckCell system -> [SystemMor system]
grothendieckCellMorphisms =
  mapMaybe gmTargetMorphism
    . chainMorphisms
    . nerveSimplexChain
    . grothendieckCellSimplex

grothendieckCellSingleMorphism :: GrothendieckCell system -> Maybe (SystemMor system)
grothendieckCellSingleMorphism grothendieckCell =
  case grothendieckCellMorphisms grothendieckCell of
    [morphismValue] -> Just morphismValue
    _ -> Nothing

grothendieckSimplexKey :: NerveSimplex (GrothendieckCategory system) -> GrothendieckSimplexKey system
grothendieckSimplexKey simplexValue =
  let chainValue = nerveSimplexChain simplexValue
   in GrothendieckSimplexKey
        { gskStartObject = chainStartObject chainValue,
          gskMorphisms = chainMorphisms chainValue
        }
