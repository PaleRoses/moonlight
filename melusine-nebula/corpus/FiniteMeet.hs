{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.FiniteMeet
  ( FiniteMeetSiteSpec (..),
    FiniteMeetSite,
    FiniteMeetMorphism,
    FiniteMeetSiteBuildError (..),
    finiteMeetMorphism,
    finiteMeetRefines,
    finiteMeetSiteCells,
    finiteMeetSiteRefinements,
    finiteMeetSiteCovers,
    finiteMeetSiteMeet,
    mkFiniteMeetSite,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( closureUnder,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )

type FiniteMeetSiteSpec :: Type -> Type
data FiniteMeetSiteSpec cell = FiniteMeetSiteSpec
  { fmssCells :: !(NonEmpty cell),
    fmssRefinements :: !(Set (cell, cell)),
    fmssCovers :: !(Map cell [NonEmpty cell])
  }
  deriving stock (Eq, Show)

type FiniteMeetSite :: Type -> Type
data FiniteMeetSite cell = FiniteMeetSite
  { fmsCellsInternal :: !(NonEmpty cell),
    fmsRefinementsInternal :: !(Set (cell, cell)),
    fmsCoversInternal :: !(Map cell [CoveringFamily cell (FiniteMeetMorphism cell)]),
    fmsMeetsInternal :: !(Map (cell, cell) cell)
  }
  deriving stock (Eq, Show)

type FiniteMeetMorphism :: Type -> Type
data FiniteMeetMorphism cell = FiniteMeetMorphism
  deriving stock (Eq, Ord, Show)

type FiniteMeetSiteBuildError :: Type -> Type
data FiniteMeetSiteBuildError cell
  = FiniteMeetUnknownSource !cell
  | FiniteMeetUnknownTarget !cell
  | FiniteMeetCoverUnknownTarget !cell
  | FiniteMeetCoverUnknownSource !cell !cell
  | FiniteMeetCoverSourceDoesNotRefineTarget !cell !cell
  | FiniteMeetAntisymmetryViolation !cell !cell
  | FiniteMeetMissingMeet !cell !cell
  | FiniteMeetAmbiguousMeet !cell !cell ![cell]
  | FiniteMeetCoverMalformed !(CoverConstructionError cell)
  deriving stock (Eq, Ord, Show)

mkFiniteMeetSite ::
  Ord cell =>
  FiniteMeetSiteSpec cell ->
  Either (FiniteMeetSiteBuildError cell) (FiniteMeetSite cell)
mkFiniteMeetSite spec = do
  traverse_ (validateRefinementEdge cellSet) (Set.toAscList (fmssRefinements spec))
  let closedRefinements =
        refinementClosure
          cellSet
          (withIdentities cellSet (fmssRefinements spec))
  traverse_ (validateAntisymmetry closedRefinements) (Set.toAscList closedRefinements)
  meets <- buildFiniteMeets cellSet closedRefinements
  covers <- buildFiniteCovers cellSet closedRefinements (fmssCovers spec)
  pure
    FiniteMeetSite
      { fmsCellsInternal = cells,
        fmsRefinementsInternal = closedRefinements,
        fmsCoversInternal = covers,
        fmsMeetsInternal = meets
      }
  where
    cells =
      nonEmptyAscCells (fmssCells spec)

    cellSet =
      Set.fromList (NonEmpty.toList cells)

finiteMeetMorphism ::
  Ord cell =>
  FiniteMeetSite cell ->
  cell ->
  cell ->
  Maybe (CheckedMorphism cell (FiniteMeetMorphism cell))
finiteMeetMorphism site source target =
  if finiteMeetRefines site source target
    then Just (checkedFiniteMeetMorphism source target)
    else Nothing
{-# INLINE finiteMeetMorphism #-}

finiteMeetRefines ::
  Ord cell =>
  FiniteMeetSite cell ->
  cell ->
  cell ->
  Bool
finiteMeetRefines site source target =
  Set.member (source, target) (fmsRefinementsInternal site)
{-# INLINE finiteMeetRefines #-}

finiteMeetSiteCells :: FiniteMeetSite cell -> NonEmpty cell
finiteMeetSiteCells =
  fmsCellsInternal
{-# INLINE finiteMeetSiteCells #-}

finiteMeetSiteRefinements :: FiniteMeetSite cell -> Set (cell, cell)
finiteMeetSiteRefinements =
  fmsRefinementsInternal
{-# INLINE finiteMeetSiteRefinements #-}

finiteMeetSiteCovers ::
  FiniteMeetSite cell ->
  Map cell [CoveringFamily cell (FiniteMeetMorphism cell)]
finiteMeetSiteCovers =
  fmsCoversInternal
{-# INLINE finiteMeetSiteCovers #-}

finiteMeetSiteMeet ::
  Ord cell =>
  FiniteMeetSite cell ->
  cell ->
  cell ->
  Maybe cell
finiteMeetSiteMeet site leftCell rightCell =
  Map.lookup
    (normalizePair leftCell rightCell)
    (fmsMeetsInternal site)
{-# INLINE finiteMeetSiteMeet #-}

instance Ord cell => Site (FiniteMeetSite cell) where
  type SiteObject (FiniteMeetSite cell) = cell
  type SiteMorphism (FiniteMeetSite cell) = FiniteMeetMorphism cell

  siteObjects =
    NonEmpty.toList . fmsCellsInternal

  siteMorphisms site =
    fmap
      (uncurry checkedFiniteMeetMorphism)
      (Set.toAscList (fmsRefinementsInternal site))

  identityAt _site objectValue =
    checkedFiniteMeetMorphism objectValue objectValue

  coversAt site objectValue =
    Map.findWithDefault [] objectValue (fmsCoversInternal site)

  composeChecked site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | not (checkedFiniteMeetMorphismBelongs site outerMorphism) =
        Nothing
    | not (checkedFiniteMeetMorphismBelongs site innerMorphism) =
        Nothing
    | finiteMeetRefines site (cmSource innerMorphism) (cmTarget outerMorphism) =
        Just
          ( checkedFiniteMeetMorphism
              (cmSource innerMorphism)
              (cmTarget outerMorphism)
          )
    | otherwise =
        Nothing

  pullbackPair site leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | not (checkedFiniteMeetMorphismBelongs site leftMorphism) =
        Nothing
    | not (checkedFiniteMeetMorphismBelongs site rightMorphism) =
        Nothing
    | otherwise = do
        apex <-
          finiteMeetSiteMeet
            site
            (cmSource leftMorphism)
            (cmSource rightMorphism)
        leftLeg <-
          finiteMeetMorphism
            site
            apex
            (cmSource leftMorphism)
        rightLeg <-
          finiteMeetMorphism
            site
            apex
            (cmSource rightMorphism)
        pure
          PullbackSquare
            { psLeftBase = leftMorphism,
              psRightBase = rightMorphism,
              psApex = apex,
              psToLeft = leftLeg,
              psToRight = rightLeg
            }

checkedFiniteMeetMorphism ::
  cell ->
  cell ->
  CheckedMorphism cell (FiniteMeetMorphism cell)
checkedFiniteMeetMorphism source target =
  CheckedMorphism
    { cmSource = source,
      cmTarget = target,
      cmWitness = FiniteMeetMorphism
    }
{-# INLINE checkedFiniteMeetMorphism #-}

checkedFiniteMeetMorphismBelongs ::
  Ord cell =>
  FiniteMeetSite cell ->
  CheckedMorphism cell (FiniteMeetMorphism cell) ->
  Bool
checkedFiniteMeetMorphismBelongs site morphism =
  finiteMeetRefines site (cmSource morphism) (cmTarget morphism)
{-# INLINE checkedFiniteMeetMorphismBelongs #-}

nonEmptyAscCells ::
  Ord cell =>
  NonEmpty cell ->
  NonEmpty cell
nonEmptyAscCells (firstCell :| remainingCells) =
  case lowerCells of
    [] ->
      firstCell :| higherCells
    lowerCell : lowerRest ->
      lowerCell :| (lowerRest <> (firstCell : higherCells))
  where
    (lowerCells, greaterOrEqualCells) =
      span (< firstCell) (Set.toAscList (Set.fromList remainingCells))

    higherCells =
      case greaterOrEqualCells of
        equalCell : restCells | equalCell == firstCell ->
          restCells
        restCells ->
          restCells

validateRefinementEdge ::
  Ord cell =>
  Set cell ->
  (cell, cell) ->
  Either (FiniteMeetSiteBuildError cell) ()
validateRefinementEdge cells (source, target)
  | not (Set.member source cells) =
      Left (FiniteMeetUnknownSource source)
  | not (Set.member target cells) =
      Left (FiniteMeetUnknownTarget target)
  | otherwise =
      Right ()

withIdentities ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  Set (cell, cell)
withIdentities cells refinements =
  Set.union
    refinements
    (Set.map (\cell -> (cell, cell)) cells)

refinementClosure ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  Set (cell, cell)
refinementClosure cells refinements =
  Set.fromList
    [ (source, target)
    | source <- Set.toAscList cells,
      target <- Set.toAscList (closureUnder (targetsFrom adjacency) (Set.singleton source))
    ]
  where
    adjacency =
      Map.fromListWith
        Set.union
        [ (source, Set.singleton target)
        | (source, target) <- Set.toAscList refinements
        ]

targetsFrom ::
  Ord cell =>
  Map cell (Set cell) ->
  cell ->
  Set cell
targetsFrom adjacency cell =
  Map.findWithDefault Set.empty cell adjacency
{-# INLINE targetsFrom #-}

validateAntisymmetry ::
  Ord cell =>
  Set (cell, cell) ->
  (cell, cell) ->
  Either (FiniteMeetSiteBuildError cell) ()
validateAntisymmetry refinements (source, target)
  | source == target =
      Right ()
  | Set.member (target, source) refinements =
      Left (FiniteMeetAntisymmetryViolation source target)
  | otherwise =
      Right ()

buildFiniteMeets ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  Either
    (FiniteMeetSiteBuildError cell)
    (Map (cell, cell) cell)
buildFiniteMeets cells refinements =
  Map.fromList
    <$> traverse
      ( \pair@(leftCell, rightCell) ->
          fmap (pair,) (meetOf cells refinements leftCell rightCell)
      )
      [ normalizePair leftCell rightCell
      | leftCell <- orderedCells,
        rightCell <- orderedCells,
        leftCell <= rightCell
      ]
  where
    orderedCells =
      Set.toAscList cells

meetOf ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  cell ->
  cell ->
  Either (FiniteMeetSiteBuildError cell) cell
meetOf cells refinements leftCell rightCell =
  case Set.toAscList greatestLowerBounds of
    [] ->
      Left (FiniteMeetMissingMeet leftCell rightCell)
    [meetCell] ->
      Right meetCell
    ambiguousCells ->
      Left (FiniteMeetAmbiguousMeet leftCell rightCell ambiguousCells)
  where
    lowerBounds =
      Set.filter
        ( \candidate ->
            Set.member (candidate, leftCell) refinements
              && Set.member (candidate, rightCell) refinements
        )
        cells

    greatestLowerBounds =
      Set.filter
        ( \candidate ->
            all
              (\lowerBound -> Set.member (lowerBound, candidate) refinements)
              (Set.toAscList lowerBounds)
        )
        lowerBounds

buildFiniteCovers ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  Map cell [NonEmpty cell] ->
  Either
    (FiniteMeetSiteBuildError cell)
    (Map cell [CoveringFamily cell (FiniteMeetMorphism cell)])
buildFiniteCovers cells refinements =
  Map.traverseWithKey (buildFiniteCoverFamilies cells refinements)

buildFiniteCoverFamilies ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  cell ->
  [NonEmpty cell] ->
  Either
    (FiniteMeetSiteBuildError cell)
    [CoveringFamily cell (FiniteMeetMorphism cell)]
buildFiniteCoverFamilies cells refinements target sourceFamilies = do
  validateCoverTarget cells target
  traverse
    (buildFiniteCoverFamily cells refinements target)
    sourceFamilies

validateCoverTarget ::
  Ord cell =>
  Set cell ->
  cell ->
  Either (FiniteMeetSiteBuildError cell) ()
validateCoverTarget cells target =
  if Set.member target cells
    then Right ()
    else Left (FiniteMeetCoverUnknownTarget target)

buildFiniteCoverFamily ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  cell ->
  NonEmpty cell ->
  Either
    (FiniteMeetSiteBuildError cell)
    (CoveringFamily cell (FiniteMeetMorphism cell))
buildFiniteCoverFamily cells refinements target sources = do
  arrows <-
    traverse
      (coverArrowFor cells refinements target)
      sources
  first
    FiniteMeetCoverMalformed
    (mkCoveringFamily target arrows)

coverArrowFor ::
  Ord cell =>
  Set cell ->
  Set (cell, cell) ->
  cell ->
  cell ->
  Either
    (FiniteMeetSiteBuildError cell)
    (CheckedMorphism cell (FiniteMeetMorphism cell))
coverArrowFor cells refinements target source
  | not (Set.member source cells) =
      Left (FiniteMeetCoverUnknownSource target source)
  | not (Set.member (source, target) refinements) =
      Left (FiniteMeetCoverSourceDoesNotRefineTarget source target)
  | otherwise =
      Right (checkedFiniteMeetMorphism source target)

normalizePair :: Ord cell => cell -> cell -> (cell, cell)
normalizePair leftCell rightCell =
  if leftCell <= rightCell
    then (leftCell, rightCell)
    else (rightCell, leftCell)
{-# INLINE normalizePair #-}
