{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Equation-shaped authoring compiled into the finite site, presheaf, and
-- morphism owners.
module Moonlight.Sheaf.Presentation
  ( Presentation,
    StalkRestrictionKernel (..),
    PresentedRestrictionFailure (..),
    PresentedComponentFailure (..),
    PresentationObstruction (..),
    CompiledPresentation,
    declareCell,
    declareRefinement,
    declareCover,
    declarePresheaf,
    declareFiber,
    restricts,
    declareMorphism,
    componentAt,
    declareIdentityMorphism,
    declareComposition,
    compilePresentation,
    presentationSite,
    presentationPresheafAt,
    presentationMorphismAt,
    FinitePresheaf,
    FinitePresheafFailure (..),
    FiniteFiber,
    finiteFiberAt,
    finiteFiberValues,
    restrictPresentedPresheaf,
    FinitePresheafMorphism,
    FinitePresheafMorphismFailure (..),
    FinitePresheafMorphismCompositionComponentFailure (..),
    finitePresheafMorphismComponentAt,
    finitePresheafMorphismComponents,
    finitePresheafMorphismSource,
    finitePresheafMorphismTarget,
  )
where

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (duplicatesOrd)
import Moonlight.Sheaf.Presheaf.Finite
  ( FiniteFiber,
    FinitePresheaf (fpRestrict, fpSite),
    FinitePresheafFailure (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismCompositionComponentFailure (..),
    FinitePresheafMorphismFailure (..),
    composeAlignedFinitePresheafMorphisms,
    finitePresheafMorphismComponentAt,
    finitePresheafMorphismComponents,
    finitePresheafMorphismSource,
    finitePresheafMorphismTarget,
    identityFinitePresheafMorphism,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkRestrictionKernel (..),
    applyStalkRestrictionKernel,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteRestrictionMorphisms,
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteBuildError,
    FiniteMeetSiteSpec (..),
    finiteMeetRefines,
    mkFiniteMeetSite,
  )

type PresentedRestrictionFailure :: Type -> Type
data PresentedRestrictionFailure cell
  = PresentedRestrictionUnavailable !cell !cell
  deriving stock (Eq, Show)

type PresentedComponentFailure :: Type -> Type
data PresentedComponentFailure cell
  = PresentedComponentUnavailable !cell
  deriving stock (Eq, Show)

type FiberMap cell stalk = Map cell [stalk]

type RestrictionMap cell stalk = Map (cell, cell) (StalkRestrictionKernel stalk)

type ComponentMap cell stalk = Map cell (stalk -> stalk)

type PresentedPresheaf cell stalk mismatch =
  FinitePresheaf
    (FiniteMeetSite cell)
    stalk
    mismatch
    (PresentedRestrictionFailure cell)

type PresentedMorphism cell stalk mismatch =
  FinitePresheafMorphism
    (FiniteMeetSite cell)
    stalk
    stalk
    mismatch
    mismatch
    (PresentedRestrictionFailure cell)
    (PresentedRestrictionFailure cell)

type PresheafDeclaration :: Type -> Type -> Type -> Type
data PresheafDeclaration cell stalk mismatch = PresheafDeclaration
  { pdMismatches :: cell -> stalk -> stalk -> [mismatch],
    pdNormalize :: cell -> stalk -> stalk
  }

type RestrictionDeclaration :: Type -> Type -> Type -> Type
data RestrictionDeclaration presheafName cell stalk = RestrictionDeclaration
  !presheafName
  !cell
  !cell
  !(StalkRestrictionKernel stalk)

type MorphismDeclaration :: Type -> Type -> Type
data MorphismDeclaration presheafName morphismName
  = ComponentMorphismDeclaration !presheafName !presheafName
  | IdentityMorphismDeclaration !presheafName
  | CompositionMorphismDeclaration !morphismName !morphismName

type ComponentDeclaration :: Type -> Type -> Type -> Type
data ComponentDeclaration morphismName cell stalk = ComponentDeclaration
  !morphismName
  !cell
  (stalk -> stalk)

type PresentationState :: Type -> Type -> Type -> Type -> Type -> Type
data PresentationState cell presheafName morphismName stalk mismatch = PresentationState
  { psCells :: ![cell],
    psRefinements :: ![(cell, cell)],
    psCovers :: ![(cell, NonEmpty cell)],
    psPresheafDeclarations :: ![(presheafName, PresheafDeclaration cell stalk mismatch)],
    psFibers :: ![(presheafName, cell, [stalk])],
    psRestrictions :: ![RestrictionDeclaration presheafName cell stalk],
    psMorphismDeclarations :: ![(morphismName, MorphismDeclaration presheafName morphismName)],
    psComponents :: ![ComponentDeclaration morphismName cell stalk]
  }

type Presentation :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype Presentation cell presheafName morphismName stalk mismatch a = Presentation
  ( PresentationState cell presheafName morphismName stalk mismatch ->
    (a, PresentationState cell presheafName morphismName stalk mismatch)
  )

instance Functor (Presentation cell presheafName morphismName stalk mismatch) where
  fmap mapResult (Presentation run) =
    Presentation
      ( \state ->
          let (result, nextState) = run state
           in (mapResult result, nextState)
      )

instance Applicative (Presentation cell presheafName morphismName stalk mismatch) where
  pure result =
    Presentation (\state -> (result, state))

  Presentation runFunction <*> Presentation runArgument =
    Presentation
      ( \state ->
          let (function, functionState) = runFunction state
              (argument, argumentState) = runArgument functionState
           in (function argument, argumentState)
      )

instance Monad (Presentation cell presheafName morphismName stalk mismatch) where
  Presentation run >>= continue =
    Presentation
      ( \state ->
          let (result, nextState) = run state
              Presentation runContinuation = continue result
           in runContinuation nextState
      )

type PresentationObstruction :: Type -> Type -> Type -> Type -> Type -> Type
data PresentationObstruction cell presheafName morphismName stalk mismatch
  = PresentationNoCells
  | PresentationDuplicateCell !cell
  | PresentationUnknownCell !cell
  | PresentationDuplicatePresheaf !presheafName
  | PresentationUnknownPresheaf !presheafName
  | PresentationDuplicateMorphism !morphismName
  | PresentationUnknownMorphism !morphismName
  | PresentationDuplicateFiber !presheafName !cell
  | PresentationIdentityRestriction !presheafName !cell
  | PresentationRestrictionNotInSite !presheafName !cell !cell
  | PresentationDuplicateRestriction !presheafName !cell !cell
  | PresentationRestrictionMissing
      !presheafName
      !(CheckedMorphism cell (FiniteMeetMorphism cell))
  | PresentationComponentNotAllowed !morphismName
  | PresentationDuplicateComponent !morphismName !cell
  | PresentationComponentMissing !morphismName !cell
  | PresentationSiteBuildFailed !(FiniteMeetSiteBuildError cell)
  | PresentationPresheafBuildFailed
      !presheafName
      !( FinitePresheafFailure
           cell
           (FiniteMeetMorphism cell)
           stalk
           mismatch
           (PresentedRestrictionFailure cell)
       )
  | PresentationMorphismBuildFailed
      !morphismName
      !( FinitePresheafMorphismFailure
           cell
           (FiniteMeetMorphism cell)
           stalk
           stalk
           (PresentedRestrictionFailure cell)
           (PresentedRestrictionFailure cell)
           mismatch
           (PresentedComponentFailure cell)
       )
  | PresentationCompositionMiddleMismatch !morphismName !morphismName
  | PresentationCompositionComponentMissing
      !morphismName
      !(FinitePresheafMorphismCompositionComponentFailure cell stalk stalk)
  deriving stock (Eq, Show)

type CompiledPresentation :: Type -> Type -> Type -> Type -> Type -> Type
data CompiledPresentation cell presheafName morphismName stalk mismatch = CompiledPresentation
  { cpsSite :: !(FiniteMeetSite cell),
    cpsPresheaves :: !(Map presheafName (PresentedPresheaf cell stalk mismatch)),
    cpsMorphisms :: !(Map morphismName (PresentedMorphism cell stalk mismatch))
  }

declareCell ::
  cell ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareCell cellValue =
  modifyPresentation (\state -> state {psCells = cellValue : psCells state})

declareRefinement ::
  cell ->
  cell ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareRefinement finerCell coarserCell =
  modifyPresentation
    (\state -> state {psRefinements = (finerCell, coarserCell) : psRefinements state})

declareCover ::
  cell ->
  NonEmpty cell ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareCover targetCell sourceCells =
  modifyPresentation
    (\state -> state {psCovers = (targetCell, sourceCells) : psCovers state})

declarePresheaf ::
  presheafName ->
  (cell -> stalk -> stalk -> [mismatch]) ->
  (cell -> stalk -> stalk) ->
  Presentation cell presheafName morphismName stalk mismatch ()
declarePresheaf presheafName mismatchAt normalizeAt =
  let declaration =
        PresheafDeclaration
          { pdMismatches = mismatchAt,
            pdNormalize = normalizeAt
          }
   in modifyPresentation
        ( \state ->
            state
              { psPresheafDeclarations =
                  (presheafName, declaration) : psPresheafDeclarations state
              }
        )

declareFiber ::
  presheafName ->
  cell ->
  [stalk] ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareFiber presheafName cellValue stalks =
  modifyPresentation
    (\state -> state {psFibers = (presheafName, cellValue, stalks) : psFibers state})

restricts ::
  presheafName ->
  cell ->
  cell ->
  StalkRestrictionKernel stalk ->
  Presentation cell presheafName morphismName stalk mismatch ()
restricts presheafName finerCell coarserCell restriction =
  modifyPresentation
    ( \state ->
        state
          { psRestrictions =
              RestrictionDeclaration presheafName finerCell coarserCell restriction
                : psRestrictions state
          }
    )

declareMorphism ::
  morphismName ->
  presheafName ->
  presheafName ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareMorphism morphismName sourceName targetName =
  declareMorphismValue
    morphismName
    (ComponentMorphismDeclaration sourceName targetName)

componentAt ::
  morphismName ->
  cell ->
  (stalk -> stalk) ->
  Presentation cell presheafName morphismName stalk mismatch ()
componentAt morphismName cellValue component =
  modifyPresentation
    ( \state ->
        state
          { psComponents =
              ComponentDeclaration morphismName cellValue component : psComponents state
          }
    )

declareIdentityMorphism ::
  morphismName ->
  presheafName ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareIdentityMorphism morphismName presheafName =
  declareMorphismValue morphismName (IdentityMorphismDeclaration presheafName)

declareComposition ::
  morphismName ->
  morphismName ->
  morphismName ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareComposition compositeName outerName innerName =
  declareMorphismValue
    compositeName
    (CompositionMorphismDeclaration outerName innerName)

modifyPresentation ::
  ( PresentationState cell presheafName morphismName stalk mismatch ->
    PresentationState cell presheafName morphismName stalk mismatch
  ) ->
  Presentation cell presheafName morphismName stalk mismatch ()
modifyPresentation update =
  Presentation (\state -> ((), update state))

declareMorphismValue ::
  morphismName ->
  MorphismDeclaration presheafName morphismName ->
  Presentation cell presheafName morphismName stalk mismatch ()
declareMorphismValue morphismName declaration =
  modifyPresentation
    ( \state ->
        state
          { psMorphismDeclarations =
              (morphismName, declaration) : psMorphismDeclarations state
          }
    )

initialPresentationState ::
  PresentationState cell presheafName morphismName stalk mismatch
initialPresentationState =
  PresentationState
    { psCells = [],
      psRefinements = [],
      psCovers = [],
      psPresheafDeclarations = [],
      psFibers = [],
      psRestrictions = [],
      psMorphismDeclarations = [],
      psComponents = []
    }

insertUnique :: Ord key => key -> value -> Map key value -> Maybe (Map key value)
insertUnique key value entries =
  case Map.insertLookupWithKey (\_ newValue _oldValue -> newValue) key value entries of
    (Nothing, inserted) -> Just inserted
    (Just _existing, _replaced) -> Nothing

compilePresentation ::
  forall cell presheafName morphismName stalk mismatch a.
  (Ord cell, Ord presheafName, Ord morphismName, Ord stalk) =>
  Presentation cell presheafName morphismName stalk mismatch a ->
  Either
    (PresentationObstruction cell presheafName morphismName stalk mismatch)
    (a, CompiledPresentation cell presheafName morphismName stalk mismatch)
compilePresentation (Presentation run) = do
  cells <-
    maybe
      (Left PresentationNoCells)
      Right
      (NonEmpty.nonEmpty declaredCells)
  traverse_ (Left . PresentationDuplicateCell) (duplicatesOrd declaredCells)
  presheafDeclarations <-
    foldM
      insertPresheafDeclaration
      Map.empty
      (reverse (psPresheafDeclarations finalState))
  morphismDeclarations <-
    foldM
      insertMorphismDeclaration
      Map.empty
      declaredMorphismEntries
  siteValue <-
    first PresentationSiteBuildFailed
      . mkFiniteMeetSite
      $ FiniteMeetSiteSpec
        { fmssCells = cells,
          fmssRefinements = Set.fromList (psRefinements finalState),
          fmssCovers = foldr insertCoverFamily Map.empty (psCovers finalState)
        }
  fiberMaps <-
    foldM
      (insertFiberDeclaration cellSet)
      Map.empty
      (reverse (psFibers finalState))
  traverse_ (requireDeclaredPresheaf presheafDeclarations) (Map.keys fiberMaps)
  restrictionMaps <-
    foldM
      (insertRestrictionDeclaration cellSet siteValue)
      Map.empty
      (reverse (psRestrictions finalState))
  traverse_ (requireDeclaredPresheaf presheafDeclarations) (Map.keys restrictionMaps)
  compiledPresheaves <-
    Map.traverseWithKey
      (compileDeclaredPresheaf siteValue fiberMaps restrictionMaps)
      presheafDeclarations
  componentMaps <-
    foldM
      (insertComponentDeclaration cellSet)
      Map.empty
      (reverse (psComponents finalState))
  traverse_ (requireComponentMorphism morphismDeclarations) (Map.keys componentMaps)
  (compiledMorphisms, _endpoints) <-
    foldM
      (compileDeclaredMorphism siteValue compiledPresheaves componentMaps)
      (Map.empty, Map.empty)
      declaredMorphismEntries
  pure
    ( result,
      CompiledPresentation
        { cpsSite = siteValue,
          cpsPresheaves = compiledPresheaves,
          cpsMorphisms = compiledMorphisms
        }
    )
  where
    (result, finalState) = run initialPresentationState

    declaredCells = reverse (psCells finalState)

    cellSet = Set.fromList declaredCells

    declaredMorphismEntries = reverse (psMorphismDeclarations finalState)

    insertCoverFamily ::
      (cell, NonEmpty cell) ->
      Map cell [NonEmpty cell] ->
      Map cell [NonEmpty cell]
    insertCoverFamily (targetCell, sourceCells) =
      Map.insertWith (flip (<>)) targetCell [sourceCells]

    insertPresheafDeclaration ::
      Map presheafName (PresheafDeclaration cell stalk mismatch) ->
      (presheafName, PresheafDeclaration cell stalk mismatch) ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (Map presheafName (PresheafDeclaration cell stalk mismatch))
    insertPresheafDeclaration declarations (presheafName, declaration) =
      maybe
        (Left (PresentationDuplicatePresheaf presheafName))
        Right
        (insertUnique presheafName declaration declarations)

    insertMorphismDeclaration ::
      Map morphismName (MorphismDeclaration presheafName morphismName) ->
      (morphismName, MorphismDeclaration presheafName morphismName) ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (Map morphismName (MorphismDeclaration presheafName morphismName))
    insertMorphismDeclaration declarations (morphismName, declaration) =
      maybe
        (Left (PresentationDuplicateMorphism morphismName))
        Right
        (insertUnique morphismName declaration declarations)

    requireKnownCell ::
      Set cell ->
      cell ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ()
    requireKnownCell knownCells cellValue =
      unless (Set.member cellValue knownCells) $
        Left (PresentationUnknownCell cellValue)

    requireDeclaredPresheaf ::
      Map presheafName (PresheafDeclaration cell stalk mismatch) ->
      presheafName ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ()
    requireDeclaredPresheaf declarations presheafName =
      unless (Map.member presheafName declarations) $
        Left (PresentationUnknownPresheaf presheafName)

    insertFiberDeclaration ::
      Set cell ->
      Map presheafName (FiberMap cell stalk) ->
      (presheafName, cell, [stalk]) ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (Map presheafName (FiberMap cell stalk))
    insertFiberDeclaration knownCells fiberMaps (presheafName, cellValue, stalks) = do
      requireKnownCell knownCells cellValue
      let presheafFibers = Map.findWithDefault Map.empty presheafName fiberMaps
      maybe
        (Left (PresentationDuplicateFiber presheafName cellValue))
        (\insertedFibers -> Right (Map.insert presheafName insertedFibers fiberMaps))
        (insertUnique cellValue stalks presheafFibers)

    insertRestrictionDeclaration ::
      Set cell ->
      FiniteMeetSite cell ->
      Map presheafName (RestrictionMap cell stalk) ->
      RestrictionDeclaration presheafName cell stalk ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (Map presheafName (RestrictionMap cell stalk))
    insertRestrictionDeclaration knownCells siteValue restrictionMaps (RestrictionDeclaration presheafName finerCell coarserCell restriction) = do
      if finerCell == coarserCell
        then do
          requireKnownCell knownCells finerCell
          Left (PresentationIdentityRestriction presheafName finerCell)
        else
          unless (finiteMeetRefines siteValue finerCell coarserCell) $ do
            requireKnownCell knownCells finerCell
            requireKnownCell knownCells coarserCell
            Left (PresentationRestrictionNotInSite presheafName finerCell coarserCell)
      let restrictionMap = Map.findWithDefault Map.empty presheafName restrictionMaps
          restrictionKey = (finerCell, coarserCell)
      maybe
        (Left (PresentationDuplicateRestriction presheafName finerCell coarserCell))
        (\insertedRestrictions -> Right (Map.insert presheafName insertedRestrictions restrictionMaps))
        (insertUnique restrictionKey restriction restrictionMap)

    compileDeclaredPresheaf ::
      FiniteMeetSite cell ->
      Map presheafName (FiberMap cell stalk) ->
      Map presheafName (RestrictionMap cell stalk) ->
      presheafName ->
      PresheafDeclaration cell stalk mismatch ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (PresentedPresheaf cell stalk mismatch)
    compileDeclaredPresheaf siteValue fiberMaps restrictionMaps presheafName declaration = do
      let restrictionMap = Map.findWithDefault Map.empty presheafName restrictionMaps
          expectedRestrictions = siteRestrictionMorphisms siteValue
      unless (Map.size restrictionMap == length expectedRestrictions) $
        traverse_
          (requireRestriction presheafName restrictionMap)
          expectedRestrictions
      first (PresentationPresheafBuildFailed presheafName) $
        mkFinitePresheaf
          siteValue
          (compileRestrictionAction restrictionMap)
          (pdMismatches declaration)
          (pdNormalize declaration)
          (Map.findWithDefault Map.empty presheafName fiberMaps)

    requireRestriction ::
      presheafName ->
      RestrictionMap cell stalk ->
      CheckedMorphism cell (FiniteMeetMorphism cell) ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ()
    requireRestriction presheafName restrictionMap morphismValue =
      unless
        (Map.member (cmSource morphismValue, cmTarget morphismValue) restrictionMap)
        (Left (PresentationRestrictionMissing presheafName morphismValue))

    compileRestrictionAction ::
      RestrictionMap cell stalk ->
      CheckedMorphism cell (FiniteMeetMorphism cell) ->
      stalk ->
      Either (PresentedRestrictionFailure cell) stalk
    compileRestrictionAction restrictionMap
      | all isIdentityRestriction (Map.elems restrictionMap) =
          \_morphismValue stalkValue -> Right stalkValue
      | otherwise =
          \morphismValue stalkValue ->
            if cmSource morphismValue == cmTarget morphismValue
              then Right stalkValue
              else
                maybe
                  (unavailable morphismValue)
                  (Right . (`applyStalkRestrictionKernel` stalkValue))
                  (Map.lookup (cmSource morphismValue, cmTarget morphismValue) restrictionMap)
      where
        unavailable ::
          CheckedMorphism cell (FiniteMeetMorphism cell) ->
          Either (PresentedRestrictionFailure cell) stalk
        unavailable morphismValue =
          Left
            ( PresentedRestrictionUnavailable
                (cmSource morphismValue)
                (cmTarget morphismValue)
            )

        isIdentityRestriction :: StalkRestrictionKernel stalk -> Bool
        isIdentityRestriction restrictionKernel =
          case restrictionKernel of
            StalkRestrictionIdentity -> True
            StalkRestrictionMap _restriction -> False

    insertComponentDeclaration ::
      Set cell ->
      Map morphismName (ComponentMap cell stalk) ->
      ComponentDeclaration morphismName cell stalk ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (Map morphismName (ComponentMap cell stalk))
    insertComponentDeclaration knownCells componentMaps (ComponentDeclaration morphismName cellValue component) = do
      requireKnownCell knownCells cellValue
      let componentMap = Map.findWithDefault Map.empty morphismName componentMaps
      maybe
        (Left (PresentationDuplicateComponent morphismName cellValue))
        (\insertedComponents -> Right (Map.insert morphismName insertedComponents componentMaps))
        (insertUnique cellValue component componentMap)

    requireComponentMorphism ::
      Map morphismName (MorphismDeclaration presheafName morphismName) ->
      morphismName ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ()
    requireComponentMorphism declarations morphismName = do
      declaration <-
        maybe
          (Left (PresentationUnknownMorphism morphismName))
          Right
          (Map.lookup morphismName declarations)
      case declaration of
        ComponentMorphismDeclaration _sourceName _targetName -> pure ()
        IdentityMorphismDeclaration _presheafName ->
          Left (PresentationComponentNotAllowed morphismName)
        CompositionMorphismDeclaration _outerName _innerName ->
          Left (PresentationComponentNotAllowed morphismName)

    resolveCompiledPresheaf ::
      Map presheafName (PresentedPresheaf cell stalk mismatch) ->
      presheafName ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        (PresentedPresheaf cell stalk mismatch)
    resolveCompiledPresheaf compiledPresheaves presheafName =
      maybe
        (Left (PresentationUnknownPresheaf presheafName))
        Right
        (Map.lookup presheafName compiledPresheaves)

    compileDeclaredMorphism ::
      FiniteMeetSite cell ->
      Map presheafName (PresentedPresheaf cell stalk mismatch) ->
      Map morphismName (ComponentMap cell stalk) ->
      ( Map morphismName (PresentedMorphism cell stalk mismatch),
        Map morphismName (presheafName, presheafName)
      ) ->
      (morphismName, MorphismDeclaration presheafName morphismName) ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ( Map morphismName (PresentedMorphism cell stalk mismatch),
          Map morphismName (presheafName, presheafName)
        )
    compileDeclaredMorphism siteValue compiledPresheaves componentMaps (compiledSoFar, endpointsSoFar) (morphismName, declaration) =
      case declaration of
        ComponentMorphismDeclaration sourceName targetName -> do
          sourcePresheaf <- resolveCompiledPresheaf compiledPresheaves sourceName
          targetPresheaf <- resolveCompiledPresheaf compiledPresheaves targetName
          let componentMap = Map.findWithDefault Map.empty morphismName componentMaps
              expectedComponents = siteObjects siteValue
          unless (Map.size componentMap == length expectedComponents) $
            traverse_ (requireComponent morphismName componentMap) expectedComponents
          morphismValue <-
            first (PresentationMorphismBuildFailed morphismName) $
              mkFinitePresheafMorphism
                sourcePresheaf
                targetPresheaf
                (applyComponent componentMap)
          pure (insertCompiled morphismValue (sourceName, targetName))
        IdentityMorphismDeclaration presheafName -> do
          presheafValue <- resolveCompiledPresheaf compiledPresheaves presheafName
          pure
            ( insertCompiled
                (identityFinitePresheafMorphism presheafValue)
                (presheafName, presheafName)
            )
        CompositionMorphismDeclaration outerName innerName -> do
          (outerSource, outerTarget) <- resolveEndpoints outerName
          (innerSource, innerTarget) <- resolveEndpoints innerName
          unless (innerTarget == outerSource) $
            Left (PresentationCompositionMiddleMismatch outerName innerName)
          outerMorphism <- resolveCompiledMorphism outerName
          innerMorphism <- resolveCompiledMorphism innerName
          morphismValue <-
            first (PresentationCompositionComponentMissing morphismName) $
              composeAlignedFinitePresheafMorphisms outerMorphism innerMorphism
          pure (insertCompiled morphismValue (innerSource, outerTarget))
      where
        insertCompiled morphismValue endpoints =
          ( Map.insert morphismName morphismValue compiledSoFar,
            Map.insert morphismName endpoints endpointsSoFar
          )

        resolveEndpoints operandName =
          maybe
            (Left (PresentationUnknownMorphism operandName))
            Right
            (Map.lookup operandName endpointsSoFar)

        resolveCompiledMorphism operandName =
          maybe
            (Left (PresentationUnknownMorphism operandName))
            Right
            (Map.lookup operandName compiledSoFar)

    requireComponent ::
      morphismName ->
      ComponentMap cell stalk ->
      cell ->
      Either
        (PresentationObstruction cell presheafName morphismName stalk mismatch)
        ()
    requireComponent morphismName componentMap cellValue =
      unless
        (Map.member cellValue componentMap)
        (Left (PresentationComponentMissing morphismName cellValue))

    applyComponent ::
      ComponentMap cell stalk ->
      cell ->
      stalk ->
      Either (PresentedComponentFailure cell) stalk
    applyComponent componentMap cellValue stalkValue =
      maybe
        (Left (PresentedComponentUnavailable cellValue))
        (Right . ($ stalkValue))
        (Map.lookup cellValue componentMap)

presentationSite ::
  CompiledPresentation cell presheafName morphismName stalk mismatch ->
  FiniteMeetSite cell
presentationSite =
  cpsSite
{-# INLINE presentationSite #-}

presentationPresheafAt ::
  Ord presheafName =>
  presheafName ->
  CompiledPresentation cell presheafName morphismName stalk mismatch ->
  Maybe (PresentedPresheaf cell stalk mismatch)
presentationPresheafAt presheafName =
  Map.lookup presheafName . cpsPresheaves
{-# INLINE presentationPresheafAt #-}

restrictPresentedPresheaf ::
  Ord cell =>
  CheckedMorphism cell (FiniteMeetMorphism cell) ->
  stalk ->
  PresentedPresheaf cell stalk mismatch ->
  Either (PresentedRestrictionFailure cell) stalk
restrictPresentedPresheaf morphismValue stalkValue presheaf
  | finiteMeetRefines
      (fpSite presheaf)
      (cmSource morphismValue)
      (cmTarget morphismValue) =
      fpRestrict presheaf morphismValue stalkValue
  | otherwise =
      Left
        ( PresentedRestrictionUnavailable
            (cmSource morphismValue)
            (cmTarget morphismValue)
        )
{-# INLINE restrictPresentedPresheaf #-}

presentationMorphismAt ::
  Ord morphismName =>
  morphismName ->
  CompiledPresentation cell presheafName morphismName stalk mismatch ->
  Maybe (PresentedMorphism cell stalk mismatch)
presentationMorphismAt morphismName =
  Map.lookup morphismName . cpsMorphisms
{-# INLINE presentationMorphismAt #-}
