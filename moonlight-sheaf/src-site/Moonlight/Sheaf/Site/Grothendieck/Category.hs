{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Site.Grothendieck.Category
  ( GrothendieckCategory,
    GrothendieckCompositor (..),
    GrothendieckMor (..),
    GrothendieckOb (..),
    GrothendieckTwoMor (..),
    baseGrothendieckMorphisms,
    grothendieckCategory,
    grothendieckCategoryFromPresentation,
    grothendieckMorphisms,
    grothendieckNerve,
    grothendieckObjects,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Category (Category (..), FiniteComposableCategory (..))
import Moonlight.Sheaf.Site.Context.Pairs
  ( downwardPairsByStrategy,
    reflexiveDownwardPairsByStrategy,
  )
import Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentation (..),
    ContextPresentationSystem (..),
  )
import Moonlight.Sheaf.Site.System (AnalyzableSystem (..), LatticeAnalyzableSystem)
import Moonlight.Category.Simplicial (NerveSimplex, nerve)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet)
import Numeric.Natural (Natural)

type GrothendieckCategory :: Type -> Type
data GrothendieckCategory system = GrothendieckCategory
  { gcPresentation :: !(ContextPresentation system),
    gcMorphismIndex :: !(GrothendieckMorphismIndex system)
  }

type GrothendieckOb :: Type -> Type
data GrothendieckOb system = GrothendieckOb
  { goContext :: SystemCtx system,
    goSystem :: system,
    goValue :: SystemOb system
  }

type GrothendieckMor :: Type -> Type
data GrothendieckMor system = GrothendieckMor
  { gmSystem :: system,
    gmSourceContext :: SystemCtx system,
    gmTargetContext :: SystemCtx system,
    gmSourceObject :: SystemOb system,
    gmTargetObject :: SystemOb system,
    gmTargetMorphism :: Maybe (SystemMor system)
  }

type GrothendieckTwoMor :: Type -> Type
data GrothendieckTwoMor system = GrothendieckTwoMor
  deriving stock (Eq, Ord, Show)

type GrothendieckCompositor :: Type -> Type
data GrothendieckCompositor system = GrothendieckCompositor
  deriving stock (Eq, Ord, Show)

type GrothendieckMorphismKey system =
  ( SystemCtx system,
    SystemCtx system,
    SystemOb system,
    SystemOb system,
    Maybe (SystemMor system)
  )

data GrothendieckMorphismIndex system = GrothendieckMorphismIndex
  { gmiBaseByKey :: !(Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)),
    gmiBaseBySource :: !(Map.Map (GrothendieckOb system) [GrothendieckMor system]),
    gmiClosureByKey :: !(Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)),
    gmiClosureBySource :: !(Map.Map (GrothendieckOb system) (Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)))
  }

instance (Eq (SystemCtx system), Eq (SystemOb system)) => Eq (GrothendieckOb system) where
  leftObject == rightObject =
    (goContext leftObject, goValue leftObject)
      ==
    (goContext rightObject, goValue rightObject)

instance (Ord (SystemCtx system), Ord (SystemOb system)) => Ord (GrothendieckOb system) where
  compare leftObject rightObject =
    compare
      (goContext leftObject, goValue leftObject)
      (goContext rightObject, goValue rightObject)

instance (Show (SystemCtx system), Show (SystemOb system)) => Show (GrothendieckOb system) where
  show objectValue = show (goContext objectValue, goValue objectValue)

instance AnalyzableSystem system => Eq (GrothendieckMor system) where
  leftMorphism == rightMorphism =
    grothendieckMorphismKey leftMorphism
      == grothendieckMorphismKey rightMorphism

instance AnalyzableSystem system => Ord (GrothendieckMor system) where
  compare leftMorphism rightMorphism =
    compare
      (grothendieckMorphismKey leftMorphism)
      (grothendieckMorphismKey rightMorphism)

instance (Show (SystemCtx system), Show (SystemOb system), Show (SystemMor system)) => Show (GrothendieckMor system) where
  show morphismValue =
    show
      ( gmSourceContext morphismValue,
        gmTargetContext morphismValue,
        gmSourceObject morphismValue,
        gmTargetObject morphismValue,
        gmTargetMorphism morphismValue
      )

grothendieckMorphismKey :: GrothendieckMor system -> GrothendieckMorphismKey system
grothendieckMorphismKey morphismValue =
  ( gmSourceContext morphismValue,
    gmTargetContext morphismValue,
    gmSourceObject morphismValue,
    gmTargetObject morphismValue,
    gmTargetMorphism morphismValue
  )

grothendieckCategory :: (ContextPresentationSystem system, LatticeAnalyzableSystem system) => system -> GrothendieckCategory system
grothendieckCategory =
  grothendieckCategoryFromPresentation . systemContextPresentation

grothendieckCategoryFromPresentation ::
  LatticeAnalyzableSystem system =>
  ContextPresentation system ->
  GrothendieckCategory system
grothendieckCategoryFromPresentation contextPresentationValue =
  GrothendieckCategory
    { gcPresentation = contextPresentationValue,
      gcMorphismIndex = mkGrothendieckMorphismIndex contextPresentationValue
    }

grothendieckObjects :: AnalyzableSystem system => ContextPresentation system -> [GrothendieckOb system]
grothendieckObjects contextPresentationValue =
  [ GrothendieckOb contextValue systemValue objectValue
  | contextValue <- cpContexts contextPresentationValue,
    objectValue <- systemObjectsInContext systemValue contextValue
  ]
  where
    systemValue = cpSystem contextPresentationValue

grothendieckMorphisms :: LatticeAnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
grothendieckMorphisms contextPresentationValue =
  Map.elems (gmiClosureByKey (mkGrothendieckMorphismIndex contextPresentationValue))

morphismsFromPreparedIndex ::
  AnalyzableSystem system =>
  GrothendieckMorphismIndex system ->
  GrothendieckOb system ->
  Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)
morphismsFromPreparedIndex morphismIndex sourceObject =
  Map.findWithDefault Map.empty sourceObject (gmiClosureBySource morphismIndex)

morphismsFromBaseIndex ::
  AnalyzableSystem system =>
  GrothendieckMorphismIndex system ->
  GrothendieckOb system ->
  Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)
morphismsFromBaseIndex morphismIndex sourceObject =
  frontierClosure composedSourceFrontier Map.empty initialFrontier
  where
    initialFrontier =
      grothendieckMorphismMap
        (Map.findWithDefault [] sourceObject (gmiBaseBySource morphismIndex))

    composedSourceFrontier frontier _expanded =
      grothendieckMorphismMap
        [ composedMorphism
        | rightMorphism <- Map.elems frontier,
          leftMorphism <- Map.findWithDefault [] (grothendieckTarget rightMorphism) (gmiBaseBySource morphismIndex),
          Just composedMorphism <- [composeGrothendieckPair leftMorphism rightMorphism]
        ]

frontierClosure ::
  Ord key =>
  (Map.Map key value -> Map.Map key value -> Map.Map key value) ->
  Map.Map key value ->
  Map.Map key value ->
  Map.Map key value
frontierClosure nextFrontier discovered frontier
  | Map.null frontier = discovered
  | otherwise =
      let expanded = Map.union discovered frontier
          frontierValue = nextFrontier frontier expanded `Map.difference` expanded
       in frontierClosure nextFrontier expanded frontierValue

mkGrothendieckMorphismIndex ::
  LatticeAnalyzableSystem system =>
  ContextPresentation system ->
  GrothendieckMorphismIndex system
mkGrothendieckMorphismIndex contextPresentationValue =
  GrothendieckMorphismIndex
    { gmiBaseByKey = baseByKey,
      gmiBaseBySource = baseBySource,
      gmiClosureByKey = Map.unions (Map.elems closureBySource),
      gmiClosureBySource = closureBySource
    }
  where
    baseByKey =
      grothendieckMorphismMap (baseGrothendieckMorphismCandidates contextPresentationValue)
    baseBySource =
      Map.fromListWith
        (<>)
        [ (grothendieckSource morphismValue, [morphismValue])
        | morphismValue <- Map.elems baseByKey
        ]
    baseIndex =
      GrothendieckMorphismIndex
        { gmiBaseByKey = baseByKey,
          gmiBaseBySource = baseBySource,
          gmiClosureByKey = Map.empty,
          gmiClosureBySource = Map.empty
        }
    closureBySource =
      Map.fromList
        [ (objectValue, morphismsFromBaseIndex baseIndex objectValue)
        | objectValue <- grothendieckObjects contextPresentationValue
        ]

baseGrothendieckMorphisms :: LatticeAnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
baseGrothendieckMorphisms =
  Map.elems . grothendieckMorphismMap . baseGrothendieckMorphismCandidates

baseGrothendieckMorphismCandidates :: LatticeAnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
baseGrothendieckMorphismCandidates contextPresentationValue =
  identityMorphisms contextPresentationValue
    <> verticalMorphisms contextPresentationValue
    <> horizontalAndDiagonalMorphisms contextPresentationValue

grothendieckMorphismMap ::
  AnalyzableSystem system =>
  [GrothendieckMor system] ->
  Map.Map (GrothendieckMorphismKey system) (GrothendieckMor system)
grothendieckMorphismMap morphismValues =
  Map.fromList
    [ (grothendieckMorphismKey normalizedMorphism, normalizedMorphism)
    | morphismValue <- morphismValues,
      let normalizedMorphism = normalizeGrothendieckMorphism morphismValue
    ]

normalizeGrothendieckMorphism :: AnalyzableSystem system => GrothendieckMor system -> GrothendieckMor system
normalizeGrothendieckMorphism morphismValue =
  morphismValue
    { gmTargetMorphism =
        fmap
          (normalizeMorphism (gmSystem morphismValue) (gmTargetContext morphismValue))
          (gmTargetMorphism morphismValue)
    }

mkGrothendieckMorphism ::
  AnalyzableSystem system =>
  system ->
  SystemCtx system ->
  SystemCtx system ->
  SystemOb system ->
  SystemOb system ->
  Maybe (SystemMor system) ->
  GrothendieckMor system
mkGrothendieckMorphism systemValue sourceContext targetContext sourceObject targetObject targetMorphism =
  normalizeGrothendieckMorphism
    GrothendieckMor
      { gmSystem = systemValue,
        gmSourceContext = sourceContext,
        gmTargetContext = targetContext,
        gmSourceObject = sourceObject,
        gmTargetObject = targetObject,
        gmTargetMorphism = targetMorphism
      }

composeGrothendieckPair :: AnalyzableSystem system => GrothendieckMor system -> GrothendieckMor system -> Maybe (GrothendieckMor system)
composeGrothendieckPair leftMorphism rightMorphism =
  if grothendieckSource leftMorphism /= grothendieckTarget rightMorphism
    then Nothing
    else
      fmap
        ( mkGrothendieckMorphism
            (gmSystem rightMorphism)
            (gmSourceContext rightMorphism)
            (gmTargetContext leftMorphism)
            (gmSourceObject rightMorphism)
            (gmTargetObject leftMorphism)
        )
        (composeGrothendieckMorphisms leftMorphism rightMorphism)

grothendieckSource :: GrothendieckMor system -> GrothendieckOb system
grothendieckSource morphismValue =
  GrothendieckOb (gmSourceContext morphismValue) (gmSystem morphismValue) (gmSourceObject morphismValue)

grothendieckTarget :: GrothendieckMor system -> GrothendieckOb system
grothendieckTarget morphismValue =
  GrothendieckOb (gmTargetContext morphismValue) (gmSystem morphismValue) (gmTargetObject morphismValue)

identityMorphisms :: AnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
identityMorphisms contextPresentationValue =
  [ mkGrothendieckMorphism
      (goSystem objectValue)
      (goContext objectValue)
      (goContext objectValue)
      (goValue objectValue)
      (goValue objectValue)
      (Just (identityMorphism (goSystem objectValue) (goContext objectValue) (goValue objectValue)))
  | objectValue <- grothendieckObjects contextPresentationValue
  ]

verticalMorphisms :: LatticeAnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
verticalMorphisms contextPresentationValue =
  [ mkGrothendieckMorphism
      systemValue
      sourceContext
      targetContext
      sourceObject
      targetObject
      Nothing
  | (sourceContext, targetContext) <- contextPairs,
    sourceObject <- systemObjectsInContext systemValue sourceContext,
    Just targetObject <- [restrictObject systemValue sourceContext targetContext sourceObject]
  ]
  where
    systemValue = cpSystem contextPresentationValue
    contextPairs =
      downwardPairsByStrategy
        (cpPairStrategy contextPresentationValue)
        systemValue
        (cpContexts contextPresentationValue)

horizontalAndDiagonalMorphisms :: LatticeAnalyzableSystem system => ContextPresentation system -> [GrothendieckMor system]
horizontalAndDiagonalMorphisms contextPresentationValue =
  [ mkGrothendieckMorphism
      systemValue
      sourceContext
      targetContext
      (morphismSource systemValue sourceMorphism)
      (morphismTarget systemValue targetMorphism)
      (Just targetMorphism)
  | (sourceContext, targetContext) <- contextPairs,
    sourceMorphism <- systemMorphismsInContext systemValue sourceContext,
    Just targetMorphism <- [restrictMorphismAlong systemValue sourceContext targetContext sourceMorphism]
  ]
  where
    systemValue = cpSystem contextPresentationValue
    contextPairs =
      reflexiveDownwardPairsByStrategy
        (cpPairStrategy contextPresentationValue)
        systemValue
        (cpContexts contextPresentationValue)

instance AnalyzableSystem system => Category (GrothendieckCategory system) where
  type Ob (GrothendieckCategory system) = GrothendieckOb system
  type Mor (GrothendieckCategory system) = GrothendieckMor system
  type TwoMor (GrothendieckCategory system) = GrothendieckTwoMor system
  type Compositor (GrothendieckCategory system) = GrothendieckCompositor system

  identity _ (GrothendieckOb contextValue systemValue objectValue) =
    Right
      ( mkGrothendieckMorphism
          systemValue
          contextValue
          contextValue
          objectValue
          objectValue
          (Just (identityMorphism systemValue contextValue objectValue))
      )

  compose _ leftMorphism rightMorphism =
    case composeGrothendieckPair leftMorphism rightMorphism of
      Just composedMorphism -> Right (composedMorphism, GrothendieckCompositor)
      Nothing -> Left ()

  source _ = Right . grothendieckSource

  target _ = Right . grothendieckTarget

instance LatticeAnalyzableSystem system => FiniteComposableCategory (GrothendieckCategory system) where
  enumerateObjects = grothendieckObjects . gcPresentation
  enumerateMorphisms = Map.elems . gmiClosureByKey . gcMorphismIndex
  enumerateMorphismsFrom categoryValue =
    Map.elems . morphismsFromPreparedIndex (gcMorphismIndex categoryValue)

grothendieckNerve :: LatticeAnalyzableSystem system => ContextPresentation system -> Natural -> TruncatedNormalizedSSet (NerveSimplex (GrothendieckCategory system))
grothendieckNerve contextPresentationValue =
  nerve (grothendieckCategoryFromPresentation contextPresentationValue)

composeGrothendieckMorphisms :: AnalyzableSystem system => GrothendieckMor system -> GrothendieckMor system -> Maybe (Maybe (SystemMor system))
composeGrothendieckMorphisms leftMorphism rightMorphism =
  do
    restrictedRight <-
      restrictToContext
        systemValue
        (gmTargetContext rightMorphism)
        (gmTargetContext leftMorphism)
        (gmTargetMorphism rightMorphism)
    combineMorphisms
      systemValue
      (gmTargetContext leftMorphism)
      (gmTargetMorphism leftMorphism)
      restrictedRight
  where
    systemValue = gmSystem leftMorphism

combineMorphisms :: AnalyzableSystem system => system -> SystemCtx system -> Maybe (SystemMor system) -> Maybe (SystemMor system) -> Maybe (Maybe (SystemMor system))
combineMorphisms systemValue contextValue leftMorphism rightMorphism =
  case (leftMorphism, rightMorphism) of
    (Nothing, Nothing) -> Just Nothing
    (Just morphismValue, Nothing) -> Just (Just morphismValue)
    (Nothing, Just morphismValue) -> Just (Just morphismValue)
    (Just leftValue, Just rightValue) ->
      either (const Nothing) (Just . Just) (composeMorphisms systemValue contextValue leftValue rightValue)

restrictToContext :: AnalyzableSystem system => system -> SystemCtx system -> SystemCtx system -> Maybe (SystemMor system) -> Maybe (Maybe (SystemMor system))
restrictToContext _ _ _ Nothing = Just Nothing
restrictToContext systemValue sourceContext targetContext (Just morphismValue)
  | sourceContext == targetContext = Just (Just morphismValue)
  | otherwise = fmap Just (restrictMorphism systemValue sourceContext targetContext morphismValue)

restrictMorphismAlong :: AnalyzableSystem system => system -> SystemCtx system -> SystemCtx system -> SystemMor system -> Maybe (SystemMor system)
restrictMorphismAlong systemValue sourceContext targetContext morphismValue
  | sourceContext == targetContext = Just morphismValue
  | otherwise = restrictMorphism systemValue sourceContext targetContext morphismValue
