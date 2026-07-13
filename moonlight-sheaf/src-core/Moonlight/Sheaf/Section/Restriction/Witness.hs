module Moonlight.Sheaf.Section.Restriction.Witness
  ( ContextMorphism,
    contextMorphismSource,
    contextMorphismTarget,
    mkContextMorphism,
    identityContextMorphism,
    composeContextMorphism,
    CanonicalContextSection (..),
    RestrictionKernel (..),
    contextRestrictionIdentity,
    contextMorphismLeftIdentity,
    contextMorphismRightIdentity,
    contextMorphismAssociative,
    contextRestrictionFunctorialAction,
    contextGlobalSectionInvariant,
  )
where

import Data.Kind (Type)
import Moonlight.Category
  ( ThinMorphism,
    identityThinMorphism,
    mkThinMorphismBy,
    thinMorphismSource,
    thinMorphismTarget,
  )

type ContextMorphism :: Type -> Type
type ContextMorphism = ThinMorphism

contextMorphismSource :: ContextMorphism c -> c
contextMorphismSource = thinMorphismSource

contextMorphismTarget :: ContextMorphism c -> c
contextMorphismTarget = thinMorphismTarget

mkContextMorphism :: (c -> c -> Either obstruction Bool) -> c -> c -> Either obstruction (Maybe (ContextMorphism c))
mkContextMorphism refinesTo sourceContext targetContext =
  fmap
    ( \refines ->
        if refines
          then mkThinMorphismBy (\_ _ -> True) sourceContext targetContext
          else Nothing
    )
    (refinesTo sourceContext targetContext)

identityContextMorphism :: c -> ContextMorphism c
identityContextMorphism =
  identityThinMorphism

composeContextMorphism ::
  Eq c =>
  (c -> c -> Either obstruction Bool) ->
  ContextMorphism c ->
  ContextMorphism c ->
  Either obstruction (Maybe (ContextMorphism c))
composeContextMorphism refinesTo firstMorphism secondMorphism =
  if contextMorphismTarget firstMorphism == contextMorphismSource secondMorphism
    then
      mkContextMorphism
        refinesTo
        (contextMorphismSource firstMorphism)
        (contextMorphismTarget secondMorphism)
    else Right Nothing

type CanonicalContextSection :: Type -> Type
newtype CanonicalContextSection section = CanonicalContextSection
  { canonicalContextSectionValue :: section
  }
  deriving stock (Eq, Show)

type RestrictionKernel :: Type -> Type -> Type -> Type
data RestrictionKernel c obstruction section = RestrictionKernel
  { rkRefinesTo :: c -> c -> Either obstruction Bool,
    rkCanonicalSectionAt :: c -> Either obstruction (CanonicalContextSection section),
    rkRestrictSectionToTarget :: c -> c -> section -> Either obstruction section,
    rkCachedContexts :: [c]
  }

restrictCanonicalSection ::
  ContextMorphism c ->
  RestrictionKernel c obstruction section ->
  CanonicalContextSection section ->
  Either obstruction (Maybe (CanonicalContextSection section))
restrictCanonicalSection contextMorphism kernel (CanonicalContextSection sectionValue) =
  let sourceContext = contextMorphismSource contextMorphism
      targetContext = contextMorphismTarget contextMorphism
   in do
        refines <- rkRefinesTo kernel sourceContext targetContext
        if refines
          then fmap (Just . CanonicalContextSection) (rkRestrictSectionToTarget kernel sourceContext targetContext sectionValue)
          else pure Nothing

isGlobalCanonicalSection ::
  Eq section =>
  c ->
  CanonicalContextSection section ->
  RestrictionKernel c obstruction section ->
  Either obstruction Bool
isGlobalCanonicalSection sourceContext sectionValue kernel =
  and
    <$>
    traverse
      ( \targetContext -> do
          maybeContextMorphism <- mkContextMorphism (rkRefinesTo kernel) sourceContext targetContext
          case maybeContextMorphism of
            Nothing ->
              pure True
            Just contextMorphism ->
              fmap
                (== Just sectionValue)
                (restrictCanonicalSection contextMorphism kernel sectionValue)
      )
      (rkCachedContexts kernel)

contextRestrictionIdentity ::
  Eq section =>
  c ->
  RestrictionKernel c obstruction section ->
  Either obstruction Bool
contextRestrictionIdentity contextValue kernel =
  do
    sectionValue <- rkCanonicalSectionAt kernel contextValue
    fmap
      (== Just sectionValue)
      (restrictCanonicalSection (identityContextMorphism contextValue) kernel sectionValue)

contextMorphismLeftIdentity ::
  Eq c =>
  (c -> c -> Either obstruction Bool) ->
  ContextMorphism c ->
  Either obstruction Bool
contextMorphismLeftIdentity refinesTo contextMorphism =
  fmap
    (== Just contextMorphism)
    ( composeContextMorphism
        refinesTo
        (identityContextMorphism (contextMorphismSource contextMorphism))
        contextMorphism
    )

contextMorphismRightIdentity ::
  Eq c =>
  (c -> c -> Either obstruction Bool) ->
  ContextMorphism c ->
  Either obstruction Bool
contextMorphismRightIdentity refinesTo contextMorphism =
  fmap
    (== Just contextMorphism)
    ( composeContextMorphism
        refinesTo
        contextMorphism
        (identityContextMorphism (contextMorphismTarget contextMorphism))
    )

contextMorphismAssociative ::
  Eq c =>
  (c -> c -> Either obstruction Bool) ->
  ContextMorphism c ->
  ContextMorphism c ->
  ContextMorphism c ->
  Either obstruction Bool
contextMorphismAssociative refinesTo firstMorphism secondMorphism thirdMorphism = do
  leftBase <- composeContextMorphism refinesTo firstMorphism secondMorphism
  leftAssociated <-
    bindMaybe
      leftBase
      (\combinedMorphism -> composeContextMorphism refinesTo combinedMorphism thirdMorphism)
  rightBase <- composeContextMorphism refinesTo secondMorphism thirdMorphism
  rightAssociated <- bindMaybe rightBase (composeContextMorphism refinesTo firstMorphism)
  pure (leftAssociated == rightAssociated)

contextRestrictionFunctorialAction ::
  (Eq c, Eq section) =>
  ContextMorphism c ->
  ContextMorphism c ->
  RestrictionKernel c obstruction section ->
  Either obstruction Bool
contextRestrictionFunctorialAction firstMorphism secondMorphism kernel =
  do
        sourceSection <- rkCanonicalSectionAt kernel (contextMorphismSource firstMorphism)
        firstRestriction <- restrictCanonicalSection firstMorphism kernel sourceSection
        sequentialRestriction <-
          bindMaybe
            firstRestriction
            (\sectionValue -> restrictCanonicalSection secondMorphism kernel sectionValue)
        composedMorphism <- composeContextMorphism (rkRefinesTo kernel) firstMorphism secondMorphism
        composedRestriction <-
          bindMaybe
            composedMorphism
            (\contextMorphism -> restrictCanonicalSection contextMorphism kernel sourceSection)
        pure (sequentialRestriction == composedRestriction)

contextGlobalSectionInvariant ::
  Eq section =>
  ContextMorphism c ->
  RestrictionKernel c obstruction section ->
  Either obstruction Bool
contextGlobalSectionInvariant contextMorphism kernel =
  let sourceContext = contextMorphismSource contextMorphism
      targetContext = contextMorphismTarget contextMorphism
   in do
        sourceSection <- rkCanonicalSectionAt kernel sourceContext
        sourceIsGlobal <- isGlobalCanonicalSection sourceContext sourceSection kernel
        restrictedSection <- restrictCanonicalSection contextMorphism kernel sourceSection
        targetIsGlobal <- isGlobalCanonicalSection targetContext sourceSection kernel
        pure
          ( not sourceIsGlobal
              || (restrictedSection == Just sourceSection && targetIsGlobal)
          )

bindMaybe ::
  Maybe value ->
  (value -> Either obstruction (Maybe result)) ->
  Either obstruction (Maybe result)
bindMaybe maybeValue next =
  case maybeValue of
    Nothing ->
      pure Nothing
    Just value ->
      next value
