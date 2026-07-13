{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.EGraph.Introspection.Core.Context.Tag
  ( TagContext (..),
    TagAwareRewriteSystem (..),
    buildTagIndex,
    expandTagContext,
    mkTagAwareRewriteSystem,
    projectToTagContext,
    rootTag,
    ruleTagContext,
    tagMorphismVisible,
    tagObjectVisible,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    DistributiveLattice,
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import Moonlight.Core (ZipMatch (..), ConstructorTag, HasConstructorTag (..), Pattern (..), PatternVar, patternVarKey)
import Moonlight.Sheaf.Site.Context.GeneratorCover
  ( ContextGeneratorCover (..),
    contextClosure,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( CompositionError,
    RewriteContext,
    RewriteMorphism,
    RewriteSystem,
    RewriteTag,
    mkRewriteSystemFromGenerators,
    rcObjects,
    rsCategory,
  )
import Moonlight.Sheaf.Site
  ( AnalyzableSystem (..),
    InterfaceDirectionEstimate (..),
    InterfaceName,
    MorphismInterface (..),
    allContexts,
    interfaceNameFromString,
  )
import Moonlight.Rewrite.Algebra (frcObjects, frcRewrites)
import Moonlight.Rewrite.System
  ( ldCondition,
  )
import Moonlight.Rewrite.Algebra qualified as KernelCompose
import Moonlight.Rewrite.Algebra
  ( identityPatternRewrite,
    patternInterfaceVariables,
    patternRewriteCreatedVars,
    patternRewriteDeletedVars,
    prDecoration,
    prInterface,
    prLeft,
    prOrigin,
    prRight,
    RewriteOrigin (..),
  )

type TagContext :: (Type -> Type) -> Type
newtype TagContext f = TagContext
  { unTagContext :: Set (ConstructorTag f)
  }

deriving stock instance Eq (ConstructorTag f) => Eq (TagContext f)

deriving stock instance Ord (ConstructorTag f) => Ord (TagContext f)

deriving stock instance Show (ConstructorTag f) => Show (TagContext f)

type TagAwareRewriteSystem :: (Type -> Type) -> Type
data TagAwareRewriteSystem f = TagAwareRewriteSystem
  { tarsInner :: !(RewriteSystem f),
    tarsTagIndex :: !(Map (ConstructorTag f) [Pattern f])
  }

instance Ord (ConstructorTag f) => JoinSemilattice (TagContext f) where
  join leftContext rightContext =
    TagContext (Set.union (unTagContext leftContext) (unTagContext rightContext))

instance Ord (ConstructorTag f) => MeetSemilattice (TagContext f) where
  meet leftContext rightContext =
    TagContext (Set.intersection (unTagContext leftContext) (unTagContext rightContext))

instance Ord (ConstructorTag f) => Lattice (TagContext f)

instance Ord (ConstructorTag f) => DistributiveLattice (TagContext f)

instance Ord (ConstructorTag f) => BoundedJoinSemilattice (TagContext f) where
  bottom =
    TagContext Set.empty

instance (HasConstructorTag f, ZipMatch f, Ord (ConstructorTag f), Ord (Pattern f)) => AnalyzableSystem (TagAwareRewriteSystem f) where
  type SystemTag (TagAwareRewriteSystem f) = RewriteTag f
  type SystemOb (TagAwareRewriteSystem f) = Pattern f
  type SystemMor (TagAwareRewriteSystem f) = RewriteMorphism f
  type SystemCtx (TagAwareRewriteSystem f) = TagContext f
  type SystemMismatch (TagAwareRewriteSystem f) = CompositionError f

  allContexts systemValue =
    contextClosure systemValue

  contextLeq _ smallerContext largerContext =
    unTagContext smallerContext `Set.isSubsetOf` unTagContext largerContext

  systemObjectsInContext systemValue contextValue =
    unTagContext contextValue
      & Set.toAscList
      & foldMap (\tagValue -> Map.findWithDefault [] tagValue (tarsTagIndex systemValue))
      & nubOrd

  systemMorphismsInContext systemValue contextValue =
    filter
      (tagMorphismVisible contextValue)
      (frcRewrites (rsCategory (tarsInner systemValue)))

  restrictObject systemValue sourceContext targetContext objectValue =
    if contextLeq systemValue targetContext sourceContext
        && tagObjectVisible sourceContext objectValue
        && tagObjectVisible targetContext objectValue
      then Just objectValue
      else Nothing

  restrictMorphism systemValue sourceContext targetContext morphismValue =
    if contextLeq systemValue targetContext sourceContext
        && tagMorphismVisible sourceContext morphismValue
        && tagMorphismVisible targetContext morphismValue
      then Just morphismValue
      else Nothing

  identityMorphism _ _ =
    identityPatternRewrite

  morphismSource _ =
    prLeft

  morphismTarget _ =
    prRight

  composeMorphisms _ _ leftSpan rightSpan =
    KernelCompose.crRewrite <$> KernelCompose.composePatternRewrites rightSpan leftSpan

  morphismInterface _ spanValue =
    MorphismInterface
      { miBoundNames = renderVars (patternInterfaceVariables (prInterface spanValue)),
        miDeletedNames = renderVars (patternRewriteDeletedVars spanValue),
        miCreatedNames = renderVars (patternRewriteCreatedVars spanValue),
        miGuarded = maybe False (const True) (ldCondition (prDecoration spanValue)),
        miDirectionEstimate =
          InterfaceDirectionEstimate
            ( Set.size (patternInterfaceVariables (prInterface spanValue))
                + if maybe False (const True) (ldCondition (prDecoration spanValue))
                  then 0
                  else 1
            )
      }

  normalizeMorphism _ _ spanValue =
    spanValue {prOrigin = RewriteIdentity}

instance (HasConstructorTag f, ZipMatch f, Ord (ConstructorTag f), Ord (Pattern f)) => ContextGeneratorCover (TagAwareRewriteSystem f) where
  contextGenerators systemValue =
    frcRewrites (rsCategory (tarsInner systemValue))
      & fmap ruleTagContext

  contextIsBottom _ contextValue =
    Set.null (unTagContext contextValue)

mkTagAwareRewriteSystem ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  [RewriteMorphism f] ->
  TagAwareRewriteSystem f
mkTagAwareRewriteSystem spanValues =
  let innerSystem = mkRewriteSystemFromGenerators spanValues
   in TagAwareRewriteSystem
        { tarsInner = innerSystem,
          tarsTagIndex = buildTagIndex (frcObjects (rsCategory innerSystem))
        }

buildTagIndex ::
  HasConstructorTag f =>
  [Pattern f] ->
  Map (ConstructorTag f) [Pattern f]
buildTagIndex objectValues =
  objectValues
    & mapMaybe (\objectValue -> fmap (\tagValue -> (tagValue, [objectValue])) (rootTag objectValue))
    & Map.fromListWith (<>)

projectToTagContext :: HasConstructorTag f => RewriteContext f -> TagContext f
projectToTagContext contextValue =
  TagContext
    ( rcObjects contextValue
        & mapMaybe rootTag
        & Set.fromList
    )

expandTagContext ::
  (HasConstructorTag f, ZipMatch f) =>
  TagAwareRewriteSystem f ->
  TagContext f ->
  [RewriteContext f]
expandTagContext systemValue tagContext =
  allContexts (tarsInner systemValue)
    & filter (\contextValue -> all (tagObjectVisible tagContext) (rcObjects contextValue))

rootTag :: HasConstructorTag f => Pattern f -> Maybe (ConstructorTag f)
rootTag patternValue =
  case patternValue of
    PatternNode patternNode -> Just (constructorTag patternNode)
    PatternVar {} -> Nothing

ruleTagContext :: HasConstructorTag f => RewriteMorphism f -> TagContext f
ruleTagContext spanValue =
  TagContext
    ( mapMaybe rootTag [prLeft spanValue, prRight spanValue]
        & Set.fromList
    )

tagMorphismVisible :: HasConstructorTag f => TagContext f -> RewriteMorphism f -> Bool
tagMorphismVisible contextValue spanValue =
  tagObjectVisible contextValue (prLeft spanValue)
    && tagObjectVisible contextValue (prRight spanValue)

tagObjectVisible :: HasConstructorTag f => TagContext f -> Pattern f -> Bool
tagObjectVisible contextValue objectValue =
  case rootTag objectValue of
    Just tagValue ->
      tagValue `Set.member` unTagContext contextValue
    Nothing ->
      True

renderVars :: Set PatternVar -> Set (InterfaceName tag)
renderVars =
  Set.map (interfaceNameFromString . show . patternVarKey)
