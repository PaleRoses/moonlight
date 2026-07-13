{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.EGraph.Pure.Saturation.Front.Binding
  ( BindingRootName,
    BindingPath,
    BindingPathSegment,
    bindingPathName,
    bindingPathSegments,
    bindingPathSegmentName,
    bindingPathSingletonNamed,
    bindingPathChild,
    bindingPathChildNamed,
    bindingPathFromSegmentsNamed,
    BindingChild (..),
    bindingChild,
    BindingFact (..),
    BindingFactArgs (..),
    BindingFactArg (..),
    BindingIngestError (..),
    BindingPlan,
    BindingPlanEntry (..),
    BindingIngestion (..),
    bindingIngestionTermAt,
    bindingIngestionScopeAt,
    bindingPlanFromEntries,
    augmentBindingPlanFacts,
    appendBindingPlanEntries,
    bindingPlanRootPath,
    bindingPlanEntries,
    bindingPlanPaths,
    bindingPlanContexts,
    emitBindingPlan,
  )
where

import Control.Monad (foldM)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.TypeLits (Symbol)
import Moonlight.EGraph.Pure.Saturation.Front
  ( ContextRef,
    EGraphFrontM,
    FactRefArgs (..),
    RelationRef,
    Term,
    TermRef,
    contextNamed,
    defAtNamed,
    factArgs,
  )
import Data.Fix (Fix)

-- | Human root for generated names, for example @"lambda"@.
type BindingRootName = String

newtype BindingPathSegment = BindingPathSegment
  { unBindingPathSegment :: String
  }
  deriving stock (Eq, Ord, Show)

newtype BindingPath = BindingPath
  { unBindingPath :: NonEmpty BindingPathSegment
  }
  deriving stock (Eq, Ord, Show)

bindingPathName :: BindingPath -> String
bindingPathName =
  intercalate "/" . NonEmpty.toList . bindingPathSegments
{-# INLINE bindingPathName #-}

bindingPathSegments :: BindingPath -> NonEmpty String
bindingPathSegments =
  fmap bindingPathSegmentName . unBindingPath
{-# INLINE bindingPathSegments #-}

bindingPathSegmentName :: BindingPathSegment -> String
bindingPathSegmentName =
  unBindingPathSegment
{-# INLINE bindingPathSegmentName #-}

bindingPathSingletonNamed :: String -> Either BindingIngestError BindingPath
bindingPathSingletonNamed rawName =
  BindingPath . (:| []) <$> bindingPathSegmentNamed rawName
{-# INLINE bindingPathSingletonNamed #-}

bindingPathChild :: BindingPath -> BindingPathSegment -> BindingPath
bindingPathChild (BindingPath segments) segment =
  BindingPath (segments <> (segment :| []))
{-# INLINE bindingPathChild #-}

bindingPathChildNamed :: BindingPath -> String -> Either BindingIngestError BindingPath
bindingPathChildNamed parentPath rawName =
  bindingPathChild parentPath <$> bindingPathSegmentNamed rawName
{-# INLINE bindingPathChildNamed #-}

bindingPathSegmentNamed :: String -> Either BindingIngestError BindingPathSegment
bindingPathSegmentNamed rawName
  | null rawName =
      Left BindingEmptyPathSegment
  | any (== '/') rawName =
      Left (BindingPathSegmentContainsSlash rawName)
  | otherwise =
      Right (BindingPathSegment rawName)
{-# INLINE bindingPathSegmentNamed #-}

type BindingChild :: (Type -> Type) -> Type -> Type -> Type
data BindingChild f context scope = BindingChild
  { bcSegment :: !BindingPathSegment,
    bcContext :: !context,
    bcScope :: !scope,
    bcTerm :: !(Fix f)
  }

bindingChild ::
  String ->
  context ->
  scope ->
  Fix f ->
  Either BindingIngestError (BindingChild f context scope)
bindingChild rawSegment contextValue scopeValue termValue =
  fmap
    ( \segment ->
        BindingChild
          { bcSegment = segment,
            bcContext = contextValue,
            bcScope = scopeValue,
            bcTerm = termValue
          }
    )
    (bindingPathSegmentNamed rawSegment)
{-# INLINE bindingChild #-}

type BindingFact :: (Symbol -> (Symbol -> Type) -> Type) -> Type
data BindingFact sig where
  BindingFact ::
    RelationRef sorts ->
    BindingFactArgs sig sorts ->
    BindingFact sig

type BindingFactArgs :: (Symbol -> (Symbol -> Type) -> Type) -> [Symbol] -> Type
data BindingFactArgs sig sorts where
  BindingFactNil :: BindingFactArgs sig '[]
  BindingFactCons ::
    BindingFactArg sig sort ->
    BindingFactArgs sig sorts ->
    BindingFactArgs sig (sort ': sorts)

infixr 5 `BindingFactCons`

type BindingFactArg :: (Symbol -> (Symbol -> Type) -> Type) -> Symbol -> Type
data BindingFactArg sig sort where
  BindingHere :: BindingFactArg sig "Expr"
  BindingAt :: BindingPath -> BindingFactArg sig "Expr"
  BindingExistingRef :: TermRef sig sort -> BindingFactArg sig sort

data BindingIngestError
  = BindingEmptyPathSegment
  | BindingPathSegmentContainsSlash !String
  | BindingDuplicatePath !BindingPath
  | BindingUnknownFactPath !BindingPath !BindingPath
  | BindingMissingRootPath !BindingPath
  | BindingMissingContextForPath !BindingPath
  | BindingMissingTermForPath !BindingPath
  | BindingMissingScopeForPath !BindingPath
  deriving stock (Eq, Ord, Show)

data BindingPlan sig context = BindingPlan
  !BindingPath
  ![BindingPlanEntry sig context]

data BindingPlanEntry sig context = BindingPlanEntry
  { bpePath :: !BindingPath,
    bpeContext :: !context,
    bpeTerm :: !(Term sig "Expr"),
    bpeFacts :: ![BindingFact sig]
  }

type BindingIngestion :: (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type
data BindingIngestion sig context = BindingIngestion
  { biRootTerm :: !(TermRef sig "Expr"),
    biPathTerms :: !(Map BindingPath (TermRef sig "Expr")),
    biPathScopes :: !(Map BindingPath (ContextRef context))
  }

bindingPathFromSegmentsNamed :: String -> [String] -> Either BindingIngestError BindingPath
bindingPathFromSegmentsNamed rawRootName rawChildNames = do
  rootPath <- bindingPathSingletonNamed rawRootName
  foldM bindingPathChildNamed rootPath rawChildNames
{-# INLINE bindingPathFromSegmentsNamed #-}

bindingIngestionTermAt ::
  BindingPath ->
  BindingIngestion sig context ->
  Either BindingIngestError (TermRef sig "Expr")
bindingIngestionTermAt path ingestion =
  maybe
    (Left (BindingMissingTermForPath path))
    Right
    (Map.lookup path (biPathTerms ingestion))
{-# INLINE bindingIngestionTermAt #-}

bindingIngestionScopeAt ::
  BindingPath ->
  BindingIngestion sig context ->
  Either BindingIngestError (ContextRef context)
bindingIngestionScopeAt path ingestion =
  maybe
    (Left (BindingMissingScopeForPath path))
    Right
    (Map.lookup path (biPathScopes ingestion))
{-# INLINE bindingIngestionScopeAt #-}

bindingPlanFromEntries ::
  BindingPath ->
  [BindingPlanEntry sig context] ->
  Either BindingIngestError (BindingPlan sig context)
bindingPlanFromEntries rootPath entries = do
  validateBindingRootPath rootPath entries
  validateBindingEntryPaths entries
  validateBindingFactPaths entries
  Right (BindingPlan rootPath entries)

augmentBindingPlanFacts ::
  (BindingPlanEntry sig context -> [BindingFact sig]) ->
  BindingPlan sig context ->
  Either BindingIngestError (BindingPlan sig context)
augmentBindingPlanFacts makeFacts (BindingPlan rootPath entries) =
  bindingPlanFromEntries rootPath (fmap augmentEntry entries)
  where
    augmentEntry entry =
      entry
        { bpeFacts = bpeFacts entry <> makeFacts entry
        }
{-# INLINE augmentBindingPlanFacts #-}

appendBindingPlanEntries ::
  [BindingPlanEntry sig context] ->
  BindingPlan sig context ->
  Either BindingIngestError (BindingPlan sig context)
appendBindingPlanEntries extraEntries (BindingPlan rootPath entries) =
  bindingPlanFromEntries rootPath (entries <> extraEntries)
{-# INLINE appendBindingPlanEntries #-}

validateBindingRootPath ::
  BindingPath ->
  [BindingPlanEntry sig context] ->
  Either BindingIngestError ()
validateBindingRootPath rootPath entries =
  if any ((== rootPath) . bpePath) entries
    then Right ()
    else Left (BindingMissingRootPath rootPath)

validateBindingEntryPaths ::
  [BindingPlanEntry sig context] ->
  Either BindingIngestError ()
validateBindingEntryPaths =
  fmap (const ()) . foldM insertPath Set.empty
  where
    insertPath ::
      Set BindingPath ->
      BindingPlanEntry sig context ->
      Either BindingIngestError (Set BindingPath)
    insertPath seen entry
      | Set.member (bpePath entry) seen =
          Left (BindingDuplicatePath (bpePath entry))
      | otherwise =
          Right (Set.insert (bpePath entry) seen)

bindingPlanRootPath :: BindingPlan sig context -> BindingPath
bindingPlanRootPath (BindingPlan rootPath _entries) =
  rootPath
{-# INLINE bindingPlanRootPath #-}

bindingPlanEntries :: BindingPlan sig context -> [BindingPlanEntry sig context]
bindingPlanEntries (BindingPlan _rootPath entries) =
  entries
{-# INLINE bindingPlanEntries #-}

bindingPlanPaths :: BindingPlan sig context -> [BindingPath]
bindingPlanPaths =
  fmap bpePath . bindingPlanEntries
{-# INLINE bindingPlanPaths #-}

bindingPlanContexts :: Ord context => BindingPlan sig context -> [context]
bindingPlanContexts =
  dedupeOrdered . fmap bpeContext . bindingPlanEntries
{-# INLINE bindingPlanContexts #-}

emitBindingPlan ::
  Ord context =>
  BindingPlan sig context ->
  EGraphFrontM sig analysis context (Either BindingIngestError (BindingIngestion sig context))
emitBindingPlan plan = do
  canonicalContexts <-
    traverse emitCanonicalBindingContext (canonicalBindingContexts (bindingPlanEntries plan))
  let canonicalContextRefs =
        Map.fromList
          [ (ecbcContext canonicalContext, ecbcContextRef canonicalContext)
          | canonicalContext <- canonicalContexts
          ]
  case traverse (emitBindingPlanEntry canonicalContextRefs) (bindingPlanEntries plan) of
    Left ingestError ->
      pure (Left ingestError)
    Right emittedEntryActions -> do
      emittedEntries <-
        sequence emittedEntryActions
      let termRefs =
            Map.fromList
              [ (bpePath (ebePlanEntry emittedEntry), ebeTermRef emittedEntry)
              | emittedEntry <- emittedEntries
              ]
          contextRefs =
            Map.fromList
              [ (bpePath (ebePlanEntry emittedEntry), ebeContextRef emittedEntry)
              | emittedEntry <- emittedEntries
              ]
      case resolveBindingFacts termRefs emittedEntries of
        Left ingestError ->
          pure (Left ingestError)
        Right resolvedFacts -> do
          traverse_ emitResolvedBindingFact resolvedFacts
          pure
            ( BindingIngestion
                <$> Map.lookup (bindingPlanRootPath plan) termRefs
                  `orMissingRoot` bindingPlanRootPath plan
                <*> Right termRefs
                <*> Right contextRefs
            )

data CanonicalBindingContext context = CanonicalBindingContext
  { cbcPath :: !BindingPath,
    cbcContext :: !context
  }

data EmittedCanonicalBindingContext context = EmittedCanonicalBindingContext
  { ecbcContext :: !context,
    ecbcContextRef :: !(ContextRef context)
  }

canonicalBindingContexts ::
  Ord context =>
  [BindingPlanEntry sig context] ->
  [CanonicalBindingContext context]
canonicalBindingContexts =
  reverse
    . fst
    . foldl'
      ( \(contexts, seen) entry ->
          if Set.member (bpeContext entry) seen
            then (contexts, seen)
            else
              ( CanonicalBindingContext (bpePath entry) (bpeContext entry) : contexts,
                Set.insert (bpeContext entry) seen
              )
      )
      ([], Set.empty)

emitCanonicalBindingContext ::
  CanonicalBindingContext context ->
  EGraphFrontM sig analysis context (EmittedCanonicalBindingContext context)
emitCanonicalBindingContext canonicalContext = do
  contextRef <-
    contextNamed (bindingPathName (cbcPath canonicalContext)) (cbcContext canonicalContext)
  pure
    EmittedCanonicalBindingContext
      { ecbcContext = cbcContext canonicalContext,
        ecbcContextRef = contextRef
      }

orMissingRoot :: Maybe value -> BindingPath -> Either BindingIngestError value
orMissingRoot maybeValue rootPath =
  maybe
    (Left (BindingMissingRootPath rootPath))
    Right
    maybeValue

validateBindingFactPaths ::
  [BindingPlanEntry sig context] ->
  Either BindingIngestError ()
validateBindingFactPaths entries =
  traverse_ validateEntry entries
  where
    knownPaths =
      Set.fromList (fmap bpePath entries)

    validateEntry entry =
      traverse_
        (validateFact (bpePath entry))
        (bpeFacts entry)

    validateFact sourcePath factValue =
      traverse_
        (validateFactPath sourcePath)
        (bindingFactReferencedPaths factValue)

    validateFactPath sourcePath targetPath =
      if Set.member targetPath knownPaths
        then Right ()
        else Left (BindingUnknownFactPath sourcePath targetPath)

bindingFactReferencedPaths :: BindingFact sig -> Set BindingPath
bindingFactReferencedPaths (BindingFact _relationRef args) =
  bindingFactArgsReferencedPaths args

bindingFactArgsReferencedPaths :: BindingFactArgs sig sorts -> Set BindingPath
bindingFactArgsReferencedPaths =
  \case
    BindingFactNil ->
      Set.empty
    BindingFactCons arg rest ->
      bindingFactArgReferencedPaths arg <> bindingFactArgsReferencedPaths rest

bindingFactArgReferencedPaths :: BindingFactArg sig sort -> Set BindingPath
bindingFactArgReferencedPaths =
  \case
    BindingHere ->
      Set.empty
    BindingAt path ->
      Set.singleton path
    BindingExistingRef _termRef ->
      Set.empty

data EmittedBindingEntry sig context = EmittedBindingEntry
  { ebePlanEntry :: !(BindingPlanEntry sig context),
    ebeContextRef :: !(ContextRef context),
    ebeTermRef :: !(TermRef sig "Expr")
  }

emitBindingPlanEntry ::
  Ord context =>
  Map context (ContextRef context) ->
  BindingPlanEntry sig context ->
  Either BindingIngestError (EGraphFrontM sig analysis context (EmittedBindingEntry sig context))
emitBindingPlanEntry contextRefs entry = do
  contextRef <-
    maybe
      (Left (BindingMissingContextForPath (bpePath entry)))
      Right
      (Map.lookup (bpeContext entry) contextRefs)
  Right $ do
    let rawName =
          bindingPathName (bpePath entry)
    termRef <-
      defAtNamed rawName contextRef (bpeTerm entry)
    pure
      EmittedBindingEntry
        { ebePlanEntry = entry,
          ebeContextRef = contextRef,
          ebeTermRef = termRef
        }

data ResolvedBindingFact sig where
  ResolvedBindingFact ::
    RelationRef sorts ->
    FactRefArgs sig sorts ->
    ResolvedBindingFact sig

resolveBindingFacts ::
  Map BindingPath (TermRef sig "Expr") ->
  [EmittedBindingEntry sig context] ->
  Either BindingIngestError [ResolvedBindingFact sig]
resolveBindingFacts termRefs emittedEntries =
  fmap concat $
    traverse
      (resolveBindingEntryFacts termRefs)
      emittedEntries

resolveBindingEntryFacts ::
  Map BindingPath (TermRef sig "Expr") ->
  EmittedBindingEntry sig context ->
  Either BindingIngestError [ResolvedBindingFact sig]
resolveBindingEntryFacts termRefs emittedEntry =
  traverse
    (resolveBindingFact termRefs (bpePath (ebePlanEntry emittedEntry)) (ebeTermRef emittedEntry))
    (bpeFacts (ebePlanEntry emittedEntry))

resolveBindingFact ::
  Map BindingPath (TermRef sig "Expr") ->
  BindingPath ->
  TermRef sig "Expr" ->
  BindingFact sig ->
  Either BindingIngestError (ResolvedBindingFact sig)
resolveBindingFact termRefs sourcePath currentTermRef (BindingFact relationRef args) =
  fmap
    (ResolvedBindingFact relationRef)
    (resolveBindingFactArgs termRefs sourcePath currentTermRef args)

resolveBindingFactArgs ::
  Map BindingPath (TermRef sig "Expr") ->
  BindingPath ->
  TermRef sig "Expr" ->
  BindingFactArgs sig sorts ->
  Either BindingIngestError (FactRefArgs sig sorts)
resolveBindingFactArgs termRefs sourcePath currentTermRef =
  \case
    BindingFactNil ->
      Right FactRefNil
    BindingFactCons arg rest -> do
      resolvedArg <-
        resolveBindingFactArg termRefs sourcePath currentTermRef arg
      resolvedRest <-
        resolveBindingFactArgs termRefs sourcePath currentTermRef rest
      Right (resolvedArg :@& resolvedRest)

resolveBindingFactArg ::
  Map BindingPath (TermRef sig "Expr") ->
  BindingPath ->
  TermRef sig "Expr" ->
  BindingFactArg sig sort ->
  Either BindingIngestError (TermRef sig sort)
resolveBindingFactArg termRefs sourcePath currentTermRef =
  \case
    BindingHere ->
      Right currentTermRef
    BindingAt targetPath ->
      maybe
        (Left (BindingUnknownFactPath sourcePath targetPath))
        Right
        (Map.lookup targetPath termRefs)
    BindingExistingRef termRef ->
      Right termRef

emitResolvedBindingFact ::
  ResolvedBindingFact sig ->
  EGraphFrontM sig analysis context ()
emitResolvedBindingFact (ResolvedBindingFact relationRef args) =
  factArgs relationRef args

dedupeOrdered :: Ord value => [value] -> [value]
dedupeOrdered =
  reverse
    . fst
    . foldl'
      ( \(values, seen) value ->
          if Set.member value seen
            then (values, seen)
            else (value : values, Set.insert value seen)
      )
      ([], Set.empty)
