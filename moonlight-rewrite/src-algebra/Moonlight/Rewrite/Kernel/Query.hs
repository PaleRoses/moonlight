module Moonlight.Rewrite.Kernel.Query
  ( PatternQuery (..),
    CompiledPatternQuery,
    cpqQuery,
    cpqPrimaryPattern,
    cpqCondition,
    singlePatternQuery,
    compiledSinglePatternQuery,
    conjunctivePatternQuery,
    guardedPatternQuery,
    normalizePatternQuery,
    patternQueryPrimaryPattern,
    patternQueryPatterns,
    patternQueryConditions,
    patternQueryCondition,
    patternQueryVariables,
    compiledPatternQueryVariablesWith,
    mapCompiledPatternQuery,
    compilePatternQuery,
    compilePatternQueryWithScope,
  )
where

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (sconcat)
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    Pattern (..),
    PatternVar,
    patternVariables,
  )

type PatternQuery :: Type -> (Type -> Type) -> Type
data PatternQuery guard f
  = SinglePatternQuery (Pattern f)
  | ConjunctivePatternQuery (NonEmpty (PatternQuery guard f))
  | GuardedPatternQuery (PatternQuery guard f) guard

deriving stock instance (Eq guard, Eq (Pattern f)) => Eq (PatternQuery guard f)
deriving stock instance (Ord guard, Ord (Pattern f)) => Ord (PatternQuery guard f)
deriving stock instance (Show guard, Show (Pattern f)) => Show (PatternQuery guard f)

type CompiledPatternQuery :: Type -> (Type -> Type) -> Type
data CompiledPatternQuery compiledGuard f = CompiledPatternQuery
  !(PatternQuery compiledGuard f)
  !(Pattern f)
  !(Maybe compiledGuard)

deriving stock instance (Eq compiledGuard, Eq (Pattern f)) => Eq (CompiledPatternQuery compiledGuard f)
deriving stock instance (Ord compiledGuard, Ord (Pattern f)) => Ord (CompiledPatternQuery compiledGuard f)
deriving stock instance (Show compiledGuard, Show (Pattern f)) => Show (CompiledPatternQuery compiledGuard f)

cpqQuery :: CompiledPatternQuery compiledGuard f -> PatternQuery compiledGuard f
cpqQuery (CompiledPatternQuery query _primaryPattern _condition) =
  query

cpqPrimaryPattern :: CompiledPatternQuery compiledGuard f -> Pattern f
cpqPrimaryPattern (CompiledPatternQuery _query primaryPattern _condition) =
  primaryPattern

cpqCondition :: CompiledPatternQuery compiledGuard f -> Maybe compiledGuard
cpqCondition (CompiledPatternQuery _query _primaryPattern condition) =
  condition

singlePatternQuery :: Pattern f -> PatternQuery guard f
singlePatternQuery =
  SinglePatternQuery

conjunctivePatternQuery :: NonEmpty (Pattern f) -> PatternQuery guard f
conjunctivePatternQuery =
  ConjunctivePatternQuery . fmap SinglePatternQuery

guardedPatternQuery :: PatternQuery guard f -> guard -> PatternQuery guard f
guardedPatternQuery =
  GuardedPatternQuery

compiledSinglePatternQuery ::
  Pattern f ->
  Maybe compiledGuard ->
  CompiledPatternQuery compiledGuard f
compiledSinglePatternQuery patternValue condition =
  CompiledPatternQuery
    ( maybe
        (singlePatternQuery patternValue)
        (guardedPatternQuery (singlePatternQuery patternValue))
        condition
    )
    patternValue
    condition

normalizePatternQuery :: (Language f, Semigroup guard) => PatternQuery guard f -> PatternQuery guard f
normalizePatternQuery patternQuery =
  let normalizedPatterns =
        canonicalizePatterns (patternQueryPatterns patternQuery)
      baseQuery = patternsQuery normalizedPatterns
   in maybe
        baseQuery
        (guardedPatternQuery baseQuery . sconcat)
        (NonEmpty.nonEmpty (patternQueryConditions patternQuery))

patternQueryPrimaryPattern :: PatternQuery guard f -> Pattern f
patternQueryPrimaryPattern patternQuery =
  case patternQueryPatterns patternQuery of
    primaryPattern :| _ ->
      primaryPattern

patternQueryPatterns :: PatternQuery guard f -> NonEmpty (Pattern f)
patternQueryPatterns patternQuery =
  case patternQuery of
    SinglePatternQuery patternValue ->
      patternValue :| []
    ConjunctivePatternQuery patternQueries ->
      sconcat (fmap patternQueryPatterns patternQueries)
    GuardedPatternQuery nestedQuery _ ->
      patternQueryPatterns nestedQuery

patternQueryConditions :: PatternQuery guard f -> [guard]
patternQueryConditions patternQuery =
  collectConditions patternQuery []
  where
    collectConditions :: PatternQuery condition node -> [condition] -> [condition]
    collectConditions query suffix =
      case query of
        SinglePatternQuery _ ->
          suffix
        ConjunctivePatternQuery patternQueries ->
          foldr collectConditions suffix patternQueries
        GuardedPatternQuery nestedQuery guardCondition ->
          collectConditions nestedQuery (guardCondition : suffix)

patternQueryCondition :: Semigroup guard => PatternQuery guard f -> Maybe guard
patternQueryCondition patternQuery =
  sconcat <$> NonEmpty.nonEmpty (patternQueryConditions patternQuery)

patternQueryVariables ::
  Foldable f =>
  PatternQuery guard f ->
  Set.Set PatternVar
patternQueryVariables =
  foldMap patternVariables . patternQueryPatterns

compiledPatternQueryVariablesWith ::
  Foldable f =>
  (guard -> Set.Set PatternVar) ->
  CompiledPatternQuery guard f ->
  Set.Set PatternVar
compiledPatternQueryVariablesWith guardVariables compiledQuery =
  patternQueryVariables (cpqQuery compiledQuery)
    <> foldMap guardVariables (patternQueryConditions (cpqQuery compiledQuery))

mapPatternQuery ::
  (Pattern f -> Pattern f) ->
  (guard -> guard') ->
  PatternQuery guard f ->
  PatternQuery guard' f
mapPatternQuery mapPatternValue mapGuard =
  \case
    SinglePatternQuery patternValue ->
      SinglePatternQuery (mapPatternValue patternValue)

    ConjunctivePatternQuery queries ->
      ConjunctivePatternQuery (fmap (mapPatternQuery mapPatternValue mapGuard) queries)

    GuardedPatternQuery nestedQuery guardValue ->
      GuardedPatternQuery
        (mapPatternQuery mapPatternValue mapGuard nestedQuery)
        (mapGuard guardValue)

mapCompiledPatternQuery ::
  (Language f, Semigroup guard') =>
  (Pattern f -> Pattern f) ->
  (guard -> guard') ->
  CompiledPatternQuery guard f ->
  CompiledPatternQuery guard' f
mapCompiledPatternQuery mapPatternValue mapGuard compiledQuery =
  canonicalCompiledPatternQuery
    (mapPatternQuery mapPatternValue mapGuard (cpqQuery compiledQuery))

compilePatternQuery ::
  Language f =>
  ([compiledGuard] -> Maybe compiledGuard) ->
  (Set.Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  PatternQuery guard f ->
  Either [PatternVar] (CompiledPatternQuery compiledGuard f)
compilePatternQuery combineCompiledGuards compileGuard = compilePatternQueryWithScope combineCompiledGuards compileGuard Set.empty

compilePatternQueryWithScope ::
  Language f =>
  ([compiledGuard] -> Maybe compiledGuard) ->
  (Set.Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  Set.Set PatternVar ->
  PatternQuery guard f ->
  Either [PatternVar] (CompiledPatternQuery compiledGuard f)
compilePatternQueryWithScope combineCompiledGuards compileGuard extraBoundVariables patternQuery =
  let flattenedPatterns =
        canonicalizePatterns (patternQueryPatterns patternQuery)
      boundVariables = extraBoundVariables <> foldMap patternVariables flattenedPatterns
   in do
        compiledConditions <- traverse (compileGuard boundVariables) (patternQueryConditions patternQuery)
        let compiledCondition = combineCompiledGuards compiledConditions
            baseQuery =
              patternsQuery flattenedPatterns
            compiledQuery =
              maybe
                baseQuery
                (guardedPatternQuery baseQuery)
                compiledCondition
        pure
          ( CompiledPatternQuery
              compiledQuery
              (patternQueryPrimaryPattern baseQuery)
              compiledCondition
          )

canonicalCompiledPatternQuery ::
  (Language f, Semigroup guard) =>
  PatternQuery guard f ->
  CompiledPatternQuery guard f
canonicalCompiledPatternQuery patternQuery =
  let normalizedQuery = normalizePatternQuery patternQuery
   in CompiledPatternQuery
        normalizedQuery
        (patternQueryPrimaryPattern normalizedQuery)
        (patternQueryCondition normalizedQuery)

patternsQuery :: NonEmpty (Pattern f) -> PatternQuery guard f
patternsQuery =
  \case
    patternValue :| [] ->
      singlePatternQuery patternValue
    patternValues ->
      conjunctivePatternQuery patternValues

canonicalizePatterns :: Language f => NonEmpty (Pattern f) -> NonEmpty (Pattern f)
canonicalizePatterns =
  fmap NonEmpty.head
    . NonEmpty.groupBy1 samePattern
    . NonEmpty.sortBy comparePattern

samePattern :: Language f => Pattern f -> Pattern f -> Bool
samePattern leftPattern rightPattern =
  comparePattern leftPattern rightPattern == EQ

comparePattern :: Language f => Pattern f -> Pattern f -> Ordering
comparePattern leftPattern rightPattern =
  case (leftPattern, rightPattern) of
    (PatternNode leftNode, PatternNode rightNode) ->
      compare (void leftNode) (void rightNode)
        <> comparePatternChildren (toList leftNode) (toList rightNode)
    (PatternNode _, PatternVar _) ->
      GT
    (PatternVar _, PatternNode _) ->
      LT
    (PatternVar leftPatternVar, PatternVar rightPatternVar) ->
      compare leftPatternVar rightPatternVar

comparePatternChildren :: Language f => [Pattern f] -> [Pattern f] -> Ordering
comparePatternChildren leftPatterns rightPatterns =
  foldMap (uncurry comparePattern) (zip leftPatterns rightPatterns)
    <> compare (length leftPatterns) (length rightPatterns)
