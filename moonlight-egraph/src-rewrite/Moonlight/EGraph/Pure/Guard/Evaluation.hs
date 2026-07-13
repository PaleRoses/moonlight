{-# LANGUAGE StandaloneKindSignatures #-}

-- | Guard-term resolution over an abstract graph view: the three quotient
-- capabilities a guard consumes (canonicalization, least-node lookup, child
-- projection), so evaluation runs identically against a materialized graph
-- or any virtual view supplying the record.
module Moonlight.EGraph.Pure.Guard.Evaluation
  ( GuardGraphView (..),
    graphGuardView,
    resolveGuardRefWith,
    resolveGuardTermWith,
    projectGuardChild,
    applyGuardPathWith,
    canonicalizeGuardEvidence,
    ufCanonical,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core (Language, safeIndexNatural)
import Moonlight.EGraph.Pure.Kernel.HashCons (lookupLeastENode)
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphClassNodes,
  )
import Moonlight.Core (UnionFind)
import Moonlight.Core qualified as UnionFind
import Moonlight.Rewrite.System
  ( GuardBase (..),
    GuardChildIndex,
    GuardEvidence (..),
    GuardPath (..),
    GuardRef (..),
    GuardTerm (..),
    guardChildIndexValue,
  )
import Moonlight.Rewrite.System qualified as Rewrite
import Moonlight.Core
  ( Substitution,
    lookupSubst
  )

type GuardGraphView :: (Type -> Type) -> Type
data GuardGraphView f = GuardGraphView
  { ggvCanonicalize :: !(ClassId -> ClassId),
    ggvLookupLeastENode :: !(ENode f -> Maybe ClassId),
    ggvChildAt :: !(ClassId -> Natural -> Maybe ClassId)
  }

graphGuardView :: Language f => EGraph f a -> GuardGraphView f
graphGuardView graph =
  GuardGraphView
    { ggvCanonicalize = canonicalizeClassId graph,
      ggvLookupLeastENode =
        \node -> canonicalizeClassId graph <$> lookupLeastENode node graph,
      ggvChildAt =
        \classId childIndex ->
          uniqueCanonicalChild
            ( Set.map
                (canonicalizeClassId graph)
                ( Set.fromList
                    ( mapMaybe
                        (\(ENode childClassIds) -> safeIndexNatural childIndex (toList childClassIds))
                        (Set.toAscList (eGraphClassNodes graph (canonicalizeClassId graph classId)))
                    )
                )
            )
    }
  where
    uniqueCanonicalChild :: Set.Set ClassId -> Maybe ClassId
    uniqueCanonicalChild canonicalChildren =
      case Set.minView canonicalChildren of
        Just (childClassId, rest) | Set.null rest ->
          Just childClassId
        _ ->
          Nothing

ufCanonical :: UnionFind -> ClassId -> ClassId
ufCanonical unionFind =
  fst . flip UnionFind.find unionFind

resolveGuardRefWith ::
  GuardGraphView f ->
  ClassId ->
  Substitution ->
  GuardRef ->
  Maybe ClassId
resolveGuardRefWith view rootClassId substitution (GuardRef (guardBase, guardPath)) = do
  baseClassId <-
    case guardBase of
      GuardFromRoot ->
        Just (ggvCanonicalize view rootClassId)
      GuardFromVar patternVar ->
        ggvCanonicalize view <$> lookupSubst patternVar substitution
  applyGuardPathWith view baseClassId guardPath

resolveGuardTermWith ::
  Traversable f =>
  GuardGraphView f ->
  ClassId ->
  Substitution ->
  GuardTerm f ->
  Maybe ClassId
resolveGuardTermWith view rootClassId substitution = resolveTerm
  where
    resolveTerm =
      \case
        GuardRefTerm guardRef ->
          resolveGuardRefWith view rootClassId substitution guardRef
        GuardProjectTerm baseTerm childIndex ->
          resolveTerm baseTerm
            >>= \baseClassId -> projectGuardChild view baseClassId childIndex
        GuardNodeTerm guardNode -> do
          childClassIds <- traverse resolveTerm guardNode
          ggvLookupLeastENode view (ENode (fmap (ggvCanonicalize view) childClassIds))

canonicalizeGuardEvidence :: UnionFind -> GuardEvidence -> GuardEvidence
canonicalizeGuardEvidence unionFind =
  Rewrite.canonicalizeGuardEvidence (ufCanonical unionFind)

projectGuardChild :: GuardGraphView f -> ClassId -> GuardChildIndex -> Maybe ClassId
projectGuardChild view classId childIndex =
  ggvChildAt view classId (fromIntegral (guardChildIndexValue childIndex))

applyGuardPathWith :: GuardGraphView f -> ClassId -> GuardPath -> Maybe ClassId
applyGuardPathWith view initialClassId (GuardPath childIndices) =
  foldM (projectGuardChild view) initialClassId childIndices
