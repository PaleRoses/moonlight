-- | Simplicial stratum for finite rewrite categories.
-- It owns the rewrite-nerve alias and projections into the generic nerve
-- construction; simplex interpretation is kept in "Moonlight.Rewrite.Algebra.Nerve.Simplex".
module Moonlight.Rewrite.Algebra.Nerve.Core
  ( RewriteNerve,
    rewriteNerve,
    rewriteNerveKan,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Moonlight.Core
  ( HasConstructorTag,
    ZipMatch,
  )
import Moonlight.Rewrite.Algebra.Category.Finite (FiniteRewriteCategory)
import Moonlight.Rewrite.Kernel.Decoration
  ( RewriteDecoration (..),
  )
import Moonlight.Category.Simplicial (Nerve, NerveSimplex, nerve, nerveInnerKan)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet)

type RewriteNerve :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
type RewriteNerve atom dec f = Nerve (FiniteRewriteCategory atom dec f)

rewriteNerve ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f,
    Ord atom,
    Ord (dec f)
  ) =>
  FiniteRewriteCategory atom dec f ->
  Natural ->
  TruncatedNormalizedSSet (NerveSimplex (FiniteRewriteCategory atom dec f))
rewriteNerve =
  nerve

rewriteNerveKan ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f,
    Ord atom,
    Ord (dec f)
  ) =>
  FiniteRewriteCategory atom dec f ->
  Natural ->
  RewriteNerve atom dec f
rewriteNerveKan =
  nerveInnerKan
