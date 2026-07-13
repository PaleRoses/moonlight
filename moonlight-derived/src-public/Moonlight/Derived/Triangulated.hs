-- |
-- The triangulated notation: chain maps, translation, cones, distinguished
-- triangles, truncation, and internal Hom over the derived category of a
-- finite poset.
--
-- This role-named module is the canonical triangulated surface. Import it
-- directly:
--
-- > import Moonlight.Derived.Triangulated
-- >
-- > example poset f a b = do
-- >   let (⟦_⟧) n x = shift n x
-- >   c   <- cone f
-- >   tri <- mkTriangleOf f
-- >   ok  <- quasiIsoTo f
-- >   pure (c, triH tri, stupidTruncateBelow 0 a, ok)
--
-- == Contracts
--
-- * A 'DerivedMap' in hand witnesses commuting squares against the sealed
--   endpoints ('mkDerivedMapChecked' is the sole gate).
-- * 'shift' and the brutal truncations are total: they preserve every seal
--   invariant structurally, so they ride the trusted path with a proof
--   obligation discharged in the carrier's haddock.
-- * The canonical truncations @τ≤n@ ('canonicalTruncateAtMost') and @τ≥n@
--   ('canonicalTruncateAtLeast') preserve cohomology on their half-window
--   and vanish off it; @τ≤@ is built from the production resolution engine
--   and @τ≥@ as the cone of @ι : τ≤n−1 A → A@. Both re-enter through the
--   checked minimizing gates. 'canonicalTruncationPair' returns the
--   decomposition @(τ≤n A, τ≥n+1 A)@ — the two halves of the truncation
--   triangle — from one shared skeleton, and agrees with the individual
--   truncations exactly; prefer it whenever both halves are wanted. They require sheaf-lawful input — differential
--   supports respecting the site order; that law is owned by the public
--   carrier. The brutal 'stupidTruncate' pair keeps its honest σ-names.
-- * 'cone' returns the /minimal normalized/ cone; 'mkTriangleOf' carries the
--   homotopy equivalence of that minimization through to honest connecting
--   maps @g : B → cone f@ and @h : cone f → A⟦1⟧@, both re-validated by the
--   carrier gate.
-- * @f@ is a quasi-isomorphism iff @cone f@ is acyclic — 'quasiIsoTo' is
--   definitionally that criterion.
-- * 'internalHom' is @𝔻(A ⊗ 𝔻B)@ and is defined only over 'GF2', because Verdier
--   duality is presently characteristic-two; the tensor-hom adjunction law
--   is stated at GF2 and at invariant level (hypercohomology dimensions),
--   never beyond what is actually proved.
--
-- The classical laws (@𝔻𝔻 ≅ id@, rotation invariance, Euler additivity
-- @χ(cone f) = χ(B) − χ(A)@) are isomorphism-level statements; structural
-- equality of minimal representatives is not promised. The laws bundle
-- states them as equalities of hypercohomology dimensions and microsupport.
module Moonlight.Derived.Triangulated
  ( DerivedMap
  , mkDerivedMapChecked
  , derivedMapSource
  , derivedMapTarget
  , derivedMapComponents
  , derivedMapComponentAt
  , identityMap
  , zeroMap
  , shift
  , zeroDerived
  , derivedObjectWindow
  , stupidTruncateBelow
  , stupidTruncateAbove
  , canonicalTruncateAtMost
  , canonicalTruncateAtLeast
  , canonicalTruncationPair
  , cone
  , Triangle
  , triA
  , triB
  , triC
  , triF
  , triG
  , triH
  , mkTriangleOf
  , rotateTriangle
  , quasiIsoTo
  , internalHom
  ) where

import Moonlight.Core (Field, MoonlightError)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)
import Moonlight.Derived.Pure.Functor.Tensor (internalHom)
import Moonlight.Derived.Pure.Gluing.Cone
  ( Triangle
  , cone
  , mkTriangleOf
  , rotateTriangle
  , triA
  , triB
  , triC
  , triF
  , triG
  , triH
  )
import Moonlight.Derived.Pure.Gluing.Truncation
  ( canonicalTruncateAtLeast
  , canonicalTruncateAtMost
  , canonicalTruncationPair
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology (hypercohomologyVanishes)
import Moonlight.Derived.Pure.Site.DerivedMap
  ( DerivedMap
  , derivedMapComponentAt
  , derivedMapComponents
  , derivedMapSource
  , derivedMapTarget
  , derivedObjectWindow
  , identityMap
  , mkDerivedMapChecked
  , shift
  , stupidTruncateAbove
  , stupidTruncateBelow
  , zeroDerived
  , zeroMap
  )

-- | The cone criterion for quasi-isomorphism: @f@ is a quasi-isomorphism iff
-- its cone is acyclic.
quasiIsoTo ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  DerivedMap a -> Either MoonlightError Bool
quasiIsoTo mapValue =
  cone mapValue >>= hypercohomologyVanishes
{-# INLINE quasiIsoTo #-}
