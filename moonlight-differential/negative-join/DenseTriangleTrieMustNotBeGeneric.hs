{-# LANGUAGE TypeApplications #-}

-- This fixture must fail to compile.  A Generic instance would reconstruct
-- the sealed trie through 'to', bypassing the checked dense-domain builder
-- that protects every unsafe index in the triangle kernel.
module DenseTriangleTrieMustNotBeGeneric where

import GHC.Generics
  ( Generic,
    Rep,
    from,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( DenseTriangleTrie,
  )

genericRepresentation :: DenseTriangleTrie -> Rep DenseTriangleTrie ()
genericRepresentation =
  from @DenseTriangleTrie
