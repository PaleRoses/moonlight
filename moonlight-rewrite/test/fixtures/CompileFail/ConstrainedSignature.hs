{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module ConstrainedSignature where

import GHC.TypeLits (Symbol)
import Moonlight.Rewrite (deriveRewriteSignature)

data ConstrainedSig (result :: Symbol) r where
  Constrained :: forall payload. Show payload => payload -> ConstrainedSig "Expr" r

$(deriveRewriteSignature ''ConstrainedSig)
