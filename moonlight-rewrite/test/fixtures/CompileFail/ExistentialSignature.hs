{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module ExistentialSignature where

import GHC.TypeLits (Symbol)
import Moonlight.Rewrite (deriveRewriteSignature)

data ExistentialSig (result :: Symbol) r where
  Existential :: forall payload recursion. payload -> ExistentialSig "Expr" recursion

$(deriveRewriteSignature ''ExistentialSig)
