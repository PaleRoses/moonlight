{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
where

import Data.Kind
  ( Type,
  )

type PropositionKey :: Type -> Type
newtype PropositionKey proposition = PropositionKey
  { unPropositionKey :: proposition
  }
  deriving stock (Eq, Ord, Show, Read)
