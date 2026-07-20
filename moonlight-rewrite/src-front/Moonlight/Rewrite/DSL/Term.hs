{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Rewrite.DSL.Term
  ( SortName,
    sortName,
    sortNameString,
    SymbolToken,
    symbolToken,
    symbolTokenText,
    TypedVar,
    SomeTypedVar (..),
    typedVar,
    typedVarName,
    typedVarSort,
    someTypedVarName,
    someTypedVarSort,
    Term (..),
    var,
    node,
  )
where

import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Moonlight.Rewrite.System
  ( SortName,
    sortNameFromString,
    sortNameString,
  )

data SymbolToken (name :: Symbol) = SymbolToken

symbolToken :: SymbolToken name
symbolToken =
  SymbolToken

symbolTokenText :: forall name. KnownSymbol name => SymbolToken name -> String
symbolTokenText _ =
  symbolVal (Proxy @name)

sortName :: forall sort. KnownSymbol sort => SymbolToken sort -> SortName
sortName =
  sortNameFromString . symbolTokenText

data TypedVar (sort :: Symbol) = TypedVar
  { tvName :: !String,
    tvSort :: !SortName
  }
  deriving stock (Show, Read)

instance Eq (TypedVar sort) where
  left == right =
    typedVarName left == typedVarName right
      && typedVarSort left == typedVarSort right

instance Ord (TypedVar sort) where
  compare left right =
    compare (typedVarName left, typedVarSort left) (typedVarName right, typedVarSort right)

data SomeTypedVar where
  SomeTypedVar :: !(TypedVar sort) -> SomeTypedVar

instance Eq SomeTypedVar where
  left == right =
    compare left right == EQ

instance Ord SomeTypedVar where
  compare left right =
    compare (someTypedVarName left, someTypedVarSort left) (someTypedVarName right, someTypedVarSort right)

instance Show SomeTypedVar where
  showsPrec precedence someTypedVariable =
    showParen (precedence > applicationPrecedence)
      ( showString "SomeTypedVar "
          . showsPrec (applicationPrecedence + 1) (someTypedVarName someTypedVariable)
          . showString " "
          . showsPrec (applicationPrecedence + 1) (someTypedVarSort someTypedVariable)
      )
    where
      applicationPrecedence = 10

typedVar ::
  (KnownSymbol name, KnownSymbol sort) =>
  SymbolToken name ->
  SymbolToken sort ->
  TypedVar sort
typedVar nameToken sortToken =
  TypedVar
    { tvName = symbolTokenText nameToken,
      tvSort = sortName sortToken
    }

typedVarName :: TypedVar sort -> String
typedVarName =
  tvName

typedVarSort :: TypedVar sort -> SortName
typedVarSort =
  tvSort

someTypedVarName :: SomeTypedVar -> String
someTypedVarName (SomeTypedVar typedVariable) =
  typedVarName typedVariable

someTypedVarSort :: SomeTypedVar -> SortName
someTypedVarSort (SomeTypedVar typedVariable) =
  typedVarSort typedVariable

data Term sig (sort :: Symbol) where
  TVar ::
    !(TypedVar sort) ->
    Term sig sort
  TNode ::
    !(sig sort (Term sig)) ->
    Term sig sort

var ::
  (KnownSymbol name, KnownSymbol sort) =>
  SymbolToken name ->
  SymbolToken sort ->
  Term sig sort
var nameToken sortToken =
  TVar (typedVar nameToken sortToken)

node :: sig sort (Term sig) -> Term sig sort
node =
  TNode

instance (KnownSymbol name, KnownSymbol sort) => IsLabel name (Term sig sort) where
  fromLabel =
    TVar
      ( TypedVar
          { tvName = symbolVal (Proxy @name),
            tvSort = sortName (symbolToken @sort)
          }
      )
