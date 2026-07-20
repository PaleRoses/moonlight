{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Fixture
  ( BenchSig (..),
    benchProgram,
    benchTerms,
    nestedTerm,
    leaf,
    wrap,
    pair,
    flag,
  )
where

import GHC.TypeLits (Symbol)
import Moonlight.Rewrite
  ( HostTerm (..),
    HTraversable (..),
    NoGuardAtom,
    Program,
    RewriteSignature (..),
    Term,
    bind,
    deriveRewriteSignature,
    forall_,
    node,
    program,
    rule,
    symbolToken,
    var,
    (==>),
  )

data BenchSig (result :: Symbol) r where
  Leaf :: Int -> BenchSig "Expr" r
  Wrap :: r "Expr" -> BenchSig "Expr" r
  Pair :: r "Expr" -> r "Expr" -> BenchSig "Expr" r
  Flag :: Int -> BenchSig "Flag" r

$(deriveRewriteSignature ''BenchSig)

benchProgram :: Program BenchSig NoGuardAtom
benchProgram =
  program $ do
    rule
      "unwrap"
      ( forall_
          (bind (symbolToken @"x") (symbolToken @"Expr"))
          (wrap xExpr ==> xExpr)
      )
    rule
      "project-left"
      ( forall_
          ( bind (symbolToken @"x") (symbolToken @"Expr")
              <> bind (symbolToken @"y") (symbolToken @"Expr")
          )
          (pair xExpr yExpr ==> xExpr)
      )

xExpr :: Term BenchSig "Expr"
xExpr =
  var (symbolToken @"x") (symbolToken @"Expr")

yExpr :: Term BenchSig "Expr"
yExpr =
  var (symbolToken @"y") (symbolToken @"Expr")

nestedTerm :: Int -> Term BenchSig "Expr"
nestedTerm size =
  foldr
    (\key accumulated -> pair (wrap accumulated) (leaf key))
    (leaf 0)
    [1 .. max 0 size]

benchTerms :: Int -> [HostTerm BenchSig]
benchTerms size =
  fmap HostTerm $
    leafTerms
      <> fmap wrap leafTerms
      <> zipWith pair leafTerms (fmap wrap leafTerms)
      <> [nestedTerm (max 1 (size `div` 2))]
  where
    leafTerms =
      fmap leaf [0 .. max 0 size]
