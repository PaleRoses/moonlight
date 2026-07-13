{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Common
  ( BenchSig (..),
    benchSizes,
    caseLabel,
    benchProgram,
    benchTerms,
    nestedTerm,
    benchCost,
    benchFixTerm,
    benchVariantFixTerm,
    expectBench,
    expectMaybeBench,
    boolWeight,
    eitherWeight,
    leaf,
    wrap,
    pair,
    flag,
  )
where

import Data.Fix (Fix (..))
import GHC.TypeLits (Symbol)
import Moonlight.Rewrite
  ( Cost (..),
    HTraversable (..),
    HostTerm (..),
    K (..),
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
import Moonlight.Rewrite.DSL (Node (..))

data BenchSig (result :: Symbol) r where
  Leaf :: Int -> BenchSig "Expr" r
  Wrap :: r "Expr" -> BenchSig "Expr" r
  Pair :: r "Expr" -> r "Expr" -> BenchSig "Expr" r
  Flag :: Int -> BenchSig "Flag" r

$(deriveRewriteSignature ''BenchSig)

benchSizes :: [Int]
benchSizes =
  [4, 16, 48]

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> "/" <> show size

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

benchCost :: Cost BenchSig Int
benchCost =
  Cost $ \case
    Leaf _ ->
      1
    Wrap (K childCost) ->
      childCost + 1
    Pair (K leftCost) (K rightCost) ->
      leftCost + rightCost + 1
    Flag _ ->
      1

benchFixTerm :: Int -> Fix (Node BenchSig)
benchFixTerm size =
  foldr
    (\key accumulated -> fixPair (fixWrap accumulated) (fixLeaf key))
    (fixLeaf 0)
    [1 .. max 0 size]

benchVariantFixTerm :: Int -> Fix (Node BenchSig)
benchVariantFixTerm size =
  foldr
    (\key accumulated -> fixPair (fixWrap accumulated) (fixLeaf (key + 1)))
    (fixLeaf 1)
    [1 .. max 0 size]

fixLeaf :: Int -> Fix (Node BenchSig)
fixLeaf key =
  Fix (Node (Leaf key))

fixWrap :: Fix (Node BenchSig) -> Fix (Node BenchSig)
fixWrap child =
  Fix (Node (Wrap (K child)))

fixPair :: Fix (Node BenchSig) -> Fix (Node BenchSig) -> Fix (Node BenchSig)
fixPair leftChild rightChild =
  Fix (Node (Pair (K leftChild) (K rightChild)))

expectBench :: Show errorValue => String -> Either errorValue value -> IO value
expectBench label =
  either (\errorValue -> fail (label <> " failed: " <> show errorValue)) pure

expectMaybeBench :: String -> Maybe value -> IO value
expectMaybeBench label =
  maybe (fail (label <> " failed")) pure

boolWeight :: Bool -> Maybe Int
boolWeight observed =
  if observed then Just 1 else Nothing

eitherWeight :: (value -> Int) -> Either errorValue value -> Maybe Int
eitherWeight weigh =
  either (const Nothing) (Just . weigh)
