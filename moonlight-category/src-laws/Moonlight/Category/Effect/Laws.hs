module Moonlight.Category.Effect.Laws
  ( tests,
  )
where

import qualified Moonlight.Category.Effect.Laws.Algebra as Algebra
import qualified Moonlight.Category.Effect.Laws.Adhesive as Adhesive
import qualified Moonlight.Category.Effect.Laws.Category as Category
import qualified Moonlight.Category.Effect.Laws.Generators as Generators
import qualified Moonlight.Category.Effect.Laws.Higher as Higher
import qualified Moonlight.Category.Effect.Laws.Limits as Limits
import qualified Moonlight.Category.Effect.Laws.Site as Site
import Moonlight.Pale.Test.LawSuite (LawBundle, lawSuiteGroup, renderLawBundles)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  lawSuiteGroup
    "moonlight-category"
    (renderLawBundles id categoryLawBundles)

categoryLawBundles :: [LawBundle String]
categoryLawBundles =
  Site.lawBundles
    <> Category.lawBundles
    <> Algebra.lawBundles
    <> Limits.lawBundles
    <> Adhesive.lawBundles
    <> Higher.lawBundles
    <> Generators.lawBundles
