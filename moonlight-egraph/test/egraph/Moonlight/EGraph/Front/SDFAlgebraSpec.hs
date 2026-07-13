{-# LANGUAGE DataKinds #-}

module Moonlight.EGraph.Front.SDFAlgebraSpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Saturation.Front (RulesetM, Term)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Front.SDF
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool)

tests :: TestTree
tests =
  testGroup
    "sdf-algebra"
    [latticeIdentityTests, complementTests, commutativityTests, smoothBlendTests, coarseApproximationTests, extractionTests]

latticeIdentityTests :: TestTree
latticeIdentityTests =
  testGroup "lattice-identities" $
    equivalenceCases
      latticeRules
      [ ("union with empty is identity (right)", sdfUnion ball sdfEmpty, ball),
        ("union with empty is identity (left)", sdfUnion sdfEmpty ball, ball),
        ("intersect with full is identity", sdfIntersect ball sdfFull, ball),
        ("union with full annihilates", sdfUnion ball sdfFull, sdfFull),
        ("intersect with empty annihilates", sdfIntersect ball sdfEmpty, sdfEmpty)
      ]

complementTests :: TestTree
complementTests =
  testGroup "complement" $
    equivalenceCases
      complementRules
      [ ("double complement is identity", sdfComplement (sdfComplement ball), ball),
        ("complement of empty is full", sdfComplement sdfEmpty, sdfFull),
        ("complement of full is empty", sdfComplement sdfFull, sdfEmpty)
      ]

commutativityTests :: TestTree
commutativityTests =
  testGroup "commutativity" $
    equivalenceCases
      commutativityRules
      [ ("union is commutative", sdfUnion ball cube, sdfUnion cube ball),
        ("intersect is commutative", sdfIntersect ball cube, sdfIntersect cube ball),
        ("smooth union is commutative", smoothUnion 0.5 ball capsuleShape, smoothUnion 0.5 capsuleShape ball)
      ]

smoothBlendTests :: TestTree
smoothBlendTests =
  testGroup "smooth-blend" $
    equivalenceCases
      smoothBlendRules
      [("smooth union with zero blend degenerates to hard union", smoothUnion 0.0 ball cube, sdfUnion ball cube)]

coarseApproximationTests :: TestTree
coarseApproximationTests =
  testGroup "coarse-approximation" $
    hunitCases
      [ HUnitCase "declared positive-radius fact enables coarse collapse" $
          assertSDFEquivalent
            coarseApproximationRules
            (smoothUnion 0.5 (sphere 2.0) cube)
            (sdfUnion (sphere 2.0) cube),
        HUnitCase "coarse collapse remains gated without the radius fact" $
          assertSDFNotEquivalent
            coarseApproximationRule
            (smoothUnion 0.5 (sphere 2.0) cube)
            (sdfUnion (sphere 2.0) cube),
        HUnitCase "positive-radius fact does not wildcard embedded blend radii" $
          assertSDFNotEquivalent
            coarseApproximationRules
            (smoothUnion 1.0 (sphere 2.0) cube)
            (sdfUnion (sphere 2.0) cube)
      ]

extractionTests :: TestTree
extractionTests =
  testGroup "cost-extraction" $
    hunitCases
      [ HUnitCase "extraction eliminates redundant union-with-empty" $
          assertSDFExtractCost "union-with-empty" (sdfUnion ball sdfEmpty) (@?= 1),
        HUnitCase "extraction prefers sphere over complement-of-complement-of-sphere" $
          assertSDFExtractCost "double-complement" (sdfComplement (sdfComplement ball)) (@?= 1),
        HUnitCase "extraction simplifies nested identity operations" $
          assertSDFExtractCost "nested-identities" (sdfIntersect (sdfUnion ball sdfEmpty) sdfFull) (@?= 1),
        HUnitCase "extraction prefers hard union over zero-blend smooth union" $
          assertSDFExtractCost "zero-blend" (smoothUnion 0.0 ball cube) $
            \cost -> assertBool "hard union cheaper than smooth" (cost <= 5)
      ]

equivalenceCases ::
  RulesetM SDFSig () ->
  [(String, Term SDFSig "Expr", Term SDFSig "Expr")] ->
  [TestTree]
equivalenceCases rules =
  hunitCases
    . fmap (\(caseName, left, right) -> HUnitCase caseName (assertSDFEquivalent rules left right))

ball :: Term SDFSig "Expr"
ball = sphere 1.0

cube :: Term SDFSig "Expr"
cube = box 2.0 2.0 2.0

capsuleShape :: Term SDFSig "Expr"
capsuleShape = capsule 1.0 2.0
