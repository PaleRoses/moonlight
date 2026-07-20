{-# LANGUAGE TypeFamilies #-}

-- | Comparison oracle pinning 'PackedNode' ordering bit-identical to the reflective 'Node' ordering.
module Moonlight.EGraph.Front.PackedNodeOracleSpec
  ( tests,
  )
where

import Data.Foldable (toList)
import Data.Fix (Fix (..))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Set qualified as Set
import GHC.TypeLits (Symbol)
import Moonlight.Core
  ( BinderId (..),
    Pattern (..),
    RewriteRuleId (..),
    ZipMatch (..),
    mkPatternVar,
    zipSameNodeShape,
  )
import Moonlight.Constraint
  ( ConstraintExpr (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( compareChildArrays,
    encodeSortKey,
    packNode,
    packPattern,
    packedChildren,
    packedNode,
    packedSortKey,
    packedTag,
    unpackPattern,
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedPlan
  ( packCompiledApplicationCondition,
    packCompiledFactRule,
    packCompiledGuard,
    packCompiledPatternExtension,
    packCompiledPatternQuery,
    packFactRule,
    packGuardAtom,
    packGuardExpr,
    packGuardTerm,
    packPatternQuery,
    packPostMatchSubst,
    packPostMatchTerm,
    packRewriteCondition,
    packRulePlan,
    packRulePlanSet,
    unpackCompiledApplicationCondition,
    unpackCompiledFactRule,
    unpackCompiledGuard,
    unpackCompiledPatternExtension,
    unpackCompiledPatternQuery,
    unpackFactRule,
    unpackGuardAtom,
    unpackGuardExpr,
    unpackGuardTerm,
    unpackPatternQuery,
    unpackPostMatchSubst,
    unpackRewriteCondition,
    unpackRulePlan,
    unpackRulePlanSet,
  )
import Moonlight.EGraph.Test.Case (PropertyCase (..), propertyCases)
import Moonlight.EGraph.Test.SDF.Core (SDFF, genSDFTerm)
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationCondition (..),
    CompiledApplicationCondition,
    CompiledPatternExtension,
    PatternExtension,
    compileApplicationCondition,
    compiledApplicationConditionExtensions,
    globalPatternExtension,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    PatternQuery (..),
    cpqQuery,
    compiledSinglePatternQuery,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst (..),
    PostMatchTerm (..),
  )
import Moonlight.Rewrite.Runtime
  ( rhsInstantiationSpec,
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan,
    certifyRulePlan,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardAtom (..),
    GuardExpr,
    GuardTerm (..),
    RewriteCondition (..),
    data GuardRoot,
    guardChildIndex,
    guardRefTerm,
    mapCompiledGuard,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
    FactRuleId (..),
    RawFactRule (..),
    cfrCompiledQuery,
    compileFactRule,
  )
import Moonlight.Rewrite.System
  ( FactId (..),
  )
import Moonlight.Rewrite.System
  ( planRuleSet,
    rulePlanNames,
    rulePlans,
  )
import Moonlight.Rewrite.System
  ( checkRuleSet,
    RuleSet,
    ruleSet,
    ruleWithId,
  )
import Moonlight.Rewrite.System
  ( combineCompiledGuards,
    mkRuleName,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseEnum,
    chooseInt,
    counterexample,
    elements,
    forAll,
    forAllBlind,
    frequency,
    listOf,
    oneof,
    property,
    suchThatMap,
    vectorOf,
    (.&&.),
    (===),
  )

data OracleTag
  = LeafTag Int
  | ForkTag
  | TieTag
  | BundleTag
  | GlyphTag
  deriving stock (Eq, Ord, Show)

data OracleCapability
  = OracleCapabilityA
  | OracleCapabilityB
  deriving stock (Eq, Ord, Show)

data OracleSig (result :: Symbol) r where
  OracleLeaf :: Int -> OracleSig "S" r
  OracleFork :: r "S" -> r "SA" -> OracleSig "S" r
  OracleTie :: r "SA" -> OracleSig "SA" r
  OracleBundle :: [r "S"] -> OracleSig "SA" r
  OracleGlyph :: r "S" -> OracleSig "Sé" r

instance HTraversable OracleSig where
  htraverseWithSort transform =
    \case
      OracleLeaf value ->
        pure (OracleLeaf value)
      OracleFork leftChild rightChild ->
        OracleFork
          <$> transform SortWitness leftChild
          <*> transform SortWitness rightChild
      OracleTie child ->
        OracleTie <$> transform SortWitness child
      OracleBundle children ->
        OracleBundle <$> traverse (transform SortWitness) children
      OracleGlyph child ->
        OracleGlyph <$> transform SortWitness child

instance RewriteSignature OracleSig where
  type NodeTag OracleSig = OracleTag

  nodeTag =
    \case
      OracleLeaf value -> LeafTag value
      OracleFork {} -> ForkTag
      OracleTie {} -> TieTag
      OracleBundle {} -> BundleTag
      OracleGlyph {} -> GlyphTag

  nodeTagDigest _ =
    \case
      LeafTag value -> fromIntegral (100 + value)
      ForkTag -> 1
      TieTag -> 2
      BundleTag -> 3
      GlyphTag -> 4

  nodeResultSort =
    \case
      OracleLeaf {} -> SortWitness
      OracleFork {} -> SortWitness
      OracleTie {} -> SortWitness
      OracleBundle {} -> SortWitness
      OracleGlyph {} -> SortWitness

instance ZipMatch (Node OracleSig) where
  zipMatch =
    zipSameNodeShape

data MonoOracleSig f (result :: Symbol) r where
  MonoOracleNode :: f (r "Expr") -> MonoOracleSig f "Expr" r

instance Traversable f => HTraversable (MonoOracleSig f) where
  htraverseWithSort transform =
    \case
      MonoOracleNode layer ->
        MonoOracleNode <$> traverse (transform SortWitness) layer

instance Traversable f => RewriteSignature (MonoOracleSig f) where
  type NodeTag (MonoOracleSig f) = f ()

  nodeTag =
    \case
      MonoOracleNode layer -> () <$ layer

  nodeTagDigest _ _ =
    0

  nodeResultSort =
    \case
      MonoOracleNode {} -> SortWitness

genChildValue :: Gen Int
genChildValue =
  chooseInt (0, 2)

genOracleNodeWith :: Gen child -> Gen (Node OracleSig child)
genOracleNodeWith genChild =
  oneof
    [ fmap (Node . OracleLeaf) (chooseInt (-1, 2)),
      (\leftChild rightChild -> Node (OracleFork (K leftChild) (K rightChild)))
        <$> genChild
        <*> genChild,
      fmap (Node . OracleTie . K) genChild,
      fmap
        (Node . OracleBundle . fmap K)
        (chooseInt (0, 3) >>= \bundleLength -> vectorOf bundleLength genChild),
      fmap (Node . OracleGlyph . K) genChild
    ]

genOracleNode :: Gen (Node OracleSig Int)
genOracleNode =
  genOracleNodeWith genChildValue

genOraclePattern :: Gen (Pattern (Node OracleSig))
genOraclePattern =
  patternAtDepth (2 :: Int)
  where
    patternAtDepth :: Int -> Gen (Pattern (Node OracleSig))
    patternAtDepth depth
      | depth <= 0 =
          genPatternVariable
      | otherwise =
          frequency
            [ (2, genPatternVariable),
              (3, fmap PatternNode (genOracleNodeWith (patternAtDepth (depth - 1))))
            ]
    genPatternVariable :: Gen (Pattern (Node OracleSig))
    genPatternVariable =
      fmap (PatternVar . mkPatternVar) (chooseInt (0, 2))

genOraclePatternQuery ::
  Gen guard ->
  Gen (PatternQuery guard (Node OracleSig))
genOraclePatternQuery genGuard =
  patternQueryAtDepth (2 :: Int)
  where
    patternQueryAtDepth depth
      | depth <= 0 =
          SinglePatternQuery <$> genOraclePattern
      | otherwise =
          oneof
            [ SinglePatternQuery <$> genOraclePattern,
              ConjunctivePatternQuery
                <$> ((:|) <$> patternQueryAtDepth (depth - 1) <*> vectorOf 2 (patternQueryAtDepth (depth - 1))),
              GuardedPatternQuery
                <$> patternQueryAtDepth (depth - 1)
                <*> genGuard
            ]

genOracleCapability :: Gen OracleCapability
genOracleCapability =
  elements [OracleCapabilityA, OracleCapabilityB]

genGuardTerm :: Int -> Gen (GuardTerm (Node OracleSig))
genGuardTerm depth
  | depth <= 0 =
      pure (guardRefTerm GuardRoot)
  | otherwise =
      frequency
        [ (2, pure (guardRefTerm GuardRoot)),
          (1, GuardProjectTerm <$> genGuardTerm (depth - 1) <*> (guardChildIndex . fromIntegral <$> chooseInt (0, 2))),
          (3, GuardNodeTerm <$> genOracleNodeWith (genGuardTerm (depth - 1)))
        ]

genGuardAtom :: Gen (GuardAtom OracleCapability (Node OracleSig))
genGuardAtom =
  oneof
    [ ClassesEquivalent <$> genGuardTerm 2 <*> genGuardTerm 2,
      HasFact <$> genFactId <*> listOf (genGuardTerm 1),
      HasCapability <$> genOracleCapability <*> listOf (genGuardTerm 1)
    ]

genGuardExpr :: Gen (GuardExpr OracleCapability (Node OracleSig))
genGuardExpr =
  exprAtDepth (2 :: Int)
  where
    exprAtDepth :: Int -> Gen (GuardExpr OracleCapability (Node OracleSig))
    exprAtDepth depth
      | depth <= 0 =
          Atom <$> genGuardAtom
      | otherwise =
          frequency
            [ (4, Atom <$> genGuardAtom),
              (1, Not <$> exprAtDepth (depth - 1)),
              (1, And <$> vectorOf 2 (exprAtDepth (depth - 1))),
              (1, Or <$> vectorOf 2 (exprAtDepth (depth - 1)))
            ]

genRewriteCondition :: Gen (RewriteCondition OracleCapability (Node OracleSig))
genRewriteCondition =
  RewriteCondition <$> genGuardExpr

compiledGuardFromExpr ::
  forall capability f.
  (Ord capability, Ord (GuardTerm f)) =>
  GuardExpr capability f ->
  CompiledGuard capability f
compiledGuardFromExpr guardExpr =
  mapCompiledGuard (const guardExpr) (mempty :: CompiledGuard capability f)

genCompiledGuard :: Gen (CompiledGuard OracleCapability (Node OracleSig))
genCompiledGuard =
  compiledGuardFromExpr <$> genGuardExpr

genCompiledPatternQuery ::
  Gen (CompiledPatternQuery (CompiledGuard OracleCapability (Node OracleSig)) (Node OracleSig))
genCompiledPatternQuery =
  compiledSinglePatternQuery
    <$> genOraclePattern
    <*> oneof [pure Nothing, Just <$> genCompiledGuard]

genPostMatchTerm :: Gen (PostMatchTerm (Node OracleSig))
genPostMatchTerm =
  oneof
    [ PostMatchVar . mkPatternVar <$> chooseInt (0, 2),
      PostMatchPattern <$> genOraclePattern
    ]

genPostMatchSubst :: Int -> Gen (PostMatchSubst (Node OracleSig))
genPostMatchSubst depth
  | depth <= 0 =
      SubstBinder
        <$> (BinderId <$> chooseInt (0, 2))
        <*> genPostMatchTerm
  | otherwise =
      oneof
        [ SubstBinder
            <$> (BinderId <$> chooseInt (0, 2))
            <*> genPostMatchTerm,
          SequentialPostMatchSubst
            <$> genPostMatchSubst (depth - 1)
            <*> genPostMatchSubst (depth - 1)
        ]

genCompiledPatternExtension ::
  Gen (CompiledPatternExtension (CompiledGuard OracleCapability (Node OracleSig)) (Node OracleSig))
genCompiledPatternExtension =
  genCompiledApplicationCondition
    `suchThatMap` (\condition -> case compiledApplicationConditionExtensions condition of
                     extension : _ -> Just extension
                     [] -> Nothing
                  )

genCompiledApplicationCondition ::
  Gen (CompiledApplicationCondition (CompiledGuard OracleCapability (Node OracleSig)) (Node OracleSig))
genCompiledApplicationCondition =
  exprAtDepth (2 :: Int)
    `suchThatMap` ( either (const Nothing) Just
                      . compileApplicationCondition
                        combineCompiledGuards
                        (\_ compiledGuard -> Right compiledGuard)
                        Set.empty
                      . ApplicationCondition
                  )
  where
    exprAtDepth ::
      Int ->
      Gen (ConstraintExpr (PatternExtension (CompiledGuard OracleCapability (Node OracleSig)) (Node OracleSig)))
    exprAtDepth depth
      | depth <= 0 =
          Atom <$> genPatternExtension
      | otherwise =
          frequency
            [ (4, Atom <$> genPatternExtension),
              (1, Not <$> exprAtDepth (depth - 1)),
              (1, And <$> vectorOf 2 (exprAtDepth (depth - 1))),
              (1, Or <$> vectorOf 2 (exprAtDepth (depth - 1)))
            ]

    genPatternExtension =
      globalPatternExtension . cpqQuery <$> genCompiledPatternQuery

genRulePlan ::
  Gen (RulePlan (CompiledGuard OracleCapability (Node OracleSig)) (Node OracleSig))
genRulePlan =
  ( (,,,)
      <$> (RewriteRuleId <$> chooseInt (0, 5))
      <*> genCompiledPatternQuery
      <*> (rhsInstantiationSpec <$> oneof [pure Nothing, Just <$> genPostMatchSubst 2] <*> genOraclePattern)
      <*> oneof [pure Nothing, Just <$> genCompiledApplicationCondition]
  )
    `suchThatMap` (\(rewriteRuleId, query, rhsSpec, applicationCondition) ->
                     either
                       (const Nothing)
                       Just
                       (certifyRulePlan rewriteRuleId query rhsSpec applicationCondition)
                  )

genFactId :: Gen FactId
genFactId =
  FactId <$> chooseInt (0, 5)

genFactRule :: Gen (FactRule OracleCapability (Node OracleSig))
genFactRule =
  FactRule
    <$> (FactRuleId <$> chooseInt (0, 5))
    <*> elements ["packed.fact.a", "packed.fact.b"]
    <*> genOraclePattern
    <*> pure [GuardRoot]
    <*> genFactId
    <*> oneof [pure Nothing, Just <$> genRewriteCondition]

genCompiledFactRule ::
  Gen (CompiledFactRule OracleCapability (Node OracleSig))
genCompiledFactRule =
  genFactRule
    `suchThatMap` (either (const Nothing) Just . compileFactRule)

sdfTermSize :: Fix SDFF -> Int
sdfTermSize (Fix layer) =
  1 + sum (fmap sdfTermSize layer)

sdfNodeLayers :: Fix SDFF -> [Node (MonoOracleSig SDFF) Int]
sdfNodeLayers (Fix layer) =
  Node (MonoOracleNode (fmap (K . sdfTermSize) layer))
    : concatMap sdfNodeLayers (toList layer)

genSDFLayerPair :: Gen (Node (MonoOracleSig SDFF) Int, Node (MonoOracleSig SDFF) Int)
genSDFLayerPair = do
  leftTerm <- genSDFTerm 3
  rightTerm <- oneof [pure leftTerm, genSDFTerm 3]
  (,)
    <$> elements (sdfNodeLayers leftTerm)
    <*> elements (sdfNodeLayers rightTerm)

genSortChar :: Gen Char
genSortChar =
  frequency
    [ (4, chooseEnum ('A', 'z')),
      (1, chooseEnum ('\x0', '\x10FFFF')),
      (2, elements "S\xE9\x7F\x80\x7FF\x800\xFFFF\x10000")
    ]

genSortStringPair :: Gen (String, String)
genSortStringPair = do
  sharedPrefix <- listOf genSortChar
  leftSuffix <- listOf genSortChar
  rightSuffix <- listOf genSortChar
  pure (sharedPrefix <> leftSuffix, sharedPrefix <> rightSuffix)

genChildListPair :: Gen ([Int], [Int])
genChildListPair = do
  sharedPrefix <- listOf genChildValue
  leftTail <- listOf genChildValue
  rightTail <- listOf genChildValue
  pure (sharedPrefix <> leftTail, sharedPrefix <> rightTail)

adjustChild :: Int -> Int
adjustChild =
  subtract 3

orderPreservedByEither ::
  (Show err, Ord original, Ord packed) =>
  (original -> Either err packed) ->
  original ->
  original ->
  Property
orderPreservedByEither transform leftValue rightValue =
  case (transform leftValue, transform rightValue) of
    (Right leftPacked, Right rightPacked) ->
      compare leftPacked rightPacked === compare leftValue rightValue
    (Left err, _) ->
      counterexample ("left conversion failed: " <> show err) False
    (_, Left err) ->
      counterexample ("right conversion failed: " <> show err) False

roundTripsByEither ::
  (Show err, Eq original) =>
  (original -> Either err packed) ->
  (packed -> Either err original) ->
  original ->
  Property
roundTripsByEither packValue unpackValue original =
  case packValue original >>= unpackValue of
    Right unpacked ->
      property (unpacked == original)
    Left err ->
      counterexample ("round-trip conversion failed: " <> show err) False

planSetRoundTripProperty :: Property
planSetRoundTripProperty =
  case (mkRuleName "packed.plan.a", mkRuleName "packed.plan.b") of
    (Right leftName, Right rightName) ->
      case
        checkRuleSet
          ( ruleSet
              [ ruleWithId
                  (RewriteRuleId 0)
                  leftName
                  (PatternNode (Node (OracleLeaf 0)))
                  (PatternNode (Node (OracleLeaf 0))),
                ruleWithId
                  (RewriteRuleId 1)
                  rightName
                  (PatternNode (Node (OracleLeaf 1)))
                  (PatternNode (Node (OracleLeaf 1)))
              ]
              :: RuleSet OracleCapability (Node OracleSig)
          )
        of
        Left checkedError ->
          counterexample ("plan set construction failed: " <> show checkedError) False
        Right checkedSystem ->
          case planRuleSet checkedSystem of
            Left planError ->
              counterexample ("plan set certification failed: " <> show planError) False
            Right planSet ->
              case packRulePlanSet planSet of
                Left err ->
                  counterexample ("plan set pack failed: " <> show err) False
                Right packedPlanSet ->
                  case unpackRulePlanSet packedPlanSet of
                    Left err ->
                      counterexample ("plan set unpack failed: " <> show err) False
                    Right unpackedPlanSet ->
                      (rulePlanNames packedPlanSet === rulePlanNames planSet)
                        .&&. (rulePlanNames unpackedPlanSet === rulePlanNames planSet)
                        .&&. property (rulePlans unpackedPlanSet == rulePlans planSet)
    _ ->
      property False

tests :: TestTree
tests =
  testGroup "packed-node-ordering-oracle" $
    propertyCases
      [ PropertyCase "packed compare mirrors reflective compare on adversarial nodes" $
          forAll ((,) <$> genOracleNode <*> genOracleNode) $ \(leftNode, rightNode) ->
            compare (packNode leftNode) (packNode rightNode) === compare leftNode rightNode,
        PropertyCase "packed compare mirrors reflective compare on unit-shaped nodes" $
          forAll ((,) <$> genOracleNode <*> genOracleNode) $ \(leftNode, rightNode) ->
            let leftShape = fmap (const ()) leftNode
                rightShape = fmap (const ()) rightNode
             in compare (packNode leftShape) (packNode rightShape) === compare leftShape rightShape,
        PropertyCase "packed equality mirrors reflective equality" $
          forAll ((,) <$> genOracleNode <*> genOracleNode) $ \(leftNode, rightNode) ->
            (packNode leftNode == packNode rightNode) === (leftNode == rightNode),
        PropertyCase "packed compare mirrors reflective compare on SDF corpus layers" $
          forAll genSDFLayerPair $ \(leftNode, rightNode) ->
            compare (packNode leftNode) (packNode rightNode) === compare leftNode rightNode,
        PropertyCase "sort key encoding preserves string order" $
          forAll genSortStringPair $ \(leftString, rightString) ->
            compare (encodeSortKey leftString) (encodeSortKey rightString)
              === compare leftString rightString,
        PropertyCase "child array comparison matches list comparison" $
          forAll genChildListPair $ \(leftChildren, rightChildren) ->
            compareChildArrays (smallArrayFromList leftChildren) (smallArrayFromList rightChildren)
              === compare leftChildren rightChildren,
        PropertyCase "fmap preserves packing invariants" $
          forAll genOracleNode $ \nodeValue ->
            let mappedPacked = fmap adjustChild (packNode nodeValue)
                repacked = packNode (fmap adjustChild nodeValue)
             in (packedSortKey mappedPacked === packedSortKey repacked)
                  .&&. (packedTag mappedPacked === packedTag repacked)
                  .&&. (toList (packedChildren mappedPacked) === toList (packedChildren repacked))
                  .&&. (compare mappedPacked repacked === EQ),
        PropertyCase "traverse Identity preserves the packed node" $
          forAll genOracleNode $ \nodeValue ->
            let packed = packNode nodeValue
                traversed = runIdentity (traverse Identity packed)
             in (packedSortKey traversed === packedSortKey packed)
                  .&&. (packedTag traversed === packedTag packed)
                  .&&. (toList traversed === toList packed)
                  .&&. (compare traversed packed === EQ),
        PropertyCase "foldable view matches reflective children" $
          forAll genOracleNode $ \nodeValue ->
            toList (packNode nodeValue) === toList nodeValue,
        PropertyCase "zip match delegates through packing" $
          forAll ((,) <$> genOracleNode <*> genOracleNode) $ \(leftNode, rightNode) ->
            fmap packedNode (zipMatch (packNode leftNode) (packNode rightNode))
              === zipMatch leftNode rightNode,
        PropertyCase "pattern hoist preserves pattern order" $
          forAll ((,) <$> genOraclePattern <*> genOraclePattern) $ \(leftPattern, rightPattern) ->
            compare (packPattern leftPattern) (packPattern rightPattern)
              === compare leftPattern rightPattern,
        PropertyCase "pattern hoist round-trips" $
          forAll genOraclePattern $ \patternValue ->
            unpackPattern (packPattern patternValue) === patternValue,
        PropertyCase "pattern query hoist preserves query order" $
          forAll ((,) <$> genOraclePatternQuery genRewriteCondition <*> genOraclePatternQuery genRewriteCondition) $ \(leftQuery, rightQuery) ->
            compare (packPatternQuery packRewriteCondition leftQuery) (packPatternQuery packRewriteCondition rightQuery)
              === compare leftQuery rightQuery,
        PropertyCase "pattern query hoist round-trips" $
          forAll (genOraclePatternQuery genRewriteCondition) $ \query ->
            unpackPatternQuery unpackRewriteCondition (packPatternQuery packRewriteCondition query) === query,
        PropertyCase "compiled pattern query hoist preserves query order" $
          forAll ((,) <$> genCompiledPatternQuery <*> genCompiledPatternQuery) $ \(leftQuery, rightQuery) ->
            orderPreservedByEither packCompiledPatternQuery leftQuery rightQuery,
        PropertyCase "compiled pattern query hoist round-trips" $
          forAll genCompiledPatternQuery $ \query ->
            roundTripsByEither packCompiledPatternQuery unpackCompiledPatternQuery query,
        PropertyCase "guard term hoist preserves guard term order" $
          forAll ((,) <$> genGuardTerm 2 <*> genGuardTerm 2) $ \(leftTerm, rightTerm) ->
            compare (packGuardTerm leftTerm) (packGuardTerm rightTerm) === compare leftTerm rightTerm,
        PropertyCase "guard term hoist round-trips" $
          forAll (genGuardTerm 2) $ \guardTerm ->
            unpackGuardTerm (packGuardTerm guardTerm) === guardTerm,
        PropertyCase "guard atom hoist preserves guard atom order" $
          forAll ((,) <$> genGuardAtom <*> genGuardAtom) $ \(leftAtom, rightAtom) ->
            compare (packGuardAtom leftAtom) (packGuardAtom rightAtom) === compare leftAtom rightAtom,
        PropertyCase "guard atom hoist round-trips" $
          forAll genGuardAtom $ \guardAtom ->
            unpackGuardAtom (packGuardAtom guardAtom) === guardAtom,
        PropertyCase "guard expression hoist preserves expression order" $
          forAll ((,) <$> genGuardExpr <*> genGuardExpr) $ \(leftExpr, rightExpr) ->
            compare (packGuardExpr leftExpr) (packGuardExpr rightExpr) === compare leftExpr rightExpr,
        PropertyCase "guard expression hoist round-trips" $
          forAll genGuardExpr $ \guardExpr ->
            unpackGuardExpr (packGuardExpr guardExpr) === guardExpr,
        PropertyCase "rewrite condition hoist preserves condition order" $
          forAll ((,) <$> genRewriteCondition <*> genRewriteCondition) $ \(leftCondition, rightCondition) ->
            compare (packRewriteCondition leftCondition) (packRewriteCondition rightCondition)
              === compare leftCondition rightCondition,
        PropertyCase "rewrite condition hoist round-trips" $
          forAll genRewriteCondition $ \condition ->
            unpackRewriteCondition (packRewriteCondition condition) === condition,
        PropertyCase "compiled guard hoist preserves compiled guard order" $
          forAll ((,) <$> genCompiledGuard <*> genCompiledGuard) $ \(leftGuard, rightGuard) ->
            compare (packCompiledGuard leftGuard) (packCompiledGuard rightGuard) === compare leftGuard rightGuard,
        PropertyCase "compiled guard hoist round-trips" $
          forAll genCompiledGuard $ \compiledGuard ->
            unpackCompiledGuard (packCompiledGuard compiledGuard) === compiledGuard,
        PropertyCase "post-match term hoist preserves post-match term order" $
          forAll ((,) <$> genPostMatchTerm <*> genPostMatchTerm) $ \(leftTerm, rightTerm) ->
            compare (packPostMatchTerm leftTerm) (packPostMatchTerm rightTerm) === compare leftTerm rightTerm,
        PropertyCase "post-match subst hoist preserves post-match subst order" $
          forAll ((,) <$> genPostMatchSubst 2 <*> genPostMatchSubst 2) $ \(leftSubst, rightSubst) ->
            compare (packPostMatchSubst leftSubst) (packPostMatchSubst rightSubst) === compare leftSubst rightSubst,
        PropertyCase "post-match subst hoist round-trips" $
          forAll (genPostMatchSubst 2) $ \subst ->
            unpackPostMatchSubst (packPostMatchSubst subst) === subst,
        PropertyCase "compiled pattern extension hoist preserves extension order" $
          forAll ((,) <$> genCompiledPatternExtension <*> genCompiledPatternExtension) $ \(leftExtension, rightExtension) ->
            orderPreservedByEither packCompiledPatternExtension leftExtension rightExtension,
        PropertyCase "compiled pattern extension hoist round-trips" $
          forAll genCompiledPatternExtension $ \extension ->
            roundTripsByEither packCompiledPatternExtension unpackCompiledPatternExtension extension,
        PropertyCase "compiled application condition hoist preserves condition order" $
          forAll ((,) <$> genCompiledApplicationCondition <*> genCompiledApplicationCondition) $ \(leftCondition, rightCondition) ->
            orderPreservedByEither packCompiledApplicationCondition leftCondition rightCondition,
        PropertyCase "compiled application condition hoist round-trips" $
          forAll genCompiledApplicationCondition $ \condition ->
            roundTripsByEither packCompiledApplicationCondition unpackCompiledApplicationCondition condition,
        PropertyCase "rule plan hoist round-trips" $
          forAllBlind genRulePlan $ \rulePlan ->
            roundTripsByEither packRulePlan unpackRulePlan rulePlan,
        PropertyCase "rule plan set hoist preserves rule-name order and round-trips" $
          planSetRoundTripProperty,
        PropertyCase "fact rule hoist round-trips" $
          forAllBlind genFactRule $ \factRuleValue ->
            let packed = packFactRule factRuleValue
                unpacked = unpackFactRule packed
             in (frId unpacked === frId factRuleValue)
                  .&&. (frName unpacked === frName factRuleValue)
                  .&&. (frPattern unpacked === frPattern factRuleValue)
                  .&&. (frProjection unpacked === frProjection factRuleValue)
                  .&&. (frFactId unpacked === frFactId factRuleValue)
                  .&&. (frCondition unpacked === frCondition factRuleValue),
        PropertyCase "compiled fact rule hoist round-trips" $
          forAllBlind genCompiledFactRule $ \compiledFactRule ->
            roundTripsByEither packCompiledFactRule unpackCompiledFactRule compiledFactRule,
        PropertyCase "compiled fact rule query hoist preserves embedded query order" $
          forAllBlind ((,) <$> genCompiledFactRule <*> genCompiledFactRule) $ \(leftRule, rightRule) ->
            case (packCompiledFactRule leftRule, packCompiledFactRule rightRule) of
              (Right packedLeft, Right packedRight) ->
                compare (cfrCompiledQuery packedLeft) (cfrCompiledQuery packedRight)
                  === compare (cfrCompiledQuery leftRule) (cfrCompiledQuery rightRule)
              (Left err, _) ->
                counterexample ("left compiled fact rule conversion failed: " <> show err) False
              (_, Left err) ->
                counterexample ("right compiled fact rule conversion failed: " <> show err) False
      ]
