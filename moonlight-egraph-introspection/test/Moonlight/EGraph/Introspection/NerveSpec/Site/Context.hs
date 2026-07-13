module Moonlight.EGraph.Introspection.NerveSpec.Site.Context
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Category (compose, identity)
import Moonlight.EGraph.Introspection.NerveSpec.Site.Prelude
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Moonlight.Sheaf.Site qualified as SheafContextPresentation
import Moonlight.Sheaf.Site qualified as SheafContextPairs

tests :: TestTree
tests =
  testGroup
    "context"
    [ testCase "validated rewrite contexts insert the root and deduplicate slices" testValidatedRewriteContexts,
      testCase "rewrite contexts canonicalize equality and form a lattice" testRewriteContextLattice,
      testCase "validated rewrite contexts reject unknown objects" testRejectsUnknownContextObjects,
      testCase "single-context Grothendieck kernel matches the rewrite nerve" testSingleContextGrothendieckKernel,
      testCase "Grothendieck nerve exposes cross-context rewrite structure" testMultiContextGrothendieckKernel,
      testCase "Grothendieck carrier contains an identity for every visible object" testGrothendieckIdentityCoverage,
      testCase "Grothendieck carrier closes visible compositions" testGrothendieckCompositionClosure,
      testCase "Grothendieck carrier is closed under categorical composition" testGrothendieckCarrierClosedUnderComposition,
      testCase "Grothendieck carrier is quotient-unique" testGrothendieckCarrierHasNoDuplicates,
      testCase "Grothendieck carrier is invariant under pair-generation strategy" testGrothendieckPairStrategyInvariance,
      testCase "Grothendieck summary isolates intrinsic Grothendieck structure" testGrothendieckSummary
    ]

withSheafPairStrategy ::
  SheafContextPresentation.ContextPresentation (RewriteSystem ArithF) ->
  SheafContextPairs.ContextPairStrategy (RewriteContext ArithF) ->
  SheafContextPresentation.ContextPresentation (RewriteSystem ArithF)
withSheafPairStrategy familyValue pairStrategy =
  SheafContextPresentation.ContextPresentation
    { SheafContextPresentation.cpSystem = SheafContextPresentation.cpSystem familyValue,
      SheafContextPresentation.cpContexts = SheafContextPresentation.cpContexts familyValue,
      SheafContextPresentation.cpPairStrategy = pairStrategy
    }

testValidatedRewriteContexts :: Assertion
testValidatedRewriteContexts =
  case
      mkRewriteSystemWithContexts
        [contextSpanAB, contextSpanBC]
        [ [PatternNode (Num 1), PatternNode (Num 2), PatternNode (Num 2)],
          [PatternNode (Num 1), PatternNode (Num 2)]
        ] of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case allContexts rewriteSystem of
        [rootContext, smallerContext] -> do
          assertEqual
            "expected the root context to contain the full rewrite carrier"
            (systemObjects rewriteSystem)
            (systemObjectsInContext rewriteSystem rootContext)
          assertEqual
            "expected duplicate context slices to collapse"
            [PatternNode (Num 1), PatternNode (Num 2)]
            (systemObjectsInContext rewriteSystem smallerContext)
        contextValues ->
          assertFailure ("expected exactly two normalized contexts, got " <> show (length contextValues))

testRewriteContextLattice :: Assertion
testRewriteContextLattice =
  case
      mkRewriteSystemWithContexts
        [contextSpanAB, contextSpanBC]
        [ [PatternNode (Num 2), PatternNode (Num 1)],
          [PatternNode (Num 3), PatternNode (Num 2)]
        ] of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem -> do
      let leftContext = mkRewriteContext 17 [PatternNode (Num 2), PatternNode (Num 1), PatternNode (Num 1)]
          rightContext = mkRewriteContext 23 [PatternNode (Num 1), PatternNode (Num 2)]
          thirdContext = mkRewriteContext 29 [PatternNode (Num 3), PatternNode (Num 2)]
          joinedContext = join rightContext thirdContext
          metContext = meet rightContext thirdContext
          maybeRootContext =
            allContexts rewriteSystem
              & filter ((== systemObjects rewriteSystem) . systemObjectsInContext rewriteSystem)
              & listToMaybe
      assertEqual
        "expected rewrite contexts to compare by canonical object content rather than ordinal"
        leftContext
        rightContext
      assertEqual
        "expected canonical contexts to sort and deduplicate their objects at construction"
        [PatternNode (Num 1), PatternNode (Num 2)]
        (rcObjects leftContext)
      assertEqual
        "expected join to compute sorted union on rewrite contexts"
        [PatternNode (Num 1), PatternNode (Num 2), PatternNode (Num 3)]
        (rcObjects joinedContext)
      assertEqual
        "expected meet to compute sorted intersection on rewrite contexts"
        [PatternNode (Num 2)]
        (rcObjects metContext)
      case maybeRootContext of
        Nothing ->
          assertFailure "expected a root context in the normalized rewrite system"
        Just rootContext -> do
          assertEqual
            "expected lattice-guided context enumeration to recover exactly the contexts below the root"
            (allContexts rewriteSystem)
            (latticeContextsBelow rewriteSystem rootContext)
          assertEqual
            "expected meet to compute common rewrite context"
            metContext
            (meet rightContext thirdContext)
          assertEqual
            "expected contextDepth to count visible objects in the queried scope"
            (length (rcObjects rightContext))
            (contextDepth rewriteSystem rightContext)

testRejectsUnknownContextObjects :: Assertion
testRejectsUnknownContextObjects =
  case
      mkRewriteSystemWithContexts
        [contextSpanAB]
        [[PatternNode (Num 1), PatternNode (Num 99)]] of
    Left (ContextContainsUnknownObjects 1) ->
      pure ()
    Left failure ->
      assertFailure ("expected unknown-object validation failure, got " <> show failure)
    Right _ ->
      assertFailure "expected explicit contexts with unknown objects to be rejected"

testSingleContextGrothendieckKernel :: Assertion
testSingleContextGrothendieckKernel =
  let familyValue = SheafContextPresentation.contextPresentation reversibleSystem
      familyNerve = grothendieckNerve familyValue 1
      familyObjects = grothendieckObjects familyValue
      familyMorphisms = grothendieckMorphisms familyValue
      family0 = length (simplicesAtDimension familyNerve 0)
      family1 = length (simplicesAtDimension familyNerve 1)
      siteAtDepth1 = mkRewriteNerveSite reversibleSystem 1
      site0 = length (siteCellsAtDimension siteAtDepth1 0)
      site1 = length (siteCellsAtDimension siteAtDepth1 1)
   in case allContexts reversibleSystem of
        [rootContext] -> do
          assertEqual
            "expected one Grothendieck object per rewrite object in the single-context kernel"
            (length (systemObjects reversibleSystem))
            (length familyObjects)
          assertBool
            "expected the unique rewrite context to be reflexively ordered"
            (contextLeq reversibleSystem rootContext rootContext)
          assertBool
            "expected all Grothendieck objects to live in the unique rewrite context"
            (all ((== rootContext) . goContext) familyObjects)
          assertBool
            "expected all Grothendieck morphisms to stay within the unique rewrite context"
            (all (\morphismValue -> gmSourceContext morphismValue == rootContext && gmTargetContext morphismValue == rootContext) familyMorphisms)
          assertEqual
            "expected Grothendieck 0-simplices to match the rewrite nerve in the single-context case"
            site0
            family0
          assertEqual
            "expected Grothendieck 1-simplices to match the rewrite nerve in the single-context case"
            site1
            family1
        contextValues ->
          assertFailure ("expected exactly one rewrite context in the single-context kernel, got " <> show (length contextValues))

testMultiContextGrothendieckKernel :: Assertion
testMultiContextGrothendieckKernel =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
          familyObjects = grothendieckObjects familyValue
          familyMorphisms = grothendieckMorphisms familyValue
          familyNerve = grothendieckNerve familyValue 1
          family1 = length (simplicesAtDimension familyNerve 1)
          siteAtDepth1 = mkRewriteNerveSite rewriteSystem 1
          site1 = length (siteCellsAtDimension siteAtDepth1 1)
       in case allContexts rewriteSystem of
            [rootContext, smallerContext] -> do
              assertBool
                "expected the smaller context to refine the root context"
                (contextLeq rewriteSystem smallerContext rootContext)
              assertBool
                "expected the root context not to refine the smaller context"
                (not (contextLeq rewriteSystem rootContext smallerContext))
              assertEqual
                "expected one Grothendieck object per visible object in each context"
                5
                (length familyObjects)
              assertEqual
                "expected exactly two vertical restriction morphisms into the smaller context"
                2
                ( length
                    [ morphismValue
                    | morphismValue <- familyMorphisms
                    , gmSourceContext morphismValue == rootContext
                    , gmTargetContext morphismValue == smallerContext
                    , isNothing (gmTargetMorphism morphismValue)
                    ]
                )
              assertBool
                "expected the visible rule to admit a cross-context diagonal morphism"
                ( any
                    (\morphismValue ->
                        gmSourceContext morphismValue == rootContext
                          && gmTargetContext morphismValue == smallerContext
                          && morphismMatchesSpan contextSpanAB (gmTargetMorphism morphismValue)
                    )
                    familyMorphisms
                )
              assertBool
                "expected the hidden rule not to admit a cross-context diagonal morphism"
                ( not
                    ( any
                        (\morphismValue ->
                            gmSourceContext morphismValue == rootContext
                              && gmTargetContext morphismValue == smallerContext
                              && morphismMatchesSpan contextSpanBC (gmTargetMorphism morphismValue)
                        )
                        familyMorphisms
                    )
                )
              assertBool
                "expected the Grothendieck nerve to add cross-context 1-simplices beyond the flat rewrite nerve"
                (family1 > site1)
            contextValues ->
              assertFailure ("expected exactly two rewrite contexts in the multi-context kernel, got " <> show (length contextValues))

morphismMatchesSpan :: RewriteMorphism ArithF -> Maybe (RewriteMorphism ArithF) -> Bool
morphismMatchesSpan expectedSpan =
  maybe False (sameRuntimeRewriteMorphism expectedSpan)

testGrothendieckCompositionClosure :: Assertion
testGrothendieckCompositionClosure =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
          familyMorphisms = grothendieckMorphisms familyValue
       in case allContexts rewriteSystem of
            [rootContext, _] ->
              assertBool
                "expected the Grothendieck carrier to include the visible composite ab;bc in the root context"
                ( any
                    (\morphismValue ->
                        gmSourceContext morphismValue == rootContext
                          && gmTargetContext morphismValue == rootContext
                          && gmSourceObject morphismValue == PatternNode (Num 1)
                          && gmTargetObject morphismValue == PatternNode (Num 3)
                    )
                    familyMorphisms
                )
            contextValues ->
              assertFailure ("expected exactly two rewrite contexts in the multi-context kernel, got " <> show (length contextValues))

testGrothendieckIdentityCoverage :: Assertion
testGrothendieckIdentityCoverage =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
          familyCategory = grothendieckCategoryFromPresentation familyValue
          familyObjects = grothendieckObjects familyValue
          familyMorphisms = Set.fromList (grothendieckMorphisms familyValue)
          missingIdentities =
            familyObjects
              & traverse
                ( \objectValue ->
                    case identity familyCategory objectValue of
                      Left categoryError ->
                        Left (objectValue, categoryError)
                      Right identityMorphismValue ->
                        Right
                          ( if identityMorphismValue `Set.member` familyMorphisms
                              then []
                              else [objectValue]
                          )
                )
       in case fmap concat missingIdentities of
            Left (objectValue, categoryError) ->
              assertFailure
                ( "expected identity construction to succeed for "
                    <> show objectValue
                    <> ", received "
                    <> show categoryError
                )
            Right missingIdentityObjects ->
              assertEqual
                "expected the Grothendieck carrier to contain the identity of every visible object"
                []
                missingIdentityObjects

testGrothendieckCarrierClosedUnderComposition :: Assertion
testGrothendieckCarrierClosedUnderComposition =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
       in assertGrothendieckCarrierClosedUnderComposition
            (grothendieckCategoryFromPresentation familyValue)
            (grothendieckMorphisms familyValue)

testGrothendieckCarrierHasNoDuplicates :: Assertion
testGrothendieckCarrierHasNoDuplicates =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyMorphisms = grothendieckMorphisms (SheafContextPresentation.contextPresentation rewriteSystem)
       in assertEqual
            "expected the Grothendieck carrier to be a quotient set rather than a multiset"
            (length familyMorphisms)
            (Set.size (Set.fromList familyMorphisms))

testGrothendieckPairStrategyInvariance :: Assertion
testGrothendieckPairStrategyInvariance =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
          morphismsFor strategyValue =
            Set.fromList
              ( grothendieckMorphisms
                  (withSheafPairStrategy familyValue strategyValue)
              )
          exhaustiveMorphisms = morphismsFor SheafContextPairs.ExhaustivePairs
          explicitExhaustiveMorphisms = morphismsFor SheafContextPairs.ExhaustivePairs
          generatorBuiltSystem = mkRewriteSystemFromGenerators [contextSpanAB, contextSpanBC]
          generatorFamily = SheafContextPresentation.contextPresentation generatorBuiltSystem
          generatorMorphismsFor strategyValue =
            Set.fromList
              ( grothendieckMorphisms
                  (withSheafPairStrategy generatorFamily strategyValue)
              )
          generatorExhaustiveMorphisms = generatorMorphismsFor SheafContextPairs.ExhaustivePairs
          generatorSeededMorphisms =
            generatorMorphismsFor (SheafContextPairs.GeneratorSeededPairs (contextGenerators generatorBuiltSystem))
       in do
            assertEqual
              "expected explicit exhaustive context pairs to preserve the Grothendieck carrier"
              exhaustiveMorphisms
              explicitExhaustiveMorphisms
            assertEqual
              "expected generator-seeded context pairs to preserve the Grothendieck carrier on generator-closed families"
              generatorExhaustiveMorphisms
              generatorSeededMorphisms

testGrothendieckSummary :: Assertion
testGrothendieckSummary =
  case (summarizeGrothendieckSystem reversibleSystem 2, summarizeRewriteSystem reversibleSystem 2, multiContextSystemResult) of
    (Left failure, _, _) ->
      assertFailure (show failure)
    (_, Left failure, _) ->
      assertFailure (show failure)
    (_, _, Left failure) ->
      assertFailure (show failure)
    (Right singleSummary, Right flatSummary, Right rewriteSystem) ->
      case summarizeGrothendieckSystem rewriteSystem 2 of
        Left failure ->
          assertFailure (show failure)
        Right multiSummary -> do
          assertEqual
            "expected the single-context Grothendieck summary to agree with the flat summary on connected components"
            (ssConnectedComponents flatSummary)
            (nhpConnectedComponents (gssHomotopyProfile singleSummary))
          assertEqual
            "expected the single-context Grothendieck summary to use the categorical nerve Betti numbers"
            [1, 0, 1]
            (nhpBettiVector (gssHomotopyProfile singleSummary))
          assertEqual
            "expected the single-context Grothendieck summary to agree with the flat summary on nilpotence"
            (if ssCoboundaryNilpotent flatSummary then SingleContextNilpotent else SingleContextNonNilpotent)
            (gssCoboundaryNilpotenceEvidence singleSummary)
          assertEqual
            "expected the single-context Grothendieck summary to have no cross-context morphisms"
            0
            (gssCrossContextMorphismCount singleSummary)
          assertBool
            "expected the multi-context Grothendieck summary to expose cross-context morphisms"
            (gssCrossContextMorphismCount multiSummary > 0)
          assertEqual
            "expected the multi-context Grothendieck summary to classify multi-context nilpotence explicitly"
            MultiContextNilpotent
            (gssCoboundaryNilpotenceEvidence multiSummary)
          assertBool
            "expected the multi-context Grothendieck summary to retain explicit site structure"
            (gssCellCount multiSummary > 0 && gssFaceCount multiSummary > 0)

assertGrothendieckCarrierClosedUnderComposition ::
  GrothendieckCategory (RewriteSystem ArithF) ->
  [GrothendieckMor (RewriteSystem ArithF)] ->
  Assertion
assertGrothendieckCarrierClosedUnderComposition categoryValue morphismValues =
  let morphismSet = Set.fromList morphismValues
      missingComposites =
        morphismValues
          & foldMap
            (\leftMorphism -> missingCompositeWitnesses categoryValue morphismSet leftMorphism morphismValues)
   in assertEqual
        "expected Grothendieck morphisms to form a carrier closed under composition"
        []
        missingComposites

missingCompositeWitnesses ::
  GrothendieckCategory (RewriteSystem ArithF) ->
  Set.Set (GrothendieckMor (RewriteSystem ArithF)) ->
  GrothendieckMor (RewriteSystem ArithF) ->
  [GrothendieckMor (RewriteSystem ArithF)] ->
  [(GrothendieckMor (RewriteSystem ArithF), GrothendieckMor (RewriteSystem ArithF), GrothendieckMor (RewriteSystem ArithF))]
missingCompositeWitnesses categoryValue morphismSet leftMorphism =
  foldMap
    (\rightMorphism ->
        case compose categoryValue leftMorphism rightMorphism of
          Right (compositeMorphism, _) ->
            [ (leftMorphism, rightMorphism, compositeMorphism)
            | compositeMorphism `Set.notMember` morphismSet
            ]
          Left _ ->
            []
    )
