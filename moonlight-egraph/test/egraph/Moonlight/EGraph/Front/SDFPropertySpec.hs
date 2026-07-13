{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Front.SDFPropertySpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Extraction
    ( termCost )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.Core
    ( canonicalMap, equivalent, find )
import Moonlight.EGraph.Pure.Rebuild ( merge, rebuild )
import Moonlight.EGraph.Pure.Types
    ( ClassId, EGraph, eGraphClassCount, eGraphUnionFind, emptyEGraph )
import Moonlight.EGraph.Test.SDF.Core
    ( depthAnalysis,
      genLeaf,
      sdfCost,
      Depth,
      SDFF(SDFSubtract, SDFUnion, SDFEmpty, SDFIntersect, SDFFull,
           Complement) )
import Moonlight.EGraph.Test.Front.Mono (monoFix)
import Moonlight.EGraph.Pure.Saturation.Front (RulesetM)
import Moonlight.EGraph.Test.Front.SDF qualified as FrontSDF
import Moonlight.EGraph.Test.Case (PropertyCase (..), propertyCases)
import Data.Fix ( Fix(..) )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.QuickCheck
    ( Property,
      arbitrary,
      counterexample,
      forAllBlind,
      oneof,
      property,
      (.&&.),
      (===) )
import Data.IntMap.Strict qualified as IntMap ( IntMap )

freshGraph :: EGraph SDFF Depth
freshGraph =
  emptyEGraph depthAnalysis

canonicalRepresentative :: ClassId -> EGraph SDFF Depth -> ClassId
canonicalRepresentative classId graph =
  fst (find classId (eGraphUnionFind graph))

canonicalMapOf :: EGraph SDFF Depth -> IntMap.IntMap ClassId
canonicalMapOf =
  canonicalMap . eGraphUnionFind

propFindIdempotent :: Property
propFindIdempotent =
  forAllBlind arbitrary $ \term ->
    withGraphResult (addTerm term freshGraph) $ \(classId, graph) ->
    let
        unionFindState = eGraphUnionFind graph
        (firstRoot, unionFindAfterFirst) = find classId unionFindState
        (secondRoot, _) = find firstRoot unionFindAfterFirst
     in counterexample
          ("classId = " <> show classId <> ", firstRoot = " <> show firstRoot <> ", secondRoot = " <> show secondRoot)
          (firstRoot === secondRoot)

propAddTermIdempotent :: Property
propAddTermIdempotent =
  forAllBlind arbitrary $ \term ->
    withGraphResult (addTerm term freshGraph) $ \(firstClassId, graphAfterFirst) ->
      withGraphResult (addTerm term graphAfterFirst) $ \(secondClassId, _) ->
        counterexample
          ("firstClassId = " <> show firstClassId <> ", secondClassId = " <> show secondClassId)
          (firstClassId === secondClassId)

propMergeCommutative :: Property
propMergeCommutative =
  forAllBlind arbitrary $ \(termA, termB) ->
    withGraphResult (addTerm termA freshGraph) $ \(classIdA, graphWithA) ->
      withGraphResult (addTerm termB graphWithA) $ \(classIdB, graphWithBoth) ->
      let
        graphMergedAB = rebuild (merge classIdA classIdB graphWithBoth)
        graphMergedBA = rebuild (merge classIdB classIdA graphWithBoth)
        rootAinAB = canonicalRepresentative classIdA graphMergedAB
        rootBinAB = canonicalRepresentative classIdB graphMergedAB
        rootAinBA = canonicalRepresentative classIdA graphMergedBA
        rootBinBA = canonicalRepresentative classIdB graphMergedBA
     in counterexample
          ( "merge(a,b): rootA=" <> show rootAinAB <> " rootB=" <> show rootBinAB
              <> " | merge(b,a): rootA=" <> show rootAinBA <> " rootB=" <> show rootBinBA
          )
          ( property (rootAinAB == rootBinAB)
              .&&. property (rootAinBA == rootBinBA)
          )

propRebuildIdempotent :: Property
propRebuildIdempotent =
  forAllBlind arbitrary $ \(termA, termB) ->
    withGraphResult (addTerm termA freshGraph) $ \(classIdA, graphWithA) ->
      withGraphResult (addTerm termB graphWithA) $ \(classIdB, graphWithBoth) ->
      let
        graphAfterMerge = merge classIdA classIdB graphWithBoth
        rebuiltOnce = rebuild graphAfterMerge
        rebuiltTwice = rebuild rebuiltOnce
     in counterexample
          ("canonicalMap once = " <> show (canonicalMapOf rebuiltOnce) <> " twice = " <> show (canonicalMapOf rebuiltTwice))
          (canonicalMapOf rebuiltOnce === canonicalMapOf rebuiltTwice)

propExtractionNeverWorse :: Property
propExtractionNeverWorse =
  forAllBlind arbitrary $ \term ->
    let originalCost = termCost sdfCost term
     in case FrontSDF.runSDFExtractCost (monoFix term) of
          Left frontError ->
            counterexample ("front extraction failed: " <> frontError) (property False)
          Right Nothing ->
            property True
          Right (Just extractedCost) ->
            counterexample
              ("original cost = " <> show originalCost <> ", extracted cost = " <> show extractedCost)
              (property (extractedCost <= originalCost))

propSaturationTerminates :: Property
propSaturationTerminates =
  forAllBlind genTermWithRuleBundle $ \(term, bundle) ->
    case FrontSDF.runSDFSaturates (rulesFor bundle) (monoFix term) of
      Left frontError ->
        counterexample ("front saturation failed: " <> frontError) (property False)
      Right () ->
        property True
  where
    genTermWithRuleBundle =
      (,) <$> arbitrary <*> oneof (fmap pure ruleBundles)

ruleBundles :: [SDFRuleBundle]
ruleBundles =
  [ SDFLatticeRules,
    SDFComplementRules,
    SDFCommutativityRules,
    SDFSmoothBlendRules,
    SDFAllRules
  ]

data SDFRuleBundle
  = SDFLatticeRules
  | SDFComplementRules
  | SDFCommutativityRules
  | SDFSmoothBlendRules
  | SDFAllRules
  deriving stock (Eq, Ord, Show)

rulesFor :: SDFRuleBundle -> RulesetM FrontSDF.SDFSig ()
rulesFor =
  \case
    SDFLatticeRules -> FrontSDF.latticeRules
    SDFComplementRules -> FrontSDF.complementRules
    SDFCommutativityRules -> FrontSDF.commutativityRules
    SDFSmoothBlendRules -> FrontSDF.smoothBlendRules
    SDFAllRules -> FrontSDF.allRules

propMergedClassesSurviveRebuild :: Property
propMergedClassesSurviveRebuild =
  forAllBlind arbitrary $ \(termA, termB) ->
    withGraphResult (addTerm termA freshGraph) $ \(classIdA, graphWithA) ->
      withGraphResult (addTerm termB graphWithA) $ \(classIdB, graphWithBoth) ->
      let
        mergedGraph = merge classIdA classIdB graphWithBoth
        rebuiltGraph = rebuild mergedGraph
     in counterexample
          ( "classIdA = " <> show classIdA <> ", classIdB = " <> show classIdB
              <> ", equivalent after rebuild = " <> show (equivalent classIdA classIdB (eGraphUnionFind rebuiltGraph))
          )
          (property (equivalent classIdA classIdB (eGraphUnionFind rebuiltGraph)))

propHashConsingStructuralSharing :: Property
propHashConsingStructuralSharing =
  forAllBlind genSharedSubtermScenario $ \(parentTerm, sharedSubterm) ->
    withGraphResult (addTerm parentTerm freshGraph) $ \(_, graphWithParent) ->
      withGraphResult (addTerm sharedSubterm graphWithParent) $ \(standaloneClassId, graphWithBoth) ->
      let
        classCountBefore = eGraphClassCount graphWithParent
        classCountAfter = eGraphClassCount graphWithBoth
     in counterexample
          ( "standaloneClassId = " <> show standaloneClassId
              <> ", classCount before standalone add = " <> show classCountBefore
              <> ", classCount after standalone add = " <> show classCountAfter
          )
          (classCountBefore === classCountAfter)
  where
    genSharedSubtermScenario = do
      sharedSubterm <- genLeaf
      parentTerm <-
        oneof
          [ pure (Fix (SDFUnion sharedSubterm (Fix SDFEmpty))),
            pure (Fix (SDFIntersect sharedSubterm (Fix SDFFull))),
            pure (Fix (Complement sharedSubterm)),
            do
              otherChild <- genLeaf
              pure (Fix (SDFSubtract sharedSubterm otherChild))
          ]
      pure (parentTerm, sharedSubterm)

withGraphResult :: Show obstruction => Either obstruction result -> (result -> Property) -> Property
withGraphResult graphResult continue =
  either
    (\obstruction -> counterexample ("graph construction obstructed: " <> show obstruction) (property False))
    continue
    graphResult

tests :: TestTree
tests =
  testGroup "sdf-property" . propertyCases $
    [ PropertyCase "find is idempotent" propFindIdempotent,
      PropertyCase "adding same term twice returns same class" propAddTermIdempotent,
      PropertyCase "merge is commutative" propMergeCommutative,
      PropertyCase "rebuild is idempotent" propRebuildIdempotent,
      PropertyCase "extraction never returns worse cost than original" propExtractionNeverWorse,
      PropertyCase "saturation terminates within budget" propSaturationTerminates,
      PropertyCase "merged classes survive rebuild" propMergedClassesSurviveRebuild,
      PropertyCase "hash-consing shares structural subtrees" propHashConsingStructuralSharing
    ]
