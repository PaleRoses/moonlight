{-# LANGUAGE TypeFamilies #-}

module Algebra
  ( algebraBenchmarkPreflight,
    algebraBenchmarks,
  )
where

import Control.Monad (void)
import Control.Exception (evaluate)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Control.DeepSeq (NFData (..))
import Moonlight.Category
  ( AdhesiveCategory (..),
    Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    MonicMatchComponents (..),
    MonicMatchWitness,
    PBPOAdhesiveCategory,
    PushoutComplementComponents (..),
    monicMatchArrow,
    witnessMonic,
  )
import Moonlight.Core
  ( Pattern (..),
    mkPatternVar,
  )
import Moonlight.Rewrite.Algebra
  ( PatternQuery,
    guardedPatternQuery,
    patternQueryConditions,
    singlePatternQuery,
  )
import Moonlight.Rewrite.Algebra
  ( PBPOLegs (..),
    PBPOMatch (..),
    PBPORule,
    PBPOStep (..),
    PBPOUntypedStep (..),
    applyPBPO,
    applyPBPOPlus,
    identityTypedRule,
    mkPBPORule,
    pbpoRuleContextType,
    pbpoRuleContextLeg,
    pbpoRuleInterface,
    pbpoRuleInterfaceTyping,
    pbpoRuleLeft,
    pbpoRuleLeftLeg,
    pbpoRuleLeftTyping,
    pbpoRuleRight,
    pbpoRuleRightLeg,
  )
import Common (expectBench, expectMaybeBench)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data RefCat = RefCat

newtype RefOb = RefOb
  { refObRefs :: IntSet
  }
  deriving stock (Eq, Show)

data RefMor = RefMor
  { refMorFrom :: !IntSet,
    refMorTo :: !IntSet
  }
  deriving stock (Eq, Show)

data RefTwoMor

newtype RefCompositor = RefCompositor ()

refInclusion :: IntSet -> IntSet -> Maybe RefMor
refInclusion fromRefs toRefs
  | fromRefs `IntSet.isSubsetOf` toRefs =
      Just (RefMor fromRefs toRefs)
  | otherwise =
      Nothing

instance Category RefCat where
  type Ob RefCat = RefOb
  type Mor RefCat = RefMor
  type TwoMor RefCat = RefTwoMor
  type Compositor RefCat = RefCompositor
  type CategoryError RefCat = ()

  identity _ (RefOb objectRefs) =
    Right (RefMor objectRefs objectRefs)

  compose _ outer inner
    | refMorTo inner == refMorFrom outer =
        Right (RefMor (refMorFrom inner) (refMorTo outer), RefCompositor ())
    | otherwise =
        Left ()

  source _ =
    Right . RefOb . refMorFrom

  target _ =
    Right . RefOb . refMorTo

instance HasPullbacks RefCat where
  pullback _ leftBase rightBase
    | refMorTo leftBase == refMorTo rightBase =
        let apexRefs =
              IntSet.intersection (refMorFrom leftBase) (refMorFrom rightBase)
         in Just
              ( RefOb apexRefs,
                RefMor apexRefs (refMorFrom leftBase),
                RefMor apexRefs (refMorFrom rightBase)
              )
    | otherwise =
        Nothing

  pullbackMediator _ leftBase rightBase coneLeft coneRight
    | refMorTo leftBase == refMorTo rightBase
        && refMorTo coneLeft == refMorFrom leftBase
        && refMorTo coneRight == refMorFrom rightBase
        && refMorFrom coneLeft == refMorFrom coneRight =
        refInclusion
          (refMorFrom coneLeft)
          (IntSet.intersection (refMorFrom leftBase) (refMorFrom rightBase))
    | otherwise =
        Nothing

instance HasPushouts RefCat where
  pushout _ leftLeg rightLeg
    | refMorFrom leftLeg == refMorFrom rightLeg =
        let apexRefs =
              IntSet.union (refMorTo leftLeg) (refMorTo rightLeg)
         in Just
              ( RefOb apexRefs,
                RefMor (refMorTo leftLeg) apexRefs,
                RefMor (refMorTo rightLeg) apexRefs
              )
    | otherwise =
        Nothing

instance AdhesiveCategory RefCat where
  monicMatchComponents _ morphism =
    MonicMatchComponents <$> refInclusion (refMorFrom morphism) (refMorTo morphism)

  pushoutComplementComponents _ ruleLeg monicMatch
    | refMorTo ruleLeg == refMorFrom (monicMatchArrow monicMatch) =
        let hostRefs =
              refMorTo (monicMatchArrow monicMatch)
            complementRefs =
              IntSet.union
                (IntSet.difference hostRefs (refMorTo ruleLeg))
                (refMorFrom ruleLeg)
         in Just
              PushoutComplementComponents
                { pushoutComplementComponentObject = RefOb complementRefs,
                  pushoutComplementComponentBorrowedLeg = RefMor complementRefs hostRefs,
                  pushoutComplementComponentResidualLeg = RefMor (refMorFrom ruleLeg) complementRefs
                }
    | otherwise =
        Nothing

instance PBPOAdhesiveCategory RefCat

data PBPOFixture = PBPOFixture
  { pfIdentityRule :: !(PBPORule RefCat String),
    pfPermissiveRule :: !(PBPORule RefCat String),
    pfRestrictiveRule :: !(PBPORule RefCat String),
    pfMatch :: !(PBPOMatch RefCat),
    pfMonic :: !(MonicMatchWitness RefCat)
  }

instance NFData PBPOFixture where
  rnf fixture =
    pfIdentityRule fixture
      `seq` pfPermissiveRule fixture
      `seq` pfRestrictiveRule fixture
      `seq` pfMatch fixture
      `seq` pfMonic fixture
      `seq` ()

algebraBenchmarks :: Benchmark
algebraBenchmarks =
  bgroup
    "algebra"
    [ bench "mkPBPORule/permissive" (nf mkPermissiveRuleWeight ()),
      bench "identityTypedRule" (nf identityTypedRuleWeight ()),
      bench "query/condition-collection depth=4096" (nf queryConditionCollectionWeight queryConditionFixture),
      env pbpoFixture $ \fixture ->
        bgroup
          "pbpo"
          [ bench "derived rule objects" (nf derivedRuleObjectsWeight fixture),
            bench "applyPBPO/untyped" (nf applyPBPOWeight fixture),
            bench "applyPBPOPlus/permissive" (nf applyPBPOPlusPermissiveWeight fixture),
            bench "applyPBPOPlus/restrictive" (nf applyPBPOPlusRestrictiveWeight fixture)
          ]
    ]

algebraBenchmarkPreflight :: IO ()
algebraBenchmarkPreflight =
  pbpoFixture >>= \fixture ->
    sequence_
      [ void (expectBench "derived interface" (pbpoRuleInterface RefCat (pfPermissiveRule fixture))),
        void (expectBench "derived left" (pbpoRuleLeft RefCat (pfPermissiveRule fixture))),
        void (expectBench "derived right" (pbpoRuleRight RefCat (pfPermissiveRule fixture))),
        void (expectBench "derived context type" (pbpoRuleContextType RefCat (pfPermissiveRule fixture))),
        void (expectBench "applyPBPO" (applyPBPO RefCat (pfIdentityRule fixture) (pfMonic fixture))),
        void (expectBench "applyPBPOPlus permissive" (applyPBPOPlus RefCat (pfPermissiveRule fixture) (pfMatch fixture))),
        void (expectBench "applyPBPOPlus restrictive" (applyPBPOPlus RefCat (pfRestrictiveRule fixture) (pfMatch fixture))),
        void (evaluate (queryConditionCollectionWeight queryConditionFixture))
      ]

queryConditionFixture :: PatternQuery Int []
queryConditionFixture =
  foldl'
    guardedPatternQuery
    (singlePatternQuery (PatternVar (mkPatternVar 0)))
    [1 .. 4096]

queryConditionCollectionWeight :: PatternQuery Int [] -> Int
queryConditionCollectionWeight =
  sum . patternQueryConditions

pbpoFixture :: IO PBPOFixture
pbpoFixture = do
  identityRule <-
    expectBench "identityTypedRule" identityTypedRuleValue
  permissiveRule <-
    expectBench "mkPBPORule permissive" permissiveRuleValue
  restrictiveRule <-
    expectBench "mkPBPORule restrictive" restrictiveRuleValue
  matchArrow <-
    expectMaybeBench "match arrow" (refInclusion (refs [1, 2]) (refs [1, 2, 3]))
  monicMatch <-
    expectMaybeBench "monic witness" (witnessMonic RefCat matchArrow)
  adherence <-
    expectMaybeBench "adherence" (refInclusion (refs [1, 2, 3]) (refs [1, 2, 3]))
  pure
    PBPOFixture
      { pfIdentityRule = identityRule,
        pfPermissiveRule = permissiveRule,
        pfRestrictiveRule = restrictiveRule,
        pfMatch =
          PBPOMatch
            { pbpoMatchMonic = monicMatch,
              pbpoMatchAdherence = adherence
            },
        pfMonic = monicMatch
      }

mkPermissiveRuleWeight :: () -> Maybe Int
mkPermissiveRuleWeight () =
  either (const Nothing) (Just . pbpoRuleWeight) permissiveRuleValue

identityTypedRuleWeight :: () -> Maybe Int
identityTypedRuleWeight () =
  either (const Nothing) (Just . pbpoRuleWeight) identityTypedRuleValue

derivedRuleObjectsWeight :: PBPOFixture -> Maybe Int
derivedRuleObjectsWeight fixture =
  sum
    <$> sequence
      [ either (const Nothing) (Just . refObWeight) (pbpoRuleInterface RefCat ruleValue),
        either (const Nothing) (Just . refObWeight) (pbpoRuleLeft RefCat ruleValue),
        either (const Nothing) (Just . refObWeight) (pbpoRuleRight RefCat ruleValue),
        either (const Nothing) (Just . refObWeight) (pbpoRuleContextType RefCat ruleValue)
      ]
  where
    ruleValue =
      pfPermissiveRule fixture

applyPBPOWeight :: PBPOFixture -> Maybe Int
applyPBPOWeight fixture =
  either (const Nothing) (Just . pbpoUntypedStepWeight) $
    applyPBPO RefCat (pfIdentityRule fixture) (pfMonic fixture)

applyPBPOPlusPermissiveWeight :: PBPOFixture -> Maybe Int
applyPBPOPlusPermissiveWeight fixture =
  either (const Nothing) (Just . pbpoStepWeight) $
    applyPBPOPlus RefCat (pfPermissiveRule fixture) (pfMatch fixture)

applyPBPOPlusRestrictiveWeight :: PBPOFixture -> Maybe Int
applyPBPOPlusRestrictiveWeight fixture =
  either (const Nothing) (Just . pbpoStepWeight) $
    applyPBPOPlus RefCat (pfRestrictiveRule fixture) (pfMatch fixture)

identityTypedRuleValue :: Either String (PBPORule RefCat String)
identityTypedRuleValue = do
  leftLeg <- leftLegValue
  rightLeg <- rightLegValue
  either (Left . show) Right $
    identityTypedRule RefCat "identity-typed" leftLeg rightLeg

permissiveRuleValue :: Either String (PBPORule RefCat String)
permissiveRuleValue =
  typedRuleValue "permissive" [1, 3]

restrictiveRuleValue :: Either String (PBPORule RefCat String)
restrictiveRuleValue =
  typedRuleValue "restrictive" [1]

typedRuleValue :: String -> [Int] -> Either String (PBPORule RefCat String)
typedRuleValue label contextInterfaceRefs = do
  leftLeg <- leftLegValue
  rightLeg <- rightLegValue
  leftTyping <- refIncl [1, 2] [1, 2, 3]
  interfaceTyping <- refIncl [1] contextInterfaceRefs
  contextLeg <- refIncl contextInterfaceRefs [1, 2, 3]
  either (Left . show) Right $
    mkPBPORule
      RefCat
      label
      PBPOLegs
        { plLeftLeg = leftLeg,
          plRightLeg = rightLeg,
          plLeftTyping = leftTyping,
          plInterfaceTyping = interfaceTyping,
          plContextLeg = contextLeg
        }

leftLegValue :: Either String RefMor
leftLegValue =
  refIncl [1] [1, 2]

rightLegValue :: Either String RefMor
rightLegValue =
  refIncl [1] [1, 4]

refIncl :: [Int] -> [Int] -> Either String RefMor
refIncl fromRefs toRefs =
  maybe
    (Left ("missing inclusion " <> show fromRefs <> " into " <> show toRefs))
    Right
    (refInclusion (refs fromRefs) (refs toRefs))

refs :: [Int] -> IntSet
refs =
  IntSet.fromList

pbpoRuleWeight :: PBPORule RefCat String -> Int
pbpoRuleWeight ruleValue =
  sum
    [ refMorWeight (pbpoRuleLeftLeg ruleValue),
      refMorWeight (pbpoRuleRightLeg ruleValue),
      refMorWeight (pbpoRuleLeftTyping ruleValue),
      refMorWeight (pbpoRuleInterfaceTyping ruleValue),
      refMorWeight (pbpoRuleContextLeg ruleValue)
    ]

pbpoUntypedStepWeight :: PBPOUntypedStep RefCat String -> Int
pbpoUntypedStepWeight stepValue =
  refObWeight (pbpoUntypedHost stepValue)
    + refMorWeight (pbpoUntypedContextToHost stepValue)
    + refMorWeight (pbpoUntypedReplacementToHost stepValue)

pbpoStepWeight :: PBPOStep RefCat String -> Int
pbpoStepWeight stepValue =
  refObWeight (pbpoStepHost stepValue)
    + refObWeight (pbpoStepContext stepValue)
    + refMorWeight (pbpoStepContextToHost stepValue)
    + refMorWeight (pbpoStepReplacementToHost stepValue)

refMorWeight :: RefMor -> Int
refMorWeight morphism =
  setWeight (refMorFrom morphism) + setWeight (refMorTo morphism)

refObWeight :: RefOb -> Int
refObWeight =
  setWeight . refObRefs

setWeight :: IntSet -> Int
setWeight values =
  IntSet.size values + IntSet.foldl' (+) 0 values
