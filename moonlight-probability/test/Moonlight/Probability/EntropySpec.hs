module Moonlight.Probability.EntropySpec
  ( tests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Probability.Distribution.Categorical (certainCategorical, uniformCategorical)
import Moonlight.Probability.Entropy
  ( DivergenceError (..),
    divergenceValue,
    entropyValue,
    klDivergence,
    jsDivergence,
    renyiEntropy,
    shannonEntropy,
  )
import Moonlight.Probability.TestSupport.Generators
  ( CategoricalWeightSample,
    PerturbedCategoricalPair (..),
    defaultPerturbationMagnitudes,
    supportFromPositiveWeights,
    withCategoricalFromPositiveWeights,
    withDisjointCategoricalPairFromPositiveWeights,
    withNearIdenticalCategoricalPairFromPositiveWeights,
    withNearIdenticalCategoricalPairsAtScales,
    withOverlappingCategoricalPairFromPositiveWeights,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
  )

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "entropy"
    [ testProperty "Shannon entropy of certain categorical is zero" propCertainShannonZero,
      testProperty "Renyi entropy of certain categorical is zero" propCertainRenyiZero,
      testProperty "Shannon entropy of uniform categorical is log cardinality" propUniformShannonLogCardinality,
      testProperty "Renyi entropy of uniform categorical is log cardinality" propUniformRenyiLogCardinality,
      testProperty "KL(p || p) is zero" propKlIdentity,
      testProperty "KL reports support mismatch on disjoint supports" propKlDisjointSupportMismatch,
      testProperty "JS(p || p) is zero" propJsIdentity,
      testProperty "JS divergence is symmetric on overlapping supports" propJsSymmetry,
      testProperty "JS divergence is non-negative" propJsNonNegative,
      testProperty "JS divergence stays tiny under tiny perturbations" propJsTinyPerturbation,
      testProperty "JS divergence decays as perturbation shrinks" propJsPerturbationMonotone
    ]

propCertainShannonZero :: Int -> Property
propCertainShannonZero outcome =
  let entropy = entropyValue (shannonEntropy (certainCategorical outcome))
   in counterexample
        ("Shannon entropy was " <> show entropy)
        (approxEq 1.0e-12 entropy 0.0)

propCertainRenyiZero :: Int -> Property
propCertainRenyiZero outcome =
  case renyiEntropy 2.0 (certainCategorical outcome) of
    Left err -> counterexample (show err) False
    Right entropy ->
      counterexample
        ("Renyi entropy was " <> show (entropyValue entropy))
        (approxEq 1.0e-12 (entropyValue entropy) 0.0)

propUniformShannonLogCardinality :: CategoricalWeightSample -> Property
propUniformShannonLogCardinality weights =
  let support = supportFromPositiveWeights weights
      expectedEntropy = log (fromIntegral (length (NonEmpty.toList support)))
      entropy = entropyValue (shannonEntropy (uniformCategorical support))
   in counterexample
        ("uniform Shannon entropy=" <> show entropy <> ", expected=" <> show expectedEntropy)
        (approxEq 1.0e-12 entropy expectedEntropy)

propUniformRenyiLogCardinality :: CategoricalWeightSample -> Property
propUniformRenyiLogCardinality weights =
  let support = supportFromPositiveWeights weights
      expectedEntropy = log (fromIntegral (length (NonEmpty.toList support)))
   in case renyiEntropy 2.0 (uniformCategorical support) of
        Left err -> counterexample (show err) False
        Right entropy ->
          counterexample
            ("uniform Renyi entropy=" <> show (entropyValue entropy) <> ", expected=" <> show expectedEntropy)
            (approxEq 1.0e-12 (entropyValue entropy) expectedEntropy)

propKlIdentity :: CategoricalWeightSample -> Property
propKlIdentity weights =
  withCategoricalFromPositiveWeights
    weights
    (\categorical ->
       case klDivergence categorical categorical of
         Left err -> counterexample ("unexpected KL failure: " <> show err) False
         Right divergence ->
           counterexample
             ("KL divergence was " <> show (divergenceValue divergence))
             (approxEq 1.0e-12 (divergenceValue divergence) 0.0)
    )

propKlDisjointSupportMismatch :: CategoricalWeightSample -> CategoricalWeightSample -> Property
propKlDisjointSupportMismatch leftWeights rightWeights =
  withDisjointCategoricalPairFromPositiveWeights leftWeights rightWeights $
    \leftCategorical rightCategorical ->
      counterexample
        "expected KL support mismatch on disjoint supports"
        (klDivergence leftCategorical rightCategorical == Left DivergenceSupportMismatch)

propJsIdentity :: CategoricalWeightSample -> Property
propJsIdentity weights =
  withCategoricalFromPositiveWeights
    weights
    (\categorical ->
       let divergence = divergenceValue (jsDivergence categorical categorical)
        in counterexample
             ("JS divergence was " <> show divergence)
             (approxEq 1.0e-12 divergence 0.0)
    )

propJsSymmetry :: CategoricalWeightSample -> CategoricalWeightSample -> Property
propJsSymmetry leftWeights rightWeights =
  withOverlappingCategoricalPairFromPositiveWeights leftWeights rightWeights $
    \leftCategorical rightCategorical ->
      let forward = divergenceValue (jsDivergence leftCategorical rightCategorical)
          backward = divergenceValue (jsDivergence rightCategorical leftCategorical)
       in counterexample
            ("forward=" <> show forward <> ", backward=" <> show backward)
            (approxEq 1.0e-12 forward backward)

propJsNonNegative :: CategoricalWeightSample -> CategoricalWeightSample -> Property
propJsNonNegative leftWeights rightWeights =
  withOverlappingCategoricalPairFromPositiveWeights leftWeights rightWeights $
    \leftCategorical rightCategorical ->
      let divergence = divergenceValue (jsDivergence leftCategorical rightCategorical)
       in counterexample
            ("JS divergence was " <> show divergence)
            (divergence >= (-1.0e-12))

propJsTinyPerturbation :: CategoricalWeightSample -> Property
propJsTinyPerturbation weights =
  withNearIdenticalCategoricalPairFromPositiveWeights weights $
    \leftCategorical rightCategorical ->
      let divergence = divergenceValue (jsDivergence leftCategorical rightCategorical)
       in counterexample
            ("JS divergence under tiny perturbation was " <> show divergence)
            (divergence <= 1.0e-10)

propJsPerturbationMonotone :: CategoricalWeightSample -> Property
propJsPerturbationMonotone weights =
  withNearIdenticalCategoricalPairsAtScales defaultPerturbationMagnitudes weights $
    \perturbedPairs ->
      let divergenceProfile =
            fmap
              (\perturbedPair ->
                 ( perturbedMagnitude perturbedPair,
                   divergenceValue
                     ( jsDivergence
                         (perturbedReferenceCategorical perturbedPair)
                         (perturbedCandidateCategorical perturbedPair)
                     )
                 )
              )
              perturbedPairs
       in counterexample
            ("divergence profile=" <> show divergenceProfile)
            (nonIncreasingWithin 1.0e-18 (fmap snd (NonEmpty.toList divergenceProfile)))

nonIncreasingWithin :: Double -> [Double] -> Bool
nonIncreasingWithin tolerance divergences =
  and
    ( zipWith
        (\left right -> left + tolerance >= right)
        divergences
        (drop 1 divergences)
    )
