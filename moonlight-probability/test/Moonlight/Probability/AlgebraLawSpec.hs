module Moonlight.Probability.AlgebraLawSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Probability.Core
  ( Prob,
    mkProb,
    positiveProbOne,
    positiveProbValue,
    probOne,
    probValue,
    probZero,
  )
import Moonlight.Probability.Distribution.Categorical
  ( Categorical,
    blendCategorical,
    categoricalCollapseAt,
    categoricalFoldMap,
    categoricalLookup,
    categoricalRestrict,
    categoricalSupport,
    categoricalTraverse,
    certainCategorical,
    uniformCategorical,
  )
import Moonlight.Probability.Distribution.Finite
  ( FiniteDistribution,
    blendFiniteDistribution,
    certainFiniteDistribution,
    finiteFoldMap,
    finiteLookup,
    finiteRestrict,
    finiteSupport,
    finiteTraverse,
    sampleAt,
    uniformFiniteDistribution,
  )
import Moonlight.Probability.TestSupport.Generators
  ( PositiveWeightSample,
    supportFromPositiveWeights,
    withCategoricalFromPositiveWeights,
    withDisjointCategoricalPairFromPositiveWeights,
    withDisjointFiniteDistributionPairFromPositiveWeights,
    withFiniteDistributionFromPositiveWeights,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Positive (..), Property, counterexample, testProperty)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "algebra-laws"
    [ testGroup
        "constructors"
        [ testProperty "certain finite is singleton and deterministic" propCertainFiniteConstructor,
          testProperty "uniform finite preserves support and equal weights" propUniformFiniteConstructor,
          testProperty "certain categorical is singleton and deterministic" propCertainCategoricalConstructor,
          testProperty "uniform categorical preserves support and equal weights" propUniformCategoricalConstructor
        ],
      testGroup
        "finite"
        [ testProperty "fold support matches support projection" propFiniteFoldSupportConsistency,
          testProperty "lookup agrees with support" propFiniteLookupSupportAgreement,
          testProperty "traverse preserves normalization" propFiniteTraversePreservesNormalization,
          testProperty "restrict preserves subset laws" propFiniteRestrictPreservesSubsetLaws,
          testProperty "restrict composes support and lookup" propFiniteRestrictLookupSupportComposition,
          testProperty "blend preserves normalization and unioned support" propFiniteBlendPreservesNormalizationAndUnionedSupport
        ],
      testGroup
        "categorical"
        [ testProperty "fold support matches support projection" propCategoricalFoldSupportConsistency,
          testProperty "lookup agrees with support" propCategoricalLookupSupportAgreement,
          testProperty "traverse preserves normalization" propCategoricalTraversePreservesNormalization,
          testProperty "restrict preserves subset laws" propCategoricalRestrictPreservesSubsetLaws,
          testProperty "restrict composes support and lookup" propCategoricalRestrictLookupSupportComposition,
          testProperty "blend preserves normalization and unioned support" propCategoricalBlendPreservesNormalizationAndUnionedSupport,
          testProperty "collapse is monotone over ordered thresholds" propCategoricalCollapseMonotone
        ],
      testGroup
        "categorical-facade"
        [ testProperty "support and lookup agree with finite core" propCategoricalFacadeSupportAndLookupAgreement,
          testProperty "restrict agrees with finite core" propCategoricalFacadeRestrictAgreement,
          testProperty "traverse agrees with finite core" propCategoricalFacadeTraverseAgreement,
          testProperty "blend agrees with finite core" propCategoricalFacadeBlendAgreement,
          testProperty "collapse agrees with finite core" propCategoricalFacadeCollapseAgreement
        ]
    ]

propCertainFiniteConstructor :: Int -> Property
propCertainFiniteConstructor outcome =
  let distribution = certainFiniteDistribution outcome
   in counterexample
        ( "support="
            <> show (finiteSupport distribution)
            <> ", lookup="
            <> show (finiteLookup outcome distribution)
        )
        ( finiteSupport distribution == Set.singleton outcome
            && finiteLookup outcome distribution == Just probOne
            && sampleAt probZero distribution == outcome
            && sampleAt probOne distribution == outcome
            && approxEq 1.0e-12 (totalFiniteProbability distribution) 1.0
        )

propUniformFiniteConstructor :: PositiveWeightSample -> Property
propUniformFiniteConstructor weightSample =
  let support = supportFromPositiveWeights weightSample
   in case uniformFiniteDistribution support of
        Left err -> counterexample (show err) False
        Right distribution ->
          let expectedSupport = Set.fromList (NonEmpty.toList support)
              expectedProbability = uniformProbability support
              probabilities = fmap (`finiteLookup` distribution) (NonEmpty.toList support)
           in counterexample
                ( "support="
                    <> show (finiteSupport distribution)
                    <> ", probabilities="
                    <> show probabilities
                )
                ( finiteSupport distribution == expectedSupport
                    && all (matchesUniformProbability expectedProbability) probabilities
                    && approxEq 1.0e-12 (totalFiniteProbability distribution) 1.0
                )

propCertainCategoricalConstructor :: Int -> Property
propCertainCategoricalConstructor outcome =
  let categorical = certainCategorical outcome
   in counterexample
        ( "support="
            <> show (categoricalSupport categorical)
            <> ", lookup="
            <> show (categoricalLookup outcome categorical)
        )
        ( categoricalSupport categorical == Set.singleton outcome
            && categoricalLookup outcome categorical == Just probOne
            && categoricalCollapseAt probZero categorical == outcome
            && categoricalCollapseAt probOne categorical == outcome
            && approxEq 1.0e-12 (totalCategoricalProbability categorical) 1.0
        )

propUniformCategoricalConstructor :: PositiveWeightSample -> Property
propUniformCategoricalConstructor weightSample =
  let support = supportFromPositiveWeights weightSample
   in case uniformCategorical support of
        Left err -> counterexample (show err) False
        Right categorical ->
          let expectedSupport = Set.fromList (NonEmpty.toList support)
              expectedProbability = uniformProbability support
              probabilities = fmap (`categoricalLookup` categorical) (NonEmpty.toList support)
           in counterexample
                ( "support="
                    <> show (categoricalSupport categorical)
                    <> ", probabilities="
                    <> show probabilities
                )
                ( categoricalSupport categorical == expectedSupport
                    && all (matchesUniformProbability expectedProbability) probabilities
                    && approxEq 1.0e-12 (totalCategoricalProbability categorical) 1.0
                )

propFiniteFoldSupportConsistency :: PositiveWeightSample -> Property
propFiniteFoldSupportConsistency weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let support = finiteSupport distribution
           foldedSupport = finiteFoldMap (Set.singleton . fst) distribution
        in counterexample
             ("support=" <> show support <> ", foldedSupport=" <> show foldedSupport)
             (support == foldedSupport)
    )

propFiniteLookupSupportAgreement :: PositiveWeightSample -> Property
propFiniteLookupSupportAgreement weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let support = finiteSupport distribution
           absentOutcome = missingOutcome support
           supportedLookupsResolve = all (isJust . (`finiteLookup` distribution)) (Set.toAscList support)
           absentLookupMisses = isNothing (finiteLookup absentOutcome distribution)
        in counterexample
             ( "support="
                 <> show support
                 <> ", absentOutcome="
                 <> show absentOutcome
                 <> ", absentLookup="
                 <> show (finiteLookup absentOutcome distribution)
             )
             (supportedLookupsResolve && absentLookupMisses)
    )

propFiniteTraversePreservesNormalization :: PositiveWeightSample -> Property
propFiniteTraversePreservesNormalization weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let Identity traversed =
             finiteTraverse (\(outcome, _) -> Identity (outcome `mod` 3)) distribution
           totalProbability = totalFiniteProbability traversed
        in counterexample
             ("totalProbability=" <> show totalProbability)
             (approxEq 1.0e-12 totalProbability 1.0)
    )

propFiniteRestrictPreservesSubsetLaws :: PositiveWeightSample -> Property
propFiniteRestrictPreservesSubsetLaws weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let support = finiteSupport distribution
           absentOutcome = missingOutcome support
           expectedSupport = retainedSupport support
           allowedSupport = Set.insert absentOutcome expectedSupport
           restricted = finiteRestrict allowedSupport distribution
           disjointRestriction = finiteRestrict (Set.singleton absentOutcome) distribution
        in case restricted of
             Nothing ->
               counterexample
                 ("expected restricted finite distribution for support=" <> show support)
                 False
             Just restrictedDistribution ->
               let restrictedSupport = finiteSupport restrictedDistribution
                   totalProbability = totalFiniteProbability restrictedDistribution
                in counterexample
                     ( "support="
                         <> show support
                         <> ", allowedSupport="
                         <> show allowedSupport
                         <> ", restrictedSupport="
                         <> show restrictedSupport
                         <> ", disjointRestriction="
                         <> show disjointRestriction
                         <> ", totalProbability="
                         <> show totalProbability
                     )
                     ( restrictedSupport == expectedSupport
                         && isNothing disjointRestriction
                         && approxEq 1.0e-12 totalProbability 1.0
                     )
    )

propFiniteRestrictLookupSupportComposition :: PositiveWeightSample -> Property
propFiniteRestrictLookupSupportComposition weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let support = finiteSupport distribution
           allowedSupport = retainedSupport support
           outcomes = candidateOutcomes support
        in case finiteRestrict allowedSupport distribution of
             Nothing ->
               counterexample
                 ("expected finite restriction to retain support=" <> show allowedSupport)
                 False
             Just restrictedDistribution ->
               let restrictedSupport = finiteSupport restrictedDistribution
                   membershipTrace =
                     fmap
                       (\outcome ->
                          ( outcome,
                            Set.member outcome allowedSupport,
                            isJust (finiteLookup outcome restrictedDistribution)
                          )
                       )
                       outcomes
                   mismatches =
                     filter
                       (\(_, expectedMember, actualMember) -> expectedMember /= actualMember)
                       membershipTrace
                in counterexample
                     ( "restrictedSupport="
                         <> show restrictedSupport
                         <> ", allowedSupport="
                         <> show allowedSupport
                         <> ", membershipTrace="
                         <> show membershipTrace
                         <> ", mismatches="
                         <> show mismatches
                     )
                     (restrictedSupport == allowedSupport && null mismatches)
    )

propFiniteBlendPreservesNormalizationAndUnionedSupport ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  Property
propFiniteBlendPreservesNormalizationAndUnionedSupport leftWeights rightWeights =
  withDisjointFiniteDistributionPairFromPositiveWeights
    leftWeights
    rightWeights
    (\leftDistribution rightDistribution ->
       let blended =
             blendFiniteDistribution
               ((positiveProbOne, leftDistribution) :| [(positiveProbOne, rightDistribution)])
           expectedSupport = Set.union (finiteSupport leftDistribution) (finiteSupport rightDistribution)
           totalProbability = totalFiniteProbability blended
        in counterexample
             ( "expectedSupport="
                 <> show expectedSupport
                 <> ", blendedSupport="
                 <> show (finiteSupport blended)
                 <> ", totalProbability="
                 <> show totalProbability
             )
             (finiteSupport blended == expectedSupport && approxEq 1.0e-12 totalProbability 1.0)
    )

propCategoricalFoldSupportConsistency :: PositiveWeightSample -> Property
propCategoricalFoldSupportConsistency weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let support = categoricalSupport categorical
           foldedSupport = categoricalFoldMap (Set.singleton . fst) categorical
        in counterexample
             ("support=" <> show support <> ", foldedSupport=" <> show foldedSupport)
             (support == foldedSupport)
    )

propCategoricalLookupSupportAgreement :: PositiveWeightSample -> Property
propCategoricalLookupSupportAgreement weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let support = categoricalSupport categorical
           absentOutcome = missingOutcome support
           supportedLookupsResolve = all (isJust . (`categoricalLookup` categorical)) (Set.toAscList support)
           absentLookupMisses = isNothing (categoricalLookup absentOutcome categorical)
        in counterexample
             ( "support="
                 <> show support
                 <> ", absentOutcome="
                 <> show absentOutcome
                 <> ", absentLookup="
                 <> show (categoricalLookup absentOutcome categorical)
             )
             (supportedLookupsResolve && absentLookupMisses)
    )

propCategoricalTraversePreservesNormalization :: PositiveWeightSample -> Property
propCategoricalTraversePreservesNormalization weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let Identity traversed =
             categoricalTraverse (\(outcome, _) -> Identity (outcome `mod` 3)) categorical
           totalProbability = totalCategoricalProbability traversed
        in counterexample
             ("totalProbability=" <> show totalProbability)
             (approxEq 1.0e-12 totalProbability 1.0)
    )

propCategoricalRestrictPreservesSubsetLaws :: PositiveWeightSample -> Property
propCategoricalRestrictPreservesSubsetLaws weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let support = categoricalSupport categorical
           absentOutcome = missingOutcome support
           expectedSupport = retainedSupport support
           allowedSupport = Set.insert absentOutcome expectedSupport
           restricted = categoricalRestrict allowedSupport categorical
           disjointRestriction = categoricalRestrict (Set.singleton absentOutcome) categorical
        in case restricted of
             Nothing ->
               counterexample
                 ("expected restricted categorical for support=" <> show support)
                 False
             Just restrictedCategorical ->
               let restrictedSupport = categoricalSupport restrictedCategorical
                   totalProbability = totalCategoricalProbability restrictedCategorical
                in counterexample
                     ( "support="
                         <> show support
                         <> ", allowedSupport="
                         <> show allowedSupport
                         <> ", restrictedSupport="
                         <> show restrictedSupport
                         <> ", disjointRestriction="
                         <> show disjointRestriction
                         <> ", totalProbability="
                         <> show totalProbability
                     )
                     ( restrictedSupport == expectedSupport
                         && isNothing disjointRestriction
                         && approxEq 1.0e-12 totalProbability 1.0
                     )
    )

propCategoricalRestrictLookupSupportComposition :: PositiveWeightSample -> Property
propCategoricalRestrictLookupSupportComposition weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let support = categoricalSupport categorical
           allowedSupport = retainedSupport support
           outcomes = candidateOutcomes support
        in case categoricalRestrict allowedSupport categorical of
             Nothing ->
               counterexample
                 ("expected categorical restriction to retain support=" <> show allowedSupport)
                 False
             Just restrictedCategorical ->
               let restrictedSupport = categoricalSupport restrictedCategorical
                   membershipTrace =
                     fmap
                       (\outcome ->
                          ( outcome,
                            Set.member outcome allowedSupport,
                            isJust (categoricalLookup outcome restrictedCategorical)
                          )
                       )
                       outcomes
                   mismatches =
                     filter
                       (\(_, expectedMember, actualMember) -> expectedMember /= actualMember)
                       membershipTrace
                in counterexample
                     ( "restrictedSupport="
                         <> show restrictedSupport
                         <> ", allowedSupport="
                         <> show allowedSupport
                         <> ", membershipTrace="
                         <> show membershipTrace
                         <> ", mismatches="
                         <> show mismatches
                     )
                     (restrictedSupport == allowedSupport && null mismatches)
    )

propCategoricalBlendPreservesNormalizationAndUnionedSupport ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  Property
propCategoricalBlendPreservesNormalizationAndUnionedSupport leftWeights rightWeights =
  withDisjointCategoricalPairFromPositiveWeights
    leftWeights
    rightWeights
    (\leftCategorical rightCategorical ->
       let blended =
             blendCategorical
               ((positiveProbOne, leftCategorical) :| [(positiveProbOne, rightCategorical)])
           expectedSupport = Set.union (categoricalSupport leftCategorical) (categoricalSupport rightCategorical)
           totalProbability = totalCategoricalProbability blended
        in counterexample
             ( "expectedSupport="
                 <> show expectedSupport
                 <> ", blendedSupport="
                 <> show (categoricalSupport blended)
                 <> ", totalProbability="
                 <> show totalProbability
             )
             (categoricalSupport blended == expectedSupport && approxEq 1.0e-12 totalProbability 1.0)
    )

propCategoricalCollapseMonotone ::
  PositiveWeightSample ->
  Positive Int ->
  Positive Int ->
  Positive Int ->
  Property
propCategoricalCollapseMonotone weightSample leftSeed rightSeed denominatorSeed =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       case orderedProbabilities leftSeed rightSeed denominatorSeed of
         Left message -> counterexample message False
         Right (leftThreshold, rightThreshold) ->
           let leftOutcome = categoricalCollapseAt leftThreshold categorical
               rightOutcome = categoricalCollapseAt rightThreshold categorical
            in counterexample
                 ( "leftThreshold="
                     <> show leftThreshold
                     <> ", rightThreshold="
                     <> show rightThreshold
                     <> ", leftOutcome="
                     <> show leftOutcome
                     <> ", rightOutcome="
                     <> show rightOutcome
                 )
                 (leftOutcome <= rightOutcome)
    )

propCategoricalFacadeSupportAndLookupAgreement :: PositiveWeightSample -> Property
propCategoricalFacadeSupportAndLookupAgreement weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       withCategoricalFromPositiveWeights
         weightSample
         (\categorical ->
            let support = finiteSupport distribution
                supportMatches = categoricalSupport categorical == support
             in counterexample
                  ( "finiteSupport="
                      <> show support
                      <> ", categoricalSupport="
                      <> show (categoricalSupport categorical)
                  )
                  (supportMatches && lookupAgreement (candidateOutcomes support) (`finiteLookup` distribution) (`categoricalLookup` categorical))
         )
    )

propCategoricalFacadeRestrictAgreement :: PositiveWeightSample -> Property
propCategoricalFacadeRestrictAgreement weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       withCategoricalFromPositiveWeights
         weightSample
         (\categorical ->
            let support = finiteSupport distribution
                allowedSupport = retainedSupport support
                finiteRestricted = finiteRestrict allowedSupport distribution
                categoricalRestricted = categoricalRestrict allowedSupport categorical
                supportMatches = fmap finiteSupport finiteRestricted == fmap categoricalSupport categoricalRestricted
                lookupMatches outcome =
                  (finiteRestricted >>= finiteLookup outcome)
                    == (categoricalRestricted >>= categoricalLookup outcome)
                lookupTrace =
                  fmap
                    (\outcome ->
                       ( outcome,
                         finiteRestricted >>= finiteLookup outcome,
                         categoricalRestricted >>= categoricalLookup outcome
                       )
                    )
                    (candidateOutcomes support)
             in counterexample
                  ( "finiteRestrictedSupport="
                      <> show (fmap finiteSupport finiteRestricted)
                      <> ", categoricalRestrictedSupport="
                      <> show (fmap categoricalSupport categoricalRestricted)
                      <> ", lookupTrace="
                      <> show lookupTrace
                  )
                  (supportMatches && all lookupMatches (candidateOutcomes support))
         )
    )

propCategoricalFacadeTraverseAgreement :: PositiveWeightSample -> Property
propCategoricalFacadeTraverseAgreement weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       withCategoricalFromPositiveWeights
         weightSample
         (\categorical ->
            let Identity traversedFinite =
                  finiteTraverse (\(outcome, _) -> Identity (outcome `mod` 3)) distribution
                Identity traversedCategorical =
                  categoricalTraverse (\(outcome, _) -> Identity (outcome `mod` 3)) categorical
                combinedSupport = Set.union (finiteSupport traversedFinite) (categoricalSupport traversedCategorical)
                supportMatches = categoricalSupport traversedCategorical == finiteSupport traversedFinite
                lookupMatches =
                  lookupAgreement
                    (candidateOutcomes combinedSupport)
                    (`finiteLookup` traversedFinite)
                    (`categoricalLookup` traversedCategorical)
             in counterexample
                  ( "finiteSupport="
                      <> show (finiteSupport traversedFinite)
                      <> ", categoricalSupport="
                      <> show (categoricalSupport traversedCategorical)
                  )
                  (supportMatches && lookupMatches)
         )
    )

propCategoricalFacadeBlendAgreement ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  Property
propCategoricalFacadeBlendAgreement leftWeights rightWeights =
  withDisjointFiniteDistributionPairFromPositiveWeights
    leftWeights
    rightWeights
    (\leftDistribution rightDistribution ->
       withDisjointCategoricalPairFromPositiveWeights
         leftWeights
         rightWeights
         (\leftCategorical rightCategorical ->
            let blendedFinite =
                  blendFiniteDistribution
                    ((positiveProbOne, leftDistribution) :| [(positiveProbOne, rightDistribution)])
                blendedCategorical =
                  blendCategorical
                    ((positiveProbOne, leftCategorical) :| [(positiveProbOne, rightCategorical)])
                combinedSupport = Set.union (finiteSupport blendedFinite) (categoricalSupport blendedCategorical)
                supportMatches = categoricalSupport blendedCategorical == finiteSupport blendedFinite
                lookupMatches =
                  lookupAgreement
                    (candidateOutcomes combinedSupport)
                    (`finiteLookup` blendedFinite)
                    (`categoricalLookup` blendedCategorical)
             in counterexample
                  ( "finiteSupport="
                      <> show (finiteSupport blendedFinite)
                      <> ", categoricalSupport="
                      <> show (categoricalSupport blendedCategorical)
                  )
                  (supportMatches && lookupMatches)
         )
    )

propCategoricalFacadeCollapseAgreement ::
  PositiveWeightSample ->
  Positive Int ->
  Positive Int ->
  Property
propCategoricalFacadeCollapseAgreement weightSample numeratorSeed denominatorSeed =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       withCategoricalFromPositiveWeights
         weightSample
         (\categorical ->
            case quantizedProbability numeratorSeed denominatorSeed of
              Left message -> counterexample message False
              Right threshold ->
                let finiteOutcome = sampleAt threshold distribution
                    categoricalOutcome = categoricalCollapseAt threshold categorical
                 in counterexample
                      ( "threshold="
                          <> show threshold
                          <> ", finiteOutcome="
                          <> show finiteOutcome
                          <> ", categoricalOutcome="
                          <> show categoricalOutcome
                      )
                      (finiteOutcome == categoricalOutcome)
         )
    )

totalFiniteProbability :: FiniteDistribution Int -> Double
totalFiniteProbability distribution =
  getSum (finiteFoldMap (Sum . positiveProbValue . snd) distribution)

totalCategoricalProbability :: Categorical Int -> Double
totalCategoricalProbability categorical =
  getSum (categoricalFoldMap (Sum . positiveProbValue . snd) categorical)

uniformProbability :: NonEmpty a -> Double
uniformProbability support =
  1.0 / fromIntegral (length (NonEmpty.toList support))

matchesUniformProbability :: Double -> Maybe Prob -> Bool
matchesUniformProbability expectedProbability maybeProbability =
  maybe False (approxEq 1.0e-12 expectedProbability . probValue) maybeProbability

retainedSupport :: Set Int -> Set Int
retainedSupport = Set.filter even

missingOutcome :: Set Int -> Int
missingOutcome = Set.size

candidateOutcomes :: Set Int -> [Int]
candidateOutcomes support =
  Set.toAscList (Set.insert (missingOutcome support) support)

lookupAgreement :: Eq probability => [Int] -> (Int -> Maybe probability) -> (Int -> Maybe probability) -> Bool
lookupAgreement outcomes leftLookup rightLookup =
  let lookupTrace =
        fmap
          (\outcome -> (outcome, leftLookup outcome, rightLookup outcome))
          outcomes
      mismatches =
        filter
          (\(_, leftProbability, rightProbability) -> leftProbability /= rightProbability)
          lookupTrace
   in null mismatches

orderedProbabilities :: Positive Int -> Positive Int -> Positive Int -> Either String (Prob, Prob)
orderedProbabilities leftSeed rightSeed denominatorSeed =
  let leftThreshold = quantizedThreshold leftSeed denominatorSeed
      rightThreshold = quantizedThreshold rightSeed denominatorSeed
   in case (mkProb (min leftThreshold rightThreshold), mkProb (max leftThreshold rightThreshold)) of
        (Right lower, Right upper) -> Right (lower, upper)
        (Left err, _) -> Left (show err)
        (_, Left err) -> Left (show err)

quantizedProbability :: Positive Int -> Positive Int -> Either String Prob
quantizedProbability numeratorSeed denominatorSeed =
  case mkProb (quantizedThreshold numeratorSeed denominatorSeed) of
    Left err -> Left (show err)
    Right probability -> Right probability

quantizedThreshold :: Positive Int -> Positive Int -> Double
quantizedThreshold numeratorSeed denominatorSeed =
  let denominator = getPositive denominatorSeed
      numerator = getPositive numeratorSeed `mod` (denominator + 1)
   in fromIntegral numerator / fromIntegral denominator
