{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.TestFixture.SheafClassLaws
  ( SheafClassLawsFixture (..),
    StalkGluingSample (..),
    StalkMergeLawsFixture (..),
    sheafClassLawsTests,
    stalkMergeLawTests,
  )
where

import Data.Maybe (isNothing)
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionArrow (..),
  )
import Moonlight.Sheaf.Section.Restriction (RestrictionIndex)
import Moonlight.Sheaf.Section.Restriction.Law
  ( checkRestrictionCompositionLaw,
    checkRestrictionIdentityLaw,
  )
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction,
    StalkAlgebra,
    mergeStalks,
    normalizeStalk,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC

data SheafClassLawsFixture cell stalk mismatch witness repairObstruction = SheafClassLawsFixture
  { sclfName :: !String,
    sclfStalkAlgebra :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    sclfRestrictions :: !(RestrictionIndex cell witness),
    sclfGenStalk :: !(QC.Gen stalk),
    sclfGenTripleOfCells :: !(QC.Gen (cell, cell, cell)),
    sclfGenSelfCell :: !(QC.Gen cell)
  }

data StalkGluingSample stalk = StalkGluingSample
  { sgsFirstStalk :: !stalk,
    sgsSecondStalk :: !stalk,
    sgsThirdStalk :: !stalk,
    sgsExpectedGluedStalk :: !stalk
  }
  deriving stock (Eq, Show)

data StalkMergeLawsFixture witness stalk mismatch repairObstruction = StalkMergeLawsFixture
  { smlfName :: !String,
    smlfStalkAlgebra :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    smlfGenStalk :: !(QC.Gen stalk),
    smlfGenCompatiblePair :: !(QC.Gen (stalk, stalk)),
    smlfGenGluingSample :: !(QC.Gen (StalkGluingSample stalk)),
    smlfLeq :: !(stalk -> stalk -> Bool)
  }

sheafClassLawsTests ::
  (Ord cell, Show cell, Show stalk, Show mismatch) =>
  SheafClassLawsFixture cell stalk mismatch witness repairObstruction ->
  TestTree
sheafClassLawsTests fixture =
  testGroup
    "restriction-laws"
    [ testGroup
        (sclfName fixture)
        [ testGroup
            "composition"
            [ QC.testProperty
                "restriction composes on the thin surface"
                (propRestrictionComposition fixture)
            ],
          testGroup
            "identity"
            [ QC.testProperty
                "restriction is identity on the thin surface"
                (propRestrictionIdentity fixture)
            ]
        ]
    ]

stalkMergeLawTests ::
  (Eq stalk, Show stalk, Show mismatch) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  TestTree
stalkMergeLawTests fixture =
  testGroup
    (smlfName fixture)
    [ QC.testProperty
        "merge is idempotent"
        (propMergeIdempotent fixture),
      QC.testProperty
        "merge verdict and value commute"
        (propMergeCommutative fixture),
      QC.testProperty
        "merge associates through the success channel"
        (propMergeAssociative fixture),
      QC.testProperty
        "merge bounds both compatible inputs from above"
        (propMergeUpperBound fixture),
      QC.testProperty
        "compatible local stalks glue associatively to their expected union"
        (propGluingAssociative fixture),
      QC.testProperty
        "normalize is idempotent"
        (propNormalizeIdempotent fixture)
    ]

propRestrictionComposition ::
  (Ord cell, Show cell, Show stalk, Show mismatch) =>
  SheafClassLawsFixture cell stalk mismatch witness repairObstruction ->
  QC.Property
propRestrictionComposition fixture =
  QC.withNumTests 100 $
    QC.forAll (sclfGenTripleOfCells fixture) $ \(cellA, cellB, cellC) ->
      QC.forAll (sclfGenStalk fixture) $ \stalkValue ->
        let lawFailure =
              checkRestrictionCompositionLaw
                (sclfStalkAlgebra fixture)
                (sclfRestrictions fixture)
                (RestrictionArrow cellA cellB)
                (RestrictionArrow cellB cellC)
                stalkValue
         in QC.counterexample (show lawFailure) (isNothing lawFailure)

propRestrictionIdentity ::
  (Ord cell, Show cell, Show stalk, Show mismatch) =>
  SheafClassLawsFixture cell stalk mismatch witness repairObstruction ->
  QC.Property
propRestrictionIdentity fixture =
  QC.withNumTests 100 $
    QC.forAll (sclfGenSelfCell fixture) $ \cellValue ->
      QC.forAll (sclfGenStalk fixture) $ \stalkValue ->
        let lawFailure =
              checkRestrictionIdentityLaw
                (sclfStalkAlgebra fixture)
                (sclfRestrictions fixture)
                cellValue
                stalkValue
         in QC.counterexample (show lawFailure) (isNothing lawFailure)

propMergeIdempotent ::
  (Eq stalk, Show stalk, Show mismatch) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propMergeIdempotent fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenStalk fixture) $ \stalkValue ->
      expectRightStalk
        "idempotent merge"
        stalkValue
        (mergeStalks (smlfStalkAlgebra fixture) stalkValue stalkValue)

propMergeCommutative ::
  (Eq stalk, Show stalk) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propMergeCommutative fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenStalk fixture) $ \leftStalk ->
      QC.forAll (smlfGenStalk fixture) $ \rightStalk ->
        mergeValue (mergeStalks (smlfStalkAlgebra fixture) leftStalk rightStalk)
          == mergeValue (mergeStalks (smlfStalkAlgebra fixture) rightStalk leftStalk)

propMergeAssociative ::
  (Eq stalk, Show stalk) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propMergeAssociative fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenStalk fixture) $ \firstStalk ->
      QC.forAll (smlfGenStalk fixture) $ \secondStalk ->
        QC.forAll (smlfGenStalk fixture) $ \thirdStalk ->
          mergeValue
            ( mergeStalks (smlfStalkAlgebra fixture) firstStalk secondStalk
                >>= \merged -> mergeStalks (smlfStalkAlgebra fixture) merged thirdStalk
            )
            == mergeValue
              ( mergeStalks (smlfStalkAlgebra fixture) secondStalk thirdStalk
                  >>= mergeStalks (smlfStalkAlgebra fixture) firstStalk
              )

propMergeUpperBound ::
  (Show stalk, Show mismatch) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propMergeUpperBound fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenCompatiblePair fixture) $ \(leftStalk, rightStalk) ->
      case mergeStalks (smlfStalkAlgebra fixture) leftStalk rightStalk of
        Left obstruction ->
          QC.counterexample
            ("compatible merge failed: " <> show obstruction)
            False
        Right mergedStalk ->
          QC.counterexample
            ("merged stalk is not an upper bound: " <> show (leftStalk, rightStalk, mergedStalk))
            (smlfLeq fixture leftStalk mergedStalk && smlfLeq fixture rightStalk mergedStalk)

propGluingAssociative ::
  (Eq stalk, Show stalk, Show mismatch) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propGluingAssociative fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenGluingSample fixture) $ \gluingSample ->
      let firstStalk = sgsFirstStalk gluingSample
          secondStalk = sgsSecondStalk gluingSample
          thirdStalk = sgsThirdStalk gluingSample
          expectedStalk = sgsExpectedGluedStalk gluingSample
          leftAssociated =
            mergeStalks (smlfStalkAlgebra fixture) firstStalk secondStalk
              >>= \merged -> mergeStalks (smlfStalkAlgebra fixture) merged thirdStalk
          rightAssociated =
            mergeStalks (smlfStalkAlgebra fixture) secondStalk thirdStalk
              >>= mergeStalks (smlfStalkAlgebra fixture) firstStalk
       in QC.conjoin
            [ expectRightStalk "left-associated gluing" expectedStalk leftAssociated,
              expectRightStalk "right-associated gluing" expectedStalk rightAssociated
            ]

propNormalizeIdempotent ::
  (Eq stalk, Show stalk) =>
  StalkMergeLawsFixture witness stalk mismatch repairObstruction ->
  QC.Property
propNormalizeIdempotent fixture =
  QC.withNumTests 100 $
    QC.forAll (smlfGenStalk fixture) $ \stalkValue ->
      let normalizedStalk =
            normalizeStalk (smlfStalkAlgebra fixture) stalkValue
       in QC.counterexample
            ("normalize is not idempotent: " <> show stalkValue)
            (normalizeStalk (smlfStalkAlgebra fixture) normalizedStalk == normalizedStalk)

mergeValue :: Either obstruction stalk -> Maybe stalk
mergeValue =
  either (const Nothing) Just

expectRightStalk ::
  (Eq stalk, Show stalk, Show mismatch) =>
  String ->
  stalk ->
  Either (MergeObstruction mismatch) stalk ->
  QC.Property
expectRightStalk lawName expectedStalk mergeResult =
  case mergeResult of
    Left obstruction ->
      QC.counterexample
        (lawName <> " failed: " <> show obstruction)
        False
    Right actualStalk ->
      QC.counterexample
        (lawName <> " produced " <> show actualStalk <> " instead of " <> show expectedStalk)
        (actualStalk == expectedStalk)
