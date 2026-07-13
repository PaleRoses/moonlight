module Moonlight.Sheaf.Core.VerdictSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    SearchVerdict (..),
    acceptIfAnyAccepted,
    acceptedUnit,
    completeSearchVerdict,
    decidedSearchVerdict,
    rejectedFromList,
    searchVerdictDecided,
    searchVerdictObstructions,
    searchVerdictRefusals,
    verdictAllowed,
    verdictRejectedList,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    forAll,
    listOf,
    oneof,
    testProperty,
    (===),
    (==>),
  )

tests :: TestTree
tests =
  testGroup
    "verdict"
    [ testProperty "verdictRejectedList is a monoid homomorphism onto obstruction lists" propRejectedListHomomorphism,
      testProperty "mempty is the two-sided identity" propMonoidIdentity,
      testProperty "obstruction accumulation is associative" propSemigroupAssociativity,
      testProperty "rejectedFromList round-trips through verdictRejectedList" propRejectedListRoundTrip,
      testProperty "verdictAllowed holds exactly on empty obstruction lists" propAllowedIffNoObstructions,
      testProperty "acceptIfAnyAccepted accepts when any branch is allowed" propAnyAcceptedAccepts,
      testProperty "acceptIfAnyAccepted concatenates obstructions when nothing is allowed" propAllRejectedAccumulates,
      testGroup
        "search-verdict"
        [ testProperty "mempty is the two-sided identity" propSearchMonoidIdentity,
          testProperty "taint accumulation is associative" propSearchSemigroupAssociativity,
          testProperty "taint absorption preserves obstruction and refusal witnesses" propSearchTaintWitnesses,
          testProperty "decidedSearchVerdict is a monoid homomorphism" propDecidedSearchVerdictHomomorphism,
          testProperty "completeSearchVerdict round-trips decided verdicts" propCompleteDecidedSearchVerdict,
          testProperty "completion and combination decidedness agree with undecidedness" propSearchDecidedness
        ]
    ]

genVerdict :: Gen (ObstructionVerdict Int)
genVerdict =
  rejectedFromList <$> listOf arbitrary

genNonEmptyInt :: Gen (NonEmpty Int)
genNonEmptyInt =
  (:|) <$> arbitrary <*> listOf arbitrary

genNonEmptyBool :: Gen (NonEmpty Bool)
genNonEmptyBool =
  (:|) <$> arbitrary <*> listOf arbitrary

genNonEmptyIntList :: Gen [Int]
genNonEmptyIntList =
  (:) <$> arbitrary <*> listOf arbitrary

genSearchVerdict :: Gen (SearchVerdict Bool Int)
genSearchVerdict =
  oneof
    [ pure SearchAccepted,
      SearchRejected <$> genNonEmptyInt,
      SearchUndecided <$> genNonEmptyBool <*> pure [],
      SearchUndecided <$> genNonEmptyBool <*> genNonEmptyIntList
    ]

propRejectedListHomomorphism :: Property
propRejectedListHomomorphism =
  forAll genVerdict $ \left ->
    forAll genVerdict $ \right ->
      verdictRejectedList (left <> right) === verdictRejectedList left <> verdictRejectedList right

propMonoidIdentity :: Property
propMonoidIdentity =
  forAll genVerdict $ \verdict ->
    (mempty <> verdict, verdict <> mempty) === (verdict, verdict)

propSemigroupAssociativity :: Property
propSemigroupAssociativity =
  forAll genVerdict $ \first ->
    forAll genVerdict $ \second ->
      forAll genVerdict $ \third ->
        ((first <> second) <> third) === (first <> (second <> third))

propRejectedListRoundTrip :: Property
propRejectedListRoundTrip =
  forAll (listOf arbitrary) $ \obstructions ->
    verdictRejectedList (rejectedFromList (obstructions :: [Int])) === obstructions

propAllowedIffNoObstructions :: Property
propAllowedIffNoObstructions =
  forAll (listOf arbitrary) $ \obstructions ->
    verdictAllowed (rejectedFromList (obstructions :: [Int])) === null obstructions

propAnyAcceptedAccepts :: Property
propAnyAcceptedAccepts =
  forAll (listOf genVerdict) $ \verdicts ->
    any verdictAllowed verdicts ==> acceptIfAnyAccepted verdicts === acceptedUnit

propAllRejectedAccumulates :: Property
propAllRejectedAccumulates =
  forAll (listOf genVerdict) $ \verdicts ->
    not (any verdictAllowed verdicts)
      ==> verdictRejectedList (acceptIfAnyAccepted verdicts) === concatMap verdictRejectedList verdicts

propSearchMonoidIdentity :: Property
propSearchMonoidIdentity =
  forAll genSearchVerdict $ \searchVerdict ->
    (mempty <> searchVerdict, searchVerdict <> mempty) === (searchVerdict, searchVerdict)

propSearchSemigroupAssociativity :: Property
propSearchSemigroupAssociativity =
  forAll genSearchVerdict $ \first ->
    forAll genSearchVerdict $ \second ->
      forAll genSearchVerdict $ \third ->
        ((first <> second) <> third) === (first <> (second <> third))

propSearchTaintWitnesses :: Property
propSearchTaintWitnesses =
  forAll genSearchVerdict $ \left ->
    forAll genSearchVerdict $ \right ->
      ( searchVerdictObstructions (left <> right),
        searchVerdictRefusals (left <> right)
      )
        === ( searchVerdictObstructions left <> searchVerdictObstructions right,
              searchVerdictRefusals left <> searchVerdictRefusals right
            )

propDecidedSearchVerdictHomomorphism :: Property
propDecidedSearchVerdictHomomorphism =
  forAll genVerdict $ \left ->
    forAll genVerdict $ \right ->
      ( decidedSearchVerdict (mempty :: ObstructionVerdict Int),
        decidedSearchVerdict (left <> right) :: SearchVerdict Bool Int
      )
        === ( mempty :: SearchVerdict Bool Int,
              decidedSearchVerdict left <> decidedSearchVerdict right
            )

propCompleteDecidedSearchVerdict :: Property
propCompleteDecidedSearchVerdict =
  forAll genVerdict $ \verdict ->
    completeSearchVerdict (decidedSearchVerdict verdict :: SearchVerdict Bool Int) === Just verdict

propSearchDecidedness :: Property
propSearchDecidedness =
  forAll genSearchVerdict $ \searchVerdict ->
    forAll genSearchVerdict $ \left ->
      forAll genSearchVerdict $ \right ->
        ( isNothing (completeSearchVerdict searchVerdict),
          searchVerdictDecided (left <> right)
        )
          === ( not (searchVerdictDecided searchVerdict),
                searchVerdictDecided left && searchVerdictDecided right
              )
