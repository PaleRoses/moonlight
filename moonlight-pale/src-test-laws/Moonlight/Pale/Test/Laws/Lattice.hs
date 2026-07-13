module Moonlight.Pale.Test.Laws.Lattice
  ( LatticeLawSeed,
    LatticeLawSeedError (..),
    latticeLawSeed,
    withBounded,
    withComparableFilter,
    withUniverse,
    unfoldLatticeLaws,
  )
where

import Control.Applicative (liftA3)
import Control.Monad (forM_)
import Data.Kind (Type)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Moonlight.Pale.Test.LawSuite (LawSuite, hUnitLaw, lawGroup)
import Test.Tasty.HUnit ((@?=))

type LatticeLawSeed :: Type -> Type
data LatticeLawSeed a = LatticeLawSeed
  { llsName :: String,
    llsJoin :: a -> a -> a,
    llsMeet :: a -> a -> a,
    llsBounds :: Maybe (LatticeLawBounds a),
    llsUniverse :: NonEmpty a,
    llsFilter :: a -> a -> Bool
  }

type LatticeLawBounds :: Type -> Type
data LatticeLawBounds a = LatticeLawBounds
  { llbBottom :: a,
    llbTop :: a
  }

type LatticeLawSeedError :: Type -> Type
data LatticeLawSeedError a
  = DuplicateUniverseElement a
  | BottomOutsideUniverse a
  | TopOutsideUniverse a
  | JoinOutsideUniverse a a a
  | MeetOutsideUniverse a a a
  deriving stock (Eq, Show)

latticeLawSeed ::
  Eq a =>
  String ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  NonEmpty a ->
  Either (NonEmpty (LatticeLawSeedError a)) (LatticeLawSeed a)
latticeLawSeed name joinOp meetOp universe =
  checkedSeed $
    LatticeLawSeed
      { llsName = name,
        llsJoin = joinOp,
        llsMeet = meetOp,
        llsBounds = Nothing,
        llsUniverse = universe,
        llsFilter = const (const True)
      }

withBounded ::
  Eq a =>
  a ->
  a ->
  LatticeLawSeed a ->
  Either (NonEmpty (LatticeLawSeedError a)) (LatticeLawSeed a)
withBounded bot tp seed =
  checkedSeed seed {llsBounds = Just (LatticeLawBounds bot tp)}

withComparableFilter :: (a -> a -> Bool) -> LatticeLawSeed a -> LatticeLawSeed a
withComparableFilter f seed =
  seed {llsFilter = f}

withUniverse ::
  Eq a =>
  NonEmpty a ->
  LatticeLawSeed a ->
  Either (NonEmpty (LatticeLawSeedError a)) (LatticeLawSeed a)
withUniverse u seed =
  checkedSeed seed {llsUniverse = u}

unfoldLatticeLaws :: (Show a, Eq a) => LatticeLawSeed a -> [LawSuite]
unfoldLatticeLaws seed =
  [ lawGroup (llsName seed <> " lattice laws") $
      concat
        [ [joinCommutativity seed],
          [meetCommutativity seed],
          [joinAssociativity seed],
          [meetAssociativity seed],
          [joinAbsorption seed],
          [meetAbsorption seed],
          [joinIdempotence seed],
          [meetIdempotence seed],
          boundedBottom seed,
          boundedTop seed
        ]
  ]

joinCommutativity :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
joinCommutativity seed =
  hUnitLaw "join is commutative" $
    forM_ (filteredPairs seed) $ \(x, y) ->
      llsJoin seed x y @?= llsJoin seed y x

meetCommutativity :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
meetCommutativity seed =
  hUnitLaw "meet is commutative" $
    forM_ (filteredPairs seed) $ \(x, y) ->
      llsMeet seed x y @?= llsMeet seed y x

joinAssociativity :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
joinAssociativity seed =
  hUnitLaw "join is associative" $
    forM_ (filteredTriples seed) $ \(x, y, z) ->
      llsJoin seed (llsJoin seed x y) z @?= llsJoin seed x (llsJoin seed y z)

meetAssociativity :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
meetAssociativity seed =
  hUnitLaw "meet is associative" $
    forM_ (filteredTriples seed) $ \(x, y, z) ->
      llsMeet seed (llsMeet seed x y) z @?= llsMeet seed x (llsMeet seed y z)

joinAbsorption :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
joinAbsorption seed =
  hUnitLaw "absorption: join a (meet a b) = a" $
    forM_ (filteredPairs seed) $ \(x, y) ->
      llsJoin seed x (llsMeet seed x y) @?= x

meetAbsorption :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
meetAbsorption seed =
  hUnitLaw "absorption: meet a (join a b) = a" $
    forM_ (filteredPairs seed) $ \(x, y) ->
      llsMeet seed x (llsJoin seed x y) @?= x

joinIdempotence :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
joinIdempotence seed =
  hUnitLaw "join is idempotent" $
    forM_ (llsUniverse seed) $ \x ->
      llsJoin seed x x @?= x

meetIdempotence :: (Show a, Eq a) => LatticeLawSeed a -> LawSuite
meetIdempotence seed =
  hUnitLaw "meet is idempotent" $
    forM_ (llsUniverse seed) $ \x ->
      llsMeet seed x x @?= x

boundedBottom :: (Show a, Eq a) => LatticeLawSeed a -> [LawSuite]
boundedBottom seed =
  case llsBounds seed of
    Nothing -> []
    Just bounds ->
      [ hUnitLaw "join with bottom is identity" $
          forM_ (llsUniverse seed) $ \x ->
            llsJoin seed (llbBottom bounds) x @?= x
      ]

boundedTop :: (Show a, Eq a) => LatticeLawSeed a -> [LawSuite]
boundedTop seed =
  case llsBounds seed of
    Nothing -> []
    Just bounds ->
      [ hUnitLaw "meet with top is identity" $
          forM_ (llsUniverse seed) $ \x ->
            llsMeet seed (llbTop bounds) x @?= x
      ]

filteredPairs :: LatticeLawSeed a -> [(a, a)]
filteredPairs seed =
  filter (uncurry (llsFilter seed)) (supportPairs seed)

filteredTriples :: LatticeLawSeed a -> [(a, a, a)]
filteredTriples seed =
  filter comparableTriple (supportTriples seed)
  where
    comparableTriple (x, y, z) =
      llsFilter seed x y && llsFilter seed y z

checkedSeed :: Eq a => LatticeLawSeed a -> Either (NonEmpty (LatticeLawSeedError a)) (LatticeLawSeed a)
checkedSeed seed =
  seed <$ rejectSeedErrors (seedErrors seed)

seedErrors :: Eq a => LatticeLawSeed a -> [LatticeLawSeedError a]
seedErrors seed =
  duplicateErrors (supportValues seed)
    <> boundErrors seed
    <> closureErrors seed

duplicateErrors :: Eq a => [a] -> [LatticeLawSeedError a]
duplicateErrors values =
  fmap DuplicateUniverseElement $
    filter duplicated (nub values)
  where
    duplicated value =
      length (filter (value ==) values) > 1

boundErrors :: Eq a => LatticeLawSeed a -> [LatticeLawSeedError a]
boundErrors seed =
  case llsBounds seed of
    Nothing -> []
    Just bounds ->
      missing BottomOutsideUniverse (llbBottom bounds)
        <> missing TopOutsideUniverse (llbTop bounds)
  where
    values = supportValues seed
    missing toError value
      | value `elem` values = []
      | otherwise = [toError value]

closureErrors :: Eq a => LatticeLawSeed a -> [LatticeLawSeedError a]
closureErrors seed =
  concatMap pairClosureErrors (supportPairs seed)
  where
    values = supportValues seed
    missing toError value
      | value `elem` values = []
      | otherwise = [toError value]
    pairClosureErrors (x, y) =
      missing (JoinOutsideUniverse x y) (llsJoin seed x y)
        <> missing (MeetOutsideUniverse x y) (llsMeet seed x y)

rejectSeedErrors :: [LatticeLawSeedError a] -> Either (NonEmpty (LatticeLawSeedError a)) ()
rejectSeedErrors errors =
  case NE.nonEmpty errors of
    Nothing -> Right ()
    Just nonEmptyErrors -> Left nonEmptyErrors

supportValues :: LatticeLawSeed a -> [a]
supportValues =
  NE.toList . llsUniverse

supportPairs :: LatticeLawSeed a -> [(a, a)]
supportPairs seed =
  (,) <$> values <*> values
  where
    values = supportValues seed

supportTriples :: LatticeLawSeed a -> [(a, a, a)]
supportTriples seed =
  liftA3 (,,) values values values
  where
    values = supportValues seed
