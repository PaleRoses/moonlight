module Moonlight.Pale.Test.Laws.Restriction
  ( RestrictionLawSeed (..),
    restrictionLawSeed,
    withComparablePairs,
    withComparableTriples,
    unfoldRestrictionLaws,
  )
where

import Control.Monad (forM_)
import Data.Kind (Type)
import Moonlight.Pale.Test.LawSuite (LawSuite, hUnitLaw, lawGroup)
import Test.Tasty.HUnit ((@?=))

type RestrictionLawSeed :: Type -> Type -> Type
data RestrictionLawSeed cell val = RestrictionLawSeed
  { rlsName :: String,
    rlsRestrict :: cell -> cell -> val -> val,
    rlsLeq :: cell -> cell -> Bool,
    rlsSections :: [(cell, val)],
    rlsComparablePairs :: [(cell, cell)],
    rlsComparableTriples :: [(cell, cell, cell)]
  }

restrictionLawSeed ::
  String ->
  (cell -> cell -> val -> val) ->
  (cell -> cell -> Bool) ->
  [(cell, val)] ->
  RestrictionLawSeed cell val
restrictionLawSeed name restrict leq sections =
  RestrictionLawSeed
    { rlsName = name,
      rlsRestrict = restrict,
      rlsLeq = leq,
      rlsSections = sections,
      rlsComparablePairs = [],
      rlsComparableTriples = []
    }

withComparablePairs :: [(cell, cell)] -> RestrictionLawSeed cell val -> RestrictionLawSeed cell val
withComparablePairs pairs seed =
  seed {rlsComparablePairs = pairs}

withComparableTriples :: [(cell, cell, cell)] -> RestrictionLawSeed cell val -> RestrictionLawSeed cell val
withComparableTriples triples seed =
  seed {rlsComparableTriples = triples}

unfoldRestrictionLaws :: (Eq cell, Show val, Eq val) => RestrictionLawSeed cell val -> [LawSuite]
unfoldRestrictionLaws seed =
  [ lawGroup (rlsName seed <> " restriction laws") $
      concat
        [ [identityLaw seed],
          compositionLaw seed,
          functorialLeftIdentity seed,
          functorialRightIdentity seed
        ]
  ]

identityLaw :: (Show val, Eq val) => RestrictionLawSeed cell val -> LawSuite
identityLaw seed =
  hUnitLaw "restriction identity" $
    forM_ (rlsSections seed) $ \(a, x) ->
      rlsRestrict seed a a x @?= x

compositionLaw :: (Eq cell, Show val, Eq val) => RestrictionLawSeed cell val -> [LawSuite]
compositionLaw seed =
  case rlsComparableTriples seed of
    [] -> []
    triples ->
      [ hUnitLaw "restriction composition" $
          forM_ triples $ \(a, b, c) ->
            forM_ (sectionsAt seed a) $ \x ->
              rlsRestrict seed b c (rlsRestrict seed a b x) @?= rlsRestrict seed a c x
      ]

functorialLeftIdentity :: (Eq cell, Show val, Eq val) => RestrictionLawSeed cell val -> [LawSuite]
functorialLeftIdentity seed =
  case rlsComparablePairs seed of
    [] -> []
    pairs ->
      [ hUnitLaw "functorial left identity" $
          forM_ pairs $ \(a, b) ->
            forM_ (sectionsAt seed a) $ \x ->
              rlsRestrict seed a a (rlsRestrict seed a b x) @?= rlsRestrict seed a b x
      ]

functorialRightIdentity :: (Eq cell, Show val, Eq val) => RestrictionLawSeed cell val -> [LawSuite]
functorialRightIdentity seed =
  case rlsComparablePairs seed of
    [] -> []
    pairs ->
      [ hUnitLaw "functorial right identity" $
          forM_ pairs $ \(a, b) ->
            forM_ (sectionsAt seed a) $ \x ->
              rlsRestrict seed a b (rlsRestrict seed b b x) @?= rlsRestrict seed a b x
      ]

sectionsAt :: Eq cell => RestrictionLawSeed cell val -> cell -> [val]
sectionsAt seed target =
  [v | (c, v) <- rlsSections seed, c == target]
