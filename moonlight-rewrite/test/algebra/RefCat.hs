{-# LANGUAGE TypeFamilies #-}

module RefCat
  ( RefCat (..),
    RefOb (..),
    RefMor,
    RefTwoMor (..),
    RefCompositor (..),
    refMorFrom,
    refMorTo,
    refInclusion,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Moonlight.Category
  ( AdhesiveCategory (..),
    Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    MonicMatchComponents (..),
    PBPOAdhesiveCategory,
    PushoutComplementComponents (..),
    monicMatchArrow,
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

newtype RefTwoMor = RefTwoMor ()

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

  identity _ (RefOb refs) =
    Right (RefMor refs refs)

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
