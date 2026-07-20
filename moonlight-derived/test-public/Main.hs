module Main (main) where

import Control.Monad (unless, when)
import Moonlight.Derived.Complex (derivedPoset)
import Moonlight.Derived.Failure ()
import Moonlight.Derived.Functor ()
import Moonlight.Derived.Gluing ()
import Moonlight.Derived.Matrix ()
import Moonlight.Derived.Morse ()
import Moonlight.Derived.Presentation.Builder ()
import Moonlight.Derived.Pruning ()
import Moonlight.Derived.Site
  ( FinObjectId (..)
  , derivedFromFiniteChainComplex
  , leqChecked
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.Derived.Triangulated ()
import Moonlight.Homology
  ( BoundaryIncidence
  , FiniteChainComplex
  , HomologicalDegree (..)
  , emptyBoundaryIncidenceOf
  , mkBoundaryEntry
  , mkBoundaryIncidence
  , mkFiniteChainComplexChecked
  )

main :: IO ()
main = do
  case mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)] of
    Left failureValue -> fail (show failureValue)
    Right _ -> pure ()
  nonzeroComplex <- expectRight (twoTermFiniteChainComplex 1)
  nonzeroDerived <- expectRight (derivedFromFiniteChainComplex nonzeroComplex)
  nonzeroIncidenceIsOrdered <-
    expectRight
      (leqChecked (derivedPoset nonzeroDerived) (FinObjectId 1) (FinObjectId 0))
  unless nonzeroIncidenceIsOrdered
    (fail "a GF2-nonzero boundary incidence must induce its derived-site order edge")
  evenComplex <- expectRight (twoTermFiniteChainComplex 2)
  evenDerived <- expectRight (derivedFromFiniteChainComplex evenComplex)
  evenIncidenceIsOrdered <-
    expectRight
      (leqChecked (derivedPoset evenDerived) (FinObjectId 1) (FinObjectId 0))
  when evenIncidenceIsOrdered
    (fail "an even boundary coefficient must not induce a spurious GF2 order edge")

twoTermFiniteChainComplex :: Int -> Either String (FiniteChainComplex Int)
twoTermFiniteChainComplex coefficientValue = do
  degreeOneBoundary <-
    either (Left . show) Right
      (mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 coefficientValue])
  either (Left . show) Right
    ( mkFiniteChainComplexChecked
        (HomologicalDegree 1)
        (boundaryAtDegree degreeOneBoundary)
    )

boundaryAtDegree :: BoundaryIncidence Int -> HomologicalDegree -> BoundaryIncidence Int
boundaryAtDegree degreeOneBoundary (HomologicalDegree degreeValue) =
  case degreeValue of
    0 -> emptyBoundaryIncidenceOf 1 0
    1 -> degreeOneBoundary
    _ -> emptyBoundaryIncidenceOf 0 0

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight resultValue =
  case resultValue of
    Left errorValue -> fail (show errorValue)
    Right value -> pure value
