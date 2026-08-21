-- | Pure exact convex-polygon algebra: admission, linear edge-angle
-- convolution, reflection, hull construction, and support-half-plane erosion.
module Moonlight.Triangulation.Internal.Minkowski.Convex
  ( convexPolygon
  , convexPolygonPoints
  , convexPolygonRegion
  , admittedConvexLoop
  , structuringElement
  , structuringElementPolygon
  , convexMinkowskiSum
  , convexMinkowskiPolygon
  , convexHullPolygon
  , reflectConvexPolygon
  , convexPolygonCentroid
  , erodeConvexBy
  , addExactPoints
  , subtractExactPoints
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , ExactVector (..)
  , addExactVectors
  , compareExactVectorAngle
  , exactVectorFromPoints
  , exactSegment
  , exactSupportingLineIntersection
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , translateExactPoint
  )
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( cyclePairs
  , cyclePairsNonEmpty
  , cyclicTriples
  , rotateCycleLeast
  , rotateCycleLeastBy
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactDivide
  )
import Moonlight.Triangulation.Internal.Minkowski.Types
  ( ConvexPolygon (..)
  , MinkowskiError (..)
  , StructuringElement (..)
  )
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , RegionPointLocation (..)
  )
import Moonlight.Triangulation.Region
  ( exactLoop
  , exactLoopPoints
  , regionPointLocation
  )

convexPolygon
  :: NonEmpty ExactPoint
  -> Either MinkowskiError ConvexPolygon
convexPolygon submitted = do
  loop <- first MinkowskiInvalidConvexLoop (exactLoop submitted)
  let points = exactLoopPoints loop
  case
    [ (index, turn)
    | (index, (previous, current, next)) <-
        zip [0 :: Int ..] (cyclicTriples (NonEmpty.toList points))
    , let turn = exactOrient2d previous current next
    , turn /= GT
    ] of
    (index, turn) : _ -> Left (MinkowskiNonConvexTurn index turn)
    [] -> Right (ConvexPolygon loop)

convexPolygonPoints :: ConvexPolygon -> NonEmpty ExactPoint
convexPolygonPoints (ConvexPolygon loop) = exactLoopPoints loop

convexPolygonRegion :: ConvexPolygon -> PlanarRegion
convexPolygonRegion (ConvexPolygon loop) =
  PlanarRegion [PolygonComponent loop []]

admittedConvexLoop :: ExactLoop -> Maybe ConvexPolygon
admittedConvexLoop loop
  | all ((== GT) . orderedTurn) (cyclicTriples (NonEmpty.toList (exactLoopPoints loop))) =
      Just (ConvexPolygon loop)
  | otherwise = Nothing
 where
  orderedTurn (previous, current, next) =
    exactOrient2d previous current next

structuringElement
  :: ConvexPolygon
  -> Either MinkowskiError StructuringElement
structuringElement polygon =
  let origin = exactPoint 0 0
      location = regionPointLocation (convexPolygonRegion polygon) origin
   in case location of
        RegionExterior -> Left (MinkowskiOriginOutside location)
        _ -> Right (StructuringElement polygon)

structuringElementPolygon :: StructuringElement -> ConvexPolygon
structuringElementPolygon (StructuringElement polygon) = polygon

convexMinkowskiSum
  :: ConvexPolygon
  -> ConvexPolygon
  -> PlanarRegion
convexMinkowskiSum left right =
  convexPolygonRegion (convexMinkowskiPolygon left right)

convexMinkowskiPolygon
  :: ConvexPolygon
  -> ConvexPolygon
  -> ConvexPolygon
convexMinkowskiPolygon left right =
  let leftPoints = rotateCycleLeastBy pointSweepKey (convexPolygonPoints left)
      rightPoints = rotateCycleLeastBy pointSweepKey (convexPolygonPoints right)
      start = addExactPoints (NonEmpty.head leftPoints) (NonEmpty.head rightPoints)
      directions =
        mergeDirections
          (edgeDirections leftPoints)
          (edgeDirections rightPoints)
      directionList = NonEmpty.toList directions
      scanned = scanl translateExactPoint start directionList
      resultPoints = start :| take (length directionList - 1) (drop 1 scanned)
   in ConvexPolygon (ExactLoop (rotateCycleLeast resultPoints))

convexHullPolygon
  :: NonEmpty ExactPoint
  -> Either MinkowskiError ConvexPolygon
convexHullPolygon submitted =
  let points = NonEmpty.toList submitted
   in case convexHullPoints points of
    Nothing -> Left (MinkowskiConvexHullDegenerate points)
    Just hullPoints -> Right (ConvexPolygon (ExactLoop (rotateCycleLeast hullPoints)))

reflectConvexPolygon :: ConvexPolygon -> ConvexPolygon
reflectConvexPolygon polygon =
  ConvexPolygon
    ( ExactLoop
        ( rotateCycleLeast
            (NonEmpty.reverse (fmap negateExactPoint (convexPolygonPoints polygon)))
        )
    )

convexPolygonCentroid
  :: ConvexPolygon
  -> Either MinkowskiError ExactPoint
convexPolygonCentroid polygon = do
  let points = convexPolygonPoints polygon
      count = fromIntegral (NonEmpty.length points)
      (sumX, sumY) =
        List.foldl'
          (\(accumulatedX, accumulatedY) point ->
             let (x, y) = exactPointCoordinates point
              in (accumulatedX + x, accumulatedY + y))
          (0, 0)
          points
  x <- first MinkowskiExactArithmetic (exactDivide sumX count)
  y <- first MinkowskiExactArithmetic (exactDivide sumY count)
  pure (exactPoint x y)

-- | Erode one convex polygon by another through the strongest translated
-- support half-plane for each source edge. Sutherland--Hodgman descent keeps
-- the construction exact; a lower-dimensional residual is represented by the
-- empty polygonal region at this two-dimensional publication boundary.
erodeConvexBy
  :: ConvexPolygon
  -> ConvexPolygon
  -> Either MinkowskiError (Maybe ConvexPolygon)
erodeConvexBy source kernel = do
  let sourcePoints = convexPolygonPoints source
      kernelPoints = convexPolygonPoints kernel
      firstKernel = NonEmpty.head kernelPoints
      initial =
        map
          (`subtractExactPoints` firstKernel)
          (NonEmpty.toList sourcePoints)
      halfPlanes =
        [ strongestHalfPlane kernelPoints from to
        | (from, to) <- cyclePairs sourcePoints
        ]
  clipped <- foldM clipPolygon initial halfPlanes
  pure (ConvexPolygon . ExactLoop . rotateCycleLeast <$> convexHullPoints clipped)

edgeDirections :: NonEmpty ExactPoint -> NonEmpty ExactVector
edgeDirections = fmap (uncurry exactVectorFromPoints) . cyclePairsNonEmpty

mergeDirections
  :: NonEmpty ExactVector
  -> NonEmpty ExactVector
  -> NonEmpty ExactVector
mergeDirections (left :| leftTail) (right :| rightTail) =
  case compareExactVectorAngle left right of
    LT -> left :| mergeRemaining leftTail (right : rightTail)
    GT -> right :| mergeRemaining (left : leftTail) rightTail
    EQ -> addExactVectors left right :| mergeRemaining leftTail rightTail

mergeRemaining :: [ExactVector] -> [ExactVector] -> [ExactVector]
mergeRemaining [] right = right
mergeRemaining left [] = left
mergeRemaining left@(leftHead : leftTail) right@(rightHead : rightTail) =
  case compareExactVectorAngle leftHead rightHead of
    LT -> leftHead : mergeRemaining leftTail right
    GT -> rightHead : mergeRemaining left rightTail
    EQ -> addExactVectors leftHead rightHead : mergeRemaining leftTail rightTail

pointSweepKey :: ExactPoint -> (ExactRational, ExactRational)
pointSweepKey point =
  let (x, y) = exactPointCoordinates point
   in (y, x)

convexHullPoints :: [ExactPoint] -> Maybe (NonEmpty ExactPoint)
convexHullPoints submitted =
  case Set.toAscList (Set.fromList submitted) of
    firstPoint : secondPoint : thirdPoint : remaining ->
      let ordered = firstPoint : secondPoint : thirdPoint : remaining
          lower = dropFinal (reverse (List.foldl' hullStep [] ordered))
          upper = dropFinal (reverse (List.foldl' hullStep [] (reverse ordered)))
       in case lower <> upper of
            firstHullPoint : secondHullPoint : thirdHullPoint : hullTail ->
              Just (firstHullPoint :| (secondHullPoint : thirdHullPoint : hullTail))
            _ -> Nothing
    _ -> Nothing

hullStep :: [ExactPoint] -> ExactPoint -> [ExactPoint]
hullStep (current : previous : remaining) candidate
  | exactOrient2d previous current candidate /= GT =
      hullStep (previous : remaining) candidate
hullStep hull candidate = candidate : hull

dropFinal :: [value] -> [value]
dropFinal values =
  case reverse values of
    _ : remaining -> reverse remaining
    [] -> []

strongestHalfPlane
  :: NonEmpty ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> (ExactPoint, ExactPoint)
strongestHalfPlane kernelPoints from to =
  let direction = exactVectorFromPoints from to
      supportPoint =
        case kernelPoints of
          initial :| remaining ->
            List.foldl'
              (\selected candidate ->
                 if directionPointCross direction candidate
                      < directionPointCross direction selected
                   then candidate
                   else selected)
              initial
              remaining
   in ( subtractExactPoints from supportPoint
      , subtractExactPoints to supportPoint
      )

directionPointCross :: ExactVector -> ExactPoint -> ExactRational
directionPointCross (ExactVector directionX directionY) point =
  let (x, y) = exactPointCoordinates point
   in directionX * y - directionY * x

clipPolygon
  :: [ExactPoint]
  -> (ExactPoint, ExactPoint)
  -> Either MinkowskiError [ExactPoint]
clipPolygon [] _ = Right []
clipPolygon polygon halfPlane =
  concat <$> traverse (clipEdge halfPlane) (cyclePairsList polygon)

clipEdge
  :: (ExactPoint, ExactPoint)
  -> (ExactPoint, ExactPoint)
  -> Either MinkowskiError [ExactPoint]
clipEdge (boundaryFrom, boundaryTo) (from, to) =
  case (inside from, inside to) of
    (True, True) -> Right [to]
    (True, False) -> (: []) <$> supportingLineIntersection from to boundaryFrom boundaryTo
    (False, True) -> do
      crossing <- supportingLineIntersection from to boundaryFrom boundaryTo
      pure [crossing, to]
    (False, False) -> Right []
 where
  inside point = exactOrient2d boundaryFrom boundaryTo point /= LT

supportingLineIntersection
  :: ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> Either MinkowskiError ExactPoint
supportingLineIntersection lineFrom lineTo boundaryFrom boundaryTo = do
  clippedSegment <- first MinkowskiInvalidSegment (exactSegment lineFrom lineTo)
  boundarySegment <- first MinkowskiInvalidSegment (exactSegment boundaryFrom boundaryTo)
  first MinkowskiLineIntersection
    (exactSupportingLineIntersection clippedSegment boundarySegment)

addExactPoints :: ExactPoint -> ExactPoint -> ExactPoint
addExactPoints left right =
  let (leftX, leftY) = exactPointCoordinates left
      (rightX, rightY) = exactPointCoordinates right
   in exactPoint (leftX + rightX) (leftY + rightY)

subtractExactPoints :: ExactPoint -> ExactPoint -> ExactPoint
subtractExactPoints left right =
  let (leftX, leftY) = exactPointCoordinates left
      (rightX, rightY) = exactPointCoordinates right
   in exactPoint (leftX - rightX) (leftY - rightY)

negateExactPoint :: ExactPoint -> ExactPoint
negateExactPoint point =
  let (x, y) = exactPointCoordinates point
   in exactPoint (negate x) (negate y)

cyclePairsList :: [value] -> [(value, value)]
cyclePairsList = maybe [] cyclePairs . NonEmpty.nonEmpty
