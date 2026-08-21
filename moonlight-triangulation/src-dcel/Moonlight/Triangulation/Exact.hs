{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact rational planar geometry over admitted binary64 points.
module Moonlight.Triangulation.Exact
  ( ExactPoint
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , ExactVector (..)
  , exactVectorFromPoints
  , addExactVectors
  , exactCross
  , compareExactVectorAngle
  , translateExactPoint
  , ExactSegment
  , ExactGeometryError (..)
  , exactSegment
  , exactSegmentEndpoints
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactOrient2d
  , exactOnClosedSegment
  , SegmentRelation (..)
  , allSegmentRelations
  , exactSegmentRelation
  , ExactIntersectionError (..)
  , exactLineIntersection
  , exactSupportingLineIntersection
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Moonlight.Triangulation.Internal.Dyadic (integerRatioToDouble)
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactDivide
  , exactRationalDenominator
  , exactRationalFromFiniteDouble
  , exactRationalNumerator
  , exactSignum
  )
import Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  , allSegmentRelations
  , segmentRelationWith
  )
import Moonlight.Triangulation.Math (mkQueryPoint)
import Moonlight.Triangulation.Types
  ( Point (..)
  , PointValidationError
  , QueryPoint
  , queryPointValue
  )

-- | A strict exact Cartesian point.
data ExactPoint = ExactPoint !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A strict exact segment whose endpoints are distinct.
data ExactSegment = ExactSegment !ExactPoint !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Witness-bearing refusals from exact segment construction.
data ExactGeometryError
  = ExactSegmentEndpointsCoincide !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Witness-bearing refusals from exact line intersection.
data ExactIntersectionError
  = ExactIntersectionAbsent !SegmentRelation
  | ExactIntersectionNonUnique !SegmentRelation
  | ExactIntersectionParallelOrDegenerate !ExactRational
  | ExactIntersectionArithmetic !ExactArithmeticError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Construct an exact point from two exact coordinates.
exactPoint :: ExactRational -> ExactRational -> ExactPoint
exactPoint = ExactPoint
{-# INLINE exactPoint #-}

-- | Read both exact point coordinates.
exactPointCoordinates :: ExactPoint -> (ExactRational, ExactRational)
exactPointCoordinates (ExactPoint x y) = (x, y)
{-# INLINE exactPointCoordinates #-}

-- | Determinant of two points regarded as vectors from the Cartesian origin.
exactPointCross :: ExactPoint -> ExactPoint -> ExactRational
exactPointCross (ExactPoint ax ay) (ExactPoint bx by) = ax * by - ay * bx
{-# INLINE exactPointCross #-}

-- | Construct an exact segment, refusing coincident endpoints with their
-- shared point as the witness.
exactSegment
  :: ExactPoint
  -> ExactPoint
  -> Either ExactGeometryError ExactSegment
exactSegment from to
  | from == to = Left (ExactSegmentEndpointsCoincide from)
  | otherwise = Right (ExactSegment from to)

-- | Read both distinct exact segment endpoints.
exactSegmentEndpoints :: ExactSegment -> (ExactPoint, ExactPoint)
exactSegmentEndpoints (ExactSegment from to) = (from, to)
{-# INLINE exactSegmentEndpoints #-}

-- | Validate and exactly embed a raw binary64 point.
exactPointFromPoint :: Point -> Either PointValidationError ExactPoint
exactPointFromPoint = fmap exactPointFromQueryPoint . mkQueryPoint

-- | Exactly embed an already-admitted query point without repeating
-- coordinate validation.
exactPointFromQueryPoint :: QueryPoint -> ExactPoint
exactPointFromQueryPoint queryPoint =
  case queryPointValue queryPoint of
    Point x y ->
      ExactPoint
        (exactRationalFromFiniteDouble x)
        (exactRationalFromFiniteDouble y)
{-# INLINE exactPointFromQueryPoint #-}

-- | Deterministically project an exact point to a validated binary64
-- embedding candidate. This is not a correctly-rounded nearest-double claim;
-- callers must certify the candidate projection before relying on it.
exactPointToEmbeddingCandidate
  :: ExactPoint
  -> Either PointValidationError Point
exactPointToEmbeddingCandidate (ExactPoint x y) =
  queryPointValue
    <$> mkQueryPoint
      ( Point
          (integerRatioToDouble (exactRationalNumerator x) (exactRationalDenominator x))
          (integerRatioToDouble (exactRationalNumerator y) (exactRationalDenominator y))
      )

-- | Exact orientation of an ordered triple. 'GT' is a positive determinant
-- and counter-clockwise turn, 'EQ' is collinear, and 'LT' is clockwise.
exactOrient2d :: ExactPoint -> ExactPoint -> ExactPoint -> Ordering
exactOrient2d
  (ExactPoint ax ay)
  (ExactPoint bx by)
  (ExactPoint cx cy) =
    exactSignum
      ((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
{-# INLINE exactOrient2d #-}

-- | Whether an exact point lies on an exact closed segment.
exactOnClosedSegment :: ExactPoint -> ExactPoint -> ExactPoint -> Bool
exactOnClosedSegment
  from@(ExactPoint ax ay)
  to@(ExactPoint bx by)
  query@(ExactPoint qx qy) =
    exactOrient2d from to query == EQ
      && qx >= min ax bx
      && qx <= max ax bx
      && qy >= min ay by
      && qy <= max ay by
{-# INLINE exactOnClosedSegment #-}

-- | Exact rational specialization of the one closed-segment relation policy.
exactSegmentRelation
  :: ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> SegmentRelation
exactSegmentRelation =
  segmentRelationWith (==) compare exactOrient2d exactOnClosedSegment
{-# INLINE exactSegmentRelation #-}

-- | Return the unique exact intersection of two exact segments. Disjoint and
-- non-unique relations are refused with their relation witness; a zero line
-- cross product and arithmetic failure retain their exact witnesses.
exactLineIntersection
  :: ExactSegment
  -> ExactSegment
  -> Either ExactIntersectionError ExactPoint
exactLineIntersection
  (ExactSegment a b)
  (ExactSegment c d) =
    case exactSegmentRelation a b c d of
      SegmentsDisjoint -> Left (ExactIntersectionAbsent SegmentsDisjoint)
      SegmentsDuplicate -> Left (ExactIntersectionNonUnique SegmentsDuplicate)
      SegmentsCollinearlyOverlap ->
        Left (ExactIntersectionNonUnique SegmentsCollinearlyOverlap)
      SegmentsShareEndpoint -> uniqueIntersection
      SegmentsProperlyCross -> uniqueIntersection
      SegmentEndpointTouchesInterior -> uniqueIntersection
 where
  uniqueIntersection =
    exactSupportingLineIntersection (ExactSegment a b) (ExactSegment c d)

-- | Intersect the infinite supporting lines of two admitted exact segments.
-- Unlike 'exactLineIntersection', the intersection need not lie inside either
-- closed segment. Parallel supporting lines retain the exact zero denominator
-- witness.
exactSupportingLineIntersection
  :: ExactSegment
  -> ExactSegment
  -> Either ExactIntersectionError ExactPoint
exactSupportingLineIntersection
  (ExactSegment a b)
  (ExactSegment c d) =
  let directionAB = exactVectorFromPoints a b
      directionCD = exactVectorFromPoints c d
      fromAToC = exactVectorFromPoints a c
      denominator = exactCross directionAB directionCD
      numerator = exactCross fromAToC directionCD
   in case exactDivide numerator denominator of
        Left ExactZeroDivisor ->
          Left (ExactIntersectionParallelOrDegenerate denominator)
        Left arithmeticError -> Left (ExactIntersectionArithmetic arithmeticError)
        Right parameter ->
          Right (translateExactPoint a (scaleExactVector parameter directionAB))

-- | A strict exact displacement vector. Points and vectors remain distinct;
-- all exact planar algorithms share this single vector carrier.
data ExactVector = ExactVector !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

exactVectorFromPoints :: ExactPoint -> ExactPoint -> ExactVector
exactVectorFromPoints (ExactPoint ax ay) (ExactPoint bx by) =
  ExactVector (bx - ax) (by - ay)

addExactVectors :: ExactVector -> ExactVector -> ExactVector
addExactVectors (ExactVector ax ay) (ExactVector bx by) =
  ExactVector (ax + bx) (ay + by)

exactCross :: ExactVector -> ExactVector -> ExactRational
exactCross (ExactVector ax ay) (ExactVector bx by) =
  ax * by - ay * bx

-- | Counter-clockwise angular order from the positive x-axis. Collinear
-- vectors on the same ray compare equal so convolution can merge them;
-- callers that need a total point order may add their own radial tie-break.
compareExactVectorAngle :: ExactVector -> ExactVector -> Ordering
compareExactVectorAngle left right =
  case compare (vectorHalf left) (vectorHalf right) of
    EQ ->
      case exactSignum (exactCross left right) of
        GT -> LT
        LT -> GT
        EQ -> EQ
    ordering -> ordering
 where
  vectorHalf (ExactVector x y)
    | exactSignum y == GT = False
    | exactSignum y == EQ && exactSignum x /= LT = False
    | otherwise = True
{-# INLINE compareExactVectorAngle #-}

scaleExactVector :: ExactRational -> ExactVector -> ExactVector
scaleExactVector scale (ExactVector x y) =
  ExactVector (scale * x) (scale * y)

translateExactPoint :: ExactPoint -> ExactVector -> ExactPoint
translateExactPoint (ExactPoint x y) (ExactVector dx dy) =
  ExactPoint (x + dx) (y + dy)
